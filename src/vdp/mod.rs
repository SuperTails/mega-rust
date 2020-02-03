use crate::cpu::instruction::Size;
use crate::cpu::Cpu;
use crate::sdl_system::SDLSystem;
use crate::Interrupt;
use bitfield::bitfield;
use bitpat::bitpat;
use sdl2::pixels::Color;
use sdl2::rect::Point;
use std::convert::TryFrom;

/// The Video Display Processor (VDP) handles all of the
/// rendering for the console. It has three types of RAM:
///
/// 64KiB VRAM: Used to store graphics data, i.e. nametable data, sprite data, and the horizontal
/// scroll
///
/// 128 bytes CRAM: Used to store 64 different colors (4 palettes of 16 colors each)
///
/// 40x10bits VSRAM: Used to store vertical scroll data
///
/// Information from segaretro.org
pub struct Vdp {
    // True means write
    ram_address: (u16, bool, RamMode),
    mode1: VdpMode1,
    mode2: VdpMode2,

    dma_length: u16,
    dma_target: (u16, RamMode),
    dma_source: u32,
    dma_mode: DmaMode,
    fill_pending: bool,

    write_pending: Option<u16>,

    plane_a_nametable: u16,
    plane_b_nametable: u16,
    window_nametable: u16,

    sprite_table_addr: u16,

    horiz_scroll_addr: u16,

    plane_height: u16,
    plane_width: u16,

    bg_color: u8,
    horiz_int_period: u8,
    horiz_int_counter: u8,

    auto_increment: u8,

    cycle: u64,
    // TODO: Use this
    #[allow(dead_code)]
    pixel: u32,
    scanline: u32,

    // 64KiB
    vram: [u8; 0x1_00_00],
    // 128 bytes
    cram: [CRamEntry; 0x40],

    // 40 10-bit entries
    vsram: [u16; 40],
}

#[derive(Debug, PartialEq)]
pub enum DmaMode {
    CpuCopy,
    VramFill,
    VramCopy,
}

bitfield! {
    struct VdpMode1(u8);
    impl Debug;

    counter_disable, _: 1;

    use_all_bits, _: 2;

    horiz_interrupt, _: 4;
}

bitfield! {
    struct VdpMode2(u8);
    impl Debug;

    cell_mode, _: 3;

    dma_enable, _: 4;

    vert_interrupt, _: 5;

    display_enable, _: 6;
}

bitfield! {
    #[derive(Clone, Copy)]
    struct CRamEntry(u16);
    impl Debug;

    red, set_red: 3, 1;

    green, set_green: 7, 5;

    blue, set_blue: 11, 9;
}

impl VdpMode2 {
    // TODO: Use this
    #[allow(dead_code)]
    pub fn width(&self) -> u32 {
        if self.cell_mode() {
            240
        } else {
            224
        }
    }
}

#[derive(Debug)]
enum Register {
    Data,
    StatusControl,
    HVCounter,
}

impl Register {
    pub fn decode(addr: u32) -> Option<Register> {
        let addr = 0xFF_FF_FF & addr as usize;

        if addr & 1 != 0 {
            return None;
        }

        match addr {
            0xC0_00_00..=0xC0_00_03 => Some(Register::Data),
            0xC0_00_04..=0xC0_00_07 => Some(Register::StatusControl),
            0xC0_00_08..=0xC0_00_0F => Some(Register::HVCounter),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
// The boolean represents whether it's a read or write:
// false is read, true is write
pub enum RamMode {
    VRam,
    CRam,
    VSRam,
}

#[derive(Debug, Clone)]
struct VdpAddress {
    pub ram_type: RamMode,
    pub address: u16,
}

#[derive(Debug)]
pub enum CDMode {
    Normal(bool),
    CpuCopy,
    VramCopy,
    VramFill,
}

fn decode_cd(cd: u8) -> (CDMode, RamMode) {
    assert_eq!(cd & !0b111_111, 0, "CD: {:#b}", cd);

    // 68k to VDP => Writing command word:
    // CD1 CD0 _ _ _ _ _ _
    // ...
    // ...
    // 1 0 0 CD2 _ _ _ _
    //
    // VRAM Fill => Requires source register T1 T0 == 1 0
    // and command word:
    // 0 1 _ _ _ _ _ _
    // ...
    // ...
    // 1 0 0 0 _ _ _ _
    //
    // VRAM Copy => Writing command word:
    // 0 0 _ _ _ _ _ _
    // ...
    // ...
    // 1 1 0 0 _ _ ? ? A15 A14

    if cd == 0b110_000 {
        (CDMode::VramCopy, RamMode::VRam)
    } else if cd == 0b100_001 {
        (CDMode::VramFill, RamMode::VRam)
    } else if cd >> 3 == 0b100 {
        let ram_mode = match cd {
            0b100_001 => RamMode::VRam,
            0b100_011 => RamMode::CRam,
            0b100_101 => RamMode::VSRam,
            _ => panic!("Unknown or invalid cd: {:#b}", cd),
        };

        (CDMode::CpuCopy, ram_mode)
    } else {
        match cd & 0xF {
            0b0000 => (CDMode::Normal(false), RamMode::VRam),
            0b0001 => (CDMode::Normal(true), RamMode::VRam),
            0b1000 => (CDMode::Normal(false), RamMode::CRam),
            0b0011 => (CDMode::Normal(true), RamMode::CRam),
            0b0100 => (CDMode::Normal(false), RamMode::VSRam),
            0b0101 => (CDMode::Normal(true), RamMode::VSRam),
            _ => panic!("Unknown cd: {:#b}", cd),
        }
    }
}

bitfield! {
    struct TileEntry(u16);

    tile_index, _: 10, 0;

    horiz_flip, _: 11;

    vert_flip, _: 12;

    palette_line, _: 14, 13;

    priority, _: 15;
}

impl Vdp {
    pub fn new() -> Vdp {
        Vdp {
            ram_address: (0, false, RamMode::VRam),
            mode1: VdpMode1(0),
            mode2: VdpMode2(0),
            dma_length: 0,
            dma_target: (0, RamMode::VRam),
            dma_mode: DmaMode::VramFill,
            fill_pending: false,
            write_pending: None,
            plane_a_nametable: 0,
            plane_b_nametable: 0,
            window_nametable: 0,
            sprite_table_addr: 0,
            horiz_scroll_addr: 0,
            plane_width: 0,
            plane_height: 0,
            bg_color: 0,
            pixel: 0,
            scanline: 0,
            horiz_int_period: 0,
            horiz_int_counter: 0,
            dma_source: 0,
            cycle: 0,
            auto_increment: 0,
            vram: [0; 0x1_00_00],
            cram: [CRamEntry(0); 0x40],
            vsram: [0; 40],
        }
    }

    fn get_tile_entry(&self, addr: u16) -> TileEntry {
        let entry = ((self.vram[addr as usize] as u16) << 8) | self.vram[addr as usize + 1] as u16;
        TileEntry(entry)
    }

    fn render_planes(&self, sdl_system: &mut SDLSystem) {
        // TODO: Maybe use this?
        let _plane_cell_height = self.plane_height / 8;
        let plane_cell_width = self.plane_width / 8;

        sdl_system.canvas().set_scale(2.0, 2.0).unwrap();
        for row in 0..self.plane_height {
            for col in 0..self.plane_width {
                let cell_row = row / 8;
                let cell_col = col / 8;

                let pixel_row = row % 8;
                let pixel_col = col % 8;

                let cell = cell_row * plane_cell_width + cell_col;

                let get_color = |addr: u16| -> Option<(u8, u8, u8)> {
                    let tile_entry = self.get_tile_entry(addr);

                    let tile_addr = tile_entry.tile_index() as usize * 0x20;
                    let palette_line = tile_entry.palette_line();

                    let tile = &self.vram[tile_addr..][0..0x20];

                    const BYTES_PER_ROW: usize = 4;

                    let data_row = &tile[pixel_row as usize * BYTES_PER_ROW..][0..BYTES_PER_ROW];
                    let pixel_byte = data_row[pixel_col as usize / 2];
                    let shift = if pixel_col % 2 == 1 { 0 } else { 4 };
                    let data = (pixel_byte >> shift) & 0xF;

                    if data == 0 {
                        None
                    } else {
                        let color = self.cram[palette_line as usize * 0x10 + data as usize];
                        Some((
                            (color.red() as u8) << 5,
                            (color.green() as u8) << 5,
                            (color.blue() as u8) << 5,
                        ))
                    }
                };

                let addr_z = cell * 2 + self.window_nametable;
                let addr_a = cell * 2 + self.plane_a_nametable;
                let addr_b = cell * 2 + self.plane_b_nametable;

                let color_z = get_color(addr_z);
                let color_a = get_color(addr_a);
                let color_b = get_color(addr_b);

                let overall_color = if let Some(c) = color_a {
                    c
                } else if let Some(c) = color_b {
                    c
                } else if let Some(c) = color_z {
                    c
                } else {
                    let bg_color = self.cram[self.bg_color as usize];
                    (
                        (bg_color.red() as u8) << 5,
                        (bg_color.green() as u8) << 5,
                        (bg_color.blue() as u8) << 5,
                    )
                };

                sdl_system
                    .canvas()
                    .set_draw_color(Color::from(overall_color));
                sdl_system
                    .canvas()
                    .draw_point(Point::new(col as i32, row as i32))
                    .unwrap();
            }
        }
    }

    fn render_vram(&self, sdl_system: &mut SDLSystem) {
        for r in 0..256 {
            for c in 0..256 {
                let entry = self.vram[r * 256 + c];

                sdl_system
                    .canvas()
                    .set_draw_color(Color::RGB(entry, entry, entry));
                sdl_system
                    .canvas()
                    .draw_point(Point::new(c as i32 + 256, r as i32))
                    .unwrap();
            }
        }

        let tables = [
            (Color::RGB(255, 0, 0), self.plane_a_nametable),
            (Color::RGB(0, 255, 0), self.plane_b_nametable),
            (Color::RGB(0, 0, 255), self.window_nametable),
        ];

        for (color, table) in tables.iter().copied() {
            let r = table / 256;
            let c = table % 256;

            sdl_system.canvas().set_draw_color(color);
            sdl_system
                .canvas()
                .draw_point(Point::new(c as i32 + 256, r as i32))
                .unwrap();
        }
    }

    fn render_cram(&self, sdl_system: &mut SDLSystem) {
        for p in 0..64 {
            let red = (self.cram[p].red() as u8) << 4;
            let green = (self.cram[p].green() as u8) << 4;
            let blue = (self.cram[p].blue() as u8) << 4;

            sdl_system
                .canvas()
                .set_draw_color(Color::RGB(red, green, blue));
            sdl_system
                .canvas()
                .draw_point(Point::new((p % 16) as i32 + 512, p as i32 / 16))
                .unwrap();
        }
    }

    fn render(&self, sdl_system: &mut SDLSystem) {
        sdl_system.canvas().set_draw_color(Color::RGB(50, 50, 50));
        sdl_system.canvas().clear();

        self.render_planes(sdl_system);
        self.render_vram(sdl_system);
        self.render_cram(sdl_system);

        sdl_system.canvas().set_scale(0.5, 0.5).unwrap();
        sdl_system.canvas().present();
    }

    // 262 line range
    // 342 pixel range
    pub fn do_cycle(&mut self, sdl_system: &mut SDLSystem, int: &mut Option<Interrupt>) -> bool {
        self.cycle += 1;

        // TODO: It should be 488.5 I think,
        // and the CPU definitely does *not* take only one cycle to execute each instruction
        //
        // TODO: But it only has a 342 pixel range!
        if self.cycle % 489 == 0 {
            // One scanline has finished
            self.scanline += 1;
            // TODO: Horizontal interrupts

            if self.horiz_int_counter == 0 {
                if self.mode1.horiz_interrupt() {
                    // TODO: FIX
                    /*assert!(
                        int == &None || int == &Some(Interrupt::Horizontal),
                        "Don't know how to handle multiple interrupts yet"
                    );*/
                    *int = Some(Interrupt::Horizontal);
                }

                self.horiz_int_counter = self.horiz_int_period;
            } else {
                self.horiz_int_counter -= 1;
            }
        }

        // TODO: This might not be correct
        if self.scanline == 262 {
            self.scanline = 0;
            // One frame has finished

            if self.mode2.vert_interrupt() {
                // TODO: FIX
                /*assert!(
                    int == &None || int == &Some(Interrupt::Vertical),
                    "Don't know how to handle multiple interrupts yet"
                );*/
                *int = Some(Interrupt::Vertical);
            }

            self.render(sdl_system);

            for event in sdl_system.event_pump.poll_iter() {
                match event {
                    sdl2::event::Event::KeyDown {
                        keycode: Some(sdl2::keyboard::Keycode::Escape),
                        ..
                    }
                    | sdl2::event::Event::Quit { .. } => return true,
                    _ => {}
                }
            }
        }

        false
    }

    pub fn read(&mut self, addr: u32) -> u16 {
        let reg = Register::decode(addr & !0x1).unwrap();

        // TODO: I think this works?
        self.write_pending = None;

        // *roughly* maps 489 cycles to 342 pixels
        let pixel = (self.cycle % 489) * 7 / 10;

        match reg {
            Register::StatusControl => {
                let vblank_bit = self.scanline > 224;
                let hblank_bit = pixel > 256;
                // TODO: Everything else

                let result = (1 << 9) | // Always show FIFO as being empty
                    ((vblank_bit as u16) << 3) |
                    ((hblank_bit as u16) << 2);

                println!("Returning {:#X} from status register", result);

                result
            }
            Register::HVCounter => {
                let jumped_pixel = u8::try_from(if pixel <= 0xE9 {
                    pixel
                } else {
                    (pixel - 0xEA) + 0x93
                })
                .unwrap();

                let jumped_scanline = u8::try_from(if self.scanline <= 0xEA {
                    self.scanline
                } else {
                    (self.scanline - 0xEB) + 0xE5
                })
                .unwrap();

                ((jumped_scanline as u16) << 8) | jumped_pixel as u16
            }
            _ => {
                println!("Returning 0 from read to {:?}", reg);
                0
            }
        }
    }

    pub fn dma_fill(&mut self, fill_value: u16) {
        match self.dma_target.1 {
            RamMode::VRam => {
                println!("DMA Fill with target {:#X} and length {:#X} of value {:#X} and auto increment {:#X}",
                         self.dma_target.0,
                         self.dma_length,
                         fill_value,
                         self.auto_increment,
                );
                self.vram[self.dma_target.0 as usize] = fill_value as u8;
                for i in 0..self.dma_length {
                    let addr = self
                        .dma_target
                        .0
                        .wrapping_add(i * self.auto_increment as u16)
                        as usize;
                    self.vram[addr ^ 1] = (fill_value >> 8) as u8;
                }
            }
            _ => unimplemented!("DMA Address mode {:?}", self.dma_target.1),
        }
    }

    pub fn vram_copy(&mut self) {
        println!(
            "Doing VRAM copy from {:#X} to {:#X} of length {:#X}",
            self.dma_source, self.dma_target.0, self.dma_length
        );
        for i in 0..self.dma_length as usize {
            self.vram[self.dma_target.0 as usize + i] = self.vram[self.dma_source as usize + i];
        }
    }

    // TODO: See the other one below!!
    #[allow(clippy::redundant_closure_call)]
    pub fn set_internal_reg(&mut self, value: u8, index: usize) {
        match index {
            0x00 => self.mode1.0 = value,
            0x01 => self.mode2.0 = value,
            0x02 => self.plane_a_nametable = value as u16 * 0x0400,
            0x03 => self.window_nametable = value as u16 * 0x400,
            0x04 => self.plane_b_nametable = value as u16 * 0x2000,
            0x05 => self.sprite_table_addr = value as u16 * 0x200,
            0x06 => assert_eq!(value, 0, "Nonzero bit 16 of sprite table address"),
            0x07 => self.bg_color = value,
            0x08 => {
                if value != 0 {
                    println!(
                        "Ignoring non-zero master system horizontal scroll write {:#X}",
                        value
                    )
                }
            }
            0x09 => {
                if value != 0 {
                    println!(
                        "Ignoring non-zero master system vertical scroll write {:#X}",
                        value
                    )
                }
            }
            0x0A => self.horiz_int_period = value,
            0x0B => println!("Ignoring write of {:#X} to VDP mode register 3", value),
            0x0C => println!("Ignoring write of {:#X} to VDP mode register 4", value),
            0x0D => self.horiz_scroll_addr = value as u16 * 0x400,
            0x0E => assert_eq!(value, 0, "Nonzero bit 16"),
            0x0F => {
                self.auto_increment = value;
                println!("Auto incr is now {:#X}", self.auto_increment)
            }
            0x10 => {
                // 00 =>  256
                // 01 =>  512
                // 11 => 1024

                let decode_size = |bits: u8| -> u16 {
                    match bits {
                        0b00 => 256,
                        0b01 => 512,
                        0b11 => 1024,
                        0b10 => panic!("Invalid plane size"),
                        _ => unreachable!(),
                    }
                };

                // TODO: Why is this lint firing??
                self.plane_width = decode_size(value & 0x3);
                self.plane_height = decode_size((value >> 4) & 0x3);
            }
            0x11 => assert_eq!(value, 0, "Nonzero window plane horizontal position"),
            0x12 => assert_eq!(value, 0, "Nonzero window plane vertical position"),
            0x13 => {
                self.dma_length &= 0xFF_00;
                self.dma_length |= value as u16;
            }
            0x14 => {
                self.dma_length &= 0x00_FF;
                self.dma_length |= (value as u16) << 8;
            }
            0x15 => {
                // Low byte
                self.dma_source &= 0xFF_FF_00;
                self.dma_source |= value as u32 * 2;
            }
            0x16 => {
                // Middle byte
                self.dma_source &= 0xFF_00_FF;
                self.dma_source |= (value as u32 * 2) << 8;
            }
            0x17 => {
                // High byte, DMA type
                self.dma_mode = match value >> 6 {
                    0b00 | 0b01 => DmaMode::CpuCopy,
                    0b10 => DmaMode::VramFill,
                    0b11 => DmaMode::VramCopy,
                    _ => unreachable!(),
                };

                let high_byte_mask = if self.dma_mode == DmaMode::CpuCopy {
                    0x7F
                } else {
                    0x3F
                };

                self.dma_source &= 0x00_FF_FF;
                self.dma_source |= (value as u32 & high_byte_mask) << 16;
                println!("DMA source is now {:#X}", self.dma_target.0);
            }
            _ => {
                unimplemented!("Ignoring write of {:#X} to register {:#X}", value, index);
            }
        }
    }

    // Returns address and CD value
    // Format for an address write is:
    // CD1 CD0 A13 A12 A11 A10 A09 A08
    // A07 A06 A05 A04 A03 A02 A01 A00
    // ___ ___ ___ ___ ___ ___ ___ ___
    // CD5 CD4 CD3 CD2 ___ ___ A15 A14
    pub fn parse_control_write(&mut self, value: u32) -> (u16, u8) {
        let bytes = value.to_be_bytes();

        assert_eq!(bytes[2], 0);
        assert_eq!(bytes[3] & 0b1100, 0);

        let address = ((value as u16 & 0b11) << 14)
            | ((bytes[0] as u16 & 0b111_111) << 8)
            | (bytes[1] as u16);

        let cd = ((bytes[3] >> 2) & 0b111_100) | (bytes[0] >> 6);

        (address, cd)
    }

    fn write_data_reg(&mut self, value: u16) {
        if self.dma_mode == DmaMode::VramFill && self.fill_pending {
            self.fill_pending = false;
            self.dma_fill(value);
        } else {
            let (address, is_write, ram_type) = self.ram_address;
            if is_write {
                match ram_type {
                    RamMode::VRam => {
                        self.vram[address as usize..][0..2].copy_from_slice(&value.to_be_bytes())
                    }
                    RamMode::CRam => self.cram[address as usize / 2].0 = value,
                    RamMode::VSRam => {
                        let address = (address & 0x4F) / 2;
                        let value = value & 0x3FF;

                        self.vsram[address as usize] = value;
                    }
                }
            } else {
                panic!("Ignoring write of {:#X} to data register", value);
            }

            self.ram_address.0 = self.ram_address.0.wrapping_add(self.auto_increment as u16);
        }
    }

    pub fn command_write(&mut self, value: u32, cpu: &mut Cpu) {
        // Separate the CD and the address bits
        let (address, cd) = self.parse_control_write(value);

        let (cd_type, ram_mode) = decode_cd(cd as u8);

        self.dma_target = (address, ram_mode);

        match cd_type {
            CDMode::Normal(is_write) => {
                self.ram_address = (address, is_write, ram_mode);
                println!(
                    "RAM address is now {:#X}, is_write: {}, mode: {:?}",
                    self.ram_address.0, self.ram_address.1, self.ram_address.2,
                );
            }
            CDMode::CpuCopy => {
                // TODO: Why????
                if self.dma_source >> 16 == 0x7F {
                    self.dma_source |= 0x80_00_00
                }
                // TODO: This may not be correct
                match self.dma_target {
                    (addr, RamMode::VRam) => {
                        println!("CPU to VRAM DMA with length {:#X}", self.dma_length);
                        for i in 0..self.dma_length {
                            self.vram[(addr + i) as usize] =
                                cpu.read(self.dma_source + i as u32, Size::Byte) as u8;
                        }
                    }
                    (addr, RamMode::CRam) => {
                        println!("CPU to CRAM DMA with length {:#X}", self.dma_length);
                        // TODO: Am I supposed to mask the DMA length like this...?
                        for i in 0..((self.dma_length & 0xFF) / 2) {
                            // TODO: This may be somewhat off, also CRAM and VSRAM uses word wide
                            // reads I believe
                            self.cram[(addr / 2 + i) as usize].0 =
                                cpu.read(self.dma_source + i as u32 * 2, Size::Word) as u16;
                        }
                    }
                    _ => todo!("CPU to VDP destination {:?}", self.dma_target),
                }
            }
            CDMode::VramCopy => {
                self.vram_copy();
            }
            CDMode::VramFill => {
                self.fill_pending = true;
            }
        }
    }

    fn write_word(&mut self, addr: u32, value: u16, cpu: &mut Cpu) {
        // TODO: Emulate this properly
        let reg = Register::decode(addr & !0x1).unwrap();

        // 68k to VDP => Writing command word:
        // CD1 CD0 _ _ _ _ _ _
        // ...
        // ...
        // 1 0 0 CD2 _ _ _ _
        //
        // VRAM Fill => Requires source register T1 T0 == 1 0
        // and command word:
        // 0 1 _ _ _ _ _ _
        // ...
        // ...
        // 1 0 0 0 _ _ _ _

        match reg {
            Register::Data => {
                self.write_data_reg(value);
                self.write_pending = None;
            }
            Register::StatusControl => {
                if let Some(first_half) = self.write_pending {
                    let value = ((first_half as u32) << 16) | value as u32;
                    self.command_write(value, cpu);
                    self.write_pending = None;
                } else if bitpat!(1 0)(value >> 14) {
                    let reg_idx = (value >> 8) & 0b11111;
                    let reg_val = value as u8;
                    self.set_internal_reg(reg_val, reg_idx as usize);
                } else {
                    self.write_pending = Some(value);
                }
            }
            _ => {
                println!("Ignoring write to {:?}", reg);
            }
        }
    }

    pub fn write(&mut self, addr: u32, mut value: u32, size: Size, cpu: &mut Cpu) {
        if size == Size::Long {
            self.write_word(addr, (value >> 16) as u16, cpu);
            self.write_word(addr, value as u16, cpu);
            return;
        }

        if size == Size::Byte {
            value |= value << 8;
        }

        self.write_word(addr, value as u16, cpu)
    }
}
