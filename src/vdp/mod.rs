use crate::cpu::instruction::Size;
use crate::cpu::address_space::AddressSpace;
use crate::sdl_system::SDLSystem;
use crate::Interrupt;
use bitfield::bitfield;
use sdl2::pixels::Color;
use sdl2::rect::Point;
use std::collections::BinaryHeap;
use bus::Bus;
use std::ops::{Deref, DerefMut};
use std::convert::TryFrom;

mod bus;

#[derive(Debug)]
struct AccessType {
    pub address: u16,
    pub write: bool,
    pub ram_type: RamMode,
}

impl AccessType {
    pub fn new() -> AccessType {
        AccessType {
            address: 0,
            write: false,
            ram_type: RamMode::VRam,
        }
    }

    pub fn write(&mut self, value: u16, vram: &mut [u8], cram: &mut [CRamEntry], vsram: &mut [u16], increment: u16) {
        if self.write {
            match self.ram_type {
                RamMode::VRam => {
                    vram[self.address as usize..][0..2].copy_from_slice(&value.to_be_bytes())
                }
                RamMode::CRam => cram[self.address as usize / 2].0 = value,
                RamMode::VSRam => {
                    let address = (self.address & 0x4F) / 2;
                    let value = value & 0x3FF;

                    vsram[address as usize] = value;
                }
            }
        } else {
            println!("Ignoring write of {:#X} to data register", value);
        }

        self.address = self.address.wrapping_add(increment);
    }

    pub fn read(&mut self, vram: &[u8], cram: &[CRamEntry], vsram: &[u16], increment: u16) -> u16 {
        let result = if !self.write {
            match self.ram_type {
                RamMode::VRam => {
                    ((vram[self.address as usize] as u16) << 8)
                        | vram[self.address as usize + 1] as u16
                }
                RamMode::CRam => cram[self.address as usize / 2].0,
                RamMode::VSRam => {
                    let address = (self.address & 0x4F) / 2;
                    vsram[address as usize] & 0x3FF
                }
            }
        } else {
            panic!("Attempt to read from VDP when in write mode")
        };

        self.address = self.address.wrapping_add(increment);

        result
    }
}

#[derive(Debug)]
struct SpriteEntry {
    y: u16,
    /// Width of the sprite, measured in cells
    width: u8,
    /// Height of the sprite, measured in cells
    height: u8,
    /// Index in the sprite attribute table of the next sprite, or zero if none
    link: u8,
    pattern_start_index: u16,
    horizontal_flip: bool,
    vertical_flip: bool,
    palette_line: u8,
    priority: bool,
    x: u16,
}

impl From<&[u8]> for SpriteEntry {
    fn from(data: &[u8]) -> SpriteEntry {
        assert_eq!(data.len(), 8);

        let mut d = [0; 8];
        d.copy_from_slice(data);
        d.into()
    }
}

impl From<[u8; 8]> for SpriteEntry {
    fn from(data: [u8; 8]) -> SpriteEntry {
        let y = (((data[0] as u16) << 8) | data[1] as u16) & 0x3FF; 
        let height = 1 + (data[2] & 0x3);
        let width = 1 + ((data[2] >> 2) & 0x3);
        let link = data[3] & 0x7F;
        let pattern_start_index = (((data[4] as u16) << 8) | data[5] as u16) & 0x7FF;
        let horizontal_flip = data[4] & (1 << 3) != 0;
        let vertical_flip = data[4] & (1 << 4) != 0;
        let palette_line = (data[4] >> 5) & 0x3;
        let priority = data[4] & (1 << 7) != 0;
        let x = (((data[6] as u16) << 8) | data[7] as u16) & 0x3FF;
        SpriteEntry {
            y,
            height,
            width,
            link,
            pattern_start_index,
            horizontal_flip,
            vertical_flip,
            palette_line,
            priority,
            x,
        }
    }
}

fn swatch_from_pattern(pattern: &[u8], pixel_row: u16, pixel_col: u16) -> u8 {
    assert!(pixel_col < 8);
    assert!(pixel_row < 8);

    const BYTES_PER_ROW: usize = 4;

    let data_row = &pattern[pixel_row as usize * BYTES_PER_ROW..][0..BYTES_PER_ROW];
    let pixel_byte = data_row[pixel_col as usize / 2];
    let shift = if pixel_col % 2 == 1 { 0 } else { 4 };
    (pixel_byte >> shift) & 0xF
}

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
pub struct VdpInner {
    // True means write
    ram_address: AccessType,
    mode1: VdpMode1,
    mode2: VdpMode2,

    dma_length: u16,
    dma_target: (u16, RamMode),
    dma_source: u32,
    dma_mode: DmaMode,
    fill_pending: bool,

    window_horizontal: u16,
    window_vertical: u16,

    /// Address in VRAM of the beginning of the nametable
    /// for plane A.
    plane_a_nametable: u16,
    /// Address in VRAM of the beginning of the nametable
    /// for plane B.
    plane_b_nametable: u16,
    /// Address in VRAM of the beginning of the nametable
    /// for the background/window.
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

    /// The array representing the VDP's 64KiB of VRAM.
    /// VRAM is used for storing graphics data, e.g.
    /// nametable data, sprite data, and the horizontal scroll
    vram: [u8; 0x1_00_00],

    /// The array representing the VDP's 128 bytes of CRAM, used for 64 different colors.
    cram: [CRamEntry; 0x40],

    // 40 10-bit entries
    vsram: [u16; 40],
}

pub struct Vdp {
    inner: VdpInner,
    bus: Bus,
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

impl VdpInner {
    pub fn new() -> VdpInner {
        let mut vram = [0; 0x1_00_00];
        for (i, e) in vram.iter_mut().enumerate() {
            *e = i as u8;
        }

        VdpInner {
            ram_address: AccessType::new(),
            mode1: VdpMode1(0),
            mode2: VdpMode2(0),
            dma_length: 0,
            dma_target: (0, RamMode::VRam),
            dma_mode: DmaMode::VramFill,
            fill_pending: false,
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
            window_horizontal: 0,
            window_vertical: 0,
            dma_source: 0,
            cycle: 0,
            auto_increment: 0,
            vram,
            cram: [CRamEntry(0); 0x40],
            vsram: [0; 40],
        }
    }

    fn get_tile_entry(&self, addr: u16) -> TileEntry {
        let entry = ((self.vram[addr as usize] as u16) << 8) | self.vram[addr as usize + 1] as u16;
        TileEntry(entry)
    }

    fn get_color(&self, swatch: u8, palette_line: u8) -> (u8, u8, u8) {
        let color = self.cram[palette_line as usize * 0x10 + swatch as usize];
        (
            (color.red() as u8) << 5,
            (color.green() as u8) << 5,
            (color.blue() as u8) << 5,
        )
    }

    fn render_sprites(&self, sdl_system: &mut SDLSystem) {
        let sprite_table = &self.vram[self.sprite_table_addr as usize..][..640];

        let mut sprite: SpriteEntry = sprite_table[..8].into();
        loop {
            let corner_x = sprite.x as i32 - 128;
            let corner_y = sprite.y as i32 - 128;
            let mut offset = 0;
            for tile_x in 0..sprite.width {
                for tile_y in 0..sprite.height {
                    for inner_x in 0..8 {
                        for inner_y in 0..8 {
                            let x = tile_x * 8 + inner_x;
                            let y = tile_y * 8 + inner_y;

                            let pattern_addr = (sprite.pattern_start_index + offset) * 0x20;

                            let swatch = swatch_from_pattern(&self.vram[pattern_addr as usize..], y as u16 % 8, x as u16 % 8);
                            if swatch != 0 {
                                let color = self.get_color(swatch, sprite.palette_line);
                                let point = Point::new(corner_x + x as i32, corner_y + y as i32);

                                sdl_system.canvas.set_draw_color(color);
                                sdl_system.canvas.draw_point(point).unwrap();
                            }
                        }
                    }

                    offset += 1;
                }
            }

            if sprite.link == 0 {
                break;
            }

            sprite = sprite_table[sprite.link as usize * 8..][..8].into();
        }
    }

    fn get_pattern_color(&self, tile_address: u16, pixel_row: u16, pixel_col: u16) -> Option<(u8, u8, u8)> {
        let tile_entry = self.get_tile_entry(tile_address);

        let tile_addr = tile_entry.tile_index() as usize * 0x20;
        let palette_line = tile_entry.palette_line();

        let tile_pattern = &self.vram[tile_addr..][0..0x20];

        let data = swatch_from_pattern(tile_pattern, pixel_row, pixel_col);

        if data == 0 {
            None
        } else {
            Some(self.get_color(data, palette_line as u8))
        }
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

                let cell = cell_row * plane_cell_width + cell_col;

                let get_color = |addr: u16, window: bool| -> Option<(u8, u8, u8)> {
                    let pixel_row = if window {
                        (row + self.window_vertical)
                    } else {
                        row
                    } % 8;

                    let pixel_col = if window {
                        (col + self.window_horizontal)
                    } else {
                        col
                    } % 8;

                    self.get_pattern_color(addr, pixel_row, pixel_col)
                };

                // Ordering, from front to back:
                // High Priority Sprites
                // High Priority Plane A
                // High Priority Plane B
                // Low Priority Sprites
                // Low Priority Plane A
                // Low Priority Plane B
                // Backdrop Color

                let addr_z = self.window_nametable.wrapping_add(cell * 2);
                let addr_a = self.plane_a_nametable.wrapping_add(cell * 2);
                let addr_b = self.plane_b_nametable.wrapping_add(cell * 2);

                let color_z = get_color(addr_z, true);
                let color_a = get_color(addr_a, false);
                let color_b = get_color(addr_b, false);

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
        self.render_sprites(sdl_system);

        sdl_system.canvas().set_scale(0.5, 0.5).unwrap();
        sdl_system.canvas().present();
    }

    // 262 line range
    // 342 pixel range
    pub fn do_cycle(
        &mut self,
        sdl_system: &mut SDLSystem,
        int: &mut BinaryHeap<Interrupt>,
    ) -> bool {
        self.cycle += 1;

        // TODO: It should be 488.5 I think,
        // and the CPU definitely does *not* take only one cycle to execute each instruction
        //
        // TODO: But it only has a 342 pixel range!
        let result = if self.cycle % 488 == 0 {
            // One scanline has finished
            self.scanline += 1;
            if self.horiz_int_counter == 0 {
                if self.mode1.horiz_interrupt() && !int.iter().any(|i| i == &Interrupt::Horizontal)
                {
                    int.push(Interrupt::Horizontal);
                }

                self.horiz_int_counter = self.horiz_int_period;
            } else {
                self.horiz_int_counter -= 1;
            }

            true
        } else {
            false
        };

        if self.scanline < 10 {
            *int = int.clone().into_iter().filter(|i| i != &Interrupt::Vertical).collect();
        }

        // TODO: This might not be correct
        if self.scanline == 262 {
            // One frame has finished
            self.scanline = 0;

            println!("Frame finished");

            if self.mode2.vert_interrupt() && !int.iter().any(|i| i == &Interrupt::Vertical) {
                int.push(Interrupt::Vertical);
            }

            self.render(sdl_system);
        }

        result
    }
    
    // TODO: Make this correct
    pub fn get_pixel(&self) -> u16 {
        // *roughly* maps 489 cycles to 342 pixels
        ((self.cycle % 488) * 703 / 1_000) as u16
    }

    pub fn get_hv_counter(&self) -> u16 {
        let pixel = self.get_pixel();

        println!("Cycle: {}", self.cycle);
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

    // TODO: Add vertical interrupt pending flag, and others
    pub fn get_status(&self) -> u16 {
        // TODO: Set the vblank bit at the proper part of the scan
        let vblank_bit = self.scanline > 0xE0 && self.scanline < 0xFF;
        let hblank_bit = self.get_pixel() >= 0xE4 || self.get_pixel() < 0x08;

        (1 << 9) | // Always show FIFO as being empty
            ((vblank_bit as u16) << 3) |
            ((hblank_bit as u16) << 2)
    }

    pub fn cpu_copy(&mut self, cpu: &mut dyn AddressSpace) {
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
                //for i in 0..((self.dma_length & 0xFF) / 2) {
                for i in 0..self.dma_length / 2 {
                    // TODO: This may be somewhat off, also CRAM and VSRAM uses word wide
                    // reads I believe
                    self.cram[(addr / 2 + i) as usize].0 =
                        cpu.read(self.dma_source + i as u32 * 2, Size::Word) as u16;
                }
            }
            _ => todo!("CPU to VDP destination {:?}", self.dma_target),
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
}

impl Vdp {
    pub fn new() -> Vdp {
        Vdp {
            inner: VdpInner::new(),
            bus: Bus::new(),
        }
    }

    pub fn read(&mut self, addr: u32) -> u16 {
        self.bus.read(addr, &mut self.inner)
    }

    pub fn write(&mut self, addr: u32, value: u32, size: Size, cpu: &mut dyn AddressSpace) {
        self.bus.write(addr, value, size, cpu, &mut self.inner);
    }
}

impl Deref for Vdp {
    type Target = VdpInner;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Vdp {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}