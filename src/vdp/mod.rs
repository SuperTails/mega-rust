use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use crate::sdl_system::SDLSystem;
use crate::Interrupt;
use access_type::AccessType;
use bitfield::bitfield;
use bus::Bus;
use log::{info, warn};
use plane::{NormalPlane, Plane, WindowPlane};
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::rect::{Point, Rect};
use sdl2::render::Canvas;
use sdl2::surface::Surface;
use sprite_entry::SpriteEntry;
use std::collections::BinaryHeap;
use std::convert::TryFrom;
use std::ops::{Deref, DerefMut};

mod access_type;
mod bus;
mod plane;
mod sprite_entry;

fn swatch_from_pattern(pattern: &[u8], pixel_row: u16, pixel_col: u16) -> u8 {
    assert!(pixel_col < 8);
    assert!(pixel_row < 8);

    const BYTES_PER_ROW: usize = 4;

    let data_row = &pattern[pixel_row as usize * BYTES_PER_ROW..][0..BYTES_PER_ROW];
    let pixel_byte = data_row[pixel_col as usize / 2];
    if pixel_col % 2 == 0 {
        (pixel_byte >> 4) & 0xF
    } else {
        pixel_byte & 0xF
    }
}

#[derive(Clone, Copy, Debug)]
enum PlaneSize {
    Small = 256,
    Medium = 512,
    Large = 1024,
}

impl PlaneSize {
    pub fn from_bits(bits: u8) -> PlaneSize {
        match bits {
            0b00 => PlaneSize::Small,
            0b01 => PlaneSize::Medium,
            0b11 => PlaneSize::Large,
            _ => panic!("Invalid plane size"),
        }
    }

    pub fn mask(self) -> u16 {
        self as u16 - 1
    }
}

fn set_pixel(target: &mut Canvas<Surface<'_>>, color: Color, point: Point) -> Result<(), String> {
    target.set_draw_color(color);
    target.draw_point(point)
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
    struct VdpMode3(u8);
    impl Debug;

    horiz_scroll_mode, _: 1, 0;

    vert_scroll_mode, _: 2;

    external_int, _: 3;
}

bitfield! {
    struct VdpMode4(u8);
    impl Debug;

    width_0, _: 0;
    width_1, _: 7;

    interlace_mode, _: 2, 1;

    shadow_highlight, _: 3;

    external_pixel_bus, _: 4;

    // TODO: Determine what these fields are
    hs, _: 5;
    vs, _: 6;
}

impl VdpMode4 {
    pub fn width(&self) -> u32 {
        assert_eq!(self.width_0(), self.width_1());
        if self.width_0() {
            320
        } else {
            256
        }
    }
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
    pub fn height(&self) -> u32 {
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
pub enum RamType {
    VRam,
    CRam,
    VSRam,
}

#[derive(Debug, Clone)]
struct VdpAddress {
    pub ram_type: RamType,
    pub address: u16,
}

/*
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
*/

enum HorizScrollMode {
    WholeScreen,
    Strips,
    Individual,
}

enum PlaneType {
    Window,
    B,
    A,
}

bitfield! {
    struct TileEntry(u16);

    tile_index, _: 10, 0;

    horiz_flip, _: 11;

    vert_flip, _: 12;

    palette_line, _: 14, 13;

    priority, _: 15;
}

pub struct VdpInner {
    // True means write
    ram_address: AccessType,
    mode1: VdpMode1,
    mode2: VdpMode2,
    mode3: VdpMode3,
    mode4: VdpMode4,

    target: Canvas<Surface<'static>>,
    debug_target: Canvas<Surface<'static>>,

    dma_length: u16,
    dma_target: (u16, RamType),
    dma_source: u32,
    dma_mode: DmaMode,
    fill_pending: bool,

    window: WindowPlane,
    plane_a: NormalPlane,
    plane_b: NormalPlane,

    sprite_table_addr: u16,

    horiz_scroll_addr: u16,

    plane_height: PlaneSize,
    plane_width: PlaneSize,

    bg_color: u8,
    horiz_int_period: u8,
    horiz_int_counter: u8,

    auto_increment: u8,

    cycle: u64,
    scanline: u32,

    /// The array representing the VDP's 64KiB of VRAM.
    /// VRAM is used for storing graphics data, e.g.
    /// nametable data, sprite data, and the horizontal scroll
    vram: [u8; 0x1_00_00],

    /// The array representing the VDP's 128 bytes of CRAM, used for 64 different colors.
    cram: [CRamEntry; 0x40],

    // 40 10-bit entries
    vsram: [u16; 40],

    debug_render: bool,
}

impl VdpInner {
    pub fn new(debug_render: bool) -> VdpInner {
        let mut vram = [0; 0x1_00_00];
        for (i, e) in vram.iter_mut().enumerate() {
            *e = i as u8;
        }

        VdpInner {
            ram_address: AccessType::new(),
            mode1: VdpMode1(0),
            mode2: VdpMode2(0),
            mode3: VdpMode3(0),
            mode4: VdpMode4(0),
            target: Surface::new(320, 224, PixelFormatEnum::RGBA8888)
                .unwrap()
                .into_canvas()
                .unwrap(),
            debug_target: Surface::new(256, 256, PixelFormatEnum::RGBA8888)
                .unwrap()
                .into_canvas()
                .unwrap(),
            dma_length: 0,
            dma_target: (0, RamType::VRam),
            dma_mode: DmaMode::VramFill,
            fill_pending: false,
            plane_a: NormalPlane::new_a(),
            plane_b: NormalPlane::new_b(),
            window: Default::default(),
            sprite_table_addr: 0,
            horiz_scroll_addr: 0,
            plane_width: PlaneSize::Small,
            plane_height: PlaneSize::Small,
            bg_color: 0,
            scanline: 0,
            horiz_int_period: 0,
            horiz_int_counter: 0,
            dma_source: 0,
            cycle: 0,
            auto_increment: 0,
            vram,
            cram: [CRamEntry(0); 0x40],
            vsram: [0; 40],
            debug_render,
        }
    }

    fn horiz_scroll_mode(&self) -> HorizScrollMode {
        match self.mode3.horiz_scroll_mode() {
            0b00 => HorizScrollMode::WholeScreen,
            0b01 => panic!("Invalid scroll mode"),
            0b10 => HorizScrollMode::Strips,
            0b11 => HorizScrollMode::Individual,
            _ => unreachable!(),
        }
    }

    /// B is false, A is true
    fn get_vert_scroll(&self, col: usize, foreground: bool) -> u16 {
        let pair = if self.mode3.vert_scroll_mode() {
            // 16 pixel columns
            (self.vsram[(col / 16) * 2], self.vsram[(col / 16) * 2 + 1])
        } else {
            // Whole screen
            (self.vsram[0], self.vsram[1])
        };

        if foreground {
            pair.0
        } else {
            pair.1
        }
    }

    fn get_horiz_scroll(&self, row: usize, foreground: bool) -> u16 {
        let word_idx = match self.horiz_scroll_mode() {
            HorizScrollMode::WholeScreen => 0,
            HorizScrollMode::Strips => row & !0xF,
            HorizScrollMode::Individual => row,
        };

        let pair = &self.vram[self.horiz_scroll_addr as usize + word_idx * 4..][..4];

        if foreground {
            ((pair[0] as u16) << 8) | pair[1] as u16
        } else {
            ((pair[2] as u16) << 8) | pair[3] as u16
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

    fn render_sprites(&mut self) {
        let sprite_table = &self.vram[self.sprite_table_addr as usize..][..640];
        // TODO: Emulate self-referencing sprites correctly
        let mut visited = [false; 80];
        visited[0] = true;

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

                            let pixel_y = y as u16 % 8;
                            let pixel_x = x as u16 % 8;

                            let pattern_addr =
                                (sprite.pattern_start_index + offset).wrapping_mul(0x20);

                            let swatch = swatch_from_pattern(
                                &self.vram[pattern_addr as usize..],
                                pixel_y,
                                pixel_x,
                            );
                            if swatch != 0 {
                                let color = self.get_color(swatch, sprite.palette_line);
                                let dest_x = if sprite.horizontal_flip {
                                    sprite.width * 8 - 1 - x
                                } else {
                                    x
                                };
                                let dest_y = if sprite.vertical_flip {
                                    sprite.height * 8 - 1 - y
                                } else {
                                    y
                                };

                                let point =
                                    Point::new(corner_x + dest_x as i32, corner_y + dest_y as i32);

                                set_pixel(&mut self.target, color.into(), point).unwrap();
                                //self.set_output_pixel(sdl_system, color.into(), point).unwrap();
                            }
                        }
                    }

                    offset += 1;
                }
            }

            if sprite.link as usize >= 80 {
                warn!("OUT OF BOUNDS SPRITE");
                break;
            }

            if sprite.link == 0 || visited[sprite.link as usize] {
                break;
            }

            visited[sprite.link as usize] = true;

            sprite = sprite_table[sprite.link as usize * 8..][..8].into();
        }
    }

    fn get_pattern_color(
        &self,
        tile_address: u16,
        pixel_row: u16,
        pixel_col: u16,
    ) -> Option<(u8, u8, u8)> {
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

    fn plane_pixel_color(&self, r: u16, c: u16, plane: PlaneType) -> Option<(u8, u8, u8)> {
        match plane {
            PlaneType::Window => self.window.pixel_color(r, c, self),
            PlaneType::B => self.plane_b.pixel_color(r, c, self),
            PlaneType::A => self.plane_a.pixel_color(r, c, self),
        }
   }

    fn render_plane_pixel(&mut self, r: u16, c: u16) {
        // Ordering, from front to back:
        // High Priority Sprites
        // High Priority Plane A
        // High Priority Plane B
        // Low Priority Sprites
        // Low Priority Plane A
        // Low Priority Plane B
        // Backdrop Color

        let overall_color = self
            .plane_pixel_color(r, c, PlaneType::A)
            .or_else(|| self.plane_pixel_color(r, c, PlaneType::B))
            .or_else(|| self.plane_pixel_color(r, c, PlaneType::Window))
            .unwrap_or_else(|| {
                let bg_color = self.cram[self.bg_color as usize];
                (
                    (bg_color.red() as u8) << 5,
                    (bg_color.green() as u8) << 5,
                    (bg_color.blue() as u8) << 5,
                )
            });

        set_pixel(
            &mut self.target,
            overall_color.into(),
            Point::new(c as i32, r as i32),
        )
        .unwrap();
    }

    fn render_planes(&mut self) {
        for r in 0..self.mode2.height() as u16 {
            for c in 0..self.mode4.width() as u16 {
                self.render_plane_pixel(r, c);
            }
        }
    }

    fn render_vram(&mut self) {
        for r in 0..256 {
            for c in 0..256 {
                let entry = self.vram[r * 256 + c];

                let color = Color::RGB(entry, entry, entry);
                set_pixel(
                    &mut self.debug_target,
                    color,
                    Point::new(c as i32, r as i32),
                )
                .unwrap();
            }
        }

        let tables = [
            (Color::RGB(255, 0, 0), self.plane_a.nametable),
            (Color::RGB(0, 255, 0), self.plane_b.nametable),
            (Color::RGB(0, 0, 255), self.window.nametable),
        ];

        for (color, table) in tables.iter().copied() {
            let r = table / 256;
            let c = table % 256;

            set_pixel(
                &mut self.debug_target,
                color,
                Point::new(c as i32, r as i32),
            )
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
                .draw_point(Point::new(
                    (p % 16) as i32 + 256 + 16 + self.mode4.width() as i32,
                    p as i32 / 16,
                ))
                .unwrap();
        }
    }

    fn render_targets(&self, sdl_system: &mut SDLSystem) {
        let creator = sdl_system.canvas.texture_creator();
        let tex = creator
            .create_texture_from_surface(self.target.surface())
            .unwrap();
        let dest = Rect::new(
            0,
            0,
            self.target.surface().width(),
            self.target.surface().height(),
        );
        sdl_system.canvas.copy(&tex, None, dest).unwrap();
 
        if self.debug_render {
            let tex2 = creator
                .create_texture_from_surface(self.debug_target.surface())
                .unwrap();
            let dest2 = Rect::new(
                8 + self.mode4.width() as i32,
                0,
                self.debug_target.surface().width(),
                self.debug_target.surface().height(),
            );
            sdl_system.canvas.copy(&tex2, None, dest2).unwrap();
        }
    }

    fn render(&mut self, sdl_system: &mut SDLSystem) {
        self.render_planes();
        self.render_sprites();
        self.render_targets(sdl_system);
        if self.debug_render {
            self.render_vram();
            self.render_cram(sdl_system);
        }
        sdl_system.canvas().present();
    }

    pub fn do_cycle2(&mut self) -> (bool, bool) {
        self.cycle += 1;

        // TODO: It should be 488.5 I think,
        // and the CPU definitely does *not* take only one cycle to execute each instruction
        //
        // TODO: But it only has a 342 pixel range!
        let result_1 = if self.cycle % 488 == 0 {
            // One scanline has finished
            self.scanline += 1;
            if self.horiz_int_counter == 0 {
                self.horiz_int_counter = self.horiz_int_period;
            } else {
                self.horiz_int_counter -= 1;
            }

            true
        } else {
            false
        };

        /*      Horizontal Size
           13 left border + 14 right border

        */

        // TODO: Support PAL
        /*      Vertical Size
            PAL         |   NTSC
        38 border       | 11 border
        224 picture     | 224 picture
        32 border       | 8 border
        3 + 3 + 3 sync  | 3 + 3 + 3 sync
        10 blank        | 10 blank
        313 TOTAL       | 262 TOTAL
         */

        /*if 11 <= self.scanline && self.scanline < 224 + 11
            && 13 <= self.get_pixel() && self.get_pixel() < 13 + self.mode4.width() as u16 {
            let row = self.scanline as u16 - 11;
            let col = self.get_pixel() as u16 - 13;
            self.render_plane_pixel(sdl_system, row, col);
        }*/

        let result_2 = if self.scanline == 262 {
            // One frame has finished
            self.scanline = 0;

            warn!("Would have rendered");

            true
        } else {
            false
        };

        (result_1, result_2)
    }

    // 262 line range
    // 342 pixel range
    pub fn do_cycle(
        &mut self,
        sdl_system: &mut SDLSystem,
        int: &mut BinaryHeap<Interrupt>,
    ) -> (bool, bool) {
        self.cycle += 1;

        // TODO: It should be 488.5 I think,
        // and the CPU definitely does *not* take only one cycle to execute each instruction
        //
        // TODO: But it only has a 342 pixel range!
        let result_1 = if self.cycle % 488 == 0 {
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
            *int = int
                .clone()
                .into_iter()
                .filter(|i| i != &Interrupt::Vertical)
                .collect();
        }

        /*      Horizontal Size
           13 left border + 14 right border

        */

        // TODO: Support PAL
        /*      Vertical Size
            PAL         |   NTSC
        38 border       | 11 border
        224 picture     | 224 picture
        32 border       | 8 border
        3 + 3 + 3 sync  | 3 + 3 + 3 sync
        10 blank        | 10 blank
        313 TOTAL       | 262 TOTAL
         */

        /*if 11 <= self.scanline && self.scanline < 224 + 11
            && 13 <= self.get_pixel() && self.get_pixel() < 13 + self.mode4.width() as u16 {
            let row = self.scanline as u16 - 11;
            let col = self.get_pixel() as u16 - 13;
            self.render_plane_pixel(sdl_system, row, col);
        }*/

        let result_2 = if self.scanline == 262 {
            // One frame has finished
            self.scanline = 0;

            if self.mode2.vert_interrupt() && !int.iter().any(|i| i == &Interrupt::Vertical) {
                int.push(Interrupt::Vertical);
            }

            self.render(sdl_system);

            true
        } else {
            false
        };

        (result_1, result_2)
    }

    // TODO: Make this correct
    pub fn get_pixel(&self) -> u16 {
        // *roughly* maps 489 cycles to 342 pixels
        ((self.cycle % 488) * 703 / 1_000) as u16
    }

    pub fn get_hv_counter(&self) -> u16 {
        let pixel = self.get_pixel();

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
        // TODO: This may not be correct
        match self.dma_target {
            (addr, RamType::VRam) => {
                info!("CPU to VRAM DMA with length {:#X}", self.dma_length);
                for i in 0..self.dma_length {
                    self.vram[(addr + i) as usize] =
                        cpu.read(self.dma_source + i as u32, Size::Byte) as u8;
                }
            }
            (addr, RamType::CRam) => {
                info!("CPU to CRAM DMA with length {:#X}", self.dma_length);
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
            RamType::VRam => {
                info!("DMA Fill with target {:#X} and length {:#X} of value {:#X} and auto increment {:#X}",
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
        info!(
            "Doing VRAM copy from {:#X} to {:#X} of length {:#X}",
            self.dma_source, self.dma_target.0, self.dma_length
        );
        for i in 0..self.dma_length as usize {
            self.vram[self.dma_target.0 as usize + i] = self.vram[self.dma_source as usize + i];
        }
    }
}

impl Vdp {
    pub fn new(debug_render: bool) -> Vdp {
        Vdp {
            inner: VdpInner::new(debug_render),
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
