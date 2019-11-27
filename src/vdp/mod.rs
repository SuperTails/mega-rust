use crate::sdl_system::SDLSystem;

/// The Video Display Processor (VDP) handles all of the 
/// rendering for the console. It has three types of RAM:
///
/// 64KiB VRAM: Used to store graphics data, i.e. nametable data, sprite data, and the horizontal
/// scroll
/// 128 bytes CRAM: Used to store 64 different colors (4 palettes of 16 colors each)
/// ?? VSRAM: Used to store vertical scroll data
///
/// Information from segaretro.org
pub struct Vdp {
    // True means write
    ram_address: (u16, bool, RamMode),
    mode1: VdpMode1,
    mode2: VdpMode2,

    dma_length: u16,
    dma_address: (u16, RamMode),
    dma_source: u32,
    dma_mode: DmaMode,

    plane_a_nametable: u16,
    plane_b_nametable: u16,

    auto_increment: u8,

    cycle: u64,

    // 64KiB
    vram: [u8; 0x1_00_00],
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

#[derive(Debug)]
enum Register {
    Data,
    StatusControl,
    HVCounter
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

#[derive(Debug)]
// The boolean represents whether it's a read or write:
// false is read, true is write
pub enum RamMode {
    VRam,
    CRam,
    VSRam,
}

#[derive(Debug)]
pub enum CDMode {
    Normal(bool, RamMode),
    Dma(RamMode),
}

fn decode_cd(cd: u8) -> CDMode {
    assert_eq!(cd & !0b111_111, 0, "CD: {:#b}", cd);

    if cd & (0b1 << 4) != 0 {
        unimplemented!("Bit CD4")
    } else if cd & (0b1 << 5) != 0 {
        match cd {
            0b100001 => CDMode::Dma(RamMode::VRam),
            0b100011 => CDMode::Dma(RamMode::CRam),
            0b100101 => CDMode::Dma(RamMode::VSRam),
            _ => panic!("Unknown or invalid cd: {:#b}", cd),
        }
    } else {
        match cd {
            0b0000 => CDMode::Normal(false, RamMode::VRam),
            0b0001 => CDMode::Normal(true, RamMode::VRam),
            0b1000 => CDMode::Normal(false, RamMode::CRam),
            0b0011 => CDMode::Normal(true, RamMode::CRam),
            0b0100 => CDMode::Normal(false, RamMode::VSRam),
            0b0101 => CDMode::Normal(true, RamMode::VSRam),
            _ => panic!("Unknown cd: {:#b}", cd),
        }
    }
}

impl Vdp {
    pub fn new() -> Vdp {
        Vdp {
            ram_address: (0, false, RamMode::VRam),
            mode1: VdpMode1(0),
            mode2: VdpMode2(0),
            dma_length: 0,
            dma_address: (0, RamMode::VRam),
            dma_mode: DmaMode::CpuCopy,
            plane_a_nametable: 0,
            plane_b_nametable: 0,
            dma_source: 0,
            cycle: 0,
            auto_increment: 0,
            vram: [0; 0x1_00_00],
        }
    }

    pub fn do_cycle(&mut self, sdl_system: &mut SDLSystem) {
        self.cycle += 1;
        if self.cycle >= 13423294 {
            self.cycle = 0;
            sdl_system.canvas().present();
        }
    }

    pub fn read(&mut self, addr: u32) -> u16 {
        let reg = Register::decode(addr).unwrap();

        println!("Returning 0 from read to {:?}", reg);

        0
    }

    pub fn write(&mut self, addr: u32, value: u32) {
        let reg = Register::decode(addr).unwrap();

        match reg {
            Register::Data => {
                if self.dma_mode == DmaMode::VramFill {
                    let fill_value = (value >> 8) as u8;
                    match self.dma_address.1 {
                        RamMode::VRam => {
                            for i in (self.dma_address.0)..(self.dma_address.0 + self.dma_length) {
                                self.vram[i as usize] = fill_value;
                            }
                        }
                        _ => unimplemented!("DMA Address mode {:?}", self.dma_address.1),
                    }
                } else {
                    println!("Ignoring write of {:#X} to data register", value);
                }
            }
            Register::StatusControl => {
                if bitpat!(1 0 0)(value >> 13) {
                    let reg_idx = (value >> 8) & 0b11111;
                    let reg_val = value as u8;
                    match reg_idx {
                        0x00 => self.mode1.0 = reg_val,
                        0x01 => self.mode2.0 = reg_val,
                        0x02 => self.plane_a_nametable = reg_val as u16 * 0x0400,
                        0x04 => self.plane_b_nametable = reg_val as u16 * 0x2000,
                        0x0F => self.auto_increment = reg_val,
                        0x13 => { self.dma_length &= 0xFF_00; self.dma_length |= reg_val as u16 }
                        0x14 => { self.dma_length &= 0x00_FF; self.dma_length |= (reg_val as u16) << 8; }
                        // TODO: Check if 0x15, 0x16, 0x17 are backwards
                        0x15 => {
                            // Low byte
                            self.dma_source &= 0xFF_FF_00;
                            self.dma_source |= (reg_val as u32 * 2) <<  0;
                        }
                        0x16 => {
                            // Middle byte
                            self.dma_source &= 0xFF_00_FF;
                            self.dma_source |= (reg_val as u32 * 2) <<  8;
                        }
                        0x17 => {
                            self.dma_source &= 0x00_FF_FF;

                            if reg_val & (1 << 7) != 0 {
                                if reg_val & (1 << 6) != 0 {
                                    self.dma_mode = DmaMode::VramCopy;
                                } else {
                                    self.dma_mode = DmaMode::VramFill;
                                }
                                self.dma_source |= (reg_val as u32 & 0x3F) << 16;
                            } else {
                                self.dma_mode = DmaMode::CpuCopy;
                                self.dma_source |= (reg_val as u32 & 0x7F) << 16;
                            }
                            println!("DMA Source is now {:#X}", self.dma_address.0);
                        }
                        _ => {
                            println!("Ignoring write of {:#X} to register {:#X}", reg_val, reg_idx);
                        }
                    }
                } else {
                    // TODO: Should these be swapped or not?
                    let lo_word = (value >> 16) as u16;
                    let hi_word = (value >>  0) as u16;

                    let cd = (((hi_word >> 4) & 0xF) << 2) | (lo_word >> 14);
                    let mode = decode_cd(cd as u8);

                    let address = ((hi_word & 0x3) << 14) | (lo_word & 0x3F_FF);

                    match mode {
                        CDMode::Normal(is_write, ram_mode) => {
                            self.ram_address = (address, is_write, ram_mode);
                            println!("RAM address is now {:#X}, is_write: {}, mode: {:?}",
                                     self.ram_address.0,
                                     self.ram_address.1,
                                     self.ram_address.2,
                            );
                        }
                        CDMode::Dma(mode) => {
                            self.dma_address.1 = mode;
                            match self.dma_mode {
                                DmaMode::CpuCopy |
                                DmaMode::VramCopy => {
                                    unimplemented!(
                                        "DMA to ({:#X}, {:?}) with length {:#X} and mode {:?}",
                                        self.dma_address.0,
                                        self.dma_address.1,
                                        self.dma_length,
                                        self.dma_mode
                                    )
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }
            _ => {
                println!("Ignoring write to {:?}", reg);
            }
        }

    }
}
