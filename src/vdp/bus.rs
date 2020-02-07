use crate::cpu::{instruction::Size, address_space::AddressSpace};
use super::{VdpInner, Register, DmaMode, decode_cd, CDMode, AccessType};
use bitpat::bitpat;

struct ControlWrite {
    pub address: u16,
    pub cd: u8,
}

// Returns address and CD value
// Format for an address write is:
// CD1 CD0 A13 A12 A11 A10 A09 A08
// A07 A06 A05 A04 A03 A02 A01 A00
// ___ ___ ___ ___ ___ ___ ___ ___
// CD5 CD4 CD3 CD2 ___ ___ A15 A14
fn parse_control_write(value: u32) -> ControlWrite {
    let bytes = value.to_be_bytes();

    let address = ((value as u16 & 0b11) << 14)
        | ((bytes[0] as u16 & 0b111_111) << 8)
        | (bytes[1] as u16);

    let cd = ((bytes[3] >> 2) & 0b111_100) | (bytes[0] >> 6);

    // I *believe* that the VDP simply ignores these bits
    //assert_eq!(bytes[2], 0, "Invalid control write: {:#X}, addr: {:#X}, CD: {:#X}", value, address, cd);
    //assert_eq!(bytes[3] & 0b1100, 0, "Invalid control write: {:#X}, addr: {:#X}, CD: {:#X}", value, address, cd);

    ControlWrite {
        address,
        cd,
    }
}

/// Handles interfacing between the VDP and the rest of the system
pub struct Bus {
    write_pending: Option<u16>,
}

impl Bus {
    pub fn new() -> Bus {
        Bus {
            write_pending: None
        }
    }

    // TODO: See the other one below!!
    #[allow(clippy::redundant_closure_call)]
    pub fn set_internal_reg(&mut self, value: u8, index: usize, vdp: &mut VdpInner) {
        match index {
            0x00 => vdp.mode1.0 = value,
            0x01 => vdp.mode2.0 = value,
            0x02 => vdp.plane_a_nametable = value as u16 * 0x0400,
            0x03 => vdp.window_nametable = value as u16 * 0x400,
            0x04 => vdp.plane_b_nametable = value as u16 * 0x2000,
            0x05 => vdp.sprite_table_addr = value as u16 * 0x200,
            0x06 => assert_eq!(value, 0, "Nonzero bit 16 of sprite table address"),
            0x07 => vdp.bg_color = value,
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
            0x0A => vdp.horiz_int_period = value,
            0x0B => println!("Ignoring write of {:#X} to VDP mode register 3", value),
            0x0C => println!("Ignoring write of {:#X} to VDP mode register 4", value),
            0x0D => vdp.horiz_scroll_addr = value as u16 * 0x400,
            0x0E => assert_eq!(value, 0, "Nonzero bit 16"),
            0x0F => {
                vdp.auto_increment = value;
                println!("Auto incr is now {:#X}", vdp.auto_increment)
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
                vdp.plane_width = decode_size(value & 0x3);
                vdp.plane_height = decode_size((value >> 4) & 0x3);
            }
            0x11 => {
                let direction = (value >> 7) & 1 != 0;
                // TODO:
                println!("IGNORING DIRECTION {}", direction);
                let offset = (value as u16 & 0x1F) * 8;
                vdp.window_horizontal = offset;
            }
            0x12 => {
                let direction = (value >> 7) & 1 != 0;
                // TODO: 
                println!("IGNORING DIRECTION {}", direction);
                let offset = (value as u16 & 0x1F) * 8;
                vdp.window_vertical = offset;
            }
            0x13 => {
                vdp.dma_length &= 0xFF_00;
                vdp.dma_length |= value as u16;
            }
            0x14 => {
                vdp.dma_length &= 0x00_FF;
                vdp.dma_length |= (value as u16) << 8;
            }
            0x15 => {
                // Low byte
                vdp.dma_source &= 0xFF_FF_00;
                vdp.dma_source |= value as u32 * 2;
            }
            0x16 => {
                // Middle byte
                vdp.dma_source &= 0xFF_00_FF;
                vdp.dma_source |= (value as u32 * 2) << 8;
            }
            0x17 => {
                // High byte, DMA type
                vdp.dma_mode = match value >> 6 {
                    0b00 | 0b01 => DmaMode::CpuCopy,
                    0b10 => DmaMode::VramFill,
                    0b11 => DmaMode::VramCopy,
                    _ => unreachable!(),
                };

                let high_byte_mask = if vdp.dma_mode == DmaMode::CpuCopy {
                    0x7F
                } else {
                    0x3F
                };

                vdp.dma_source &= 0x00_FF_FF;
                vdp.dma_source |= (value as u32 & high_byte_mask) << 16;
                println!("DMA source is now {:#X}", vdp.dma_target.0);
            }
            _ => {
                todo!(
                    "TODO: Ignoring write of {:#X} to register {:#X}",
                    value, index
                );
            }
        }
    }

    fn write_data_port(&mut self, value: u16, vdp: &mut VdpInner) {
        if vdp.dma_mode == DmaMode::VramFill && vdp.fill_pending {
            vdp.fill_pending = false;
            vdp.dma_fill(value);
        } else {
            vdp.ram_address.write(
                value,
                &mut vdp.vram[..],
                &mut vdp.cram[..],
                &mut vdp.vsram[..],
                vdp.auto_increment as u16
            );
        }
    }

    fn write_control_port(&mut self, value: u16, vdp: &mut VdpInner, cpu: &mut dyn AddressSpace) {
        if let Some(first_half) = self.write_pending {
            let value = ((first_half as u32) << 16) | value as u32;
            self.command_write(value, cpu, vdp);
            self.write_pending = None;
        } else if bitpat!(1 0)(value >> 14) {
            let reg_idx = (value >> 8) & 0b11111;
            let reg_val = value as u8;
            self.set_internal_reg(reg_val, reg_idx as usize, vdp);
        } else {
            self.write_pending = Some(value);
        }
    }

    fn command_write(&mut self, value: u32, cpu: &mut dyn AddressSpace, vdp: &mut VdpInner) {
        // Separate the CD and the address bits
        let ControlWrite { address, cd } = parse_control_write(value);

        let (cd_type, ram_type) = decode_cd(cd as u8);

        vdp.dma_target = (address, ram_type);

        match cd_type {
            CDMode::Normal(write) => {
                vdp.ram_address = AccessType{ address, write, ram_type };
                println!("RAM address is now {:#X?}", vdp.ram_address);
            }
            CDMode::CpuCopy => {
                vdp.cpu_copy(cpu);
            }
            CDMode::VramCopy => {
                vdp.vram_copy();
            }
            CDMode::VramFill => {
                vdp.fill_pending = true;
            }
        }
    }

    fn write_word(&mut self, addr: u32, value: u16, cpu: &mut dyn AddressSpace, vdp: &mut VdpInner) {
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
                self.write_data_port(value, vdp);
                self.write_pending = None;
            }
            Register::StatusControl => {
                self.write_control_port(value, vdp, cpu);
            }
            Register::HVCounter => {
                println!("Ignoring write of {:#X} to HV counter", value);
            }
        }

    }

    pub fn write(&mut self, addr: u32, mut value: u32, size: Size, cpu: &mut dyn AddressSpace, vdp: &mut VdpInner) {
        if size == Size::Long {
            self.write_word(addr, (value >> 16) as u16, cpu, vdp);
            self.write_word(addr, value as u16, cpu, vdp);
            return;
        }

        if size == Size::Byte {
            value |= value << 8;
        }

        self.write_word(addr, value as u16, cpu, vdp)
    }

    pub fn read(&mut self, addr: u32, vdp: &mut VdpInner) -> u16 {
        let reg = Register::decode(addr & !0x1).unwrap();

        self.write_pending = None;

        match reg {
            Register::StatusControl => vdp.get_status(),
            // TODO: The even byte should return the V counter,
            // and the odd byte shoudl return the H counter
            Register::HVCounter => vdp.get_hv_counter(),
            Register::Data => vdp.ram_address.read(&vdp.vram[..], &vdp.cram[..], &vdp.vsram[..], vdp.auto_increment as u16),
        }
    }
}