pub mod cpu;
mod inner;
pub mod psg;
pub mod ym3438;

use crate::controller::Controller;
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use crate::vdp::Vdp;
use crate::z80::Z80;
use log::{info, warn};
use once_cell::sync::OnceCell;
use psg::Psg;

pub static mut SYSTEM_STATE: OnceCell<SystemState> = OnceCell::new();
pub static mut VDP: OnceCell<Vdp> = OnceCell::new();
pub static mut Z80: OnceCell<Z80> = OnceCell::new();
pub static mut PSG: OnceCell<Psg> = OnceCell::new();

pub struct SystemState {
    pub rom: Box<[u8]>,
    pub cart_ram: Box<[u8]>,
    pub ram: Box<[u8]>,
    pub controller_1: Controller,
    pub controller_2: Controller,
}

impl AddressSpace for SystemState {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        if size == Size::Long && addr == 0xFFFF_FFFC {
            info!("RETURNING 'init'");
            return u32::from_be_bytes(*b"init");
        }

        match addr {
            0xA1_0000..=0xA1_0001 => {
                // TODO: Is this correct?
                return u32::from_be_bytes(*b"UEUE") & size.mask();
            }
            0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                return self.controller_1.read_reg1() as u32;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                return self.controller_2.read_reg1() as u32;
            }
            0xA1_1100..=0xA1_1101 => {
                return !(unsafe { Z80.get() }.unwrap().stopped) as u32;
            }
            0xC0_0000..=0xC0_000F => {
                return unsafe { VDP.get_mut() }.unwrap().read(addr as u32) as u32;
            }
            _ => {}
        }

        let mut temp = [0];

        let loc = match addr {
            0..=0x3F_FFFF if addr < self.rom.len() => &self.rom[addr..],
            0..=0x3F_FFFF => &self.cart_ram[addr - self.rom.len()..],
            0xA0_0000..=0xA0_FFFF => {
                temp[0] = unsafe { Z80.get_mut() }
                    .unwrap()
                    .read_ext(addr as u16, self);
                &temp
            }
            0xFF_0000..=0xFF_FFFF => &self.ram[addr - 0xFF_0000..],
            _ => {
                warn!("Unimplemented address {:#08X}, reading zero", addr);
                &[0, 0, 0, 0]
            }
        };

        match size {
            Size::Byte => u8::from_be_bytes([loc[0]]) as u32,
            Size::Word => u16::from_be_bytes([loc[0], loc[1]]) as u32,
            Size::Long => u32::from_be_bytes([loc[0], loc[1], loc[2], loc[3]]) as u32,
        }
    }

    fn write(&mut self, address: u32, value: u32, size: Size) {
        let length = size.len();
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(
            addr & (align - 1),
            0,
            "Invalid unaligned write of size {:?} with addr: {:#08X}",
            size,
            addr
        );

        match addr {
            0xA0_0000..=0xA0_FFFF => {
                // TODO: Check if Z80 needs to be stopped
                assert_eq!(size, Size::Byte);
                unsafe {
                    Z80.get_mut()
                        .unwrap()
                        .write_ext(addr as u16, value as u8, self)
                }
                return;
            }
            0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                self.controller_1.write_reg1(value as u8);
                return;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                self.controller_2.write_reg1(value as u8);
                return;
            }
            0xA1_0008..=0xA1_0009 => {
                assert_eq!(size, Size::Byte);
                self.controller_1.write_reg2(value as u8);
                return;
            }
            0xA1_000A..=0xA1_000B => {
                assert_eq!(size, Size::Byte);
                self.controller_2.write_reg2(value as u8);
                return;
            }
            0xA1_0000..=0xA1_0002 | 0xA1_0005..=0xA1_000F => {
                // TODO:
                warn!("Ignoring write to some stuff: {:#08X}", addr);
                return;
            }
            0xA1_1100..=0xA1_1101 => {
                // TODO:
                match value {
                    0x0100 => unsafe { Z80.get_mut() }.unwrap().stopped = true,
                    0x0000 => unsafe { Z80.get_mut() }.unwrap().stopped = false,
                    _ => panic!("Invalid bus request write {:#X}", value),
                }
                return;
            }
            0xA1_1200 => {
                // TODO:
                warn!("Ignoring Z80 reset");
                return;
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO: is this correct?
                assert_eq!(size, Size::Byte);
                unsafe { PSG.get_mut() }.unwrap().write(value as u8);
                return;
            }
            0xA1_0020..=0xA1_10FF => {
                // Should we panic here, or not?
                warn!("Ignoring write to reserved memory");
                return;
            }
            0xA1_30F1..=0xA1_30F2 => {
                // TODO:
                warn!("Ignoring write of {:#X} to SRAM register", value);
                return;
            }
            0xC0_0000..=0xC0_000F => {
                unsafe { VDP.get_mut() }
                    .unwrap()
                    .write(addr as u32, value, size, self);
                return;
            }
            _ => {}
        }

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < self.rom.len() {
                info!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_2000).contains(&byte_addr) {
                unsafe { Z80.get_mut() }.unwrap().ram_mut()[byte_addr - 0xA0_0000] = *byte;
            } else {
                warn!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}
