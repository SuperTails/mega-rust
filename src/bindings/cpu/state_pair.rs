use std::ptr::null_mut;
use super::MusashiCpu;
use super::super::context::CpuViewRaw;
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use log::{info, warn};

pub struct StatePair(pub *mut MusashiCpu, pub CpuViewRaw);

impl StatePair {
    pub fn read_immediate_16(&mut self, address: u32) -> u32 {
        let addr = (address & 0xFF_FF_FF) as usize;

        let rom_len = unsafe { (*self.1.rom).len() };

        let loc = unsafe { match addr {
            0..=0x3F_FFFF if addr < rom_len => &(*self.1.rom)[addr..],
            0..=0x3F_FFFF => &(*self.1.cart_ram)[addr - rom_len..],
            0xFF_0000..=0xFF_FFFF => &(*self.1.ram)[addr - 0xFF_0000..],
            _ => panic!("Invalid immediate address"),
        } };

        u16::from_be_bytes([loc[0], loc[1]]) as u32
    }

    pub fn read_immediate_32(&mut self, address: u32) -> u32 {
        let addr = (address & 0xFF_FF_FF) as usize;

        let rom_len = unsafe { (*self.1.rom).len() };

        let loc = unsafe { match addr {
            0..=0x3F_FFFF if addr < rom_len => &(*self.1.rom)[addr..],
            0..=0x3F_FFFF => &(*self.1.cart_ram)[addr - rom_len..],
            0xFF_0000..=0xFF_FFFF => &(*self.1.ram)[addr - 0xFF_0000..],
            _ => panic!("Invalid immediate address"),
        } };

        u32::from_be_bytes([loc[0], loc[1], loc[2], loc[3]]) as u32
    }
}

impl AddressSpace for StatePair {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let cpu = unsafe { &mut *self.0 };

        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        match addr {
            0xA1_0000..=0xA1_0001 => {
                // TODO: Is this correct?
                return u32::from_be_bytes(*b"UEUE") & size.mask();
            }
            0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                cpu.end_timeslice(&mut unsafe { self.1.as_safe() });
                return unsafe { (*self.1.controller_1).read_reg1() } as u32;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                cpu.end_timeslice(&mut unsafe { self.1.as_safe() });
                return unsafe { (*self.1.controller_2).read_reg1() } as u32;
            }
            0xA1_1100..=0xA1_1101 => {
                cpu.end_timeslice(&mut unsafe { self.1.as_safe() });
                return !(unsafe { (*self.1.z80).stopped }) as u32;
            }
            0xC0_0000..=0xC0_000F => {
                cpu.end_timeslice(&mut unsafe { self.1.as_safe() });
                return unsafe { (*self.1.vdp).read(addr as u32) } as u32;
            }
            0xFFFF_FFFC => {
                assert_eq!(size, Size::Long);
                info!("RETURNING 'init'");
                return u32::from_be_bytes(*b"init");
            }
            _ => {}
        }

        let mut temp = [0];
        let rom_len = unsafe { (*self.1.rom).len() };

        let loc = match addr {
            0..=0x3F_FFFF if addr < rom_len => unsafe { &(*self.1.rom)[addr..] },
            0..=0x3F_FFFF => unsafe { &(*self.1.cart_ram)[addr - rom_len..] },
            0xA0_0000..=0xA0_FFFF => {
                cpu.end_timeslice(&mut unsafe { self.1.as_safe() });
                temp[0] = unsafe { (*self.1.z80).read_ext(addr as u16, self) };
                &temp
            }
            0xFF_0000..=0xFF_FFFF => unsafe { &(*self.1.ram)[addr - 0xFF_0000..] },
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
        let cpu = unsafe { &mut *self.0 };
        let mut context = unsafe { self.1.as_safe() };

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
                cpu.end_timeslice(&mut context);
                context.z80.write_ext(addr as u16, value as u8, self);
                return;
            }
            0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                cpu.end_timeslice(&mut context);
                context.controller_1.write_reg1(value as u8);
                return;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                cpu.end_timeslice(&mut context);
                context.controller_2.write_reg1(value as u8);
                return;
            }
            0xA1_0008..=0xA1_0009 => {
                assert_eq!(size, Size::Byte);
                cpu.end_timeslice(&mut context);
                context.controller_1.write_reg2(value as u8);
                return;
            }
            0xA1_000A..=0xA1_000B => {
                assert_eq!(size, Size::Byte);
                cpu.end_timeslice(&mut context);
                context.controller_2.write_reg2(value as u8);
                return;
            }
            0xA1_0000..=0xA1_0002 | 0xA1_0005..=0xA1_000F => {
                // TODO:
                cpu.end_timeslice(&mut context);
                warn!("Ignoring write to some stuff: {:#08X}", addr);
                return;
            }
            0xA1_1100..=0xA1_1101 => {
                // TODO:
                cpu.end_timeslice(&mut context);
                match value {
                    0x0100 => context.z80.stopped = true,
                    0x0000 => context.z80.stopped = false,
                    _ => panic!("Invalid bus request write {:#X}", value),
                }
                return;
            }
            0xA1_1200 => {
                // TODO:
                cpu.end_timeslice(&mut context);
                warn!("Ignoring Z80 reset");
                return;
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO: is this correct?
                cpu.end_timeslice(&mut context);
                assert_eq!(size, Size::Byte);
                context.psg.write(value as u8);
                return;
            }
            0xA1_0020..=0xA1_10FF => {
                cpu.end_timeslice(&mut context);
                // Should we panic here, or not?
                warn!("Ignoring write to reserved memory");
                return;
            }
            0xA1_30F1..=0xA1_30F2 => {
                cpu.end_timeslice(&mut context);
                // TODO:
                warn!("Ignoring write of {:#X} to SRAM register", value);
                return;
            }
            0xC0_0000..=0xC0_000F => {
                cpu.end_timeslice(&mut context);
                context.vdp.write(addr as u32, value, size, self);
                return;
            }
            _ => {}
        }

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < context.rom.len() {
                info!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                context.cart_ram[byte_addr - context.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                context.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_2000).contains(&byte_addr) {
                cpu.end_timeslice(&mut context);
                context.z80.ram_mut()[byte_addr - 0xA0_0000] = *byte;
            } else {
                warn!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }

    }
}

/// These pointers are valid ***ONLY*** while `MusashiCpu::do_cycle` is running 
pub static mut TEMP_DATA: StatePair = StatePair(std::ptr::null_mut(), CpuViewRaw {
    vdp: null_mut(),
    z80: null_mut(),
    psg: null_mut(),
    controller_1: null_mut(),
    controller_2: null_mut(),
    rom: null_mut::<[u8; 0]>() as *mut [u8],
    cart_ram: null_mut::<[u8; 0]>() as *mut [u8],
    ram: null_mut::<[u8; 0]>() as *mut [u8],
});
