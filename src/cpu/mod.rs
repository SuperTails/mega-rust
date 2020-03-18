// TODO: Maybe remove this later?
#![allow(dead_code)]

pub mod address_space;
mod cpu_core;
pub mod instruction;

pub use cpu_core::*;

use crate::bindings::context::CpuView;
use crate::bindings::context::MemoryView;
use crate::get_four_bytes;
use crate::Interrupt;
use address_space::AddressSpace;
use instruction::{Instruction, Size};
use lazy_static::lazy_static;
use log::{info, trace, warn};
use std::sync::atomic::{AtomicBool, Ordering};

pub trait Cpu {
    fn step(&mut self, context: &mut CpuView);

    fn execute(&mut self, context: &mut CpuView) {
        self.step(context)
    }
}

lazy_static! {
    static ref LOG_INSTR: AtomicBool = AtomicBool::new(false);
}

pub fn do_log(do_l: bool) {
    LOG_INSTR.store(do_l, Ordering::SeqCst)
}

pub fn log_instr() -> bool {
    LOG_INSTR.load(Ordering::SeqCst)
}

#[derive(Debug, Clone, PartialEq)]
pub struct RustCpu {
    pub core: CpuCore,
}

impl RustCpu {
    pub fn new() -> RustCpu {
        RustCpu {
            core: CpuCore::new()
        }
    }

    fn with_context<'a, 'b>(&'a mut self, context: &'a mut CpuView<'b>) -> CpuAndContext<'a, 'b> {
        CpuAndContext {
            core: &mut self.core,
            context,
        }
    }

    fn do_run(&mut self, context: &mut CpuView) {
        self.handle_interrupts(context);

        assert_eq!(self.core.pc % 2, 0);

        let pc = self.core.pc as u32;
        let instr = self.instr_at(pc, context);

        if log_instr() {
            trace!("Instr: {:?}", instr.opcode);
        }

        let prev_sup = self.core.sr.supervisor();

        // TODO: Cycle counts
        instr.execute(&mut self.with_context(context));

        if prev_sup != self.core.sr.supervisor() {
            std::mem::swap(&mut self.core.addr[7], &mut self.core.usp);
        }
    }

    pub fn instr_at(&mut self, addr: u32, context: &mut CpuView) -> Instruction {
        Instruction::new(self.with_context(context).read(addr, Size::Word) as u16)
    }

    fn do_reset(&mut self, context: &mut CpuView) {
        assert_eq!(self.core.pc % 4, 0);
        assert!(self.core.pc < 0x100);

        let offset = self.core.pc as usize;
        let vector = u32::from_be_bytes(get_four_bytes(&context.rom[offset..offset + 4]));

        // TODO: THIS IS WRONG APPARENTLY
        let sp = u32::from_be_bytes(get_four_bytes(&context.rom[0..4]));

        info!(
            "Using vector at {:#08X}, PC will be {:#08X}",
            offset, vector
        );

        // TODO: Determine actual cycle count and behavior
        self.core.addr[7] = sp;
        self.core.pc = vector;
        self.core.cycle += 1;
        self.core.state = State::Run;
        self.core.sr.set_priority(7);
        self.core.ccr.set_zero(true);
    }

    fn on_external_int(&mut self, _context: &mut CpuView) {
        todo!("External interrupt")
    }

    fn on_horizontal_int(&mut self, context: &mut CpuView) {
        // TODO: This should use the SSP instead of A7 I think
        self.core.addr[7] = self.core.addr[7].wrapping_sub(4);
        self.write(context, self.core.addr[7], self.core.pc, Size::Long);
        self.core.addr[7] = self.core.addr[7].wrapping_sub(2);
        self.write(
            context,
            self.core.addr[7],
            ((self.core.sr.0 as u32) << 8) | self.core.ccr.0 as u32,
            Size::Word,
        );

        let vector = u32::from_be_bytes(get_four_bytes(&context.rom[0x70..][..4]));

        self.core.sr.set_priority(4);
        self.core.sr.set_supervisor(true);
        self.core.pc = vector;
        info!("Did horizontal interrupt");
    }

    fn write(&mut self, context: &mut CpuView, address: u32, value: u32, size: Size) {
        self.with_context(context).write(address, value, size)
    }

    fn on_vertical_int(&mut self, context: &mut CpuView) {
        // TODO: This should use the SSP instead of A7 I think
        self.core.addr[7] = self.core.addr[7].wrapping_sub(4);
        self.write(context, self.core.addr[7], self.core.pc, Size::Long);
        self.core.addr[7] = self.core.addr[7].wrapping_sub(2);
        self.write(
            context,
            self.core.addr[7],
            ((self.core.sr.0 as u32) << 8) | self.core.ccr.0 as u32,
            Size::Word,
        );

        let vector = u32::from_be_bytes(get_four_bytes(&context.rom[0x78..][..4]));

        self.core.sr.set_priority(6);
        self.core.sr.set_supervisor(true);
        self.core.pc = vector;
        info!("Did vertical interrupt");
    }

    fn handle_interrupts(&mut self, context: &mut CpuView) {
        if let Some(int) = context.pending.peek() {
            if *int as u8 > self.core.sr.priority() {
                let int = context.pending.pop().unwrap();
                match int {
                    Interrupt::External => {
                        self.on_external_int(context);
                    }
                    Interrupt::Horizontal => {
                        self.on_horizontal_int(context);
                    }
                    Interrupt::Vertical => {
                        self.on_vertical_int(context);
                    }
                }
            }
        }
    }
}

impl Cpu for RustCpu {
    fn step(&mut self, context: &mut CpuView) {
        match self.core.state {
            State::Reset => {
                self.do_reset(context);
            }
            State::Run => {
                self.do_run(context);
            }
        }
    }
}

pub struct CpuAndContext<'a, 'b> {
    core: &'a mut CpuCore,
    context: &'a mut CpuView<'b>,
}

impl AddressSpace for CpuAndContext<'_, '_> {
    fn read(&mut self, address: u32, size: Size) -> u32 {
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
                return self.context.controller_1.read_reg1() as u32;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                return self.context.controller_2.read_reg1() as u32;
            }
            0xA1_1100..=0xA1_1101 => {
                // TODO:
                warn!("Returning 0 from Z80 bus request");
                return 0;
            },
            0xC0_0000..=0xC0_000F => {
                return self.context.vdp.read(addr as u32) as u32;
            }
            0xFFFF_FFFC => {
                assert_eq!(size, Size::Long);
                info!("RETURNING 'init'");
                return u32::from_be_bytes(*b"init");
            }
            _ => {}
        }

        let rom_len = self.context.rom.len();

        let loc = match addr {
            0..=0x3F_FFFF if addr < rom_len => &self.context.rom[addr..],
            0..=0x3F_FFFF => &self.context.cart_ram[addr - rom_len..],
            0xA0_0000..=0xA0_FFFF => &self.context.z80.ram_mut()[addr - 0xA0_0000..],
            0xFF_0000..=0xFF_FFFF => &self.context.ram[addr - 0xFF_0000..],
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
           0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                self.context.controller_1.write_reg1(value as u8);
                return;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                self.context.controller_2.write_reg1(value as u8);
                return;
            }
            0xA1_0008..=0xA1_0009 => {
                assert_eq!(size, Size::Byte);
                self.context.controller_1.write_reg2(value as u8);
                return;
            }
            0xA1_000A..=0xA1_000B => {
                assert_eq!(size, Size::Byte);
                self.context.controller_2.write_reg2(value as u8);
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
                    0x0100 => self.context.z80.stopped = true,
                    0x0000 => self.context.z80.stopped = false,
                    _ => panic!("Invalid bus request write {:#X}", value),
                }
                return;
            }
            0xA1_1200 => {
                warn!("Ignoring Z80 reset");
                return;
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO: is this correct?
                assert_eq!(size, Size::Byte);
                self.context.psg.write_io(value as u8);
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
            0xA1_4000..=0xA1_4003 => {
                // TODO:
                info!("Ignoring TMSS register write of {:#X}", value);
                return;
            }
            0xC0_0000..=0xC0_000F => {
                let mut memory = MemoryView {
                    ram: self.context.ram,
                    cart_ram: self.context.cart_ram,
                    rom: self.context.rom,
                };
                self.context.vdp.write(addr as u32, value, size, &mut memory);
                return;
            }
            _ => {}
        }

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < self.context.rom.len() {
                info!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.context.cart_ram[byte_addr - self.context.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.context.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_2000).contains(&byte_addr) {
                self.context.z80.ram_mut()[byte_addr - 0xA0_0000] = *byte;
            } else {
                warn!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}