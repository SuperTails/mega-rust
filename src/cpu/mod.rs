// TODO: Maybe remove this later?
#![allow(dead_code)]

pub mod address_space;
mod cpu_core;
pub mod instruction;

pub use cpu_core::*;

use crate::controller::Controller;
use crate::get_four_bytes;
use crate::vdp::Vdp;
use crate::Interrupt;
use address_space::AddressSpace;
use instruction::{Instruction, Size};
use lazy_static::lazy_static;
use log::{info, trace, warn};
use std::collections::BinaryHeap;
use std::ops::Deref;
use std::ops::DerefMut;
use std::sync::atomic::{AtomicBool, Ordering};

lazy_static! {
    static ref LOG_INSTR: AtomicBool = AtomicBool::new(false);
}

pub fn do_log(do_l: bool) {
    LOG_INSTR.store(do_l, Ordering::SeqCst)
}

pub fn log_instr() -> bool {
    LOG_INSTR.load(Ordering::SeqCst)
}

pub struct Cpu {
    pub vdp: Vdp,
    no_vdp: CpuNoVdp,
}

impl Cpu {
    pub fn new(rom: &[u8], controller1: Controller, controller2: Controller, vdp_debug: bool) -> Cpu {
        Cpu {
            vdp: Vdp::new(vdp_debug),
            no_vdp: CpuNoVdp::new(rom, controller1, controller2),
        }
    }

    fn do_run(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        self.handle_interrupts(pending);

        assert_eq!(self.core.pc % 2, 0);

        let pc = self.core.pc as u32;
        let instr = self.instr_at(pc);

        if log_instr() {
            trace!("Instr: {:?}", instr.opcode);
        }

        let prev_sup = self.core.sr.supervisor();

        // TODO: Cycle counts
        instr.execute(self);

        if prev_sup != self.core.sr.supervisor() {
            std::mem::swap(&mut self.no_vdp.core.addr[7], &mut self.no_vdp.core.usp);
        }
    }

    // TODO: Access Z80, VDP, expansion ports, and IO registers
    pub fn do_cycle(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        match self.core.state {
            State::Reset => {
                self.do_reset();
            }
            State::Run => {
                self.do_run(pending);
            }
        }
    }
}

impl Deref for Cpu {
    type Target = CpuNoVdp;

    fn deref(&self) -> &Self::Target {
        &self.no_vdp
    }
}

impl DerefMut for Cpu {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.no_vdp
    }
}

pub struct CpuNoVdp {
    pub core: CpuCore,
    pub rom: Box<[u8]>,
    pub cart_ram: Box<[u8]>,
    pub ram: [u8; 0x1_0000],
    pub z80_ram: [u8; 0x1_0000],
    pub controller1: Controller,
    pub controller2: Controller,
}

impl CpuNoVdp {
    fn new(rom: &[u8], controller1: Controller, controller2: Controller) -> CpuNoVdp {
        let rom: Box<[u8]> = rom.into();
        let cart_ram = {
            let mut cart_ram = Vec::new();
            cart_ram.resize(0x40_0000 - rom.len(), 0);
            Box::from(&cart_ram[..])
        };

        CpuNoVdp {
            core: CpuCore::new(),
            rom,
            cart_ram,
            ram: [0; 0x1_0000],
            z80_ram: [0; 0x1_0000],
            controller1,
            controller2,
        }
    }

    pub fn instr_at(&mut self, addr: u32) -> Instruction {
        Instruction::new(self.read(addr, Size::Word) as u16)
    }

    fn do_reset(&mut self) {
        assert_eq!(self.core.pc % 4, 0);
        assert!(self.core.pc < 0x100);

        let offset = self.core.pc as usize;
        let vector = u32::from_be_bytes(get_four_bytes(&self.rom[offset..offset + 4]));
        let sp = u32::from_be_bytes(get_four_bytes(&self.rom[0..4]));

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

    fn on_external_int(&mut self) {
        todo!("External interrupt")
    }

    fn on_horizontal_int(&mut self) {
        // TODO: This should use the SSP instead of A7 I think
        self.core.addr[7] = self.core.addr[7].wrapping_sub(4);
        self.write(self.core.addr[7], self.core.pc, Size::Long);
        self.core.addr[7] = self.core.addr[7].wrapping_sub(2);
        self.write(
            self.core.addr[7],
            ((self.core.sr.0 as u32) << 8) | self.core.ccr.0 as u32,
            Size::Word,
        );

        let vector = u32::from_be_bytes(get_four_bytes(&self.rom[0x70..][..4]));

        self.core.sr.set_priority(4);
        self.core.sr.set_supervisor(true);
        self.core.pc = vector;
        info!("Did horizontal interrupt");
    }

    fn on_vertical_int(&mut self) {
        // TODO: This should use the SSP instead of A7 I think
        self.core.addr[7] = self.core.addr[7].wrapping_sub(4);
        self.write(self.core.addr[7], self.core.pc, Size::Long);
        self.core.addr[7] = self.core.addr[7].wrapping_sub(2);
        self.write(
            self.core.addr[7],
            ((self.core.sr.0 as u32) << 8) | self.core.ccr.0 as u32,
            Size::Word,
        );

        let vector = u32::from_be_bytes(get_four_bytes(&self.rom[0x78..][..4]));

        self.core.sr.set_priority(6);
        self.core.sr.set_supervisor(true);
        self.core.pc = vector;
        info!("Did vertical interrupt");
    }

    fn handle_interrupts(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        if let Some(int) = pending.peek() {
            if *int as u8 > self.core.sr.priority() {
                let int = pending.pop().unwrap();
                match int {
                    Interrupt::External => {
                        self.on_external_int();
                    }
                    Interrupt::Horizontal => {
                        self.on_horizontal_int();
                    }
                    Interrupt::Vertical => {
                        self.on_vertical_int();
                    }
                }
            }
        }
    }
}

impl AddressSpace for Cpu {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let align = size.alignment();

        let addr = address & 0xFF_FF_FF;
        assert_eq!(
            addr as usize & (align - 1),
            0,
            "Misaligned read: {:#X}",
            addr
        );

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            self.no_vdp.read(addr, size)
        } else {
            self.vdp.read(addr as u32) as u32
        }
    }

    fn write(&mut self, address: u32, value: u32, size: Size) {
        let align = size.alignment();

        let addr = address & 0xFF_FF_FF;
        assert_eq!(
            addr as usize & (align - 1),
            0,
            "Misaligned write: {:#X}",
            addr
        );

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            self.no_vdp.write(addr, value, size)
        } else {
            self.vdp.write(addr, value, size, &mut self.no_vdp)
        }
    }
}

impl AddressSpace for CpuNoVdp {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let length = size.len();
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        if size == Size::Long && addr == 0xFFFF_FFFC {
            info!("RETURNING 'init'");
            return u32::from_be_bytes(*b"init");
        }

        if addr == 0xA1_0000 || addr == 0xA1_0001 {
            // TODO: Is this correct?
            return u32::from_be_bytes(*b"\0\0UE");
        }

        if addr == 0xA1_0003 || addr == 0xA1_0004 {
            assert_eq!(size, Size::Byte);
            return self.controller1.read_reg1() as u32;
        }

        if addr == 0xA1_0005 || addr == 0xA1_0006 {
            assert_eq!(size, Size::Byte);
            return self.controller2.read_reg1() as u32;
        }

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            panic!("Attempt to access VDP from CpuNoVdp!")
        }

        let mut result = 0;

        for offset in 0..length {
            result <<= 8;
            let byte_addr = addr + offset;
            let byte = if byte_addr < self.rom.len() {
                self.rom[byte_addr]
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()]
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000]
            } else if (0xA0_0000..=0xA0_FFFF).contains(&byte_addr) {
                // TODO: Determine if this is specially mapped
                self.z80_ram[byte_addr - 0xA0_0000]
            } else {
                warn!("Unimplemented address {:#X}, reading zero", addr);
                0
            };

            result |= byte as u32;
        }

        result
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

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            panic!("Attempt to access VDP from CpuNoVdp!");
        }

        match addr {
            0xA1_0003..=0xA1_0004 => {
                self.controller1.write_reg1(value as u8);
                return;
            }
            0xA1_0009..=0xA1_000A => {
                assert_eq!(size, Size::Byte);
                self.controller1.write_reg2(value as u8);
                return;
            }
            0xA1_0000..=0xA1_0002 | 0xA1_0005..=0xA1_000F => {
                // TODO:
                warn!("Ignoring write to some stuff: {:#08X}", addr);
                return;
            }
            0xA1_1100..=0xA1_1101 => {
                // TODO:
                warn!("Ignoring Z80 bus request");
                return;
            }
            0xA1_1200 => {
                // TODO:
                warn!("Ignoring Z80 reset");
                return;
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO:
                warn!("Ignoring write to PSG output");
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
            0xC0_0000..=0xC0_000F => panic!("Attempt to access VDP from CpuNoVdp!"),
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
            } else if (0xA0_0000..=0xA0_FFFF).contains(&byte_addr) {
                // TODO: Determine if this is specially mapped
                self.z80_ram[byte_addr - 0xA0_0000] = *byte;
            } else {
                warn!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}
