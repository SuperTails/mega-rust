// TODO: Maybe remove this later?
#![allow(dead_code)]

pub mod instruction;
pub mod address_space;
use address_space::AddressSpace;
use crate::controller::Controller;
use crate::get_four_bytes;
use crate::vdp::Vdp;
use crate::Interrupt;
use bitfield::bitfield;
use instruction::{Instruction, Size};
use lazy_static::lazy_static;
use std::collections::BinaryHeap;
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Mutex, Weak};

lazy_static! {
    static ref LOG_INSTR: AtomicBool = AtomicBool::new(false);
}

pub fn do_log(do_l: bool) {
    LOG_INSTR.store(do_l, Ordering::SeqCst)
}

pub fn log_instr() -> bool {
    LOG_INSTR.load(Ordering::SeqCst)
}

bitfield! {
    #[derive(Clone, Copy, PartialEq)]
    pub struct Sr(u8);
    impl Debug;

    pub priority, set_priority: 2, 0;
    pub master, set_master: 4;
    pub supervisor, set_supervisor: 5;
    pub trace, set_trace: 7, 6;
}

impl fmt::Display for Sr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.supervisor() {
            write!(f, "SUP ")?;
        } else {
            write!(f, "USR ")?;
        }

        write!(f, "IPL: {:>3}", self.priority())
    }
}

bitfield! {
    #[derive(Clone, Copy, PartialEq)]
    pub struct Ccr(u8);
    impl Debug;

    pub extend, set_extend: 4;
    pub negative, set_negative: 3;
    pub zero, set_zero: 2;
    pub overflow, set_overflow: 1;
    pub carry, set_carry: 0;
}

impl Ccr {
    pub fn set_negative_sized(&mut self, value: u32, size: Size) {
        self.set_negative(size.negative_sized(value))
    }
}

impl fmt::Display for Ccr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let x_char = if self.extend() { 'X' } else { '-' };
        let n_char = if self.negative() { 'N' } else { '-' };
        let z_char = if self.zero() { 'Z' } else { '-' };
        let v_char = if self.overflow() { 'V' } else { '-' };
        let c_char = if self.carry() { 'C' } else { '-' };
        write!(f, "---{}{}{}{}{}", x_char, n_char, z_char, v_char, c_char)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum State {
    Run,
    Reset,
}

impl fmt::Display for CpuCore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "======== CPU Core State =========")?;
        writeln!(
            f,
            "PC: {:#06X} USP: {:#06X}, SR: {}|{} ",
            self.pc, self.usp, self.sr, self.ccr,
        )?;
        writeln!(f, "------------- Registers ---------")?;
        for (idx, (d, a)) in self.data.iter().zip(self.addr.iter()).enumerate() {
            writeln!(f, "d{}/a{} | {:#010X} | {:#010X}", idx, idx, d, a)?;
        }
        write!(f, "---------------------------------")
    }
}

/// Represents the current state of the 68k
/// processor, indepedent of memory state
#[derive(Debug, PartialEq, Clone)]
pub struct CpuCore {
    pub data: [u32; 8],
    pub addr: [u32; 8],
    usp: u32,
    pub pc: u32,
    pub ccr: Ccr,
    pub sr: Sr,

    cycle: usize,
    state: State,
}

pub struct Cpu {
    pub core: CpuCore,
    pub rom: Box<[u8]>,
    pub cart_ram: Box<[u8]>,
    pub ram: [u8; 0x1_0000],
    pub z80_ram: [u8; 0x1_0000],
    vdp: Weak<Mutex<Vdp>>,
    controller1: Weak<Mutex<Controller>>,
    controller2: Weak<Mutex<Controller>>,
}

impl Cpu {
    pub fn new(
        rom: &[u8],
        vdp: Weak<Mutex<Vdp>>,
        controller1: Weak<Mutex<Controller>>,
        controller2: Weak<Mutex<Controller>>,
    ) -> Cpu {
        let rom: Box<[u8]> = rom.into();
        let cart_ram = {
            let mut cart_ram = Vec::new();
            cart_ram.resize(0x40_0000 - rom.len(), 0);
            Box::from(&cart_ram[..])
        };

        Cpu {
            core: CpuCore::new(),
            rom,
            cart_ram,
            ram: [0; 0x1_0000],
            z80_ram: [0; 0x1_0000],
            vdp,
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

        println!(
            "Using vector at {:#08X}, PC will be {:#08X}",
            offset, vector
        );

        // TODO: Determine actual cycle count and behavior
        self.core.pc = vector;
        self.core.cycle += 1;
        self.core.state = State::Run;
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

        let vector =
            u32::from_be_bytes(get_four_bytes(&self.rom[0x70..][..4]));

        self.core.sr.set_priority(4);
        self.core.sr.set_supervisor(true);
        self.core.pc = vector;
        println!("Did horizontal interrupt");
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

        let vector =
            u32::from_be_bytes(get_four_bytes(&self.rom[0x78..][..4]));

        self.core.sr.set_priority(6);
        self.core.sr.set_supervisor(true);
        self.core.pc = vector;
        println!("Did vertical interrupt");
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

    fn do_run(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        self.handle_interrupts(pending);

        assert_eq!(self.core.pc % 2, 0);

        let instr = self.instr_at(self.core.pc as u32);

        if log_instr() {
            println!("Instr: {:?}", instr.opcode);
        }

        let prev_sup = self.core.sr.supervisor();

        // TODO: Cycle counts
        instr.execute(self);

        if prev_sup != self.core.sr.supervisor() {
            std::mem::swap(&mut self.core.addr[7], &mut self.core.usp);
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

#[derive(Debug)]
struct LogParseError {
    data: String,
}

impl std::fmt::Display for LogParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl std::error::Error for LogParseError {}

impl CpuCore {
    pub fn new() -> CpuCore {
        CpuCore {
            data: [0; 8],
            addr: [0; 8],
            usp: 0,
            pc: 0x0004,
            ccr: Ccr(0),
            sr: { let mut sr = Sr(0); sr.set_supervisor(true); sr },
            cycle: 0,
            state: State::Reset,
        }
    }

    pub fn read_log(log: &str) -> Result<Vec<CpuCore>, Box<dyn std::error::Error>> {
        let line_split = log.lines().skip(2).collect::<Vec<&str>>();

        let groups = line_split.chunks_exact(6);

        if !groups.remainder().is_empty() {
            return Err(Box::new(LogParseError {
                data: format!("Excess lines: {:?}", groups.remainder()),
            })
            .into());
        }

        let mut result = Vec::new();

        for group in groups {
            let mut state = CpuCore::new();

            let parse_one = |d: &str| {
                u32::from_str_radix(d.trim(), 16).map_err(|_| LogParseError {
                    data: d.to_string(),
                })
            };

            let read_regs = |line: &str| -> Result<(u32, u32, u32, u32), LogParseError> {
                Ok((
                    parse_one(&line[3..12])?,
                    parse_one(&line[15..24])?,
                    parse_one(&line[27..36])?,
                    parse_one(&line[39..48])?,
                ))
            };

            for (line_num, line) in group[1..5].iter().enumerate() {
                let read = read_regs(line)?;
                state.data[line_num] = read.0;
                state.data[4 + line_num] = read.1;
                state.addr[line_num] = read.2;
                state.addr[4 + line_num] = read.3;
            }

            let sr = u16::from_str_radix(&group[2][51..], 2)?;
            state.sr.0 = (sr >> 8) as u8;
            state.ccr.0 = sr as u8;
            let us = parse_one(&group[3][51..])?;
            state.usp = us;
            // TODO: What is this
            let _ss = parse_one(&group[4][51..])?;

            let pc = parse_one(&group[5][3..12])?;
            state.pc = pc;
            state.state = State::Run;

            result.push(state);
        }

        Ok(result)
    }
}

impl AddressSpace for Cpu {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let length = size.len();
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        if size == Size::Long && addr == 0xFFFF_FFFC {
            println!("RETURNING 'init'");
            return u32::from_be_bytes(*b"init");
        }

        if addr == 0xA1_0000 || addr == 0xA1_0001 {
            // TODO: Is this correct?
            return u32::from_be_bytes(*b"\0\0UE");
        }

        if addr == 0xA1_0003 || addr == 0xA1_0004 {
            assert_eq!(size, Size::Byte);
            let controller1 = self.controller1.upgrade().unwrap();
            return controller1.lock().unwrap().read_reg1() as u32;
        }

        if addr == 0xA1_0005 || addr == 0xA1_0006 {
            assert_eq!(size, Size::Byte);
            let controller2 = self.controller2.upgrade().unwrap();
            return controller2.lock().unwrap().read_reg1() as u32;
        }

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            let vdp = self.vdp.upgrade().unwrap();
            return vdp.lock().unwrap().read(addr as u32) as u32;
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
                println!("Unimplemented address {:#X}, reading zero", addr);
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
            let vdp = self.vdp.upgrade().unwrap();
            vdp.lock().unwrap().write(addr as u32, value, size, self);
            return;
        }

        match addr {
            0xA1_0003..=0xA1_0004 => {
                let controller1 = self.controller1.upgrade().unwrap();
                controller1.lock().unwrap().write_reg1(value as u8);
                return;
            }
            0xA1_0009..=0xA1_000A => {
                assert_eq!(size, Size::Byte);
                let controller1 = self.controller1.upgrade().unwrap();
                controller1.lock().unwrap().write_reg2(value as u8);
                return;
            }
            0xA1_0000..=0xA1_0002 | 0xA1_0005..=0xA1_000F => {
                // TODO:
                println!("Ignoring write to some stuff: {:#08X}", addr);
                return;
            }
            0xA1_1100..=0xA1_1101 => {
                // TODO:
                println!("Ignoring Z80 bus request");
                return;
            }
            0xA1_1200 => {
                // TODO:
                println!("Ignoring Z80 reset");
                return;
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO:
                println!("Ignoring write to PSG output");
                return;
            }
            0xA1_0020..=0xA1_10FF => {
                // Should we panic here, or not?
                println!("Ignoring write to reserved memory");
                return;
            }
            0xA1_30F1..=0xA1_30F2 => {
                // TODO:
                println!("Ignoring write of {:#X} to SRAM register", value);
                return;
            }
            0xC0_0000..=0xC0_000F => {
                self.vdp
                    .upgrade()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .write(addr as u32, value, size, self);
            }
            _ => {}
        }

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < self.rom.len() {
                //self.rom[byte_addr] = byte;
                println!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_FFFF).contains(&byte_addr) {
                // TODO: Determine if this is specially mapped
                self.z80_ram[byte_addr - 0xA0_0000] = *byte;
            } else {
                println!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}