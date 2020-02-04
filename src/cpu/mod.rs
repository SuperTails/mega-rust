pub mod instruction;
use crate::get_four_bytes;
use crate::get_two_bytes;
use crate::vdp::Vdp;
use crate::Interrupt;
use bitfield::bitfield;
use instruction::{Instruction, Size};
use std::fmt;
use std::io::Write;
use std::sync::{Mutex, Weak};
use std::sync::atomic::{AtomicBool, Ordering};
use std::collections::BinaryHeap;
use lazy_static::lazy_static;

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
        writeln!(f, "PC: {:#06X} USP: {:#06X}, CCR: {}", self.pc, self.usp, self.ccr)?;
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
    sr: u8,

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
}

impl Cpu {
    pub fn new(rom: &[u8], vdp: Weak<Mutex<Vdp>>) -> Cpu {
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
        }
    }

    pub fn read(&self, addr: u32, size: Size) -> u32 {
        let length = size.len();
        let align = size.alignment();

        let addr = (addr & 0xFF_FF_FF) as usize;

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
            println!("Returning 0 from controller 1 read");
            return 0;
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

    pub fn write(&mut self, addr: u32, value: u32, size: Size) {
        let length = size.len();
        let align = size.alignment();

        let addr = (addr & 0xFF_FF_FF) as usize;

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
                println!("Ignoring write to controller 1");
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

    pub fn instr_at(&self, addr: u32) -> Instruction {
        Instruction::new(u16::from_be_bytes(get_two_bytes(
            &self.rom[addr as usize..][..2],
        )))
    }

    // TODO: Access Z80, VDP, expansion ports, and IO registers
    pub fn do_cycle(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        match self.core.state {
            State::Reset => {
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
            State::Run => {
                // 0x376 -> finish clearing RAM, then branch to:
                // VDPSetupGame @ 0x1222 - 0x129C
                // SoundDriverLoad @ 0x134A
                //  - KosDec @ 0x1894
                // JoypadInit
                //
                //
                /*if self.core.pc == 0x462 {
                    panic!("Hit 0x462");
                } else if self.core.pc == 0x43A {
                    panic!("Hit 0x43A");
                }*/

                if let Some(int) = pending.peek() {
                    let mask_level = self.core.sr & 0x7;

                    if *int as u8 > mask_level {
                        let int = pending.pop().unwrap();
                        match int {
                            Interrupt::External if mask_level < 2 => {
                                unimplemented!("External interrupt");
                            }
                            Interrupt::Horizontal if mask_level < 4 => {
                                unimplemented!("Horizontal interrupt");
                            }
                            Interrupt::Vertical if mask_level < 6 => {
                                // TODO: This should use the SSP instead of A7 I think
                                self.core.addr[7] = self.core.addr[7].wrapping_sub(4);
                                self.write(self.core.addr[7], self.core.pc, Size::Long);
                                self.core.addr[7] = self.core.addr[7].wrapping_sub(2);
                                self.write(
                                    self.core.addr[7],
                                    ((self.core.sr as u32) << 8) | self.core.ccr.0 as u32,
                                    Size::Word,
                                );

                                let vector = u32::from_be_bytes(get_four_bytes(&self.rom[0x78..][..4]));

                                self.core.sr = 6;
                                self.core.pc = vector;
                                println!("Did vertical interrupt");
                            }
                            _ => {}
                        }
                    }
                }

                if self.core.pc >= 0x3F_FFFF {
                    unimplemented!(
                        "PC is {:#X}, Error at (v_errortype) is {:#X}",
                        self.core.pc,
                        self.read(0xFFFF_FC44, Size::Long)
                    );
                } else {
                    assert_eq!(self.core.pc % 2, 0);
                    assert!(self.core.pc <= 0x3F_FFFF);

                    let offset = self.core.pc % self.rom.len() as u32;

                    if log_instr() {
                        println!(
                            "\nPC:{:08X}, A7: {:08X} CCR: {}",
                            self.core.pc, self.core.addr[7], self.core.ccr,
                        );
                        std::io::stdout().flush().unwrap();
                    }

                    let instr = self.instr_at(offset as u32);

                    if log_instr() {
                        println!("Instr: {:?}", instr.opcode);
                    }

                    // TODO: Cycle counts
                    instr.execute(self);
                }
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
            sr: 0,
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
            state.sr = (sr >> 8) as u8;
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
