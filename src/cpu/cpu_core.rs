use super::instruction::Size;
use crate::bindings::cpu::{MusashiCpu, Register};
use bitfield::bitfield;
use std::fmt;

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

/// Represents the current state of the 68k
/// processor, indepedent of memory state
#[derive(Debug, PartialEq, Clone)]
pub struct CpuCore {
    pub data: [u32; 8],
    pub addr: [u32; 8],
    pub usp: u32,
    pub pc: u32,
    pub ccr: Ccr,
    pub sr: Sr,

    pub cycle: usize,
    pub state: State,
}

impl CpuCore {
    pub fn new() -> CpuCore {
        CpuCore {
            data: [0; 8],
            addr: [0; 8],
            usp: 0,
            pc: 0x0004,
            ccr: Ccr(0),
            sr: {
                let mut sr = Sr(0);
                sr.set_supervisor(true);
                sr
            },
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

    pub fn from_musashi() -> CpuCore {
        let pc = MusashiCpu::get_reg(Register::Pc);
        let full_sr = MusashiCpu::get_reg(Register::Sr);
        let sr = Sr((full_sr >> 8) as u8);
        let ccr = Ccr(full_sr as u8);
        let data = {
            let mut data = [0; 8];
            for (idx, d) in data.iter_mut().enumerate() {
                *d = MusashiCpu::get_reg(Register::data(idx));
            }
            data
        };
        let addr = {
            let mut addr = [0; 8];
            for (idx, a) in addr.iter_mut().enumerate() {
                *a = MusashiCpu::get_reg(Register::addr(idx));
            }
            addr
        };

        CpuCore {
            sr,
            ccr,
            data,
            addr,
            pc,
            state: State::Run,
            usp: 0,
            cycle: 0,
        }
    }
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
