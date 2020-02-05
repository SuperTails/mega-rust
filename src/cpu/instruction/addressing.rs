use super::Size;
use crate::cpu::log_instr;
use crate::cpu::Cpu;
use std::fmt;

#[derive(PartialEq, Clone, Copy)]
pub enum AddrMode {
    DataReg(u8),
    AddrReg(u8),
    Addr(u8),
    AddrPostIncr(u8),
    AddrPreDecr(u8),
    AddrDisp(u8),
    AddrIndex(u8),
    PcDisp,
    PcIndex,
    AbsShort,
    AbsLong,
    Immediate,
}

impl fmt::Debug for AddrMode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddrMode::DataReg(d) => write!(f, "d{}", d),
            AddrMode::AddrReg(d) => write!(f, "a{}", d),
            AddrMode::Immediate => write!(f, "#imm"),
            AddrMode::AddrPostIncr(d) => write!(f, "(a{})+", d),
            AddrMode::AddrPreDecr(d) => write!(f, "-(a{})", d),
            AddrMode::Addr(d) => write!(f, "(a{})", d),
            AddrMode::PcDisp => write!(f, "pc($disp)"),
            AddrMode::PcIndex => write!(f, "PC INDEX"),
            AddrMode::AbsShort => write!(f, "($addr).w"),
            AddrMode::AbsLong => write!(f, "($addr).l"),
            AddrMode::AddrDisp(d) => write!(f, "$addr(a{})", d),
            AddrMode::AddrIndex(d) => write!(f, "$addr(a{}, INDEX REG)", d),
        }
    }
}

impl AddrMode {
    pub fn new(data: u8) -> AddrMode {
        assert!(data <= 0x3F);

        let mode = data >> 3;
        let field = data & 0x7;

        match mode {
            0 => AddrMode::DataReg(field),
            1 => AddrMode::AddrReg(field),
            2 => AddrMode::Addr(field),
            3 => AddrMode::AddrPostIncr(field),
            4 => AddrMode::AddrPreDecr(field),
            5 => AddrMode::AddrDisp(field),
            6 => AddrMode::AddrIndex(field),
            7 => match field {
                2 => AddrMode::PcDisp,
                3 => AddrMode::PcIndex,
                0 => AddrMode::AbsShort,
                1 => AddrMode::AbsLong,
                4 => AddrMode::Immediate,
                _ => panic!("Invalid addressing mode suffix {:?}", field),
            },
            _ => unreachable!(),
        }
    }

    // TODO: HOW DOES THIS WORK AT ALL?!
    // TODO: HOW TO FIND SCALE
    fn address_index(self, extra: u32, cpu: &mut Cpu, reg: u32) -> Address {
        let index_info = cpu.read(cpu.core.pc + extra + 2, Size::Byte) as u8;
        let offset = cpu.read(cpu.core.pc + extra + 3, Size::Byte) as u8;

        if log_instr() {
            print!("Base offset of {:#X} ", offset);
        }

        // False is data, true is address
        let reg_type = (index_info >> 7) != 0;
        let reg_index = (index_info >> 4) & 0x7;
        let reg_offset = if reg_type {
            if log_instr() {
                println!("and using register a{}", reg_index);
            }
            cpu.core.addr[reg_index as usize]
        } else {
            if log_instr() {
                println!("and using register d{}", reg_index);
            }
            cpu.core.data[reg_index as usize]
        };

        if index_info & 0x7 != 0 {
            unimplemented!(
                "Don't know what the low three bits of {:#b} means",
                index_info
            )
        }

        Address::Address(reg.wrapping_add(reg_offset).wrapping_add(offset as u32))
    }

    pub fn address(self, has_immediate: bool, size: Size, cpu: &mut Cpu) -> Address {
        let extra = if has_immediate {
            (size.len() as u32 + 1) / 2 * 2
        } else {
            0
        };

        self.address_offset(extra, cpu)
    }

    pub fn address_offset(self, extra: u32, cpu: &mut Cpu) -> Address {
        match self {
            AddrMode::AbsLong => Address::Address(cpu.read(cpu.core.pc + extra + 2, Size::Long)),
            AddrMode::AbsShort => {
                Address::Address(cpu.read(cpu.core.pc + extra + 2, Size::Word) as i16 as i32 as u32)
            }
            AddrMode::PcDisp => {
                let offset = cpu.read(cpu.core.pc + extra + 2, Size::Word) as i16 as i64;
                Address::Address((cpu.core.pc as i64 + 2 + offset) as u32)
            }
            AddrMode::PcIndex => self.address_index(extra, cpu, cpu.core.pc + 2),
            AddrMode::AddrIndex(reg) => {
                let reg = cpu.core.addr[reg as usize];
                self.address_index(extra, cpu, reg)
            }
            AddrMode::DataReg(reg) => Address::DataReg(reg),
            AddrMode::AddrReg(reg) => Address::AddrReg(reg),
            AddrMode::AddrPreDecr(reg) | AddrMode::AddrPostIncr(reg) | AddrMode::Addr(reg) => {
                Address::Address(cpu.core.addr[reg as usize])
            }
            AddrMode::AddrDisp(reg) => {
                let offset = cpu.read(cpu.core.pc + 2 + extra, Size::Word) as i16 as i32 as u32;
                let reg = cpu.core.addr[reg as usize];

                let address = reg.wrapping_add(offset);

                Address::Address(address)
            }
            AddrMode::Immediate => Address::Address(cpu.core.pc + 2 + extra),
        }
    }

    pub fn read_offset(self, immediate_offset: u32, size: Size, cpu: &mut Cpu) -> u32 {
        if let AddrMode::AddrPreDecr(reg) = self {
            let reg = &mut cpu.core.addr[reg as usize];
            *reg = reg.wrapping_sub(size.len() as u32);
        }
        let address = self.address_offset(immediate_offset, cpu);
        if let AddrMode::AddrPostIncr(reg) = self {
            let reg = &mut cpu.core.addr[reg as usize];
            *reg = reg.wrapping_add(size.len() as u32);
        }
        address.read(size, cpu)
    }

    pub fn write_offset(self, value: u32, immediate_offset: u32, size: Size, cpu: &mut Cpu) {
        if let AddrMode::AddrPreDecr(reg) = self {
            let reg = &mut cpu.core.addr[reg as usize];
            *reg = reg.wrapping_sub(size.len() as u32);
        }
        let address = self.address_offset(immediate_offset, cpu);
        if let AddrMode::AddrPostIncr(reg) = self {
            let reg = &mut cpu.core.addr[reg as usize];
            *reg = reg.wrapping_add(size.len() as u32);
        }
        address.write(value, size, cpu)
    }

    pub fn arg_length(self, size: Size) -> u32 {
        match self {
            AddrMode::Immediate => {
                if size == Size::Long {
                    4
                } else {
                    2
                }
            }

            AddrMode::DataReg(_)
            | AddrMode::AddrReg(_)
            | AddrMode::AddrPreDecr(_)
            | AddrMode::AddrPostIncr(_)
            | AddrMode::Addr(_) => 0,

            AddrMode::AbsLong => 4,

            // TODO: Number of extension words can vary?!
            AddrMode::PcIndex | AddrMode::AddrIndex(_) => 2,

            AddrMode::AbsShort | AddrMode::PcDisp | AddrMode::AddrDisp(_) => 2,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Address {
    DataReg(u8),
    AddrReg(u8),
    Address(u32),
}

impl Address {
    pub fn read(&self, size: Size, cpu: &mut Cpu) -> u32 {
        match self {
            Address::DataReg(reg) => DataReg::new(*reg).read_sized(size, cpu),
            Address::AddrReg(reg) => AddrReg::new(*reg).read_sized(size, cpu),
            Address::Address(addr) => cpu.read(*addr, size),
        }
    }

    pub fn write(&self, value: u32, size: Size, cpu: &mut Cpu) {
        match self {
            Address::DataReg(reg) => {
                DataReg::new(*reg).write_sized(value, size, cpu);
            }
            Address::AddrReg(reg) => {
                AddrReg::new(*reg).write_sized(value, size, cpu);
            }
            Address::Address(addr) => cpu.write(*addr, value, size),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct DataReg(u8);

impl DataReg {
    pub fn new(reg: u8) -> DataReg {
        assert!(reg < 8);
        DataReg(reg)
    }

    pub fn read(self, cpu: &Cpu) -> u32 {
        cpu.core.data[self.0 as usize]
    }

    pub fn read_sized(self, size: Size, cpu: &Cpu) -> u32 {
        cpu.core.data[self.0 as usize] & size.mask()
    }

    pub fn write(self, value: u32, cpu: &mut Cpu) {
        cpu.core.data[self.0 as usize] = value;
    }

    pub fn write_sized(self, value: u32, size: Size, cpu: &mut Cpu) {
        cpu.core.data[self.0 as usize] &= !size.mask();
        cpu.core.data[self.0 as usize] |= value & size.mask();
    }

    pub fn into_inner(self) -> u8 {
        self.0
    }
}

impl Into<u8> for DataReg {
    fn into(self) -> u8 {
        self.0
    }
}

#[derive(Debug, Clone, Copy)]
pub struct AddrReg(u8);

impl AddrReg {
    pub fn new(reg: u8) -> AddrReg {
        assert!(reg < 8);
        AddrReg(reg)
    }

    pub fn read(self, cpu: &Cpu) -> u32 {
        cpu.core.addr[self.0 as usize]
    }

    pub fn read_sized(self, size: Size, cpu: &Cpu) -> u32 {
        cpu.core.addr[self.0 as usize] & size.mask()
    }

    pub fn write(self, value: u32, cpu: &mut Cpu) {
        cpu.core.addr[self.0 as usize] = value;
    }

    pub fn write_sized(self, value: u32, size: Size, cpu: &mut Cpu) {
        cpu.core.addr[self.0 as usize] &= !size.mask();
        cpu.core.addr[self.0 as usize] |= value & size.mask();
    }

    pub fn into_inner(self) -> u8 {
        self.0
    }
}

impl Into<u8> for AddrReg {
    fn into(self) -> u8 {
        self.0
    }
}
