use super::{Ccr, Cpu, LOG_INSTR};
use either::Either;
use num_traits::FromPrimitive;
pub use pages::*;
use std::convert::TryFrom;
use std::fmt;

mod pages;

#[derive(PartialEq)]
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
                _ => panic!(),
            },
            _ => unreachable!(),
        }
    }

    // TODO: HOW DOES THIS WORK AT ALL?!
    // TODO: HOW TO FIND SCALE
    fn address_index(&self, extra: u32, cpu: &mut Cpu, reg: u32) -> Address {
        let index_info = cpu.read(cpu.core.pc + extra + 2, Size::Byte) as u8;
        let offset = cpu.read(cpu.core.pc + extra + 3, Size::Byte) as u8;

        if LOG_INSTR {
            print!("Base offset of {:#X} ", offset);
        }

        // False is data, true is address
        let reg_type = (index_info >> 7) != 0;
        let reg_index = (index_info >> 4) & 0x7;
        let reg_offset = if reg_type {
            if LOG_INSTR {
                println!("and using register a{}", reg_index);
            }
            cpu.core.addr[reg_index as usize]
        } else {
            if LOG_INSTR {
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

    pub fn address(&self, has_immediate: bool, size: Size, cpu: &mut Cpu) -> Address {
        let extra = if has_immediate {
            (size.len() as u32 + 1) / 2 * 2
        } else {
            0
        };
        match self {
            AddrMode::AbsLong => Address::Address(cpu.read(cpu.core.pc + extra + 2, Size::Long)),
            AddrMode::AbsShort => {
                Address::Address(cpu.read(cpu.core.pc + extra + 2, Size::Word) as i16 as i32 as u32)
            }
            AddrMode::PcDisp => {
                // TODO: Is the displacement signed?
                let offset = cpu.read(cpu.core.pc + extra + 2, Size::Word);
                Address::Address(cpu.core.pc + 2 + offset)
            }
            AddrMode::PcIndex => self.address_index(extra, cpu, cpu.core.pc + 2),
            AddrMode::AddrIndex(reg) => {
                let reg = cpu.core.addr[*reg as usize];
                self.address_index(extra, cpu, reg)
            }
            AddrMode::DataReg(reg) => Address::DataReg(*reg),
            AddrMode::AddrReg(reg) => Address::AddrReg(*reg),
            AddrMode::AddrPreDecr(reg) | AddrMode::AddrPostIncr(reg) | AddrMode::Addr(reg) => {
                Address::Address(cpu.core.addr[*reg as usize])
            }
            AddrMode::AddrDisp(reg) => {
                let offset = cpu.read(cpu.core.pc + 2 + extra, Size::Word) as i16 as i32 as u32;
                let reg = cpu.core.addr[*reg as usize];

                let address = reg.wrapping_add(offset);

                Address::Address(address)
            }
            AddrMode::Immediate => Address::Address(cpu.core.pc + 2 + extra),
        }
    }

    pub fn read(&self, has_immediate: bool, size: Size, cpu: &mut Cpu) -> u32 {
        if let AddrMode::AddrPreDecr(reg) = self {
            let reg = &mut cpu.core.addr[*reg as usize];
            *reg = reg.wrapping_sub(size.len() as u32);
        }
        let address = self.address(has_immediate, size, cpu);
        if let AddrMode::AddrPostIncr(reg) = self {
            let reg = &mut cpu.core.addr[*reg as usize];
            *reg = reg.wrapping_add(size.len() as u32);
        }
        address.read(size, cpu)
    }

    pub fn write(&self, value: u32, has_immediate: bool, size: Size, cpu: &mut Cpu) {
        if let AddrMode::AddrPreDecr(reg) = self {
            let reg = &mut cpu.core.addr[*reg as usize];
            *reg = reg.wrapping_sub(size.len() as u32);
        }
        let address = self.address(has_immediate, size, cpu);
        if let AddrMode::AddrPostIncr(reg) = self {
            let reg = &mut cpu.core.addr[*reg as usize];
            *reg = reg.wrapping_add(size.len() as u32);
        }
        address.write(value, size, cpu)
    }

    pub fn arg_length(&self, size: Size) -> u32 {
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

#[derive(Debug, FromPrimitive, PartialEq)]
pub enum Condition {
    True = 0b0000,
    False = 0b0001,
    Higher = 0b0010,
    LowerOrSame = 0b0011,
    CarryClear = 0b0100,
    CarrySet = 0b0101,
    NotEqual = 0b0110,
    Equal,
    OverflowClear,
    OverflowSet,
    Plus,
    Minus,
    GreaterOrEqual,
    LessThan,
    GreaterThan,
    LessOrEqual,
}

impl Condition {
    fn short_name(&self) -> &'static str {
        match self {
            Condition::True => "ra",
            Condition::False => "sr",
            Condition::Higher => "hi",
            Condition::LowerOrSame => "ls",
            Condition::CarryClear => "cc",
            Condition::CarrySet => "cs",
            Condition::NotEqual => "ne",
            Condition::Equal => "eq",
            Condition::OverflowClear => "vc",
            Condition::OverflowSet => "vs",
            Condition::Plus => "pl",
            Condition::Minus => "mi",
            Condition::GreaterOrEqual => "ge",
            Condition::LessThan => "lt",
            Condition::GreaterThan => "gt",
            Condition::LessOrEqual => "le",
        }
    }
}

impl Condition {
    pub fn check(&self, ccr: Ccr) -> bool {
        match self {
            Condition::True => true,
            Condition::False => false,
            Condition::Higher => !ccr.carry() && !ccr.zero(),
            Condition::LowerOrSame => ccr.carry() || ccr.zero(),
            Condition::CarryClear => !ccr.carry(),
            Condition::CarrySet => ccr.carry(),
            Condition::NotEqual => !ccr.zero(),
            Condition::Equal => ccr.zero(),
            Condition::OverflowClear => !ccr.overflow(),
            Condition::OverflowSet => ccr.overflow(),
            Condition::Plus => !ccr.negative(),
            Condition::Minus => ccr.negative(),
            Condition::GreaterOrEqual => {
                (ccr.negative() && ccr.overflow()) || (!ccr.negative() && !ccr.overflow())
            }
            Condition::LessThan => {
                (ccr.negative() && !ccr.overflow()) || (!ccr.negative() && ccr.overflow())
            }
            Condition::GreaterThan => {
                (ccr.negative() && ccr.overflow() && ccr.zero())
                    || (!ccr.negative() && !ccr.overflow() && !ccr.zero())
            }
            Condition::LessOrEqual => {
                ccr.zero()
                    || (ccr.negative() && !ccr.overflow())
                    || (!ccr.negative() && ccr.overflow())
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Size {
    Byte,
    Word,
    Long,
}

impl Size {
    pub fn negative_sized(self, value: u32) -> bool {
        match self {
            Size::Byte => (value as i8) < 0,
            Size::Word => (value as i16) < 0,
            Size::Long => (value as i32) < 0,
        }
    }

    pub fn normal(data: u8) -> Size {
        match data {
            0 => Size::Byte,
            1 => Size::Word,
            2 => Size::Long,
            _ => panic!("Invalid size {}", data),
        }
    }

    pub fn suffix(self) -> &'static str {
        match self {
            Size::Byte => "b",
            Size::Word => "w",
            Size::Long => "l",
        }
    }

    pub fn mask(self) -> u32 {
        match self {
            Size::Byte => 0xFF,
            Size::Word => 0xFF_FF,
            Size::Long => 0xFF_FF_FF_FF,
        }
    }

    pub fn single_bit(data: bool) -> Size {
        if data {
            Size::Word
        } else {
            Size::Long
        }
    }

    pub fn higher(data: u8) -> Size {
        match data {
            1 => Size::Byte,
            3 => Size::Word,
            2 => Size::Long,
            _ => panic!("Invalid size {}", data),
        }
    }

    pub fn len(self) -> usize {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::Long => 4,
        }
    }

    pub fn alignment(self) -> usize {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::Long => 2,
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

#[derive(Debug, FromPrimitive)]
pub enum SimpleOp {
    Or = 0b000,
    And = 0b001,
    Sub = 0b010,
    Add = 0b011,
    Eor = 0b101,
    Cmp = 0b110,
}

impl SimpleOp {
    pub fn name(&self) -> &'static str {
        match self {
            SimpleOp::Or => "ori",
            SimpleOp::And => "andi",
            SimpleOp::Sub => "subi",
            SimpleOp::Add => "addi",
            SimpleOp::Eor => "eori",
            SimpleOp::Cmp => "cmpi",
        }
    }

    pub fn apply(&self, mut original: u32, arg: u32, size: Size, mut old_ccr: Ccr) -> (u32, Ccr) {
        let mask = size.mask();

        let result = match self {
            SimpleOp::Or => original | arg,
            SimpleOp::And => original & arg,
            SimpleOp::Eor => original ^ arg,
            SimpleOp::Add => original.wrapping_add(arg),
            SimpleOp::Sub => original.wrapping_sub(arg),
            SimpleOp::Cmp => original,
        };

        original &= !mask;
        original |= result & mask;

        let new_ccr = match self {
            SimpleOp::Sub | SimpleOp::Cmp => compare(original, arg, size, false),
            SimpleOp::Add => compare(original, arg, size, true),
            _ => {
                let mut ccr = Ccr(0);
                ccr.set_overflow(false);
                ccr.set_carry(false);
                ccr.set_zero(result & mask == 0);
                ccr.set_negative_sized(result, size);
                ccr
            }
        };

        // Notice that this does *not* change the extend bit
        old_ccr.set_overflow(new_ccr.overflow());
        old_ccr.set_carry(new_ccr.carry());
        old_ccr.set_zero(new_ccr.zero());
        old_ccr.set_negative(new_ccr.negative());

        (original, old_ccr)
    }
}

#[derive(Debug, FromPrimitive)]
pub enum BitOpType {
    Test = 0,
    Change,
    Clear,
    Set,
}

impl BitOpType {
    fn short_name(&self) -> &'static str {
        match self {
            BitOpType::Test => "tst",
            BitOpType::Change => "chg",
            BitOpType::Clear => "clr",
            BitOpType::Set => "set",
        }
    }
}

pub struct Immediates {
    pub mode: AddrMode,
    pub size: Size,
    pub op: SimpleOp,
}

impl fmt::Debug for Immediates {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{} #imm,", self.op.name(), self.size.suffix())?;
        if self.use_sr() {
            write!(f, "sr")
        } else if self.use_ccr() {
            write!(f, "ccr")
        } else {
            write!(f, "{:?}", self.mode)
        }
    }
}

impl TryFrom<u16> for Immediates {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let mode = AddrMode::new(data as u8 & 0x3F);
        let size = Size::normal(data as u8 >> 6);
        let op = SimpleOp::from_u16((data >> 9) & 0x7)
            .ok_or_else(|| format!("Invalid op type {:#b}", (data >> 9) & 0x7))?;

        Ok(Immediates { mode, size, op })
    }
}

impl Immediates {
    fn use_ccr(&self) -> bool {
        if let AddrMode::Immediate = &self.mode {
            self.size == Size::Byte
        } else {
            false
        }
    }

    fn use_sr(&self) -> bool {
        if let AddrMode::Immediate = &self.mode {
            self.size == Size::Word
        } else {
            false
        }
    }

    fn read(&self, cpu: &mut Cpu) -> u32 {
        if self.use_ccr() {
            (cpu.core.ccr.0 as u32)
        } else if self.use_sr() {
            (cpu.core.sr as u32) << 8 | (cpu.core.ccr.0 as u32)
        } else {
            self.mode.read(true, self.size, cpu)
        }
    }

    fn write(&self, value: u32, cpu: &mut Cpu) {
        if self.use_ccr() {
            cpu.core.ccr.0 = value as u8;
        } else if self.use_sr() {
            cpu.core.sr = (value >> 8) as u8;
            cpu.core.ccr.0 = value as u8;
        } else {
            self.mode.write(value, true, self.size, cpu)
        }
    }
}

impl Instr for Immediates {
    fn size(&self) -> u32 {
        2 + AddrMode::Immediate.arg_length(self.size)
            + if let AddrMode::Immediate = self.mode {
                // Special mode for interacting with the CCR or the SR
                0
            } else {
                // Any size can be passed because we know the mode is not immediate
                self.mode.arg_length(Size::Byte)
            }
    }

    fn execute(&self, cpu: &mut Cpu) {
        let value = self.read(cpu);

        let arg = cpu.read(cpu.core.pc + 2, self.size);
        let (value, ccr) = self.op.apply(value, arg, self.size, cpu.core.ccr);

        self.write(value, cpu);
        cpu.core.ccr = ccr;
    }
}

pub struct BitOperation {
    mode: AddrMode,
    data_reg: Option<DataReg>,
    op_type: BitOpType,
}

impl fmt::Debug for BitOperation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{} ", self.op_type.short_name())?;
        if let Some(reg) = self.data_reg {
            write!(f, "d{},", reg.into_inner())?;
        } else {
            write!(f, "#imm,")?;
        }

        write!(f, "{:?}", self.mode)
    }
}

impl TryFrom<u16> for BitOperation {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let mode = AddrMode::new(data as u8 & 0x3F);
        let op_type = BitOpType::from_u16((data >> 6) & 0x3)
            .ok_or_else(|| format!("Invalid op {:?}", (data >> 6) & 0x3))?;

        let has_immediate = (data >> 8) & 1 == 0;
        let data_reg = if has_immediate {
            None
        } else {
            Some(DataReg::new((data >> 9) as u8 & 0x7))
        };

        Ok(BitOperation {
            mode,
            data_reg,
            op_type,
        })
    }
}

impl BitOperation {
    fn shift_mod(&self) -> u32 {
        if let AddrMode::DataReg(_) = self.mode {
            32
        } else {
            8
        }
    }

    fn data_size(&self) -> Size {
        if let AddrMode::DataReg(_) = self.mode {
            Size::Long
        } else {
            Size::Byte
        }
    }
}

impl BitOperation {
    fn size(&self) -> u32 {
        let shift_size = if self.data_reg.is_some() { 0 } else { 2 };
        2 + shift_size + self.mode.arg_length(Size::Byte)
    }

    fn execute(&self, cpu: &mut Cpu) {
        let shift_amount = if let Some(data_reg) = self.data_reg {
            data_reg.read(cpu)
        } else {
            cpu.read(cpu.core.pc + 2, Size::Byte)
        };

        let shift_amount = shift_amount % self.shift_mod();
        let bit = 1 << shift_amount;

        let mut value = self
            .mode
            .read(self.data_reg.is_none(), self.data_size(), cpu);
        cpu.core.ccr.set_zero(value & bit == 0);

        match self.op_type {
            BitOpType::Test => {}
            BitOpType::Clear => value &= !bit,
            BitOpType::Set => value |= bit,
            BitOpType::Change => value ^= bit,
        }

        self.mode
            .write(value, self.data_reg.is_none(), self.data_size(), cpu);
    }
}

#[derive(Debug)]
pub enum Miscellaneous {
    // If size is Some, then it's a TST instruction,
    // otherwise it's a TAS instruction
    Test(Option<Size>, AddrMode),
    Lea(AddrReg, AddrMode),
    Swap(DataReg),
    Pea(AddrMode),
    // The bool is the direction of the move
    MoveM(bool, Size, AddrMode),
    // The bool is the direction of the move (OPPOSITE of the one for MoveM!)
    MoveUsp(bool, AddrReg),
    MoveToSr(AddrMode),
    MoveFromSr(AddrMode),
    MoveToCcr(AddrMode),
    Clear(Size, AddrMode),
    Jsr(AddrMode),
    Not(Size, AddrMode),
    Jmp(AddrMode),
    Ext(Size, DataReg),
    Neg(Size, AddrMode),
    Link(AddrReg),
    Unlink(AddrReg),
}

impl Instr for Miscellaneous {
    fn size(&self) -> u32 {
        match self {
            Miscellaneous::Unlink(_) => 2,
            Miscellaneous::Link(_) => 4,
            Miscellaneous::Test(size, mode) => {
                // The size only matters for an immediate instruction, and
                // an immediate addressing mode is not legal for the TAS
                // instruction, so using *any* default is okay
                let addr_size = mode.arg_length(*size.as_ref().unwrap_or(&Size::Byte));

                2 + addr_size
            }
            Miscellaneous::Neg(size, mode) => 2 + mode.arg_length(*size),
            Miscellaneous::Ext(_, _) => 2,
            Miscellaneous::Swap(_) => 2,
            Miscellaneous::Pea(mode) | Miscellaneous::Lea(_, mode) => {
                let addr_size = mode.arg_length(Size::Long);

                2 + addr_size
            }
            Miscellaneous::MoveM(_, size, mode) => 4 + mode.arg_length(*size),
            Miscellaneous::MoveUsp(_, _) => 2,
            Miscellaneous::MoveToSr(mode) => 2 + mode.arg_length(Size::Word),
            Miscellaneous::MoveFromSr(mode) => 2 + mode.arg_length(Size::Word),
            Miscellaneous::MoveToCcr(mode) => 2 + mode.arg_length(Size::Byte),
            Miscellaneous::Clear(size, mode) => 2 + mode.arg_length(*size),
            Miscellaneous::Jsr(mode) => 2 + mode.arg_length(Size::Byte),
            Miscellaneous::Not(size, mode) => 2 + mode.arg_length(*size),
            Miscellaneous::Jmp(mode) => 2 + mode.arg_length(Size::Byte),
        }
    }

    // TODO: oh god please no
    fn execute(&self, cpu: &mut Cpu) {
        match self {
            Miscellaneous::Link(reg) => {
                cpu.core.addr[7] = cpu.core.addr[7].wrapping_sub(4);
                cpu.write(cpu.core.addr[7], reg.read(cpu), Size::Long);
                reg.write(cpu.core.addr[7], cpu);
                let disp = cpu.read(cpu.core.pc + 2, Size::Word) as i16 as i32 as u32;
                cpu.core.addr[7] = cpu.core.addr[7].wrapping_add(disp);
            }
            Miscellaneous::Unlink(reg) => {
                cpu.core.addr[7] = reg.read(cpu);
                let disp = cpu.read(cpu.core.addr[7], Size::Long);
                reg.write(disp, cpu);
                cpu.core.addr[7] = cpu.core.addr[7].wrapping_add(4);
            }
            Miscellaneous::Neg(size, mode) => {
                let mut value = mode.read(false, *size, cpu);
                cpu.core.ccr = compare(0, value, *size, false);
                value = 0_u32.wrapping_sub(value);
                mode.write(value, false, *size, cpu);
            }
            Miscellaneous::Swap(reg) => {
                let value = reg.read(cpu);
                let result = (value << 16) | (value >> 16);
                reg.write(result, cpu);
            }
            Miscellaneous::Ext(size, reg) => match size {
                Size::Word => {
                    let result = reg.read(cpu) as i8 as i16 as u32;
                    cpu.core.ccr.set_zero(result == 0);
                    cpu.core.ccr.set_negative_sized(result, Size::Word);
                    reg.write_sized(result, Size::Word, cpu);
                }
                Size::Long => {
                    let result = reg.read(cpu) as i16 as i32 as u32;
                    cpu.core.ccr.set_zero(result == 0);
                    cpu.core.ccr.set_negative_sized(result, Size::Long);
                    reg.write_sized(result, Size::Long, cpu);
                }
                _ => unreachable!(),
            },
            Miscellaneous::Test(size, mode) => {
                if let Some(size) = size {
                    let value = mode.read(false, *size, cpu);
                    cpu.core.ccr.set_zero(value == 0);
                    cpu.core.ccr.set_overflow(false);
                    cpu.core.ccr.set_carry(false);
                    cpu.core.ccr.set_negative_sized(value, *size);
                } else {
                    unimplemented!()
                }
            }
            Miscellaneous::Jsr(mode) => {
                let stored = cpu.core.pc + self.size();
                cpu.core.addr[7] = cpu.core.addr[7].wrapping_sub(4);
                cpu.write(cpu.core.addr[7], stored, Size::Long);

                if LOG_INSTR {
                    println!("Pushed return address {:#X}", stored)
                }

                if let Address::Address(dest) = mode.address(false, Size::Byte, cpu) {
                    cpu.core.pc = dest - self.size();
                } else {
                    panic!()
                }
            }
            Miscellaneous::Jmp(mode) => {
                if let Address::Address(dest) = mode.address(false, Size::Byte, cpu) {
                    cpu.core.pc = dest - self.size();
                    if LOG_INSTR {
                        println!("JMP, PC will be {:#X}", cpu.core.pc + self.size());
                    }
                } else {
                    panic!()
                }
            }
            Miscellaneous::Lea(reg, mode) => {
                // TODO: Make sure this is actually what this instruction does
                if let Address::Address(address) = mode.address(false, Size::Long, cpu) {
                    if LOG_INSTR {
                        println!("Lea {:?} = {:#X}", reg, address)
                    }
                    reg.write(address, cpu);
                } else {
                    panic!()
                }
            }
            Miscellaneous::Pea(mode) => {
                if let Address::Address(address) = mode.address(false, Size::Long, cpu) {
                    cpu.core.addr[7] = cpu.core.addr[7].wrapping_sub(4);
                    cpu.write(cpu.core.addr[7], address, Size::Long);
                } else {
                    panic!()
                }
            }
            Miscellaneous::Clear(size, addr_mode) => {
                addr_mode.write(0, false, *size, cpu);
                cpu.core.ccr.set_negative(false);
                cpu.core.ccr.set_overflow(false);
                cpu.core.ccr.set_carry(false);
                cpu.core.ccr.set_zero(true);
            }
            Miscellaneous::MoveM(direction, size, mode) => {
                let extend = |val: u32| -> u32 {
                    match size {
                        Size::Byte => val as i8 as i32 as u32,
                        Size::Word => val as i16 as i32 as u32,
                        Size::Long => val,
                    }
                };

                match mode {
                    AddrMode::AbsLong | AddrMode::AbsShort => {
                        let mask = cpu.read(cpu.core.pc + 2, Size::Word) as u16;

                        let mut addr =
                            if let Address::Address(addr) = mode.address(true, Size::Word, cpu) {
                                addr
                            } else {
                                unreachable!()
                            };

                        for i in 0..16 {
                            if (mask >> i) & 1 != 0 {
                                if *direction {
                                    let value = extend(cpu.read(addr, *size));
                                    let dst = if i >= 8 {
                                        &mut cpu.core.addr
                                    } else {
                                        &mut cpu.core.data
                                    };
                                    dst[i % 8] = value;
                                } else {
                                    let src = if i >= 8 {
                                        &cpu.core.addr
                                    } else {
                                        &cpu.core.data
                                    };
                                    let value = src[i % 8];
                                    cpu.write(addr, value, *size);
                                }

                                addr += size.len() as u32;
                            }
                        }
                    }
                    AddrMode::AddrPreDecr(reg) => {
                        // Only register-to-memory transfers are allowed in this mode
                        assert_eq!(*direction, false);

                        let mask = cpu.read(cpu.core.pc + 2, Size::Word) as u16;

                        let reg = *reg as usize;

                        for i in 0..16 {
                            if (mask >> i) & 1 != 0 {
                                cpu.core.addr[reg] =
                                    cpu.core.addr[reg].wrapping_sub(size.len() as u32);

                                if i < 8 {
                                    cpu.write(cpu.core.addr[reg], cpu.core.addr[7 - (i % 8)], *size)
                                } else {
                                    cpu.write(cpu.core.addr[reg], cpu.core.data[7 - (i % 8)], *size)
                                }
                            }
                        }
                    }
                    AddrMode::AddrPostIncr(reg) => {
                        // Only memory-to-register transfers are allowed in this mode
                        assert_eq!(*direction, true);

                        let mask = cpu.read(cpu.core.pc + 2, Size::Word) as u16;

                        let reg = *reg as usize;

                        for i in 0..16 {
                            if (mask >> i) & 1 != 0 {
                                let value = cpu.read(cpu.core.addr[reg], *size);

                                if i >= 8 {
                                    cpu.core.addr[i % 8] = extend(value);
                                } else {
                                    cpu.core.data[i % 8] = extend(value);
                                }

                                cpu.core.addr[reg] =
                                    cpu.core.addr[reg].wrapping_add(size.len() as u32);
                            }
                        }
                    }
                    AddrMode::Addr(reg) => {
                        let mask = (cpu.read(cpu.core.pc + 2, Size::Word) as u16).swap_bytes();

                        // false -> register to memory
                        // true -> memory to register

                        let mut addr = cpu.core.addr[*reg as usize];
                        for i in 0..16 {
                            if (mask >> i) & 1 != 0 {
                                if *direction {
                                    let value = cpu.read(addr, *size);
                                    let dst = if i >= 8 {
                                        &mut cpu.core.data
                                    } else {
                                        &mut cpu.core.addr
                                    };
                                    dst[i % 8] = value;
                                } else {
                                    let src = if i >= 8 {
                                        &cpu.core.addr
                                    } else {
                                        &cpu.core.data
                                    };
                                    let value = src[i % 8];
                                    cpu.write(addr, value, *size);
                                }

                                addr = addr.wrapping_add(size.len() as u32);
                            }
                        }
                    }
                    _ => unimplemented!(
                        "Mode {:?}, direction: {:?}, size: {:?}",
                        mode,
                        direction,
                        size
                    ),
                }
            }
            Miscellaneous::MoveUsp(dir, reg) => {
                // TODO: Check if these are swapped
                let reg = reg.into_inner() as usize;
                if *dir {
                    cpu.core.addr[reg] = cpu.core.usp;
                } else {
                    cpu.core.usp = cpu.core.addr[reg];
                }
            }
            Miscellaneous::MoveFromSr(mode) => {
                let value = (cpu.core.sr as u16) << 8 | cpu.core.ccr.0 as u16;
                mode.write(value as u32, false, Size::Word, cpu);
            }
            Miscellaneous::MoveToSr(mode) => {
                let data = mode.read(false, Size::Word, cpu);
                cpu.core.sr = (data >> 8) as u8;
                cpu.core.ccr.0 = data as u8;
            }
            Miscellaneous::MoveToCcr(mode) => {
                cpu.core.ccr.0 = mode.read(false, Size::Word, cpu) as u8;
            }
            Miscellaneous::Not(size, mode) => {
                let result = !mode.read(false, *size, cpu);
                cpu.core.ccr.set_overflow(false);
                cpu.core.ccr.set_carry(false);
                cpu.core.ccr.set_zero(result == 0);
                cpu.core.ccr.set_negative_sized(result, *size);
                mode.write(result, false, *size, cpu);
            }
        }
    }
}

impl TryFrom<u16> for Miscellaneous {
    type Error = String;

    fn try_from(data: u16) -> Result<Miscellaneous, Self::Error> {
        if data >> 12 != 0b0100 {
            return Err(format!("Invalid top nybble {:#b}", data >> 12));
        }

        if (data >> 3) & 0b111_111_111 == 0b100_001_000 {
            return Ok(Miscellaneous::Swap(DataReg::new(data as u8 & 0x7)));
        }

        if (data >> 3) & 0b111_111_111 == 0b111_001_010 {
            return Ok(Miscellaneous::Link(AddrReg::new(data as u8 & 0x7)));
        }

        if (data >> 3) & 0b111_111_111 == 0b111_001_011 {
            return Ok(Miscellaneous::Unlink(AddrReg::new(data as u8 & 0x7)));
        }

        let get_mode = || AddrMode::new(data as u8 & 0x3F);
        match (data >> 6) & 0b111_111 {
            0b000_011 => return Ok(Miscellaneous::MoveFromSr(get_mode())),
            0b010_011 => return Ok(Miscellaneous::MoveToCcr(get_mode())),
            0b011_011 => return Ok(Miscellaneous::MoveToSr(get_mode())),
            0b100_001 => return Ok(Miscellaneous::Pea(get_mode())),
            0b111_011 => return Ok(Miscellaneous::Jmp(get_mode())),
            0b111_010 => return Ok(Miscellaneous::Jsr(get_mode())),
            _ => {}
        }

        let low_byte = data as u8;
        let next_nybble = (data >> 8) as u8 & 0b1111;
        if next_nybble == 0b1010 {
            let mode = AddrMode::new(low_byte & 0x3F);
            let size = low_byte >> 6;
            let size = if size == 0b11 {
                None
            } else {
                Some(Size::normal(size))
            };

            Ok(Miscellaneous::Test(size, mode))
        } else if bitpat!(1 0 0 0 1 _ 0 0 0)(data >> 3) {
            let size = Size::single_bit((data >> 6) != 0);
            let reg = DataReg::new(data as u8 & 0x7);
            Ok(Miscellaneous::Ext(size, reg))
        } else if (data >> 6) & 0b111 == 0b111 {
            let reg = AddrReg::new((data >> 9) as u8 & 0b111);
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::Lea(reg, mode))
        } else if (data >> 8) & 0b1111 == 0b0100 {
            let mode = AddrMode::new(low_byte & 0x3F);
            let size = Size::normal((data >> 6) as u8 & 3);
            Ok(Miscellaneous::Neg(size, mode))
        } else if (data >> 8) & 0b1111 == 0b0110 {
            let mode = AddrMode::new(low_byte & 0x3F);
            let size = Size::normal((data >> 6) as u8 & 3);
            Ok(Miscellaneous::Not(size, mode))
        } else if (data >> 7) & 0b111 == 0b001 {
            let direction = (data >> 10) & 1 != 0;
            let size = Size::single_bit((data >> 6) & 1 != 0);
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::MoveM(direction, size, mode))
        } else if (data >> 4) & 0xFF == 0b1110_0110 {
            let direction = (data >> 3) & 1 != 0;
            let reg = AddrReg::new(data as u8 & 0x7);
            Ok(Miscellaneous::MoveUsp(direction, reg))
        } else if data >> 8 == 0b0100_0010 {
            let size = Size::normal(data as u8 >> 6);
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::Clear(size, mode))
        } else {
            unimplemented!(
                "Word: {:04b} {:04b} {:04b} {:04b}",
                (data >> 12) & 0xF,
                (data >> 8) & 0xF,
                (data >> 4) & 0xF,
                (data) & 0xF,
            );
        }
    }
}

pub struct Branch {
    pub condition: Condition,
    pub data: u8,
}

impl fmt::Debug for Branch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "b{} ", self.condition.short_name())?;
        match self.data {
            0x00 => write!(f, "word offset"),
            0xFF => write!(f, "long offset"),
            d => {
                let d = d as i8;
                if d < 0 {
                    write!(f, "-{:#X}", -(d as i16))
                } else {
                    write!(f, "+{:#X}", d)
                }
            }
        }
    }
}

impl TryFrom<u16> for Branch {
    type Error = String;
    fn try_from(data: u16) -> Result<Branch, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b0110 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        let condition = Condition::from_u16((data >> 8) & 0xF).unwrap();

        Ok(Branch {
            condition,
            data: data as u8,
        })
    }
}

impl Branch {
    fn disp_length(&self) -> u32 {
        match self.data {
            0xFF => 4,
            0x00 => 2,
            _ => 0,
        }
    }

    fn displacement(&self, cpu: &mut Cpu) -> i32 {
        match self.data {
            0xFF => cpu.read(cpu.core.pc + 2, Size::Long) as i32,
            0x00 => cpu.read(cpu.core.pc + 2, Size::Word) as i16 as i32,
            _ => self.data as i8 as i32,
        }
    }
}

impl Instr for Branch {
    fn size(&self) -> u32 {
        if self.disp_length() != 0 {
            2 + self.disp_length() as u32
        } else {
            2
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        let displacement = self.displacement(cpu);

        if self.condition == Condition::False {
            cpu.core.addr[7] = cpu.core.addr[7].wrapping_sub(4);
            cpu.write(cpu.core.addr[7], cpu.core.pc + self.size(), Size::Long);

            cpu.core.pc = cpu
                .core
                .pc
                .wrapping_add(displacement.wrapping_add(2) as u32);

            if LOG_INSTR {
                println!(
                    "Branch (to subroutine) taken, PC will be {:#X}",
                    cpu.core.pc
                );
            }

            cpu.core.pc = cpu.core.pc.wrapping_sub(self.size());
        } else if self.condition.check(cpu.core.ccr) {
            cpu.core.pc = cpu
                .core
                .pc
                .wrapping_add(displacement.wrapping_add(2) as u32);
            if LOG_INSTR {
                println!("Branch taken, PC will be {:#X}", cpu.core.pc);
            }

            // Kind of a hack to cancel out the auto-increment
            cpu.core.pc = cpu.core.pc.wrapping_sub(self.size());
        }
    }
}

pub struct Move {
    size: Size,
    from_mode: AddrMode,
    to_mode: AddrMode,
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "move.{} {:?},{:?}",
            self.size.suffix(),
            self.from_mode,
            self.to_mode
        )
    }
}

impl TryFrom<u16> for Move {
    type Error = String;

    fn try_from(data: u16) -> Result<Move, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble == 0b0000 || top_nybble > 0b0011 {
            return Err(format!("Invalid top nybble: {:#b}", data));
        }

        let mut to_mode = (data >> 6) as u8 & 0x3F;
        to_mode = (to_mode >> 3) | ((to_mode & 0x7) << 3);
        let to_mode = AddrMode::new(to_mode);
        let from_mode = AddrMode::new(data as u8 & 0x3F);

        let size = Size::higher((data >> 12) as u8 & 3);

        Ok(Move {
            size,
            to_mode,
            from_mode,
        })
    }
}

impl Instr for Move {
    fn size(&self) -> u32 {
        2 + self.to_mode.arg_length(self.size) + self.from_mode.arg_length(self.size)
    }

    fn execute(&self, cpu: &mut Cpu) {
        let value = self.from_mode.read(false, self.size, cpu);

        if let AddrMode::AddrReg(_) = self.to_mode {
            // Don't set flags for a MOVEA
        } else {
            cpu.core.ccr.set_overflow(false);
            cpu.core.ccr.set_carry(false);
            cpu.core.ccr.set_zero(value == 0);
            cpu.core.ccr.set_negative_sized(value, self.size);
        }

        let has_immediate = if let AddrMode::Immediate = self.from_mode {
            true
        } else {
            false
        };

        if let AddrMode::AddrReg(reg) = self.to_mode {
            let value_ext = if self.size == Size::Word {
                value as i16 as i32 as u32
            } else {
                value
            };
            AddrReg::new(reg).write(value_ext, cpu);
        } else {
            self.to_mode.write(value, has_immediate, self.size, cpu);
        }
    }
}

pub struct MoveQ {
    pub reg: DataReg,
    pub value: i8,
}

impl fmt::Debug for MoveQ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "moveq #{},d{}", self.value, self.reg.into_inner())
    }
}

impl TryFrom<u16> for MoveQ {
    type Error = String;

    fn try_from(data: u16) -> Result<MoveQ, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b0111 {
            return Err(format!("Invalid top nybble: {:#b}", top_nybble));
        }

        if (data >> 8) & 1 != 0 {
            return Err("Invalid bit for MOVEQ".to_string());
        }

        let reg = DataReg::new((data >> 9) as u8 & 0x7);
        let value = data as i8;

        Ok(MoveQ { reg, value })
    }
}

impl Instr for MoveQ {
    fn size(&self) -> u32 {
        2
    }

    fn execute(&self, cpu: &mut Cpu) {
        cpu.core.ccr.set_overflow(false);
        cpu.core.ccr.set_carry(false);
        cpu.core.ccr.set_zero(self.value == 0);
        cpu.core.ccr.set_negative(self.value < 0);

        let new_value = self.value as i32 as u32;

        self.reg.write(new_value, cpu);
    }
}

pub struct LinearOp {
    pub is_add: bool,
    pub size: Size,
    pub op_type: LinearOpType,
}

impl fmt::Debug for LinearOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_add {
            write!(f, "add")?;
        } else {
            write!(f, "sub")?;
        }

        match self.op_type {
            LinearOpType::Normal(_, _, _) => write!(f, "")?,
            LinearOpType::WithExtend(_) => write!(f, "x")?,
            LinearOpType::Address(_, _) => write!(f, "a")?,
        }

        write!(f, ".{} ", self.size.suffix())?;

        match &self.op_type {
            LinearOpType::Normal(dir, reg, ea) => {
                if *dir {
                    write!(f, "d{},{:?}", reg.into_inner(), ea)?;
                } else {
                    write!(f, "{:?},d{}", ea, reg.into_inner())?;
                }
            }
            LinearOpType::Address(reg, ea) => {
                write!(f, "{:?},a{}", ea, reg.into_inner())?;
            }
            _ => write!(f, "data: {:?}", self.op_type)?,
        }

        Ok(())
    }
}

#[derive(Debug)]
pub enum LinearOpType {
    // bool is Direction
    Normal(bool, DataReg, AddrMode),
    // Mode is either data register,
    // or address register with predecrement
    WithExtend(Either<(DataReg, DataReg), (AddrReg, AddrReg)>),
    Address(AddrReg, AddrMode),
}

fn compare(dst: u32, src: u32, size: Size, is_add: bool) -> Ccr {
    let masked_dst = dst & size.mask();
    let masked_src = src & size.mask();

    let result = if is_add {
        masked_dst.wrapping_add(masked_src)
    } else {
        masked_dst.wrapping_sub(masked_src)
    };
    let masked_result = result & size.mask();

    let dst_sign = size.negative_sized(dst);
    let src_sign = size.negative_sized(src);
    let result_sign = size.negative_sized(result);

    let overflow = if is_add {
        (src_sign && dst_sign && !result_sign) || (!src_sign && !dst_sign && result_sign)
    } else {
        // This is true on either:
        // Negative - Positive -> Positive
        // Positive - Negative -> Negative
        (!src_sign && dst_sign && !result_sign) || (src_sign && !dst_sign && result_sign)
    };

    let carry = if is_add {
        (src_sign && dst_sign) || (!result_sign && dst_sign) || (src_sign && !result_sign)
    } else {
        (src_sign && !dst_sign) || (result_sign && !dst_sign) || (src_sign && result_sign)
    };

    if LOG_INSTR {
        println!(
            "Compared dst: {:#X} - src {:#X} = {:#X}",
            dst, src, masked_result
        );
    }

    let mut ccr = Ccr(0);
    ccr.set_extend(carry);
    ccr.set_carry(carry);
    ccr.set_zero(masked_result == 0);
    ccr.set_negative(result_sign);
    ccr.set_overflow(overflow);

    ccr
}

impl LinearOp {
    pub fn do_op(is_add: bool, dst: u32, src: u32, size: Size) -> (u32, Ccr) {
        let result = if is_add {
            (dst & size.mask()).wrapping_add(src & size.mask())
        } else {
            (dst & size.mask()).wrapping_sub(src & size.mask())
        };

        let sized_result = (dst & !size.mask()) | (result & size.mask());
        let ccr = compare(dst, src, size, is_add);

        (sized_result, ccr)
    }
}

impl TryFrom<u16> for LinearOp {
    type Error = String;

    fn try_from(data: u16) -> Result<LinearOp, Self::Error> {
        let top_nybble = data >> 12;
        if !(top_nybble == 0b1001 || top_nybble == 0b1101) {
            return Err(format!("Invalid top nybble: {:#b}", top_nybble));
        }

        let is_add = (data >> 14) & 1 != 0;

        if (data >> 6) & 3 == 0b11 {
            let mode = AddrMode::new(data as u8 & 0x3F);
            let reg = AddrReg::new((data >> 9) as u8 & 0x7);
            let size = Size::single_bit((data >> 8) & 1 != 0);
            let op_type = LinearOpType::Address(reg, mode);

            Ok(LinearOp {
                is_add,
                size,
                op_type,
            })
        } else if bitpat!(1 _ _ 0 0)((data >> 4) & 0b11111) {
            let size = Size::normal((data >> 6) as u8 & 3);
            let to_reg = (data >> 9) as u8 & 0x7;
            let from_reg = data as u8 & 0x7;
            let mode = if (data >> 3) & 1 != 0 {
                Either::Left((DataReg::new(to_reg), DataReg::new(from_reg)))
            } else {
                Either::Right((AddrReg::new(to_reg), AddrReg::new(from_reg)))
            };

            let op_type = LinearOpType::WithExtend(mode);

            Ok(LinearOp {
                is_add,
                size,
                op_type,
            })
        } else {
            let size = Size::normal((data >> 6) as u8 & 3);
            let to_reg = DataReg::new((data >> 9) as u8 & 7);
            let mode = AddrMode::new(data as u8 & 0x3F);
            let direction = (data >> 8) & 1 != 0;

            let op_type = LinearOpType::Normal(direction, to_reg, mode);

            Ok(LinearOp {
                is_add,
                size,
                op_type,
            })
        }
    }
}

impl Instr for LinearOp {
    fn size(&self) -> u32 {
        2 + match &self.op_type {
            LinearOpType::Normal(_, _, mode) => mode.arg_length(self.size),
            LinearOpType::Address(_, mode) => mode.arg_length(self.size),
            LinearOpType::WithExtend(_) => 0,
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match &self.op_type {
            LinearOpType::Normal(dir, reg, mode) => {
                let (dst, src) = if *dir {
                    (mode.read(false, self.size, cpu), reg.read(cpu))
                } else {
                    (reg.read(cpu), mode.read(false, self.size, cpu))
                };

                let (result, ccr) = LinearOp::do_op(self.is_add, dst, src, self.size);
                cpu.core.ccr = ccr;

                if *dir {
                    mode.write(result, false, self.size, cpu)
                } else {
                    reg.write(result, cpu)
                }
            }
            LinearOpType::Address(reg, mode) => reg.write_sized(
                reg.read(cpu) + mode.read(false, self.size, cpu),
                self.size,
                cpu,
            ),
            _ => unimplemented!("Linear mode {:?}", self.op_type),
        }
    }
}

#[derive(Debug)]
pub enum ConditionsQuicks {
    // Bool is true if it's a subtraction
    AddSubQ(bool, u8, Size, AddrMode),
    Set(Condition, AddrMode),
    DecBranch(Condition, DataReg),
}

impl Instr for ConditionsQuicks {
    fn size(&self) -> u32 {
        2 + match self {
            ConditionsQuicks::AddSubQ(_, _, size, mode) => mode.arg_length(*size),
            ConditionsQuicks::Set(_, mode) => mode.arg_length(Size::Byte),
            ConditionsQuicks::DecBranch(_, _) => 2,
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match self {
            ConditionsQuicks::AddSubQ(is_sub, data, size, mode) => {
                let dst = mode.read(false, *size, cpu);
                let arg = if *data == 0 { 8 } else { *data };
                let (result, ccr) = LinearOp::do_op(!is_sub, dst, arg as u32, *size);
                mode.write(result, false, *size, cpu);
                if let AddrMode::AddrReg(_) = mode {
                    // Don't set flags
                } else {
                    cpu.core.ccr = ccr;
                }
            }
            ConditionsQuicks::DecBranch(condition, reg) => {
                if condition.check(cpu.core.ccr) {
                    // Continue
                } else {
                    let mut low_word = reg.read(cpu) as i16;
                    low_word = low_word.wrapping_sub(1);
                    reg.write_sized(low_word as u32, Size::Word, cpu);

                    if LOG_INSTR {
                        println!("Reg is now {:#b}", reg.read(cpu));
                    }

                    if low_word != -1 {
                        let disp = cpu.read(cpu.core.pc + 2, Size::Word) as i16;
                        let new_pc = (cpu.core.pc as i16 + disp + 2) as u16;
                        cpu.core.pc &= !0xFF_FF;
                        cpu.core.pc |= new_pc as u32;

                        // To adjust for the increment after the instruction
                        cpu.core.pc -= self.size();
                    }
                }
            }
            ConditionsQuicks::Set(condition, mode) => {
                let value = if condition.check(cpu.core.ccr) {
                    0xFF
                } else {
                    0x00
                };

                mode.write(value, false, Size::Byte, cpu);
            }
        }
    }
}

impl TryFrom<u16> for ConditionsQuicks {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b0101 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        let size = (data as u8) >> 6;
        if size == 0b11 {
            let condition = Condition::from_u16((data >> 8) & 0xF).unwrap();

            if (data >> 3) & 7 == 0b001 {
                let reg = DataReg::new(data as u8 & 0x7);
                Ok(ConditionsQuicks::DecBranch(condition, reg))
            } else {
                let mode = AddrMode::new(data as u8 & 0x3F);
                Ok(ConditionsQuicks::Set(condition, mode))
            }
        } else {
            let size = Size::normal(size);
            let is_sub = (data >> 8) & 1 != 0;
            let value = (data >> 9) as u8 & 7;
            let mode = AddrMode::new(data as u8 & 0x3F);
            Ok(ConditionsQuicks::AddSubQ(is_sub, value, size, mode))
        }
    }
}

#[derive(Debug)]
pub enum CompareMode {
    // Dest reg, source reg
    Memory(AddrReg, AddrReg),
    Normal(DataReg, AddrMode),
    Address(AddrReg, AddrMode),
}

#[derive(Debug)]
pub struct CompareEor {
    pub size: Size,
    pub data: Either<CompareMode, (DataReg, AddrMode)>,
}

impl TryFrom<u16> for CompareEor {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b1011 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        if bitpat!(1 _ _ 0 0 1)(data >> 3) {
            // TODO: These may be flipped
            let dest_reg = AddrReg::new((data >> 9) as u8 & 0x7);
            let source_reg = AddrReg::new(data as u8 & 0x7);
            let mode = CompareMode::Memory(dest_reg, source_reg);
            let size = Size::normal((data >> 6) as u8 & 0x7);
            let data = Either::Left(mode);
            Ok(CompareEor { size, data })
        } else if (data >> 6) & 0b11 == 0b11 {
            let dest_reg = AddrReg::new((data >> 9) as u8 & 0x7);
            let source_mode = AddrMode::new(data as u8 & 0x3F);
            let size = Size::single_bit((data >> 8) & 1 != 0);
            let mode = CompareMode::Address(dest_reg, source_mode);
            let data = Either::Left(mode);
            Ok(CompareEor { size, data })
        } else if (data >> 8) & 1 == 0 {
            let dest_reg = DataReg::new((data >> 9) as u8 & 0x7);
            let source_mode = AddrMode::new(data as u8 & 0x3F);
            let size = Size::normal((data >> 6) as u8 & 3);
            let mode = CompareMode::Normal(dest_reg, source_mode);
            let data = Either::Left(mode);
            Ok(CompareEor { size, data })
        } else {
            let size = Size::normal((data >> 6) as u8 & 3);
            let source_mode = AddrMode::new(data as u8 & 0x3F);
            let dest_reg = DataReg::new((data >> 9) as u8 & 0x7);
            let data = Either::Right((dest_reg, source_mode));
            Ok(CompareEor { size, data })
        }
    }
}

impl Instr for CompareEor {
    fn size(&self) -> u32 {
        match &self.data {
            Either::Left(mode) => match mode {
                CompareMode::Address(_, source_mode) | CompareMode::Normal(_, source_mode) => {
                    2 + source_mode.arg_length(self.size)
                }
                CompareMode::Memory(_, _) => 2,
            },
            Either::Right((_, mode)) => 2 + mode.arg_length(self.size),
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match &self.data {
            Either::Left(mode) => match mode {
                CompareMode::Normal(dest_reg, source_mode) => {
                    let dest = dest_reg.read(cpu);
                    let source = source_mode.read(false, self.size, cpu);
                    let new_ccr = compare(dest, source, self.size, false);
                    cpu.core.ccr = new_ccr;
                }
                _ => unimplemented!(),
            },
            Either::Right((reg, mode)) => {
                let mut value = reg.read(cpu);
                value ^= mode.read(false, self.size, cpu) & self.size.mask();
                reg.write(value, cpu);

                cpu.core.ccr.set_zero(value == 0);
                cpu.core.ccr.set_negative_sized(value, self.size);
            }
        }
    }
}

#[derive(Debug, FromPrimitive, PartialEq)]
pub enum ShiftType {
    Arithmetic = 0b00,
    Logical = 0b01,
    RotateExtend = 0b10,
    Rotate = 0b11,
}

impl ShiftType {
    fn prefix(&self) -> &'static str {
        match self {
            ShiftType::Arithmetic => "as",
            ShiftType::Logical => "ls",
            ShiftType::RotateExtend => "rox",
            ShiftType::Rotate => "ro",
        }
    }
}

pub struct Shifts {
    // Right is false, left is true
    direction: bool,
    shift_type: ShiftType,
    data: Either<(Size, Either<u8, DataReg>, DataReg), AddrMode>,
}

impl fmt::Debug for Shifts {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.shift_type.prefix())?;
        if self.direction {
            write!(f, "l")?;
        } else {
            write!(f, "r")?;
        }

        write!(f, " {:?}", self.data)
    }
}

impl Shifts {
    // Left is thing being shifted, right is shift amount
    fn get_args(&self, cpu: &mut Cpu) -> (u32, u32) {
        match &self.data {
            Either::Left((_, amount, shiftee)) => {
                let amount = amount.either(
                    // Shift count of zero actually represents 8
                    |val| if val == 0 { 8 } else { val as u32 },
                    |reg| reg.read(cpu) % 64,
                );
                (shiftee.read(cpu), amount)
            }
            Either::Right(mode) => (mode.read(false, Size::Word, cpu), 1),
        }
    }

    fn get_size(&self) -> Size {
        if let Either::Left((size, _, _)) = &self.data {
            *size
        } else {
            Size::Word
        }
    }

    fn write_back(&self, value: u32, cpu: &mut Cpu) {
        match &self.data {
            Either::Left((size, _, shiftee)) => {
                shiftee.write_sized(value, *size, cpu);
            }
            Either::Right(mode) => {
                mode.write(value, false, Size::Word, cpu);
            }
        }
    }
}

impl Instr for Shifts {
    fn size(&self) -> u32 {
        match &self.data {
            Either::Left((_, _, _)) => 2,
            Either::Right(mode) => 2 + mode.arg_length(Size::Word),
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        let (mut value, amount) = self.get_args(cpu);

        let width = self.get_size().len() * 8;
        let top_bit_mask = 1 << (width - 1);

        if self.direction {
            match &self.shift_type {
                // TODO: These are probably all wrong
                ShiftType::Logical | ShiftType::Arithmetic => {
                    if self.direction {
                        cpu.core.ccr.set_carry(false);

                        let mut overflow_occurred = false;
                        for _ in 0..amount {
                            let prev_value = value;
                            value <<= 1;
                            let prev_sign = self.get_size().negative_sized(prev_value);
                            let new_sign = self.get_size().negative_sized(value);
                            if prev_sign != new_sign {
                                overflow_occurred = true;
                            }
                            cpu.core.ccr.set_carry(prev_sign);
                            cpu.core.ccr.set_extend(prev_sign);
                        }

                        let new_overflow = if self.shift_type == ShiftType::Arithmetic {
                            overflow_occurred
                        } else {
                            false
                        };

                        cpu.core.ccr.set_overflow(new_overflow);
                    } else {
                        unimplemented!()
                    }
                }
                ShiftType::Rotate => {
                    cpu.core.ccr.set_carry(false);
                    cpu.core.ccr.set_overflow(false);

                    for _ in 0..amount {
                        let msb = value & top_bit_mask != 0;
                        let bottom_bit = msb as u32;
                        value <<= 1;
                        value |= bottom_bit;
                        cpu.core.ccr.set_carry(msb);
                    }
                }
                ShiftType::RotateExtend => {
                    cpu.core.ccr.set_overflow(false);
                    cpu.core.ccr.set_carry(false);
                    cpu.core.ccr.set_extend(false);

                    for _ in 0..amount {
                        let new_bit = (top_bit_mask & value) != 0;

                        value <<= 1;
                        value |= cpu.core.ccr.extend() as u32;

                        cpu.core.ccr.set_carry(new_bit);
                        cpu.core.ccr.set_extend(new_bit);
                    }
                }
            }
        } else {
            match &self.shift_type {
                ShiftType::Logical => {
                    cpu.core.ccr.set_carry(false);
                    cpu.core.ccr.set_overflow(false);

                    for _ in 0..amount {
                        let lsb = value & 1 != 0;
                        value = (value & self.get_size().mask()) >> 1;
                        cpu.core.ccr.set_carry(lsb);
                        cpu.core.ccr.set_extend(lsb);
                    }
                }
                ShiftType::Rotate => {
                    cpu.core.ccr.set_carry(false);
                    cpu.core.ccr.set_overflow(false);

                    for _ in 0..amount {
                        let lsb = value & 1 != 0;
                        let top_bit = top_bit_mask * (lsb as u32);
                        value >>= 1;
                        value &= !top_bit_mask;
                        value |= top_bit;
                        cpu.core.ccr.set_carry(lsb);
                    }
                }
                a => unimplemented!("Shift right {:?}", a),
            }
        }

        cpu.core.ccr.set_zero(value & self.get_size().mask() == 0);
        cpu.core.ccr.set_negative_sized(value, self.get_size());

        self.write_back(value, cpu);
    }
}

impl TryFrom<u16> for Shifts {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b1110 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        let direction = data & (1 << 8) != 0;
        let raw_size = (data >> 6) as u8 & 0b11;

        if raw_size == 0b11 {
            let mode = AddrMode::new(data as u8 & 0x3F);
            let shift_type = ShiftType::from_u16((data >> 9) & 0b111).unwrap();

            let data = Either::Right(mode);

            Ok(Shifts {
                direction,
                shift_type,
                data,
            })
        } else {
            let size = Size::normal(raw_size);

            let source = (data >> 9) as u8 & 0x7;
            let source = if data & (1 << 5) != 0 {
                // Source is a register
                Either::Right(DataReg::new(source))
            } else {
                Either::Left(source)
            };

            let data_reg = DataReg::new(data as u8 & 0x7);
            let shift_type = ShiftType::from_u16((data >> 3) & 0b11).unwrap();

            let data = Either::Left((size, source, data_reg));

            Ok(Shifts {
                direction,
                shift_type,
                data,
            })
        }
    }
}

// TODO: Implement these
#[allow(dead_code)]
#[derive(Debug)]
pub enum DivSubdOr {
    // True means signed
    Div(DataReg, bool, AddrMode),
    Subd(u8, AddrMode),
    // False means Dn | <ea> -> Dn
    // True means <ea> | Dn -> <ea>
    Or(DataReg, bool, Size, AddrMode),
}

impl TryFrom<u16> for DivSubdOr {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b1000 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        if (data >> 4) & 0x1F == 0b1_00_00 {
            unimplemented!("Subd")
        } else if (data >> 6) & 0x3 == 0b11 {
            unimplemented!("Divu, Divs")
        } else {
            let data_reg = DataReg::new((data >> 9) as u8 & 0x7);
            let direction = (data >> 8) & 1 != 0;
            let size = Size::normal((data >> 6) as u8 & 0x3);
            let mode = AddrMode::new(data as u8 & 0x3F);

            Ok(DivSubdOr::Or(data_reg, direction, size, mode))
        }
    }
}

impl Instr for DivSubdOr {
    fn size(&self) -> u32 {
        match self {
            DivSubdOr::Div(_, _, mode) => 2 + mode.arg_length(Size::Word),
            DivSubdOr::Subd(_, mode) => 2 + mode.arg_length(Size::Byte),
            DivSubdOr::Or(_, _, size, mode) => 2 + mode.arg_length(*size),
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match self {
            DivSubdOr::Div(_, _, _) => unimplemented!(),
            DivSubdOr::Subd(_, _) => unimplemented!(),
            DivSubdOr::Or(data_reg, direction, size, mode) => {
                let arg = mode.read(false, *size, cpu);
                let result = data_reg.read(cpu) | arg;

                cpu.core.ccr.set_negative_sized(result, *size);
                cpu.core.ccr.set_zero(result == 0);
                cpu.core.ccr.set_overflow(false);
                cpu.core.ccr.set_carry(false);

                if *direction {
                    mode.write(result, false, *size, cpu);
                } else {
                    let mask = size.mask();
                    data_reg.write((result & mask) | (data_reg.read(cpu) & !mask), cpu);
                }
            }
        }
    }
}

// TODO: Implement these
#[allow(dead_code)]
#[derive(Debug)]
pub enum MulAddExgAnd {
    // True means signed
    Mul(bool, DataReg, AddrMode),
    Abcd,
    Exchange,
    And(DataReg, bool, Size, AddrMode),
}

impl TryFrom<u16> for MulAddExgAnd {
    type Error = String;

    fn try_from(data: u16) -> Result<Self, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b1100 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        if (data >> 4) & 0x1F == 0b1_00_00 {
            unimplemented!("Abcd")
        } else if (data >> 6) & 0x3 == 0b11 {
            unimplemented!("Mulu and Muls")
        } else if bitpat!(1 _ _ 0 0)(data >> 4) {
            unimplemented!("Exchange")
        } else {
            let data_reg = DataReg::new((data >> 9) as u8 & 0x7);
            let direction = (data >> 8) & 1 != 0;
            let size = Size::normal((data >> 6) as u8 & 0x3);
            let mode = AddrMode::new(data as u8 & 0x3F);

            Ok(MulAddExgAnd::And(data_reg, direction, size, mode))
        }
    }
}

impl Instr for MulAddExgAnd {
    fn size(&self) -> u32 {
        match self {
            MulAddExgAnd::And(_, _, size, mode) => 2 + mode.arg_length(*size),
            _ => unimplemented!(),
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match self {
            MulAddExgAnd::And(data_reg, direction, size, mode) => {
                let arg = mode.read(false, *size, cpu);
                let result = data_reg.read(cpu) & arg;

                cpu.core.ccr.set_negative_sized(result, *size);
                cpu.core.ccr.set_zero(result == 0);
                cpu.core.ccr.set_overflow(false);
                cpu.core.ccr.set_carry(false);

                if *direction {
                    mode.write(result, false, *size, cpu);
                } else {
                    let mask = size.mask();
                    data_reg.write((result & mask) | (data_reg.read(cpu) & !mask), cpu);
                }
            }
            _ => unimplemented!(),
        }
    }
}

macro_rules! standalone_operation {
    ($name:ident, fn $func_name:ident (&$self_name:ident, $cpu_name:ident : &mut Cpu) $body:block) => {
        #[derive(Debug)]
        pub struct $name;

        impl pages::Instr for $name {
            fn size(&self) -> u32 {
                2
            }

            fn $func_name(&$self_name, $cpu_name : &mut Cpu) {
                $body
            }
        }
    }
}

standalone_operation!(Illegal, fn execute(&self, _cpu: &mut Cpu) { panic!("Encountered illegal opcode") });
standalone_operation!(Reset,   fn execute(&self, _cpu: &mut Cpu) { unimplemented!("Reset") });
standalone_operation!(Nop,     fn execute(&self, _cpu: &mut Cpu) { });
standalone_operation!(Stop,    fn execute(&self, _cpu: &mut Cpu) { unimplemented!("Stop") });
standalone_operation!(ExceptionReturn, fn execute(&self, cpu: &mut Cpu) { 
    let new_sr = cpu.read(cpu.core.addr[7], Size::Word) as u16;
    cpu.core.addr[7] = cpu.core.addr[7].wrapping_add(2);
    let new_pc = cpu.read(cpu.core.addr[7], Size::Long);
    cpu.core.addr[7] = cpu.core.addr[7].wrapping_add(4);

    cpu.core.pc = new_pc;
    cpu.core.sr    = (new_sr >> 8) as u8;
    cpu.core.ccr.0 = (new_sr     ) as u8;

    if LOG_INSTR { println!("Return from exception, PC will be {:#X}", cpu.core.pc) }

    cpu.core.pc = cpu.core.pc.wrapping_sub(self.size());
});

standalone_operation!(Return, fn execute(&self, cpu: &mut Cpu) {
    let new = cpu.read(cpu.core.addr[7], Size::Long);
    cpu.core.addr[7] = cpu.core.addr[7].wrapping_add(4);

    cpu.core.pc = new;

    if LOG_INSTR { println!("Returning, PC will be {:#X}", cpu.core.pc) }

    cpu.core.pc = cpu.core.pc.wrapping_sub(self.size());
});
standalone_operation!(TrapOverflow, fn execute(&self, _cpu: &mut Cpu) { unimplemented!("Trap overflow") });
standalone_operation!(RestoreReturn, fn execute(&self, _cpu: &mut Cpu) { unimplemented!("Restore return") });

/// Instructions on the 68k can be roughly divided into 'pages'
/// based on the high nybble of their encoding.

#[derive(Debug)]
pub struct Instruction {
    pub opcode: Pages,
}

impl Instruction {
    pub fn new(word: u16) -> Instruction {
        Instruction {
            opcode: Pages::try_from(word).unwrap(),
        }
    }

    pub fn execute(&self, cpu: &mut Cpu) {
        self.opcode.execute(cpu);

        cpu.core.pc = cpu.core.pc.wrapping_add(self.opcode.size());
    }
}
