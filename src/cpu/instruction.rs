use num_traits::FromPrimitive;
use super::{Cpu, Ccr};
use either::Either;
use std::convert::TryFrom;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Address {
    DataReg(u8),
    AddrReg(u8),
    Address(u32),
}

impl Address {
    pub fn read(&self, size: &Size, cpu: &mut Cpu) -> u32 {
        let mask = match size {
            Size::Byte => 0xFF,
            Size::Word => 0xFF_FF,
            Size::Long => 0xFF_FF_FF_FF,
        };

        match self {
            Address::DataReg(reg) => cpu.core.data[*reg as usize] & mask,
            Address::AddrReg(reg) => cpu.core.addr[*reg as usize] & mask,
            Address::Address(addr) => cpu.read(*addr, size),
        }
    }

    pub fn write(&self, value: u32, size: &Size, cpu: &mut Cpu) {
        let mask = match size {
            Size::Byte => 0xFF,
            Size::Word => 0xFF_FF,
            Size::Long => 0xFF_FF_FF_FF,
        };

        match self {
            Address::DataReg(reg) => {
                let reg = &mut cpu.core.data[*reg as usize];
                *reg &= !mask;
                *reg |= value & mask;
            }
            Address::AddrReg(reg) => {
                let reg = &mut cpu.core.addr[*reg as usize];
                *reg &= !mask;
                *reg |= value & mask;
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
            7 => {
                match field {
                    2 => AddrMode::PcDisp,
                    3 => AddrMode::PcIndex,
                    0 => AddrMode::AbsShort,
                    1 => AddrMode::AbsLong,
                    4 => AddrMode::Immediate,
                    _ => panic!()
                }
            }
            _ => unreachable!()
        }
    }

    pub fn address(&self, has_immediate: bool, size: &Size, cpu: &mut Cpu) -> Address {
        let extra = if has_immediate { (size.len() as u32 + 1) / 2 * 2} else { 0 };
        match self {
            AddrMode::AbsLong => {
                Address::Address(cpu.read(cpu.core.pc + extra + 2, &Size::Long))
            }
            AddrMode::AbsShort => {
                let address = cpu.read(cpu.core.pc + extra + 2, &Size::Word) as i16;

                if address < 0 {
                    Address::Address(0xFF_FF_FF_FF - (-address) as u32 + 1)
                } else {
                    Address::Address(address as u32)
                }
            }
            AddrMode::PcDisp => {
                // TODO: Is the displacement signed?
                let offset = cpu.read(cpu.core.pc + extra + 2, &Size::Word);
                Address::Address(cpu.core.pc + 2 + offset)
            }
            AddrMode::DataReg(reg) => {
                Address::DataReg(*reg)
            }
            AddrMode::AddrReg(reg) => {
                Address::AddrReg(*reg)
            }
            AddrMode::AddrPreDecr(reg) |
            AddrMode::AddrPostIncr(reg) |
            AddrMode::Addr(reg) => {
                Address::Address(cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize])
            }
            AddrMode::AddrDisp(reg) => {
                let offset = cpu.read(cpu.core.pc + 2 + extra, &Size::Word) as i16;
                let reg = cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize];

                let address = if offset < 0 {
                    reg.wrapping_sub((-offset) as u32)
                } else {
                    reg.wrapping_add(offset as u32)
                };

                // TODO: Can I just cast as i32
                Address::Address(address)
            }
            AddrMode::Immediate => {
                Address::Address(cpu.core.pc + 2 + extra)
            }
            _ => unimplemented!("Mode: {:?}", self)
        }
    }
    
    pub fn read(&self, has_immediate: bool, size: &Size, cpu: &mut Cpu) -> u32 {
        if let AddrMode::AddrPreDecr(reg) = self {
            let reg = &mut cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize];
            *reg = reg.wrapping_sub(size.len() as u32);
        }
        let address = self.address(has_immediate, size, cpu);
        if let AddrMode::AddrPostIncr(reg) = self {
            cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize] += size.len() as u32;
        }
        address.read(size, cpu)
    }

    pub fn write(&self, value: u32, has_immediate: bool, size: &Size, cpu: &mut Cpu) {
        if let AddrMode::AddrPreDecr(reg) = self {
            let reg = &mut cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize];
            *reg = reg.wrapping_sub(size.len() as u32);
        }
        let address = self.address(has_immediate, size, cpu);
        if let AddrMode::AddrPostIncr(reg) = self {
            let reg = &mut cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize];
            *reg = reg.wrapping_add(size.len() as u32);
        }
        address.write(value, size, cpu)
    }

    pub fn arg_length(&self, size: &Size) -> u32 {
        match self {
            AddrMode::Immediate => if size == &Size::Long { 4 } else { 2 }

            AddrMode::DataReg(_) |
            AddrMode::AddrReg(_) |
            AddrMode::AddrPreDecr(_) |
            AddrMode::AddrPostIncr(_) |
            AddrMode::Addr(_) => 0,

            AddrMode::AbsLong => 4,

            AddrMode::AbsShort | 
            AddrMode::PcDisp |
            AddrMode::AddrDisp(_) => 2,
            _ => unimplemented!("mode: {:?}", self)
        }
    }
}

#[derive(Debug, FromPrimitive, PartialEq)]
pub enum Condition {
    True   = 0b0000,
    False  = 0b0001,
    Higher = 0b0010,
    LowerOrSame,
    CarryClear,
    CarrySet,
    NotEqual,
    Equal,
    OverflowClear,
    OverflowSet,
    Plus,
    Minus,
    GreaterOrEqual,
    LessThan,
    GreaterThan,
    LessOrEqual
}

impl Condition {
    pub fn check(&self, ccr: &Ccr) -> bool {
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
            Condition::GreaterOrEqual => (ccr.negative() && ccr.overflow()) || (!ccr.negative() && !ccr.overflow()),
            Condition::LessThan => (ccr.negative() && !ccr.overflow()) || (!ccr.negative() && ccr.overflow()),
            Condition::GreaterThan => (ccr.negative() && ccr.overflow() && ccr.zero()) || (!ccr.negative() && !ccr.overflow() && !ccr.zero()),
            Condition::LessOrEqual => ccr.zero() || (ccr.negative() && !ccr.overflow()) || (!ccr.negative() && ccr.overflow()),
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
    pub fn normal(data: u8) -> Size {
        match data {
            0 => Size::Byte,
            1 => Size::Word,
            2 => Size::Long,
            _ => panic!("Invalid size {}", data),
        }
    }

    pub fn single_bit(data: bool) -> Size {
        match data {
            false => Size::Word,
            true  => Size::Long,
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

    pub fn len(&self) -> usize {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::Long => 4,
        }
    }

    pub fn alignment(&self) -> usize {
        match self {
            Size::Byte => 1,
            Size::Word => 2,
            Size::Long => 2,
        }
    }
}

trait Instr: TryFrom<u16> {
    fn size(&self) -> u32;

    fn execute(&self, cpu: &mut Cpu);
}

#[derive(Debug, Clone, Copy)]
pub struct DataReg(u8);

impl DataReg {
    pub fn new(reg: u8) -> DataReg {
        assert!(reg < 8);
        DataReg(reg)
    }

    pub fn read(&self, cpu: &Cpu) -> u32 {
        cpu.core.data[self.0 as usize]
    }

    pub fn write(&self, value: u32, cpu: &mut Cpu) {
        cpu.core.data[self.0 as usize] = value;
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

    pub fn read(&self, cpu: &Cpu) -> u32 {
        cpu.core.data[self.0 as usize]
    }

    pub fn write(&self, value: u32, cpu: &mut Cpu) {
        cpu.core.data[self.0 as usize] = value;
    }
}

impl Into<u8> for AddrReg {
    fn into(self) -> u8 {
        self.0
    }
}

#[derive(Debug, FromPrimitive)]
pub enum SimpleOp {
    Or  = 0b000,
    And = 0b001,
    Sub = 0b010,
    Add = 0b011,
    Eor = 0b101,
    Cmp = 0b110,
}

impl SimpleOp {
    pub fn apply(&self, mut original: u32, arg: u32, size: &Size, mut old_ccr: Ccr) -> (u32, Ccr) { 
        let mask = match size {
            Size::Byte => 0xFF,
            Size::Word => 0xFF_FF,
            Size::Long => 0xFF_FF_FF_FF,
        };

        let result = match self {
            SimpleOp::Or => original | arg,
            SimpleOp::And => original & arg, 
            SimpleOp::Eor => original ^ arg,
            SimpleOp::Cmp => original,
            _ => unimplemented!("Op: {:?}", self)
        };

        original &= !mask;
        original |= result & mask;

        if let SimpleOp::Cmp = self {
            old_ccr = compare(original, arg, size);
        } else {
            old_ccr.set_overflow(false);
            old_ccr.set_carry(false);
            old_ccr.set_zero(result & mask == 0);
            old_ccr.set_negative({
                let value = result & mask;
                match size {
                    Size::Byte => (value as i8) < 0,
                    Size::Word => (value as i16) < 0,
                    Size::Long => (value as i32) < 0,
                }
            });
        }

        (original, old_ccr)
    }
}

#[derive(Debug, FromPrimitive)]
pub enum BitOpType {
    Test = 0,
    Change,
    Clear,
    Set
}

#[derive(Debug)]
pub struct ImmediatesBits {
    pub mode: AddrMode,

    // Left: Simple arithmetic operation
    // (ORI, ANDI, SUBI, EORI, CMPI)
    //
    // Right: Bit test/operation
    // (BTST, BCHG, BCLR, BSET)
    // If the register is Some, then it specifies the bit number to use
    // Otherwise, the bit number to use is specified in an immediate argument
    pub data: Either<(Size, SimpleOp), (Option<DataReg>, BitOpType)>,
}

impl Instr for ImmediatesBits {
    fn size(&self) -> u32 {
        // TODO: Figure out size properly
        let instr_size = self.data
            .as_ref()
            .either(
                |(size, _)| ((size.len() + 1) / 2) as u32 * 2 + self.mode.arg_length(size),
                |reg| match reg { 
                    (Some(_), _) => 0,
                    (None, _) => AddrMode::Immediate.arg_length(&Size::Byte) + self.mode.arg_length(&Size::Byte),
                }
            );

        instr_size + 2
    }
    
    fn execute(&self, cpu: &mut Cpu) {
        match &self.data {
            Either::Left((size, op)) => {
                let arg = cpu.read(cpu.core.pc + 2, size);
                let mut value = if self.use_ccr() {
                    unimplemented!()   
                } else if self.use_sr() {
                    (cpu.core.sr as u32) << 8 | (cpu.core.ccr.0 as u32)
                } else {
                    self.mode.read(false, size, cpu)
                };

                let (value, ccr) = op.apply(value, arg, size, cpu.core.ccr);

                if self.use_ccr() {
                    unimplemented!()
                } else if self.use_sr() {
                    // TODO: Should this affect the CCR? Should the shift be there?
                    cpu.core.sr = (value >> 8) as u8;
                } else {
                    self.mode.write(value, false, size, cpu);
                    cpu.core.ccr = ccr;
                }
            }
            Either::Right((reg, op_type)) => {
                let shift_amount = if let Some(data_reg) = reg {
                    data_reg.read(cpu)
                } else {
                    cpu.read(cpu.core.pc + 2, &Size::Byte)
                };

                if let AddrMode::DataReg(reg) = self.mode {
                    let reg = DataReg::new(reg);
                    let shift_amount = shift_amount % 32;
                    let bit = 1 << shift_amount;
                    cpu.core.ccr.set_zero(reg.read(cpu) & bit == 0);
                    match op_type {
                        BitOpType::Test => {},
                        BitOpType::Clear => reg.write(reg.read(cpu) & !bit, cpu),
                        BitOpType::Set => reg.write(reg.read(cpu) | bit, cpu),
                        BitOpType::Change => reg.write(reg.read(cpu) ^ bit, cpu)
                    }
                } else {
                    let shift_amount = shift_amount % 8;
                    let mut value = self.mode.read(false, &Size::Byte, cpu);
                    let bit = 1 << shift_amount;
                    cpu.core.ccr.set_zero(value & bit == 0);
                    match op_type {
                        BitOpType::Test => {},
                        BitOpType::Clear => value &= !bit,
                        BitOpType::Set => value |= bit,
                        BitOpType::Change => value ^= bit,
                    }
                }
            }
        }
    }
}

impl TryFrom<u16> for ImmediatesBits {
    type Error = String;

    fn try_from(data: u16) -> Result<ImmediatesBits, Self::Error> {
        if data >> 12 != 0 {
            return Err(format!("Incorrect top nybble {}", data >> 12));
        }

        let mode = AddrMode::new(data as u8 & 0x3F);

        let is_bitop_reg = (data >> 8) & 1 != 0;
        let is_bitop_imm = (data >> 9) & 0b111 == 0b100;

        let data = if is_bitop_reg {
            let reg = DataReg::new((data >> 9) as u8 & 0b111);
            let op_type = BitOpType::from_u16((data >> 6) & 3).unwrap();
            Either::Right((Some(reg), op_type))
        } else if is_bitop_imm {
            let op_type = BitOpType::from_u16((data >> 6) & 3).unwrap();
            Either::Right((None, op_type))
        } else {
            let size = Size::normal((data >> 6) as u8 & 3);
            let op = (data >> 9) as u8 & 0b111;
            Either::Left((size, SimpleOp::from_u8(op).unwrap()))
        };

        Ok(ImmediatesBits {
            mode,
            data,
        })
    }
}

impl ImmediatesBits {
    fn use_ccr(&self) -> bool {
        if self.mode == AddrMode::Immediate {
            if let Either::Left((size, op)) = &self.data {
                if size == &Size::Byte {
                    if let SimpleOp::And | SimpleOp::Or | SimpleOp::Eor = op {
                        return true;
                    }
                }
            }
        }

        false
    }

    fn use_sr(&self) -> bool {
        if self.mode == AddrMode::Immediate {
            if let Either::Left((size, op)) = &self.data {
                if size == &Size::Word {
                    if let SimpleOp::And | SimpleOp::Or | SimpleOp::Eor = op {
                        return true;
                    }
                }
            }
        }

        false
    }


}

#[derive(Debug)]
pub enum Miscellaneous {
    Illegal,
    // If size is Some, then it's a TST instruction,
    // otherwise it's a TAS instruction
    Test(Option<Size>, AddrMode),
    Lea(AddrReg, AddrMode),
    // The bool is the direction of the move
    MoveM(bool, Size, AddrMode),
    // The bool is the direction of the move (OPPOSITE of the one for MoveM!)
    MoveUsp(bool, AddrReg),
    MoveToSr(AddrMode),
    Clear(Size, AddrMode),
    Return,
    Nop,
}

impl Instr for Miscellaneous {
    fn size(&self) -> u32 {
        match self {
            Miscellaneous::Illegal => 2,
            Miscellaneous::Test(size, mode) => {
                // The size only matters for an immediate instruction, and
                // an immediate addressing mode is not legal for the TAS
                // instruction, so using *any* default is okay
                let addr_size = mode.arg_length(size.as_ref().unwrap_or(&Size::Byte));

                2 + addr_size
            }
            Miscellaneous::Lea(_, mode) => {
                let addr_size = mode.arg_length(&Size::Long);

                2 + addr_size
            }
            Miscellaneous::MoveM(_, _, _) => {
                4
            }
            Miscellaneous::MoveUsp(_, _) => {
                2
            }
            Miscellaneous::MoveToSr(mode) => {
                2 + mode.arg_length(&Size::Word)
            }
            Miscellaneous::Clear(size, mode) => {
                2 + mode.arg_length(size)
            }
            Miscellaneous::Return => {
                2
            }
            Miscellaneous::Nop => {
                2
            }
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match self {
            Miscellaneous::Illegal => panic!("Illegal instruction"),
            Miscellaneous::Test(size, mode) => {
                if let Some(size) = size {
                    let value = mode.read(false, size, cpu);
                    cpu.core.ccr.set_zero(value == 0);
                    cpu.core.ccr.set_overflow(false);
                    cpu.core.ccr.set_carry(false);
                    cpu.core.ccr.set_negative(
                        match size {
                            Size::Byte => (value as i8) < 0,
                            Size::Word => (value as i16) < 0,
                            Size::Long => (value as i32) < 0,
                        }
                    )
                } else {
                    unimplemented!()
                }
            }
            Miscellaneous::Lea(reg, mode) => {
                // TODO: Make sure this is actually what this instruction does
                if let Address::Address(address) = mode.address(false, &Size::Long, cpu) {
                    cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize] = address;
                } else {
                    panic!()
                }
            }
            Miscellaneous::Clear(size, addr_mode) => {
                addr_mode.write(0, false, size, cpu);
                cpu.core.ccr.set_negative(false);
                cpu.core.ccr.set_overflow(false);
                cpu.core.ccr.set_carry(false);
                cpu.core.ccr.set_zero(true);
            }
            Miscellaneous::Nop => {},
            Miscellaneous::Return => {
                let new = cpu.read(cpu.core.addr[7], &Size::Long);
                cpu.core.addr[7] = cpu.core.addr[7].wrapping_add(4);

                cpu.core.pc = new;

                println!("Returning, PC will be {:#X}", cpu.core.pc + 2);
            }
            Miscellaneous::MoveM(direction, size, mode) => {
                match mode {
                    AddrMode::AddrPostIncr(reg) => {
                        // Only memory-to-register transfers are allowed in this mode
                        assert_eq!(*direction, true);

                        // TODO: Is flipping it correct?
                        let mask = cpu.read(cpu.core.pc + 2, &Size::Word) as u16;

                        let reg = <_ as Into<u8>>::into(*reg) as usize;

                        for i in 0..16 {
                            if (mask >> i) & 1 != 0 {
                                let value = cpu.read(cpu.core.addr[reg], size);

                                if i >= 8 {
                                    cpu.core.addr[i - 8] = value;
                                } else {
                                    cpu.core.data[i - 0] = value;
                                }

                                cpu.core.addr[reg] += size.len() as u32;
                            }
                        }
                    }
                    AddrMode::Addr(reg) => {
                        let mask = (cpu.read(cpu.core.pc + 2, &Size::Word) as u16).swap_bytes();

                        // false -> register to memory
                        // true -> memory to register
                        
                        let mut addr = cpu.core.addr[<_ as Into<u8>>::into(*reg) as usize];
                        for i in 0..16 {
                            if (mask >> i) & 1 != 0 {
                                if *direction {
                                    let value = cpu.read(addr, size);
                                    if i >= 8 {
                                        cpu.core.addr[i - 8] = value;
                                    } else {
                                        cpu.core.data[i - 0] = value;
                                    }
                                } else {
                                    if i >= 8 {
                                        cpu.write(addr, cpu.core.addr[i - 8], size);
                                    } else {
                                        cpu.write(addr, cpu.core.data[i - 0], size);
                                    }
                                }
 
                                addr = addr.wrapping_add(size.len() as u32);
                            }
                        }
                    }
                    _ => unimplemented!("Mode {:?}", mode)
                }
            }
            Miscellaneous::MoveUsp(dir, reg) => {
                // TODO: Check if these are swapped
                let reg = <_ as Into<u8>>::into(*reg) as usize;
                if *dir {
                    cpu.core.addr[reg] = cpu.core.usp;
                } else {
                    cpu.core.usp = cpu.core.addr[reg];
                }
            }
            Miscellaneous::MoveToSr(mode) => {
                let data = mode.read(false, &Size::Word, cpu);
                cpu.core.sr = (data >> 8) as u8;
                cpu.core.ccr.0 = data as u8;
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

        let next_nybble = (data >> 8) as u8 & 0b1111;
        let low_byte = data as u8;

        if next_nybble == 0b1010 {
            if low_byte == 0b1111_1100 {
                Ok(Miscellaneous::Illegal)
            } else {
                let mode = AddrMode::new(low_byte & 0x3F);
                let size = low_byte >> 6;
                let size = if size == 0b11 {
                    None
                } else {
                    Some(Size::normal(size))
                };

                Ok(Miscellaneous::Test(size, mode))
            }
        } else if (data >> 6) & 0b1111_11 == 0b0110_11 {
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::MoveToSr(mode))
        } else if (data >> 6) & 0b111 == 0b111 {
            let reg = AddrReg::new((data >> 9) as u8 & 0b111);
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::Lea(reg, mode))
        } else if (data >> 7) & 0b111 == 0b001 {
            let direction = (data >> 10) & 1 != 0;
            let size = Size::single_bit((data >> 6) & 1 != 0);
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::MoveM(direction, size, mode))
        } else if (data >> 4) & 0xFF == 0b1110_0110 {
            let direction = (data >> 3) & 1 != 0;
            let reg = AddrReg::new(data as u8 & 0x7);
            Ok(Miscellaneous::MoveUsp(direction, reg))
        } else if data >> 6 == 0b0100_0010_10 {
            let size = Size::normal(data as u8 >> 6);
            let mode = AddrMode::new(low_byte & 0x3F);
            Ok(Miscellaneous::Clear(size, mode))
        } else if data == 0b0100_1110_0111_0101 {
            Ok(Miscellaneous::Return)
        } else if data == 0b0100_1110_0111_0001 {
            Ok(Miscellaneous::Nop)
        } else {
            unimplemented!("Word: {:#b}", data);
        }
    }
}

#[derive(Debug)]
pub struct Branch {
    pub condition: Condition,
    pub displacement: i32,
    pub disp_length: usize,
}

impl TryFrom<u16> for Branch {
    type Error = String;
    fn try_from(data: u16) -> Result<Branch, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b0110 {
            return Err(format!("Invalid top nybble {:#b}", top_nybble));
        }

        let condition = Condition::from_u16((data >> 8) & 0xF).unwrap();

        // TODO: How do I handle long offsets?
        let (displacement, disp_length) = if data as i8 != 0 {
            ((data as i8) as i32, 0)
        } else {
            (0, 2)
        };

        Ok(Branch {
            condition,
            displacement,
            disp_length,
        })
    }
}

impl Instr for Branch {
    fn size(&self) -> u32 {
        if self.disp_length != 0 {
            2 + self.disp_length as u32
        } else {
            2
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        let displacement = if self.disp_length != 0 {
            cpu.read(cpu.core.pc + 2, &Size::Word) as i32 - self.disp_length as i32
        } else {
            self.displacement
        };

        if self.condition == Condition::False {
            // BSR
            let stored = cpu.core.pc + 2;
            cpu.core.addr[7] = cpu.core.addr[7].wrapping_sub(4);
            cpu.write(cpu.core.addr[7], stored, &Size::Long);

            cpu.core.pc = cpu.core.pc.wrapping_add(displacement as u32);

            println!("JSR, PC will be {:#X}", cpu.core.pc + 2);
        }
        else if self.condition.check(&cpu.core.ccr) {
            cpu.core.pc = cpu.core.pc.wrapping_add(displacement as u32);
            println!("Branch taken, PC will be {:#X}", cpu.core.pc + 2);
        }
    }
}

#[derive(Debug)]
pub struct Move {
    size: Size,
    to_mode: AddrMode,
    from_mode: AddrMode,
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
        2 + self.to_mode.arg_length(&self.size) + self.from_mode.arg_length(&self.size)
    }

    fn execute(&self, cpu: &mut Cpu) {
        let value = self.from_mode.read(false, &self.size, cpu);

        cpu.core.ccr.set_overflow(false);
        cpu.core.ccr.set_carry(false);
        cpu.core.ccr.set_zero(value == 0);
        cpu.core.ccr.set_negative(
            match self.size {
                Size::Byte => (value as i8) < 0,
                Size::Word => (value as i16) < 0,
                Size::Long => (value as i32) < 0,
            }
        );

        let has_immediate = if let AddrMode::Immediate = self.from_mode { true } else { false };
        self.to_mode.write(value, has_immediate, &self.size, cpu);
    }
}

#[derive(Debug)]
pub struct MoveQ {
    pub reg: DataReg,
    pub value: i8,
}

impl TryFrom<u16> for MoveQ {
    type Error = String;

    fn try_from(data: u16) -> Result<MoveQ, Self::Error> {
        let top_nybble = data >> 12;
        if top_nybble != 0b0111 {
            return Err(format!("Invalid top nybble: {:#b}", top_nybble));
        }

        if (data >> 8) & 1 != 0 {
            return Err(format!("Invalid bit for MOVEQ"));
        }

        let reg = DataReg::new((data >> 9) as u8 & 0x7);
        let value = data as i8;

        Ok(MoveQ {
            reg,
            value
        })
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

        let new_value = if self.value < 0 {
            let data = (-self.value) as u32;
            0xFF_FF_FF_FF - data + 1
        } else {
            self.value as u32
        };

        cpu.core.ccr.set_negative((self.value as i32) < 0);

        cpu.core.data[<_ as Into<u8>>::into(self.reg) as usize] = new_value;
    }
}

#[derive(Debug)]
pub struct LinearOp {
    pub is_add: bool,
    pub size: Size,
    pub op_type: LinearOpType,
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

fn compare(lhs: u32, rhs: u32, size: &Size) -> Ccr {
    let max_val = match size {
        Size::Byte => 0xFF,
        Size::Word => 0xFF_FF,
        Size::Long => 0xFF_FF_FF_FF,
    };

    let lhs = lhs as u64;
    let rhs = rhs as u64;

    let result = (lhs & max_val).wrapping_sub(rhs & max_val);

    let carry = result & !max_val != 0;

    // TODO: This may be wrong, and definitely isn't pretty
    let bits = max_val.count_ones();
    let left_sign = ((lhs & max_val) >> (bits - 1)) & 1 != 0;
    let right_sign = ((rhs & max_val) >> (bits - 1)) & 1 != 0;
    let result_sign = ((result & max_val) >> (bits - 1)) & 1 != 0;

    let overflow = left_sign == right_sign && left_sign != result_sign;

    let mut ccr = Ccr(0);
    ccr.set_extend(carry);
    ccr.set_carry(carry);
    ccr.set_zero((result & max_val) == 0);
    ccr.set_negative(result_sign);
    ccr.set_overflow(overflow);

    ccr
}

impl LinearOp {
    pub fn do_op(is_add: bool, lhs: u32, rhs: u32, size: &Size) -> (u32, Ccr) {
        let max_val = match size {
            Size::Byte => 0xFF,
            Size::Word => 0xFF_FF,
            Size::Long => 0xFF_FF_FF_FF,
        };

        let result = if is_add {
            (lhs & max_val).wrapping_add(rhs & max_val)
        } else {
            (lhs & max_val).wrapping_sub(rhs & max_val)
        };

        ((lhs & !max_val) | (result & max_val), compare(lhs, rhs, size))
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
            LinearOpType::Normal(_, _, mode) => mode.arg_length(&self.size),
            LinearOpType::Address(_, mode) => mode.arg_length(&self.size),
            LinearOpType::WithExtend(_) => 0,
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match &self.op_type {
            LinearOpType::Normal(dir, reg, mode) => {
                let (lhs, rhs) = if *dir {
                    (mode.read(false, &self.size, cpu), reg.read(cpu))
                } else {
                    (reg.read(cpu), mode.read(false, &self.size, cpu))
                };

                let (result, ccr) = LinearOp::do_op(self.is_add, lhs, rhs, &self.size);
                cpu.core.ccr = ccr;

                if *dir {
                    mode.write(result, false, &self.size, cpu)
                } else {
                    reg.write(result, cpu)
                }
            }
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
            ConditionsQuicks::AddSubQ(_, _, size, mode) => mode.arg_length(size),
            ConditionsQuicks::Set(_, mode) => mode.arg_length(&Size::Byte),
            ConditionsQuicks::DecBranch(_, _) => 2,
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match self {
            ConditionsQuicks::AddSubQ(is_sub, data, size, mode) => {
                let lhs = mode.read(false, size, cpu);
                let (result, ccr) = LinearOp::do_op(!is_sub, lhs, *data as u32, size);
                mode.write(result, false, size, cpu);
                cpu.core.ccr = ccr;
            }
            ConditionsQuicks::DecBranch(condition, reg) => {
                if condition.check(&cpu.core.ccr) {
                    // Continue
                } else {
                    let mut low_word = reg.read(cpu) as i16;
                    low_word = low_word.wrapping_sub(1);
                    reg.write(low_word as u32, cpu);

                    if low_word != -1 {
                        let disp = cpu.read(cpu.core.pc + 2, &Size::Word) as i16;
                        let new_pc = cpu.core.pc as i16 + disp;
                        cpu.core.pc &= !0xFF_FF;
                        cpu.core.pc |= new_pc as u32;
                        // To adjust for the increment after the instruction
                        cpu.core.pc -= 2;
                    }
                }
            }
            _ => unimplemented!("CQ: {:?}", self)
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
            let data = (data >> 9) as u8 & 7;
            let mode = AddrMode::new(data & 0x3F);
            Ok(ConditionsQuicks::AddSubQ(is_sub, data, size, mode))
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
    pub data: Either<CompareMode, (DataReg, AddrMode)>
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
            Ok(CompareEor {
                size,
                data
            })
        } else if (data >> 6) & 0b11 == 0b11 {
            let dest_reg = AddrReg::new((data >> 9) as u8 & 0x7);
            let source_mode = AddrMode::new(data as u8 & 0x3F);
            let size = Size::single_bit((data >> 8) & 1 != 0);
            let mode = CompareMode::Address(dest_reg, source_mode);
            let data = Either::Left(mode);
            Ok(CompareEor {
                size,
                data,
            })
        } else if (data >> 8) & 1 == 0 {
            let dest_reg = DataReg::new((data >> 9) as u8 & 0x7);
            let source_mode = AddrMode::new(data as u8 & 0x3F);
            let size = Size::normal((data >> 6) as u8 & 3);
            let mode = CompareMode::Normal(dest_reg, source_mode);
            let data = Either::Left(mode);
            Ok(CompareEor {
                size,
                data,
            })
        } else {
            let size = Size::normal((data >> 6) as u8 & 3);
            let source_mode = AddrMode::new(data as u8 & 0x3F);
            let dest_reg = DataReg::new((data >> 9) as u8 & 0x7);
            let data = Either::Right((dest_reg, source_mode));
            Ok(CompareEor {
                size,
                data,
            })
        }
    }
}

impl Instr for CompareEor {
    fn size(&self) -> u32 {
        match &self.data {
            Either::Left(mode) => {
                match mode {
                    CompareMode::Address(_, source_mode) |
                    CompareMode::Normal(_, source_mode) => 2 + source_mode.arg_length(&self.size),
                    CompareMode::Memory(_, _) => 2,
                }
            }
            Either::Right(_) => {
                unimplemented!()
            }
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match &self.data {
            Either::Left(mode) => {
                match mode {
                    CompareMode::Normal(dest_reg, source_mode) => {
                        let dest = dest_reg.read(cpu);
                        let source = source_mode.read(false, &self.size, cpu);
                        let new_ccr = compare(dest, source, &self.size);
                        cpu.core.ccr = new_ccr;
                    }
                    _ => unimplemented!()
                }
            }
            Either::Right(_) => unimplemented!()
        }
    }
}

/// Instructions on the 68k can be roughly divided into 'pages'
/// based on the high nybble of their encoding. 
#[derive(Debug)]
pub enum Pages {
    ImmediatesBits(ImmediatesBits), // 0b0000
    Move(Move), // 0b0001, 0b0010, 0b0011
    Miscellaneous(Miscellaneous), // 0b0100
    ConditionsQuicks(ConditionsQuicks), // 0b0101
    Branch(Branch), // 0b0110
    MoveQ(MoveQ), // 0b0111
    CompareEor(CompareEor), // 0b1011
    // 0b1001 means it's a subtraction,
    // 0b1101 means it's an addition
    LinearOp(LinearOp),
}

impl TryFrom<u16> for Pages {
    type Error = String;

    fn try_from(word: u16) -> Result<Pages, Self::Error> {
        let page = word >> 12;
        match page {
            0b0000 => Ok(Pages::ImmediatesBits(ImmediatesBits::try_from(word).unwrap())),
            0b0001 |
            0b0010 |
            0b0011 => Ok(Pages::Move(Move::try_from(word).unwrap())),
            0b0100 => Ok(Pages::Miscellaneous(Miscellaneous::try_from(word).unwrap())),
            0b0101 => Ok(Pages::ConditionsQuicks(ConditionsQuicks::try_from(word).unwrap())),
            0b0110 => Ok(Pages::Branch(Branch::try_from(word).unwrap())),
            0b0111 => Ok(Pages::MoveQ(MoveQ::try_from(word).unwrap())),
            0b1011 => Ok(Pages::CompareEor(CompareEor::try_from(word).unwrap())),
            0b1001 |
            0b1101 => Ok(Pages::LinearOp(LinearOp::try_from(word).unwrap())),
            0b10000..=std::u16::MAX => unreachable!(),
            _ => unimplemented!("Top nybble: {:#b}, Word: {:#06X}", page, word),
        }
    }
}

// TODO: Use actual deferring
impl Instr for Pages {
    fn size(&self) -> u32 {
        match self {
            Pages::ImmediatesBits(a) => a.size(),
            Pages::Miscellaneous(a) => a.size(),
            Pages::Branch(a) => a.size(),
            Pages::Move(a) => a.size(),
            Pages::MoveQ(a) => a.size(),
            Pages::LinearOp(a) => a.size(),
            Pages::ConditionsQuicks(a) => a.size(),
            Pages::CompareEor(a) => a.size(),
        }
    }

    fn execute(&self, cpu: &mut Cpu) {
        match self {
            Pages::ImmediatesBits(a) => a.execute(cpu),
            Pages::Miscellaneous(a) => a.execute(cpu),
            Pages::Branch(a) => a.execute(cpu),
            Pages::Move(a) => a.execute(cpu),
            Pages::MoveQ(a) => a.execute(cpu),
            Pages::LinearOp(a) => a.execute(cpu),
            Pages::ConditionsQuicks(a) => a.execute(cpu),
            Pages::CompareEor(a) => a.execute(cpu),
        }
    }
}

#[derive(Debug)]
pub struct Instruction {
    opcode: Pages
}

impl Instruction {
    pub fn new(word: u16) -> Instruction {
        Instruction {
            opcode: Pages::try_from(word).unwrap()
        }
    }

    pub fn execute(&self, cpu: &mut Cpu) {
        self.opcode.execute(cpu);

        cpu.core.pc += self.opcode.size();
    }
}
