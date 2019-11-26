use num_traits::FromPrimitive;
use super::Cpu;

#[derive(Debug)]
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

impl AddrMode {
    pub fn new(data: u8) -> AddrMode {
        assert!(data < 0x3F);

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

    pub fn arg_length(&self, size: &Size) -> u32 {
        match self {
            AddrMode::DataReg(_) |
            AddrMode::AddrReg(_) |
            AddrMode::AddrPreDecr(_) |
            AddrMode::AddrPostIncr(_) |
            AddrMode::Addr(_) => 0,
            AddrMode::Immediate => if size == &Size::Long { 4 } else { 2 }
            AddrMode::AddrDisp(_) |
            _ => 2,
        }
    }
}

#[derive(Debug)]
pub enum BSpecialKind {
    Or,
    And,
    Eor,
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

#[derive(Debug, FromPrimitive)]
pub enum Condition {
    True = 0b0000,
    False,
    Higher,
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
}

#[derive(Debug)]
pub struct AddSub {
    is_add: bool, // Determined by bit 14
    size: Size,
    to_mode: AddrMode,
    from_mode: AddrMode,
}

#[derive(Debug)]
pub enum Opcode {
    // ORI/ANDI/EORI to CCR or SR, `true` indicates SR
    BitwiseSpecial(BSpecialKind, bool),
    Illegal,
    // Some(size) for TST, or if None then it's a TAS
    Test(Option<u8>, AddrMode),
    SimpleOp(SimpleOp, Size, AddrMode),
    Branch(Condition, u8),
    Lea(u8, AddrMode),
    MoveM(bool, Size, AddrMode),
    Move(Size, AddrMode, AddrMode),
    MoveQ(u8, u8),
    MoveUsp(bool, u8),
    AddSub(AddSub),
}

impl Opcode {
    pub fn length(&self) -> u32 {
        let additional = match self {
            Opcode::BitwiseSpecial(_, false) => 2,
            Opcode::BitwiseSpecial(_, true) => 2,
            Opcode::Illegal => 0,
            Opcode::Test(None, _) => 2,
            Opcode::Test(Some(_), _) => 4,
            Opcode::SimpleOp(_, size, _) => if size == &Size::Long { 4 } else { 2 },
            Opcode::Branch(_, offset) => match offset { 0x00 => 2, 0xFF => 4, _ => 0 },
            Opcode::Lea(_, _) => 2,
            Opcode::MoveM(_, _, _) => 2,
            Opcode::AddSub(AddSub { size, to_mode, from_mode, .. }) |
            Opcode::Move(size, to_mode, from_mode) => to_mode.arg_length(size) + from_mode.arg_length(size),
            Opcode::MoveQ(_, _) => 0,
            Opcode::MoveUsp(_, _) => 0,
        };

        2 + additional
    }
}

#[derive(Debug)]
pub struct Instruction {
    opcode: Opcode
}

impl Instruction {
    pub fn new(word: u16) -> Instruction {
        let top_nybble = word >> 12;
        let next_nybble = (word >> 8) & 0b1111;
        let low_three = word & 0xFFF;

        match top_nybble {
            0b0000 => {
                if bitpat!(1 _ _ _)(next_nybble) || bitpat!(_ _ _ 1)(next_nybble) {
                    // Single-bit operations
                    unimplemented!()
                } else if bitpat!(_ 0 _ 0 0 _ 1 1 1 1 0 0)(low_three) {
                    // Special cases for ORI, ANDI, and EORI
                    let kind = match (word >> 9) & 0b111 {
                        0b000 => BSpecialKind::Or,
                        0b001 => BSpecialKind::And,
                        0b101 => BSpecialKind::Eor,
                        _ => unreachable!(),
                    };

                    let sr = (word >> 6) & 1 == 1;

                    Instruction { opcode: Opcode::BitwiseSpecial(kind, sr) }
                } else {
                    // Normal ORI, ANDI, SUBI, ADDI, EORI, and CMPI
                    let op = SimpleOp::from_u16((word >> 9) & 0b111).unwrap();
                    let size = Size::normal((word >> 6) as u8 & 0b11);

                    Instruction { opcode: Opcode::SimpleOp(op, size, AddrMode::new(word as u8 & 0x3F)) }
                }
            }
            0b0001 |
            0b0010 |
            0b0011 => {
                let size = Size::higher(top_nybble as u8);
                let to_mode = (word >> 6) as u8 & 0x3F;
                let to_mode = (to_mode >> 3) | ((to_mode & 0x7) << 3);
                let to_mode = AddrMode::new(to_mode);

                println!("Word {:#X}", word);

                Instruction { opcode: Opcode::Move(size, to_mode, AddrMode::new(word as u8 & 0x3F)) }
            }
            0b0100 => {
                if bitpat!(1 0 1 0)(next_nybble) {
                    if (word >> 6) & 0b11 == 0b11 {
                        // TAS or illegal
                        if (word & 0x3F) == 0b111100 {
                            Instruction { opcode: Opcode::Illegal }
                        } else {
                            Instruction { opcode: Opcode::Test(None, AddrMode::new(word as u8 & 0x3F)) }
                        }
                    } else {
                        let size = (word >> 6) & 0b11;
                        Instruction { opcode: Opcode::Test(Some(size as u8), AddrMode::new(word as u8 & 0x3F)) }
                    }
                } else if (word >> 6) & 0b111 == 0b111 {
                    let reg = (word >> 9) as u8 & 0b111;

                    Instruction { opcode: Opcode::Lea(reg, AddrMode::new(word as u8 & 0x3F)) }
                } else if bitpat!(1 _ 0 0 1)((word >> 7) & 0x1F) {
                    let dir = (word >> 10) & 1 == 1;
                    let size = Size::single_bit((word >> 6) & 1 == 1);
                    Instruction { opcode: Opcode::MoveM(dir, size, AddrMode::new(word as u8 & 0x3F)) }
                } else if bitpat!(1 1 1 0 0 1 1 0)(word >> 4) {
                    let dir = (word >> 3) == 1;
                    let reg = word as u8 & 0x7;

                    Instruction { opcode: Opcode::MoveUsp(dir, reg) }
                } else {
                    unimplemented!("Next nybble: {:#b}", next_nybble);
                }
            }
            0b0110 => {
                let condition = Condition::from_u16((word >> 8) & 0xF).unwrap();
                let displacement = word as u8;
                Instruction { opcode: Opcode::Branch(condition, displacement) }
            }
            0b0111 => {
                let reg = (word >> 9) as u8 & 0x7;
                let data = word as u8;

                Instruction { opcode: Opcode::MoveQ(reg, data) }
            }
            0b1001 |
            0b1101 => {
                let is_add = top_nybble & 0b0100 != 0;
                if bitpat!(1  _ _  0 0  _  _ _ _)(low_three) {
                    
                }
            }
            _ => unimplemented!("Top four: {:#06b}", top_nybble),
        }
    }

    pub fn execute(&self, cpu: &mut Cpu) {
        cpu.pc += self.opcode.length();
    }
}
