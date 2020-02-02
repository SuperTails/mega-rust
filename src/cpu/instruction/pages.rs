use super::*;

pub trait Instr {
    fn size(&self) -> u32;

    fn execute(&self, cpu: &mut Cpu);
}

macro_rules! make_dispatcher {
    (@enum $name:ident $($varname:ident)*) => {
        #[derive(Debug)]
        pub enum $name {
            $($varname($varname),)*
        }
    };
    (@impls $name:ident $($varname:ident)*) => {
        impl Instr for $name {
            fn size(&self) -> u32 {
                match self {
                    $(Self::$varname(x) => {
                        x.size()
                    })*
                }
            }

            fn execute(&self, cpu: &mut Cpu) {
                match self {
                    $(Self::$varname(x) => {
                        x.execute(cpu)
                    })*
                }
            }
        }
    };
    ($name:ident : $($varname:ident,)*) => {
        make_dispatcher!(@enum $name $($varname)*);
        make_dispatcher!(@impls $name $($varname)*);
    };
}

make_dispatcher! { Pages :
    Immediates, // First half of 0b0000
    BitOperation, // Other half of 0b0000

    Move, //0b0001, 0b0010, 0b0011

    Illegal,         // 0b0100_1010_1111_1100
    Reset,           // 0b0100_1110_0111_0000
    Nop,             // 0b0100_1110_0111_0001
    Stop,            // 0b0100_1110_0111_0010
    ExceptionReturn, // 0b0100_1110_0111_0011
    Return,          // 0b0100_1110_0111_0101
    TrapOverflow,    // 0b0100_1110_0111_0110
    RestoreReturn,   // 0b0100_1110_0111_0111

    Miscellaneous, // 0b0100
    ConditionsQuicks, // 0b0101
    Branch, // 0b0110
    MoveQ, // 0b0111
    DivSubdOr, // 0b1000
    MulAddExgAnd, // 0b1100
    CompareEor, // 0b1011
    LinearOp, // 0b1001 for sub, 0b1101 for add
    Shifts, // 0b1110
}

impl TryFrom<u16> for Pages {
    type Error = String;

    // This is intentional I swear
    #[allow(clippy::inconsistent_digit_grouping)]
    fn try_from(word: u16) -> Result<Pages, Self::Error> {
        if word == 0b0100_1010_1111_1100 {
            return Ok(Pages::Illegal(Illegal {}));
        }

        if word >> 3 == 0b0100_1110_0111_0 {
            match word & 0x7 {
                0b0000 => return Ok(Pages::Reset(Reset {})),
                0b0001 => return Ok(Pages::Nop(Nop {})),
                0b0010 => return Ok(Pages::Stop(Stop {})),
                0b0011 => return Ok(Pages::ExceptionReturn(ExceptionReturn {})),
                0b0101 => return Ok(Pages::Return(Return {})),
                0b0110 => return Ok(Pages::TrapOverflow(TrapOverflow {})),
                0b0111 => return Ok(Pages::RestoreReturn(RestoreReturn {})),
                0b0100 => {}
                _ => unreachable!(),
            }
        }

        let page = word >> 12;
        match page {
            0b0000 => {
                if bitpat!(1 0 0 _)(word >> 8) || bitpat!(_ _ _ 1)(word >> 8) {
                    Ok(Pages::BitOperation(BitOperation::try_from(word).unwrap()))
                } else {
                    Ok(Pages::Immediates(Immediates::try_from(word).unwrap()))
                }
            }
            0b0001 | 0b0010 | 0b0011 => Ok(Pages::Move(Move::try_from(word).unwrap())),
            0b0100 => Ok(Pages::Miscellaneous(Miscellaneous::try_from(word).unwrap())),
            0b0101 => Ok(Pages::ConditionsQuicks(
                ConditionsQuicks::try_from(word).unwrap(),
            )),
            0b0110 => Ok(Pages::Branch(Branch::try_from(word).unwrap())),
            0b0111 => Ok(Pages::MoveQ(MoveQ::try_from(word).unwrap())),
            0b1000 => Ok(Pages::DivSubdOr(DivSubdOr::try_from(word).unwrap())),
            0b1011 => Ok(Pages::CompareEor(CompareEor::try_from(word).unwrap())),
            0b1100 => Ok(Pages::MulAddExgAnd(MulAddExgAnd::try_from(word).unwrap())),
            0b1001 | 0b1101 => Ok(Pages::LinearOp(LinearOp::try_from(word).unwrap())),
            0b1110 => Ok(Pages::Shifts(Shifts::try_from(word).unwrap())),
            0b10000..=std::u16::MAX => unreachable!(),
            _ => unimplemented!("Top nybble: {:#b}, Word: {:#06X}", page, word),
        }
    }
}
