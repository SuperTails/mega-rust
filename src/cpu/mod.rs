pub mod instruction;

use crate::get_four_bytes;
use crate::get_two_bytes;
use instruction::Instruction;

bitfield! {
    pub struct Ccr(u8);
    impl Debug;

    extend, set_extend: 0;
    negative, set_negative: 1;
    zero, set_zero: 2;
    overflow, set_overflow: 3;
    carry, set_carry: 4;
}

pub enum State {
    Run,
    Reset
}

/// Represents the current state of the 68k
/// processor, indepedent of memory state
pub struct Cpu {
    data: [u32; 8],
    addr: [u32; 8],
    usp: u32,
    pc: u32,
    ccr: Ccr,

    cycle: usize,
    state: State,
}

impl Cpu {
    pub fn new() -> Cpu {
        Cpu {
            data: [0; 8],
            addr: [0; 8],
            usp: 0,
            pc: 0x0004,
            ccr: Ccr(0),
            cycle: 0,
            state: State::Reset,
        }
    }

    // TODO: Access Z80, VDP, expansion ports, and IO registers
    pub fn do_cycle(&mut self, rom: &[u8]) {
        match self.state {
            State::Reset => {
                assert_eq!(self.pc % 4, 0);
                assert!(self.pc < 0x100);

                let offset = self.pc as usize;
                let vector = u32::from_be_bytes(get_four_bytes(&rom[offset..offset+4]));

                println!("Using vector at {:#08X}, PC will be {:#08X}", offset, vector);

                // TODO: Determine actual cycle count and behavior
                self.pc = vector;
                self.cycle += 1;
                self.state = State::Run;
            }
            State::Run => {
                if self.pc >= 0x3FFFFF {
                    unimplemented!()
                } else {
                    assert_eq!(self.pc % 2, 0);
                    assert!(self.pc <= 0x3FFFFF);

                    let offset = self.pc as usize % rom.len();

                    let instr = Instruction::new(
                        u16::from_be_bytes(
                            get_two_bytes(&rom[offset..offset+2])
                        )
                    );

                    let last_pc = self.pc;

                    println!("PC:{:08X} Instr: {:?}", self.pc, instr);

                    // TODO: Cycle counts
                    instr.execute(self);
                }
            }
        }
    }
}
