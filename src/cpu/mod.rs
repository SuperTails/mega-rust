pub mod instruction;

use std::rc::Weak;
use std::cell::RefCell;
use crate::get_four_bytes;
use crate::get_two_bytes;
use crate::vdp::Vdp;
use instruction::{Instruction, Size};

const LOG_INSTR: bool = false;

bitfield! {
    #[derive(Clone, Copy)]
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
pub struct CpuCore {
    data: [u32; 8],
    addr: [u32; 8],
    usp: u32,
    pc: u32,
    ccr: Ccr,
    sr: u8,

    cycle: usize,
    state: State,
}

pub struct Cpu {
    pub core: CpuCore,
    pub rom: Box<[u8]>,
    pub cart_ram: Box<[u8]>,
    pub ram: [u8; 0x10000],
    vdp: Weak<RefCell<Vdp>>,
}

impl Cpu {
    pub fn new(rom: &[u8], vdp: Weak<RefCell<Vdp>>) -> Cpu {
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
            ram: [0; 0x10000],
            vdp,
        }
    }

    pub fn read(&self, addr: u32, size: &Size) -> u32 {
        let length = size.len();
        let align = size.alignment();

        let addr = (addr & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0);

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            let vdp = self.vdp.upgrade().unwrap();
            return vdp.borrow_mut().read(addr as u32) as u32;
        }

        let mut result = 0;

        for offset in 0..length {
            result <<= 8;
            let byte_addr = addr + offset;
            let byte = if byte_addr < self.rom.len() {
                self.rom[byte_addr]
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()]
            } else if (0xFF0000..=0xFFFFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF0000]
            } else {
                println!("Unimplemented address {:#X}, reading zero", addr);
                0
            };

            result |= byte as u32;
        }

        result
    }

    pub fn write(&mut self, addr: u32, value: u32, size: &Size) {
        let length = size.len();
        let align = size.alignment();

        let addr = (addr & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Addr: {:#06X}", addr);

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            let vdp = self.vdp.upgrade().unwrap();
            vdp.borrow_mut().write(addr as u32, value);
            return;
        }

        let value = value.to_be_bytes();

        for offset in 0..length {
            let byte_addr = addr + offset;
            let byte = value[offset];
            if byte_addr < self.rom.len() {
                //self.rom[byte_addr] = byte;
                println!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()] = byte;
            } else if (0xFF0000..=0xFFFFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF0000] = byte;
            } else {
                println!("Ignoring write to {:#010X}", byte_addr);
            };
        }
    }

    // TODO: Access Z80, VDP, expansion ports, and IO registers
    pub fn do_cycle(&mut self) {
        match self.core.state {
            State::Reset => {
                assert_eq!(self.core.pc % 4, 0);
                assert!(self.core.pc < 0x100);

                let offset = self.core.pc as usize;
                let vector = u32::from_be_bytes(get_four_bytes(&self.rom[offset..offset+4]));

                println!("Using vector at {:#08X}, PC will be {:#08X}", offset, vector);

                // TODO: Determine actual cycle count and behavior
                self.core.pc = vector;
                self.core.cycle += 1;
                self.core.state = State::Run;
            }
            State::Run => {
                if self.core.pc >= 0x3FFFFF {
                    unimplemented!()
                } else {
                    assert_eq!(self.core.pc % 2, 0);
                    assert!(self.core.pc <= 0x3FFFFF);

                    let offset = self.core.pc as usize % self.rom.len();

                    if LOG_INSTR { print!("PC:{:08X}, A7: {:08X} ", self.core.pc, self.core.addr[7]); }

                    let instr = Instruction::new(
                        u16::from_be_bytes(
                            get_two_bytes(&self.rom[offset..offset+2])
                        )
                    );

                    if LOG_INSTR { println!("Instr: {:?}", instr); }

                    // TODO: Cycle counts
                    instr.execute(self);
                }
            }
        }
    }

}

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
}
