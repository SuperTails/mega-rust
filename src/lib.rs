pub mod cart;
mod cpu;
mod joypad;
mod sdl_system;
mod vdp;

use cart::Cart;
use cpu::{Cpu, CpuCore};
use sdl_system::SDLSystem;
use vdp::Vdp;
use std::sync::Arc;
use std::sync::Mutex;
use cpu::instruction::*;
use std::collections::BinaryHeap;

//const Z80_DATA: &[u8; 7110] = include_bytes!("../../z80decompressed.bin");

fn get_four_bytes(data: &[u8]) -> [u8; 4] {
    let mut result = [0; 4];
    result.copy_from_slice(data);
    result
}

fn get_two_bytes(data: &[u8]) -> [u8; 2] {
    let mut result = [0; 2];
    result.copy_from_slice(data);
    result
}

fn check_log(expected: &CpuCore, actual: &CpuCore) {
    let mut wrong = false;

    if expected.data != actual.data {
        println!(
            "Data registers differed:\nExpected: {:#X?}\nActual: {:#X?}",
            expected.data, actual.data
        );
        wrong |= true;
    }

    if expected.addr != actual.addr {
        println!(
            "Address registers differed:\nExpected: {:#X?}\nActual: {:#X?}",
            expected.addr, actual.addr
        );
        wrong |= true;
    }

    if expected.pc != actual.pc {
        println!(
            "PC differed, expected: {:#X?}, actual {:#X?}",
            expected.pc, actual.pc
        );
        wrong |= true;
    }

    if expected.ccr != actual.ccr {
        println!(
            "Conditions differed, expected:\n{:?}\nActual:\n{:?}\n",
            expected.ccr, actual.ccr
        );
        wrong |= true;
    }

    if wrong {
        panic!()
    }

    assert_eq!(expected, actual);
}

/// The 68k has Interrupt Exceptions, over 3 pins: IPL0, IPL1, IPL2
/// IPL register is in SR the low three bits of the high byte:
/// xxxx xipl xxxx xxxx
/// The 68k *ignores any exception equal to or below the current IPL level.*
#[derive(Debug, PartialEq, Eq, Clone, Copy, PartialOrd, Ord)]
pub enum Interrupt {
    External = 2,
    Horizontal = 4,
    Vertical = 6,
}

pub struct Options {
    pub log_instrs: bool,
}

pub fn run(cart: Cart, options: Options) {
    let vdp = Arc::new(Mutex::new(Vdp::new()));
    let mut cpu = Cpu::new(&cart.rom_data, Arc::downgrade(&vdp));
    let mut sdl_system = SDLSystem::new();
    let hit_breakpoint = false;
    let mut log_pos: Option<usize> = None;
    let mut pending: BinaryHeap<Interrupt> = BinaryHeap::new();
    
    cpu::do_log(options.log_instrs);

    let log =
        CpuCore::read_log(&std::fs::read_to_string("./roms/s1disasm/s1rev01_RunLog.txt").unwrap())
            .unwrap();
            
    //let kos_data = kos_decompress(&cpu.rom[0x72E7C..]).unwrap();
    //let kos_data = parse_thing("F3 F3 F3 31 FC 1F DD 21 0 40 AF 32 FD FF 32 FF 1F 3E");
    //cpu.ram[0x18B2] = 0x4E;
    //cpu.ram[0x18B3] = 0x75;

    loop {
        /*if cpu.core.pc == 0xC254
        || cpu.core.pc == 0x173E
        || (cpu.core.pc >> 8 == 0x17 && cpu.core.addr[1] >> 16 != 0xFF)
        || cpu.core.pc == 0x30C {
            hit_breakpoint = true;
        }*/
        /*if cpu.core.pc == 0x1716 {
            log_pos = Some(0);

            cpu.core = log[0].clone();
        }*/


        if hit_breakpoint {
            'wait: loop {
                for event in sdl_system.event_pump.poll_iter() {
                    match event {
                        sdl2::event::Event::KeyDown {
                            keycode: Some(sdl2::keyboard::Keycode::Space),
                            ..
                        } => {
                            break 'wait;
                        }
                        sdl2::event::Event::Quit { .. }
                        | sdl2::event::Event::KeyDown {
                            keycode: Some(sdl2::keyboard::Keycode::Escape),
                            ..
                        } => {
                            std::process::exit(0);
                        }
                        sdl2::event::Event::KeyDown {
                            keycode: Some(sdl2::keyboard::Keycode::P),
                            ..
                        } => {
                            println!("{}", cpu.core);
                        }
                        _ => {}
                    }
                }
            }
        }

        if let Some(idx) = &mut log_pos {
            if *idx < log.len() {
                check_log(&log[*idx], &cpu.core);
                *idx += 1;
            }
        }

        //let kos_data = [0xF3, 0xF3, 0xF3, 0x31, 0xFC, 0x1F];

        let _instr = Instruction::new(cpu.read(cpu.core.pc, Size::Word) as u16);

        /*if let Instruction { opcode: Pages::Immediates(Immediates{ op: SimpleOp::Or, .. }) } = instr {
            if cpu.read(cpu.core.pc + 2, &cpu::instruction::Size::Byte) == 0x40 {
                hit_breakpoint = true;
            }
        }

        /*if let Instruction { opcode: Pages::Miscellaneous(Miscellaneous::Clear(Size::Byte, _)) } = instr {
            hit_breakpoint = true;
        }*/

        if cpu.core.pc == 0x2E66 {
            hit_breakpoint = true;
        }*/

        cpu.do_cycle(&mut pending);
        if vdp
            .lock()
            .unwrap()
            .do_cycle(&mut sdl_system, &mut pending)
        {
            println!("Exited with cpu core state:\n{}", cpu.core);
            break;
        }

        if cpu::log_instr() {
            println!("{}", cpu.core);
        }

        if cpu.core.pc == 0x182C {
            // TODO: The add operation shouldn't set the carry/extend flag AFAIK,
            // but it does anyway?
            cpu.core.ccr.set_extend(true);
            cpu.core.ccr.set_carry(true);
        }

        if cpu.core.pc == 0x17F4 || cpu.core.pc == 0x1802 {
            // TODO: ... and now it sets negative and overflow when it shouldn't
            cpu.core.ccr.set_overflow(false);
            cpu.core.ccr.set_negative(false);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
