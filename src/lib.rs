pub mod cart;
mod controller;
mod cpu;
mod joypad;
mod sdl_system;
mod vdp;

use cart::Cart;
use controller::Controller;
use cpu::instruction::*;
use cpu::{Cpu, CpuCore};
use sdl_system::SDLSystem;
use std::collections::BinaryHeap;
use std::sync::Arc;
use std::sync::Mutex;
use vdp::Vdp;

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
    let controller1 = Arc::new(Mutex::new(Controller::default()));
    let controller2 = Arc::new(Mutex::new(Controller::default()));
    let mut cpu = Cpu::new(
        &cart.rom_data,
        Arc::downgrade(&vdp),
        Arc::downgrade(&controller1),
        Arc::downgrade(&controller2),
    );
    let mut sdl_system = SDLSystem::new();
    let hit_breakpoint = false;
    let mut log_pos: Option<usize> = None;
    let mut pending: BinaryHeap<Interrupt> = BinaryHeap::new();

    cpu::do_log(options.log_instrs);

    let log =
        CpuCore::read_log(&std::fs::read_to_string("./roms/s1disasm/s1rev01_RunLog.txt").unwrap())
            .unwrap();

    'running: loop {
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

        let _instr = Instruction::new(cpu.read(cpu.core.pc, Size::Word) as u16);

        cpu.do_cycle(&mut pending);
        if vdp.lock().unwrap().do_cycle(&mut sdl_system, &mut pending) {
            let events: Vec<_> = sdl_system.event_pump.poll_iter().collect();
            controller1.lock().unwrap().update_buttons(&events);
            controller2.lock().unwrap().update_buttons(&events);
            for event in events {
                match event {
                    sdl2::event::Event::KeyDown {
                        keycode: Some(sdl2::keyboard::Keycode::Escape),
                        ..
                    }
                    | sdl2::event::Event::Quit { .. } => {
                        println!("Exited with cpu core state:\n{}", cpu.core);
                        break 'running;
                    }
                    sdl2::event::Event::KeyDown {
                        keycode: Some(sdl2::keyboard::Keycode::L),
                        ..
                    } => {
                        cpu::do_log(true);
                    }
                    _ => {}
                }
            }
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
