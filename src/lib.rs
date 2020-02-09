pub mod cart;
mod controller;
mod cpu;
mod cpu_bindings;
mod joypad;
mod sdl_system;
mod vdp;

use cart::Cart;
use controller::Controller;
use cpu_bindings::{MusashiCpu, SYSTEM_STATE, VDP};
use sdl_system::SDLSystem;
use std::collections::BinaryHeap;
use std::time::Instant;
use vdp::Vdp;

//const Z80_DATA: &[u8; 7110] = include_bytes!("../../z80decompressed.bin");

fn get_four_bytes(data: &[u8]) -> [u8; 4] {
    let mut result = [0; 4];
    result.copy_from_slice(data);
    result
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

fn on_breakpoint(sdl_system: &mut SDLSystem) {
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
                    //println!("{}", cpu.core);
                }
                _ => {}
            }
        }
    }
}

pub struct Options {
    pub log_instrs: bool,
}

pub fn run(cart: Cart, options: Options) {
    let vdp = Vdp::new();
    /*let mut cpu = cpu::Cpu::new(
        &cart.rom_data,
        Arc::downgrade(&vdp),
        Arc::downgrade(&controller1),
        Arc::downgrade(&controller2),
    );*/
    unsafe {
        VDP.set(vdp).ok().unwrap();
    };
    let mut cpu2 = MusashiCpu::new(&cart.rom_data, Controller::default(), Controller::default());
    let mut sdl_system = SDLSystem::new();
    sdl_system.canvas.set_scale(2.0, 2.0).unwrap();
    let hit_breakpoint = false;
    let mut pending: BinaryHeap<Interrupt> = BinaryHeap::new();

    cpu::do_log(options.log_instrs);

    let mut frames = vec![];

    'running: loop {
        if hit_breakpoint {
            on_breakpoint(&mut sdl_system);
        }

        unsafe { SYSTEM_STATE.get_mut() }
            .unwrap()
            .controller_1
            .update();
        unsafe { SYSTEM_STATE.get_mut() }
            .unwrap()
            .controller_2
            .update();

        /*let instr = cpu.instr_at(cpu.core.pc);

        if let Pages::Stop(_) = instr.opcode {
        } else {
            cpu.do_cycle(&mut pending);
        }*/

        //cpu.do_cycle(&mut pending);
        cpu2.do_cycle(&mut pending);

        /*if !cpu_eq(&cpu, &cpu2) {
            println!("Main CPU state:\n{}\nMusashi CPU state:\n{}", cpu.core, cpu::CpuCore::from(&cpu2));
            break 'running;
        }*/

        let (scan_finish, frame_finish) = unsafe { VDP.get_mut() }
            .unwrap()
            .do_cycle(&mut sdl_system, &mut pending);

        if scan_finish {
            let events: Vec<_> = sdl_system.event_pump.poll_iter().collect();
            unsafe { SYSTEM_STATE.get_mut() }
                .unwrap()
                .controller_1
                .update_buttons(&events);
            unsafe { SYSTEM_STATE.get_mut() }
                .unwrap()
                .controller_2
                .update_buttons(&events);
            for event in events {
                match event {
                    sdl2::event::Event::KeyDown {
                        keycode: Some(sdl2::keyboard::Keycode::Escape),
                        ..
                    }
                    | sdl2::event::Event::Quit { .. } => {
                        //println!("Exited with cpu core state:\n{}", cpu.core);
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

        if frame_finish {
            if frames.len() == 10 {
                frames.remove(0);
            }
            frames.push(Instant::now());
            if frames.len() == 10 {
                let time_per_frame = (frames[9] - frames[0]).as_secs_f64() / 10.0;

                println!("FPS: {}", 1.0 / time_per_frame);
            }
        }

        /*if cpu::log_instr() {
            println!("{}", cpu.core);
        }*/
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}

fn cpu_eq(cpu1: &cpu::Cpu, cpu2: &MusashiCpu) -> bool {
    let mut cpu2_core: cpu::CpuCore = cpu2.into();
    cpu2_core.cycle = cpu1.core.cycle;
    cpu2_core.usp = cpu1.core.usp;

    cpu1.core == cpu2_core
}
