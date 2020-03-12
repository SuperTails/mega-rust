mod bindings;
pub mod cart;
mod controller;
mod cpu;
mod joypad;
mod sdl_system;
mod vdp;
mod z80;

use bindings::cpu::MusashiCpu;
use bindings::{SYSTEM_STATE, VDP, Z80};
use cart::Cart;
use controller::Controller;
use log::info;
pub use log::LevelFilter;
use once_cell::sync::OnceCell;
use sdl2::audio::AudioSpecDesired;
use sdl_system::SDLSystem;
use std::collections::BinaryHeap;
use std::time::Instant;
use vdp::Vdp;
use z80::Z80;

//const Z80_DATA: &[u8; 7110] = include_bytes!("../../z80decompressed.bin");

static mut CPU: OnceCell<MusashiCpu> = OnceCell::new();

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

fn on_scanline(sdl_system: &mut SDLSystem) -> bool {
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
                return true;
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

    false
}

fn on_frame(frames: &mut Vec<Instant>, i: &mut u32, last_i: &mut u32) {
    if frames.len() == 10 {
        frames.remove(0);
    }
    frames.push(Instant::now());
    if frames.len() == 10 {
        let time_per_frame = (frames[9] - frames[0]).as_secs_f64() / 10.0;

        info!(
            "Frame cycles: {}, FPS: {}",
            *i - *last_i,
            1.0 / time_per_frame
        );
    }

    *last_i = *i;
}

fn init_sdl(md_sound: z80::MdAudio) -> SDLSystem {
    let mut sdl_system = SDLSystem::new();
    sdl_system.canvas.set_scale(2.0, 2.0).unwrap();

    let desired = AudioSpecDesired {
        freq: Some(16000),
        channels: Some(1),
        samples: None,
    };
    sdl_system.audio_device = Some(
        sdl_system
            .audio_subsystem
            .open_playback(None, &desired, move |_spec| md_sound)
            .unwrap(),
    );
    sdl_system.audio_device.as_ref().unwrap().resume();

    sdl_system
}

fn update(pending: &mut BinaryHeap<Interrupt>, sdl_system: &mut SDLSystem) -> (bool, bool) {
    unsafe { SYSTEM_STATE.get_mut() }
        .unwrap()
        .controller_1
        .update();
    unsafe { SYSTEM_STATE.get_mut() }
        .unwrap()
        .controller_2
        .update();
    unsafe { CPU.get_mut() }.unwrap().do_cycle(pending);
    unsafe { Z80.get_mut() }.unwrap().do_cycle();

    unsafe { VDP.get_mut() }
        .unwrap()
        .do_cycle(sdl_system, pending)
}

fn init_logger(level: LevelFilter) -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "{}[{}][{}] {}",
                chrono::Local::now().format("[%H:%M:%S]"),
                record.target(),
                record.level(),
                message,
            ))
        })
        .level(level)
        .chain(std::io::stdout())
        .chain(fern::log_file("output.log")?)
        .apply()?;

    Ok(())
}

pub struct Options {
    pub trace_instructions: bool,
    pub log_level: LevelFilter,
}

pub fn run(cart: Cart, options: Options) {
    init_logger(options.log_level).unwrap();

    let (md_sound, z80) = Z80::new();

    unsafe {
        VDP.set(Vdp::new()).ok().unwrap();
        Z80.set(z80).ok().unwrap();
        CPU.set(MusashiCpu::new(
            &cart.rom_data,
            Controller::default(),
            Controller::default(),
        ).unwrap())
        .ok()
        .unwrap();
    };

    let hit_breakpoint = false;
    let mut pending: BinaryHeap<Interrupt> = BinaryHeap::new();
    let mut sdl_system = init_sdl(md_sound);

    cpu::do_log(options.trace_instructions);

    let mut frames = vec![];

    let mut last_i = 0;
    let mut i = 0;

    'running: loop {
        i += 1;

        if hit_breakpoint {
            on_breakpoint(&mut sdl_system);
        }

        let (scan_finish, frame_finish) = update(&mut pending, &mut sdl_system);

        if scan_finish && on_scanline(&mut sdl_system) {
            break 'running;
        }

        if frame_finish {
            on_frame(&mut frames, &mut i, &mut last_i);
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

#[allow(dead_code)]
fn cpu_eq(cpu1: &cpu::Cpu, cpu2: &MusashiCpu) -> bool {
    let mut cpu2_core = cpu::CpuCore::from_musashi(cpu2);
    cpu2_core.cycle = cpu1.core.cycle;
    cpu2_core.usp = cpu1.core.usp;

    cpu1.core == cpu2_core
}
