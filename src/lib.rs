mod bindings;
pub mod cart;
mod controller;
mod cpu;
mod sdl_system;
mod vdp;
mod z80;

use bindings::context::Context;
use bindings::cpu::MusashiCpu;
use cart::Cart;
use log::info;
pub use log::LevelFilter;
use sdl2::audio::AudioSpecDesired;
use sdl_system::SDLSystem;
use std::collections::BinaryHeap;
use std::time::Instant;
use z80::MdAudio;

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

fn on_scanline(context: &mut Context, sdl_system: &mut SDLSystem) -> bool {
    let events: Vec<_> = sdl_system.event_pump.poll_iter().collect();
    context.controller_1.update_buttons(&events);
    context.controller_2.update_buttons(&events);
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

fn init_sdl(md_sound: z80::MdAudioData, silent: bool) -> SDLSystem {
    let mut sdl_system = SDLSystem::new();
    sdl_system.canvas.set_scale(2.0, 2.0).unwrap();

    let desired = AudioSpecDesired {
        freq: Some(44100),
        channels: Some(1),
        samples: None,
    };
    sdl_system.audio_device = Some(
        sdl_system
            .audio_subsystem
            .open_playback(None, &desired, move |_spec| md_sound)
            .unwrap(),
    );

    if !silent {
        sdl_system.audio_device.as_ref().unwrap().resume();
    }

    sdl_system
}

#[cfg(not(miri))]
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
        .apply()?;

    Ok(())
}

#[cfg(miri)]
fn init_logger(level: LevelFilter) -> Result<(), fern::InitError> {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{}][{}] {}",
                record.target(),
                record.level(),
                message,
            ))
        })
        .level(level)
        .chain(std::io::stdout())
        .apply()?;

    Ok(())
}

pub struct Options {
    pub trace_instructions: bool,
    pub log_level: LevelFilter,
    pub silent: bool,
    pub vdp_debug: bool,
}

pub fn run(cart: Cart, options: Options) {
    init_logger(options.log_level).unwrap();

    log::warn!("TESTTESTTEST");

    let md_audio = MdAudio::new();

    let mut context = Context::new(cart.rom_data, md_audio.clone(), options.vdp_debug).unwrap();

    let hit_breakpoint = false;
    let mut pending: BinaryHeap<Interrupt> = BinaryHeap::new();
    let mut sdl_system = init_sdl(md_audio.data, options.silent);

    cpu::do_log(options.trace_instructions);

    let mut frames = vec![];

    let mut last_i = 0;
    let mut i = 0;

    'running: loop {
        i += 1;

        if hit_breakpoint {
            on_breakpoint(&mut sdl_system);
        }

        let (scan_finish, frame_finish) = context.update(&mut pending, &mut sdl_system);

        if scan_finish && on_scanline(&mut context, &mut sdl_system) {
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
