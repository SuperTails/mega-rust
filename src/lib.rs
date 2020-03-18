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
use log::{info, error};
pub use log::LevelFilter;
use sdl2::audio::AudioSpecDesired;
use sdl_system::SDLSystem;
use std::time::Instant;
use z80::MdAudio;
use circular_queue::CircularQueue;

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

fn on_scanline<T: cpu::Cpu>(context: &mut Context<T>, sdl_system: &mut SDLSystem) -> bool {
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

#[allow(dead_code)]
fn compare(mut cart: Cart, options: Options) {
    cart.rom_data[0..4].copy_from_slice(&[0xFF, 0xFF, 0xFE, 0x00]);
    init_logger(options.log_level).unwrap();

    let md_audio = MdAudio::new();

    let mut context = Context::new_musashi(cart.rom_data.clone(), md_audio.clone(), options.vdp_debug).unwrap();
    let mut context2 = Context::new_rust(cart.rom_data, md_audio.clone(), options.vdp_debug).unwrap();

    let mut history = CircularQueue::with_capacity(5);

    let hit_breakpoint = false;
    let mut sdl_system = init_sdl(md_audio.data, options.silent);

    cpu::do_log(options.trace_instructions);

    let mut frames = vec![];

    let mut last_i = 0;
    let mut i = 0;

    'running: loop {
        i += 1;

        if i == 1 {
            context2.step(&mut sdl_system);
        }

        if hit_breakpoint {
            on_breakpoint(&mut sdl_system);
        }

        let instr = {
            let (cpu, mut view) = context2.cpu_view();
            cpu.instr_at(cpu.core.pc, &mut view)
        };
        history.push((instr, context2.cpu.core.clone(), cpu::CpuCore::from_musashi(&context.cpu)));

        context2.vdp.sync(&mut context.vdp);

        let (scan_finish, frame_finish) = context.step(&mut sdl_system);
        let (sf, _) = context2.step(&mut sdl_system);

        if scan_finish && on_scanline(&mut context, &mut sdl_system) {
            break 'running;
        }
        if sf && on_scanline(&mut context2, &mut sdl_system) {
            break 'running;
        }

        if frame_finish {
            on_frame(&mut frames, &mut i, &mut last_i);
        }

        if !cpu_eq(&context2.cpu, &context.cpu) {
            let instrs = history.asc_iter().map(|i| format!("{:?}", i.0.opcode)).collect::<Vec<_>>();
            error!("Instructions:\n{}", instrs.join("\n"));
            error!("CPUs were not equal");
            error!("Musashi core:\n{}", cpu::CpuCore::from_musashi(&context.cpu));
            error!("Rust core:\n{}", context2.cpu.core);
            panic!();
        }
    }
}

pub fn run(mut cart: Cart, options: Options) {
    cart.rom_data[0..4].copy_from_slice(&[0xFF, 0xFF, 0xFE, 0x00]);
    init_logger(options.log_level).unwrap();

    let md_audio = MdAudio::new();

    let mut context = Context::new_musashi(cart.rom_data.clone(), md_audio.clone(), options.vdp_debug).unwrap();
    let mut context2 = Context::new_rust(cart.rom_data, md_audio.clone(), options.vdp_debug).unwrap();

    //let mut history = CircularQueue::with_capacity(5);

    let hit_breakpoint = false;
    let mut sdl_system = init_sdl(md_audio.data, options.silent);

    cpu::do_log(options.trace_instructions);

    let mut frames = vec![];

    let mut last_i = 0;
    let mut i = 0;

    'running: loop {
        i += 1;

        if i == 1 {
            context2.step(&mut sdl_system);
        }

        if hit_breakpoint {
            on_breakpoint(&mut sdl_system);
        }

        /*let instr = {
            let (cpu, mut view) = context2.cpu_view();
            cpu.instr_at(cpu.core.pc, &mut view)
        };
        history.push((instr, context2.cpu.core.clone(), cpu::CpuCore::from_musashi(&context.cpu)));

        context2.vdp.sync(&mut context.vdp);*/

        let (scan_finish, frame_finish) = context.step(&mut sdl_system);
        //let (sf, _) = context2.step(&mut sdl_system);

        if scan_finish && on_scanline(&mut context, &mut sdl_system) {
            break 'running;
        }
        /*if sf && on_scanline(&mut context2, &mut sdl_system) {
            break 'running;
        }*/

        if frame_finish {
            on_frame(&mut frames, &mut i, &mut last_i);
        }

        /*if !cpu_eq(&context2.cpu, &context.cpu) {
            let instrs = history.asc_iter().map(|i| format!("{:?}", i.0.opcode)).collect::<Vec<_>>();
            error!("Instructions:\n{}", instrs.join("\n"));
            error!("CPUs were not equal");
            error!("Musashi core:\n{}", cpu::CpuCore::from_musashi(&context.cpu));
            error!("Rust core:\n{}", context2.cpu.core);
            panic!();
        }*/
    }
}

#[allow(dead_code)]
#[allow(clippy::useless_let_if_seq)]
fn cpu_eq(cpu1: &cpu::RustCpu, cpu2: &MusashiCpu) -> bool {
    let mut cpu2_core = cpu::CpuCore::from_musashi(cpu2);
    cpu2_core.cycle = cpu1.core.cycle;
    cpu2_core.usp = cpu1.core.usp;
    let mut cpu1 = cpu1.clone();

    let mut correct = true;

    if cpu1.core.addr != cpu2_core.addr {
        error!("Incorrect address registers:");
        error!("RUS: {:X?}", cpu1.core.addr);
        error!("MUS: {:X?}", cpu2_core.addr);
        cpu1.core.addr = cpu2_core.addr;
        correct = false;
    }

    if cpu1.core.data != cpu2_core.data {
        error!("Incorrect data registers:");
        error!("RUS: {:X?}", cpu1.core.data);
        error!("MUS: {:X?}", cpu2_core.data);
        cpu1.core.data = cpu2_core.data;
        correct = false;
    }

    if cpu1.core.pc != cpu2_core.pc {
        error!("Incorrect PC registers:");
        error!("RUS: {:#X}", cpu1.core.pc);
        error!("MUS: {:#X}", cpu2_core.pc);
        cpu1.core.pc = cpu2_core.pc;
        correct = false;
    }

    if cpu1.core.ccr != cpu2_core.ccr {
        error!("Incorrect CCR:");
        error!("RUS: {}", cpu1.core.ccr);
        error!("MUS: {}", cpu2_core.ccr);
        cpu1.core.ccr = cpu2_core.ccr;
        correct = false;
    }

    if cpu1.core != cpu2_core {
        error!("Other difference");
        correct = false;
    }

    correct
}
