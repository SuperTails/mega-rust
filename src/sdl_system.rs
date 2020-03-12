use crate::z80::MdAudio;
use sdl2::render::WindowCanvas;
use sdl2::{audio::AudioDevice, AudioSubsystem, EventPump, Sdl, VideoSubsystem};

pub struct SDLSystem {
    pub ctx: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub audio_subsystem: AudioSubsystem,
    pub audio_device: Option<AudioDevice<MdAudio>>,
    pub event_pump: EventPump,
    pub canvas: WindowCanvas,
}

impl SDLSystem {
    pub fn new() -> SDLSystem {
        let ctx = sdl2::init().unwrap();
        let video_subsystem = ctx.video().unwrap();
        let audio_subsystem = ctx.audio().unwrap();
        let window = video_subsystem
            .window("Nintendn't", 1024 + 256, 480)
            .position_centered()
            .build()
            .unwrap();
        let canvas = window.into_canvas().build().unwrap();
        let event_pump = ctx.event_pump().unwrap();
        SDLSystem {
            ctx,
            video_subsystem,
            audio_subsystem,
            audio_device: None,
            canvas,
            event_pump,
        }
    }

    pub fn canvas(&mut self) -> &mut WindowCanvas {
        &mut self.canvas
    }
}
