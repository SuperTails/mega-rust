use sdl2::render::WindowCanvas;
use sdl2::{EventPump, Sdl, VideoSubsystem};

pub struct SDLSystem {
    pub ctx: Sdl,
    pub video_subsystem: VideoSubsystem,
    pub canvas: WindowCanvas,
    pub event_pump: EventPump,
}

impl SDLSystem {
    pub fn new() -> SDLSystem {
        let ctx = sdl2::init().unwrap();
        let video_subsystem = ctx.video().unwrap();
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
            canvas,
            event_pump,
        }
    }

    pub fn canvas(&mut self) -> &mut WindowCanvas {
        &mut self.canvas
    }
}
