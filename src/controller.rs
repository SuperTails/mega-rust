use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::collections::HashMap;

#[derive(Default)]
pub struct ButtonStates {
    pub up: bool,
    pub down: bool,
    pub left: bool,
    pub right: bool,
    pub start: bool,
    pub mode: bool,
    pub a: bool,
    pub b: bool,
    pub c: bool,
    pub x: bool,
    pub y: bool,
    pub z: bool,
}

#[derive(Default)]
pub struct Controller {
    pub states: ButtonStates,
    /// 0 = read, 1 = write
    pub th: bool,
    pub state: u32,
    timeout: u32,
}

macro_rules! table {
    ($s:ident, $($e:pat => ($($r:tt,)+),)*) => {
        match ($s.state, $s.th) {
            $($e => table!(@buttons $s 6, $($r,)+),)*
            a => panic!("Invalid state {:?}", a),
        }
    };
    (@buttons $s:ident $shift:expr, ) => {
        0
    };
    (@buttons $s:ident $shift:expr, 0, $($bt:tt,)*) => {
        table!(@buttons $s ($shift - 1), $($bt,)*)
    };
    (@buttons $s:ident $shift:expr, 1, $($bt:tt,)*) => {
        { (1_u8 << $shift) | table!(@buttons $s ($shift - 1), $($bt,)*) }
    };
    (@buttons $s:ident $shift:expr, th, $($bt:tt,)*) => {
        { (($s.th as u8) << $shift) | table!(@buttons $s ($shift - 1), $($bt,)*) }
    };
    (@buttons $s:ident $shift:expr, $b:tt, $($bt:tt,)*) => {
        { ((!$s.states.$b as u8) << $shift) | table!(@buttons $s ($shift - 1), $($bt,)*) }
    };
}

impl Controller {
    pub fn update(&mut self) {
        self.timeout += 1;
        if self.timeout >= 1000 {
            self.state = 0;
        }
    }

    pub fn update_buttons(&mut self, events: &[Event]) {
        let mut mappings: HashMap<Keycode, &mut bool> = vec![
            (Keycode::Down, &mut self.states.down),
            (Keycode::Up, &mut self.states.up),
            (Keycode::Left, &mut self.states.left),
            (Keycode::Right, &mut self.states.right),
            (Keycode::Space, &mut self.states.start),
            (Keycode::A, &mut self.states.a),
            (Keycode::S, &mut self.states.b),
            (Keycode::D, &mut self.states.c),
        ].into_iter().collect();

        for event in events.iter() {
            match event {
                Event::KeyDown {
                    keycode: Some(keycode),
                    ..
                } => {
                    if let Some(button) = mappings.get_mut(keycode) {
                        **button = true;
                    }
                }
                Event::KeyUp {
                    keycode: Some(keycode),
                    ..
                } => {
                    if let Some(button) = mappings.get_mut(keycode) {
                        **button = false;
                    }
                }
                _ => {}
            }
        }

        for event in events {
            match event {
                Event::KeyDown {
                    keycode: Some(Keycode::Down),
                    ..
                } => {
                    self.states.down = true;
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Down),
                    ..
                } => {
                    self.states.down = false;
                }
                Event::KeyDown {
                    keycode: Some(Keycode::Return),
                    ..
                } => {
                    self.states.start = true;
                }
                Event::KeyUp {
                    keycode: Some(Keycode::Return),
                    ..
                } => {
                    self.states.down = false;
                }
                _ => {}
            }
        }
    }

    pub fn write_reg1(&mut self, value: u8) {
        self.timeout = 0;
        //let old_th = self.th;
        self.th = (value >> 6) & 1 != 0;
        /*if !old_th && self.th {
            self.state += 1;
            self.state %= 4;
        }*/
    }

    pub fn read_reg1(&mut self) -> u8 {
        self.timeout = 0;
        table! { self,
            (0, true) => (th, c, b, right, left, down, up, ),
            (0, false)  => (th, start, a, 0, 0, down, up, ),
            (1, true) => (th, c, b, right, left, down, up, ),
            (1, false)  => (th, start, a, 0, 0, 0, 0, ),
            (2, true) => (th, c, b, x, y, z, mode, ),
            (2, false)  => (th, start, a, 1, 1, 1, 1, ),
            (3, true) => (th, c, b, right, left, down, up, ),
            (3, false)  => (th, start, a, 0, 0, down, up, ),
        }
    }

    pub fn write_reg2(&self, value: u8) {
        // TODO:
        if value != 0x40 {
            println!("Ignoring write to controller RW register of {:#X}", value);
        }
    }
}
