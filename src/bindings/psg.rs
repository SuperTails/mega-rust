use super::inner::{
    SNG_calc, SNG_calc_stereo, SNG_delete, SNG_new, SNG_readReg, SNG_reset, SNG_setVolumeMode,
    SNG_set_quality, SNG_set_rate, SNG_writeGGIO, SNG_writeIO, SNG,
};
use crate::z80::MdAudio;
use log::warn;

#[no_mangle]
unsafe extern "C" fn SNG_readIO(_: *mut SNG) -> u8 {
    warn!("WHERE DID THE READ IO FUNCTION GO WHY DOESN'T IT EXIST");
    0
}

pub struct Psg {
    raw: RawPsg,
    cycle: u64,
    audio_data: MdAudio,
}

impl Psg {
    pub fn new(audio_data: MdAudio) -> Psg {
        Psg {
            raw: RawPsg::new(3_579_545, 44_100),
            cycle: 0,
            audio_data,
        }
    }

    /// This function should be called at the CPU clock rate
    /// (7.670454 MHz)
    pub fn do_cycle(&mut self) {
        self.cycle += 1;
        // TODO: It's technically 479.5
        if self.cycle % 174 == 0 {
            self.audio_data.add_psg_sample(self.raw.calc() as f32)
        }
    }

    pub fn read(&mut self) -> u8 {
        self.raw.read_io()
    }

    pub fn write(&mut self, value: u8) {
        self.raw.write_io(value as u32)
    }
}

struct RawPsg(*mut SNG);

#[allow(dead_code)]
impl RawPsg {
    pub fn new(clock: u32, rate: u32) -> RawPsg {
        let mut s = RawPsg(unsafe { SNG_new(clock, rate) });
        s.reset();
        s
    }

    pub fn reset(&mut self) {
        unsafe { SNG_reset(self.0) }
    }

    pub fn set_rate(&mut self, rate: u32) {
        unsafe { SNG_set_rate(self.0, rate) }
    }

    pub fn set_quality(&mut self, quality: u32) {
        unsafe { SNG_set_quality(self.0, quality) }
    }

    pub fn write_io(&mut self, value: u32) {
        unsafe { SNG_writeIO(self.0, value) }
    }

    pub fn read_register(&mut self, register: u32) -> u8 {
        unsafe { SNG_readReg(self.0, register) }
    }

    pub fn read_io(&mut self) -> u8 {
        unsafe { SNG_readIO(self.0) }
    }

    pub fn calc(&mut self) -> i16 {
        unsafe { SNG_calc(self.0) }
    }

    pub fn set_volume_mode(&mut self, mode: i32) {
        unsafe { SNG_setVolumeMode(self.0, mode) }
    }

    pub fn calc_stereo(&mut self) -> (i32, i32) {
        let mut buf = [0, 0];
        unsafe { SNG_calc_stereo(self.0, buf.as_mut_ptr()) }
        (buf[0], buf[1])
    }

    pub fn write_ggio(&mut self, value: u32) {
        unsafe { SNG_writeGGIO(self.0, value) }
    }
}

impl Drop for RawPsg {
    fn drop(&mut self) {
        unsafe { SNG_delete(self.0) }
    }
}
