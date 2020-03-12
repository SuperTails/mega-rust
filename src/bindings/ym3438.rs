use super::inner::{
    ym3438_t, OPN2_Clock, OPN2_Read, OPN2_ReadIRQPin, OPN2_ReadTestPin, OPN2_Reset, OPN2_Write,
};

pub struct Ym3438(ym3438_t);

#[allow(dead_code)]
impl Ym3438 {
    pub fn new() -> Ym3438 {
        let mut chip = Ym3438(unsafe { std::mem::zeroed::<ym3438_t>() });
        chip.reset();
        chip
    }

    pub fn reset(&mut self) {
        unsafe {
            OPN2_Reset(&mut self.0 as *mut _);
        }
    }

    pub fn clock(&mut self) -> (i16, i16) {
        let mut buffer = [0, 0];
        unsafe {
            OPN2_Clock(&mut self.0 as *mut _, buffer.as_mut_ptr());
        }
        (buffer[0], buffer[1])
    }

    pub fn write(&mut self, port: u32, data: u8) {
        unsafe { OPN2_Write(&mut self.0 as *mut _, port, data) };
    }

    pub fn read_test_pin(&mut self) -> u32 {
        unsafe { OPN2_ReadTestPin(&mut self.0 as *mut _) }
    }

    pub fn read_irq_pin(&mut self) -> u32 {
        unsafe { OPN2_ReadIRQPin(&mut self.0 as *mut _) }
    }

    pub fn read(&mut self, port: u32) -> u8 {
        unsafe { OPN2_Read(&mut self.0 as *mut _, port) }
    }
}
