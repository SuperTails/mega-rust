use crate::bindings::context::Z80View;
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use sdl2::audio::AudioCallback;
use std::cell::RefCell;
use std::num::{NonZeroU16, NonZeroU8};
use std::sync::{Arc, Mutex};
use z80emu::{host::cycles, Clock, Cpu, Io, Memory, Z80NMOS};
use nuked_opn2_sys::Ym3438;
use emu76489_sys::Psg;

#[derive(Clone)]
pub struct MdAudioData(Arc<Mutex<([f32; 2048], usize, usize)>>);

#[derive(Clone)]
pub struct MdAudio{
    // Measured in Z80 cycles
    pub cycle: u64,
    pub data: MdAudioData,
}

impl MdAudio {
    pub fn new() -> MdAudio {
        MdAudio {
            cycle: 0,
            data: MdAudioData::new()
        }
    }

    // Returns true if it is time to add a sample
    pub fn do_cycle(&mut self) -> bool {
        self.cycle += 1;
        self.cycle % 87 == 0
    }

    pub fn add_sample(&self, psg: &mut Psg, ym: &mut Ym3438) {
        let mut locked = self.data.0.lock().unwrap();
        let idx = locked.2;

        let c = ym.clock();

        // TODO: Adjust volume properly
        let value = psg.calc() as f32 / 1000.0 + (c.0 as f32 + c.1 as f32) / 1000.0;

        locked.0[idx] = value / 1000.0;
        locked.2 = (locked.2 + 1) % 2048;
    }
}

impl MdAudioData {
    pub fn new() -> MdAudioData {
        MdAudioData(Arc::new(Mutex::new(([0.0; 2048], 0, 0))))
    }
}

impl AudioCallback for MdAudioData {
    type Channel = f32;

    fn callback(&mut self, data: &mut [Self::Channel]) {
        data.copy_from_slice(&self.0.lock().unwrap().0[..]);
    }
}

/* - Memory Map -
 *
 * $0000 - $2000 => Sound RAM
 * $2000 - $4000 => Reserved
 * $4000 - $6000 => YM2612
 *  $4000 => A0
 *  $4001 => D0
 *  $4002 => A1
 *  $4003 => D1
 *  $4004 - $6000 => Prohibited
 * $6000 - $8000 => Misc
 *  $6000         => Bank Register
 *  $6001 - $7F10 => Prohibited
 *  $7F11         => PSG 76489
 *  $7F12         => Prohibited
 * $8000 - $FFFF => 68k bank (??)
 *
 */

struct Z80ControlInner {
    ram: [u8; 0x2000],
    ym_chip: RefCell<Ym3438>,
    bank_address: u32,
    bank_address_bit: u32,
}

struct Z80Control<'a, 'b>(&'a mut Z80ControlInner, RefCell<Z80View<'b>>);

impl Z80ControlInner {
    pub fn new() -> Z80ControlInner {
        Z80ControlInner {
            ram: [0; 0x2000],
            ym_chip: RefCell::new(Ym3438::new()),
            bank_address: 0,
            bank_address_bit: 0,
        }
    }
}

impl Memory for Z80Control<'_, '_> {
    type Timestamp = u64;

    fn read_debug(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1FFF => self.0.ram[address as usize],
            0x2000..=0x3FFF => self.0.ram[address as usize - 0x2000],
            0x4000..=0x4003 => self.0.ym_chip.borrow_mut().read(address as u32 - 0x4000),
            0x4004..=0x5FFF => panic!("Reserved memory"),
            0x6000..=0x6000 => 0xFF, // Read from bank register
            0x6001..=0x7F10 => panic!("Reserved memory"),
            0x7F11..=0x7F11 => self.1.borrow_mut().psg.read_io(),
            0x7F12..=0x7FFF => todo!("VDP"),
            0x8000..=0xFFFF => self
                .1
                .borrow_mut()
                .read((self.0.bank_address << 15) | address as u32, Size::Byte)
                as u8,
        }
    }

    fn write_mem(&mut self, address: u16, value: u8, _ts: Self::Timestamp) {
        match address {
            0x0000..=0x1FFF => self.0.ram[address as usize] = value,
            0x2000..=0x3FFF => self.0.ram[address as usize - 0x2000] = value,
            0x4000..=0x4003 => self
                .0
                .ym_chip
                .get_mut()
                .write(address as u32 - 0x4000, value),
            0x4004..=0x5FFF => panic!("Reserved memory"),
            0x6000..=0x6000 => {
                let mask = 1 << (self.0.bank_address_bit + 15);
                self.0.bank_address_bit += 1;
                self.0.bank_address_bit %= 9;

                if value as u32 & 0x1 != 0 {
                    self.0.bank_address |= mask;
                } else {
                    self.0.bank_address &= !mask;
                }
            }
            0x6001..=0x7F10 => panic!("Reserved memory"),
            0x7F11..=0x7F11 => self.1.borrow_mut().psg.write_io(value),
            0x7F12..=0x7FFF => todo!("What is this?"),
            0x8000..=0xFFFF => todo!("Bank area"),
        }
    }
}

impl Io for Z80Control<'_, '_> {
    type Timestamp = u64;
    type WrIoBreak = ();
    type RetiBreak = ();
}

struct Z80Clock {
    time: <Self as Clock>::Timestamp,
}

impl Z80Clock {
    pub fn new() -> Z80Clock {
        Z80Clock { time: 0 }
    }
}

impl Clock for Z80Clock {
    type Limit = u64;
    type Timestamp = u64;

    fn is_past_limit(&self, limit: Self::Limit) -> bool {
        self.time > limit
    }

    fn add_irq(&mut self, _pc: u16) -> Self::Timestamp {
        todo!()
    }

    fn add_no_mreq(&mut self, _address: u16, add_ts: NonZeroU8) {
        self.time += add_ts.get() as u64;
    }

    fn add_m1(&mut self, _address: u16) -> Self::Timestamp {
        self.time += cycles::M1_CYCLE_TS as u64;
        self.as_timestamp()
    }

    fn add_mreq(&mut self, _address: u16) -> Self::Timestamp {
        self.time += cycles::MEMRW_CYCLE_TS as u64;
        self.as_timestamp()
    }

    fn add_io(&mut self, _port: u16) -> Self::Timestamp {
        todo!()
    }

    fn add_wait_states(&mut self, _bus: u16, _wait_states: NonZeroU16) {
        todo!()
    }

    fn as_timestamp(&self) -> Self::Timestamp {
        self.time
    }
}

pub struct Z80 {
    control: Z80ControlInner,
    clock: Z80Clock,
    cpu: Z80NMOS,
    pub stopped: bool,
    cycle_count: u8,
    audio_data: MdAudio,
}

impl Z80 {
    pub fn new(audio_data: MdAudio) -> Z80 {
        let mut cpu = Z80NMOS::new();
        cpu.reset();
        Z80 {
            control: Z80ControlInner::new(),
            clock: Z80Clock::new(),
            cpu: Default::default(),
            stopped: true,
            cycle_count: 0,
            audio_data,
        }
    }

    pub fn read_ext(&mut self, addr: u16, cpu: &mut dyn AddressSpace) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.control.ram[addr as usize],
            0x2000..=0x3FFF => self.control.ram[addr as usize - 0x2000],
            0x4000..=0x5FFF => self.control.ym_chip.get_mut().read((addr as u32) % 4),
            0x6000..=0x60FF => todo!("Bank address register"),
            0x6100..=0x7EFF => panic!("Unused"),
            0x7F00..=0x7FFF => todo!("VDP"),
            0x8000..=0xFFFF => {
                cpu.read(self.control.bank_address << 15 | addr as u32, Size::Byte) as u8
            }
        }
    }

    pub fn write_ext(&mut self, addr: u16, value: u8, cpu: &mut dyn AddressSpace) {
        match addr {
            0x0000..=0x1FFF => self.control.ram[addr as usize] = value,
            0x2000..=0x3FFF => self.control.ram[addr as usize - 0x2000] = value,
            0x4000..=0x5FFF => self
                .control
                .ym_chip
                .get_mut()
                .write((addr as u32) % 4, value),
            0x6000..=0x60FF => todo!("Bank address register"),
            0x6100..=0x7EFF => panic!("Unused"),
            0x7F00..=0x7FFF => todo!("VDP"),
            0x8000..=0xFFFF => cpu.write(
                self.control.bank_address << 15 | addr as u32,
                value as u32,
                Size::Byte,
            ),
        }
    }

    pub fn ram_mut(&mut self) -> &mut [u8; 0x2000] {
        &mut self.control.ram
    }

    pub fn do_cycle(&mut self, view: Z80View<'_>) {
        self.cycle_count += 1;
        self.cycle_count %= 6;

        if self.audio_data.do_cycle() {
            self.audio_data.add_sample(view.psg, self.control.ym_chip.get_mut());
        } else if self.cycle_count == 0 {
            self.control.ym_chip.get_mut().clock();
        }

        if !self.stopped && self.cycle_count % 2 == 0 {
            let dbg = |_| {};
            self.cpu
                .execute_next(
                    &mut Z80Control(&mut self.control, RefCell::new(view)),
                    &mut self.clock,
                    Some(dbg),
                )
                .unwrap();
        }
    }
}
