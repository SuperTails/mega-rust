use crate::bindings::ym3438::Ym3438;
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use sdl2::audio::AudioCallback;
use std::cell::RefCell;
use std::num::{NonZeroU16, NonZeroU8};
use std::sync::{Arc, Mutex};
use z80emu::{host::cycles, Clock, Cpu, Io, Memory, Z80NMOS};

pub struct MdAudio(Arc<Mutex<[f32; 1024]>>);

impl AudioCallback for MdAudio {
    type Channel = f32;

    fn callback(&mut self, data: &mut [Self::Channel]) {
        data.copy_from_slice(&self.0.lock().unwrap()[..]);
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

struct Z80Control {
    ram: [u8; 0x2000],
    ym_chip: RefCell<Ym3438>,
    bank_address: u32,
    bank_address_bit: u32,
}

impl Z80Control {
    pub fn new() -> Z80Control {
        Z80Control {
            ram: [0; 0x2000],
            ym_chip: RefCell::new(Ym3438::new()),
            bank_address: 0,
            bank_address_bit: 0,
        }
    }
}

impl Memory for Z80Control {
    type Timestamp = u64;

    fn read_debug(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x1FFF => self.ram[address as usize],
            0x2000..=0x3FFF => self.ram[address as usize - 0x2000],
            0x4000..=0x5FFF => self
                .ym_chip
                .borrow_mut()
                .read((address as u32 - 0x4000) % 4),
            0x6000..=0x60FF => 0xFF,
            0x6100..=0x7EFF => panic!("Reserved memory"),
            0x7F00..=0x7FFF => todo!("VDP"),
            0x8000..=0xFFFF => unsafe { crate::bindings::SYSTEM_STATE.get_mut() }
                .unwrap()
                .read((self.bank_address << 15) | address as u32, Size::Byte)
                as u8,
        }
    }

    fn write_mem(&mut self, address: u16, value: u8, _ts: Self::Timestamp) {
        match address {
            0x0000..=0x1FFF => self.ram[address as usize] = value,
            0x2000..=0x3FFF => self.ram[address as usize - 0x2000] = value,
            0x4000..=0x5FFF => self
                .ym_chip
                .get_mut()
                .write((address as u32 - 0x4000) % 4, value),
            0x6000..=0x60FF => {
                let mask = 1 << (self.bank_address_bit + 15);
                self.bank_address_bit += 1;
                self.bank_address_bit %= 9;

                if value as u32 & 0x1 != 0 {
                    self.bank_address |= mask;
                } else {
                    self.bank_address &= !mask;
                }
            }
            0x6100..=0x7EFF => panic!("Reserved memory"),
            0x7F00..=0x7FFF => todo!("VDP"),
            0x8000..=0xFFFF => todo!("Bank area"),
        }
    }
}

impl Io for Z80Control {
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
    control: Z80Control,
    clock: Z80Clock,
    cpu: Z80NMOS,
    pub stopped: bool,
    cycle_count: u8,
    audio_data: Arc<Mutex<[f32; 1024]>>,
    idx: usize,
}

impl Z80 {
    pub fn new() -> (MdAudio, Z80) {
        let mut cpu = Z80NMOS::new();
        cpu.reset();
        let audio_data = Arc::new(Mutex::new([0.0; 1024]));
        (
            MdAudio(Arc::clone(&audio_data)),
            Z80 {
                control: Z80Control::new(),
                clock: Z80Clock::new(),
                cpu: Default::default(),
                stopped: true,
                cycle_count: 0,
                idx: 0,
                audio_data,
            },
        )
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

    pub fn ram_mut(&mut self) -> &mut [u8; 0x2000] {
        &mut self.control.ram
    }

    pub fn do_cycle(&mut self) {
        self.cycle_count += 1;
        self.cycle_count %= 6;

        if !self.stopped && self.cycle_count % 2 == 0 {
            let dbg = |_| {};
            self.cpu
                .execute_next(&mut self.control, &mut self.clock, Some(dbg))
                .unwrap();
        }

        if self.cycle_count == 0 {
            let o = self.control.ym_chip.get_mut().clock();
            let o2 = (o.0 as f32 + o.1 as f32) / 1_000.0;

            self.audio_data.lock().unwrap()[self.idx] = o2;

            self.idx += 1;
            self.idx %= 1024;
        }
    }
}
