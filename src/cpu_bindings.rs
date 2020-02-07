#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(clippy::unreadable_literal)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

use once_cell::sync::OnceCell;
use std::sync::Weak;
use std::sync::Mutex;
use std::sync::atomic::{AtomicBool, Ordering};
use std::collections::BinaryHeap;
use crate::Interrupt;
use crate::controller::Controller;
use crate::vdp::Vdp;
use crate::cpu::{instruction::Size, address_space::AddressSpace};
use ::std::os::raw::{c_uint, c_int};

pub struct SystemState {
    pub rom: Box<[u8]>,
    pub cart_ram: Box<[u8]>,
    pub ram: Box<[u8]>,
    pub z80_ram: Box<[u8]>,
    pub vdp: Weak<Mutex<Vdp>>,
    pub controller_1: Weak<Mutex<Controller>>,
    pub controller_2: Weak<Mutex<Controller>>,
}

impl AddressSpace for SystemState {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let length = size.len();
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        if size == Size::Long && addr == 0xFFFF_FFFC {
            println!("RETURNING 'init'");
            return u32::from_be_bytes(*b"init");
        }

        if addr == 0xA1_0000 || addr == 0xA1_0001 {
            // TODO: Is this correct?
            return u32::from_be_bytes(*b"UEUE") & size.mask();
        }

        if addr == 0xA1_0003 || addr == 0xA1_0004 {
            assert_eq!(size, Size::Byte);
            let controller1 = self.controller_1.upgrade().unwrap();
            return controller1.lock().unwrap().read_reg1() as u32;
        }

        if addr == 0xA1_0005 || addr == 0xA1_0006 {
            assert_eq!(size, Size::Byte);
            let controller2 = self.controller_2.upgrade().unwrap();
            return controller2.lock().unwrap().read_reg1() as u32;
        }

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            let vdp = self.vdp.upgrade().unwrap();
            return vdp.lock().unwrap().read(addr as u32) as u32;
        }

        let mut result = 0;

        for offset in 0..length {
            result <<= 8;
            let byte_addr = addr + offset;
            let byte = if byte_addr < self.rom.len() {
                self.rom[byte_addr]
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()]
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000]
            } else if (0xA0_0000..=0xA0_FFFF).contains(&byte_addr) {
                // TODO: Determine if this is specially mapped
                self.z80_ram[byte_addr - 0xA0_0000]
            } else {
                println!("Unimplemented address {:#X}, reading zero", addr);
                0
            };

            result |= byte as u32;
        }

        result
    }

    fn write(&mut self, address: u32, value: u32, size: Size) {
        let length = size.len();
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(
            addr & (align - 1),
            0,
            "Invalid unaligned write of size {:?} with addr: {:#08X}",
            size,
            addr
        );

        if (0xC0_00_00..=0xC0_00_0F).contains(&addr) {
            let vdp = self.vdp.upgrade().unwrap();
            vdp.lock().unwrap().write(addr as u32, value, size, self);
            return;
        }

        match addr {
            0xA1_0003..=0xA1_0004 => {
                let controller1 = self.controller_1.upgrade().unwrap();
                controller1.lock().unwrap().write_reg1(value as u8);
                return;
            }
            0xA1_0009..=0xA1_000A => {
                assert_eq!(size, Size::Byte);
                let controller1 = self.controller_1.upgrade().unwrap();
                controller1.lock().unwrap().write_reg2(value as u8);
                return;
            }
            0xA1_0000..=0xA1_0002 | 0xA1_0005..=0xA1_000F => {
                // TODO:
                println!("Ignoring write to some stuff: {:#08X}", addr);
                return;
            }
            0xA1_1100..=0xA1_1101 => {
                // TODO:
                println!("Ignoring Z80 bus request");
                return;
            }
            0xA1_1200 => {
                // TODO:
                println!("Ignoring Z80 reset");
                return;
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO:
                println!("Ignoring write to PSG output");
                return;
            }
            0xA1_0020..=0xA1_10FF => {
                // Should we panic here, or not?
                println!("Ignoring write to reserved memory");
                return;
            }
            0xA1_30F1..=0xA1_30F2 => {
                // TODO:
                println!("Ignoring write of {:#X} to SRAM register", value);
                return;
            }
            0xC0_0000..=0xC0_000F => {
                self.vdp
                    .upgrade()
                    .unwrap()
                    .lock()
                    .unwrap()
                    .write(addr as u32, value, size, self);
            }
            _ => {}
        }

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < self.rom.len() {
                //self.rom[byte_addr] = byte;
                println!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_FFFF).contains(&byte_addr) {
                // TODO: Determine if this is specially mapped
                self.z80_ram[byte_addr - 0xA0_0000] = *byte;
            } else {
                println!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}

static POP_INT: AtomicBool = AtomicBool::new(false);
static SYSTEM_STATE: OnceCell<Mutex<SystemState>> = OnceCell::new();

fn read_memory(address: c_uint, size: Size) -> c_uint {
    SYSTEM_STATE.get().unwrap().lock().unwrap().read(address, size)
}

fn write_memory(address: c_uint, size: Size, value: c_uint) {
    SYSTEM_STATE.get().unwrap().lock().unwrap().write(address, value, size);
}

#[no_mangle]
extern fn m68k_read_memory_8(address: c_uint) -> c_uint {
    read_memory(address, Size::Byte)
}

#[no_mangle]
extern fn m68k_read_memory_16(address: c_uint) -> c_uint {
    read_memory(address, Size::Word)
}

#[no_mangle]
extern fn m68k_read_memory_32(address: c_uint) -> c_uint {
    read_memory(address, Size::Long)
}

#[no_mangle]
extern fn m68k_write_memory_8(address: c_uint, value: c_uint) {
    write_memory(address, Size::Byte, value);
}

#[no_mangle]
extern fn m68k_write_memory_16(address: c_uint, value: c_uint) {
    write_memory(address, Size::Word, value);
}

#[no_mangle]
extern fn m68k_write_memory_32(address: c_uint, value: c_uint) {
    write_memory(address, Size::Long, value);
}

extern fn on_instruction(_pc: c_uint) {
    //println!("PC is now {:#X}", pc);
}

extern fn on_interrupt_ack(level: c_int) -> c_int {
    println!("Interrupt ack with level {}", level);
    POP_INT.store(true, Ordering::SeqCst);
    M68K_INT_ACK_AUTOVECTOR as i32
}

pub struct MusashiCpu {

}

impl MusashiCpu {
    pub fn new(
        rom_data: &[u8],
        vdp: Weak<Mutex<Vdp>>,
        controller_1: Weak<Mutex<Controller>>,
        controller_2: Weak<Mutex<Controller>>
    ) -> MusashiCpu {
        let cart_ram = {
            let mut cart_ram = Vec::new();
            cart_ram.resize(0x40_0000 - rom_data.len(), 0);
            Box::from(&cart_ram[..])
        };

        SYSTEM_STATE.set(
            Mutex::new(SystemState {
                rom: rom_data.into(),
                vdp,
                controller_1,
                controller_2,
                ram: [0; 0x1_0000][..].into(),
                z80_ram: [0; 0x1_0000][..].into(),
                cart_ram,
            })
        ).ok().unwrap();

        unsafe {
            m68k_init();
            m68k_set_cpu_type(M68K_CPU_TYPE_68000);
            m68k_set_instr_hook_callback(Some(on_instruction));
            m68k_set_int_ack_callback(Some(on_interrupt_ack));
            m68k_pulse_reset();
        };

        MusashiCpu {

        }
    }

    pub fn do_cycle(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        if POP_INT.swap(false, Ordering::SeqCst) {
            println!("Popped {:?}", pending.pop());
            unsafe { m68k_set_irq(0); }
        }

        unsafe { m68k_execute(5); };

        if let Some(int) = pending.peek() {
            unsafe { m68k_set_irq(*int as u32); }
        }
    }
}