use super::inner::{
    m68k_disassemble, m68k_execute, m68k_get_reg, m68k_init, m68k_pulse_reset, m68k_register_t,
    m68k_register_t_M68K_REG_D0, m68k_set_cpu_type, m68k_set_instr_hook_callback, m68k_set_irq,
    M68K_CPU_TYPE_68000,
};
use super::{SystemState, SYSTEM_STATE};
use crate::controller::Controller;
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use crate::Interrupt;
use log::trace;
use std::collections::BinaryHeap;
use std::ffi::CStr;
use std::os::raw::c_uint;

fn read_memory(address: c_uint, size: Size) -> c_uint {
    unsafe { SYSTEM_STATE.get_mut() }
        .unwrap()
        .read(address, size)
}

fn write_memory(address: c_uint, size: Size, value: c_uint) {
    unsafe { SYSTEM_STATE.get_mut() }
        .unwrap()
        .write(address, value, size);
}

#[no_mangle]
extern "C" fn m68k_read_memory_8(address: c_uint) -> c_uint {
    read_memory(address, Size::Byte)
}

#[no_mangle]
extern "C" fn m68k_read_memory_16(address: c_uint) -> c_uint {
    read_memory(address, Size::Word)
}

#[no_mangle]
extern "C" fn m68k_read_memory_32(address: c_uint) -> c_uint {
    read_memory(address, Size::Long)
}

#[no_mangle]
extern "C" fn m68k_read_disassembler_8(address: c_uint) -> c_uint {
    m68k_read_memory_8(address)
}

#[no_mangle]
extern "C" fn m68k_read_disassembler_16(address: c_uint) -> c_uint {
    m68k_read_memory_16(address)
}

#[no_mangle]
extern "C" fn m68k_read_disassembler_32(address: c_uint) -> c_uint {
    m68k_read_memory_32(address)
}

#[no_mangle]
extern "C" fn m68k_write_memory_8(address: c_uint, value: c_uint) {
    write_memory(address, Size::Byte, value);
}

#[no_mangle]
extern "C" fn m68k_write_memory_16(address: c_uint, value: c_uint) {
    write_memory(address, Size::Word, value);
}

#[no_mangle]
extern "C" fn m68k_write_memory_32(address: c_uint, value: c_uint) {
    write_memory(address, Size::Long, value);
}

extern "C" fn on_instruction(pc: c_uint) {
    if crate::cpu::log_instr() {
        trace!("PC is now {:#X}", pc);
        trace!("State is {}", crate::cpu::CpuCore::from_musashi());
        let mut buffer = [0; 100];
        unsafe { m68k_disassemble(buffer.as_mut_ptr() as *mut i8, pc, M68K_CPU_TYPE_68000) };
        let nul_idx = buffer.iter().enumerate().find(|(_, i)| **i == 0).unwrap().0;
        let buffer = &buffer[0..=nul_idx];
        let instr = CStr::from_bytes_with_nul(buffer).unwrap();
        trace!("{:?}\n", instr);
    }
}

// I hope Musashi never reorders this enum
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(dead_code)]
pub enum Register {
    D0 = m68k_register_t_M68K_REG_D0 as isize,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    A0,
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    Pc,
    Sr,
    Sp,
    Usp,
    Isp,
    Msp,
    Sfc,
    Dfc,
    Vbr,
    Cacr,
    Caar,
    Addr,
    Data,
    Ppc,
    Ir,
    CpuType,
}

impl Register {
    pub fn data(idx: usize) -> Register {
        match idx {
            0 => Register::D0,
            1 => Register::D1,
            2 => Register::D2,
            3 => Register::D3,
            4 => Register::D4,
            5 => Register::D5,
            6 => Register::D6,
            7 => Register::D7,
            _ => panic!("Invalid data register index {}", idx),
        }
    }

    pub fn addr(idx: usize) -> Register {
        match idx {
            0 => Register::A0,
            1 => Register::A1,
            2 => Register::A2,
            3 => Register::A3,
            4 => Register::A4,
            5 => Register::A5,
            6 => Register::A6,
            7 => Register::A7,
            _ => panic!("Invalid address register index {}", idx),
        }
    }
}

pub struct MusashiCpu {}

impl MusashiCpu {
    pub fn new(rom_data: &[u8], controller_1: Controller, controller_2: Controller) -> MusashiCpu {
        let cart_ram = {
            let mut cart_ram = Vec::new();
            cart_ram.resize(0x40_0000 - rom_data.len(), 0);
            Box::from(&cart_ram[..])
        };

        unsafe {
            SYSTEM_STATE
                .set(SystemState {
                    rom: rom_data.into(),
                    controller_1,
                    controller_2,
                    ram: [0; 0x1_0000][..].into(),
                    cart_ram,
                })
                .ok()
                .unwrap()
        };

        unsafe {
            m68k_init();
            m68k_set_cpu_type(M68K_CPU_TYPE_68000);
            m68k_set_instr_hook_callback(Some(on_instruction));
            m68k_pulse_reset();
        };

        MusashiCpu {}
    }

    pub fn do_cycle(&mut self, pending: &mut BinaryHeap<Interrupt>) {
        unsafe {
            m68k_execute(1);
        };

        if let Some(int) = pending.peek() {
            if *int as u32 > (Self::get_reg(Register::Sr) >> 8) & 0x7 {
                unsafe {
                    m68k_set_irq(*int as u32);
                }
                pending.pop();
            }
        }
    }

    // TODO: Replace with a proper enum
    pub fn get_reg(reg: Register) -> u32 {
        unsafe { m68k_get_reg(std::ptr::null_mut(), reg as m68k_register_t) }
    }
}
