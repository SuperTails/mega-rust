mod state_pair;
use super::context::CpuView;
use super::inner::{
    m68k_cycles_run, m68k_disassemble, m68k_end_timeslice, m68k_execute, m68k_get_reg, m68k_init,
    m68k_pulse_reset, m68k_register_t, m68k_register_t_M68K_REG_D0, m68k_set_cpu_type,
    m68k_set_instr_hook_callback, m68k_set_irq, M68K_CPU_TYPE_68000,
};
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use crate::cpu::Cpu;
use log::trace;
use state_pair::*;
use std::ffi::CStr;
use std::os::raw::c_uint;
use std::sync::atomic::{AtomicBool, Ordering};


fn read_memory(address: c_uint, size: Size) -> c_uint {
    unsafe { TEMP_DATA.read(address, size) }
}

fn write_memory(address: c_uint, size: Size, value: c_uint) {
    unsafe { TEMP_DATA.write(address, value, size) }
}

static mut INSTRUCTION_HAPPENED: bool = false;

extern "C" fn on_instruction(pc: c_uint) {
    unsafe { INSTRUCTION_HAPPENED = true; }

    if crate::cpu::log_instr() {
        trace!("PC is now {:#X}", pc);
        // TODO: We know that a cpu must exist, so we can print it, but ughhh
        //trace!("State is {}", crate::cpu::CpuCore::from_musashi());
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

// Only one instance of a `MusashiCpu` is allowed to exist at a time,
// otherwise we can run into *gasp* soundness problems.
// And also it just won't make sense
static CPU_EXISTS: AtomicBool = AtomicBool::new(false);

pub struct MusashiCpu {
    _deny: (),
}

impl MusashiCpu {
    pub unsafe fn partial_new() -> Result<MusashiCpu, ()> {
        // If the CPU already existed, then we can't create another
        if CPU_EXISTS.swap(true, Ordering::SeqCst) {
            return Err(());
        }

        m68k_init();
        m68k_set_cpu_type(M68K_CPU_TYPE_68000);
        m68k_set_instr_hook_callback(Some(on_instruction));

        Ok(MusashiCpu { _deny: () })
    }

    #[allow(dead_code)]
    pub fn new(context: CpuView) -> Result<MusashiCpu, ()> {
        let mut m = unsafe { Self::partial_new()? };
        m.pulse_reset(context);
        Ok(m)
    }
    
    pub fn get_reg(&self, reg: Register) -> u32 {
        unsafe { m68k_get_reg(std::ptr::null_mut(), reg as m68k_register_t) }
    }

    pub fn pulse_reset(&mut self, mut context: CpuView) {
        unsafe {
            TEMP_DATA = StatePair(self as *mut _, context.as_raw());
            m68k_pulse_reset();
            TEMP_DATA = EMPTY_STATE;
        }
    }

    pub fn end_timeslice(&mut self, context: &mut CpuView) {
        let cycles = unsafe { m68k_cycles_run() };
        unsafe { m68k_end_timeslice() };

        for _ in 0..(cycles as u32).saturating_sub(1) {
            context.controller_1.update();
            context.controller_2.update();

            let view = super::context::Z80View {
                cpu: self,
                vdp: context.vdp,
                psg: context.psg,
                controller_1: context.controller_1,
                controller_2: context.controller_2,
                cart_ram: context.cart_ram,
                ram: context.ram,
                rom: context.rom,
                pending: context.pending,
            };
            context.z80.do_cycle(view);

            // TODO: ADVANCE THE PSG COUNTER TO OUTPUT SOUND

            // TODO: GET RENDERING FROM THIS
            context.vdp.do_cycle2(context.pending);
        }
    }
}

impl Drop for MusashiCpu {
    fn drop(&mut self) {
        // If we're dropping a CPU that "doesn't exist,"
        // then we have REALLY BIG PROBLEMS
        assert!(CPU_EXISTS.swap(false, Ordering::SeqCst));
    }
}

impl Cpu for MusashiCpu {
    fn step(&mut self, context: &mut CpuView) {
        unsafe {
            INSTRUCTION_HAPPENED = false;
            TEMP_DATA = StatePair(self as *mut _, context.as_raw());
        };

        unsafe {
            while !INSTRUCTION_HAPPENED {
                m68k_execute(1);
            }
        }

        self.end_timeslice(context);

        if let Some(int) = context.pending.peek() {
            if *int as u32 > (self.get_reg(Register::Sr) >> 8) & 0x7 {
                unsafe {
                    m68k_set_irq(*int as u32);
                }
                context.pending.pop();
            }
        }

        // Don't wanna be keeping those pointers around
        unsafe {
            TEMP_DATA = EMPTY_STATE;
        }
    }

    /// When the CPU is executing, it should have exclusive access to the other
    /// components of the system
    fn execute(&mut self, context: &mut CpuView) {
        unsafe {
            TEMP_DATA = StatePair(self as *mut _, context.as_raw());
        };

        unsafe {
            m68k_execute(1_000);
        };

        self.end_timeslice(context);

        if let Some(int) = context.pending.peek() {
            if *int as u32 > (self.get_reg(Register::Sr) >> 8) & 0x7 {
                unsafe {
                    m68k_set_irq(*int as u32);
                }
                context.pending.pop();
            }
        }

        // Don't wanna be keeping those pointers around
        unsafe {
            TEMP_DATA = EMPTY_STATE;
        }
    }
}

#[no_mangle]
extern "C" fn m68k_read_immediate_16(address: c_uint) -> c_uint {
    unsafe { TEMP_DATA.read_immediate_16(address) }
}

#[no_mangle]
extern "C" fn m68k_read_immediate_32(address: c_uint) -> c_uint {
    unsafe { TEMP_DATA.read_immediate_32(address) }
}

#[no_mangle]
extern "C" fn m68k_read_pcrelative_8(address: c_uint) -> c_uint {
    m68k_read_memory_8(address)
}

#[no_mangle]
extern "C" fn m68k_read_pcrelative_16(address: c_uint) -> c_uint {
    m68k_read_memory_16(address)
}

#[no_mangle]
extern "C" fn m68k_read_pcrelative_32(address: c_uint) -> c_uint {
    m68k_read_memory_32(address)
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