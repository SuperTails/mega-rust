use crate::bindings::cpu::MusashiCpu;
use crate::controller::Controller;
use crate::cpu::address_space::AddressSpace;
use crate::cpu::instruction::Size;
use crate::cpu::{Cpu, RustCpu};
use crate::vdp::Vdp;
use crate::z80::MdAudio;
use crate::z80::Z80;
use emu76489_sys::{Psg, GENESIS_CLOCK_NTSC};
use log::{info, warn};
use std::collections::BinaryHeap;
use crate::Interrupt;
use crate::sdl_system::SDLSystem;

pub struct Context<T: Cpu> {
    pub cpu: T,
    pub vdp: Vdp,
    pub z80: Z80,
    pub psg: Psg,
    pub controller_1: Controller,
    pub controller_2: Controller,
    pub rom: Box<[u8]>,
    pub cart_ram: Box<[u8]>,
    pub ram: Box<[u8]>,
    pub pending: BinaryHeap<Interrupt>,
}

pub struct ContextView<'a, T: Cpu> {
    pub cpu: &'a mut T,
    pub vdp: &'a mut Vdp,
    pub z80: &'a mut Z80,
    pub psg: &'a mut Psg,
    pub controller_1: &'a mut Controller,
    pub controller_2: &'a mut Controller,
    pub rom: &'a mut [u8],
    pub cart_ram: &'a mut [u8],
    pub ram: &'a mut [u8],
    pub pending: &'a mut BinaryHeap<Interrupt>,
}

pub struct MemoryView<'a> {
    pub rom: &'a mut [u8],
    pub cart_ram: &'a mut [u8],
    pub ram: &'a mut [u8],
}

impl Context<RustCpu> {
    pub fn new_rust(rom: Box<[u8]>, md_audio: MdAudio, vdp_debug: bool) -> Result<Self, ()> {
        let cart_ram = {
            let mut c = Vec::new();
            c.resize(0x40_0000 - rom.len(), 0);
            Box::from(&c[..])
        };

        Ok(Context {
            cpu: RustCpu::new(),
            vdp: Vdp::new(vdp_debug),
            z80: Z80::new(md_audio),
            psg: Psg::new(GENESIS_CLOCK_NTSC, 44100),
            controller_1: Controller::default(),
            controller_2: Controller::default(),
            rom,
            cart_ram,
            ram: [0; 0x1_0000][..].into(),
            pending: BinaryHeap::new(),
        })
    }
}

impl Context<MusashiCpu> {
    pub fn new_musashi(rom: Box<[u8]>, md_audio: MdAudio, vdp_debug: bool) -> Result<Self, ()> {
        let cart_ram = {
            let mut c = Vec::new();
            c.resize(0x40_0000 - rom.len(), 0);
            Box::from(&c[..])
        };

        let mut m = Context {
            cpu: unsafe { MusashiCpu::partial_new()? },
            vdp: Vdp::new(vdp_debug),
            z80: Z80::new(md_audio),
            psg: Psg::new(GENESIS_CLOCK_NTSC, 44100),
            controller_1: Controller::default(),
            controller_2: Controller::default(),
            rom,
            cart_ram,
            ram: [0; 0x1_0000][..].into(),
            pending: BinaryHeap::new(),
        };

        let (l, r) = m.cpu_view();
        l.pulse_reset(r);

        Ok(m)
    }
} 

impl<T: Cpu> Context<T> {
    #[allow(dead_code)]
    pub fn update(&mut self, sdl_system: &mut SDLSystem) -> (bool, bool) {
        self.controller_1.update();
        self.controller_2.update();
        let (l, mut r) = self.cpu_view();
        l.execute(&mut r);
        let (l, r) = self.z80_view();
        l.do_cycle(r);
        // TODO: UPDATE THE PSG CYCLE COUNT SO IT MAKES SOUND
        self.vdp.do_cycle(sdl_system, &mut self.pending)
    }

    pub fn step(&mut self, sdl_system: &mut SDLSystem) -> (bool, bool) {
        self.controller_1.update();
        self.controller_2.update();
        let (l, mut r) = self.cpu_view();
        l.step(&mut r);
        let (l, r) = self.z80_view();
        l.do_cycle(r);
        // TODO: UPDATE THE PSG CYCLE COUNT SO IT MAKES SOUND
        self.vdp.do_cycle(sdl_system, &mut self.pending)
    }

    pub fn cpu_view(&mut self) -> (&mut T, CpuView) {
        (
            &mut self.cpu,
            CpuView {
                vdp: &mut self.vdp,
                z80: &mut self.z80,
                psg: &mut self.psg,
                controller_1: &mut self.controller_1,
                controller_2: &mut self.controller_2,
                rom: &mut self.rom,
                cart_ram: &mut self.cart_ram,
                ram: &mut self.ram,
                pending: &mut self.pending,
            },
        )
    }

    pub fn z80_view(&mut self) -> (&mut Z80, Z80View) {
        (
            &mut self.z80,
            Z80View {
                cpu: &mut self.cpu,
                vdp: &mut self.vdp,
                psg: &mut self.psg,
                controller_1: &mut self.controller_1,
                controller_2: &mut self.controller_2,
                rom: &mut self.rom,
                cart_ram: &mut self.cart_ram,
                ram: &mut self.ram,
                pending: &mut self.pending,
            },
        )
    }

    #[allow(dead_code)]
    fn as_view(&mut self) -> ContextView<T> {
        ContextView {
            cpu: &mut self.cpu,
            vdp: &mut self.vdp,
            z80: &mut self.z80,
            psg: &mut self.psg,
            controller_1: &mut self.controller_1,
            controller_2: &mut self.controller_2,
            rom: &mut self.rom,
            cart_ram: &mut self.cart_ram,
            ram: &mut self.ram,
            pending: &mut self.pending,
        }
    }
}

macro_rules! make_view {
    (pub struct $rawname:ident; pub struct $name:ident<'a> { $(pub $varname:ident: &'a mut $vartype:tt,)* }) => {
        pub struct $name<'a> {
            $(pub $varname: &'a mut $vartype,)*
        }

        impl $name<'_> {
            #[allow(dead_code)]
            pub fn as_raw(&mut self) -> $rawname {
                $rawname {
                    $($varname: self.$varname as *mut _,)*
                }
            }
        }

        #[allow(dead_code)]
        pub struct $rawname {
            $(pub $varname: *mut $vartype,)*
        }

        impl $rawname {
            #[allow(dead_code)]
            pub fn new() -> Self {
                unsafe { std::mem::zeroed() }
            }

            #[allow(dead_code)]
            pub unsafe fn as_safe(&mut self) -> $name<'static> {
                $name {
                    $($varname: &mut *self.$varname,)*
                }
            }
        }
    };
}

type InterruptHeap = BinaryHeap<Interrupt>;

make_view! {
    pub struct CpuViewRaw;
    pub struct CpuView<'a> {
        pub vdp: &'a mut Vdp,
        pub z80: &'a mut Z80,
        pub psg: &'a mut Psg,
        pub controller_1: &'a mut Controller,
        pub controller_2: &'a mut Controller,
        pub rom: &'a mut [u8],
        pub cart_ram: &'a mut [u8],
        pub ram: &'a mut [u8],
        pub pending: &'a mut InterruptHeap,
    }
}

pub struct Z80View<'a> {
    pub cpu: &'a mut dyn Cpu,
    pub vdp: &'a mut Vdp,
    pub psg: &'a mut Psg,
    pub controller_1: &'a mut Controller,
    pub controller_2: &'a mut Controller,
    pub rom: &'a mut [u8],
    pub cart_ram: &'a mut [u8],
    pub ram: &'a mut [u8],
    pub pending: &'a mut BinaryHeap<Interrupt>,
}

impl AddressSpace for MemoryView<'_> {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        let rom_len = self.rom.len();

        let loc = match addr {
            0..=0x3F_FFFF if addr < rom_len => &self.rom[addr..],
            0..=0x3F_FFFF => &self.cart_ram[addr - rom_len..],
            0xA0_0000..=0xA0_FFFF => panic!("Attempt to read Z80 from memory view"),
            0xFF_0000..=0xFF_FFFF => &self.ram[addr - 0xFF_0000..],
            _ => {
                warn!("Unimplemented address {:#08X}, reading zero", addr);
                &[0, 0, 0, 0]
            }
        };

        match size {
            Size::Byte => u8::from_be_bytes([loc[0]]) as u32,
            Size::Word => u16::from_be_bytes([loc[0], loc[1]]) as u32,
            Size::Long => u32::from_be_bytes([loc[0], loc[1], loc[2], loc[3]]) as u32,
        }
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

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < self.rom.len() {
                info!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_2000).contains(&byte_addr) {
                panic!("Attempt to write to Z80 from `MemoryView`");
            } else {
                warn!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}


impl AddressSpace for Z80View<'_> {
    fn read(&mut self, address: u32, size: Size) -> u32 {
        let align = size.alignment();

        let addr = (address & 0xFF_FF_FF) as usize;

        assert_eq!(addr & (align - 1), 0, "Misaligned read: {:#X}", addr);

        match addr {
            0xA1_0000..=0xA1_0001 => {
                // TODO: Is this correct?
                return u32::from_be_bytes(*b"UEUE") & size.mask();
            }
            0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                return self.controller_1.read_reg1() as u32;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                return self.controller_2.read_reg1() as u32;
            }
            0xA1_1100..=0xA1_1101 => panic!("Z80 attempted to read itself"),
            0xC0_0000..=0xC0_000F => {
                return self.vdp.read(addr as u32) as u32;
            }
            0xFFFF_FFFC => {
                assert_eq!(size, Size::Long);
                info!("RETURNING 'init'");
                return u32::from_be_bytes(*b"init");
            }
            _ => {}
        }

        let rom_len = self.rom.len();

        let loc = match addr {
            0..=0x3F_FFFF if addr < rom_len => &self.rom[addr..],
            0..=0x3F_FFFF => &self.cart_ram[addr - rom_len..],
            0xA0_0000..=0xA0_FFFF => panic!("Z80 attempted to read itself"),
            0xFF_0000..=0xFF_FFFF => &self.ram[addr - 0xFF_0000..],
            _ => {
                warn!("Unimplemented address {:#08X}, reading zero", addr);
                &[0, 0, 0, 0]
            }
        };

        match size {
            Size::Byte => u8::from_be_bytes([loc[0]]) as u32,
            Size::Word => u16::from_be_bytes([loc[0], loc[1]]) as u32,
            Size::Long => u32::from_be_bytes([loc[0], loc[1], loc[2], loc[3]]) as u32,
        }
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

        match addr {
            0xA0_0000..=0xA0_FFFF => {
                panic!("Z80 attempted to write to itself");
            }
            0xA1_0002..=0xA1_0003 => {
                assert_eq!(size, Size::Byte);
                self.controller_1.write_reg1(value as u8);
                return;
            }
            0xA1_0004..=0xA1_0005 => {
                assert_eq!(size, Size::Byte);
                self.controller_2.write_reg1(value as u8);
                return;
            }
            0xA1_0008..=0xA1_0009 => {
                assert_eq!(size, Size::Byte);
                self.controller_1.write_reg2(value as u8);
                return;
            }
            0xA1_000A..=0xA1_000B => {
                assert_eq!(size, Size::Byte);
                self.controller_2.write_reg2(value as u8);
                return;
            }
            0xA1_0000..=0xA1_0002 | 0xA1_0005..=0xA1_000F => {
                // TODO:
                warn!("Ignoring write to some stuff: {:#08X}", addr);
                return;
            }
            0xA1_1100..=0xA1_1101 => {
                panic!("Z80 attempted to write to itself");
            }
            0xA1_1200 => {
                panic!("Z80 attempted to write to itself");
            }
            0xC0_0011 | 0xC0_0013 | 0xC0_0015 | 0xC0_0017 => {
                // TODO: is this correct?
                assert_eq!(size, Size::Byte);
                self.psg.write_io(value as u8);
                return;
            }
            0xA1_0020..=0xA1_10FF => {
                // Should we panic here, or not?
                warn!("Ignoring write to reserved memory");
                return;
            }
            0xA1_30F1..=0xA1_30F2 => {
                // TODO:
                warn!("Ignoring write of {:#X} to SRAM register", value);
                return;
            }
            0xC0_0000..=0xC0_000F => {
                // TODO: Is this even legal?
                todo!("Write to VDP")
            }
            _ => {}
        }

        let value = &value.to_be_bytes()[4 - length..4];

        for (offset, byte) in value.iter().enumerate() {
            let byte_addr = addr + offset;
            if byte_addr < self.rom.len() {
                info!("Ignoring write to ROM at {:#010X}", byte_addr);
            } else if byte_addr < 0x3F_FFFF {
                self.cart_ram[byte_addr - self.rom.len()] = *byte;
            } else if (0xFF_0000..=0xFF_FFFF).contains(&byte_addr) {
                self.ram[byte_addr - 0xFF_0000] = *byte;
            } else if (0xA0_0000..=0xA0_2000).contains(&byte_addr) {
                panic!("Z80 attempted to write to itself");
            } else {
                warn!("UNIMPLEMENTED Write to {:#010X}", byte_addr);
            };
        }
    }
}
