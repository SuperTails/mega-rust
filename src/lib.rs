#[macro_use]
extern crate bitfield;

#[macro_use]
extern crate bitpat;

#[macro_use]
extern crate num_derive;

extern crate either;

extern crate num_traits;

extern crate sdl2;

pub mod cart;
mod cpu;
mod vdp;
mod sdl_system;

use cart::Cart;
use cpu::Cpu;
use vdp::Vdp;
use std::cell::RefCell;
use std::rc::Rc;
use sdl_system::SDLSystem;

fn get_four_bytes(data: &[u8]) -> [u8; 4] {
    let mut result = [0; 4];
    result.copy_from_slice(data);
    result
}

fn get_two_bytes(data: &[u8]) -> [u8; 2] {
    let mut result = [0; 2];
    result.copy_from_slice(data);
    result
}

struct System {
    cpu: Cpu,
    vdp: Vdp,
}

pub fn run(cart: Cart) {
    let mut vdp = Rc::new(RefCell::new(Vdp::new()));
    let mut cpu = Cpu::new(&cart.rom_data, Rc::downgrade(&vdp));
    let mut sdl_system = SDLSystem::new();

    loop {
        cpu.do_cycle();
        vdp.borrow_mut().do_cycle(&mut sdl_system);
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
