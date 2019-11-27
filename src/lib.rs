#[macro_use]
extern crate bitfield;

#[macro_use]
extern crate bitpat;

#[macro_use]
extern crate num_derive;

extern crate either;

extern crate num_traits;

pub mod cart;
mod cpu;

use cart::Cart;
use cpu::Cpu;

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

pub fn run(cart: Cart) {
    let mut cpu = Cpu::new(&cart.rom_data);

    loop {
        cpu.do_cycle();
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
