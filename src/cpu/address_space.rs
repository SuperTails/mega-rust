use super::Size;

pub trait AddressSpace {
    fn read(&mut self, address: u32, size: Size) -> u32;

    fn write(&mut self, address: u32, value: u32, size: Size);
}
