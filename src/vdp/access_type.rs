use super::CRamEntry;
use super::RamType;
use log::info;

#[derive(Debug)]
pub struct AccessType {
    pub address: u16,
    pub write: bool,
    pub ram_type: RamType,
}

impl AccessType {
    pub fn new() -> AccessType {
        AccessType {
            address: 0,
            write: false,
            ram_type: RamType::VRam,
        }
    }

    pub(super) fn write(
        &mut self,
        value: u16,
        vram: &mut [u8],
        cram: &mut [CRamEntry],
        vsram: &mut [u16],
        increment: u16,
    ) {
        if self.write {
            match self.ram_type {
                RamType::VRam => {
                    vram[self.address as usize..][0..2].copy_from_slice(&value.to_be_bytes())
                }
                RamType::CRam => cram[(self.address as usize / 2) % cram.len()].0 = value,
                RamType::VSRam => {
                    let address = (self.address & 0x4F) / 2;
                    let value = value & 0x3FF;

                    vsram[address as usize] = value;
                }
            }
        } else {
            info!("Ignoring write of {:#X} to data register", value);
        }

        self.address = self.address.wrapping_add(increment);
    }

    pub(super) fn read(
        &mut self,
        vram: &[u8],
        cram: &[CRamEntry],
        vsram: &[u16],
        increment: u16,
    ) -> u16 {
        let result = if !self.write {
            match self.ram_type {
                RamType::VRam => {
                    ((vram[self.address as usize] as u16) << 8)
                        | vram[self.address as usize + 1] as u16
                }
                RamType::CRam => cram[(self.address as usize / 2) % cram.len()].0,
                RamType::VSRam => {
                    let address = (self.address & 0x4F) / 2;
                    vsram[address as usize] & 0x3FF
                }
            }
        } else {
            panic!("Attempt to read from VDP when in write mode")
        };

        self.address = self.address.wrapping_add(increment);

        result
    }
}
