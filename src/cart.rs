use crate::get_four_bytes;
use std::convert::TryFrom;
use std::fmt;
use std::io::prelude::*;
use std::path::Path;

/// Represents a ROM file extracted from a cartridge
pub struct Cart {
    pub is_genesis: Option<bool>,
    pub domestic_name: String,
    pub overseas_name: String,
    pub rom_data: Box<[u8]>,
}

impl Cart {
    pub fn open(path: &Path) -> Result<Cart, Box<dyn std::error::Error>> {
        let mut file = std::fs::File::open(path)?;

        let mut data = Vec::new();
        file.read_to_end(&mut data)?;

        Ok(Cart::try_from(&data[..])?)
    }
}

#[derive(Debug)]
pub enum CartError {
    InvalidConsoleName(Vec<u8>),
    WrongSize(usize, usize),
    NonzeroRomStart,
}

impl fmt::Display for CartError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CartError::InvalidConsoleName(name) => {
                let name_string = String::from_utf8_lossy(name);
                write!(
                    f,
                    "Incorrect console name in header: {:X?}, '{}'",
                    name, name_string
                )
            }
            CartError::WrongSize(expected, actual) => write!(
                f,
                "Cart size {} did not match size in header {}",
                actual, expected
            ),
            CartError::NonzeroRomStart => write!(f, "Nonzero ROM start not supported yet"),
        }
    }
}

impl std::error::Error for CartError {}

impl TryFrom<&[u8]> for Cart {
    type Error = CartError;

    fn try_from(data: &[u8]) -> Result<Cart, Self::Error> {
        const MEGADRIVE_HEADER: &[u8] = b"SEGA MEGA DRIVE ";
        const GENESIS_HEADER: &[u8] = b"SEGA GENESIS    ";

        let name = &data[0x0100..0x0110];

        let is_genesis = if name == GENESIS_HEADER {
            Some(true)
        } else if name == MEGADRIVE_HEADER {
            Some(false)
        } else {
            None
            //return Err(CartError::InvalidConsoleName(name.to_vec()));
        };

        // TODO: Maybe record field 2 (copyright)?

        let domestic_name = String::from_utf8_lossy(&data[0x0120..0x0150]).to_string();
        let overseas_name = String::from_utf8_lossy(&data[0x0150..0x0180]).to_string();

        // TODO: Maybe record field 5 (product type)?

        // TODO: Maybe record field 6 (product name/version number)?

        // TODO: Maybe record field 7 (Checksum)?

        // TODO: Maybe record field 8 (IO Support)?

        let rom_start = u32::from_be_bytes(get_four_bytes(&data[0x01A0..0x01A4]));
        let rom_end = u32::from_be_bytes(get_four_bytes(&data[0x01A4..0x01A8]));

        if rom_start != 0 {
            return Err(CartError::NonzeroRomStart);
        }

        // There's some possibly unused data before this, what does it do?
        let _ram_start = u32::from_be_bytes(get_four_bytes(&data[0x01B4..0x01B8]));
        let _ram_end = u32::from_be_bytes(get_four_bytes(&data[0x01B8..0x01BC]));

        // TODO: Maybe record field 11 (Modem)?

        // Field 12 is unused, but maybe we should record it anyway so it's not lost?

        // TODO: Maybe record field 13 (Release countries)?

        if data.len() - 1 != (rom_end - rom_start) as usize {
            //return Err(CartError::WrongSize((rom_end - rom_start) as usize, data.len() - 1));
        }

        //let rom_data = Box::from(&data[rom_start as usize..=rom_end as usize]);

        let rom_data = Box::from(&data[..]);

        //let _checksum = &data[0x200..].iter().chunks(2).map(|s| (s[0] as usize) << 8 + s[1] as usize).sum();

        Ok(Cart {
            is_genesis,
            domestic_name,
            overseas_name,
            rom_data,
        })
    }
}
