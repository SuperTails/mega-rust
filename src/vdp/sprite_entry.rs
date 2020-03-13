#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SpriteEntry {
    pub pattern_start_index: u16,
    pub x: u16,
    pub y: u16,
    /// Width of the sprite, measured in cells
    pub width: u8,
    /// Height of the sprite, measured in cells
    pub height: u8,
    /// Index in the sprite attribute table of the next sprite, or zero if none
    pub link: u8,
    pub palette_line: u8,
    pub horizontal_flip: bool,
    pub vertical_flip: bool,
    pub priority: bool,
}

impl From<&[u8]> for SpriteEntry {
    fn from(data: &[u8]) -> SpriteEntry {
        assert_eq!(data.len(), 8);

        let mut d = [0; 8];
        d.copy_from_slice(data);
        SpriteEntry::from_array(d)
    }
}

impl SpriteEntry {
    fn from_array(data: [u8; 8]) -> SpriteEntry {
        let y = (((data[0] as u16) << 8) | data[1] as u16) & 0x3FF;
        let height = 1 + (data[2] & 0x3);
        let width = 1 + ((data[2] >> 2) & 0x3);
        let link = data[3] & 0x7F;
        let pattern_start_index = (((data[4] as u16) << 8) | data[5] as u16) & 0x7FF;
        let horizontal_flip = data[4] & (1 << 3) != 0;
        let vertical_flip = data[4] & (1 << 4) != 0;
        let palette_line = (data[4] >> 5) & 0x3;
        let priority = data[4] & (1 << 7) != 0;
        let x = (((data[6] as u16) << 8) | data[7] as u16) & 0x3FF;
        SpriteEntry {
            y,
            height,
            width,
            link,
            pattern_start_index,
            horizontal_flip,
            vertical_flip,
            palette_line,
            priority,
            x,
        }
    }
}
