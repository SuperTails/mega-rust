use super::VdpInner;

pub trait Plane: Sized + Copy {
    fn nametable(self) -> u16;

    fn horizontal_scroll(self, row: usize, vdp: &VdpInner) -> u16;

    fn vertical_scroll(self, col: usize, vdp: &VdpInner) -> u16;

    fn pixel_color(self, r: u16, c: u16, vdp: &VdpInner) -> Option<(u8, u8, u8)> {
        let plane_cell_width = vdp.plane_width as u16 / 8;

        let row = r.wrapping_add(self.vertical_scroll(c as usize, vdp)) & vdp.plane_height.mask();
        let col = c.wrapping_sub(self.horizontal_scroll(r as usize, vdp)) & vdp.plane_width.mask();

        let cell_row = row / 8;
        let cell_col = col / 8;

        let cell = cell_row * plane_cell_width + cell_col;

        let mut pixel_row = row % 8;
        let mut pixel_col = col % 8;

        let addr = self.nametable().wrapping_add(cell * 2);

        let tile = vdp.get_tile_entry(addr);

        if tile.horiz_flip() {
            pixel_col = 7 - pixel_col;
        }

        if tile.vert_flip() {
            pixel_row = 7 - pixel_row;
        }

        vdp.get_pattern_color(addr, pixel_row, pixel_col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct WindowPlane {
    pub nametable: u16,
    pub horizontal_scroll: u16,
    pub vertical_scroll: u16,
}

impl Plane for WindowPlane {
    fn nametable(self) -> u16 {
        self.nametable
    }

    fn horizontal_scroll(self, _: usize, _: &VdpInner) -> u16 {
        self.horizontal_scroll
    }

    fn vertical_scroll(self, _: usize, _: &VdpInner) -> u16 {
        self.vertical_scroll
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NormalPlane {
    pub nametable: u16,
    is_a: bool,
}

impl NormalPlane {
    pub fn new_b() -> NormalPlane {
        NormalPlane {
            nametable: 0,
            is_a: false,
        }
    }

    pub fn new_a() -> NormalPlane {
        NormalPlane {
            nametable: 0,
            is_a: true,
        }
    }
}

impl Plane for NormalPlane {
    fn nametable(self) -> u16 {
        self.nametable
    }

    fn horizontal_scroll(self, row: usize, vdp: &VdpInner) -> u16 {
        vdp.get_horiz_scroll(row, self.is_a)
    }

    fn vertical_scroll(self, col: usize, vdp: &VdpInner) -> u16 {
        vdp.get_vert_scroll(col, self.is_a)
    }
}
