use crate::{bits::BitOps, memory::MutAccess, pin::Pin, rom::Mirroring};
use log::*;
use std::mem;

#[derive(Debug, Clone, Copy, PartialEq)]
enum Frame {
    Even,
    Odd,
}

impl Frame {
    fn next(self) -> Self {
        match self {
            Frame::Even => Frame::Odd,
            Frame::Odd => Frame::Even,
        }
    }
}

const VBLANK_START_SCANLINE: u16 = 241;
const PRE_RENDER_SCANLINE: u16 = 261;

const PALETTE_RAM_AT_BOOT: [u8; 0x20] = [
    0x09, 0x01, 0x00, 0x01, 0x00, 0x02, 0x02, 0x0D, 0x08, 0x10, 0x08, 0x24, 0x00,
    0x00, 0x04, 0x2C, 0x09, 0x01, 0x34, 0x03, 0x00, 0x04, 0x00, 0x14, 0x08, 0x3A,
    0x00, 0x02, 0x00, 0x20, 0x2C, 0x08,
];

#[allow(clippy::unreadable_literal)]
pub static COLORS: [u32; 64] = [
    0x666666, 0x002A88, 0x1412A7, 0x3B00A4, 0x5C007E, 0x6E0040, 0x6C0600,
    0x561D00, 0x333500, 0x0B4800, 0x005200, 0x004F08, 0x00404D, 0x000000,
    0x000000, 0x000000, 0xADADAD, 0x155FD9, 0x4240FF, 0x7527FE, 0xA01ACC,
    0xB71E7B, 0xB53120, 0x994E00, 0x6B6D00, 0x388700, 0x0C9300, 0x008F32,
    0x007C8D, 0x000000, 0x000000, 0x000000, 0xFFFEFF, 0x64B0FF, 0x9290FF,
    0xC676FF, 0xF36AFF, 0xFE6ECC, 0xFE8170, 0xEA9E22, 0xBCBE00, 0x88D800,
    0x5CE430, 0x45E082, 0x48CDDE, 0x4F4F4F, 0x000000, 0x000000, 0xFFFEFF,
    0xC0DFFF, 0xD3D2FF, 0xE8C8FF, 0xFBC2FF, 0xFEC4EA, 0xFECCC5, 0xF7D8A5,
    0xE4E594, 0xCFEF96, 0xBDF4AB, 0xB3F3CC, 0xB5EBF2, 0xB8B8B8, 0x000000,
    0x000000,
];

#[derive(Clone)]
pub struct Ppu {
    scanline: u16,
    cycle: u16,
    frame: Frame,

    /// $2000 w
    ///
    /// 7  bit  0
    /// ---- ----
    /// VPHB SINN
    /// |||| ||||
    /// |||| ||++- Base nametable address
    /// |||| ||    (0 = $2000; 1 = $2400; 2 = $2800; 3 = $2C00)
    /// |||| |+--- VRAM address increment per CPU read/write of PPUDATA
    /// |||| |     (0: add 1, going across; 1: add 32, going down)
    /// |||| +---- Sprite pattern table address for 8x8 sprites
    /// ||||       (0: $0000; 1: $1000; ignored in 8x16 mode)
    /// |||+------ Background pattern table address (0: $0000; 1: $1000)
    /// ||+------- Sprite size (0: 8x8; 1: 8x16)
    /// |+-------- PPU master/slave select
    /// |          (0: read backdrop from EXT pins; 1: output color on EXT pins)
    /// +--------- Generate an NMI at the start of the
    /// vertical blanking interval (0: off; 1: on)
    control: Control,

    /// $2001 w
    ///
    /// 7  bit  0
    /// ---- ----
    /// BGRs bMmG
    /// |||| ||||
    /// |||| |||+- Greyscale (0: normal color, 1: produce a greyscale display)
    /// |||| ||+-- 1: Show background in leftmost 8 pixels of screen, 0: Hide
    /// |||| |+--- 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
    /// |||| +---- 1: Show background
    /// |||+------ 1: Show sprites
    /// ||+------- Emphasize red*
    /// |+-------- Emphasize green*
    /// +--------- Emphasize blue*
    mask: Mask,

    /// $2002
    ///
    /// 7  bit  0
    /// ---- ----
    /// VSO. ....
    /// |||| ||||
    /// |||+-++++- Least significant bits previously written into a PPU register
    /// |||        (due to register not being updated for this address)
    /// ||+------- Sprite overflow. The intent was for this flag to be set
    /// ||         whenever more than eight sprites appear on a scanline, but a
    /// ||         hardware bug causes the actual behavior to be more complicated
    /// ||         and generate false positives as well as false negatives; see
    /// ||         PPU sprite evaluation. This flag is set during sprite
    /// ||         evaluation and cleared at dot 1 (the second dot) of the
    /// ||         pre-render line.
    /// |+-------- Sprite 0 Hit.  Set when a nonzero pixel of sprite 0 overlaps
    /// |          a nonzero background pixel; cleared at dot 1 of the pre-render
    /// |          line.  Used for raster timing.
    /// +--------- Vertical blank has started (0: not in vblank; 1: in vblank).
    ///            Set at dot 1 of line 241 (the line *after* the post-render
    ///            line); cleared after reading $2002 and at dot 1 of the
    ///            pre-render line.
    status: Status,

    oam_addr: u8,
    oam_data: [u8; 256],

    /// $2005 w x2
    scroll: u8,

    /// $2006 w x2
    /// yyy NN YYYYY XXXXX
    /// ||| || ||||| +++++-- coarse X scroll
    /// ||| || +++++-------- coarse Y scroll
    /// ||| ++-------------- nametable select
    /// +++----------------- fine Y scroll
    vram_addr: VRamAddr,
    temp_vram_addr: u16,
    vram: [u8; 0x800],
    temp_vram_read_buffer: u8,

    palette_ram: [u8; 0x20],

    open_bus: OpenBus,

    write_toggle: bool,
    nmi_pin: Pin,

    nametable_byte: u8,
    attribute_table_byte: u8,
    low_tile_byte: u8,
    high_tile_byte: u8,
    tile_data: u64,

    sprite_count: usize,
    sprite_patterns: [u32; 8],
    sprite_positions: [u8; 8],
    sprite_priorities: [u8; 8],
    sprite_indexes: [u8; 8],

    current_screen: [u8; 256 * 240],
    prev_screen: [u8; 256 * 240],

    mirroring: Mirroring,
}

impl Ppu {
    pub fn new(nmi_pin: Pin) -> Self {
        Self {
            status: Status::default(),
            control: Control::default(),
            mask: Mask::default(),
            cycle: 0,
            scanline: PRE_RENDER_SCANLINE,
            frame: Frame::Even,
            oam_addr: 0,
            oam_data: [0; 256],
            vram: [0; 0x800],
            palette_ram: PALETTE_RAM_AT_BOOT,
            scroll: 0,
            vram_addr: VRamAddr::default(),
            temp_vram_addr: 0,
            temp_vram_read_buffer: 0,
            open_bus: OpenBus::default(),
            write_toggle: false,
            nmi_pin,
            nametable_byte: 0,
            attribute_table_byte: 0,
            low_tile_byte: 0,
            high_tile_byte: 0,
            tile_data: 0,
            current_screen: [0; 256 * 240],
            prev_screen: [0; 256 * 240],
            mirroring: Mirroring::Vertical,
            sprite_count: 0,
            sprite_patterns: [0; 8],
            sprite_positions: [0; 8],
            sprite_priorities: [0; 8],
            sprite_indexes: [0; 8],
        }
    }

    pub fn set_mirroring(&mut self, mirroring: Mirroring) {
        self.mirroring = mirroring;
    }

    pub fn mut_read<M: MutAccess>(&mut self, addr: u16, mapper: &mut M) -> u8 {
        let val = match 0x2000 + (addr % 8) {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => self.open_bus.as_u8(),
            0x2002 => self.status(),
            0x2004 => {
                self.open_bus.refresh();
                self.read_from_oam()
            }
            0x2007 => {
                self.open_bus.refresh();
                self.mut_read_from_data(mapper)
            }
            _ => unreachable!(), // TODO(low): replace with std::hint::unreachable_unchecked.
        };

        self.open_bus.set(val);
        val
    }

    pub fn read(&self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => self.open_bus.as_u8(),
            0x2002 => self.status_read_only(),
            0x2004 => self.read_from_oam(),
            0x2007 => 0,         // TODO: unimplemented.
            _ => unreachable!(), // TODO(low): replace with std::hint::unreachable_unchecked.
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        self.open_bus.set(val);

        match 0x2000 + (addr % 8) {
            0x2000 => self.write_to_control(val),
            0x2001 => self.mask.set(val),
            0x2002 => (),
            0x2003 => self.oam_addr = val,
            0x2004 => self.write_to_oam(val),
            0x2005 => self.write_to_scroll(val),
            0x2006 => self.write_to_addr(val),
            0x2007 => self.write_to_data(val),
            _ => unreachable!(), // TODO(low): replace with std::hint::unreachable_unchecked.
        }

        self.open_bus.refresh();
    }

    pub fn screen(&self) -> &[u8] {
        &self.prev_screen
    }

    pub fn palette(&self) -> &[u8] {
        &self.palette_ram
    }

    pub fn step<M: MutAccess>(&mut self, mapper: &mut M) {
        trace!(
            "step cycle {:3}, scanline {:3}, frame {:4?}",
            self.cycle,
            self.scanline,
            self.frame
        );

        self.open_bus.step();

        match (self.scanline, self.cycle) {
            (VBLANK_START_SCANLINE, 1) => {
                trace!("vblank starts");

                mem::swap(&mut self.current_screen, &mut self.prev_screen);
                self.status.set_vblank();

                if self.control.nmi_at_next_vblank() {
                    self.nmi_pin.pull();
                }
            }

            (PRE_RENDER_SCANLINE, 1) => {
                trace!("vblank ends");
                self.status.clear_vblank();
                self.status.clear_sprite_overflow();
                self.status.clear_sprite_zero_hit();
            }
            _ => (),
        }

        if self.is_rendering_enabled() {
            match self.scanline {
                0..=239 => {
                    match self.cycle {
                        // Data for each tile is fetched.
                        1..=256 => {
                            self.draw_pixel();
                            self.load_tiles(mapper);

                            if self.cycle % 8 == 0 {
                                self.vram_addr.increment_x();
                            }

                            if self.cycle == 256 {
                                self.vram_addr.increment_y();
                            }
                        }

                        257 => {
                            self.vram_addr.copy_x(self.temp_vram_addr);

                            self.evaluate_sprites(mapper);
                        }

                        // First two tiles on next scanline.
                        321..=336 => {
                            self.load_tiles(mapper);

                            if self.cycle % 8 == 0 {
                                self.vram_addr.increment_x();
                            }
                        }

                        // Unused nametable fetches.
                        337 | 339 => {
                            let addr = self.vram_addr.nametable_addr();
                            let _ = self.mem_read(addr, mapper);
                        }
                        _ => {}
                    }

                    // TODO: sprite evaluation.
                }
                PRE_RENDER_SCANLINE => match self.cycle {
                    1..=256 => {
                        self.load_tiles(mapper);

                        if self.cycle % 8 == 0 {
                            self.vram_addr.increment_x();
                        }

                        if self.cycle == 256 {
                            self.vram_addr.increment_y();
                        }
                    }
                    257 => {
                        self.vram_addr.copy_x(self.temp_vram_addr);
                    }
                    280..=304 => {
                        self.vram_addr.copy_y(self.temp_vram_addr);
                    }
                    321..=336 => {
                        self.load_tiles(mapper);

                        if self.cycle % 8 == 0 {
                            self.vram_addr.increment_x();
                        }
                    }
                    _ => {}
                },
                _ => {} // TODO.
            }
        }

        match (self.frame, self.scanline, self.cycle) {
            // On odd frames, with background enabled, skip one cycle.
            (Frame::Odd, PRE_RENDER_SCANLINE, 339)
                if self.mask.show_background() =>
            {
                trace!("premature end of odd frame");
                self.cycle = 0;
                self.scanline = 0;
                self.frame = self.frame.next();
            }
            (_, PRE_RENDER_SCANLINE, 340) => {
                trace!("end of {:?} frame", self.frame);
                self.cycle = 0;
                self.scanline = 0;
                self.frame = self.frame.next();
            }
            (_, _, 340) => {
                trace!("end of scanline {}", self.scanline);
                self.cycle = 0;
                self.scanline += 1;
            }
            _ => {
                self.cycle += 1;
            }
        }
    }

    fn load_tiles<M: MutAccess>(&mut self, mapper: &mut M) {
        self.tile_data <<= 4;

        match self.cycle % 8 {
            0 => {
                let mut data: u32 = 0;
                for _ in 0..8 {
                    let p1 = (self.low_tile_byte & 0x80) >> 7;
                    let p2 = (self.high_tile_byte & 0x80) >> 6;
                    self.low_tile_byte <<= 1;
                    self.high_tile_byte <<= 1;
                    data <<= 4;
                    data |= u32::from(self.attribute_table_byte | p1 | p2);
                }

                self.tile_data |= u64::from(data);
            }
            1 => {
                let addr = self.vram_addr.nametable_addr();
                self.nametable_byte = self.mem_read(addr, mapper);
            }
            3 => {
                let addr = self.vram_addr.attribute_addr();
                let shift = self.vram_addr.shift();
                self.attribute_table_byte =
                    ((self.mem_read(addr, mapper) >> shift) & 3) << 2;
            }
            5 => {
                self.low_tile_byte = self.mem_read(self.low_tile_addr(), mapper);
            }
            7 => {
                self.high_tile_byte =
                    self.mem_read(self.high_tile_addr(), mapper);
            }
            _ => {}
        }
    }

    fn evaluate_sprites<M: MutAccess>(&mut self, mapper: &mut M) {
        self.sprite_count = 0;

        let height = self.control.sprite_height();

        for i in 0..64 {
            let offset = i * 4;
            let y = self.oam_data[offset];

            let (row, overflow) = self.scanline.overflowing_sub(u16::from(y));

            if overflow || row >= height {
                continue;
            }

            if self.sprite_count < 8 {
                let tile = self.oam_data[offset + 1];
                let attributes = self.oam_data[offset + 2];
                let x = self.oam_data[offset + 3];

                self.sprite_patterns[self.sprite_count] = self
                    .fetch_sprite_pattern(row, height, tile, attributes, mapper);
                self.sprite_positions[self.sprite_count] = x;
                self.sprite_priorities[self.sprite_count] = (attributes >> 5) & 1;
                self.sprite_indexes[self.sprite_count] = i as u8;
            }

            self.sprite_count += 1;
        }

        if self.sprite_count > 8 {
            self.sprite_count = 8;
            self.status.set_sprite_overflow();
        }
    }

    fn fetch_sprite_pattern<M: MutAccess>(
        &mut self,
        mut row: u16,
        height: u16,
        mut tile: u8,
        attributes: u8,
        mapper: &mut M,
    ) -> u32 {
        let addr = if height == 8 {
            if attributes.is_bit_set(7) {
                row = 7 - row;
            }

            self.control
                .sprite_pattern_table_addr()
                .wrapping_add(u16::from(tile) << 4)
                .wrapping_add(row)
        } else {
            if attributes.is_bit_set(7) {
                row = 15 - row;
            }

            let table: u16 = if tile.is_bit_set(0) { 0x1000 } else { 0x0000 };
            tile &= 0xFE;

            if row > 7 {
                tile += 1;
                row -= 8;
            }

            table.wrapping_add(u16::from(tile) << 4).wrapping_add(row)
        };

        let a = (attributes & 3) << 2;
        let mut low_tile = self.mem_read(addr, mapper);
        let mut high_tile = self.mem_read(addr.wrapping_add(8), mapper);

        let mut data = 0;
        for _ in 0..8 {
            let p1: u8;
            let p2: u8;

            if attributes & 0x40 == 0x40 {
                p1 = low_tile & 1;
                p2 = (high_tile & 1) << 1;
                low_tile >>= 1;
                high_tile >>= 1;
            } else {
                p1 = (low_tile & 0x80) >> 7;
                p2 = (high_tile & 0x80) >> 6;
                low_tile <<= 1;
                high_tile <<= 1;
            }

            data <<= 4;
            data |= u32::from(a | p1 | p2)
        }

        data
    }

    fn background_pixel(&self) -> u8 {
        if !self.mask.show_background() {
            return 0;
        }

        let data = self.tile_data >> 32 as u32;
        let shift = (7 - self.scroll) * 4;
        (data >> shift) as u8 & 0x0F
    }

    fn sprite_pixel(&self, x: u16) -> (usize, u8) {
        if !self.mask.show_sprites() {
            return (0, 0);
        }

        for i in 0..self.sprite_count {
            let (offset, overflowed) =
                x.overflowing_sub(u16::from(self.sprite_positions[i]));
            if overflowed || offset > 7 {
                continue;
            }

            let offset = 7u16.wrapping_sub(offset);
            let color =
                ((self.sprite_patterns[i] >> (offset << 2) as u8) & 0x0F) as u8;
            if color % 4 == 0 {
                continue;
            }

            return (i, color);
        }

        (0, 0)
    }

    fn draw_pixel(&mut self) {
        let x = self.cycle.wrapping_sub(1);
        let y = self.scanline;

        let mut bg = self.background_pixel();
        let (idx, mut sprite) = self.sprite_pixel(x);

        if x < 8 {
            if !self.mask.show_background_in_contour() {
                bg = 0;
            }

            if !self.mask.show_sprites_in_contour() {
                sprite = 0;
            }
        }

        let palette_idx = match (bg % 4 == 0, sprite % 4 == 0) {
            (true, true) => 0,
            (true, false) => sprite | 0x10,
            (false, true) => bg,
            (false, false) => {
                if self.sprite_indexes[idx] == 0 && x < 255 {
                    self.status.set_sprite_zero_hit();
                }

                if self.sprite_priorities[idx] == 0 {
                    sprite | 0x10
                } else {
                    bg
                }
            }
        };

        self.write_to_screen(y, x, self.read_palette(palette_idx as usize));
    }

    fn write_to_screen(&mut self, y: u16, x: u16, color: u8) {
        let idx = (y * 256) + x;
        self.current_screen[idx as usize] = color;
    }

    fn read_palette(&self, mut idx: usize) -> u8 {
        if idx >= 16 && idx % 4 == 0 {
            idx -= 16
        }

        self.palette_ram[idx]
    }

    // $2002
    // NOTE: keep in sync with `status_read_only`.
    fn status(&mut self) -> u8 {
        self.write_toggle = false;

        let mut ret = self.status.as_u8();
        self.status.clear_vblank();

        // tweaking affects test `ppu_vbl_nmi::suppression`.
        if let (VBLANK_START_SCANLINE, 2..=4) = (self.scanline, self.cycle) {
            self.nmi_pin.clear();

            // tweaking affects test `ppu_vbl_nmi::vbl_{set,clear}_time`.
            if self.cycle == 2 {
                ret.clear_bit(7);
            }
        }

        ret | (self.open_bus.as_u8() & 0b0001_1111)
    }

    // $2002
    // NOTE: keep in sync with `status`.
    fn status_read_only(&self) -> u8 {
        let mut ret = self.status.as_u8();

        if let (VBLANK_START_SCANLINE, 2) = (self.scanline, self.cycle) {
            ret.clear_bit(7);
        }

        ret | (self.open_bus.as_u8() & 0b0001_1111)
    }

    // $2000
    fn write_to_control(&mut self, val: u8) {
        // t: ...BA.. ........ = d: ......BA
        self.temp_vram_addr &= !0b000_1100_0000_0000;
        self.temp_vram_addr |= (u16::from(val) & 0b11) << 10;

        self.nmi_quirk_on_control_write(val);

        self.control.set(val);
    }

    fn nmi_quirk_on_control_write(&mut self, val: u8) {
        if val.is_bit_set(7) {
            // affects test `ppu_vbl_nmi::nmi_{control,on_timing}`.
            if !self.control.nmi_at_next_vblank() && self.status.is_vblank_set() {
                self.nmi_pin.pull_with_delay(1);
            }
        } else {
            // tweaking affects test `ppu_vbl_nmi::nmi_off_timing`.
            if let (VBLANK_START_SCANLINE, 2..=4) = (self.scanline, self.cycle) {
                self.nmi_pin.clear();
            }
        }
    }

    // $2005
    fn write_to_scroll(&mut self, val: u8) {
        if !self.write_toggle {
            // t: ... .... ...H GFED = d: HGFE D...
            // x:                CBA = d: .... .CBA
            self.temp_vram_addr &= !0b000_0000_0001_1111;
            self.temp_vram_addr |= (u16::from(val) & 0b1111_1000) >> 3;
            self.scroll = val & 0b111;
        } else {
            // t: CBA ..HG FED. .... = d: HGFE DCBA
            self.temp_vram_addr &= !0b111_0011_1110_0000;
            self.temp_vram_addr |= (u16::from(val) & 0b0000_0111) << 12;
            self.temp_vram_addr |= (u16::from(val) & 0b0011_1000) << 2;
            self.temp_vram_addr |= (u16::from(val) & 0b1100_0000) << 2;
        }

        self.write_toggle = !self.write_toggle;
    }

    // $2006
    fn write_to_addr(&mut self, val: u8) {
        if !self.write_toggle {
            // t: .FE DCBA .... .... = d: ..FE DCBA
            // t: X.. .... .... .... = 0
            self.temp_vram_addr &= !0b011_1111_0000_0000;
            self.temp_vram_addr |= (u16::from(val) & 0b11_1111) << 8;
            self.temp_vram_addr &= !0b100_0000_0000_0000;
        } else {
            // t: ... .... HGFE DCBA = d: HGFE DCBA
            // v                     = t
            self.temp_vram_addr &= !0b000_0000_1111_1111;
            self.temp_vram_addr |= u16::from(val);
            self.vram_addr.set(self.temp_vram_addr);
        }

        self.write_toggle = !self.write_toggle;
    }

    // $2004
    fn read_from_oam(&self) -> u8 {
        self.oam_data[self.oam_addr as usize]
    }

    // $2004
    fn write_to_oam(&mut self, mut val: u8) {
        match self.scanline {
            0..=239 | PRE_RENDER_SCANLINE if self.is_rendering_enabled() => {
                // TODO(low): glitchy OAM increment by bumping only the high 6 bits of OAMADDR.
            }
            _ => {
                if self.oam_addr & 0x03 == 0x02 {
                    val &= 0xE3;
                }

                self.oam_data[self.oam_addr as usize] = val;
                self.oam_addr = self.oam_addr.wrapping_add(1);
            }
        }
    }

    // $2007
    fn mut_read_from_data<M: MutAccess>(&mut self, mapper: &mut M) -> u8 {
        let addr = self.vram_addr.get();
        self.inc_vram_addr();

        let mut val = self.mem_read(addr, mapper);

        if addr <= 0x3EFF {
            mem::swap(&mut self.temp_vram_read_buffer, &mut val);
        }

        val
    }

    fn mem_read<M: MutAccess>(&self, addr: u16, mapper: &mut M) -> u8 {
        match addr {
            // Pattern Tables
            0x0000..=0x1FFF => mapper.mut_read(addr),

            // Nametables
            0x2000..=0x3EFF => self.vram[self.resolve_address(addr)],

            // Mirrored palette RAM indexes
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.mem_read(addr - 0x10, mapper)
            }

            // Palette RAM indexes
            0x3F00..=0x3F0F
            | 0x3F11..=0x3F13
            | 0x3F15..=0x3F17
            | 0x3F19..=0x3F1B
            | 0x3F1D..=0x3F1F => {
                self.palette_ram[(addr - 0x3F00) as usize]
                    | (self.open_bus.as_u8() & 0b1100_0000)
            }

            // Mirror of 0x3F00..=0x3F1F
            0x3F20..=0x3FFF => self.mem_read(0x3F00 + (addr % 0x20), mapper),

            _ => unimplemented!("reading from 0x4000"),
        }
    }

    fn resolve_address(&self, addr: u16) -> usize {
        use self::Mirroring::*;

        let addr = (addr as usize - 0x2000) % 0x1000;
        let table = addr / 0x0400;
        let offset = addr % 0x0400;

        let factor = match (self.mirroring, table) {
            (Horizontal, 0) => 0,
            (Horizontal, 1) => 0,
            (Horizontal, 2) => 1,
            (Horizontal, 3) => 1,
            (Vertical, 0) => 0,
            (Vertical, 1) => 1,
            (Vertical, 2) => 0,
            (Vertical, 3) => 1,
            _ => unreachable!(),
        };

        (0x2000 + factor * 0x040 + offset) % 0x800
    }

    fn write_to_data(&mut self, val: u8) {
        let addr = self.vram_addr.get();
        self.mem_write(addr, val);

        self.inc_vram_addr();
    }

    #[allow(unused)]
    fn debug_cycle(&self) {
        debug!("vram addr: {:04X}", self.vram_addr.get());
        debug!("nametable: {}", self.vram_addr.nametable_idx());
        debug!(
            "location: {}, {}",
            ((self.cycle - 1) / 8),
            (self.scanline / 8)
        );
        debug!("nametable addr: {:04X}", self.vram_addr.nametable_addr());
        debug!("nametable byte: {:02X}", self.nametable_byte);
        debug!("attribute addr: {:04X}", self.vram_addr.attribute_addr());
        debug!("attribute table byte: {:02X}", self.attribute_table_byte);
        debug!("low tile addr: {:04X}", self.low_tile_addr());
        debug!("high tile addr: {:04X}", self.high_tile_addr());
        debug!("tile data: {:0X}", self.tile_data);
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        match addr {
            // Patter= Table 0
            0x0000..=0x0FFF => (),

            // Pattern Table 1
            0x1000..=0x1FFF => (),

            // Nametables
            0x2000..=0x3EFF => self.vram[self.resolve_address(addr)] = val,

            // Mirrored palette RAM indexes
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.mem_write(addr - 0x10, val);
            }

            // Palette RAM indexes
            0x3F00..=0x3F0F
            | 0x3F11..=0x3F13
            | 0x3F15..=0x3F17
            | 0x3F19..=0x3F1B
            | 0x3F1D..=0x3F1F => {
                self.palette_ram[(addr - 0x3F00) as usize] = val;
            }

            // Mirror of 0x3F00..=0x3F1F
            0x3F20..=0x3FFF => {
                self.mem_write(0x3F00 + (addr % 0x20), val);
            }
            _ => (),
        }
    }

    fn inc_vram_addr(&mut self) {
        let by = self.control.vram_addr_incr();
        self.vram_addr.increment(by);
    }

    pub fn low_tile_addr(&self) -> u16 {
        self.control
            .background_pattern_table_addr()
            .wrapping_add(u16::from(self.nametable_byte).wrapping_mul(16))
            .wrapping_add(self.vram_addr.fine_y_scroll())
    }

    pub fn high_tile_addr(&self) -> u16 {
        self.low_tile_addr().wrapping_add(8)
    }

    fn is_rendering_enabled(&self) -> bool {
        self.mask.show_background() || self.mask.show_sprites()
    }
}

/// 010 00 00000 00000
/// yyy NN YYYYY XXXXX
/// ||| || ||||| +++++-- coarse X scroll
/// ||| || +++++-------- coarse Y scroll
/// ||| ++-------------- nametable select
/// +++----------------- fine Y scroll
#[derive(Debug, Clone, Copy, Default)]
struct VRamAddr(u16);

impl VRamAddr {
    pub fn get(self) -> u16 {
        self.0
    }

    pub fn nametable_idx(self) -> u8 {
        (self.0 >> 10) as u8 & 2
    }

    pub fn set(&mut self, val: u16) {
        trace!("vram set to {:04x}", val);

        self.0 = val;
    }

    pub fn fine_y_scroll(self) -> u16 {
        (self.0 >> 12) & 7
    }

    pub fn increment(&mut self, by: u16) {
        self.0 = self.0.wrapping_add(by);
    }

    pub fn increment_x(&mut self) {
        // increment hori(v)
        // if coarse X == 31
        if self.0 & 0x001F == 31 {
            // coarse X = 0
            self.0 &= 0xFFE0;

            // switch horizontal nametable;
            self.0 ^= 0x0400;
        } else {
            self.increment(1);
        }
    }

    pub fn increment_y(&mut self) {
        // increment vert(v)
        // if fine Y < 7
        if self.0 & 0x7000 != 0x7000 {
            // increment fine Y
            self.increment(0x1000);
        } else {
            // fine Y = 0
            self.0 &= 0x8FFF;

            // let y = coarse Y
            let mut y = (self.0 & 0x03E0) >> 5;
            if y == 29 {
                // coarse Y = 0
                y = 0;

                // switch vertical nametable
                self.0 ^= 0x0800;
            } else if y == 31 {
                // coarse Y = 0, nametable not switched
                y = 0
            } else {
                // increment coarse Y
                y = y.wrapping_add(1);
            }

            // put coarse Y back into v
            self.0 = (self.0 & 0xFC1F) | (y << 5);
        }
    }

    pub fn nametable_addr(self) -> u16 {
        // https://wiki.nesdev.com/w/index.php/PPU_scrolling#Tile_and_attribute_fetching
        0x2000 | (self.0 & 0x0FFF)
    }

    pub fn attribute_addr(self) -> u16 {
        // https://wiki.nesdev.com/w/index.php/PPU_scrolling#Tile_and_attribute_fetching
        0x23C0
            | (self.0 & 0x0C00)
            | ((self.0 >> 4) & 0x38)
            | ((self.0 >> 2) & 0x07)
    }

    pub fn shift(self) -> u16 {
        ((self.0 >> 4) & 4) | (self.0 & 2)
    }

    pub fn copy_x(&mut self, t: u16) {
        // v: .....F.. ...EDCBA = t: .....F.. ...EDCBA
        self.0 = (self.0 & 0xFBE0) | (t & 0x041F);
    }

    pub fn copy_y(&mut self, t: u16) {
        // v: IHG F.ED CBA. .... = t: IHG F.ED CBA. ....
        self.0 = (self.0 & 0x841F) | (t & 0x7BE0);
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Status(u8);

impl Status {
    pub fn as_u8(self) -> u8 {
        self.0
    }

    pub fn set_sprite_overflow(&mut self) {
        self.0.set_bit(5);
    }

    pub fn clear_sprite_overflow(&mut self) {
        self.0.clear_bit(5);
    }

    pub fn set_sprite_zero_hit(&mut self) {
        self.0.set_bit(6);
    }

    pub fn clear_sprite_zero_hit(&mut self) {
        self.0.clear_bit(6);
    }

    pub fn is_vblank_set(self) -> bool {
        self.0.get_bit(7) == 1
    }

    pub fn set_vblank(&mut self) {
        self.0.set_bit(7);
    }

    pub fn clear_vblank(&mut self) {
        self.0.clear_bit(7)
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Control(u8);

impl Control {
    pub fn set(&mut self, v: u8) {
        self.0 = v;
    }

    pub fn vram_addr_incr(self) -> u16 {
        if self.0.is_bit_set(2) {
            32
        } else {
            1
        }
    }

    #[allow(dead_code)]
    pub fn sprite_pattern_table_addr(self) -> u16 {
        if self.0.is_bit_set(3) {
            0x1000
        } else {
            0x0000
        }
    }

    pub fn background_pattern_table_addr(self) -> u16 {
        if self.0.is_bit_set(4) {
            0x1000
        } else {
            0x0000
        }
    }

    #[allow(dead_code)]
    pub fn sprite_height(self) -> u16 {
        if self.0.is_bit_set(5) {
            16
        } else {
            8
        }
    }

    pub fn nmi_at_next_vblank(self) -> bool {
        self.0.is_bit_set(7)
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Mask(u8);

impl Mask {
    pub fn set(&mut self, v: u8) {
        self.0 = v;
    }

    #[allow(dead_code)]
    pub fn grayscale(self) -> bool {
        self.0.is_bit_set(0)
    }

    pub fn show_background_in_contour(self) -> bool {
        self.0.is_bit_set(1)
    }

    #[allow(dead_code)]
    pub fn show_sprites_in_contour(self) -> bool {
        self.0.is_bit_set(2)
    }

    pub fn show_background(self) -> bool {
        self.0.is_bit_set(3)
    }

    pub fn show_sprites(self) -> bool {
        self.0.is_bit_set(4)
    }

    #[allow(dead_code)]
    pub fn emphasize_red(self) -> bool {
        self.0.is_bit_set(5)
    }

    #[allow(dead_code)]
    pub fn emphasize_green(self) -> bool {
        self.0.is_bit_set(6)
    }

    #[allow(dead_code)]
    pub fn emphasize_blue(self) -> bool {
        self.0.is_bit_set(7)
    }
}

#[derive(Clone, Copy, Default)]
struct OpenBus {
    val: u8,
    decay_cycles: usize,
}

// FIXME: this is probably not correct.
impl OpenBus {
    pub fn as_u8(&self) -> u8 {
        self.val
    }

    pub fn set(&mut self, val: u8) {
        self.val = val;
    }

    pub fn refresh(&mut self) {
        self.decay_cycles = 4_288_392; // TODO: empyrical constant, prove it.
    }

    pub fn step(&mut self) {
        self.decay_cycles = self.decay_cycles.saturating_sub(1);

        if self.decay_cycles == 0 {
            self.set(0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use std::iter;

    const CYCLES_ONE_SCANLINE: usize = 341;
    const CYCLES_TO_VBLANK_SCANLINE: usize = 341 * 242;
    const CYCLES_FULL_FRAME: usize = 341 * 262;

    mod control {
        use super::super::*;

        #[test]
        fn vram_addr_incr() {
            let mut control = Control::default();

            control.set(0b0000_0000);
            assert_eq!(1, control.vram_addr_incr());
            control.set(0b0000_0100);
            assert_eq!(32, control.vram_addr_incr());
        }

        #[test]
        fn sprite_pattern_table_addr() {
            let mut control = Control::default();
            control.set(0b0000_0000);
            assert_eq!(0x0000, control.sprite_pattern_table_addr());
            control.set(0b0000_1000);
            assert_eq!(0x1000, control.sprite_pattern_table_addr());
        }

        #[test]
        fn background_pattern_table_addr() {
            let mut control = Control::default();
            control.set(0b0000_0000);
            assert_eq!(0x0000, control.background_pattern_table_addr());
            control.set(0b0001_0000);
            assert_eq!(0x1000, control.background_pattern_table_addr());
        }

        #[test]
        fn sprite_height() {
            let mut control = Control::default();
            control.set(0b0000_0000);
            assert_eq!(8, control.sprite_height());
            control.set(0b0010_0000);
            assert_eq!(16, control.sprite_height());
        }

        #[test]
        fn nmi_at_next_vblank() {
            let mut control = Control::default();
            control.set(0b0000_0000);
            assert_eq!(false, control.nmi_at_next_vblank());
            control.set(0b1000_0000);
            assert_eq!(true, control.nmi_at_next_vblank());
        }
    }

    mod mask {
        use super::super::*;

        #[test]
        fn grayscale() {
            let mut mask = Mask::default();
            mask.set(0b0000_0000);
            assert_eq!(false, mask.grayscale());
            mask.set(0b0000_0001);
            assert_eq!(true, mask.grayscale());
        }
    }

    struct DummyMapper;

    impl MutAccess for DummyMapper {
        fn mut_read(&mut self, _addr: u16) -> u8 {
            0
        }

        fn write(&mut self, _addr: u16, _val: u8) {}
    }

    fn build_ppu() -> (Ppu, DummyMapper) {
        let ppu = Ppu::new(Pin::default());
        (ppu, DummyMapper)
    }

    #[test]
    fn open_bus() {
        let (mut ppu, mut mapper) = build_ppu();

        for offset in &[0u16, 1, 3, 4, 5, 6, 7] {
            let val = (*offset as u8) + 0xF0;
            ppu.write(0x2000 + offset, val);

            for offset2 in &[0u16, 1, 3, 5, 6] {
                let addr = 0x2000 + offset2;
                assert_eq!(val.to_bitstring(), ppu.read(addr).to_bitstring());
            }
        }

        ppu.write(0x2001, 0b0101_1010);
        ppu.status.set_vblank();
        assert_eq!("10011010", ppu.read(0x2002).to_bitstring());
        assert_eq!("10011010", ppu.mut_read(0x2002, &mut mapper).to_bitstring());
        assert_eq!("10011010", ppu.read(0x2000).to_bitstring());
    }

    #[test]
    fn vblank_start_with_nmi_at_next_vblank_pulls_nmi() {
        let (mut ppu, mut mapper) = build_ppu();
        assert!(!ppu.read(0x2002).is_bit_set(7));
        ppu.write(0x2000, 0b1000_0000);

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 2) {
            ppu.step(&mut mapper);
        }

        assert!(ppu.nmi_pin.is_pulled());
    }

    #[test]
    fn vblank_has_started_and_reading_status_resets_it() {
        let (mut ppu, mut mapper) = build_ppu();
        assert!(!ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 3) {
            ppu.step(&mut mapper);
        }

        assert!(ppu.mut_read(0x2002, &mut mapper).is_bit_set(7));
        assert!(!ppu.read(0x2002).is_bit_set(7));
    }

    #[test]
    fn vblank_has_ended() {
        let (mut ppu, mut mapper) = build_ppu();
        assert!(!ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 3) {
            ppu.step(&mut mapper);
        }

        assert!(ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(341 * 20) {
            ppu.step(&mut mapper);
        }

        assert!(!ppu.read(0x2002).is_bit_set(7));
    }

    #[test]
    fn writing_to_control_on_vblank_pulls_pin_with_one_instr_delay() {
        let (mut ppu, mut mapper) = build_ppu();

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 2) {
            ppu.step(&mut mapper);
        }

        ppu.write(0x2000, 0b1000_0000);

        ppu.nmi_pin.decr_delay();
        assert!(ppu.nmi_pin.is_pulled());
    }

    #[test]
    fn writing_to_control_within_vblank_clears_nmi_pin() {
        for cycle_delay in &[2, 3] {
            println!("cycle delay: {}", cycle_delay);

            let (mut ppu, mut mapper) = build_ppu();
            ppu.write(0x2000, 0b1000_0000);

            for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + cycle_delay) {
                ppu.step(&mut mapper);
            }
            assert!(ppu.nmi_pin.is_pulled());

            ppu.write(0x2000, 0);
            assert!(!ppu.nmi_pin.is_pulled());
        }
    }

    #[test]
    fn reading_status_exactly_on_vblank_cycle_returns_false() {
        let (mut ppu, mut mapper) = build_ppu();
        ppu.write(0x2000, 0b1000_0000);

        for _ in 0..=CYCLES_TO_VBLANK_SCANLINE {
            ppu.step(&mut mapper);
        }
        println!("({}, {})", ppu.scanline, ppu.cycle);

        assert!(!ppu.mut_read(0x2002, &mut mapper).is_bit_set(7));
    }

    #[test]
    fn odd_frames_are_shorter_by_one_cycle() {
        let (mut ppu, mut mapper) = build_ppu();
        ppu.write(0x2001, 0b0000_1000);
        assert!(ppu.mask.show_background());

        for _ in 0..CYCLES_ONE_SCANLINE {
            ppu.step(&mut mapper);
        }

        for (cycles, frame) in [CYCLES_FULL_FRAME - 1, CYCLES_FULL_FRAME]
            .iter()
            .cycle()
            .zip([Frame::Odd, Frame::Even].iter().cycle())
            .take(10)
        {
            assert_eq!(0, ppu.scanline);
            assert_eq!(0, ppu.cycle);
            assert_eq!(*frame, ppu.frame);

            for _ in 0..*cycles {
                ppu.step(&mut mapper);
            }
        }
    }

    #[test]
    fn all_frames_are_equal_when_bg_is_disabled() {
        let (mut ppu, mut mapper) = build_ppu();
        assert!(!ppu.mask.show_background());

        for _ in 0..CYCLES_ONE_SCANLINE {
            ppu.step(&mut mapper);
        }

        for (cycles, frame) in iter::repeat(CYCLES_FULL_FRAME)
            .zip([Frame::Odd, Frame::Even].iter().cycle())
            .take(10)
        {
            assert_eq!(0, ppu.scanline);
            assert_eq!(0, ppu.cycle);
            assert_eq!(*frame, ppu.frame);

            for _ in 0..cycles {
                ppu.step(&mut mapper);
            }
        }
    }

    /// https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Register_controls
    #[test]
    fn scroll_and_addr_write_example() {
        let (mut ppu, mut mapper) = build_ppu();

        // $2000 write
        ppu.write(0x2000, 0b0000_0011);
        assert_eq!(0b000_1100_0000_0000, ppu.temp_vram_addr);

        // $2002 read
        ppu.mut_read(0x2002, &mut mapper);
        assert_eq!(false, ppu.write_toggle);

        // $2005 first write (w is 0)
        ppu.write(0x2005, 0b1010_1101);
        assert_eq!(0b000_1100_0001_0101, ppu.temp_vram_addr);
        assert_eq!(0b101, ppu.scroll);
        assert_eq!(true, ppu.write_toggle);

        // $2005 second write (w is 1)j
        ppu.write(0x2005, 0b1110_1101);
        assert_eq!(0b101_1111_1011_0101, ppu.temp_vram_addr);
        assert_eq!(false, ppu.write_toggle);

        // $2006 first write (w is 0)
        ppu.write(0x2006, 0b0010_1010);
        assert_eq!(0b010_1010_1011_0101, ppu.temp_vram_addr);
        assert_eq!(true, ppu.write_toggle);

        // $2006 second write (w is 1)
        ppu.write(0x2006, 0b0100_1010);
        assert_eq!(0b010_1010_0100_1010, ppu.temp_vram_addr);
        assert_eq!(ppu.vram_addr.get(), ppu.temp_vram_addr);
        assert_eq!(false, ppu.write_toggle);
    }

    #[test]
    fn write_to_data_increments_vram_addr() {
        let (mut ppu, _) = build_ppu();

        assert_eq!(0, ppu.vram_addr.get());
        assert_eq!(1, ppu.control.vram_addr_incr());

        ppu.write(0x2007, 0xFF);
        assert_eq!(1, ppu.vram_addr.get());

        ppu.write(0x2000, 0b0000_0100);
        assert_eq!(32, ppu.control.vram_addr_incr());

        ppu.write(0x2007, 0xFF);
        assert_eq!(33, ppu.vram_addr.get());
    }

    #[test]
    fn read_from_data_increments_vram_addr() {
        let (mut ppu, mut mapper) = build_ppu();

        assert_eq!(0, ppu.vram_addr.get());
        assert_eq!(1, ppu.control.vram_addr_incr());

        ppu.mut_read(0x2007, &mut mapper);
        assert_eq!(1, ppu.vram_addr.get());

        ppu.write(0x2000, 0b0000_0100);
        assert_eq!(32, ppu.control.vram_addr_incr());

        ppu.mut_read(0x2007, &mut mapper);
        assert_eq!(33, ppu.vram_addr.get());
    }

    #[test]
    fn read_from_data_not_in_palettes_address_range_is_delayed_by_one_read() {
        // TODO: also test pattern tables address space.
        let (mut ppu, mut mapper) = build_ppu();

        assert_eq!(1, ppu.control.vram_addr_incr());
        ppu.vram_addr.set(0x2000);
        ppu.write(0x2007, 0xFF);
        ppu.write(0x2007, 0xA0);

        ppu.vram_addr.set(0x2000);
        assert_eq!(0x00, ppu.mut_read(0x2007, &mut mapper));
        assert_eq!(0xFF, ppu.mut_read(0x2007, &mut mapper));
        assert_eq!(0xA0, ppu.mut_read(0x2007, &mut mapper));
    }

    #[test]
    fn read_immediately_from_data_in_palettes_address_range() {
        let (mut ppu, mut mapper) = build_ppu();

        assert_eq!(1, ppu.control.vram_addr_incr());
        ppu.vram_addr.set(0x3F00);
        ppu.write(0x2007, 0xFF);
        ppu.write(0x2007, 0xA0);

        let open_bus_filter_mask = 0b0011_1111;

        ppu.vram_addr.set(0x3F00);
        assert_eq!(
            open_bus_filter_mask,
            ppu.mut_read(0x2007, &mut mapper) & open_bus_filter_mask
        );
        assert_eq!(
            0xA0 & open_bus_filter_mask,
            ppu.mut_read(0x2007, &mut mapper) & open_bus_filter_mask
        );
    }

    #[test]
    fn write_and_read_oam() {
        let (mut ppu, _) = build_ppu();

        ppu.write(0x2003, 0x00);
        ppu.write(0x2004, 0xFF);
        ppu.write(0x2004, 0xFE);
        ppu.write(0x2004, 0xFD);
        ppu.write(0x2004, 0xFC);

        ppu.write(0x2003, 0x00);
        assert_eq!(0xFF, ppu.read(0x2004));

        ppu.write(0x2003, 0x01);
        assert_eq!(0xFE, ppu.read(0x2004));

        ppu.write(0x2003, 0x02);
        assert_eq!(0xFD & 0xE3, ppu.read(0x2004));

        ppu.write(0x2003, 0x03);
        assert_eq!(0xFC, ppu.read(0x2004));
    }
}
