use self::{MasterSlaveSelect::*, SpriteSize::*, VRamAddrIncr::*};
use crate::{bits::BitOps, pin::Pin};
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

    /// $2003 w
    oam_addr: u8,

    /// $2004 r/w
    /// AKA sprite table
    oam_ram: [u8; 256],

    /// $2005 w x2
    scroll: u8,

    /// $2006 w x2
    /// yyy NN YYYYY XXXXX
    /// ||| || ||||| +++++-- coarse X scroll
    /// ||| || +++++-------- coarse Y scroll
    /// ||| ++-------------- nametable select
    /// +++----------------- fine Y scroll
    vram_addr: u16,
    temp_vram_addr: u16,
    vram: [u8; 0x800],
    temp_vram_read_buffer: u8,

    palette_ram: [u8; 0xFF],

    open_bus: OpenBus,

    write_toggle: bool,
    nmi_pin: Pin,
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
            oam_ram: [0; 256],
            vram: [0; 0x800],
            palette_ram: [0; 0xFF],
            scroll: 0,
            vram_addr: 0,
            temp_vram_addr: 0,
            temp_vram_read_buffer: 0,
            open_bus: OpenBus::default(),
            write_toggle: false,
            nmi_pin,
        }
    }

    #[cfg(test)]
    pub fn with_detached_pin() -> Self {
        Self::new(Pin::default())
    }

    pub fn mut_read(&mut self, addr: u16) -> u8 {
        let val = match 0x2000 + (addr % 8) {
            0x2000 | 0x2001 | 0x2003 | 0x2005 | 0x2006 => self.open_bus.as_u8(),
            0x2002 => self.status(),
            0x2004 => {
                self.open_bus.refresh();
                self.read_from_oam()
            }
            0x2007 => {
                self.open_bus.refresh();
                self.mut_read_from_data()
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

    pub fn step(&mut self) {
        trace!(
            "step cycle {:3}, scanline {:3}, frame {:4?}",
            self.cycle,
            self.scanline,
            self.frame
        );

        self.open_bus.step();

        match (self.scanline, self.cycle) {
            (VBLANK_START_SCANLINE, 1) => {
                debug!("vblank starts");
                self.status.set_vblank();

                if self.control.nmi_at_next_vblank() {
                    self.nmi_pin.pull();
                }
            }

            (PRE_RENDER_SCANLINE, 1) => {
                debug!("vblank ends");
                self.status.clear_vblank();
            }
            _ => (),
        }

        match (self.frame, self.scanline, self.cycle) {
            // On odd frames, with background enabled, skip one cycle.
            (Frame::Odd, PRE_RENDER_SCANLINE, 339)
                if self.mask.show_background() =>
            {
                debug!("premature end of odd frame");
                self.cycle = 0;
                self.scanline = 0;
                self.frame = self.frame.next();
            }
            (_, PRE_RENDER_SCANLINE, 340) => {
                debug!("end of {:?} frame", self.frame);
                self.cycle = 0;
                self.scanline = 0;
                self.frame = self.frame.next();
            }
            (_, _, 340) => {
                debug!("end of scanline {}", self.scanline);
                self.cycle = 0;
                self.scanline += 1;
            }
            _ => {
                self.cycle += 1;
            }
        }
    }

    // $2002
    // NOTE: keep in sync with `status_read_only`.
    fn status(&mut self) -> u8 {
        self.write_toggle = false;

        let mut ret = self.status.as_u8();
        self.status.clear_vblank();

        // tweaking affects test `ppu_vbl_nmi::suppression`.
        if let (VBLANK_START_SCANLINE, 2...4) = (self.scanline, self.cycle) {
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
            if let (VBLANK_START_SCANLINE, 2...4) = (self.scanline, self.cycle) {
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
            self.vram_addr = self.temp_vram_addr;
        }

        self.write_toggle = !self.write_toggle;
    }

    // $2004
    fn read_from_oam(&self) -> u8 {
        self.oam_ram[self.oam_addr as usize]
    }

    // $2004
    fn write_to_oam(&mut self, mut val: u8) {
        match self.scanline {
            0...239 | PRE_RENDER_SCANLINE if self.is_rendering_enabled() => {
                // TODO(low): glitchy OAM increment by bumping only the high 6 bits of OAMADDR.
            }
            _ => {
                // The three unimplemented bits of each sprite's byte 2 do not exist
                // in the PPU and always read back as 0 on PPU revisions that allow
                // reading PPU OAM through OAMDATA ($2004).
                if self.oam_addr & 0x03 == 0x02 {
                    val &= 0xE3;
                }
                self.oam_ram[self.oam_addr as usize] = val;
                self.oam_addr = self.oam_addr.wrapping_add(1);
            }
        }
    }

    // $2007
    fn mut_read_from_data(&mut self) -> u8 {
        let addr = self.vram_addr;
        let mut val = self.mem_read(addr);

        if addr <= 0x3EFF {
            mem::swap(&mut self.temp_vram_read_buffer, &mut val);
        }

        self.inc_vram_addr();
        val
    }

    fn mem_read(&self, addr: u16) -> u8 {
        match addr {
            // Pattern Table 0
            0x0000...0x0FFF => 0,

            // Pattern Table 1
            0x1000...0x1FFF => 0,

            // Nametable 0
            0x2000...0x23FF => self.vram[(addr - 0x2000) as usize],

            // Nametable 1
            0x2400...0x27FF => self.vram[(addr - 0x2400) as usize],

            // Nametable 2
            0x2800...0x2BFF => self.vram[(addr - 0x2400) as usize],

            // Nametable 3
            0x2C00...0x2FFF => self.vram[(addr - 0x2800) as usize],

            // Mirror of 0x2000...0x2EFF
            0x3000...0x3EFF => self.mem_read(0x2000 + (addr % 0xEF00)),

            // Mirrored palette RAM indexes
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => self.mem_read(addr - 0x10),

            // Palette RAM indexes
            0x3F00...0x3F0F
            | 0x3F11...0x3F13
            | 0x3F15...0x3F17
            | 0x3F19...0x3F1B
            | 0x3F1D...0x3F1F => {
                self.palette_ram[(addr - 0x3F00) as usize]
                    | (self.open_bus.as_u8() & 0b1100_0000)
            }

            // Mirror of 0x3F00...0x3F1F
            0x3F20...0x3FFF => self.mem_read(0x3F00 + (addr % 0x20)),

            _ => 0,
        }
    }

    fn write_to_data(&mut self, val: u8) {
        let addr = self.vram_addr;
        self.mem_write(addr, val);

        self.inc_vram_addr();
    }

    fn mem_write(&mut self, addr: u16, val: u8) {
        match addr {
            // Pattern Table 0
            0x0000...0x0FFF => (),

            // Pattern Table 1
            0x1000...0x1FFF => (),

            // Nametable 0
            0x2000...0x23FF => self.vram[(addr - 0x2000) as usize] = val,

            // Nametable 1
            0x2400...0x27FF => self.vram[(addr - 0x2400) as usize] = val,

            // Nametable 2
            0x2800...0x2BFF => self.vram[(addr - 0x2400) as usize] = val,

            // Nametable 3
            0x2C00...0x2FFF => self.vram[(addr - 0x2800) as usize] = val,

            // Mirror of 0x2000...0x2EFF
            0x3000...0x3EFF => {
                self.mem_write(0x2000 + (addr % 0xEF00), val);
            }

            // Mirrored palette RAM indexes
            0x3F10 | 0x3F14 | 0x3F18 | 0x3F1C => {
                self.mem_write(addr - 0x10, val);
            }

            // Palette RAM indexes
            0x3F00...0x3F0F
            | 0x3F11...0x3F13
            | 0x3F15...0x3F17
            | 0x3F19...0x3F1B
            | 0x3F1D...0x3F1F => {
                self.palette_ram[(addr - 0x3F00) as usize] = val;
            }

            // Mirror of 0x3F00...0x3F1F
            0x3F20...0x3FFF => {
                self.mem_write(0x3F00 + (addr % 0x20), val);
            }
            _ => (),
        }
    }

    fn inc_vram_addr(&mut self) {
        let incr = match self.control.vram_addr_incr() {
            VRamAddrIncr::Add1GoingAcross => 1,
            VRamAddrIncr::Add32GoingDown => 32,
        };
        self.vram_addr = self.vram_addr.wrapping_add(incr);
    }

    fn is_rendering_enabled(&self) -> bool {
        self.mask.show_background() || self.mask.show_sprites()
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Status(u8);

impl Status {
    pub fn as_u8(self) -> u8 {
        self.0
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

#[allow(dead_code)]
impl Control {
    pub fn set(&mut self, v: u8) {
        self.0 = v;
    }

    pub fn base_nametable_addr(self) -> u16 {
        match self.0 & 3 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => unreachable!(),
        }
    }

    pub fn vram_addr_incr(self) -> VRamAddrIncr {
        if self.0.is_bit_set(2) {
            Add32GoingDown
        } else {
            Add1GoingAcross
        }
    }

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

    pub fn sprite_size(self) -> SpriteSize {
        if self.0.is_bit_set(5) {
            EightBySixteen
        } else {
            EightByEight
        }
    }

    pub fn master_slave_select(self) -> MasterSlaveSelect {
        if self.0.is_bit_set(6) {
            OutputColorOnExt
        } else {
            ReadBackdropFromExt
        }
    }

    pub fn nmi_at_next_vblank(self) -> bool {
        self.0.is_bit_set(7)
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, Default)]
struct Mask(u8);

#[allow(dead_code)]
impl Mask {
    pub fn set(&mut self, v: u8) {
        self.0 = v;
    }

    pub fn grayscale(self) -> bool {
        self.0.is_bit_set(0)
    }

    pub fn show_background_in_contour(self) -> bool {
        self.0.is_bit_set(1)
    }

    pub fn show_sprites_in_contour(self) -> bool {
        self.0.is_bit_set(2)
    }

    pub fn show_background(self) -> bool {
        self.0.is_bit_set(3)
    }

    pub fn show_sprites(self) -> bool {
        self.0.is_bit_set(4)
    }

    pub fn emphasize_red(self) -> bool {
        self.0.is_bit_set(5)
    }

    pub fn emphasize_green(self) -> bool {
        self.0.is_bit_set(6)
    }

    pub fn emphasize_blue(self) -> bool {
        self.0.is_bit_set(7)
    }
}

#[derive(Debug, PartialEq)]
pub enum VRamAddrIncr {
    Add1GoingAcross,
    Add32GoingDown,
}

#[derive(Debug, PartialEq)]
pub enum SpriteSize {
    EightByEight,
    EightBySixteen,
}

#[derive(Debug, PartialEq)]
pub enum MasterSlaveSelect {
    ReadBackdropFromExt,
    OutputColorOnExt,
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
        fn name_table_addr() {
            let mut control = Control::default();

            control.set(0b00000000);
            assert_eq!(0x2000, control.base_nametable_addr());
            control.set(0b00000001);
            assert_eq!(0x2400, control.base_nametable_addr());
            control.set(0b00000010);
            assert_eq!(0x2800, control.base_nametable_addr());
            control.set(0b00000011);
            assert_eq!(0x2C00, control.base_nametable_addr());
        }

        #[test]
        fn vram_addr_incr() {
            let mut control = Control::default();

            control.set(0b00000000);
            assert_eq!(Add1GoingAcross, control.vram_addr_incr());
            control.set(0b00000100);
            assert_eq!(Add32GoingDown, control.vram_addr_incr());
        }

        #[test]
        fn sprite_pattern_table_addr() {
            let mut control = Control::default();
            control.set(0b00000000);
            assert_eq!(0x0000, control.sprite_pattern_table_addr());
            control.set(0b00001000);
            assert_eq!(0x1000, control.sprite_pattern_table_addr());
        }

        #[test]
        fn background_pattern_table_addr() {
            let mut control = Control::default();
            control.set(0b00000000);
            assert_eq!(0x0000, control.background_pattern_table_addr());
            control.set(0b00010000);
            assert_eq!(0x1000, control.background_pattern_table_addr());
        }

        #[test]
        fn sprite_size() {
            let mut control = Control::default();
            control.set(0b00000000);
            assert_eq!(EightByEight, control.sprite_size());
            control.set(0b00100000);
            assert_eq!(EightBySixteen, control.sprite_size());
        }

        #[test]
        fn master_slave_select() {
            let mut control = Control::default();
            control.set(0b00000000);
            assert_eq!(ReadBackdropFromExt, control.master_slave_select());
            control.set(0b01000000);
            assert_eq!(OutputColorOnExt, control.master_slave_select());
        }

        #[test]
        fn nmi_at_next_vblank() {
            let mut control = Control::default();
            control.set(0b00000000);
            assert_eq!(false, control.nmi_at_next_vblank());
            control.set(0b10000000);
            assert_eq!(true, control.nmi_at_next_vblank());
        }
    }

    mod mask {
        use super::super::*;

        #[test]
        fn grayscale() {
            let mut mask = Mask::default();
            mask.set(0b00000000);
            assert_eq!(false, mask.grayscale());
            mask.set(0b00000001);
            assert_eq!(true, mask.grayscale());
        }
    }

    #[test]
    fn open_bus() {
        let mut ppu = Ppu::with_detached_pin();

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
        assert_eq!("10011010", ppu.mut_read(0x2002).to_bitstring());
        assert_eq!("10011010", ppu.read(0x2000).to_bitstring());
    }

    #[test]
    fn vblank_start_with_nmi_at_next_vblank_pulls_nmi() {
        let mut ppu = Ppu::with_detached_pin();
        assert!(!ppu.read(0x2002).is_bit_set(7));
        ppu.write(0x2000, 0b1000_0000);

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 2) {
            ppu.step();
        }

        assert!(ppu.nmi_pin.is_pulled());
    }

    #[test]
    fn vblank_has_started_and_reading_status_resets_it() {
        let mut ppu = Ppu::with_detached_pin();
        assert!(!ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 3) {
            ppu.step();
        }

        assert!(ppu.mut_read(0x2002).is_bit_set(7));
        assert!(!ppu.read(0x2002).is_bit_set(7));
    }

    #[test]
    fn vblank_has_ended() {
        let mut ppu = Ppu::with_detached_pin();
        assert!(!ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 3) {
            ppu.step();
        }

        assert!(ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(341 * 20) {
            ppu.step();
        }

        assert!(!ppu.read(0x2002).is_bit_set(7));
    }

    #[test]
    fn writing_to_control_on_vblank_pulls_pin_with_one_instr_delay() {
        let mut ppu = Ppu::with_detached_pin();

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 2) {
            ppu.step();
        }

        ppu.write(0x2000, 0b1000_0000);

        ppu.nmi_pin.decr_delay();
        assert!(ppu.nmi_pin.is_pulled());
    }

    #[test]
    fn writing_to_control_within_vblank_clears_nmi_pin() {
        for cycle_delay in &[2, 3] {
            println!("cycle delay: {}", cycle_delay);

            let mut ppu = Ppu::with_detached_pin();
            ppu.write(0x2000, 0b1000_0000);

            for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + cycle_delay) {
                ppu.step();
            }
            assert!(ppu.nmi_pin.is_pulled());

            ppu.write(0x2000, 0);
            assert!(!ppu.nmi_pin.is_pulled());
        }
    }

    #[test]
    fn reading_status_exactly_on_vblank_cycle_returns_false() {
        let mut ppu = Ppu::with_detached_pin();
        ppu.write(0x2000, 0b1000_0000);

        for _ in 0..(CYCLES_TO_VBLANK_SCANLINE + 1) {
            ppu.step();
        }
        println!("({}, {})", ppu.scanline, ppu.cycle);

        assert!(!ppu.mut_read(0x2002).is_bit_set(7));
    }

    #[test]
    fn odd_frames_are_shorter_by_one_cycle() {
        let mut ppu = Ppu::with_detached_pin();
        ppu.write(0x2001, 0b0000_1000);
        assert!(ppu.mask.show_background());

        for _ in 0..CYCLES_ONE_SCANLINE {
            ppu.step();
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
                ppu.step();
            }
        }
    }

    #[test]
    fn all_frames_are_equal_when_bg_is_disabled() {
        let mut ppu = Ppu::with_detached_pin();
        assert!(!ppu.mask.show_background());

        for _ in 0..CYCLES_ONE_SCANLINE {
            ppu.step();
        }

        for (cycles, frame) in iter::repeat(CYCLES_FULL_FRAME)
            .zip([Frame::Odd, Frame::Even].iter().cycle())
            .take(10)
        {
            assert_eq!(0, ppu.scanline);
            assert_eq!(0, ppu.cycle);
            assert_eq!(*frame, ppu.frame);

            for _ in 0..cycles {
                ppu.step();
            }
        }
    }

    #[test]
    fn oam_write_and_read() {
        let mut ppu = Ppu::with_detached_pin();
        ppu.write(0x2004, 0xFF);
        ppu.write(0x2004, 0xFE);
        ppu.write(0x2004, 0xFD);
        ppu.write(0x2004, 0xFC);

        ppu.write(0x2003, 0x00);
        assert_eq!(0xFF, ppu.mut_read(0x2004));

        ppu.write(0x2003, 0x01);
        assert_eq!(0xFE, ppu.mut_read(0x2004));

        ppu.write(0x2003, 0x02);
        assert_eq!(0xFD & 0xE3, ppu.mut_read(0x2004));

        ppu.write(0x2003, 0x03);
        assert_eq!(0xFC, ppu.mut_read(0x2004));
    }

    /// https://wiki.nesdev.com/w/index.php?title=PPU_scrolling#Register_controls
    #[test]
    fn scroll_and_addr_write_example() {
        let mut ppu = Ppu::with_detached_pin();

        // $2000 write
        ppu.write(0x2000, 0b0000_0011);
        assert_eq!(0b000_1100_0000_0000, ppu.temp_vram_addr);

        // $2002 read
        ppu.mut_read(0x2002);
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
        assert_eq!(ppu.vram_addr, ppu.temp_vram_addr);
        assert_eq!(false, ppu.write_toggle);
    }

    #[test]
    fn write_to_data_increments_vram_addr() {
        let mut ppu = Ppu::with_detached_pin();

        assert_eq!(0, ppu.vram_addr);
        assert_eq!(VRamAddrIncr::Add1GoingAcross, ppu.control.vram_addr_incr());

        ppu.write(0x2007, 0xFF);
        assert_eq!(1, ppu.vram_addr);

        ppu.write(0x2000, 0b0000_0100);
        assert_eq!(VRamAddrIncr::Add32GoingDown, ppu.control.vram_addr_incr());

        ppu.write(0x2007, 0xFF);
        assert_eq!(33, ppu.vram_addr);
    }

    #[test]
    fn read_from_data_increments_vram_addr() {
        let mut ppu = Ppu::with_detached_pin();

        assert_eq!(0, ppu.vram_addr);
        assert_eq!(VRamAddrIncr::Add1GoingAcross, ppu.control.vram_addr_incr());

        ppu.mut_read(0x2007);
        assert_eq!(1, ppu.vram_addr);

        ppu.write(0x2000, 0b0000_0100);
        assert_eq!(VRamAddrIncr::Add32GoingDown, ppu.control.vram_addr_incr());

        ppu.mut_read(0x2007);
        assert_eq!(33, ppu.vram_addr);
    }

    #[test]
    fn read_from_data_not_in_palettes_address_range_is_delayed_by_one_read() {
        // TODO: also test pattern tables address space.
        let mut ppu = Ppu::with_detached_pin();

        assert_eq!(VRamAddrIncr::Add1GoingAcross, ppu.control.vram_addr_incr());
        ppu.vram_addr = 0x2000;
        ppu.write(0x2007, 0xFF);
        ppu.write(0x2007, 0xA0);

        ppu.vram_addr = 0x2000;
        assert_eq!(0x00, ppu.mut_read(0x2007));
        assert_eq!(0xFF, ppu.mut_read(0x2007));
        assert_eq!(0xA0, ppu.mut_read(0x2007));
    }

    #[test]
    fn read_immediately_from_data_in_palettes_address_range() {
        let mut ppu = Ppu::with_detached_pin();

        assert_eq!(VRamAddrIncr::Add1GoingAcross, ppu.control.vram_addr_incr());
        ppu.vram_addr = 0x3F00;
        ppu.write(0x2007, 0xFF);
        ppu.write(0x2007, 0xA0);

        let open_bus_filter_mask = 0b0011_1111;

        ppu.vram_addr = 0x3F00;
        assert_eq!(
            0xFF & open_bus_filter_mask,
            ppu.mut_read(0x2007) & open_bus_filter_mask
        );
        assert_eq!(
            0xA0 & open_bus_filter_mask,
            ppu.mut_read(0x2007) & open_bus_filter_mask
        );
    }
}
