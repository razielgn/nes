use self::MasterSlaveSelect::*;
use self::SpriteSize::*;
use self::VRamAddrIncr::*;
use bits::BitOps;

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

#[derive(Clone, Copy)]
pub struct Ppu {
    scanline: u16,
    cycle: u16,
    vblank: bool,
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

    /// $2003 w
    oam_addr: u8,

    /// $2004 r/w
    oam_ram: [u8; 256],
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            control: Control::new(),
            mask: Mask::new(),
            cycle: 0,
            scanline: 0,
            vblank: false,
            frame: Frame::Even,
            oam_addr: 0,
            oam_ram: [0; 256],
        }
    }

    pub fn mut_read(&mut self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control.as_u8(),
            0x2001 => self.mask.as_u8(),
            0x2002 => self.status(),
            0x2004 => self.read_from_oam(),
            _ => 0,
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control.as_u8(),
            0x2001 => self.mask.as_u8(),
            0x2002 => self.status_read_only(),
            0x2004 => self.read_from_oam(),
            _ => 0,
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control.set(val),
            0x2001 => self.mask.set(val),
            0x2003 => self.oam_addr = val,
            0x2004 => self.write_to_oam(val),
            _ => (),
        }
    }

    pub fn step(&mut self) -> bool {
        trace!(
            "step cycle {:3}, scanline {:3}, frame {:4?}",
            self.cycle,
            self.scanline,
            self.frame
        );

        let mut nim = false;

        match (self.cycle, self.scanline) {
            (1, 241) => {
                debug!("vblank starts");
                self.vblank = true;

                if self.control.nmi_at_next_vblank() {
                    nim = true;
                }
            }
            (1, 261) => {
                debug!("vblank ends");
                self.vblank = false
            }
            _ => (),
        }

        self.cycle += 1;

        match (self.frame, self.cycle, self.scanline) {
            (Frame::Even, 341, 261) | (Frame::Odd, 340, 261) => {
                debug!("end of {:?} frame", self.frame);
                self.cycle = 0;
                self.scanline = 0;
                self.frame = self.frame.next();
            }
            (_, 341, _) => {
                debug!("end of scanline {}", self.scanline);
                self.cycle = 0;
                self.scanline += 1;
            }
            _ => {}
        }

        nim
    }

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
    pub fn status(&mut self) -> u8 {
        let mut status = self.control.as_u8() & 0x1F;

        if self.vblank {
            status.set_bit(7);
            self.vblank = false;
            debug!("vblank reset");
        }

        status
    }

    pub fn status_read_only(&self) -> u8 {
        let mut status = self.control.as_u8() & 0x1F;

        if self.vblank {
            status.set_bit(7);
        }

        status
    }

    fn read_from_oam(&self) -> u8 {
        self.oam_ram[self.oam_addr as usize]
    }

    fn write_to_oam(&mut self, mut val: u8) {
        match self.scanline {
            0...239 | 261 if self.is_rendering_enabled() => {
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

    fn is_rendering_enabled(&self) -> bool {
        self.mask.show_background() || self.mask.show_sprites()
    }
}

#[allow(dead_code)]
#[derive(Debug, Clone, Copy)]
struct Control(u8);

#[allow(dead_code)]
impl Control {
    pub fn new() -> Self {
        Control(0)
    }

    pub fn set(&mut self, v: u8) {
        self.0 = v;
    }

    pub fn as_u8(self) -> u8 {
        self.0
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
#[derive(Debug, Clone, Copy)]
struct Mask(u8);

#[allow(dead_code)]
impl Mask {
    pub fn new() -> Self {
        Mask(0)
    }

    pub fn set(&mut self, v: u8) {
        self.0 = v;
    }

    pub fn as_u8(self) -> u8 {
        self.0
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

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum VRamAddrIncr {
    Add1GoingAcross,
    Add32GoingDown,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum SpriteSize {
    EightByEight,
    EightBySixteen,
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum MasterSlaveSelect {
    ReadBackdropFromExt,
    OutputColorOnExt,
}

#[cfg(test)]
mod tests {
    use super::*;

    mod control {
        use super::super::*;

        #[test]
        fn name_table_addr() {
            let mut control = Control::new();

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
            let mut control = Control::new();

            control.set(0b00000000);
            assert_eq!(Add1GoingAcross, control.vram_addr_incr());
            control.set(0b00000100);
            assert_eq!(Add32GoingDown, control.vram_addr_incr());
        }

        #[test]
        fn sprite_pattern_table_addr() {
            let mut control = Control::new();
            control.set(0b00000000);
            assert_eq!(0x0000, control.sprite_pattern_table_addr());
            control.set(0b00001000);
            assert_eq!(0x1000, control.sprite_pattern_table_addr());
        }

        #[test]
        fn background_pattern_table_addr() {
            let mut control = Control::new();
            control.set(0b00000000);
            assert_eq!(0x0000, control.background_pattern_table_addr());
            control.set(0b00010000);
            assert_eq!(0x1000, control.background_pattern_table_addr());
        }

        #[test]
        fn sprite_size() {
            let mut control = Control::new();
            control.set(0b00000000);
            assert_eq!(EightByEight, control.sprite_size());
            control.set(0b00100000);
            assert_eq!(EightBySixteen, control.sprite_size());
        }

        #[test]
        fn master_slave_select() {
            let mut control = Control::new();
            control.set(0b00000000);
            assert_eq!(ReadBackdropFromExt, control.master_slave_select());
            control.set(0b01000000);
            assert_eq!(OutputColorOnExt, control.master_slave_select());
        }

        #[test]
        fn nmi_at_next_vblank() {
            let mut control = Control::new();
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
            let mut mask = Mask::new();
            mask.set(0b00000000);
            assert_eq!(false, mask.grayscale());
            mask.set(0b00000001);
            assert_eq!(true, mask.grayscale());
        }
    }

    #[test]
    fn vblank_has_started_and_reading_status_resets_it() {
        let mut ppu = Ppu::new();
        assert!(!ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(341 * 241) + 2 {
            ppu.step();
        }

        assert!(ppu.mut_read(0x2002).is_bit_set(7));
        assert!(!ppu.read(0x2002).is_bit_set(7));
    }

    #[test]
    fn vblank_has_ended() {
        let mut ppu = Ppu::new();
        assert!(!ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(341 * 241) + 2 {
            ppu.step();
        }

        assert!(ppu.read(0x2002).is_bit_set(7));

        for _ in 0..(341 * 20) {
            ppu.step();
        }

        assert!(!ppu.read(0x2002).is_bit_set(7));
    }

    #[test]
    fn odd_frames_are_shorter_by_one_cycle() {
        let mut ppu = Ppu::new();
        assert_eq!(Frame::Even, ppu.frame);

        for _ in 0..341 * 262 {
            ppu.step();
        }

        assert_eq!(0, ppu.scanline);
        assert_eq!(0, ppu.cycle);
        assert_eq!(Frame::Odd, ppu.frame);

        for _ in 0..(341 * 262) - 1 {
            ppu.step();
        }

        assert_eq!(0, ppu.scanline);
        assert_eq!(0, ppu.cycle);
        assert_eq!(Frame::Even, ppu.frame);
    }

    #[test]
    fn oam_write_and_read() {
        let mut ppu = Ppu::new();
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
}
