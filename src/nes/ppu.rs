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

#[derive(Debug, Clone, Copy)]
pub struct Ppu {
    control: Control,
    mask: Mask,
    cycle: u16,
    scanline: u16,
    vblank: bool,
    frame: Frame,
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
        }
    }

    pub fn mut_read(&mut self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control.as_u8(),
            0x2001 => self.mask.as_u8(),
            0x2002 => self.status(),
            _ => 0,
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control.as_u8(),
            0x2001 => self.mask.as_u8(),
            0x2002 => self.status_read_only(),
            _ => 0,
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control.set(val),
            0x2001 => self.mask.set(val),
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
}
