use self::MasterSlaveSelect::*;
use self::SpriteSize::*;
use self::VRamAddrIncr::*;
use bits::BitOps;

pub struct Ppu {
    control: u8,
    mask: u8,
    spr_ram_addr: usize,
    spr_ram: [u8; 0xFF],
    vram_addr: usize,
    vram: [u8; 0xFFFF],
    sprite_overflow: bool,
    sprite_zero_hit: bool,
    nmi_occurred: bool,
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            control: 0,
            mask: 0,
            spr_ram_addr: 0,
            spr_ram: [0; 0xFF],
            vram_addr: 0x0000,
            vram: [0; 0xFFFF],
            sprite_overflow: false,
            sprite_zero_hit: false,
            nmi_occurred: false,
        }
    }

    pub fn read(&mut self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control,
            0x2001 => self.mask,
            0x2002 => self.status(),
            0x2003 => self.spr_ram_addr as u8,
            0x2004 => self.spr_ram[self.spr_ram_addr],
            0x2005 => (self.vram_addr >> 8) as u8,
            0x2006 => self.vram_addr as u8,
            0x2007 => self.vram[self.vram_addr],
            _ => panic!("Unhandled PPU read at addr {:04X}", addr),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control = val,
            0x2001 => self.mask = val,
            0x2002 => (),
            0x2003 => self.spr_ram_addr = val as usize,
            0x2004 => self.spr_ram[self.spr_ram_addr] = val,
            0x2005 => self.vram_addr |= (val as usize) << 8,
            0x2006 => self.vram_addr |= val as usize,
            0x2007 => self.vram[self.vram_addr] = val,
            _ => unreachable!(),
        }
    }

    pub fn status(&mut self) -> u8 {
        let mut status = self.control & 0x1F;

        if self.sprite_overflow {
            status.set_bit(5);
        }
        if self.sprite_zero_hit {
            status.set_bit(6);
        }
        if self.nmi_occurred {
            status.set_bit(7);
        }

        status
    }

    pub fn base_nametable_addr(&self) -> u16 {
        match self.control & 3 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => unreachable!(),
        }
    }

    pub fn vram_addr_incr(&self) -> VRamAddrIncr {
        if self.control.is_bit_set(2) {
            Add32GoingDown
        } else {
            Add1GoingAcross
        }
    }

    pub fn sprite_pattern_table_addr(&self) -> u16 {
        if self.control.is_bit_set(3) {
            0x1000
        } else {
            0x0000
        }
    }

    pub fn background_pattern_table_addr(&self) -> u16 {
        if self.control.is_bit_set(4) {
            0x1000
        } else {
            0x0000
        }
    }

    pub fn sprite_size(&self) -> SpriteSize {
        if self.control.is_bit_set(5) {
            SixteenBySixteen
        } else {
            EightByEight
        }
    }

    pub fn master_slave_select(&self) -> MasterSlaveSelect {
        if self.control.is_bit_set(6) {
            OutputColorOnExt
        } else {
            ReadBackdropFromExt
        }
    }

    pub fn nmi_at_next_vblank(&self) -> bool {
        self.control.is_bit_set(7)
    }

    pub fn grayscale(&self) -> bool {
        self.mask.is_bit_set(0)
    }

    pub fn show_background_in_contour(&self) -> bool {
        self.mask.is_bit_set(1)
    }

    pub fn show_sprites_in_contour(&self) -> bool {
        self.mask.is_bit_set(2)
    }

    pub fn show_background(&self) -> bool {
        self.mask.is_bit_set(3)
    }

    pub fn show_sprites(&self) -> bool {
        self.mask.is_bit_set(4)
    }

    pub fn emphasize_red(&self) -> bool {
        self.mask.is_bit_set(5)
    }

    pub fn emphasize_green(&self) -> bool {
        self.mask.is_bit_set(6)
    }

    pub fn emphasize_blue(&self) -> bool {
        self.mask.is_bit_set(7)
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
    SixteenBySixteen,
}

#[derive(Debug, PartialEq)]
pub enum MasterSlaveSelect {
    ReadBackdropFromExt,
    OutputColorOnExt,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_table_addr() {
        let mut ppu = Ppu::new();

        ppu.write(0x2000, 0b00000000);
        assert_eq!(0x2000, ppu.base_nametable_addr());
        ppu.write(0x2000, 0b00000001);
        assert_eq!(0x2400, ppu.base_nametable_addr());
        ppu.write(0x2000, 0b00000010);
        assert_eq!(0x2800, ppu.base_nametable_addr());
        ppu.write(0x2000, 0b00000011);
        assert_eq!(0x2C00, ppu.base_nametable_addr());
    }

    #[test]
    fn vram_addr_incr() {
        let mut ppu = Ppu::new();

        ppu.write(0x2000, 0b00000000);
        assert_eq!(Add1GoingAcross, ppu.vram_addr_incr());
        ppu.write(0x2000, 0b00000100);
        assert_eq!(Add32GoingDown, ppu.vram_addr_incr());
    }

    #[test]
    fn sprite_pattern_table_addr() {
        let mut ppu = Ppu::new();
        ppu.write(0x2000, 0b00000000);
        assert_eq!(0x0000, ppu.sprite_pattern_table_addr());
        ppu.write(0x2000, 0b00001000);
        assert_eq!(0x1000, ppu.sprite_pattern_table_addr());
    }

    #[test]
    fn background_pattern_table_addr() {
        let mut ppu = Ppu::new();
        ppu.write(0x2000, 0b00000000);
        assert_eq!(0x0000, ppu.background_pattern_table_addr());
        ppu.write(0x2000, 0b00010000);
        assert_eq!(0x1000, ppu.background_pattern_table_addr());
    }

    #[test]
    fn sprite_size() {
        let mut ppu = Ppu::new();
        ppu.write(0x2000, 0b00000000);
        assert_eq!(EightByEight, ppu.sprite_size());
        ppu.write(0x2000, 0b00100000);
        assert_eq!(SixteenBySixteen, ppu.sprite_size());
    }

    #[test]
    fn master_slave_select() {
        let mut ppu = Ppu::new();
        ppu.write(0x2000, 0b00000000);
        assert_eq!(ReadBackdropFromExt, ppu.master_slave_select());
        ppu.write(0x2000, 0b01000000);
        assert_eq!(OutputColorOnExt, ppu.master_slave_select());
    }

    #[test]
    fn nmi_at_next_vblank() {
        let mut ppu = Ppu::new();
        ppu.write(0x2000, 0b00000000);
        assert_eq!(false, ppu.nmi_at_next_vblank());
        ppu.write(0x2000, 0b10000000);
        assert_eq!(true, ppu.nmi_at_next_vblank());
    }

    #[test]
    fn grayscale() {
        let mut ppu = Ppu::new();
        ppu.write(0x2001, 0b00000000);
        assert_eq!(false, ppu.grayscale());
        ppu.write(0x2001, 0b00000001);
        assert_eq!(true, ppu.grayscale());
    }
}
