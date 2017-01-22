pub struct Ppu {
    control1: u8,
    control2: u8,
    status: u8,
    spr_ram_addr: usize,
    spr_ram: [u8; 0xFF],
    vram_addr: usize,
    vram: [u8; 0xFFFF],
}

impl Ppu {
    pub fn new() -> Self {
        Ppu {
            control1: 0,
            control2: 0,
            status: 0,
            spr_ram_addr: 0,
            spr_ram: [0; 0xFF],
            vram_addr: 0x0000,
            vram: [0; 0xFFFF],
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        match 0x2000 + (addr % 8) {
            0x2000 => self.control1,
            0x2001 => self.control2,
            0x2002 => self.status,
            0x2003 => self.spr_ram_addr as u8,
            0x2004 => self.spr_ram[self.spr_ram_addr],
            0x2005 => (self.vram_addr >> 8) as u8,
            0x2006 => self.vram_addr as u8,
            0x2007 => self.vram[self.vram_addr],
            _ => panic!("Unhandled PPU read at addr {:04X}", addr),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x2000 => self.control1 = val,
            0x2001 => self.control2 = val,
            0x2003 => self.spr_ram_addr = val as usize,
            0x2004 => self.spr_ram[self.spr_ram_addr] = val,
            0x2005 => self.vram_addr |= (val as usize) << 8,
            0x2006 => self.vram_addr |= val as usize,
            0x2007 => self.vram[self.vram_addr] = val,
            _ => panic!("Unhandled PPU write at addr {:04X}", addr),
        }
    }
}
