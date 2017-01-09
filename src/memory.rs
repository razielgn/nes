use rom::Rom;

pub struct Memory<'rom> {
    ram: [u8; 0x2000],
    rom: Rom<'rom>,
}

impl<'rom> Memory<'rom> {
    pub fn new(rom: Rom<'rom>) -> Self {
        Memory {
            ram: [0; 0x2000],
            rom: rom,
        }
    }

    pub fn fetch<A>(&self, addr: A) -> u8
        where A: Into<u16>
    {
        let addr = addr.into() as usize;

        let prg_banks = self.rom.prg.len() / 0x4000;
        let prg_bank2 = (prg_banks - 1) * 0x4000;

        match addr {
            0x0000...0x1FFF => self.ram[addr],
            0x2000...0x3FFF => panic!("PPU register"),
            0x4000...0x401F => panic!("I/O registers"),
            0x4020...0x5FFF => self.rom.chr[addr - 0x4020],
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000],
            0x8000...0xBFFF => self.rom.prg[addr - 0x8000],
            0xC000...0xFFFF => self.rom.prg[prg_bank2 + (addr - 0xC000)],
            _ => unimplemented!(),
        }
    }

    pub fn fetch_double(&self, addr: u16) -> u16 {
        let lo = self.fetch(addr) as u16;
        let hi = self.fetch(addr + 1) as u16;
        hi << 8 | lo
    }

    pub fn fetch_double_bug(&self, addr: u16) -> u16 {
        let lo = self.fetch(addr) as u16;
        let hi =
            self.fetch((addr & 0xff00) |
                       ((addr as u8).wrapping_add(1)) as u16) as u16;
        hi << 8 | lo
    }

    pub fn store<A, V>(&mut self, addr: A, val: V)
        where A: Into<u16>,
              V: Into<u8>
    {
        let addr = addr.into() as usize;
        let val = val.into();

        if addr < 0x2000 {
            self.ram[addr] = val;
        } else {
            panic!("cannot write outside handled memory");
        }
    }
}
