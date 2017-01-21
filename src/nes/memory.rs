use ppu::Ppu;
use rom::Rom;

pub struct Memory {
    ram: [u8; 0x2000],
    rom: Rom,
    banks: usize,
    bank1: usize,
    bank2: usize,
    ppu: Ppu,
}

impl Memory {
    pub fn new(rom: Rom) -> Self {
        let banks = rom.prg.len() / 0x4000;

        Memory {
            ram: [0; 0x2000],
            rom: rom,
            banks: banks,
            bank1: 0,
            bank2: banks.saturating_sub(1),
            ppu: Ppu::new(),
        }
    }

    pub fn fetch<A>(&self, addr: A) -> u8
        where A: Into<u16>
    {
        let addr = addr.into() as usize;

        match addr {
            0x0000...0x1FFF => self.ram[addr % 0x0800],
            0x2000...0x3FFF => self.ppu.read(0x2000 + (addr % 8) as u16),
            0x4000...0x401F => 0xFF, // TODO read from I/O registers
            0x4020...0x5FFF => self.rom.chr[addr - 0x4020],
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000],
            0x8000...0xBFFF => {
                self.rom.prg[self.bank1 * 0x4000 + (addr - 0x8000)]
            }
            0xC000...0xFFFF => {
                self.rom.prg[self.bank2 * 0x4000 + (addr - 0xC000)]
            }
            _ => unreachable!(),
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

    pub fn fetch_multi(&self, offset: u16, bytes: usize) -> Vec<u8> {
        (0..bytes).map(|i| self.fetch(offset + i as u16)).collect()
    }

    pub fn store<A, V>(&mut self, addr: A, val: V)
        where A: Into<u16>,
              V: Into<u8>
    {
        let addr = addr.into() as usize;
        let val = val.into();

        match addr {
            0x0000...0x1FFF => self.ram[addr % 0x0800] = val,
            0x2000...0x3FFF => self.ppu.write(0x2000 + (addr % 8) as u16, val),
            0x4000...0x401F => (), // TODO: write to I/O registers
            0x4020...0x5FFF => panic!("write in ROM-CHR"),
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000] = val,
            0x8000...0xFFFF => self.bank1 = val as usize % self.banks,
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Memory;
    use rom::Rom;

    #[test]
    fn ram_access() {
        let mut memory = Memory::new(Rom::empty());
        memory.store(0x0000u16, 0xFF);

        assert_eq!(0xFF, memory.fetch(0x0000u16));
        assert_eq!(0xFF, memory.fetch(0x0800u16));
        assert_eq!(0xFF, memory.fetch(0x1000u16));
        assert_eq!(0xFF, memory.fetch(0x1800u16));

        memory.store(0x0800u16, 0xAA);

        assert_eq!(0xAA, memory.fetch(0x0000u16));
        assert_eq!(0xAA, memory.fetch(0x0800u16));
        assert_eq!(0xAA, memory.fetch(0x1000u16));
        assert_eq!(0xAA, memory.fetch(0x1800u16));
    }
}
