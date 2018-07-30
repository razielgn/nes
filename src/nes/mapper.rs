use bits::HighLowBits;
use rom::Rom;

pub struct Mapper {
    rom: Rom,
    banks: usize,
    bank1: usize,
    bank2: usize,
}

impl Mapper {
    pub fn new(rom: Rom) -> Self {
        let banks = rom.prg.len() / 0x4000;

        Mapper {
            rom,
            banks,
            bank1: 0,
            bank2: banks.saturating_sub(1),
        }
    }

    pub fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x4020...0x5FFF => self.rom.chr[addr - 0x4020],
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000],
            0x8000...0xBFFF => {
                let idx = self.bank1 * 0x4000 + (addr - 0x8000);
                self.rom.prg[idx]
            }
            0xC000...0xFFFF => {
                let idx = self.bank2 * 0x4000 + (addr - 0xC000);
                self.rom.prg[idx]
            }
            _ => unreachable!(),
        }
    }

    pub fn read_word(&self, addr: u16) -> u16 {
        let lo = self.read(addr);
        let hi = self.read(addr.wrapping_add(1));
        u16::from_hilo(hi, lo)
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            0x4020...0x5FFF => {
                warn!("illegal write at {:04X} of {:02X}", addr, addr);
            }
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000] = val,
            0x8000...0xFFFF => self.bank1 = val as usize % self.banks,
            _ => unreachable!(),
        }
    }
}
