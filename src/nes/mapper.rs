use memory::MutMemoryAccess;
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
            rom: rom,
            banks: banks,
            bank1: 0,
            bank2: banks.saturating_sub(1),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            0x4020...0x5FFF => panic!("write in ROM-CHR"),
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000] = val,
            0x8000...0xFFFF => self.bank1 = val as usize % self.banks,
            _ => unimplemented!(),
        }
    }
}

impl MutMemoryAccess for Mapper {
    fn read(&mut self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x4020...0x5FFF => self.rom.chr[addr - 0x4020],
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000],
            0x8000...0xBFFF => {
                self.rom.prg[self.bank1 * 0x4000 + (addr - 0x8000)]
            }
            0xC000...0xFFFF => {
                self.rom.prg[self.bank2 * 0x4000 + (addr - 0xC000)]
            }
            _ => unimplemented!(),
        }
    }
}
