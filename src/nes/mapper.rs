use crate::{
    memory::{Access, MutAccess},
    rom::Rom,
};

#[derive(Clone)]
pub struct Mapper {
    pub rom: Rom,
    banks: usize,
    bank1: usize,
    bank2: usize,
}

impl Mapper {
    pub fn new(rom: Rom) -> Self {
        let banks = rom.prg.len() / 0x4000;

        Self {
            rom,
            banks,
            bank1: 0,
            bank2: banks.saturating_sub(1),
        }
    }
}

impl Access for Mapper {
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x0000...0x1FFF => {
                if let Some(val) = self.rom.chr.get(addr) {
                    *val
                } else {
                    error!("out of bound CHR read at ${:04x}", addr);
                    0
                }
            }
            0x6000...0x7FFF => self.rom.sram[addr - 0x6000],
            0x8000...0xBFFF => {
                let idx = self.bank1 * 0x4000 + (addr - 0x8000);
                self.rom.prg[idx]
            }
            0xC000...0xFFFF => {
                let idx = self.bank2 * 0x4000 + (addr - 0xC000);
                self.rom.prg[idx]
            }
            _ => unreachable!("mapper: accessed 0x{:04X}", addr),
        }
    }
}

impl MutAccess for Mapper {
    fn mut_read(&mut self, addr: u16) -> u8 {
        self.read(addr)
    }

    fn write(&mut self, addr: u16, val: u8) {
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
