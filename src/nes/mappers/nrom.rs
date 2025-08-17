use crate::{Access, Mapper, MutAccess, Rom};

#[derive(Clone)]
pub struct Nrom {
    pub rom: Rom,
    banks: usize,
    bank1: usize,
    bank2: usize,
}

impl Nrom {
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

impl Mapper for Nrom {
    fn name(&self) -> &'static str {
        "NROM"
    }
}

impl Access for Nrom {
    fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x0000..=0x1FFF => {
                if let Some(val) = self.rom.chr.get(addr) {
                    *val
                } else {
                    panic!("out of bound CHR read at ${addr:04x}");
                }
            }
            0x6000..=0x7FFF => self.rom.sram[addr - 0x6000],
            0x8000..=0xBFFF => {
                self.rom.prg[self.bank1 * 0x4000 + (addr - 0x8000)]
            }
            0xC000..=0xFFFF => {
                self.rom.prg[self.bank2 * 0x4000 + (addr - 0xC000)]
            }
            _ => unreachable!("mapper: accessed 0x{:04X}", addr),
        }
    }
}

impl MutAccess for Nrom {
    fn mut_read(&mut self, addr: u16) -> u8 {
        self.read(addr)
    }

    fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            0x4020..=0x5FFF => {
                panic!("illegal write at {addr:04X} of {addr:02X}");
            }
            0x6000..=0x7FFF => self.rom.sram[addr - 0x6000] = val,
            0x8000..=0xFFFF => self.bank1 = val as usize % self.banks,
            _ => unreachable!(),
        }
    }
}
