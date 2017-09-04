use mapper::SharedMapper;
use ppu::Ppu;

pub trait MutMemoryAccess {
    fn read(&mut self, addr: u16) -> u8;

    fn read_double(&mut self, addr: u16) -> u16 {
        let lo = self.read(addr) as u16;
        let hi = self.read(addr + 1) as u16;
        hi << 8 | lo
    }

    fn read_double_bug(&mut self, addr: u16) -> u16 {
        let lo = self.read(addr) as u16;
        let hi =
            self.read((addr & 0xff00) | ((addr as u8).wrapping_add(1)) as u16) as
                u16;
        hi << 8 | lo
    }

    fn read_multi(&mut self, offset: u16, bytes: usize) -> Vec<u8> {
        (0..bytes).map(|i| self.read(offset + i as u16)).collect()
    }
}

const RAM_SIZE: usize = 0x2000;

pub struct Ram([u8; RAM_SIZE]);

impl Ram {
    pub fn new() -> Self {
        Ram([0; RAM_SIZE])
    }

    pub fn read(&self, addr: u16) -> u8 {
        let addr = addr as usize;

        match addr {
            0x0000...0x1FFF => self.0[addr % 0x0800],
            _ => unimplemented!(),
        }
    }

    pub fn write(&mut self, addr: u16, val: u8) {
        let addr = addr as usize;

        match addr {
            0x0000...0x1FFF => self.0[addr % 0x0800] = val,
            _ => unimplemented!(),
        }
    }
}

pub struct MutMemory<'a> {
    pub ram: &'a mut Ram,
    pub mapper: SharedMapper,
    pub ppu: &'a mut Ppu,
}

impl<'a> MutMemory<'a> {
    fn dma_transfer(&mut self, val: u8) {
        let address = (val as u16) << 8;

        for i in 0..256 {
            let b = self.read(address + i);
            self.ppu.write(0x2004, b);
        }
    }
}

impl<'a> MutMemory<'a> {
    pub fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000...0x1FFF => self.ram.write(addr, val),
            0x2000...0x3FFF => self.ppu.write(addr, val),
            0x4014 => self.dma_transfer(val),
            0x4000...0x401F => (), // TODO write to I/O registers
            0x4020...0xFFFF => {
                let mut mapper = self.mapper.borrow_mut();
                mapper.write(addr, val);
            }
            _ => unreachable!(),
        }
    }
}

impl<'a> MutMemoryAccess for MutMemory<'a> {
    fn read(&mut self, addr: u16) -> u8 {
        match addr {
            0x0000...0x1FFF => self.ram.read(addr),
            0x2000...0x3FFF => self.ppu.read(addr),
            0x4000...0x401F => 0xFF, // TODO read from I/O registers
            0x4020...0xFFFF => {
                let mut mapper = self.mapper.borrow_mut();
                mapper.read(addr)
            }
            _ => unreachable!(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Ram;

    #[test]
    fn ram_wraparound() {
        let mut ram = Ram::new();
        ram.write(0x0000u16, 0xFF);

        assert_eq!(0xFF, ram.read(0x0000u16));
        assert_eq!(0xFF, ram.read(0x0800u16));
        assert_eq!(0xFF, ram.read(0x1000u16));
        assert_eq!(0xFF, ram.read(0x1800u16));

        ram.write(0x0800u16, 0xAA);

        assert_eq!(0xAA, ram.read(0x0000u16));
        assert_eq!(0xAA, ram.read(0x0800u16));
        assert_eq!(0xAA, ram.read(0x1000u16));
        assert_eq!(0xAA, ram.read(0x1800u16));
    }
}
