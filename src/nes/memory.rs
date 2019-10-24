use crate::{
    bits::HighLowBits, controller::Controller, mapper::Mapper, ppu::Ppu,
};
use log::*;
use std::{cell::RefCell, rc::Rc};

pub trait MutAccess {
    fn mut_read(&mut self, addr: u16) -> u8;
    fn write(&mut self, addr: u16, val: u8);

    fn mut_read_word(&mut self, addr: u16) -> u16 {
        let lo = self.mut_read(addr);
        let hi = self.mut_read(addr.wrapping_add(1));
        u16::from_hilo(hi, lo)
    }

    fn mut_read_word_bug(&mut self, addr: u16) -> u16 {
        let lo = self.mut_read(addr);
        let hi_addr = (addr & 0xff00) | u16::from(addr.low().wrapping_add(1));
        let hi = self.mut_read(hi_addr);
        u16::from_hilo(hi, lo)
    }
}

pub trait Access {
    fn read(&self, addr: u16) -> u8;

    fn read_word(&self, addr: u16) -> u16 {
        let lo = self.read(addr);
        let hi = self.read(addr.wrapping_add(1));
        u16::from_hilo(hi, lo)
    }

    fn read_multi(&self, addr: u16, bytes: u16) -> Vec<u8> {
        (0_u16..bytes)
            .map(|i| self.read(addr.wrapping_add(i)))
            .collect()
    }
}

const RAM_SIZE: usize = 0x2000;

pub struct Ram([u8; RAM_SIZE]);

impl Default for Ram {
    fn default() -> Self {
        Self([0; RAM_SIZE])
    }
}

impl Ram {
    fn read(&self, addr: u16) -> u8 {
        self.0[(addr % 0x0800) as usize]
    }

    fn write(&mut self, addr: u16, val: u8) {
        self.0[(addr % 0x0800) as usize] = val;
    }
}

pub struct MemoryMap {
    pub ram: Rc<RefCell<Ram>>,
    pub mapper: Rc<RefCell<Mapper>>,
    pub ppu: Rc<RefCell<Ppu>>,
    pub controller1: Rc<RefCell<Controller>>,
    pub controller2: Rc<RefCell<Controller>>,
}

impl Access for MemoryMap {
    fn read(&self, addr: u16) -> u8 {
        let read = match addr {
            0x0000...0x1FFF => self.ram.borrow().read(addr),
            0x2000...0x3FFF => self.ppu.borrow_mut().mut_read(addr),
            0x4000...0x4014 => 0xFF, // TODO read from I/O registers
            0x4015 => 0,             // TODO: APU.
            0x4016 => self.controller1.borrow_mut().mut_read(),
            0x4017 => self.controller1.borrow_mut().mut_read(),
            0x4018...0x5FFF => 0, // TODO: I/O registers.
            0x6000...0xFFFF => self.mapper.borrow_mut().mut_read(addr),
        };

        debug!("read {:04x} => {:02x}", addr, read);

        read
    }
}

impl MutAccess for MemoryMap {
    fn mut_read(&mut self, addr: u16) -> u8 {
        self.read(addr)
    }

    fn write(&mut self, addr: u16, val: u8) {
        match addr {
            0x0000...0x1FFF => self.ram.borrow_mut().write(addr, val),
            0x2000...0x3FFF => self.ppu.borrow_mut().write(addr, val),
            0x4000...0x4013 => (),
            0x4014 => unreachable!(), // TODO: handled in CPU.
            0x4015 => (),             // TODO: APU.
            0x4016 => {
                self.controller1.borrow_mut().write(val);
                self.controller2.borrow_mut().write(val);
            }
            0x4017 => (),          // TODO: APU.
            0x4018...0x401F => (), // TODO write to I/O registers
            0x4020...0xFFFF => self.mapper.borrow_mut().write(addr, val),
        }
    }
}

#[cfg(test)]
mod test {
    use super::Ram;
    use pretty_assertions::assert_eq;

    #[test]
    fn ram_wraparound() {
        let mut ram = Ram::default();
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
