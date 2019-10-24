#[cfg(test)]
extern crate pretty_assertions;

mod bits;
mod controller;
mod cpu;
// mod debug;
mod instruction;
mod mapper;
mod memory;
mod pin;
mod ppu;
mod rom;

use bits::BitOps;
pub use controller::Button;
use controller::Controller;
pub use cpu::Cpu;
// use debug::DebugState;
use log::*;
use mapper::Mapper;
pub use memory::{Access, MemoryMap, MutAccess, Ram};
use pin::Pin;
use ppu::Ppu;
pub use ppu::COLORS as PPU_COLORS;
pub use rom::{Mirroring, Rom};
use std::{
    cell::RefCell,
    io::{self, Cursor, Write},
    path::Path,
    rc::Rc,
};

const CYCLES_FULL_FRAME: usize = 341 * 262;

pub trait Renderer {
    fn update(&mut self, buffer: &[u8]);
}

pub struct NullRenderer;

impl Renderer for NullRenderer {
    fn update(&mut self, _buffer: &[u8]) {}
}

pub struct Nes {
    cpu: Cpu,
    ram: Rc<RefCell<Ram>>,
    ppu: Rc<RefCell<Ppu>>,
    mapper: Rc<RefCell<Mapper>>,
    controller1: Rc<RefCell<Controller>>,
    controller2: Rc<RefCell<Controller>>,
}

impl Nes {
    pub fn from_path<P: AsRef<Path>>(
        path: P,
        renderer: Rc<RefCell<dyn Renderer>>,
    ) -> Self {
        let rom = Rom::from_path(path);
        Self::from_rom(rom, renderer)
    }

    pub fn from_buf(buf: &[u8], renderer: Rc<RefCell<dyn Renderer>>) -> Self {
        Self::from_rom(Rom::from_buf(buf), renderer)
    }

    pub fn from_rom(rom: Rom, renderer: Rc<RefCell<dyn Renderer>>) -> Self {
        info!("Mapper ID: {:03}", rom.mapper_id);
        info!("Mirroring: {:?}", rom.mirroring);
        info!("PRG ROM: {} x 16 KiB", rom.prg_banks);
        info!("CHR ROM: {} x  8 KiB", rom.chr_banks);

        let mapper = Rc::new(RefCell::new(Mapper::new(rom)));

        let nmi_pin = Pin::default();

        let ppu = Rc::new(RefCell::new(Ppu::new(
            nmi_pin.clone(),
            mapper.clone(),
            renderer,
        )));
        {
            ppu.borrow_mut()
                .set_mirroring(mapper.borrow_mut().rom.mirroring);
        }

        let controller1 = Rc::new(RefCell::new(Controller::default()));
        let controller2 = Rc::new(RefCell::new(Controller::default()));

        let ram = Rc::new(RefCell::new(Ram::default()));
        let memory = MemoryMap {
            ram: ram.clone(),
            mapper: mapper.clone(),
            ppu: ppu.clone(),
            controller1: controller1.clone(),
            controller2: controller2.clone(),
        };

        let pc = { mapper.borrow_mut().mut_read_word(0xFFFC) };
        let cpu =
            Cpu::new(pc, nmi_pin, Box::new(memory), ppu.clone(), mapper.clone());

        Self {
            cpu,
            ram,
            mapper,
            ppu,
            controller1,
            controller2,
        }
    }

    // pub fn debug_step(&mut self) -> DebugState {
    //     let prev = self.clone();
    //     self.step();
    //     let curr = self.clone();

    //     DebugState { prev, curr }
    // }

    pub fn step(&mut self) -> usize {
        self.cpu.step()
    }

    pub fn controller_set(&mut self, button: Button) {
        info!("set button: {:?}", button);
        self.controller1.borrow_mut().set_button(button);
        self.controller2.borrow_mut().set_button(button);
    }

    pub fn controller_unset(&mut self, button: Button) {
        info!("unset button: {:?}", button);
        self.controller1.borrow_mut().unset_button(button);
        self.controller2.borrow_mut().unset_button(button);
    }

    pub fn render_screen(&self, buf: &mut [u8]) -> io::Result<()> {
        let mut cur = Cursor::new(buf);

        for color_idx in self.ppu.borrow().screen().iter() {
            let color = ppu::COLORS[*color_idx as usize % 64];
            cur.write_all(&color.to_be_bytes()[1..])?;
        }

        Ok(())
    }

    pub fn render_palette(&self, buf: &mut [u8]) -> io::Result<()> {
        debug_assert_eq!(32 * 3, buf.len());

        let ppu = self.ppu.borrow();

        let mut cur = Cursor::new(buf);
        for color_idx in ppu.palette().iter() {
            let color = ppu::COLORS[*color_idx as usize % 64];
            cur.write_all(&color.to_be_bytes()[1..])?;
        }

        Ok(())
    }

    pub fn render_chr_left(&self, buf: &mut [u8]) -> io::Result<()> {
        self.render_chr(0x0000, buf)
    }

    pub fn render_chr_right(&self, buf: &mut [u8]) -> io::Result<()> {
        self.render_chr(0x1000, buf)
    }

    pub fn render_chr(&self, offset: u16, buf: &mut [u8]) -> io::Result<()> {
        debug_assert_eq!(128 * 128 * 3, buf.len());

        let ppu = self.ppu.borrow();

        let mut cur = Cursor::new(buf);

        // Grid is 16x16;
        // Each cell is 8x8;
        for n in 0u16..0x100 {
            let (y, x) = (u64::from(n) / 16, u64::from(n) % 16);
            let addr = n * 16;
            let bytes = self.mapper.borrow().read_multi(offset + addr, 16);

            for (i, (lo, hi)) in
                bytes[..8].iter().zip(bytes[8..].iter()).enumerate()
            {
                let idx = (y * (128 * 8) + (i as u64) * 128) + (x * 8);
                cur.set_position(idx * 3);

                for b in (0..8).rev() {
                    let palette_idx = lo.get_bit(b) | (hi.get_bit(b) << 1);
                    let color_idx = ppu.palette()[palette_idx as usize];
                    let color = ppu::COLORS[color_idx as usize];
                    cur.write_all(&color.to_be_bytes()[1..])?;
                }
            }
        }

        Ok(())
    }

    pub fn reset(&mut self) {
        self.cpu.reset()
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.jump(pc);
    }
}

impl Access for Nes {
    fn read(&self, addr: u16) -> u8 {
        let memory = MemoryMap {
            ram: self.ram.clone(),
            mapper: self.mapper.clone(),
            ppu: self.ppu.clone(),
            controller1: self.controller1.clone(),
            controller2: self.controller2.clone(),
        };

        memory.read(addr)
    }
}
