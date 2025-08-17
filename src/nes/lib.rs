#![allow(clippy::missing_errors_doc, clippy::enum_glob_use)]

#[cfg(test)]
extern crate pretty_assertions;

mod bits;
mod controller;
mod cpu;
mod debug;
mod instruction;
mod mappers;
mod memory;
mod pin;
mod ppu;
mod rom;

use bits::BitOps;
pub use controller::Button;
use controller::Controller;
pub use cpu::{Cpu, Cycles};
use debug::DebugState;
use log::debug;
use mappers::Mapper;
pub use memory::{Access, Memory, MutAccess, MutMemory, Ram};
use pin::Pin;
use ppu::Ppu;
pub use rom::Rom;
use std::{
    cell::Cell,
    io::{self, Cursor, Write},
    path::Path,
    rc::Rc,
};

const INIT_PC_ADDR: u16 = 0xFFFC;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Mirroring {
    #[default]
    Horizontal,
    Vertical,
}

pub type CurrentMirroring = Rc<Cell<Mirroring>>;

#[derive(Clone)]
pub struct Nes {
    cpu: Cpu,
    mapper: Box<dyn Mapper>,
    ppu: Ppu,
    ram: Ram,
    controller1: Controller,
    controller2: Controller,
}

macro_rules! mut_memory {
    ($self:expr) => {
        MutMemory {
            ram: &mut $self.ram,
            mapper: $self.mapper.as_mut(),
            ppu: &mut $self.ppu,
            controller1: &mut $self.controller1,
            controller2: &mut $self.controller2,
        }
    };
}

impl Nes {
    #[must_use]
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let rom = Rom::from_path(path);
        Self::from_rom(rom)
    }

    #[must_use]
    pub fn from_buf(buf: &[u8]) -> Self {
        Self::from_rom(Rom::from_buf(buf))
    }

    #[must_use]
    pub fn from_rom(rom: Rom) -> Self {
        let (mapper, current_mirroring) = mappers::load(rom);
        let pc = mapper.read_word(INIT_PC_ADDR);
        let nmi_pin = Pin::default();

        let cpu = Cpu::with_pc_and_nmi_pin(pc, nmi_pin.clone());
        let ppu = Ppu::new(nmi_pin, current_mirroring);

        Self {
            cpu,
            mapper,
            ppu,
            ram: Ram::default(),
            controller1: Controller::default(),
            controller2: Controller::default(),
        }
    }

    pub fn debug_step(&mut self) -> DebugState {
        let prev = Box::new(self.clone());
        self.step();
        let curr = Box::new(self.clone());

        DebugState { prev, curr }
    }

    pub fn step(&mut self) -> Cycles {
        debug!("step");

        let cycles = {
            let mut mem = mut_memory!(self);
            self.cpu.step(&mut mem)
        };

        for _ in 0..cycles * 3 {
            self.ppu.step(self.mapper.as_mut());
            self.mapper.step();
        }

        cycles
    }

    pub fn ppu_frame_ready_latch(&mut self) -> bool {
        if self.ppu.frame_ready {
            self.ppu.frame_ready = false;
            return true;
        }

        false
    }

    pub fn controller1_set(&mut self, button: Button) {
        self.controller1.set_button(button);
    }

    pub fn controller1_unset(&mut self, button: Button) {
        self.controller1.unset_button(button);
    }

    pub fn controller2_set(&mut self, button: Button) {
        self.controller2.set_button(button);
    }

    pub fn controller2_unset(&mut self, button: Button) {
        self.controller2.unset_button(button);
    }

    pub fn render_screen(&self, buf: &mut [u8]) -> io::Result<()> {
        debug_assert_eq!(256 * 240 * 3, buf.len());

        let mut cur = Cursor::new(buf);

        for color_idx in self.ppu.screen() {
            let color = ppu::COLORS[*color_idx as usize % 64];
            cur.write_all(&color.to_be_bytes()[1..])?;
        }

        Ok(())
    }

    pub fn render_palette(&self, buf: &mut [u8]) -> io::Result<()> {
        debug_assert_eq!(32 * 3, buf.len());

        let palette = self.ppu.palette();

        let mut cur = Cursor::new(buf);
        for color_idx in palette {
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

        let palette = self.ppu.palette();

        let mut cur = Cursor::new(buf);

        // Grid is 16x16;
        // Each cell is 8x8;
        for n in 0u16..0x100 {
            let (y, x) = (u64::from(n) / 16, u64::from(n) % 16);
            let addr = n * 16;
            let bytes = self.mapper.read_multi(offset + addr, 16);

            for (i, (lo, hi)) in
                bytes[..8].iter().zip(bytes[8..].iter()).enumerate()
            {
                let idx = (y * (128 * 8) + (i as u64) * 128) + (x * 8);
                cur.set_position(idx * 3);

                for b in (0..8).rev() {
                    let palette_idx = lo.get_bit(b) | (hi.get_bit(b) << 1);
                    let color_idx = palette[palette_idx as usize];
                    let color = ppu::COLORS[color_idx as usize];
                    cur.write_all(&color.to_be_bytes()[1..])?;
                }
            }
        }

        Ok(())
    }

    pub fn reset(&mut self) {
        let mut mem = mut_memory!(self);
        self.cpu.reset(&mut mem);
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.jump(pc);
    }

    #[must_use]
    pub fn memory(&self) -> Memory<'_> {
        Memory {
            ram: &self.ram,
            mapper: self.mapper.as_ref(),
            ppu: &self.ppu,
        }
    }
}

impl Access for Nes {
    fn read(&self, addr: u16) -> u8 {
        self.memory().read(addr)
    }
}
