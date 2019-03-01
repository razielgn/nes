#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;
#[cfg(test)]
extern crate pretty_assertions;

mod bits;
mod cpu;
mod debug;
mod instruction;
mod mapper;
mod memory;
mod ppu;
mod rom;

pub use cpu::{Cpu, Cycles};
use debug::DebugState;
use mapper::Mapper;
pub use memory::{Access, Memory, MutMemory, Ram};
use ppu::{Ppu, StepResult};
pub use rom::{Mirroring, Rom};
use std::path::Path;

#[derive(Clone)]
pub struct Nes {
    cpu: Cpu,
    mapper: Mapper,
    ppu: Ppu,
    ram: Ram,
}

macro_rules! mut_memory {
    ($self:expr) => {
        MutMemory {
            ram: &mut $self.ram,
            mapper: &mut $self.mapper,
            ppu: &mut $self.ppu,
        }
    };
}

impl Nes {
    pub fn from_path<P: AsRef<Path>>(path: P) -> Self {
        let rom = Rom::from_path(path);
        Self::from_rom(rom)
    }

    pub fn from_buf(buf: &[u8]) -> Self {
        Self::from_rom(Rom::from_buf(buf))
    }

    pub fn from_rom(rom: Rom) -> Self {
        let mapper = Mapper::new(rom);
        let pc = mapper.read_word(0xFFFC);

        Self {
            cpu: Cpu::new(pc),
            mapper,
            ppu: Ppu::default(),
            ram: Ram::default(),
        }
    }

    pub fn debug_step(&mut self) -> DebugState {
        let prev = self.clone();
        self.step();
        let curr = self.clone();

        DebugState { prev, curr }
    }

    pub fn step(&mut self) -> Cycles {
        debug!("step");

        let cycles = {
            let mut mem = mut_memory!(self);
            self.cpu.step(&mut mem)
        };

        for _ in 0..cycles * 3 {
            match self.ppu.step() {
                StepResult::Nothing => {}
                StepResult::NmiPulled => {
                    self.cpu.nmi = true;
                }
                StepResult::NmiCleared => {
                    self.cpu.nmi = false;
                }
            }
        }

        cycles
    }

    pub fn reset(&mut self) {
        let mut mem = mut_memory!(self);
        self.cpu.reset(&mut mem)
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.jump(pc);
    }

    pub fn memory(&self) -> Memory {
        Memory {
            ram: &self.ram,
            mapper: &self.mapper,
            ppu: &self.ppu,
        }
    }
}

impl Access for Nes {
    fn read(&self, addr: u16) -> u8 {
        self.memory().read(addr)
    }
}
