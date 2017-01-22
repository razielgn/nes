#![cfg_attr(feature = "cargo-clippy", allow(new_without_default))]

#[macro_use]
extern crate nom;

mod bits;
mod cpu;
mod instruction;
mod mapper;
mod memory;
mod ppu;
mod rom;

pub use cpu::Cpu;
use instruction::Instruction;
use mapper::Mapper;
use memory::{MemoryMap, Ram};
use ppu::Ppu;
pub use rom::Rom;
use std::fmt;
use std::path::Path;

pub struct Nes {
    cpu: Cpu,
    mapper: Mapper,
    ppu: Ppu,
    ram: Ram,
}

impl Nes {
    pub fn from_rom<P: AsRef<Path>>(path: P) -> Self {
        let rom = Rom::from_path(path);
        let mapper = Mapper::new(rom);
        let pc = mapper.read_double(0xFFFC);

        Nes {
            cpu: Cpu::new(pc),
            mapper: mapper,
            ppu: Ppu::new(),
            ram: Ram::new(),
        }
    }

    pub fn state(&mut self) -> State {
        let mmap = MemoryMap {
            ram: &mut self.ram,
            mapper: &mut self.mapper,
            ppu: &mut self.ppu,
        };

        State {
            pc: self.cpu.pc,
            a: self.cpu.a,
            x: self.cpu.x,
            y: self.cpu.y,
            p: self.cpu.p.into(),
            sp: self.cpu.sp,
            cycles: self.cpu.cycles,
            instr: self.cpu.fetch(&mmap),
            memory: mmap,
        }
    }

    pub fn step(&mut self) {
        let mut mmap = MemoryMap {
            ram: &mut self.ram,
            mapper: &mut self.mapper,
            ppu: &mut self.ppu,
        };

        self.cpu.step(&mut mmap);
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.jump(pc);
    }
}

pub struct State<'a> {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub p: u8,
    pub sp: u8,
    pub cycles: usize,
    pub instr: Instruction,
    pub memory: MemoryMap<'a>,
}

impl<'a> fmt::Display for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{pc:04X}  {bytecode:9} {instr:31} A:{a:02X} X:{x:02X} \
                Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
               pc = self.pc,
               bytecode = self.instr.bytecode(&self),
               instr = self.instr.to_string(&self),
               a = self.a,
               x = self.x,
               y = self.y,
               p = self.p,
               sp = self.sp,
               cyc = (self.cycles * 3) % 341)
    }
}
