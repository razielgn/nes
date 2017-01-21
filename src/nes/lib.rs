#[macro_use]
extern crate nom;

mod cpu;
mod instruction;
mod memory;
mod ppu;
mod rom;

pub use cpu::Cpu;
use instruction::Instruction;
use memory::Memory;
use ppu::Ppu;
pub use rom::Rom;
use std::fmt;
use std::path::Path;

pub struct Nes {
    cpu: Cpu,
    memory: Memory,
    ppu: Ppu,
}

impl Nes {
    pub fn from_rom<P: AsRef<Path>>(path: P) -> Self {
        let rom = Rom::from_path(path);
        let memory = Memory::new(rom);
        let pc = memory.fetch_double(0xFFFC);

        Nes {
            cpu: Cpu::new(pc),
            memory: memory,
            ppu: Ppu::new(),
        }
    }

    pub fn state(&self) -> State {
        State {
            pc: self.cpu.pc,
            a: self.cpu.a,
            x: self.cpu.x,
            y: self.cpu.y,
            p: self.cpu.p.into(),
            sp: self.cpu.sp,
            cycles: self.cpu.cycles,
            instr: self.cpu.fetch(&self.memory),
            memory: &self.memory,
        }
    }

    pub fn step(&mut self) {
        self.cpu.step(&mut self.memory);
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
    pub memory: &'a Memory,
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
