#![cfg_attr(feature = "cargo-clippy", allow(new_without_default))]

#[macro_use]
extern crate nom;
#[macro_use]
extern crate log;

mod bits;
mod cpu;
mod instruction;
mod mapper;
mod memory;
mod ppu;
mod rom;

pub use cpu::Cpu;
use instruction::Instruction;
use mapper::{Mapper, SharedMapper};
pub use memory::{MutMemory, MutMemoryAccess, Ram};
use ppu::Ppu;
pub use rom::Rom;
use std::fmt;
use std::path::Path;

pub struct Nes {
    cpu: Cpu,
    mapper: SharedMapper,
    ppu: Ppu,
    ram: Ram,
}

impl Nes {
    pub fn from_rom<P: AsRef<Path>>(path: P) -> Self {
        let rom = Rom::from_path(path);
        let mut mapper = Mapper::new(rom);
        let pc = mapper.read_double(0xFFFC);

        Nes {
            cpu: Cpu::new(pc),
            mapper: mapper.into_shared(),
            ppu: Ppu::new(),
            ram: Ram::new(),
        }
    }

    pub fn step(&mut self) -> CpuState {
        debug!("step");

        let last_cpu = self.cpu;

        let (cycles, instr) = {
            let mut mmap = MutMemory {
                ram: &mut self.ram,
                mapper: self.mapper.clone(),
                ppu: &mut self.ppu,
            };

            self.cpu.step(&mut mmap)
        };

        for _ in 0..cycles * 3 {
            let nmi_triggered = self.ppu.step();

            if nmi_triggered {
                self.cpu.trigger_nmi();
            }
        }

        CpuState {
            instr,
            cpu: last_cpu,
        }
    }

    pub fn set_pc(&mut self, pc: u16) {
        self.cpu.jump(pc);
    }
}

impl MutMemoryAccess for Nes {
    fn read(&mut self, addr: u16) -> u8 {
        let mut mmap = MutMemory {
            ram: &mut self.ram,
            mapper: self.mapper.clone(),
            ppu: &mut self.ppu,
        };

        mmap.read(addr)
    }
}

pub struct CpuState {
    pub instr: Instruction,
    pub cpu: Cpu,
}

impl fmt::Display for CpuState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let p: u8 = self.cpu.p.into();

        write!(
            f,
            "{pc:04X}  {bytecode:9} {instr:31} A:{a:02X} X:{x:02X} \
             Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
            pc = self.cpu.pc,
            bytecode = self.instr.bytecode(&self.cpu),
            instr = self.instr.to_string(&self.cpu),
            a = self.cpu.a,
            x = self.cpu.x,
            y = self.cpu.y,
            p = p,
            sp = self.cpu.sp,
            cyc = (self.cpu.cycles * 3) % 341
        )
    }
}
