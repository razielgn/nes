use rom::Rom;
use std::fmt;

pub struct Cpu<'rom> {
    cycles: usize,
    pc: u16,
    sp: u8,
    p: P,
    a: u8,
    x: u8,
    y: u8,

    /// 0x100   => Zero Page
    /// 0x200   => Stack
    /// 0x800   => RAM
    /// 0x2000  => Mirrors (0-0x7FF)
    /// 0x2008  => I/O Registers
    /// 0x4000  => Mirrors (0x2000-0x2007)
    /// 0x4020  => I/O Registers
    /// 0x6000  => Expansion ROM
    /// 0x8000  => SRAM
    /// 0xC000  => PRG-ROM (Lower Bank)
    /// 0x10000 => PRG-ROM (Upper Bank)
    memory: Memory<'rom>,
}

impl<'rom> Cpu<'rom> {
    pub fn new(rom: Rom<'rom>) -> Self {
        Cpu {
            cycles: 0,
            pc: 0xc000,
            sp: 0xfd,
            p: P::new(),
            a: 0,
            x: 0,
            y: 0,
            memory: Memory::new(rom),
        }
    }

    pub fn state(&self) -> CpuState {
        CpuState {
            pc: self.pc,
            a: self.a,
            x: self.x,
            y: self.y,
            p: self.p,
            sp: self.sp,
            cycles: self.cycles,
            instr: self.fetch(),
        }
    }

    pub fn step(&mut self) {
        use self::AddressingMode::*;
        use self::Label::*;

        let instr = self.fetch();

        match (instr.label, instr.mode) {
            (JMP, Absolute(addr)) => {
                self.pc = addr;
                self.cycles += 3;
            }
            (JSR, Absolute(addr)) => {
                let ret = self.pc + 3;

                let hi = (ret >> 8) as u8;
                self.push(hi);
                let low = ret as u8;
                self.push(low);

                self.pc = addr;
                self.cycles += 6;
            }
            (LDX, Immediate(v)) => {
                let val = self.memory.fetch(v as u16);
                self.p.set_zero(val);
                self.p.set_negative(val);

                self.x = val;
                self.pc += 2;
                self.cycles += 2;
            }
            (STX, ZeroPage(v)) => {
                self.memory.store(v as u16, self.x);
                self.pc += 2;
                self.cycles += 3;
            }
            (NOP, Implicit) => {
                self.pc += 1;
                self.cycles += 2;
            }
            (SEC, Implicit) => {
                self.p.set_carry(true);
                self.pc += 1;
                self.cycles += 2;
            }
            (CLC, Implicit) => {
                self.p.set_carry(false);
                self.pc += 1;
                self.cycles += 2;
            }
            (BCS, Relative(offset)) => {
                if self.p.is_carry() {
                    self.apply_offset_to_pc(offset);
                    self.cycles += 1;
                }

                self.pc += 2;
                self.cycles += 2;
            }
            (BCC, Relative(offset)) => {
                if !self.p.is_carry() {
                    self.apply_offset_to_pc(offset);
                    self.cycles += 1;
                }

                self.pc += 2;
                self.cycles += 2;
            }
            _ => panic!("can't execute {:?}", instr),
        }
    }

    fn fetch(&self) -> Instruction {
        use self::AddressingMode::*;
        use self::Label::*;

        let next = self.pc + 1;
        let op = self.memory.fetch(self.pc);
        let (label, mode) = match op {
            0x18 => (CLC, Implicit),
            0x20 => (JSR, Absolute(self.memory.fetch_double(next))),
            0x38 => (SEC, Implicit),
            0x4c => (JMP, Absolute(self.memory.fetch_double(next))),
            0x86 => (STX, ZeroPage(self.memory.fetch(next))),
            0xa2 => (LDX, Immediate(self.memory.fetch(next))),
            0xb0 => (BCS, Relative(self.memory.fetch(next) as i8)),
            0xea => (NOP, Implicit),
            0x90 => (BCC, Relative(self.memory.fetch(next) as i8)),
            op => panic!("unknown opcode: 0x{:X}", op),
        };

        Instruction {
            mode: mode,
            label: label,
            op: op,
        }
    }

    fn push(&mut self, val: u8) {
        self.memory.store(0x100 + self.sp as u16, val);
        self.sp -= 1;
    }

    fn apply_offset_to_pc(&mut self, offset: i8) {
        self.pc = if offset < 0 {
            self.pc - offset as u16
        } else {
            self.pc + offset as u16
        };
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct P(pub u8);

impl P {
    pub fn new() -> Self {
        P(0x24)
    }

    fn set_zero(&mut self, val: u8) {
        if val == 0 {
            self.0 |= 1 << 1;
        }
    }

    fn set_negative(&mut self, val: u8) {
        if (val as i8) < 0 {
            self.0 |= 1 << 7;
        }
    }

    fn set_carry(&mut self, state: bool) {
        if state {
            self.0 |= 1;
        } else {
            self.0 &= !1;
        }
    }

    fn is_carry(&self) -> bool {
        self.0 & 1 == 1
    }
}

#[derive(Debug, PartialEq)]
pub struct CpuState {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    p: P,
    sp: u8,
    cycles: usize,
    instr: Instruction,
}

impl fmt::Display for CpuState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
               "{pc:04X}  {bytecode:9} {instr:31} A:{a:02X} X:{x:02X} \
                Y:{y:02X} P:{p:02X} SP:{sp:2X} CYC:{cyc:3?}",
               pc = self.pc,
               bytecode = self.instr.bytecode(),
               instr = self.instr.to_string(&self),
               a = self.a,
               x = self.x,
               y = self.y,
               p = self.p.0,
               sp = self.sp,
               cyc = (self.cycles * 3) % 341)
    }
}

struct Memory<'rom> {
    ram: [u8; 0x2000],
    rom: Rom<'rom>,
}

impl<'rom> Memory<'rom> {
    pub fn new(rom: Rom<'rom>) -> Self {
        Memory {
            ram: [0; 0x2000],
            rom: rom,
        }
    }

    pub fn fetch(&self, address: u16) -> u8 {
        if address < 0x2000 {
            self.ram[address as usize]
        } else {
            self.rom.prg[address as usize - 0xc000]
        }
    }

    pub fn fetch_double(&self, address: u16) -> u16 {
        ((self.fetch(address + 1) as u16) << 8) + self.fetch(address) as u16
    }

    pub fn store(&mut self, address: u16, value: u8) -> u8 {
        if address < 0x2000 {
            let old_val = self.ram[address as usize];
            self.ram[address as usize] = value;
            old_val
        } else {
            panic!("cannot write outside handled memory");
        }
    }
}

//#[derive(Debug, PartialEq)]
//#pub enum Interrupt {
//#IrqBrk,
//#Nmi,
//#Reset,
//

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(dead_code)]
pub enum AddressingMode {
    Implicit,
    Accumulator,
    Immediate(u8),
    ZeroPage(u8),
    ZeroPageX(u8),
    ZeroPageY(u8),
    Relative(i8),
    Absolute(u16),
    AbsoluteX(u16),
    AbsoluteY(u16),
    Indirect(u16),
    IndexedIndirect(u8),
    IndirectIndexed(u8),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Instruction {
    mode: AddressingMode,
    label: Label,
    op: u8,
}

impl Instruction {
    pub fn bytecode(&self) -> String {
        use self::AddressingMode::*;

        let args = match self.mode {
            Absolute(addr) => format!("{:02X} {:02X}", addr as u8, addr >> 8),
            Immediate(val) | ZeroPage(val) => format!("{:02X}", val),
            Relative(val) => format!("{:02X}", val as u8),
            _ => "".into(),
        };

        format!("{:02X} {:6}", self.op, args)
    }

    pub fn to_string(&self, s: &CpuState) -> String {
        use self::AddressingMode::*;
        use self::Label::*;

        let args = match (self.mode, self.label) {
            (Absolute(addr), _) => format!("${:04X}", addr),
            (Immediate(val), _) => format!("#${:02X}", val),
            (ZeroPage(addr), STX) => format!("${:02X} = {:02X}", addr, s.x),
            (Relative(offset), _) => {
                let addr = if offset < 0 {
                    s.pc - offset as u16
                } else {
                    s.pc + offset as u16
                };

                format!("${:04X}", addr + 2)
            }
            _ => "".into(),
        };

        format!("{:?} {}", self.label, args)
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(dead_code, non_snake_case)]
pub enum Label {
    ADC,
    AND,
    ASL,
    BCC,
    BCS,
    BEQ,
    BIT,
    BMI,
    BNE,
    BPL,
    BRK,
    BVC,
    BVS,
    CLC,
    CLD,
    CLI,
    CLV,
    CMP,
    CPX,
    CPY,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    JMP,
    JSR,
    LDA,
    LDX,
    LDY,
    LSR,
    NOP,
    ORA,
    PHA,
    PHP,
    PLA,
    PLP,
    ROL,
    ROR,
    RTI,
    RTS,
    SBC,
    SEC,
    SED,
    SEI,
    STA,
    STX,
    STY,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
}

#[cfg(test)]
mod test {
    use rom::Rom;
    use std::fs::File;
    use std::io::BufRead;
    use std::io::BufReader;
    use std::io::Read;
    use std::path::Path;
    use super::Cpu;

    #[test]
    fn nestest() {
        let buf = {
            let mut f = File::open(Path::new("test/nestest.nes")).unwrap();
            let mut buf = Vec::new();
            f.read_to_end(&mut buf).unwrap();
            buf
        };

        let (_, rom) = Rom::parse(buf.as_slice()).unwrap();

        let expected_states = {
            let f = File::open(Path::new("test/nestest.log")).unwrap();
            let reader = BufReader::new(f);
            reader.lines().collect::<Vec<_>>()
        };

        let mut cpu = Cpu::new(rom);

        println!("");
        for expected_state in expected_states {
            let state = cpu.state();
            let as_str = format!("{}", state);
            println!("{}", as_str);
            assert_eq!(expected_state.unwrap(), as_str);

            cpu.step();
        }
    }

    #[test]
    fn bit_ops_on_p() {
        let mut p = super::P::new();
        p.set_carry(true);
        assert!(p.is_carry());
    }
}
