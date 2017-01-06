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
            mem: &self.memory,
        }
    }

    pub fn step(&mut self) {
        use self::AddressingMode::*;
        use self::Label::*;
        use self::Status::*;

        let instr = self.fetch();

        match (instr.label, instr.mode) {
            (JMP, Absolute(addr)) => {
                self.pc = addr;
                self.cycles += 3;
            }
            (JSR, Absolute(addr)) => {
                let ret = self.pc + 2;
                self.push_double(ret);

                self.pc = addr;
                self.cycles += 6;
            }
            (RTS, Implied) => {
                self.pc = self.pop_double() + 1;
                self.cycles += 6;
            }
            (RTI, Implied) => {
                self.pop_p();

                self.pc = self.pop_double();
                self.cycles += 6;
            }
            (PHP, Implied) => {
                let mut p = self.p;
                p.set(BreakCommand);
                self.push(p.into());

                self.pc += 1;
                self.cycles += 3;
            }
            (PHA, Implied) => {
                let a = self.a;
                self.push(a);

                self.pc += 1;
                self.cycles += 3;
            }
            (PLA, Implied) => {
                self.a = self.pop();
                self.p.set_if_zn(self.a);

                self.pc += 1;
                self.cycles += 4;
            }
            (PLP, Implied) => {
                self.pop_p();

                self.pc += 1;
                self.cycles += 4;
            }
            (LDA, mode) => {
                self.a = match mode {
                    Immediate(m) => {
                        self.pc += 2;
                        self.cycles += 2;

                        m
                    }
                    Absolute(addr) => {
                        self.pc += 3;
                        self.cycles += 4;

                        self.memory.fetch(addr)
                    }
                    _ => unimplemented!(),
                };
                self.p.set_if_zn(self.a);
            }
            (LDX, mode) => {
                self.x = match mode {
                    Immediate(m) => {
                        self.pc += 2;
                        self.cycles += 2;

                        m
                    }
                    Absolute(addr) => {
                        self.pc += 3;
                        self.cycles += 4;

                        self.memory.fetch(addr)
                    }
                    _ => unimplemented!(),
                };
                self.p.set_if_zn(self.x);
            }
            (LDY, mode) => {
                self.y = match mode {
                    Immediate(m) => {
                        self.pc += 2;
                        self.cycles += 2;

                        m
                    }
                    Absolute(addr) => {
                        self.pc += 3;
                        self.cycles += 4;

                        self.memory.fetch(addr)
                    }
                    _ => unimplemented!(),
                };
                self.p.set_if_zn(self.y);
            }
            (STX, mode) => {
                let addr = match mode {
                    ZeroPage(addr) => {
                        self.pc += 2;
                        self.cycles += 3;

                        addr.into()
                    }
                    Absolute(addr) => {
                        self.pc += 3;
                        self.cycles += 4;

                        addr
                    }
                    _ => unimplemented!(),
                };

                self.memory.store(addr, self.x);
            }
            (STA, ZeroPage(addr)) => {
                self.memory.store(addr, self.a);
                self.pc += 2;
                self.cycles += 3;
            }
            (NOP, Implied) => {
                self.pc += 1;
                self.cycles += 2;
            }
            (SEC, Implied) => {
                self.p.set(CarryFlag);
                self.pc += 1;
                self.cycles += 2;
            }
            (SEI, Implied) => {
                self.p.set(InterruptDisable);
                self.pc += 1;
                self.cycles += 2;
            }
            (SED, Implied) => {
                self.p.set(DecimalMode);
                self.pc += 1;
                self.cycles += 2;
            }
            (CLC, Implied) => {
                self.p.unset(CarryFlag);
                self.pc += 1;
                self.cycles += 2;
            }
            (CLD, Implied) => {
                self.p.unset(DecimalMode);
                self.pc += 1;
                self.cycles += 2;
            }
            (CLV, Implied) => {
                self.p.unset(OverflowFlag);
                self.pc += 1;
                self.cycles += 2;
            }
            (BCS, Relative(offset)) => {
                let cond = self.p.is_set(CarryFlag);
                self.branch_if(cond, offset);
            }
            (BCC, Relative(offset)) => {
                let cond = !self.p.is_set(CarryFlag);
                self.branch_if(cond, offset);
            }
            (BEQ, Relative(offset)) => {
                let cond = self.p.is_set(ZeroFlag);
                self.branch_if(cond, offset);
            }
            (BNE, Relative(offset)) => {
                let cond = !self.p.is_set(ZeroFlag);
                self.branch_if(cond, offset);
            }
            (BVS, Relative(offset)) => {
                let cond = self.p.is_set(OverflowFlag);
                self.branch_if(cond, offset);
            }
            (BVC, Relative(offset)) => {
                let cond = !self.p.is_set(OverflowFlag);
                self.branch_if(cond, offset);
            }
            (BPL, Relative(offset)) => {
                let cond = !self.p.is_set(NegativeFlag);
                self.branch_if(cond, offset);
            }
            (BMI, Relative(offset)) => {
                let cond = self.p.is_set(NegativeFlag);
                self.branch_if(cond, offset);
            }
            (BIT, ZeroPage(addr)) => {
                let m = self.memory.fetch(addr);
                let res = self.a & m;

                self.p.set_if_zero(res);
                self.p.set_if_negative(m);
                self.p.set_if(OverflowFlag, (m >> 6) & 1 == 1);

                self.pc += 2;
                self.cycles += 3;
            }
            (AND, Immediate(m)) => {
                self.a &= m;

                self.p.set_if_zn(self.a);

                self.pc += 2;
                self.cycles += 2;
            }
            (ORA, Immediate(m)) => {
                self.a |= m;

                self.p.set_if_zn(self.a);

                self.pc += 2;
                self.cycles += 2;
            }
            (EOR, Immediate(m)) => {
                self.a ^= m;

                self.p.set_if_zn(self.a);

                self.pc += 2;
                self.cycles += 2;
            }
            (CMP, Immediate(m)) => {
                let n = self.a.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.a >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if(ZeroFlag, n == 0);

                self.pc += 2;
                self.cycles += 2;
            }
            (CPY, Immediate(m)) => {
                let n = self.y.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.y >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if(ZeroFlag, n == 0);

                self.pc += 2;
                self.cycles += 2;
            }
            (CPX, Immediate(m)) => {
                let n = self.x.wrapping_sub(m);
                self.p.set_if(CarryFlag, self.x >= m);
                self.p.set_if(NegativeFlag, (n as i8) < 0);
                self.p.set_if(ZeroFlag, n == 0);

                self.pc += 2;
                self.cycles += 2;
            }
            (ADC, Immediate(m)) => {
                let c = if self.p.is_set(CarryFlag) { 1 } else { 0 };
                let (sum, overflow1) = self.a.overflowing_add(m);
                let (sum, overflow2) = sum.overflowing_add(c);
                let overflow = overflow1 || overflow2;

                self.p.set_if(CarryFlag, overflow);
                self.p.set_if(OverflowFlag,
                              (self.a ^ sum) & 0x80 != 0 &&
                              (self.a ^ m) & 0x80 == 0);
                self.p.set_if_zn(sum);

                self.a = sum;
                self.pc += 2;
                self.cycles += 2;
            }
            (SBC, Immediate(m)) => {
                let c = if self.p.is_set(CarryFlag) { 0 } else { 1 };
                let (sub, overflow1) = self.a.overflowing_sub(m);
                let (sub, overflow2) = sub.overflowing_sub(c);
                let overflow = overflow1 || overflow2;

                self.p.unset_if(CarryFlag, overflow);
                self.p.set_if(OverflowFlag,
                              (self.a ^ sub) & 0x80 != 0 &&
                              (self.a ^ m) & 0x80 != 0);
                self.p.set_if_zn(sub);

                self.a = sub;
                self.pc += 2;
                self.cycles += 2;
            }
            (INY, Implied) => {
                self.y = self.y.wrapping_add(1);
                self.p.set_if_zn(self.y);

                self.pc += 1;
                self.cycles += 2;
            }
            (DEY, Implied) => {
                self.y = self.y.wrapping_sub(1);
                self.p.set_if_zn(self.y);

                self.pc += 1;
                self.cycles += 2;
            }
            (INX, Implied) => {
                self.x = self.x.wrapping_add(1);
                self.p.set_if_zn(self.x);

                self.pc += 1;
                self.cycles += 2;
            }
            (DEX, Implied) => {
                self.x = self.x.wrapping_sub(1);
                self.p.set_if_zn(self.x);

                self.pc += 1;
                self.cycles += 2;
            }
            (TAY, Implied) => {
                self.y = self.a;
                self.p.set_if_zn(self.y);

                self.pc += 1;
                self.cycles += 2;
            }
            (TAX, Implied) => {
                self.x = self.a;
                self.p.set_if_zn(self.x);

                self.pc += 1;
                self.cycles += 2;
            }
            (TYA, Implied) => {
                self.a = self.y;
                self.p.set_if_zn(self.a);

                self.pc += 1;
                self.cycles += 2;
            }
            (TXA, Implied) => {
                self.a = self.x;
                self.p.set_if_zn(self.a);

                self.pc += 1;
                self.cycles += 2;
            }
            (TXS, Implied) => {
                self.sp = self.x;

                self.pc += 1;
                self.cycles += 2;
            }
            (TSX, Implied) => {
                self.x = self.sp;
                self.p.set_if_zn(self.x);

                self.pc += 1;
                self.cycles += 2;
            }
            _ => panic!("can't execute {:?}", instr),
        }
    }

    fn branch_if(&mut self, cond: bool, offset: i8) {
        if cond {
            self.apply_offset_to_pc(offset);
            self.cycles += 1;
        }

        self.pc += 2;
        self.cycles += 2;
    }

    fn fetch(&self) -> Instruction {
        use self::AddressingMode::*;
        use self::Label::*;

        let op = self.memory.fetch(self.pc);
        let (label, mode) = match op {
            0x08 => (PHP, Implied),
            0x09 => (ORA, self.immediate()),
            0x10 => (BPL, self.relative()),
            0x18 => (CLC, Implied),
            0x20 => (JSR, self.absolute()),
            0x24 => (BIT, self.zero_page()),
            0x28 => (PLP, Implied),
            0x29 => (AND, self.immediate()),
            0x30 => (BMI, self.relative()),
            0x35 => (AND, self.zero_page_x()),
            0x38 => (SEC, Implied),
            0x40 => (RTI, Implied),
            0x48 => (PHA, Implied),
            0x49 => (EOR, self.immediate()),
            0x4C => (JMP, self.absolute()),
            0x50 => (BVC, self.relative()),
            0x60 => (RTS, Implied),
            0x68 => (PLA, Implied),
            0x69 => (ADC, self.immediate()),
            0x70 => (BVS, self.relative()),
            0x78 => (SEI, Implied),
            0x85 => (STA, self.zero_page()),
            0x86 => (STX, self.zero_page()),
            0x88 => (DEY, Implied),
            0x8A => (TXA, Implied),
            0x8E => (STX, self.absolute()),
            0x90 => (BCC, self.relative()),
            0x98 => (TYA, Implied),
            0x9A => (TXS, Implied),
            0xA0 => (LDY, self.immediate()),
            0xA2 => (LDX, self.immediate()),
            0xA8 => (TAY, Implied),
            0xA9 => (LDA, self.immediate()),
            0xAA => (TAX, Implied),
            0xAD => (LDA, self.absolute()),
            0xAE => (LDX, self.absolute()),
            0xB0 => (BCS, self.relative()),
            0xB8 => (CLV, Implied),
            0xBA => (TSX, Implied),
            0xC0 => (CPY, self.immediate()),
            0xC8 => (INY, Implied),
            0xC9 => (CMP, self.immediate()),
            0xCA => (DEX, Implied),
            0xD0 => (BNE, self.relative()),
            0xD8 => (CLD, Implied),
            0xE0 => (CPX, self.immediate()),
            0xE8 => (INX, Implied),
            0xE9 => (SBC, self.immediate()),
            0xEA => (NOP, Implied),
            0xF0 => (BEQ, self.relative()),
            0xF8 => (SED, Implied),
            op => panic!("unknown opcode 0x{:X} at addr {:04X}", op, self.pc),
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

    fn push_double(&mut self, val: u16) {
        let hi = (val >> 8) as u8;
        let lo = val as u8;

        self.push(hi);
        self.push(lo);
    }

    fn pop(&mut self) -> u8 {
        self.sp += 1;
        self.memory.fetch(0x100 + self.sp as u16)
    }

    fn pop_double(&mut self) -> u16 {
        let lo = self.pop() as u16;
        let hi = self.pop() as u16;

        hi << 8 | lo
    }

    fn pop_p(&mut self) {
        self.p = self.pop().into();
        self.p.unset(Status::BreakCommand);
        self.p.set(Status::UnusedFlag);
    }

    fn apply_offset_to_pc(&mut self, offset: i8) {
        self.pc = if offset < 0 {
            self.pc - offset as u16
        } else {
            self.pc + offset as u16
        };
    }

    fn absolute(&self) -> AddressingMode {
        AddressingMode::Absolute(self.memory.fetch_double(self.pc + 1))
    }

    fn zero_page(&self) -> AddressingMode {
        AddressingMode::ZeroPage(self.memory.fetch(self.pc + 1))
    }

    fn zero_page_x(&self) -> AddressingMode {
        AddressingMode::ZeroPageX(self.memory.fetch(self.pc + 1) + self.x)
    }

    fn relative(&self) -> AddressingMode {
        AddressingMode::Relative(self.memory.fetch(self.pc + 1) as i8)
    }

    fn immediate(&self) -> AddressingMode {
        AddressingMode::Immediate(self.memory.fetch(self.pc + 1))
    }
}

#[allow(dead_code)]
enum Status {
    CarryFlag = 0,
    ZeroFlag = 1,
    InterruptDisable = 2,
    DecimalMode = 3,
    BreakCommand = 4,
    UnusedFlag = 5,
    OverflowFlag = 6,
    NegativeFlag = 7,
}

#[derive(Debug, PartialEq, Clone, Copy)]
struct P(u8);

impl P {
    fn new() -> Self {
        P(0x24)
    }

    fn set_if_zn(&mut self, val: u8) {
        self.set_if_zero(val);
        self.set_if_negative(val);
    }

    fn set_if_zero(&mut self, val: u8) {
        self.set_if(Status::ZeroFlag, val == 0)
    }

    fn set_if_negative(&mut self, val: u8) {
        self.set_if(Status::NegativeFlag, (val as i8) < 0)
    }

    fn set_if(&mut self, s: Status, v: bool) {
        if v {
            self.set(s);
        } else {
            self.unset(s);
        }
    }

    fn unset_if(&mut self, s: Status, v: bool) {
        self.set_if(s, !v)
    }

    fn set(&mut self, s: Status) {
        self.0 |= 1 << (s as u8);
    }

    fn unset(&mut self, s: Status) {
        self.0 &= !(1 << (s as u8));
    }

    fn is_set(&self, s: Status) -> bool {
        (self.0 >> (s as u8)) & 1 == 1
    }
}

impl From<u8> for P {
    fn from(p: u8) -> Self {
        P(p)
    }
}

impl Into<u8> for P {
    fn into(self) -> u8 {
        self.0
    }
}

impl fmt::UpperHex for P {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:02X}", self.0)
    }
}

//#[derive(Debug, PartialEq)]
pub struct CpuState<'mem, 'rom: 'mem> {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    p: P,
    sp: u8,
    cycles: usize,
    instr: Instruction,
    mem: &'mem Memory<'rom>,
}

impl<'a, 'b> fmt::Display for CpuState<'a, 'b> {
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
               p = self.p,
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

    pub fn fetch<A>(&self, addr: A) -> u8
        where A: Into<u16>
    {
        let addr = addr.into() as usize;

        if addr < 0x2000 {
            self.ram[addr]
        } else {
            self.rom.prg[addr - 0xc000]
        }
    }

    pub fn fetch_double(&self, addr: u16) -> u16 {
        ((self.fetch(addr + 1) as u16) << 8) + self.fetch(addr) as u16
    }

    pub fn store<A, V>(&mut self, addr: A, val: V)
        where A: Into<u16>,
              V: Into<u8>
    {
        let addr = addr.into() as usize;
        let val = val.into();

        if addr < 0x2000 {
            self.ram[addr] = val;
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
    Implied,
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

        let args = match (self.label, self.mode) {
            (STX, Absolute(addr)) |
            (LDX, Absolute(addr)) |
            (LDA, Absolute(addr)) => {
                format!("${:04X} = {:02X}", addr, s.mem.fetch(addr))
            }
            (_, Absolute(addr)) => format!("${:04X}", addr),
            (_, Immediate(val)) => format!("#${:02X}", val),
            (_, ZeroPage(addr)) => {
                format!("${:02X} = {:02X}", addr, s.mem.fetch(addr as u16))
            }
            (_, Relative(offset)) => {
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
    use std::io::{BufRead, BufReader, Read};
    use std::path::Path;
    use super::{Cpu, Status};

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
            {
                let state = cpu.state();
                let as_str = format!("{}", state);
                println!("{}", as_str);
                assert_eq!(expected_state.unwrap(), as_str);
            }

            cpu.step();
        }
    }

    #[test]
    fn bit_ops_on_p() {
        let mut p = super::P::new();

        p.set(Status::CarryFlag);
        assert!(p.is_set(Status::CarryFlag));

        p.unset(Status::CarryFlag);
        assert!(!p.is_set(Status::CarryFlag));
    }
}
