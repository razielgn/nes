use self::AddressingMode::*;
use self::Label::*;
use cpu::CpuState;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum AddressingMode {
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndexedIndirect,
    IndirectIndexed,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Instruction {
    pub mode: AddressingMode,
    pub label: Label,
    pub op: u8,
    pub addr: u16,
    pub size: u8,
    pub cycles: u8,
    pub page_cycles: u8,
    pub page_crossed: bool,
}

impl Instruction {
    pub fn bytecode(&self, s: &CpuState) -> String {
        let args = match self.mode {
            Absolute => {
                format!("{:02X} {:02X}", self.addr as u8, self.addr >> 8 as u8)
            }
            Indirect | AbsoluteX | AbsoluteY => {
                let param = s.mem.fetch_double(s.pc + 1);
                format!("{:02X} {:02X}", param as u8, param >> 8 as u8)
            }
            ZeroPageX | ZeroPageY => {
                let param = s.mem.fetch(s.pc + 1);
                format!("{:02X}", param)
            }
            Immediate => format!("{:02X}", s.mem.fetch(self.addr)),
            Implied | Accumulator => "".into(),
            Relative => {
                format!("{:02X}",
                        self.addr.wrapping_sub(s.pc).wrapping_sub(2) as u8)
            }
            IndexedIndirect | IndirectIndexed => {
                let param = s.mem.fetch(s.pc + 1);
                format!("{:02X}", param)
            }
            _ => format!("{:02X}", self.addr as u8),
        };

        format!("{:02X} {:6}", self.op, args)
    }

    pub fn to_string(&self, s: &CpuState) -> String {
        let args = match self.mode {
            Absolute => {
                match self.label {
                    JMP | JSR => format!("${:04X}", self.addr),
                    _ => {
                        format!("${:04X} = {:02X}",
                                self.addr,
                                s.mem.fetch(self.addr))
                    }
                }
            }
            AbsoluteX => {
                let param = s.mem.fetch_double(s.pc + 1);
                format!("${:04X},X @ {:04X} = {:02X}",
                        param,
                        self.addr,
                        s.mem.fetch(self.addr))
            }
            AbsoluteY => {
                let param = s.mem.fetch_double(s.pc + 1);
                format!("${:04X},Y @ {:04X} = {:02X}",
                        param,
                        self.addr,
                        s.mem.fetch(self.addr))
            }
            Immediate => format!("#${:02X}", s.mem.fetch(self.addr)),
            ZeroPage => {
                format!("${:02X} = {:02X}", self.addr, s.mem.fetch(self.addr))
            }
            ZeroPageX => {
                let addr = s.mem.fetch(s.pc + 1);
                format!("${:02X},X @ {:02X} = {:02X}",
                        addr,
                        addr.wrapping_add(s.x),
                        s.mem.fetch(self.addr))
            }
            ZeroPageY => {
                let addr = s.mem.fetch(s.pc + 1);
                format!("${:02X},Y @ {:02X} = {:02X}",
                        addr,
                        addr.wrapping_add(s.y),
                        s.mem.fetch(self.addr))
            }
            Relative => format!("${:04X}", self.addr),
            Accumulator => "A".into(),
            Indirect => {
                let param = s.mem.fetch_double(s.pc + 1);
                let value = s.mem.fetch_double_bug(param);
                format!("(${:04X}) = {:04X}", param, value)
            }
            IndexedIndirect => {
                let param = s.mem.fetch(s.pc + 1);
                let indir = param.wrapping_add(s.x);
                let val = s.mem.fetch_double_bug(self.addr);
                format!("(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                        param,
                        indir,
                        self.addr,
                        val)
            }
            IndirectIndexed => {
                let param = s.mem.fetch(s.pc + 1);
                let indir = s.mem.fetch_double_bug(param as u16);
                let val = s.mem.fetch(indir.wrapping_add(s.y as u16));
                format!("(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                        param,
                        indir,
                        self.addr,
                        val)
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
    AHX,
    ALR,
    ANC,
    AND,
    ARR,
    ASL,
    AXS,
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
    DCP,
    DEC,
    DEX,
    DEY,
    EOR,
    INC,
    INX,
    INY,
    ISB,
    JMP,
    JSR,
    KIL,
    LAS,
    LAX,
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
    RLA,
    ROL,
    ROR,
    RRA,
    RTI,
    RTS,
    SAX,
    SBC,
    SEC,
    SED,
    SEI,
    SHX,
    SHY,
    SLO,
    SRE,
    STA,
    STX,
    STY,
    TAS,
    TAX,
    TAY,
    TSX,
    TXA,
    TXS,
    TYA,
    XAA,
}
