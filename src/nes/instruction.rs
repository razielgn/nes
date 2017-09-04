use self::AddressingMode::*;
use self::Label::*;
use Cpu;

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
    pub args: (u8, u8),
    pub read: u8,
    pub size: u8,
    pub cycles: u8,
    pub page_cycles: u8,
    pub page_crossed: bool,
}

impl Instruction {
    pub fn bytecode(&self, cpu: &Cpu) -> String {
        let args = match self.mode {
            Absolute => {
                format!("{:02X} {:02X}", self.addr as u8, self.addr >> 8 as u8)
            }
            Indirect | AbsoluteX | AbsoluteY => {
                format!("{:02X} {:02X}", self.args.0, self.args.1)
            }
            ZeroPageX | ZeroPageY | IndexedIndirect | IndirectIndexed => {
                format!("{:02X}", self.args.0)
            }
            Immediate => format!("{:02X}", self.read),
            Implied | Accumulator => "".into(),
            Relative => format!(
                "{:02X}",
                self.addr.wrapping_sub(cpu.pc).wrapping_sub(2) as u8
            ),
            ZeroPage => format!("{:02X}", self.addr as u8),
        };

        format!("{:02X} {:6}", self.op, args)
    }

    pub fn to_string(&self, cpu: &Cpu) -> String {
        let args = match self.mode {
            Absolute => match self.label {
                JMP | JSR => format!("${:04X}", self.addr),
                _ => format!("${:04X} = {:02X}", self.addr, self.read),
            },
            AbsoluteX => format!(
                "${:04X},X @ {:04X} = {:02X}",
                self.args_u16(),
                self.addr,
                self.read
            ),
            AbsoluteY => format!(
                "${:04X},Y @ {:04X} = {:02X}",
                self.args_u16(),
                self.addr,
                self.read
            ),
            Immediate => format!("#${:02X}", self.read),
            ZeroPage => format!("${:02X} = {:02X}", self.addr, self.read),
            ZeroPageX => {
                let addr = self.args.0;
                format!(
                    "${:02X},X @ {:02X} = {:02X}",
                    addr,
                    addr.wrapping_add(cpu.x),
                    self.read
                )
            }
            ZeroPageY => {
                let addr = self.args.0;
                format!(
                    "${:02X},Y @ {:02X} = {:02X}",
                    addr,
                    addr.wrapping_add(cpu.y),
                    self.read
                )
            }
            Relative => format!("${:04X}", self.addr),
            Accumulator => "A".into(),
            Indirect => format!("(${:04X}) = {:04X}", self.args_u16(), self.addr),
            IndexedIndirect => {
                let param = self.args.0;
                let indir = param.wrapping_add(cpu.x);
                format!(
                    "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    param,
                    indir,
                    self.addr,
                    self.read
                )
            }
            IndirectIndexed => {
                let param = self.args.0;
                let indir = self.addr.wrapping_sub(cpu.y as u16);
                format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    param,
                    indir,
                    self.addr,
                    self.read
                )
            }
            Implied => "".into(),
        };

        format!("{:?} {}", self.label, args)
    }

    fn args_u16(&self) -> u16 {
        (self.args.1 as u16) << 8 | self.args.0 as u16
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
