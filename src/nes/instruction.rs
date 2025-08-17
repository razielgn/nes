use self::{AddressingMode::*, Label::*};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum AddressingMode {
    None,
    Implied,
    Accumulator,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Relative,
    Absolute,
    AbsoluteX(bool),
    AbsoluteY(bool),
    Indirect,
    IndexedIndirect,
    IndirectIndexed(bool),
}

#[rustfmt::skip]
static OP_MODES: [AddressingMode; 256] = [
//  0          1                       2          3                       4          5          6          7          8        9                 A            B                 C                 D                 E                 F
    Implied,   IndexedIndirect,        None,      IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Accumulator, Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // 0
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageX, ZeroPageX, Implied, AbsoluteY(false), Implied,     AbsoluteY(true),  AbsoluteX(false), AbsoluteX(false), AbsoluteX(true),  AbsoluteX(true),  // 1
    Absolute,  IndexedIndirect,        None,      IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Accumulator, Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // 2
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageX, ZeroPageX, Implied, AbsoluteY(false), Implied,     AbsoluteY(true),  AbsoluteX(false), AbsoluteX(false), AbsoluteX(true),  AbsoluteX(true),  // 3
    Implied,   IndexedIndirect,        None,      IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Accumulator, Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // 4
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageX, ZeroPageX, Implied, AbsoluteY(false), Implied,     AbsoluteY(true),  AbsoluteX(false), AbsoluteX(false), AbsoluteX(true),  AbsoluteX(true),  // 5
    Implied,   IndexedIndirect,        None,      IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Accumulator, Immediate,        Indirect,         Absolute,         Absolute,         Absolute,         // 6
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageX, ZeroPageX, Implied, AbsoluteY(false), Implied,     AbsoluteY(true),  AbsoluteX(false), AbsoluteX(false), AbsoluteX(true),  AbsoluteX(true),  // 7
    Immediate, IndexedIndirect,        Immediate, IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Implied,     Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // 8
    Relative,  IndirectIndexed(true),  None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageY, ZeroPageY, Implied, AbsoluteY(true),  Implied,     AbsoluteY(true),  AbsoluteX(true),  AbsoluteX(true),  AbsoluteY(true),  AbsoluteY(true),  // 9
    Immediate, IndexedIndirect,        Immediate, IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Implied,     Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // A
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(false), ZeroPageX, ZeroPageX, ZeroPageY, ZeroPageY, Implied, AbsoluteY(false), Implied,     AbsoluteY(false), AbsoluteX(false), AbsoluteX(false), AbsoluteY(false), AbsoluteY(false), // B
    Immediate, IndexedIndirect,        Immediate, IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Implied,     Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // C
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageX, ZeroPageX, Implied, AbsoluteY(false), Implied,     AbsoluteY(true),  AbsoluteX(false), AbsoluteX(false), AbsoluteX(true),  AbsoluteX(true),  // D
    Immediate, IndexedIndirect,        Immediate, IndexedIndirect,        ZeroPage,  ZeroPage,  ZeroPage,  ZeroPage,  Implied, Immediate,        Implied,     Immediate,        Absolute,         Absolute,         Absolute,         Absolute,         // E
    Relative,  IndirectIndexed(false), None,      IndirectIndexed(true),  ZeroPageX, ZeroPageX, ZeroPageX, ZeroPageX, Implied, AbsoluteY(false), Implied,     AbsoluteY(true),  AbsoluteX(false), AbsoluteX(false), AbsoluteX(true),  AbsoluteX(true),  // F
];

impl From<u8> for AddressingMode {
    fn from(b: u8) -> Self {
        OP_MODES[b as usize]
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
#[allow(non_snake_case, clippy::upper_case_acronyms)]
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

#[rustfmt::skip]
static OP_CODES: [Label; 256] = [
//  0    1    2    3    4    5    6    7    8    9    A    B    C    D    E    F
    BRK, ORA, KIL, SLO, NOP, ORA, ASL, SLO, PHP, ORA, ASL, ANC, NOP, ORA, ASL, SLO, // 0
    BPL, ORA, KIL, SLO, NOP, ORA, ASL, SLO, CLC, ORA, NOP, SLO, NOP, ORA, ASL, SLO, // 1
    JSR, AND, KIL, RLA, BIT, AND, ROL, RLA, PLP, AND, ROL, ANC, BIT, AND, ROL, RLA, // 2
    BMI, AND, KIL, RLA, NOP, AND, ROL, RLA, SEC, AND, NOP, RLA, NOP, AND, ROL, RLA, // 3
    RTI, EOR, KIL, SRE, NOP, EOR, LSR, SRE, PHA, EOR, LSR, ALR, JMP, EOR, LSR, SRE, // 4
    BVC, EOR, KIL, SRE, NOP, EOR, LSR, SRE, CLI, EOR, NOP, SRE, NOP, EOR, LSR, SRE, // 5
    RTS, ADC, KIL, RRA, NOP, ADC, ROR, RRA, PLA, ADC, ROR, ARR, JMP, ADC, ROR, RRA, // 6
    BVS, ADC, KIL, RRA, NOP, ADC, ROR, RRA, SEI, ADC, NOP, RRA, NOP, ADC, ROR, RRA, // 7
    NOP, STA, NOP, SAX, STY, STA, STX, SAX, DEY, NOP, TXA, XAA, STY, STA, STX, SAX, // 8
    BCC, STA, KIL, AHX, STY, STA, STX, SAX, TYA, STA, TXS, TAS, SHY, STA, SHX, AHX, // 9
    LDY, LDA, LDX, LAX, LDY, LDA, LDX, LAX, TAY, LDA, TAX, LAX, LDY, LDA, LDX, LAX, // A
    BCS, LDA, KIL, LAX, LDY, LDA, LDX, LAX, CLV, LDA, TSX, LAS, LDY, LDA, LDX, LAX, // B
    CPY, CMP, NOP, DCP, CPY, CMP, DEC, DCP, INY, CMP, DEX, AXS, CPY, CMP, DEC, DCP, // C
    BNE, CMP, KIL, DCP, NOP, CMP, DEC, DCP, CLD, CMP, NOP, DCP, NOP, CMP, DEC, DCP, // D
    CPX, SBC, NOP, ISB, CPX, SBC, INC, ISB, INX, SBC, NOP, SBC, CPX, SBC, INC, ISB, // E
    BEQ, SBC, KIL, ISB, NOP, SBC, INC, ISB, SED, SBC, NOP, ISB, NOP, SBC, INC, ISB, // F
];

impl From<u8> for Label {
    fn from(b: u8) -> Self {
        OP_CODES[b as usize]
    }
}
