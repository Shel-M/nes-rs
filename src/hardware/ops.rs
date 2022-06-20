use crate::hardware::cpu::AddressingMode;
use crate::hardware::cpu::AddressingMode::*;
use crate::hardware::ops::Instruction::*;

#[derive(Copy, Clone)]
#[allow(clippy::upper_case_acronyms)] // These being caps makes slightly more sense than usual here.
pub enum Instruction {
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

    NOTIMPLEMENTED,
}

pub struct Operation {
    #[allow(dead_code)]
    code: u8,
    pub instruction: Instruction,
    pub bytes: u8,
    pub cycles: u8,
    pub addr_mode: AddressingMode,
}

const UNIMPL_OP: Operation = Operation {
    code: 0x00,
    instruction: NOTIMPLEMENTED,
    bytes: 0,
    cycles: 0,
    addr_mode: NoneAddressing,
};

impl Operation {
    fn new(
        code: u8,
        instruction: Instruction,
        bytes: u8,
        cycles: u8,
        addr_mode: AddressingMode,
    ) -> Self {
        Operation {
            code,
            instruction,
            bytes,
            cycles,
            addr_mode,
        }
    }
}

pub struct Operations {
    pub ops: [Operation; 0xff],
}

impl Operations {
    pub fn new() -> Self {
        let mut ops: [Operation; 0xff] = [UNIMPL_OP; 0xff];
        macro_rules! op {
            (
                $instruction: expr,
                $addr_mode: expr,
                $code: literal,
                $bytes: literal,
                $cycles: literal
            ) => {
                ops[$code] = Operation::new($code, $instruction, $bytes, $cycles, $addr_mode);
            };
            (
            $instruction: ident,
            $addr_mode: expr,
            $code: literal,
            $bytes: literal,
            $cycles: literal
            ) => {
                ops[$code] = Operation::new($code, $instruction, $bytes, $cycles, $addr_mode);
            };
        }

        let inst = ADC;
        op!(inst, Immediate, 0x69, 2, 2);
        op!(inst, ZeroPage, 0x65, 2, 3);
        op!(inst, ZeroPageX, 0x75, 2, 3);
        op!(inst, Absolute, 0x6d, 3, 3);
        op!(inst, AbsoluteX, 0x7d, 3, 3);
        op!(inst, AbsoluteY, 0x79, 3, 3);
        op!(inst, IndirectX, 0x61, 2, 3);
        op!(inst, IndirectY, 0x71, 2, 3);

        let inst = AND;
        op!(inst, Immediate, 0x29, 2, 2);
        op!(inst, ZeroPage, 0x25, 2, 3);
        op!(inst, ZeroPageX, 0x35, 2, 4);
        op!(inst, Absolute, 0x2d, 3, 4);
        op!(inst, AbsoluteX, 0x3d, 3, 4);
        op!(inst, AbsoluteY, 0x39, 3, 4);
        op!(inst, IndirectX, 0x21, 2, 6);
        op!(inst, IndirectY, 0x31, 2, 5);

        let inst = ASL;
        op!(inst, NoneAddressing, 0x0a, 1, 2); // Actual addressing: Accumulator
        op!(inst, ZeroPage, 0x06, 2, 5);
        op!(inst, ZeroPageX, 0x16, 2, 6);
        op!(inst, Absolute, 0x0e, 3, 6);
        op!(inst, AbsoluteX, 0x1e, 3, 7);

        // Actual addressing for branch instructions: Relative
        op!(BCC, ZeroPage, 0x90, 2, 2);
        op!(BCS, ZeroPage, 0xb0, 2, 2);
        op!(BEQ, ZeroPage, 0xf0, 2, 2);

        let inst = BIT;
        op!(inst, ZeroPage, 0x24, 2, 3);
        op!(inst, Absolute, 0x2c, 3, 4);

        // Actual addressing for branch instructions: Relative
        op!(BMI, ZeroPage, 0x30, 2, 2);
        op!(BNE, ZeroPage, 0xd0, 2, 2);
        op!(BPL, ZeroPage, 0x10, 2, 2);

        op!(BRK, NoneAddressing, 0x00, 1, 7);

        // Actual addressing for branch instructions: Relative
        op!(BVC, ZeroPage, 0x50, 2, 2);
        op!(BVS, ZeroPage, 0x70, 2, 2);

        op!(CLC, NoneAddressing, 0x18, 1, 2);
        op!(CLD, NoneAddressing, 0xd8, 1, 2);
        op!(CLI, NoneAddressing, 0x58, 1, 2);
        op!(CLV, NoneAddressing, 0xb8, 1, 2);

        let inst = CMP;
        op!(inst, Immediate, 0xc9, 2, 2);
        op!(inst, ZeroPage, 0xc5, 2, 3);
        op!(inst, ZeroPageX, 0xd5, 2, 4);
        op!(inst, Absolute, 0xcd, 3, 4);
        op!(inst, AbsoluteX, 0xdd, 3, 4);
        op!(inst, AbsoluteY, 0xd9, 3, 4);
        op!(inst, IndirectX, 0xc1, 2, 6);
        op!(inst, IndirectY, 0xd1, 2, 5);

        let inst = CPX;
        op!(inst, Immediate, 0xe0, 2, 2);
        op!(inst, ZeroPage, 0xe4, 2, 3);
        op!(inst, Absolute, 0xec, 3, 4);

        let inst = CPY;
        op!(inst, Immediate, 0xc0, 2, 2);
        op!(inst, ZeroPage, 0xc4, 2, 3);
        op!(inst, Absolute, 0xcc, 3, 4);

        let inst = DEC;
        op!(inst, ZeroPage, 0xc6, 2, 5);
        op!(inst, ZeroPageX, 0xd6, 2, 6);
        op!(inst, Absolute, 0xce, 3, 6);
        op!(inst, AbsoluteX, 0xde, 3, 7);

        op!(DEX, NoneAddressing, 0xca, 1, 2);
        op!(DEY, NoneAddressing, 0x88, 1, 2);

        let inst = EOR;
        op!(inst, Immediate, 0x49, 2, 2);
        op!(inst, ZeroPage, 0x45, 2, 3);
        op!(inst, ZeroPageX, 0x55, 2, 4);
        op!(inst, Absolute, 0x4d, 3, 4);
        op!(inst, AbsoluteX, 0x5d, 3, 4);
        op!(inst, AbsoluteY, 0x59, 3, 4);
        op!(inst, IndirectX, 0x41, 2, 6);
        op!(inst, IndirectY, 0x51, 2, 5);

        let inst = INC;
        op!(inst, ZeroPage, 0xe6, 2, 5);
        op!(inst, ZeroPageX, 0xf6, 2, 6);
        op!(inst, Absolute, 0xee, 3, 6);
        op!(inst, AbsoluteX, 0xfe, 3, 7);

        op!(INX, NoneAddressing, 0xe8, 1, 2);
        op!(INY, NoneAddressing, 0xc8, 1, 2);

        let inst = JMP;
        op!(inst, Absolute, 0x4c, 3, 3);
        op!(inst, Indirect, 0x6c, 3, 5);

        op!(JSR, Absolute, 0x20, 3, 6);

        let inst = LDA;
        op!(inst, Immediate, 0xa9, 2, 2);
        op!(inst, ZeroPage, 0xa5, 2, 3);
        op!(inst, ZeroPageX, 0xb5, 2, 2);
        op!(inst, Absolute, 0xad, 2, 2);
        op!(inst, AbsoluteX, 0xbd, 2, 2);
        op!(inst, AbsoluteY, 0xb9, 2, 2);
        op!(inst, IndirectX, 0xa1, 2, 2);
        op!(inst, IndirectY, 0xb1, 2, 2);

        let inst = LDX;
        op!(inst, Immediate, 0xa2, 2, 2);
        op!(inst, ZeroPage, 0xa6, 2, 3);
        op!(inst, ZeroPageY, 0xb6, 2, 4);
        op!(inst, Absolute, 0xae, 3, 4);
        op!(inst, AbsoluteY, 0xbe, 3, 4);

        let inst = LDY;
        op!(inst, Immediate, 0xa0, 2, 2);
        op!(inst, ZeroPage, 0xa4, 2, 3);
        op!(inst, ZeroPageX, 0xb4, 2, 4);
        op!(inst, Absolute, 0xac, 3, 4);
        op!(inst, AbsoluteX, 0xbc, 3, 4);

        let inst = LSR;
        op!(inst, NoneAddressing, 0x4a, 1, 2);
        op!(inst, ZeroPage, 0x46, 2, 5);
        op!(inst, ZeroPageX, 0x56, 2, 6);
        op!(inst, Absolute, 0x4e, 3, 6);
        op!(inst, AbsoluteX, 0x5e, 3, 7);

        op!(NOP, NoneAddressing, 0xea, 1, 2);

        let inst = ORA;
        op!(inst, Immediate, 0x09, 2, 2);
        op!(inst, ZeroPage, 0x05, 2, 3);
        op!(inst, ZeroPageX, 0x15, 2, 4);
        op!(inst, Absolute, 0x0D, 3, 4);
        op!(inst, AbsoluteX, 0x1D, 3, 4);
        op!(inst, AbsoluteY, 0x19, 3, 4);
        op!(inst, IndirectX, 0x01, 2, 6);
        op!(inst, IndirectY, 0x11, 2, 5);

        op!(PHA, NoneAddressing, 0x48, 1, 3);
        op!(PHP, NoneAddressing, 0x08, 1, 3);
        op!(PLA, NoneAddressing, 0x68, 1, 4);
        op!(PLP, NoneAddressing, 0x28, 1, 4);

        let inst = ROL;
        op!(inst, NoneAddressing, 0x2a, 1, 2);
        op!(inst, ZeroPage, 0x26, 2, 5);
        op!(inst, ZeroPageX, 0x36, 2, 6);
        op!(inst, Absolute, 0x2e, 3, 6);
        op!(inst, AbsoluteX, 0x3e, 3, 7);

        let inst = ROR;
        op!(inst, NoneAddressing, 0x6a, 1, 2);
        op!(inst, ZeroPage, 0x66, 2, 5);
        op!(inst, ZeroPageX, 0x76, 2, 6);
        op!(inst, Absolute, 0x6e, 3, 6);
        op!(inst, AbsoluteX, 0x7e, 3, 7);

        op!(RTS, NoneAddressing, 0x60, 1, 6);

        let inst = SBC;
        op!(inst, Immediate, 0xe9, 2, 2);
        op!(inst, ZeroPage, 0xe5, 2, 3);
        op!(inst, ZeroPageX, 0xf5, 2, 4);
        op!(inst, Absolute, 0xed, 3, 4);
        op!(inst, AbsoluteX, 0xfd, 3, 4);
        op!(inst, AbsoluteY, 0xf9, 3, 4);
        op!(inst, IndirectX, 0xe1, 2, 6);
        op!(inst, IndirectY, 0xf1, 2, 5);

        op!(SEC, NoneAddressing, 0x38, 1, 2);
        op!(SED, NoneAddressing, 0xf8, 1, 2);
        op!(SEI, NoneAddressing, 0x78, 1, 2);

        let inst = STA;
        op!(inst, ZeroPage, 0x85, 2, 3);
        op!(inst, ZeroPageX, 0x95, 2, 4);
        op!(inst, Absolute, 0x8d, 3, 4);
        op!(inst, AbsoluteX, 0x9d, 3, 5);
        op!(inst, AbsoluteY, 0x99, 3, 5);
        op!(inst, IndirectX, 0x81, 2, 6);
        op!(inst, IndirectY, 0x91, 2, 6);

        let inst = STX;
        op!(inst, ZeroPage, 0x86, 2, 3);
        op!(inst, ZeroPageY, 0x96, 2, 4);
        op!(inst, Absolute, 0x8e, 3, 4);

        let inst = STY;
        op!(inst, ZeroPage, 0x84, 2, 3);
        op!(inst, ZeroPageX, 0x94, 2, 4);
        op!(inst, Absolute, 0x8c, 3, 4);

        op!(TAX, NoneAddressing, 0xaa, 1, 2);
        op!(TAY, NoneAddressing, 0xa8, 1, 2);
        op!(TSX, NoneAddressing, 0xba, 1, 2);
        op!(TXA, NoneAddressing, 0x8a, 1, 2);
        op!(TXS, NoneAddressing, 0x9a, 1, 2);
        op!(TYA, NoneAddressing, 0x98, 1, 2);

        Operations { ops }
    }
}
