use crate::hardware::ops::Instruction::*;
use crate::hardware::ops::Operations;

#[derive(Debug)]
pub enum StatusFlags {
    Carry,
    Zero,
    InterruptDisable,
    DecimalMode,
    Brk,
    Brk2,
    Overflow,
    Negative,
}

fn checkflag(c: StatusFlags) -> u8 {
    match c {
        StatusFlags::Carry => 0b0000_0001,
        StatusFlags::Zero => 0b0000_0010,
        StatusFlags::InterruptDisable => 0b0000_0100,
        StatusFlags::DecimalMode => 0b0000_1000,
        StatusFlags::Brk => 0b0001_0000,
        StatusFlags::Brk2 => 0b0010_0000,
        StatusFlags::Overflow => 0b0100_0000,
        StatusFlags::Negative => 0b1000_0000,
    }
}

#[derive(Debug)]
pub enum AddressingMode {
    // Operand Size (bytes)
    Immediate,      // 1
    ZeroPage,       // 1
    ZeroPageX,      // 1
    ZeroPageY,      // 1
    Absolute,       // 2
    AbsoluteX,      // 2
    AbsoluteY,      // 2
    Indirect,       // 2
    IndirectX,      // 1
    IndirectY,      // 1
    NoneAddressing, // 0
}

const STACK: u16 = 0x0100;

#[allow(clippy::upper_case_acronyms)] // I just think cpu::CPU separates things a bit.
pub struct CPU {
    pub accumulator: u8,
    pub reg_x: u8,
    pub reg_y: u8,
    pub status: u8,
    pub program_counter: u16,
    pub stack_pointer: u8,

    // Make memory available to tests and debug, but not public in release.
    #[cfg(test)]
    pub memory: [u8; 0xffff],
    #[cfg(not(test))]
    memory: [u8; 0xffff],
}

impl CPU {
    pub fn new() -> Self {
        CPU {
            accumulator: 0,
            reg_x: 0,
            reg_y: 0,
            status: 0,
            program_counter: 0,
            stack_pointer: 0xff,

            memory: [0; 0xffff],
        }
    }

    pub fn mem_read(&self, addr: u16) -> u8 {
        self.memory[addr as usize]
    }

    fn mem_read_u16(&self, addr: u16) -> u16 {
        <u16>::from_le_bytes([self.mem_read(addr), self.mem_read(addr + 1)])
    }

    pub fn mem_write(&mut self, addr: u16, data: u8) {
        self.memory[addr as usize] = data
    }

    fn mem_write_u16(&mut self, addr: u16, data: u16) {
        let data = data.to_le_bytes();
        self.mem_write(addr, data[0]);
        self.mem_write(addr + 1, data[1])
    }

    fn stack_push(&mut self, data: u8) {
        self.mem_write(STACK | self.stack_pointer as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(1)
    }

    fn stack_push_u16(&mut self, data: u16) {
        self.mem_write_u16(STACK | (self.stack_pointer - 1) as u16, data);
        self.stack_pointer = self.stack_pointer.wrapping_sub(2)
    }

    // In the 6502 the term for taking the last entry from the stack is "pull" rather than the more generally used "pop"
    fn stack_pull(&mut self) -> u8 {
        self.stack_pointer = self.stack_pointer.wrapping_add(1);
        self.mem_read(STACK | self.stack_pointer as u16)
    }

    pub fn reset(&mut self) {
        self.accumulator = 0;
        self.reg_x = 0;
        self.reg_y = 0;
        self.status = 0b0010_0100;

        self.program_counter = self.mem_read_u16(0xfffc);
        self.stack_pointer = 0xff;
    }

    pub fn load(&mut self, program: Vec<u8>) {
        self.memory[0x8000..(0x8000 + program.len())].copy_from_slice(&program[..]);
        self.program_counter = 0x8000
    }

    pub fn load_and_run(&mut self, program: Vec<u8>) {
        self.load(program);
        self.run()
    }

    pub fn run(&mut self) {
        self.run_with_callback(|_| {})
    }

    pub fn run_with_callback<F>(&mut self, mut callback: F)
    where
        F: FnMut(&mut CPU)
    {
        let ref operations = Operations::new();

        loop {
            callback(self);

            let operation = &operations.ops[self.mem_read(self.program_counter) as usize];
            self.program_counter += 1;

            match operation.instruction {
                BRK => {
                    /* todo: Properly implement BRK
                    // Strictly speaking, the BRK command is more complex than just ending the program.
                    // However, for now I've opted to leave that out. If it becomes relevant, I'll revisit.
                    self.stack_push_u16(self.program_counter);
                    self.status_set(StatusFlags::Brk, true);
                    self.status_set(StatusFlags::Brk2, true);
                    self.stack_push(self.status);
                    self.program_counter = self.mem_read_u16(0xfffe)
                    */
                    return
                },

                ADC => self.adc(&operation.addr_mode),
                AND => self.and(&operation.addr_mode),
                ASL => self.asl(&operation.addr_mode),
                BCC => self.bcc(),
                BCS => self.bcs(),
                BEQ => self.beq(),
                BIT => self.bit(&operation.addr_mode),
                BMI => self.bmi(),
                BNE => self.bne(),
                BPL => self.bpl(),
                BVC => self.bvc(),
                BVS => self.bvs(),
                CLC => self.clc(),
                CLD => self.cld(),
                CLI => self.cli(),
                CLV => self.clv(),
                CMP => self.cmp(&operation.addr_mode),
                CPX => self.cpx(&operation.addr_mode),
                CPY => self.cpy(&operation.addr_mode),
                DEC => self.dec(&operation.addr_mode),
                DEX => self.dex(),
                DEY => self.dey(),
                EOR => self.eor(&operation.addr_mode),
                INC => self.inc(&operation.addr_mode),
                INX => self.inx(),
                INY => self.iny(),
                JMP => {
                    self.jmp(&operation.addr_mode);
                    continue; // We don't want to increment the program counter after this operation
                }
                JSR => {
                    self.jsr();
                    continue; // we don't want to increment the program counter after this operation
                }
                LDA => self.lda(&operation.addr_mode),
                LDX => self.ldx(&operation.addr_mode),
                LDY => self.ldy(&operation.addr_mode),
                LSR => self.lsr(&operation.addr_mode),
                NOP => continue,
                ORA => self.ora(&operation.addr_mode),
                PHA => self.pha(),
                PHP => self.php(),
                PLA => self.pla(),
                PLP => self.plp(),
                ROL => self.rol(&operation.addr_mode),
                ROR => self.ror(&operation.addr_mode),
                RTS => self.rts(),
                SBC => self.sbc(&operation.addr_mode),
                SEC => self.sec(),
                SED => self.sed(),
                SEI => self.sei(),
                STA => self.sta(&operation.addr_mode),
                STX => self.stx(&operation.addr_mode),
                STY => self.sty(&operation.addr_mode),
                TAX => self.tax(),
                TAY => self.tay(),
                TSX => self.tsx(),
                TXA => self.txa(),
                TXS => self.txs(),
                TYA => self.tya(),

                _ => panic!(
                    "{}",
                    format!(
                        "0x{:0>2x} is not implemented",
                        self.mem_read(self.program_counter - 1)
                    )
                ),
            }

            self.program_counter += (&operation.bytes - 1) as u16
        }
    }

    // CPU Status Interaction
    pub fn status_read(&self, status_flag: StatusFlags) -> bool {
        let status_flag = checkflag(status_flag);
        self.status & status_flag == status_flag
    }

    fn status_insert(&mut self, status_flag: StatusFlags) {
        self.status |= checkflag(status_flag)
    }

    fn status_remove(&mut self, status_flag: StatusFlags) {
        self.status &= !checkflag(status_flag)
    }

    fn status_set(&mut self, status_flag: StatusFlags, set_to: bool) {
        if set_to {
            self.status_insert(status_flag)
        } else {
            self.status_remove(status_flag)
        }
    }
    // End CPU Status Interaction functions

    // Operation implementation
    ///Add with Carry
    fn adc(&mut self, addr_mode: &AddressingMode) {
        let data = self.get_data(addr_mode);
        let result =
            self.accumulator as u16 + data as u16 + self.status_read(StatusFlags::Carry) as u16;

        self.status_set(StatusFlags::Carry, result > 0xff);
        self.status_set(
            StatusFlags::Overflow,
            ((self.accumulator ^ result as u8) & (data ^ result as u8) & 0x80) == 0x80,
        );
        self.set_accumulator(result as u8);
    }

    /// Logical AND
    fn and(&mut self, addr_mode: &AddressingMode) {
        self.set_accumulator(self.accumulator & self.get_data(addr_mode))
    }

    /// Arithmetic Shift Left
    fn asl(&mut self, addr_mode: &AddressingMode) {
        match addr_mode {
            AddressingMode::NoneAddressing => {
                let data = self.accumulator;
                self.status_set(StatusFlags::Carry, (data >> 7) == 1);
                self.set_accumulator(data << 1);
            }
            _ => {
                let addr = self.get_operand_address(addr_mode);
                let data = self.mem_read(addr);
                self.status_set(StatusFlags::Carry, (data >> 7) == 1);
                let data = data << 1;
                self.mem_write(addr, data);
                self.update_zero_and_negative_flags(data);
            }
        }
    }

    /// Branch if Carry Clear
    fn bcc(&mut self) {
        self.branch(!self.status_read(StatusFlags::Carry))
    }

    /// Branch if Carry Set
    fn bcs(&mut self) {
        self.branch(self.status_read(StatusFlags::Carry))
    }

    /// Branch if Equal
    fn beq(&mut self) {
        self.branch(self.status_read(StatusFlags::Zero))
    }

    /// Bit test
    fn bit(&mut self, addr_mode: &AddressingMode) {
        let data = self.get_data(addr_mode);

        self.status_set(StatusFlags::Zero, self.accumulator & data == 0b0000_0000);
        self.status_set(StatusFlags::Negative, data & 0b1000_0000 == 0b1000_0000);
        self.status_set(StatusFlags::Overflow, data & 0b0100_0000 == 0b0100_0000);
    }

    /// Branch if Minus
    fn bmi(&mut self) {
        self.branch(self.status_read(StatusFlags::Negative))
    }

    /// Branch if Not Equal
    fn bne(&mut self) {
        self.branch(!self.status_read(StatusFlags::Zero))
    }

    /// Branch if Positive
    fn bpl(&mut self) {
        self.branch(!self.status_read(StatusFlags::Negative))
    }

    /// Branch if Overflow Clear
    fn bvc(&mut self) {
        self.branch(!self.status_read(StatusFlags::Overflow))
    }

    /// Branch if Overflow Set
    fn bvs(&mut self) {
        self.branch(self.status_read(StatusFlags::Overflow))
    }

    /// Clear Carry Flag
    fn clc(&mut self) {
        self.status_remove(StatusFlags::Carry)
    }

    /// Clear Decimal Mode
    /// Note: This generally shouldn't be used by NES games, but the Ricoh 2A03 technically
    /// does still allow it as a valid instruction.
    fn cld(&mut self) {
        self.status_remove(StatusFlags::DecimalMode)
    }

    /// Clear Interrupt Disable
    fn cli(&mut self) {
        self.status_remove(StatusFlags::InterruptDisable)
    }

    /// Clear Overflow Flag
    fn clv(&mut self) {
        self.status_remove(StatusFlags::Overflow)
    }

    /// Compare
    fn cmp(&mut self, addr_mode: &AddressingMode) {
        self.compare_register(self.accumulator, self.get_data(addr_mode))
    }

    /// Compare X Register
    fn cpx(&mut self, addr_mode: &AddressingMode) {
        self.compare_register(self.reg_x, self.get_data(addr_mode))
    }

    /// Compare Y Register
    fn cpy(&mut self, addr_mode: &AddressingMode) {
        self.compare_register(self.reg_y, self.get_data(addr_mode))
    }

    /// Decrement Memory
    fn dec(&mut self, addr_mode: &AddressingMode) {
        let addr = self.get_operand_address(addr_mode);
        self.set_mem(addr, self.mem_read(addr) - 1);
    }

    /// Decrement Register X
    fn dex(&mut self) {
        self.set_reg_x(self.reg_x - 1)
    }

    /// Decrement Register Y
    fn dey(&mut self) {
        self.set_reg_y(self.reg_y - 1)
    }

    /// Exclusive OR
    fn eor(&mut self, addr_mode: &AddressingMode) {
        self.set_accumulator(self.accumulator ^ self.get_data(addr_mode))
    }

    /// Increment Memory
    fn inc(&mut self, addr_mode: &AddressingMode) {
        let addr = self.get_operand_address(addr_mode);
        self.set_mem(addr, self.mem_read(addr) + 1);
    }

    /// Increment X
    fn inx(&mut self) {
        self.set_reg_x(self.reg_x + 1);
    }

    /// Increment Y
    fn iny(&mut self) {
        self.set_reg_y(self.reg_y + 1);
    }

    /// Jump
    fn jmp(&mut self, addr_mode: &AddressingMode) {
        match addr_mode {
            AddressingMode::Absolute => {
                let addr = self.mem_read_u16(self.program_counter);
                self.program_counter = addr;
            }
            AddressingMode::Indirect => {
                // This addressing mode has a bug in the original hardware.
                // Essentially, if the address crosses the page boundary (ex: Least-sig byte $10FF) then the
                // most significant byte will come from $1000 instead of $1100.
                let addr = self.mem_read_u16(self.program_counter);
                self.program_counter = if addr & 0x00ff == 0x00ff {
                    <u16>::from_le_bytes([self.mem_read(addr), self.mem_read(addr & 0xff00)])
                } else {
                    self.mem_read_u16(addr)
                }
            }
            _ => { /* There are no other AddressingModes for JMP, but match dictates we cover all possible patterns */
            }
        }
    }

    /// Jump to Subroutine
    fn jsr(&mut self) {
        let data = self.program_counter + 1; // +2 for the address the instruction points to, -1 required to replicate behavior
        self.stack_push_u16(data);

        self.program_counter = self.mem_read_u16(self.program_counter)
    }

    /// Load Accumulator
    fn lda(&mut self, addr_mode: &AddressingMode) {
        self.set_accumulator(self.get_data(addr_mode))
    }

    /// Load Register X
    fn ldx(&mut self, addr_mode: &AddressingMode) {
        self.set_reg_x(self.get_data(addr_mode));
    }

    /// Load Register Y
    fn ldy(&mut self, addr_mode: &AddressingMode) {
        self.set_reg_y(self.get_data(addr_mode));
    }

    /// Logical Shift Right
    fn lsr(&mut self, addr_mode: &AddressingMode) {
        if let AddressingMode::NoneAddressing = addr_mode {
            let data = self.accumulator;
            self.status_set(StatusFlags::Carry, (0b0001 & data) == 1);
            self.set_accumulator(data >> 1);
        } else {
            let addr = self.get_operand_address(addr_mode);
            let data = self.mem_read(addr);
            self.status_set(StatusFlags::Carry, 0b0001 & data == 1);
            self.set_mem(addr, data >> 1);
        };
    }

    /// Logical Inclusive OR
    fn ora(&mut self, addr_mode: &AddressingMode) {
        let data = self.get_data(addr_mode);

        self.set_accumulator(self.accumulator | data);
    }

    /// Push Accumulator
    fn pha(&mut self) {
        self.stack_push(self.accumulator);
    }

    /// Push Processor Status
    fn php(&mut self) {
        self.status_set(StatusFlags::Brk, true);
        self.status_set(StatusFlags::Brk2, true);
        self.stack_push(self.status);
    }

    /// Pull Accumulator
    fn pla(&mut self) {
        let stack_value = self.stack_pull();
        self.set_accumulator(stack_value);
    }

    /// Pull Processor Status
    fn plp(&mut self) {
        self.status = self.stack_pull();
    }

    /// Rotate Left
    fn rol(&mut self, addr_mode: &AddressingMode) {
        let rotate = |data: u8| -> (bool, u8) {
            (
                0b1000_0000 & data == 0b1000_0000,
                data << 1 | checkflag(StatusFlags::Carry),
            )
        };

        self.set_rotation(addr_mode, rotate);
    }

    /// Rotate Right
    fn ror(&mut self, addr_mode: &AddressingMode) {
        let rotate = |data: u8| -> (bool, u8) {
            (
                0b0000_0001 & data == 0b0000_0001,
                data >> 1 | checkflag(StatusFlags::Carry) << 7,
            )
        };

        self.set_rotation(addr_mode, rotate);
    }

    /// Return from Interrupt
    fn rti(&mut self) {
        self.status = self.stack_pull();
    }

    /// Return from Subroutine
    fn rts(&mut self) {
        let lo = self.stack_pull();
        let hi = self.stack_pull();
        self.program_counter = <u16>::from_le_bytes([lo, hi]) + 1;
    }

    /// Subtract with Carry
    fn sbc(&mut self, addr_mode: &AddressingMode) {
        let data = self.get_data(addr_mode);

        let (result, carry) = self.accumulator.overflowing_sub(data);
        let (result, carry2) = match self.status_read(StatusFlags::Carry) {
            false => {
                let (r, c) = result.overflowing_sub(1);
                (r, c)
            }
            true => (result, false),
        };
        let carry = !(carry || carry2);

        self.status_set(StatusFlags::Carry, carry);
        self.status_set(
            StatusFlags::Overflow,
            (((self.accumulator ^ data) & 0x80) == 0x80)
                && (((self.accumulator ^ result) & 0x80) == 0x80),
        );
        self.set_accumulator(result);
    }

    /// Set Carry Flag
    fn sec(&mut self) {
        self.status_set(StatusFlags::Carry, true);
    }

    /// Set Decimal-mode Flag
    fn sed(&mut self) {
        self.status_set(StatusFlags::DecimalMode, true);
    }

    /// Set Interrupt-disable Flag
    fn sei(&mut self) {
        self.status_set(StatusFlags::InterruptDisable, true);
    }

    /// Store Accumulator in memory
    fn sta(&mut self, addr_mode: &AddressingMode) {
        self.mem_write(self.get_operand_address(addr_mode), self.accumulator);
    }

    /// Store X Register in memory
    fn stx(&mut self, addr_mode: &AddressingMode) {
        self.mem_write(self.get_operand_address(addr_mode), self.reg_x)
    }

    /// Store Y Register in memory
    fn sty(&mut self, addr_mode: &AddressingMode) {
        self.mem_write(self.get_operand_address(addr_mode), self.reg_y)
    }

    /// Transfer A -> X
    fn tax(&mut self) {
        self.set_reg_x(self.accumulator)
    }

    /// Transfer A -> Y
    fn tay(&mut self) {
         self.set_reg_y(self.accumulator)
    }

    /// Transfer Stack-Pointer -> X
    fn tsx(&mut self) {
        self.set_reg_x(self.stack_pointer)
    }

    /// Transfer X -> Accumulator
    fn txa(&mut self) {
        self.set_accumulator(self.reg_x)
    }

    /// Transfer X -> Stack-Pointer
    fn txs(&mut self) {
        self.stack_pointer = self.reg_x
    }

    /// Transfer Y -> Accumulator
    fn tya(&mut self) {
        self.set_accumulator(self.reg_y)
    }

    // End: Operations

    /// Compares a value against common criteria to set StatusFlags::{Zero, Negative} as appropriate
    fn update_zero_and_negative_flags(&mut self, result: u8) {
        // Zero bit
        if result == 0 {
            self.status_insert(StatusFlags::Zero);
        } else {
            self.status_remove(StatusFlags::Zero);
        }
        // Negative bit
        if result & 0b1000_0000 != 0 {
            self.status_insert(StatusFlags::Negative);
        } else {
            self.status_remove(StatusFlags::Negative);
        }
    }

    /// Small helper function to get data from an addressing mode in fewer keystrokes.
    fn get_data(&self, addr_mode: &AddressingMode) -> u8 {
        self.mem_read(self.get_operand_address(addr_mode))
    }

    /// Helper function to translate an AddressingMode into an actionable memory address
    fn get_operand_address(&self, addr_mode: &AddressingMode) -> u16 {
        match addr_mode {
            AddressingMode::NoneAddressing => 0,
            AddressingMode::Immediate => self.program_counter,
            AddressingMode::ZeroPage => self.mem_read(self.program_counter) as u16,
            AddressingMode::Absolute => self.mem_read_u16(self.program_counter),
            AddressingMode::ZeroPageX => (self.mem_read(self.program_counter) + self.reg_x) as u16,
            AddressingMode::ZeroPageY => (self.mem_read(self.program_counter) + self.reg_y) as u16,
            AddressingMode::AbsoluteX => {
                self.mem_read_u16(self.program_counter) + (self.reg_x as u16)
            }
            AddressingMode::AbsoluteY => {
                self.mem_read_u16(self.program_counter) + (self.reg_y as u16)
            }

            // Indirect, IndirectX, and IndirectY - it affects all three indirect modes
            AddressingMode::Indirect => self.mem_read_u16(self.program_counter),
            AddressingMode::IndirectX => {
                let pointer = self.mem_read(self.program_counter) + self.reg_x;
                <u16>::from_le_bytes([
                    self.mem_read(pointer as u16),
                    self.mem_read((pointer + 1) as u16),
                ])
            }
            AddressingMode::IndirectY => {
                let pointer = self.mem_read(self.program_counter);
                <u16>::from_le_bytes([
                    self.mem_read(pointer as u16),
                    self.mem_read((pointer + 1) as u16),
                ]) + self.reg_y as u16
            }
        }
    }

    /// Writes to register a (the accumulator) then updates zero and negative flags
    fn set_accumulator(&mut self, value: u8) {
        self.accumulator = value;
        self.update_zero_and_negative_flags(self.accumulator);
    }

    /// Writes to register x then updates zero and negative flags
    fn set_reg_x(&mut self, value: u8) {
        self.reg_x = value;
        self.update_zero_and_negative_flags(self.reg_x);
    }

    /// Writes to register y then updates zero and negative flags
    fn set_reg_y(&mut self, value: u8) {
        self.reg_y = value;
        self.update_zero_and_negative_flags(self.reg_y);
    }

    /// Writes to memory address then updates zero and negative flags
    fn set_mem(&mut self, addr: u16, value: u8) {
        self.mem_write(addr, value);
        self.update_zero_and_negative_flags(value)
    }

    /// Helper function for branching instructions
    fn branch(&mut self, condition: bool) {
        if condition {
            self.program_counter = (self.program_counter as i16
                + (self.mem_read(self.program_counter) as i8) as i16)
                as u16;
        }
    }

    /// Helper function for comparison instructions to set flags
    fn compare_register(&mut self, register: u8, data: u8) {
        self.status_set(StatusFlags::Carry, register >= data);
        self.status_set(StatusFlags::Zero, register == data);
        self.status_set(StatusFlags::Negative, (register as i8 - data as i8) < 0)
    }

    /// Helper function for ROL and ROR instructions
    fn set_rotation(&mut self, addr_mode: &AddressingMode, rotate: fn(u8) -> (bool, u8)) {
        let res = if let AddressingMode::NoneAddressing = addr_mode {
            let res = rotate(self.accumulator);

            self.set_accumulator(res.1);
            res.0
        } else {
            let addr = self.get_operand_address(addr_mode);
            let res = rotate(self.mem_read(addr));

            self.set_mem(addr, res.1);
            res.0
        };
        self.status_set(StatusFlags::Carry, res);
    }
}
