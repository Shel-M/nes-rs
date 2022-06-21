#[cfg(test)]
mod cpu_tests {
    use crate::hardware::cpu::{StatusFlags, CPU};

    macro_rules! setup_test {
        ($($x:expr),+ $(,)?) => {{
            let mut cpu = crate::hardware::cpu::CPU::new();
            cpu.load_and_run(vec![$($x),+]);
            cpu
        }}
    }

    impl CPU {
        fn assert_accumulator_value(&self, value: u8) {
            assert_eq!(
                self.accumulator, value,
                "cpu.accumulator 0x{:0>2x} did not match expected value 0x{:0>2x}",
                self.accumulator, value
            )
        }

        fn assert_reg_x_value(&self, value: u8) {
            assert_eq!(
                self.reg_x, value,
                "cpu.reg_x 0x{:0>2x} did not match expected value 0x{:0>2x}",
                self.reg_x, value
            )
        }

        fn assert_reg_y_value(&self, value: u8) {
            assert_eq!(
                self.reg_y, value,
                "cpu.reg_y did not match expected value 0x{:0>2x}",
                self.reg_y
            )
        }

        fn assert_status_set(&self, flag: StatusFlags) {
            let message = format!("cpu.status {:?} not set {:0>8b}", flag, self.status);
            assert!(self.status_read(flag), "{message}")
        }

        fn assert_status_clear(&self, flag: StatusFlags) {
            let message = format!("cpu.status {:?} not clear {:0>8b}", flag, self.status);
            assert!(!(self.status_read(flag)), "{message}")
        }

        fn assert_memory_value(&self, addr: u16, value: u8) {
            let data = self.memory[addr as usize];
            assert_eq!(
                data, value,
                "cpu.memory[0x{addr:0>4x}] 0x{data:0>2x} did not match expected value 0x{value:0>2x}"
            )
        }

        fn assert_program_counter_value(&self, value: u16) {
            assert_eq!(
                self.program_counter, value,
                "cpu.program_counter 0x{:0>4x} did not match expected value 0x{:0>4x}",
                self.program_counter, value
            )
        }

        fn assert_stack_pointer_value(&self, value: u8) {
            assert_eq!(
                self.stack_pointer, value,
                "cpu.stack_pointer 0x{:0>2x} did not match expected value 0x{:0>2x}",
                self.stack_pointer, value
            )
        }

        fn assert_stack_values(&self, values: Vec<u8>) {
            let mut local_stack_pointer = self.stack_pointer;

            for value in values {
                // stack_val should approximate a cpu.stack_pull call without modifying state
                let stack_val = {
                    |cpu: &CPU| -> u8 {
                        local_stack_pointer = local_stack_pointer.wrapping_add(1);
                        cpu.memory[(0x0100 | local_stack_pointer as u16) as usize]
                    }
                }(self);
                assert_eq!(
                    value,
                    stack_val,
                    "stack cpu.memory[0x{:0>4x}] 0x{:0>2x} did not match expected value 0x{:0>2x}",
                    0x0100 | self.stack_pointer as u16,
                    stack_val,
                    value
                )
            }
        }
    }

    // All memory modes tested under ADC
    #[cfg(test)]
    mod adc {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x69_adc_immediate() {
            let cpu = setup_test!(0xa9, 0x05, 0x69, 0xf5, 0x00);

            cpu.assert_accumulator_value(0xfa);
            cpu.assert_status_set(StatusFlags::Negative)
        }

        #[test]
        fn test_0xa9_adc_zero_status_flag() {
            let cpu = setup_test!(0xa9, 0xff, 0x69, 0x01, 0x00);

            cpu.assert_accumulator_value(0x00);
            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Carry);
        }

        #[test]
        fn test_0x65_adc_zero_page() {
            let cpu = setup_test!(0xa9, 0xc5, 0x85, 0x01, 0x65, 0x01, 0x00);

            cpu.assert_accumulator_value(0x8a);
            cpu.assert_status_set(StatusFlags::Negative);
            cpu.assert_status_set(StatusFlags::Carry)
        }

        #[test]
        fn test_0x75_adc_zero_page_x() {
            let cpu = setup_test!(0xa9, 0x05, 0xa2, 0x01, 0x85, 0x02, 0x75, 0x01, 0x00);

            cpu.assert_accumulator_value(0x0a)
        }

        #[test]
        fn test_0x6d_adc_absolute() {
            let cpu = setup_test!(0xa9, 0x05, 0xa2, 0x01, 0x8d, 0x01, 0x10, 0x6d, 0x01, 0x10, 0x00);

            cpu.assert_accumulator_value(0x0a)
        }

        #[test]
        fn test_0x7d_adc_absolute_x() {
            let cpu = setup_test!(0xa9, 0x05, 0x8d, 0x01, 0x10, 0x7d, 0x01, 0x10, 0x00);

            cpu.assert_accumulator_value(0x0a)
        }

        #[test]
        fn test_0x61_adc_indirect_x() {
            let cpu = setup_test![
                0xa2, 0x01, 0xa9, 0x05, 0x85, 0x01, 0xa9, 0x07, 0x85, 0x02, 0xa0, 0x0a, 0x8c, 0x05,
                0x07, 0xa1, 0x00
            ];

            cpu.assert_accumulator_value(0x0a)
        }

        #[test]
        fn test_0x71_adc_indirect_y() {
            let cpu = setup_test![
                0xa0, 0x01, 0xa9, 0x03, 0x85, 0x01, 0xa9, 0x07, 0x85, 0x02, 0xa2, 0x0a, 0x8e, 0x04,
                0x07, 0xb1, 0x01, 0x00
            ];

            cpu.assert_accumulator_value(0x0a)
        }
    }

    #[cfg(test)]
    mod and {
        #[test]
        fn test_0x29_and() {
            let cpu = setup_test!(0xa9, 0b0011_0101, 0x29, 0b0011_1010);

            cpu.assert_accumulator_value(0b0011_0000);
        }
    }

    #[cfg(test)]
    mod asl {
        #[test]
        fn test_0x0a_asl() {
            let cpu = setup_test!(0xa9, 0b0000_0001, 0x0a);

            cpu.assert_accumulator_value(0b00000_0010);
        }
    }

    #[cfg(test)]
    mod bcc {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x90_bcc() {
            let cpu = setup_test!(0xa9, 0x01, 0x69, 0x07, 0x90, 0xfc, 0x00);

            cpu.assert_accumulator_value(0x04);
            cpu.assert_status_set(StatusFlags::Carry);
        }
    }

    #[cfg(test)]
    mod bcs {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xb0_bcs() {
            let cpu = setup_test!(0xa9, 0x80, 0x69, 0xf0, 0xb0, 0xfc, 0x00);

            cpu.assert_accumulator_value(0xf8);
            cpu.assert_status_clear(StatusFlags::Carry)
        }
    }

    #[cfg(test)]
    mod beq {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xf0_beq() {
            let cpu = setup_test!(0xa9, 0x05, 0xe8, 0xe9, 0x04, 0xf0, 0xfb, 0x00);

            cpu.assert_accumulator_value(0xfc);
            cpu.assert_reg_x_value(0x02);
            cpu.assert_status_set(StatusFlags::Negative);
        }
    }

    #[cfg(test)]
    mod bit {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x24_bit_0_0_0() {
            let cpu = setup_test!(0xa9, 0x05, 0x85, 0x55, 0x24, 0x55, 0x00);

            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Overflow);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0x24_bit_status_1_0_0() {
            let cpu = setup_test!(0xa9, 0x01, 0x85, 0x55, 0xa9, 0x02, 0x24, 0x55, 0x00);

            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Overflow);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0x24_bit_status_0_1_0() {
            let cpu = setup_test!(0xa9, 0x40, 0x85, 0x55, 0x24, 0x55, 0x00);

            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Overflow);
            cpu.assert_status_clear(StatusFlags::Negative);
        }

        #[test]
        fn test_0x24_bit_status_0_0_1() {
            let cpu = setup_test!(0xa9, 0x80, 0x85, 0x55, 0x24, 0x55, 0x00);

            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Overflow);
            cpu.assert_status_set(StatusFlags::Negative);
        }

        #[test]
        fn test_0x24_bit_status_1_1_1() {
            let cpu = setup_test!(0xa9, 0xfd, 0x85, 0x55, 0xa9, 0x02, 0x24, 0x55, 0x00);

            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Overflow);
            cpu.assert_status_set(StatusFlags::Negative);
        }
    }

    #[cfg(test)]
    mod bmi {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x30_bmi() {
            let cpu = setup_test!(0xa9, 0xff, 0xe8, 0xe9, 0x07, 0x30, 0xfb, 0x00);

            cpu.assert_accumulator_value(0x79);
            cpu.assert_reg_x_value(0x13);
            cpu.assert_status_clear(StatusFlags::Negative);
        }
    }

    #[cfg(test)]
    mod bne {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xd0_bne() {
            let cpu = setup_test!(0xa9, 0x40, 0xe8, 0xe9, 0x09, 0xd0, 0xfb, 0x00);

            cpu.assert_accumulator_value(0x00);
            cpu.assert_reg_x_value(0x07);
            cpu.assert_status_set(StatusFlags::Zero);
        }
    }

    #[cfg(test)]
    mod bpl {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x10_bpl_negative() {
            /* Disassembly for this test
            Address  Hexdump   Disassembly
            -------------------------------
            $0600    a2 f0     LDX #$f0
            $0602    ca        DEX
            $0603    10 fd     BPL $0602

             */
            let cpu = setup_test!(0xa2, 0xf0, 0xca, 0x10, 0xfd);

            cpu.assert_reg_x_value(0xef);
            cpu.assert_status_set(StatusFlags::Negative);
        }

        #[test]
        fn test_0x10_bpl_positive() {
            /* Disassembly for this test
            Address  Hexdump   Disassembly
            -------------------------------
            $0600    a2 01     LDX #$01
            $0602    e8        INX
            $0603    10 fd     BPL $8002
             */
            let cpu = setup_test!(0xa2, 0x01, 0xe8, 0x10, 0xfd);

            cpu.assert_reg_x_value(0x80);
            cpu.assert_status_set(StatusFlags::Negative) // Should have wrapped
        }
    }

    #[cfg(test)]
    mod bvc {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x50_bvc() {
            let cpu = setup_test!(0xa9, 0x40, 0xe8, 0xe9, 0x09, 0x50, 0xfb, 0x00);

            cpu.assert_accumulator_value(0x78);
            cpu.assert_reg_x_value(0x16);
            cpu.assert_status_set(StatusFlags::Overflow)
        }
    }

    #[cfg(test)]
    mod bvs {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x70_bvs() {
            let cpu = setup_test!(0xa9, 0x79, 0xe8, 0x69, 0x20, 0x70, 0xfb);

            cpu.assert_accumulator_value(0xb9);
            cpu.assert_reg_x_value(0x02);
            cpu.assert_status_set(StatusFlags::Negative);
        }
    }

    #[cfg(test)]
    mod clc {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x18_clc() {
            let cpu = setup_test!(0xa9, 0xc5, 0x69, 0xc5, 0x18, 0x00);

            cpu.assert_accumulator_value(0x8a);
            cpu.assert_status_clear(StatusFlags::Carry);
        }
    }

    #[cfg(test)]
    mod cld {
        use crate::hardware::cpu::{StatusFlags, CPU};

        #[test]
        fn test_0xd8_cld() {
            let mut cpu = CPU::new();
            cpu.status ^= 0b0000_1000;
            cpu.load_and_run(vec![0xd8, 0x00]);

            cpu.assert_status_clear(StatusFlags::DecimalMode)
        }
    }

    #[cfg(test)]
    mod cli {
        use crate::hardware::cpu::{StatusFlags, CPU};

        #[test]
        fn test_0x58_cli() {
            let mut cpu = CPU::new();
            cpu.status ^= 0b0000_0100;
            cpu.load_and_run(vec![0x58, 0x00]);

            cpu.assert_status_clear(StatusFlags::InterruptDisable)
        }
    }

    #[cfg(test)]
    mod clv {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xb8_clv() {
            let cpu = setup_test!(0xa9, 0x50, 0xe9, 0xb0, 0xb8, 0x00);

            cpu.assert_accumulator_value(0x9f);
            cpu.assert_status_clear(StatusFlags::Overflow)
        }
    }

    #[cfg(test)]
    mod cmp {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xc9_cmp_carry() {
            let cpu = setup_test!(0xa9, 0x10, 0xc9, 0x05, 0x00);

            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xc9_cmp_zero() {
            let cpu = setup_test!(0xa9, 0x10, 0xc9, 0x010, 0x00);

            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xc9_cmp_negative() {
            let cpu = setup_test!(0xa9, 0x10, 0xc9, 0x015, 0x00);

            cpu.assert_status_clear(StatusFlags::Carry);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod cpx {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xe0_cpx_carry() {
            let cpu = setup_test!(0xa2, 0x10, 0xe0, 0x05, 0x00);

            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xe0_cpx_zero() {
            let cpu = setup_test!(0xa2, 0x10, 0xe0, 0x010, 0x00);

            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xe0_cpx_negative() {
            let cpu = setup_test!(0xa2, 0x10, 0xe0, 0x015, 0x00);

            cpu.assert_status_clear(StatusFlags::Carry);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod cpy {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xc0_cpy_carry() {
            let cpu = setup_test!(0xa0, 0x10, 0xc0, 0x05, 0x00);

            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xc0_cpy_zero() {
            let cpu = setup_test!(0xa0, 0x10, 0xc0, 0x010, 0x00);

            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xc0_cpy_negative() {
            let cpu = setup_test!(0xa0, 0x10, 0xc0, 0x015, 0x00);

            cpu.assert_status_clear(StatusFlags::Carry);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod dec {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xc6_dec() {
            let cpu = setup_test!(0xa9, 0x10, 0x85, 0x01, 0xc6, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x0f);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xc6_dec_zero() {
            let cpu = setup_test!(0xa9, 0x01, 0x85, 0x01, 0xc6, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x00);
            cpu.assert_status_set(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }

        #[test]
        fn test_0xc6_dec_neg() {
            let cpu = setup_test!(0xc6, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0xff);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_set(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod dex {
        #[test]
        fn test_0xca_dex() {
            let cpu = setup_test!(0xa2, 0x10, 0xca, 0x00);

            cpu.assert_reg_x_value(0x0f);
        }
    }

    #[cfg(test)]
    mod dey {
        #[test]
        fn test_0x88_dey() {
            let cpu = setup_test!(0xa0, 0x10, 0x88, 0x00);

            cpu.assert_reg_y_value(0x0f);
        }
    }

    #[cfg(test)]
    mod eor {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0x49_eor() {
            let cpu = setup_test!(0xa9, 0x5c, 0x49, 0xd3, 0x00);

            cpu.assert_accumulator_value(0x8f);
            cpu.assert_status_set(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod inc {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xe6_inc() {
            let cpu = setup_test!(0xa9, 0x10, 0x85, 0x01, 0xe6, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x11);
            cpu.assert_status_clear(StatusFlags::Zero);
            cpu.assert_status_clear(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod inx {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xe8_inx() {
            let cpu = setup_test!(0xa2, 0x01, 0xe8, 0x00);

            cpu.assert_reg_x_value(2)
        }

        #[test]
        fn test_0xe8_inx_negative_overflow() {
            let cpu = setup_test!(0xa2, 127, 0xe8, 0x00);

            cpu.assert_status_set(StatusFlags::Negative)
        }

        #[test]
        fn test_0xe8_inx_overflow_to_zero() {
            let cpu = setup_test!(0xa2, 0xff, 0xe8, 0x00);

            cpu.assert_reg_x_value(0);
            cpu.assert_status_set(StatusFlags::Zero)
        }
    }

    #[cfg(test)]
    mod iny {
        #[test]
        fn test_0xc8_inx() {
            let cpu = setup_test!(0xa0, 0x01, 0xc8, 0x00);

            cpu.assert_reg_y_value(2)
        }
    }

    #[cfg(test)]
    mod jmp {
        #[test]
        fn test_0x4c_jmp_absolute() {
            let cpu = setup_test!(0xa2, 0x01, 0x4c, 0x07, 0x80, 0xa2, 0x10, 0xe8, 0x00);

            cpu.assert_reg_x_value(2);
        }

        #[test]
        fn test_0x6c_jmp_indirect() {
            /* Assembly program for this test
            LDA #$01
            STA $f0
            LDA #$cc
            STA $f1
            JMP ($00f0)
             */
            let cpu = setup_test!(0xa9, 0x01, 0x85, 0xf0, 0xa9, 0xcc, 0x85, 0xf1, 0x6c, 0xf0, 0x00);

            cpu.assert_program_counter_value(0xcc02)
        }

        #[test]
        fn test_0x6c_jmp_indirect_original_hw_bug() {
            /* Assembly program for this test
            LDA #$01
            STA $ff
            LDA #$cc
            STA $00
            JMP ($00ff)
             */
            let cpu = setup_test!(0xa9, 0x01, 0x85, 0xff, 0xa9, 0xcc, 0x85, 0x00, 0x6c, 0xff, 0x00);

            cpu.assert_program_counter_value(0xcc02)
        }
    }

    #[cfg(test)]
    mod jsr {
        #[test]
        fn test_0x20_jsr() {
            let cpu = setup_test!(0x20, 0x04, 0x80, 0x00, 0xa9, 0x99, 0x00);

            cpu.assert_program_counter_value(0x8007);
            cpu.assert_memory_value(0x01fe, 0x02);
            cpu.assert_memory_value(0x01ff, 0x80);
            cpu.assert_accumulator_value(0x99);
        }
    }

    #[cfg(test)]
    mod lda {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xa9_lda() {
            let cpu = setup_test!(0xa9, 0x05, 0x00);

            cpu.assert_accumulator_value(0x05);
        }

        #[test]
        fn test_0xa9_lda_neg_flag() {
            let cpu = setup_test!(0xa9, 0b1000_0001, 0x00);

            cpu.assert_status_set(StatusFlags::Negative);
        }

        #[test]
        fn test_0xa9_lda_zero_flag() {
            let cpu = setup_test!(0xa9, 0x00, 0x00);

            cpu.assert_status_set(StatusFlags::Zero)
        }
    }

    #[cfg(test)]
    mod ldx {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xa2_ldx_immediate() {
            let cpu = setup_test!(0xa2, 0x05, 0x00);

            cpu.assert_reg_x_value(0x05)
        }

        #[test]
        fn test_0xa2_ldx_zero_flag() {
            let cpu = setup_test!(0xa2, 0x00, 0x00);

            cpu.assert_reg_x_value(0x00);
            cpu.assert_status_set(StatusFlags::Zero)
        }

        #[test]
        fn test_0xa2_ldx_neg_flag() {
            let cpu = setup_test!(0xa2, 0xf0, 0x00);

            cpu.assert_reg_x_value(0xf0);
            cpu.assert_status_set(StatusFlags::Negative);
        }
    }

    #[cfg(test)]
    mod ldy {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xa2_ldy_immediate() {
            let cpu = setup_test!(0xa0, 0x05, 0x00);

            cpu.assert_reg_y_value(0x05)
        }

        #[test]
        fn test_0xa2_ldy_zero_flag() {
            let cpu = setup_test!(0xa0, 0x00, 0x00);

            cpu.assert_reg_y_value(0x00);
            cpu.assert_status_set(StatusFlags::Zero)
        }

        #[test]
        fn test_0xa2_ldy_neg_flag() {
            let cpu = setup_test!(0xa0, 0xf0, 0x00);

            cpu.assert_reg_y_value(0xf0);
            cpu.assert_status_set(StatusFlags::Negative)
        }
    }

    #[cfg(test)]
    mod lsr {
        use crate::hardware::cpu::StatusFlags::Carry;

        #[test]
        fn test_0x4a_lsr_acc_addressing() {
            let cpu = setup_test!(0xa9, 0x99, 0x4a, 0x00);

            cpu.assert_accumulator_value(0x4c);
            cpu.assert_status_set(Carry);
        }

        #[test]
        fn test_0x46_lsr_memory_addressing() {
            let cpu = setup_test!(0xa9, 0x99, 0x85, 0x01, 0x46, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x4c);
            cpu.assert_status_set(Carry);
        }
    }

    #[cfg(test)]
    mod nop {
        #[test]
        fn test_0xea_nop() {
            let cpu = setup_test!(0xea, 0xa9, 0x99, 0xea, 0xa9, 0x98, 0x00);

            cpu.assert_accumulator_value(0x98);
        }
    }

    #[cfg(test)]
    mod ora {
        use crate::hardware::cpu::StatusFlags::Negative;

        #[test]
        fn test_0x09_ora() {
            let cpu = setup_test!(0xa9, 0b0011_1100, 0x09, 0b1100_0011);

            cpu.assert_accumulator_value(0b1111_1111);
            cpu.assert_status_set(Negative);
        }
    }

    #[cfg(test)]
    mod pha {
        #[test]
        fn test_0x48_pha() {
            let cpu = setup_test!(0xa9, 0x99, 0x48, 0xa9, 0x98, 0x48);

            cpu.assert_stack_pointer_value(0xfd);
            cpu.assert_stack_values(vec![0x98, 0x99]);
        }
    }

    #[cfg(test)]
    mod php {
        #[test]
        fn test_0x08_php() {
            let cpu = setup_test!(0xa9, 0xf0, 0x08, 0x00);

            cpu.assert_stack_pointer_value(0xfe);
            cpu.assert_stack_values(vec![0b1011_0000]);
        }
    }

    #[cfg(test)]
    mod pla {
        #[test]
        fn test_0x68_pla() {
            let cpu = setup_test!(0xa9, 0x10, 0x48, 0xa9, 0x11, 0x68, 0x00);

            cpu.assert_accumulator_value(0x10);
        }
    }

    #[cfg(test)]
    mod plp {
        use crate::hardware::cpu::StatusFlags::*;

        #[test]
        fn test_0x28_plp() {
            let cpu = setup_test!(0xa9, 0b1100_0001, 0x48, 0x28, 0x00);

            //Negative, Overflow, Carry
            cpu.assert_status_set(Negative);
            cpu.assert_status_set(Overflow);
            cpu.assert_status_set(Carry);
        }
    }

    #[cfg(test)]
    mod rol {
        use crate::hardware::cpu::StatusFlags::Carry;

        #[test]
        fn test_0x2a_rol_acc_addressing() {
            let cpu = setup_test!(0xa9, 0b1001_0111, 0x38, 0x2a, 0x00);

            cpu.assert_status_set(Carry);
            cpu.assert_accumulator_value(0b0010_1111)
        }
    }

    #[cfg(test)]
    mod ror {
        use crate::hardware::cpu::StatusFlags::Carry;

        #[test]
        fn test_0x2a_rol_acc_addressing() {
            let cpu = setup_test!(0xa9, 0b1001_0111, 0x38, 0x6a, 0x00);

            cpu.assert_status_set(Carry);
            cpu.assert_accumulator_value(0b1100_1011)
        }
    }

    #[cfg(test)]
    mod rts {
        #[test]
        fn test_0x60_rts() {
            /* Testing program :
            Address  Hexdump   Disassembly
            -------------------------------
            $8000    20 06 80  JSR $8005
            $8003    8a        TXA
            $8004    00        BRK
            $8005    e8        INX
            $8006    e0 05     CPX #$05
            $8008    d0 fb     BNE $8005
            $800a    60        RTS
            $800b    00        BRK
             */
            let cpu = setup_test!(0x20, 0x05, 0x80, 0x8a, 0x00, 0xe8, 0xe0, 0x05, 0xd0, 0xfb, 0x60, 0x00);

            cpu.assert_reg_x_value(0x05);
            cpu.assert_accumulator_value(0x05);
            cpu.assert_program_counter_value(0x8005);
            cpu.assert_stack_pointer_value(0xff);
        }
    }

    #[cfg(test)]
    mod sbc {
        use crate::hardware::cpu::StatusFlags;

        #[test]
        fn test_0xe9_sbc_immediate() {
            let cpu = setup_test!(0xa9, 0x0a, 0xe9, 0x05, 0x00);

            cpu.assert_accumulator_value(0x04);
        }

        #[test]
        fn test_0xe9_sbc_carry_flag() {
            let cpu = setup_test!(0xa9, 0x0a, 0xe9, 0x05, 0x00);

            cpu.assert_accumulator_value(0x04);
            cpu.assert_status_set(StatusFlags::Carry)
        }

        #[test]
        fn test_0xe9_sbc_zero_flag() {
            let cpu = setup_test!(0xa9, 0x0a, 0xe9, 0x09, 0x00);

            cpu.assert_accumulator_value(0x00);
            cpu.assert_status_set(StatusFlags::Carry);
            cpu.assert_status_set(StatusFlags::Zero)
        }

        #[test]
        fn test_0xe9_sbc_overflow_flag() {
            let cpu = setup_test!(0xa9, 0x50, 0xe9, 0xb0, 0x00);

            cpu.assert_accumulator_value(0x9f);
            cpu.assert_status_set(StatusFlags::Overflow)
        }

        #[test]
        fn test_0xe9_sbc_over_zero_status_flags() {
            let cpu = setup_test!(0xe9, 0x08, 0x00);

            cpu.assert_accumulator_value(0xf7);
            cpu.assert_status_clear(StatusFlags::Overflow);
            cpu.assert_status_clear(StatusFlags::Carry);
        }
    }

    #[cfg(test)]
    mod sec {
        use crate::hardware::cpu::StatusFlags::Carry;

        #[test]
        fn test_0x38_sec() {
            let cpu = setup_test!(0x38, 0x00);

            cpu.assert_status_set(Carry)
        }
    }

    #[cfg(test)]
    mod sed {
        use crate::hardware::cpu::StatusFlags::DecimalMode;

        #[test]
        fn test_0xf8_sed() {
            let cpu = setup_test!(0xf8, 0x00);

            cpu.assert_status_set(DecimalMode)
        }
    }

    #[cfg(test)]
    mod sei {
        use crate::hardware::cpu::StatusFlags::InterruptDisable;

        #[test]
        fn test_0x78_sei() {
            let cpu = setup_test!(0x78, 0x00);

            cpu.assert_status_set(InterruptDisable)
        }
    }

    #[cfg(test)]
    mod sta {
        #[test]
        fn test_0x85_sta() {
            let cpu = setup_test!(0xa9, 0x50, 0x85, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x50)
        }
    }

    #[cfg(test)]
    mod stx {
        #[test]
        fn test_0x86_stx() {
            let cpu = setup_test!(0xa2, 0x50, 0x86, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x50);
        }
    }

    #[cfg(test)]
    mod sty {
        #[test]
        fn test_0x84_sty() {
            let cpu = setup_test!(0xa0, 0x50, 0x84, 0x01, 0x00);

            cpu.assert_memory_value(0x01, 0x50);
        }
    }

    #[cfg(test)]
    mod tax {
        #[test]
        fn test_0xaa_tax() {
            let cpu = setup_test!(0xa9, 10, 0xaa, 0x00);

            cpu.assert_reg_x_value(10);
        }
    }

    #[cfg(test)]
    mod tay {
        #[test]
        fn test_0xa8_tay() {
            let cpu = setup_test!(0xa9, 10, 0xa8, 0x00);

            cpu.assert_reg_y_value(10);
        }
    }

    #[cfg(test)]
    mod tsx {
        #[test]
        fn test_0xba_tsx() {
            let cpu = setup_test!(0xa9, 0x99, 0x48, 0xba, 0x00);

            cpu.assert_reg_x_value(0xfe)
        }
    }

    #[cfg(test)]
    mod txa {
        #[test]
        fn test_0x8a_txa() {
            let cpu = setup_test!(0xa2, 10, 0x8a, 0x00);

            cpu.assert_accumulator_value(10);
        }
    }

    #[cfg(test)]
    mod txs {
        #[test]
        fn test_0x9a_txs() {
            let cpu = setup_test!(0xa2, 0xfe, 0xa9, 0x99, 0x48, 0x68, 0x9a, 0x00);

            cpu.assert_stack_values(vec!(0x99));
            cpu.assert_stack_pointer_value(0xfe);
        }
    }

    #[cfg(test)]
    mod tya {
        #[test]
        fn test_0x98_tya() {
            let cpu = setup_test!(0xa0, 0x99, 0x98, 0x00);

            cpu.assert_accumulator_value(0x99);
        }
    }
}
