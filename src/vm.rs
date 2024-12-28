#![allow(dead_code)]

use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use std::ops::{Index, IndexMut};
use tracing::{debug, error};

use crate::util::Extractable;

const MEM_MAX: usize = 1 << 16;
const PC_START: u16 = 0x3000;

pub struct Machine {
    mem: [u16; MEM_MAX],
    reg: [u16; 11],
}

impl Index<Register> for Machine {
    type Output = u16;

    fn index(&self, r: Register) -> &Self::Output {
        &self.reg[r as usize]
    }
}

impl IndexMut<Register> for Machine {
    fn index_mut(&mut self, register: Register) -> &mut Self::Output {
        &mut self.reg[register as usize]
    }
}

// further reading: https://en.wikipedia.org/wiki/Two%27s_complement
fn sign_extend(x: u16, bit_count: i32) -> u16 {
    let mut y = x;
    if y >> (bit_count - 1) & 1 != 0 {
        y |= 0xFFFFu16 << bit_count;
    }
    debug!("+/- {:#016b} -> {:#016b}", x, y);
    y
}

impl Machine {
    pub fn new() -> Self {
        let mem = [0; MEM_MAX];
        let reg = [0; 11];
        let mut machine = Self { mem, reg };

        // TODO:: this may be better moved into the run method.
        // since exactly one condition flag should be set at any given time, set the Z flag
        machine[Register::Cond] = Condition::Zero as u16;
        // set the PC to the starting position
        machine[Register::PC] = PC_START;

        machine
    }

    // Simulates a mem_read(reg[PC]++)
    fn incr_pc(&mut self) -> u16 {
        let current = self[Register::PC];
        self[Register::PC] += 1;
        current
    }

    // update the condition register
    fn update_flags(&mut self) {
        let value = self[Register::R0];
        self[Register::Cond] = if value == 0 {
            //COUNTER STRIKE
            Condition::Zero as u16
        } else if value >> 15 == 1 {
            Condition::Negative as u16
        } else {
            Condition::Positive as u16
        }
    }

    fn add(&mut self, instruction: u16) {
        // Add has two modes, immediate and register mode.

        /*
                    ╔══════════════════╗
                    ║  REGISTER MODE   ║
                    ╚══════════════════╝
         ┌──────────┬───────┬───────┬─┬────┬───────┐
         │   0001   │   DR  │  SR1  │0│ 00 │  SR2  │
         ├──────────┼───────┼───────┼─┼────┼───────┤
         │          │       │       │ │    │       │

                    ╔══════════════════╗
                    ║  IMMEDIATE MODE  ║
                    ╚══════════════════╝
         ┌──────────┬───────┬───────┬─┬────────────┐
         │   0001   │   DR  │  SR1  │1│    imm5    │
         ├──────────┼───────┼───────┼─┼────────────┤
         │          │       │       │ │            │
        */
        debug!("ADD {:#016b}", instruction);
        // Common for both modes
        let destination_register = to_reg(instruction.extract(9..=11));
        let source_register = to_reg(instruction.extract(6..=8));
        // otherwise register mode.
        let is_imm_mode = (instruction >> 5) & 0x1;
        // alternativley, imm_mode = (instruction >> 5) & 0x1

        if is_imm_mode == 0 {
            let source_register_2 = to_reg(instruction & 0x7);
            debug!(
                "ADD REG SR1 {:?} + SR2 {:?} -> DR {:?}",
                source_register, source_register_2, destination_register
            );
            self[destination_register] = self[source_register] + self[source_register_2];
        } else {
            let imm5 = sign_extend(instruction.extract(0..=5), 5);
            //let imm5 = sign_extend(instruction & 0x1F, 5);
            self[destination_register] = self[source_register] + imm5; // Simple addition in u16
        }

        self.update_flags();
    }

    fn load_indirect(&mut self, instruction: u16) {
        /*
         ┌──────────┬───────┬─────────────────────────┐
         │   1010   │  DR   │       PCOffset9         │
         ├──────────┼───────┼─────────────────────────┤
         │          │       │                         │
        */
        let destination_register = to_reg(instruction.extract(9..=11));
        let pc_offset_9 = sign_extend(instruction.extract(0..=8), 9);
        let idx = self[Register::PC] + pc_offset_9;
        debug!(
            "LDI PC {} + {} = {} -> {:?}",
            self[Register::PC],
            pc_offset_9,
            idx,
            destination_register
        );
        self[destination_register] = self.mem[idx as usize];
        self.update_flags();
    }

    pub fn run(&mut self) {
        let running = 1;
        loop {
            // TODO: What is this supposed to mean?
            // FETCH
            let instruction = self.incr_pc();
            let op = instruction >> 12;
            match Op::from_u16(op) {
                Some(Op::Add) => self.add(instruction),
                Some(Op::And) => {}
                Some(Op::Branch) => {}
                Some(Op::Jump) => {}
                Some(Op::JumpRegister) => {}
                Some(Op::Load) => {}
                Some(Op::LoadIndirect) => {}
                Some(Op::LoadEffectiveAddress) => {}
                None => error!(
                    "Failed to decode op! PC was {:x}, shifted we got {:x}",
                    instruction, op
                ),
                _ => todo!(),
            }
        }
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(FromPrimitive, ToPrimitive, Debug)]
enum Register {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    PC,
    Cond,
    Count,
}

// Helper function to hammer a u16 (likely masked) into a Register.
fn to_reg(value: u16) -> Register {
    Register::from_u16(value).expect("Couldn't find register with value. Has it been shifted?")
}

#[derive(FromPrimitive, ToPrimitive, Debug)]
enum Op {
    Branch = 0x0,         // BR
    Add,                  // ADD
    Load,                 // LD
    Store,                // ST
    JumpRegister,         // JSR
    And,                  // AND
    LoadRegister,         // LDR
    StoreRegister,        // STR
    Unused,               // RTI
    Not,                  // NOT
    LoadIndirect,         // LDI
    StoreIndirect,        // STI
    Jump,                 // JMP
    Reserved,             // RES
    LoadEffectiveAddress, // LEA
    Trap,                 // TRAP
}

// TODO: if this register is never used for anything else, maybe we could change this into an enum?
#[derive(ToPrimitive, FromPrimitive, Debug)]
enum Condition {
    Positive = 1 << 0,
    Negative = 1 << 1,
    Zero = 1 << 2,
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unusual_byte_groupings)]
    use super::*;
    use num_traits::ToPrimitive;
    use test_log::test;
    use tracing::info;

    #[test]
    fn test_add_reg_instr() {
        let mut m = Machine::new();

        m[Register::R0] = 5;
        m[Register::R1] = 3;

        //                  0001  DR SR1 0 00 SR2
        //                   ADD  R2  R0 R --  R2
        let instruction = 0b0001_010_000_0_00_001;
        info!("🔎: ADD REG R0 + R1 -> R2");
        m.add(instruction);

        assert_eq!(m[Register::R2], 8);
        assert_eq!(m[Register::Cond], 1); // positive
    }

    #[test]
    fn test_add_imm_instr() {
        let mut m = Machine::new();

        m[Register::R1] = 5;
        //                   ADD  R0  R1 I     3
        let instruction = 0b0001_000_001_1_00011;
        info!("🔎: ADD IMM 3 + R1 -> R0");
        m.add(instruction);

        assert_eq!(m[Register::R0], 8);
        assert_eq!(m[Register::Cond], 1); // positive
    }

    #[test]
    fn test_add_negative_result() {
        let mut m = Machine::new();

        // Set up for a negative result
        m[Register::R1] = 5;

        //                   ADD  R0  R1 I -  10
        let instruction = 0b0001_000_001_1_11001;
        info!("🔎: ADD IMM -7 + R1 -> R0");
        m.add(instruction);

        debug!("R0 as u16: {}", m[Register::R0]);
        debug!("R0 as i16: {}", m[Register::R0] as i16);

        assert_eq!(m[Register::R0] as i16, -2);
        // Test negative flag
        assert_eq!(m[Register::Cond], Condition::Negative.to_u16().unwrap()); // Negative flag
    }

    #[test]
    fn test_ldi() {
        let mut m = Machine::new();
        m.mem[6969] = 'a' as u16; // Far data
        m[Register::PC] = 6900;

        //                   LDI  R0       69
        let instruction = 0b0101_000_01000101;
        info!("🔎: LDI R0 PC + 69 -> R0");
        m.load_indirect(instruction);

        assert_eq!(m[Register::R0], 'a' as u16);
        assert_eq!(m[Register::Cond], 1); // positive
    }
}
