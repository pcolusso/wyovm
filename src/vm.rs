use std::ops::{Index, IndexMut};
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use tracing::{debug, error, info};

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
    let mut x = x;
    if x >> (bit_count - 1) & 1 != 0 {
        x |= 0xFFFF << bit_count;
    }
    x
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

        // Common for both modes
        let destination_register = to_reg((instruction >> 9) & 0x7);
        let source_register = to_reg((instruction >> 6) & 0x7);
        // otherwise register mode.
        let is_imm_mode = (instruction >> 5) & 0x1; 
        // alternativley, imm_mode = (instruction >> 5) & 0x1
        info!("Source: {:?} Dest: {:?} Mode: {:?}", source_register, destination_register, is_imm_mode);

        if is_imm_mode == 0 {
            let source_register_2 = to_reg(instruction & 0x7);
            self[Register::R0] =
                self[source_register] + self[source_register_2];
        } else {
            let imm5 = sign_extend(instruction & 0x14, 5);
            self[destination_register] = self[source_register] + imm5;
        }

        self.update_flags();
    }

    pub fn run(&mut self) -> () {
        let running = 1;
        loop {
            // TODO: What is this supposed to mean?
            // FETCH
            let instruction = self.incr_pc();
            let op = instruction >> 12;
            match Op::from_shifted_u16(op) {
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

#[repr(u16)]
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

impl Op {
    fn from_shifted_u16(value: u16) -> Option<Op> {
        // Safety: We know Op variants are 0-15
        if value <= 0xF {
            // Safe because we verified value is in range
            Some(unsafe { std::mem::transmute(value) })
        } else {
            None
        }
    }
}

enum Condition {
    Positive = 1 << 0,
    Negative = 1 << 1,
    Zero = 1 << 2,
}

#[cfg(test)]
mod tests {
    use super::*;
    use test_log::test;

    #[test]
    fn test_shifting_op() {
        for i in 0..=0xF {
            let target: Op = unsafe { std::mem::transmute(i) };
            let packed = i << 12;
            let shifted = packed >> 12; // This does absolute jack shit, you know that right?
            let unpacked = Op::from_shifted_u16(shifted).expect("???");
            assert!(matches!(target, unpacked));
        }
    }

    #[test]
    fn test_add_reg_instr() {
        let mut m = Machine::new();

        m[Register::R1] = 5;
        m[Register::R2] = 3;

        //                       ADD  R0  R1  - --  R2
        let instruction = 0b0001_000_001_0_00_010;

        m.add(instruction);

        assert_eq!(m[Register::R0], 8);
        assert_eq!(m[Register::Cond], 1); // positive
    }

    #[test]
    fn test_add_imm_instr() {
        let mut m = Machine::new();

        m[Register::R1] = 5;
        let instruction = 0b0001_000_001_1_00011;
        m.add(instruction);

        assert_eq!(m[Register::R0], 8);
        assert_eq!(m[Register::Cond], 1); // positive
   }

   #[test]
   fn test_add_negative_result() {
       let mut m = Machine::new();
       
       // Set up for a negative result
       m[Register::R1] = 5;
       
       // Add -10 (in 5-bit two's complement)
       let instruction = 0b0001_000_001_1_10110; // -10 in 5 bits
       
       m.add(instruction);
       
       assert_eq!(m[Register::R0] as i16, -5);
       // Test negative flag
       assert_eq!(m[Register::Cond], 4); // Negative flag
   }
}
