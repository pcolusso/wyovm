use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::FromPrimitive;
use tracing::error;

const MEM_MAX: usize = 1 << 16;
const PC_START: u16 = 0x3000;

pub struct Machine {
    mem: [u16; MEM_MAX],
    reg: [u16; 11],
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
        *(machine.r(Register::Cond)) = Condition::Zero as u16;
        // set the PC to the starting position
        *(machine.r(Register::PC)) = PC_START;

        machine
    }

    fn r(&mut self, register: Register) -> &mut u16 {
        match register {
            Register::R0 => &mut self.reg[0],
            Register::R1 => &mut self.reg[1],
            Register::R2 => &mut self.reg[2],
            Register::R3 => &mut self.reg[3],
            Register::R4 => &mut self.reg[4],
            Register::R5 => &mut self.reg[5],
            Register::R6 => &mut self.reg[6],
            Register::R7 => &mut self.reg[7],
            Register::PC => &mut self.reg[8],
            Register::Cond => &mut self.reg[9],
            Register::Count => &mut self.reg[10], // TODO: is count not actually a register?
        }
    }

    fn get_register(&self, register: Register) -> u16 {
        match register {
            Register::R0 => self.reg[0],
            Register::R1 => self.reg[1],
            Register::R2 => self.reg[2],
            Register::R3 => self.reg[3],
            Register::R4 => self.reg[4],
            Register::R5 => self.reg[5],
            Register::R6 => self.reg[6],
            Register::R7 => self.reg[7],
            Register::PC => self.reg[8],
            Register::Cond => self.reg[9],
            Register::Count => self.reg[10], // TODO: is count not actually a register?
        }
    }

    // Simulates a mem_read(reg[PC]++)
    fn incr_pc(&mut self) -> u16 {
        let current = self.get_register(Register::PC);
        *(self.r(Register::PC)) += 1;
        current
    }

    fn update_flags(&mut self) {
        let value = self.get_register(Register::R0);
        if value == 0 {
            //                          COUNTER STRIKE
            *(self.r(Register::Cond)) = Condition::Zero as u16
        } else if value >> 15 == 1 {
            *(self.r(Register::Cond)) = Condition::Negative as u16
        } else {
            *(self.r(Register::Cond)) = Condition::Positive as u16
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
        let is_register_mode = instruction >> 5 == 0;
        // alternativley, imm_mode = (instruction >> 5) & 0x1

        if is_register_mode {
            let source_register_2 = to_reg(instruction & 0x7);
            *(self.r(Register::R0)) =
                self.get_register(source_register) + self.get_register(source_register_2);
        } else {
            let imm5 = sign_extend(instruction & 0x14, 5);
            *(self.r(destination_register)) = self.get_register(source_register) + imm5;
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

#[derive(FromPrimitive, ToPrimitive)]
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

        m.reg[Register::R1 as usize] = 5;
        m.reg[Register::R2 as usize] = 3;

        //                       ADD  R0  R1  - --  R2
        let instruction = 0b0001_000_001_0_00_010;

        m.add(instruction);

        assert_eq!(m.reg[Register::R0 as usize], 8);
        assert_eq!(m.reg[Register::Cond as usize], 1); // positive
    }
}
