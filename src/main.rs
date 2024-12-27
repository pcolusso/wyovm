use clap::Parser;
use thiserror::Error;
use tracing::{error, info};

const MEM_MAX: usize = 1 << 16;
const PC_START: u16 = 0x3000;

struct Machine {
    mem: [u16; MEM_MAX],
    reg: [u16; 11],
}

impl Machine {
    fn new() -> Self {
        let mem = [0; MEM_MAX];
        let reg = [0; 11];
        let mut machine = Self { mem, reg };

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

    fn run(&mut self) -> () {
        let mut running = 1;
        while running > 0 {
            // TODO: What is this supposed to mean?
            // FETCH
            let instruction = self.incr_pc();
            let op = instruction >> 12;
            match Op::from_shifted_u16(op) {
                Some(Op::Add) => {}
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

#[derive(Parser)]
struct App {
    image_path: std::path::PathBuf,
}

#[derive(Error, Debug)]
enum AppError {
    #[error("I/O Error")]
    Io(#[from] std::io::Error),
}

fn main() -> Result<(), AppError> {
    tracing_subscriber::fmt::init();
    // TODO: https://www.jmeiners.com/lc3-vm/#:setup

    let app = App::parse();
    let code = std::fs::read(app.image_path)?;

    let mut machine = Machine::new();

    info!("done!");

    Ok(())
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
}
