#![allow(dead_code)]

use byteorder::{BigEndian, ReadBytesExt};
use num_derive::{FromPrimitive, ToPrimitive};
use num_traits::{FromPrimitive, ToPrimitive};
use std::{
    cmp::Ordering,
    io::{self, Read, Write},
    ops::{Index, IndexMut},
};
use tracing::{debug, error, trace};

use crate::util::{sign_extend, Extractable};

pub const MEM_MAX: usize = 1 << 16;
pub const PC_START: u16 = 0x3000;

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
#[derive(ToPrimitive, FromPrimitive, Debug, PartialEq)]
enum Condition {
    Positive = 1 << 0,
    Negative = 1 << 1,
    Zero = 1 << 2,
}
#[derive(Debug, ToPrimitive, FromPrimitive)]
enum TrapCode {
    GetChar = 0x20,
    Out,
    Put,
    In,
    PutSP,
    Halt,
}

#[derive(FromPrimitive, ToPrimitive, Debug, Copy, Clone)]
pub enum Register {
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
pub struct Machine {
    pub mem: [u16; MEM_MAX],
    pub reg: [u16; 11],
    pub running: bool,
    out: Box<dyn Write>,
}

// not too sure if worthwhile implementing Index<u16> for mem access.
// Registers can sometimes come from u16, so it could be confusing.
// Maybe a newtype?
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

impl Machine {
    pub fn new(out: Box<dyn Write>) -> Self {
        let mem = [0; MEM_MAX];
        let reg = [0; 11];
        let running = true;
        let mut machine = Self { mem, reg, running, out };

        // TODO:: this may be better moved into the run method.
        // since exactly one condition flag should be set at any given time, set the Z flag
        machine[Register::Cond] = Condition::Zero as u16;
        // set the PC to the starting position
        machine[Register::PC] = PC_START;

        machine
    }

    pub fn load_image(&mut self, mut source: impl Read) {
        let origin = source
            .read_u16::<BigEndian>()
            .expect("can't read origin");
        debug!("Origin is 0x{:04x}", origin);

        let mut current_addr = origin as usize;
        while current_addr < MEM_MAX {
            if let Ok(value) = source.read_u16::<BigEndian>() {
                let op = Op::from_u16(value >> 12);
                debug!("Read 0x{:04x}, {:?}", value, op);
                self.mem[current_addr] = value;
                current_addr += 1;
            } else {
                break;
            }
        }

        self[Register::PC] = origin;
    }

    // update the condition register
    fn update_flags(&mut self, value: u16) {
        let value = value as i8;
        debug!("COND: Got {}", value);
        self[Register::Cond] = match value.cmp(&0) {
            Ordering::Equal => Condition::Zero,
            Ordering::Less => Condition::Negative,
            Ordering::Greater => Condition::Positive,
        } as u16;
    }

    // Convert the Condition register into the enum
    fn get_condition(&self) -> Condition {
        Condition::from_u16(self[Register::Cond]).expect("Invalid value in condition register")
    }

    // Indirection for accessing memory. We need to add memory-mapped regions for keyboard
    // input.
    fn mem_read(&mut self, addr: u16) -> u16 {
        if addr == 0xFE00 {
            let mut buffer = [0; 1];
            io::stdin()
                .read_exact(&mut buffer)
                .expect("Failed to read input");
            self.mem[0xFE00] = 1 << 15;
            self.mem[0xFE02] = buffer[0] as u16;
        } else {
            self.mem[0xFE00] = 0;
        }
        self.mem[addr as usize]
    }

    // MARK: Instruction implementations

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
            self[destination_register] = self[source_register].wrapping_add(self[source_register_2]);
        } else {
            let imm5 = sign_extend(instruction.extract(0..=4), 5);
            //let imm5 = sign_extend(instruction & 0x1F, 5);
            self[destination_register] = self[source_register].wrapping_add(imm5);
            debug!(
                "ADD IMM {} + {:?} -> {:?}",
                imm5, source_register, destination_register
            );
        }
        // Is this correct?
        self.update_flags(self[destination_register]);
    }

    fn jump(&mut self, instruction: u16) {
        let base_register = to_reg(instruction.extract(6..=9));
        let value = self[base_register];
        debug!("JMP Set PC to {} from {:?}", value, base_register);
        self[Register::PC] = value;
    }

    fn jump_register(&mut self, instruction: u16) {
        let go_long = instruction.extract_flag(11);
        self[Register::R7] = self[Register::PC];
        if go_long {
            let offset = sign_extend(instruction.extract(0..=10), 11);
            self[Register::PC] = offset.wrapping_add(self[Register::PC]);
            debug!(
                "JSR {} + {} ~> {}",
                self[Register::R7],
                offset,
                self[Register::PC]
            );
        } else {
            let base_register = to_reg(instruction.extract(6..=8));
            self[Register::PC] = self[base_register];
            debug!(
                "JSRR Set PC to value in {:?} ({})",
                base_register, self[base_register]
            );
        }
    }

    fn branch(&mut self, instruction: u16) {
        let n = instruction.extract_flag(11);
        let z = instruction.extract_flag(10);
        let p = instruction.extract_flag(9);
        let pc_offset = sign_extend(instruction.extract(0..=8), 9);
        let c = self[Register::Cond];

        let bn = n && c == Condition::Negative.to_u16().unwrap();
        let bp = p && c == Condition::Positive.to_u16().unwrap();
        let bz = z && c == Condition::Zero.to_u16().unwrap();

        debug!("BR n {} p {} z {} c {}", bn, bp, bz, self[Register::Cond]);
        if bn || bp || bz {
            self[Register::PC] = pc_offset.wrapping_add(self[Register::PC]);
            debug!("BR ~> {:0x}", self[Register::PC]);
        }
    }

    fn load(&mut self, instruction: u16) {
        let dr = instruction.extract(9..=12); // WHYYYYY
        debug!("INSR: {:016b}", instruction);
        debug!("DR: {:08b}", dr);
        let destination_register = to_reg(dr);
        let offset = instruction.extract(0..=9);
        let signed = sign_extend(offset, 9);
        let addr = signed.wrapping_add(self[Register::PC]);

        self[destination_register] = self.mem_read(addr);
        debug!(
            "LD Reading from 0x{:0x} ({}) into {:?}",
            addr,
            self.mem_read(addr),
            destination_register
        );
    }

    fn load_indirect(&mut self, instruction: u16) {
        /*
         ┌──────────┬───────┬─────────────────────────┐
         │   1010   │  DR   │       PCOffset9         │
         ├──────────┼───────┼─────────────────────────┤
         │          │       │                         │
        */
        debug!("{:016b}", instruction);
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
        self[destination_register] = self.mem_read(idx);
        self.update_flags(self[destination_register]);
    }

    fn load_effective_address(&mut self, instruction: u16) {
        assert!(instruction.extract(12..=15) == 0b1110);
        let dr = instruction.extract(9..=11);
        let destination_register = to_reg(dr);
        let offset = sign_extend(instruction.extract(0..=8), 9);
        let addr = offset.wrapping_add(self[Register::PC]);
        self[destination_register] = addr;
        self.update_flags(addr);
        debug!(
            "LEA 0x{:0x} -> {:?} ({:?})",
            addr,
            destination_register,
            self[Register::Cond]
        );
    }

    fn load_register(&mut self, instruction: u16) {
        assert!(instruction.extract(12..=15) == 0b0110);
        let dr = instruction.extract(9..=11);
        let br = instruction.extract(6..=8);
        let offset = sign_extend(instruction.extract(0..=5), 6);
        let destination_register = to_reg(dr);
        let base_register = to_reg(br);
        let address = self[base_register].wrapping_add(offset);
        self[destination_register] = self.mem_read(address);
        self.update_flags(self[destination_register]);
    }

    fn store(&mut self, instruction: u16) {
        assert!(instruction.extract(12..=15) == 0b0011);
        let sr = instruction.extract(9..=11);
        let source_register = to_reg(sr);
        let offset = sign_extend(instruction.extract(0..=8), 9);
        let addr = offset.wrapping_add(self[Register::PC]);
        self.mem[addr as usize] = self[source_register];
        debug!(
            "ST {:?} ({}) -> 0x{:0x}",
            source_register, self[source_register], addr
        );
    }

    fn store_indirect(&mut self, instruction: u16) {
        assert!(instruction.extract(12..=15) == 0b1011);
        let sr = instruction.extract(9..=11);
        let source_register = to_reg(sr);
        let offset = sign_extend(instruction.extract(0..=8), 9);
        let addr = offset.wrapping_add(self[Register::PC]);
        let actual_address = self.mem_read(addr);
        self.mem[actual_address as usize] = self[source_register];
    }

    fn store_register(&mut self, instruction: u16) {
        assert!(instruction.extract(12..=15) == 0b0111);
        let sr = instruction.extract(9..=11);
        let source_register = to_reg(sr);
        let br = instruction.extract(6..=8);
        let base_register = to_reg(br);
        let offset = sign_extend(instruction.extract(0..=5), 6);
        let addr = offset.wrapping_add(self[base_register]);
        self.mem[addr as usize] = self[source_register];
    }

    fn bitwise_not(&mut self, instruction: u16) {
        let destination_register = to_reg(instruction.extract(9..=11));
        let source_register = to_reg(instruction.extract(6..=8));
        self[destination_register] = !self[source_register];
        debug!(
            "NOT ! {:?} ({:08b}) -> {:?} ({:08b})",
            source_register,
            self[source_register],
            destination_register,
            self[destination_register]
        );
        self.update_flags(self[destination_register]);
    }

    fn bitwise_and(&mut self, instruction: u16) {
        // im gonna stop doing the ascii diagrams for now, as they don't actually tell me the info
        // i need.
        // REF: Pg525
        let destination_register = to_reg(instruction.extract(9..=11));
        let source_register_1 = to_reg(instruction.extract(6..=8));
        let register_mode = instruction.extract_flag(5);
        debug!("AND INSTR: {:016b}", instruction);
        debug!(
            "AND DR {:03b}, SR1 {:08b}",
            instruction.extract(9..=11),
            instruction.extract(6..=9)
        );

        if register_mode {
            // register mode
            let source_register_2 = to_reg(instruction.extract(0..=3));
            self[destination_register] = self[source_register_1] & self[source_register_2];
            debug!(
                "AND REG {:?} ({}) & {:?} ({}) -> {:?} ({})",
                source_register_1,
                self[source_register_1],
                source_register_2,
                self[source_register_2],
                destination_register,
                self[destination_register]
            );
        } else {
            // immediate mode
            let imm = sign_extend(instruction.extract(0..=5), 5);
            self[destination_register] = self[source_register_1] & imm;
            debug!(
                "AND IMM {:?} ({}) & {} -> {:?} ({})",
                source_register_1,
                self[source_register_1],
                imm,
                destination_register,
                self[destination_register]
            );
        }

        self.update_flags(self[destination_register]);
    }

    fn trap(&mut self, instruction: u16) {
        assert!(instruction.extract(12..=15) == 0b1111);
        let vec = instruction.extract(0..=7);
        self[Register::R7] = self[Register::PC];
        let trap = TrapCode::from_u16(vec);
        debug!("TRAP: {:?}", trap);
        match trap {
            Some(TrapCode::GetChar) => {
                let mut buffer = [0; 1];
                io::stdin()
                    .read_exact(&mut buffer)
                    .expect("Failed to read input");
                self[Register::R0] = buffer[0] as u16;
                self.update_flags(self[Register::R0]);
            }
            Some(TrapCode::Out) => {
                let char = self[Register::R0] as u8 as char;
                write!(self.out, "{}", char);
                io::stdout().flush().expect("plumbing failure");
            }
            Some(TrapCode::In) => {
                write!(self.out, "Enter a character: ");
                let mut buffer = [0; 1];
                io::stdin()
                    .read_exact(&mut buffer)
                    .expect("Failed to read input");
                write!(self.out, "{}", buffer[0]);
                self[Register::R0] = buffer[0] as u16;
                self.update_flags(self[Register::R0]);
            }
            Some(TrapCode::Put) => {
                let start = self[Register::R0] as usize;
                let end = self.mem[start..MEM_MAX].iter().position(|c| *c == 0).expect("Unterminated string");
                let string: String = self.mem[start..start+end].iter().map(|&x| char::from(x as u8)).collect();
                writeln!(self.out, "{}", string);
            }
            Some(TrapCode::PutSP) => {
                let addr = self[Register::R0] as usize;
                let mut c: &[u16] = &self.mem[addr..];

                // TODO: do better
                while let Some(&word) = c.first() {
                    if word == 0 {
                        break; // Stop if the current word is zero
                    }

                    // Extract and print the first char (lower byte)
                    let char1 = (word & 0xFF) as u8 as char;
                    write!(self.out, "{}", char1);

                    // Extract and print the second char (upper byte), if it's not zero
                    let char2 = (word >> 8) as u8;
                    if char2 != 0 {
                        write!(self.out, "{}", char2 as char);
                    }

                    // Move to the next memory location
                    c = &c[1..];
                }

                // Flush the output to ensure all characters are printed
                io::stdout().flush().expect("Failed to flush stdout");
            }
            Some(TrapCode::Halt) => self.running = false,
            None => panic!("bad trap"),
        }
    }

    // Simulates a mem_read(reg[PC]++)
    fn incr_pc(&mut self) -> u16 {
        let current = self.mem_read(self[Register::PC]);
        self[Register::PC] += 1;
        current
    }

    pub fn run(&mut self) {
        while self.running {
            self.cycle();
        }
    }

    pub fn cycle(&mut self) {
        // TODO: Perhaps a Instruction newtype, that Op can From?
        let instruction = self.incr_pc();
        trace!("Loaded: 0x{:0x}", instruction);
        trace!(
            "PC: 0x{:0x} R0: 0x{:0x} R1 0x{:0x}",
            self[Register::PC],
            self[Register::R0],
            self[Register::R1]
        );
        let op = instruction >> 12;
        match Op::from_u16(op) {
            Some(Op::Add) => self.add(instruction),
            Some(Op::And) => self.bitwise_and(instruction),
            Some(Op::Not) => self.bitwise_not(instruction),
            Some(Op::Branch) => self.branch(instruction),
            Some(Op::Jump) => self.jump(instruction),
            Some(Op::JumpRegister) => self.jump_register(instruction),
            Some(Op::Load) => self.load(instruction),
            Some(Op::LoadIndirect) => self.load_indirect(instruction),
            Some(Op::LoadEffectiveAddress) => self.load_effective_address(instruction),
            Some(Op::Store) => self.store(instruction),
            Some(Op::StoreRegister) => self.store_register(instruction),
            Some(Op::StoreIndirect) => self.store_indirect(instruction),
            Some(Op::LoadRegister) => self.load_register(instruction),
            Some(Op::Unused) => panic!("Unused"),
            Some(Op::Reserved) => panic!("Reserved"),
            Some(Op::Trap) => self.trap(instruction),
            None => error!(
                "Failed to decode op! PC was {:x}, shifted we got {:x}",
                instruction, op
            ),
        }
    }
}

impl Default for Machine {
    fn default() -> Self {
        Self::new(Box::new(std::io::stdout()))
    }
}

// Helper function to hammer a u16 (likely masked) into a Register.
fn to_reg(value: u16) -> Register {
    Register::from_u16(value).expect("Couldn't find register with value. Has it been shifted?")
}

#[cfg(test)]
mod tests {
    #![allow(clippy::unusual_byte_groupings)]
    use super::*;
    use num_traits::ToPrimitive;
    use test_log::test;
    use tracing::info;

    #[test]
    fn add_register() {
        let mut m = Machine::default();

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
    fn add_immediate() {
        let mut m = Machine::default();

        m[Register::R1] = 5;
        //                   ADD  R0  R1 I     3
        let instruction = 0b0001_000_001_1_00011;
        info!("🔎: ADD IMM 3 + R1 -> R0");
        m.add(instruction);

        assert_eq!(m[Register::R0], 8);
        assert_eq!(m[Register::Cond], 1); // positive
    }

    #[test]
    fn add_negative() {
        let mut m = Machine::default();

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
    fn load_indirect() {
        let mut m = Machine::default();
        m.mem[6969] = 'a' as u16; // Far data
        m[Register::PC] = 6900;

        //                   LDI  R0       69
        let instruction = 0b0101_000_001000101;
        info!("🔎: LDI R0 PC + 69 -> R0");
        m.load_indirect(instruction);

        assert_eq!(m[Register::R0], 'a' as u16);
        assert_eq!(m[Register::Cond], 1); // positive
    }

    #[test]
    fn and_register_mode() {
        let mut m = Machine::default();
        m[Register::R7] = 7;
        m[Register::R5] = 5;

        //                   AND  R2  R7_0_00_ R5
        let instruction = 0b1010_010_111_0_00_101;
        info!("🇸🇽: AND REG R7 (7) & R5(5) -> R2 (5)");
        m.bitwise_and(instruction);

        assert_eq!(m[Register::R2], 5);
        assert_eq!(m[Register::Cond], 1);
    }

    #[test]
    fn test_and_immediate() {
        let mut m = Machine::default();
        m[Register::R6] = 4;

        //                   AND  R3  R6     9
        let instruction = 0b1010_011_110_001001;
        info!("🔎: AND IMM R6 (4) & 9 -> R3 (0)");
        m.bitwise_and(instruction);

        assert_eq!(m[Register::R3], 0);
        assert_eq!(m[Register::Cond], Condition::Zero.to_u16().unwrap());
    }

    #[test]
    fn not() {
        let mut m = Machine::default();
        m[Register::R2] = 12;

        //                   NOT  R3  R2 111111
        let instruction = 0b1001_011_010_111111;
        info!("🔎: NOT R2 (12) -> R3()");
        m.bitwise_not(instruction);

        assert_eq!(m[Register::R3], 0b1111_1111_1111_0011);
        assert_eq!(
            m[Register::Cond],
            Condition::Negative.to_u16().unwrap(), // I guess it's negative because the orig was
            // +ve?
            "Condition flag"
        );
    }

    #[test]
    fn branch_positive() {
        let mut m = Machine::default();
        let offset = 0x44;
        m[Register::PC] = 0x23;
        let dest = offset + m[Register::PC];
        m[Register::Cond] = Condition::Positive.to_u16().unwrap();

        //                    BR n z p
        let instruction = 0b0000_0_0_1_000000000 + offset;
        info!("BR + {:0x} ~> {:0x}", m[Register::PC], dest);
        m.branch(instruction);

        assert_eq!(m[Register::PC], dest);
    }

    #[test]
    fn branch_negative() {
        let mut m = Machine::default();
        let offset = 12_i16 as u8; // can't seem to branch negative.
        m[Register::PC] = 52;
        let dest = 64;
        m[Register::Cond] = Condition::Negative.to_u16().unwrap();

        //                    BR n z p
        let instruction: u16 = 0b0000_1_0_0_000000000 | offset as u16;
        info!("BR + {:0x} ~> {:0x}", m[Register::PC], dest);
        m.branch(instruction);

        assert_eq!(m[Register::PC], dest);
    }

    #[test]
    fn do_not_branch() {
        let mut m = Machine::default();
        let offset = 12_i16 as u8; // can't seem to branch negative.
        m[Register::PC] = 52;
        let dest = 64;
        m[Register::Cond] = Condition::Positive.to_u16().unwrap();

        //                    BR n z p
        let instruction = 0b0000_1_0_0_000000000 | offset as u16;
        info!("BR + {:0x} ~> {:0x}", m[Register::PC], dest);
        m.branch(instruction);

        assert_eq!(m[Register::PC], 52);
    }

    #[test]
    fn jump() {
        let mut m = Machine::default();
        let dest = 0x98;
        m[Register::PC] = 0x69;
        m[Register::R6] = dest;

        //                  JMP 000  R6 000000
        let instuction = 0b1100_000_110_000000;
        info!("JMP Set PC to {} from R6", dest);
        m.jump(instuction);

        assert_eq!(m[Register::PC], dest);
    }

    // here be tests writtne by claude, cause it got tiring.
    #[test]
    fn jump_register_long_mode() {
        let mut m = Machine::default();
        m[Register::PC] = 0x6969;

        // JSR with offset of 5
        //                   JSR 1  offset=5
        let instruction = 0b0100_1_00000000101;
        info!("JSR jumping from {} with offset 5", 0x3100);

        m.jump_register(instruction);

        assert_eq!(m[Register::PC], 0x6969 + 5);
    }

    #[test]
    fn jump_register_reg_mode() {
        let mut m = Machine::default();
        m[Register::PC] = 0x3000;
        m[Register::R7] = 0x3100;
        m[Register::R3] = 0x4000;

        // JSRR using R3
        //                   JSRR 0  011 000000
        let instruction = 0b0100_0_00_011_000000;
        info!("JSRR jumping to value in R3 ({})", 0x4000);

        m.jump_register(instruction);

        assert_eq!(m[Register::PC], 0x4000);
    }

    #[test]
    fn jump_register_long_mode_negative_offset() {
        let mut m = Machine::default();
        m[Register::PC] = 0x3000;
        m[Register::R7] = 0x3100;

        // JSR with offset of -5
        //                   JSR 1  offset=-5 (11 bits)
        let instruction = 0b0100_1_11111111011;
        info!("JSR jumping from {} with offset -5", 0x3100);

        m.jump_register(instruction);

        assert_eq!(m[Register::PC], 0x3000 - 5);
    }

    #[test]
    fn jump_register_saves_r7() {
        let mut m = Machine::default();
        let original_pc = 0x3000;
        m[Register::PC] = original_pc;
        m[Register::R3] = 0x4000;

        // JSRR using R3
        //                   JSRR 0  011 000000
        let instruction = 0b0100_0_00_011_000000;
        info!(
            "JSRR jumping to value in R3 ({}) and saving PC ({}) to R7",
            0x4000, original_pc
        );

        m.jump_register(instruction);

        assert_eq!(m[Register::R7], original_pc);
    }

    #[test]
    fn load_positive_offset() {
        let mut m = Machine::default();
        // Set up initial state
        m[Register::PC] = 0x3000;
        // Put a known value in memory
        m.mem[0x3005] = 420;

        // Create instruction: LD R2, #5
        //                    LD  DR PCoffset9
        let instruction = 0b0010_010_000000101;
        m.load(instruction);

        // Verify R2 contains the value we loaded from memory
        assert_eq!(m[Register::R2], 420);
    }

    #[test]
    fn load_negative_offset() {
        let mut m = Machine::default();
        m[Register::PC] = 0x325;
        m.mem[0x320] = 0xBEEF;

        // Create instruction: LD R3, #-5
        //                 opcode DR  PCoffset9 (negative)
        let instruction = 0b0010_011_111111011;
        m.load(instruction);

        assert_eq!(m[Register::R3], 0xBEEF);
    }

    #[test]
    fn load_zero_offset() {
        let mut m = Machine::default();
        m[Register::PC] = 0x3000;
        m.mem[0x3000] = 0xABCD;

        // Create instruction: LD R4, #0
        //                 opcode DR  PCoffset9
        let instruction = 0b0010_100_000000000;
        m.load(instruction);

        assert_eq!(m[Register::R4], 0xABCD);
    }

    #[test]
    fn load_effective_address_positive_offset() {
        let mut m = Machine::default();
        m[Register::PC] = 0x3000;

        //                  LEA R1 offset=0x20
        let instruction = 0b1110_001_000100000;
        info!("LEA R1, #32 (Positive offset)");
        m.load_effective_address(instruction);

        assert_eq!(m[Register::R1], 0x3020);
        assert_eq!(m.get_condition(), Condition::Positive); // Should be positive
    }

    #[test]
    fn load_effective_address_negative_offset() {
        let mut m = Machine::default();
        m[Register::PC] = 0x3000;

        //                  LEA R2 offset=-16
        let instruction = 0b1110_010_111110000;
        info!("LEA R2, #-16 (Negative offset)");
        m.load_effective_address(instruction);

        assert_eq!(m[Register::R2], 0x2FF0); // 0x3000 + 1 - 16
        assert_eq!(m.get_condition(), Condition::Negative); // Should be negative
    }

    #[test]
    fn load_effective_address_zero_result() {
        let mut m = Machine::default();
        m[Register::PC] = 0x0011;

        let instruction = 0b1110_011_111101111;
        info!("LEA R3, #-17 (Should result in 0)");
        m.load_effective_address(instruction);

        assert_eq!(m[Register::R3], 0x0000);
        assert_eq!(m.get_condition(), Condition::Zero); // Should be zero
    }

    #[test]
    fn load_effective_address_max_positive_offset() {
        let mut m = Machine::default();
        m[Register::PC] = 0x3001;

        //                  LEA R4 offset=255
        let instruction = 0b1110_100_011111111;
        info!("LEA R4, #255 (Maximum positive offset)");
        m.load_effective_address(instruction);

        assert_eq!(m[Register::R4], 0x3100);
        assert_eq!(m.get_condition(), Condition::Zero);
    }

    #[test]
    fn store() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::PC] = 0x3000; // Some starting PC value
        m[Register::R2] = 0x4242; // Value to store
        let offset = 0x0012; // Positive offset

        //                  STR   R2  offset
        let instruction = 0b0011_010_000010010;
        info!(
            "STR R2 to memory location PC + {:#x} ({:#x})",
            offset,
            m[Register::PC].wrapping_add(offset)
        );

        m.store(instruction);

        // Check if value was stored at correct memory location
        assert_eq!(m.mem[m[Register::PC].wrapping_add(offset) as usize], 0x4242);
    }

    #[test]
    fn store_negative_offset() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::PC] = 0x3000; // Some starting PC value
        m[Register::R4] = 0xABCD; // Value to store
        let offset = -8i16 as u16; // Negative offset

        //                  STR   R4  offset (111111000 in binary)
        let instruction = 0b0011_100_111111000;
        info!(
            "STR R4 to memory location PC - 8 ({:#x})",
            m[Register::PC].wrapping_add(offset)
        );

        m.store(instruction);

        // Check if value was stored at correct memory location
        assert_eq!(m.mem[m[Register::PC].wrapping_add(offset) as usize], 0xABCD);
    }

    #[test]
    fn store_indirect() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::PC] = 0x3000; // Starting PC value
        m[Register::R3] = 0xBEEF; // Value to store
        let offset = 0x0001; // Offset to pointer
        let target_addr = 0x4000; // Where we actually want to store

        // Set up the pointer in memory
        m.mem[(m[Register::PC].wrapping_add(offset)) as usize] = target_addr;

        //                  STI   R3  offset
        let instruction = 0b1011_011_000000001;
        info!(
            "STI R3 using pointer at PC + {:#x}, storing to {:#x}",
            offset, target_addr
        );

        m.store_indirect(instruction);

        // Check if value was stored at the location pointed to
        assert_eq!(m.mem[target_addr as usize], 0xBEEF);
    }

    #[test]
    fn store_indirect_negative_offset() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::PC] = 0x3000; // Starting PC value
        m[Register::R5] = 0xCAFE; // Value to store
        let offset = -4i16 as u16; // Negative offset to pointer
        let target_addr = 0x5000; // Where we actually want to store

        // Set up the pointer in memory
        m.mem[(m[Register::PC].wrapping_add(offset)) as usize] = target_addr;

        //                  STI   R5  offset (111111100 in binary)
        let instruction = 0b1011_101_111111100;
        info!(
            "STI R5 using pointer at PC - 4, storing to {:#x}",
            target_addr
        );

        m.store_indirect(instruction);

        // Check if value was stored at the location pointed to
        assert_eq!(m.mem[target_addr as usize], 0xCAFE);
    }

    #[test]
    fn store_base() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::R2] = 0x3000; // Base register value
        m[Register::R4] = 0xDEAD; // Value to store
        let offset = 0x0A; // Positive offset

        //                  STR   R4    R2   offset
        let instruction = 0b0111_100_010_001010;
        info!(
            "STR R4 to memory location R2 + {:#x} ({:#x})",
            offset,
            m[Register::R2].wrapping_add(offset)
        );

        m.store_register(instruction);

        // Check if value was stored at correct memory location
        assert_eq!(m.mem[m[Register::R2].wrapping_add(offset) as usize], 0xDEAD);
    }

    #[test]
    fn store_base_negative_offset() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::R1] = 0x4000; // Base register value
        m[Register::R3] = 0xFACE; // Value to store
        let offset = -8i16 as u16; // Negative offset

        //                  STR   R3    R1   offset (111000 in binary)
        let instruction = 0b0111_011_001_111000;
        info!(
            "STR R3 to memory location R1 - 8 ({:#x})",
            m[Register::R1].wrapping_add(offset)
        );

        m.store_register(instruction);

        // Check if value was stored at correct memory location
        assert_eq!(m.mem[m[Register::R1].wrapping_add(offset) as usize], 0xFACE);
    }

    #[test]
    fn store_base_same_register() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::R5] = 0x2000; // Both base and value to store
        let offset = 0x0F; // Positive offset

        //                  STR   R5    R5   offset
        let instruction = 0b0111_101_101_001111;
        info!(
            "STR R5 to memory location R5 + {:#x} ({:#x})",
            offset,
            m[Register::R5].wrapping_add(offset)
        );

        m.store_register(instruction);

        // Check if value was stored at correct memory location
        assert_eq!(m.mem[m[Register::R5].wrapping_add(offset) as usize], 0x2000);
    }

    #[test]
    fn can_read_origin() {
        let mut m = Machine::default();
        let f = std::fs::File::open("samples/hello_world.obj").expect("hello world sample lost");
        m.load_image(f);
        assert_eq!(m.mem[0x3000], 0xE206, "start instr is not LEA R1 x3007");
    }

    #[test]
    fn load_register_positive_offset() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::R2] = 0x3000; // Base address in register R2
        m.mem[0x3002usize] = 0x1234; // Memory location to load from

        //                LDR   R1  R2  offset (10 in binary)
        let instruction = 0b0110_001_010_000010; // LDR R1, R2, #2

        info!("LDR R1 from (R2 + 2 = 0x3002), expects 0x1234");

        // Execute the instruction
        m.load_register(instruction);

        // Check if the value was loaded correctly
        assert_eq!(m[Register::R1], 0x1234);
    }

    #[test]
    fn load_register_negative_offset() {
        let mut m = Machine::default();

        // Setup initial conditions
        m[Register::R3] = 0x3010; // Base address in register R3
        m.mem[0x300Eusize] = 0x5678; // Memory location to load from

        //                LDR   R4  R3  offset (-2 in binary)
        let instruction = 0b0110_100_011_111110; // LDR R4, R3, #-2

        info!("LDR R4 from (R3 - 2 = 0x300E), expects 0x5678");

        // Execute the instruction
        m.load_register(instruction);

        // Check if the value was loaded correctly
        assert_eq!(m[Register::R4], 0x5678);
    }

    #[test]
    fn hello_world_walkthrough() {
        let mut m = Machine::default();
        let f = std::fs::File::open("samples/hello_world.obj").expect("hello world sample lost");
        m.load_image(f);

        // First instruction is LEA into R1 the value at 0x3007.
        m.cycle();
        assert_eq!(m.mem_read(0x3007), 'H' as u16);

        // Next, use LDR to load the value of the memory address offset by 0 into R0.
        m.cycle();
        assert_eq!(m[Register::R0], 'H' as u16);

        // Next, branch to HALT if the cond flag is Zero (it shouldn't be yet.)
        assert_ne!(m[Register::Cond], Condition::Zero as u16);
    }
}
