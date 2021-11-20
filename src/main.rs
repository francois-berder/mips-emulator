/*
 * Emulator of the MIPS I instruction set.
 */

use std::fmt;

const MEMORY_SIZE: usize = 1024 * 1024;
const CPU_REG_COUNT: usize = 32;

struct CPU {
    regs: [u32; CPU_REG_COUNT as usize],
    hi: u32,
    lo: u32,
    pc: u32,
    epc: u32,
}

impl CPU {
    fn get_reg(&self, r: u8) -> u32 {
        if r == 0 {
            0
        } else {
            self.regs[r as usize]
        }
    }

    fn set_reg(&mut self, r: u8, value: u32) {
        if r != 0 {
            self.regs[r as usize] = value
        }
    }
}

enum Instruction {
    // ALU
    Add { rs: u8, rt: u8, rd: u8 },
    Addi { rs: u8, rt: u8, imm: u16 },
    Addiu { rs: u8, rt: u8, imm: u16 },
    Addu { rs: u8, rt: u8, rd: u8 },
    And { rs: u8, rt: u8, rd: u8 },
    Andi { rs: u8, rt: u8, imm: u16 },
    Lui { rt: u8, imm: u16 },
    Nor { rs: u8, rt: u8, rd: u8 },
    Or { rs: u8, rt: u8, rd: u8 },
    Ori { rs: u8, rt: u8, imm: u16 },
    Slt { rs: u8, rt: u8, rd: u8 },
    Slti { rs: u8, rt: u8, imm: u16 },
    Sltiu { rs: u8, rt: u8, imm: u16 },
    Sltu { rs: u8, rt: u8, rd: u8 },
    Sub { rs: u8, rt: u8, rd: u8 },
    Subu { rs: u8, rt: u8, rd: u8 },
    Xor { rs: u8, rt: u8, rd: u8 },
    Xori { rs: u8, rt: u8, imm: u16 },

    // Shifter
    Sll { rt: u8, rd: u8, sa: u8 },
    Sllv { rs: u8, rt: u8, rd: u8 },
    Sra { rt: u8, rd: u8, sa: u8 },
    Srav { rs: u8, rt: u8, rd: u8 },
    Srl { rt: u8, rd: u8, sa: u8 },
    Srlv { rs: u8, rt: u8, rd: u8 },

    // Multiply/Divide
    Div { rs: u8, rt: u8 },
    Divu { rs: u8, rt: u8 },
    Mfhi { rd: u8 },
    Mflo { rd: u8 },
    Mthi { rs: u8 },
    Mtlo { rs: u8 },
    Mult { rs: u8, rt: u8 },
    Multu { rs: u8, rt: u8 },

    // Branch
    Beq { rs: u8, rt: u8, offset: u16 },
    Bgez { rs: u8, offset: u16 },
    Bgezal { rs: u8, offset: u16 },
    Bgtz { rs: u8, offset: u16 },
    Blez { rs: u8, offset: u16 },
    Bltz { rs: u8, offset: u16 },
    Bltzal { rs: u8, offset: u16 },
    Bne { rs: u8, rt: u8, offset: u16 },
    Break,
    J { target: u32 },
    Jal { target: u32 },
    Jalr { rs: u8 },
    Jr { rs: u8 },
    Mfc0 { rt: u8, rd: u8 },
    Mtc0 { rt: u8, rd: u8 },
    Syscall,

    // Memory Access
    Lb { rs: u8, rt: u8, offset: u16 },
    Lbu { rs: u8, rt: u8, offset: u16 },
    Lh { rs: u8, rt: u8, offset: u16 },
    Lhu { rs: u8, rt: u8, offset: u16 },
    Lw { rs: u8, rt: u8, offset: u16 },
    Sb { rs: u8, rt: u8, offset: u16 },
    Sh { rs: u8, rt: u8, offset: u16 },
    Sw { rs: u8, rt: u8, offset: u16 },
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Instruction::Add { rs, rt, rd } => write!(f, "add r{},r{},r{}", rd, rs, rt),
            Instruction::Addi { rs, rt, imm } => write!(f, "addi r{},r{},{}", rs, rt, imm),
            Instruction::Addiu { rs, rt, imm } => write!(f, "addiu r{},r{},{}", rs, rt, imm),
            Instruction::Addu { rs, rt, rd } => write!(f, "addu r{},r{},r{}", rd, rs, rt),
            Instruction::And { rs, rt, rd } => write!(f, "and r{},r{},r{}", rd, rs, rt),
            Instruction::Andi { rs, rt, imm } => write!(f, "andi r{},r{},{}", rt, rs, imm),
            Instruction::Lui { rt, imm } => write!(f, "lui r{},{}", rt, imm),
            Instruction::Nor { rs, rt, rd } => write!(f, "nor r{},r{},r{}", rd, rs, rt),
            Instruction::Or { rs, rt, rd } => write!(f, "or r{},r{},r{}", rd, rs, rt),
            Instruction::Ori { rs, rt, imm } => write!(f, "ori r{},r{},{}", rt, rs, imm),
            Instruction::Slt { rs, rt, rd } => write!(f, "slt r{},r{},r{}", rd, rs, rt),
            Instruction::Slti { rs, rt, imm } => write!(f, "slti r{},r{},{}", rt, rs, imm),
            Instruction::Sltiu { rs, rt, imm } => write!(f, "sltiu r{},r{},{}", rt, rs, imm),
            Instruction::Sltu { rs, rt, rd } => write!(f, "sltu r{},r{},r{}", rd, rs, rt),
            Instruction::Sub { rs, rt, rd } => write!(f, "sub r{},r{},r{}", rd, rs, rt),
            Instruction::Subu { rs, rt, rd } => write!(f, "subu r{},r{},r{}", rd, rs, rt),
            Instruction::Xor { rs, rt, rd } => write!(f, "xor r{},r{},r{}", rd, rs, rt),
            Instruction::Xori { rs, rt, imm } => write!(f, "xori r{},r{},{}", rt, rs, imm),
            Instruction::Sll { rt, rd, sa } => write!(f, "sll r{},r{},{}", rd, rt, sa),
            Instruction::Sllv { rs, rt, rd } => write!(f, "sllv r{},r{},r{}", rd, rs, rt),
            Instruction::Sra { rt, rd, sa } => write!(f, "sra r{},r{},{}", rd, rt, sa),
            Instruction::Srav { rs, rt, rd } => write!(f, "srav r{},r{},r{}", rd, rs, rt),
            Instruction::Srl { rt, rd, sa } => write!(f, "srl r{},r{},{}", rd, rt, sa),
            Instruction::Srlv { rs, rt, rd } => write!(f, "srlv r{},r{},r{}", rd, rs, rt),
            Instruction::Div { rs, rt } => write!(f, "div r{},r{}", rs, rt),
            Instruction::Divu { rs, rt } => write!(f, "divu r{},r{}", rs, rt),
            Instruction::Mfhi { rd } => write!(f, "mfhi r{}", rd),
            Instruction::Mflo { rd } => write!(f, "mflo r{}", rd),
            Instruction::Mthi { rs } => write!(f, "mthi r{}", rs),
            Instruction::Mtlo { rs } => write!(f, "mtlo r{}", rs),
            Instruction::Mult { rs, rt } => write!(f, "mult r{},r{}", rs, rt),
            Instruction::Multu { rs, rt } => write!(f, "multu r{},r{}", rs, rt),
            Instruction::Beq { rs, rt, offset } => write!(f, "beq r{},r{},{}", rs, rt, offset),
            Instruction::Bgez { rs, offset } => write!(f, "bgez r{},{}", rs, offset),
            Instruction::Bgezal { rs, offset } => write!(f, "bgezal r{},{}", rs, offset),
            Instruction::Bgtz { rs, offset } => write!(f, "bgtz r{},{}", rs, offset),
            Instruction::Blez { rs, offset } => write!(f, "blez r{},{}", rs, offset),
            Instruction::Bltz { rs, offset } => write!(f, "bltz r{},{}", rs, offset),
            Instruction::Bltzal { rs, offset } => write!(f, "bltzal r{},{}", rs, offset),
            Instruction::Bne { rs, rt, offset } => write!(f, "bne r{},r{},{}", rs, rt, offset),
            Instruction::Break => write!(f, "break"),
            Instruction::J { target } => write!(f, "j {}", target),
            Instruction::Jal { target } => write!(f, "jal {}", target),
            Instruction::Jalr { rs } => write!(f, "jalr r{}", rs),
            Instruction::Jr { rs } => write!(f, "jr r{}", rs),
            Instruction::Mfc0 { rt, rd } => write!(f, "mfc0 r{},r{}", rt, rd),
            Instruction::Mtc0 { rt, rd } => write!(f, "mtc0 r{},r{}", rt, rd),
            Instruction::Syscall => write!(f, "syscall"),
            Instruction::Lb { rs, rt, offset } => write!(f, "lb r{},{}(r{})", rt, offset, rs),
            Instruction::Lbu { rs, rt, offset } => write!(f, "lbu r{},{}(r{})", rt, offset, rs),
            Instruction::Lh { rs, rt, offset } => write!(f, "lh r{},{}(r{})", rt, offset, rs),
            Instruction::Lhu { rs, rt, offset } => write!(f, "lhu r{},{}(r{})", rt, offset, rs),
            Instruction::Lw { rs, rt, offset } => write!(f, "lw r{},{}(r{})", rt, offset, rs),
            Instruction::Sb { rs, rt, offset } => write!(f, "sb r{},{}(r{})", rt, offset, rs),
            Instruction::Sh { rs, rt, offset } => write!(f, "sh r{},{}(r{})", rt, offset, rs),
            Instruction::Sw { rs, rt, offset } => write!(f, "sw r{},{}(r{})", rt, offset, rs),
        }
    }
}

fn decode_program(program: &[u32]) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::<Instruction>::with_capacity(program.len());

    for instruction in program {
        match decode_instruction(*instruction) {
            Ok(inst) => instructions.push(inst),
            Err(s) => println!("error: {}", s),
        }
    }

    instructions
}

fn encode_r_instruction(opcode: u8, func: u8, rs: u8, rt: u8, rd: u8, sa: u8) -> u32 {
    (opcode as u32) << 26
        | (((rs as u32) & 0x1F) << 21)
        | (((rt as u32) & 0x1F) << 16)
        | (((rd as u32) & 0x1F) << 11)
        | (((sa as u32) & 0x1F) << 6)
        | ((func as u32) & 0x3F)
}

fn encode_i_instruction(opcode: u8, rs: u8, rt: u8, imm: u16) -> u32 {
    (opcode as u32) << 26
        | (((rs as u32) & 0x1F) << 21)
        | (((rt as u32) & 0x1F) << 16)
        | ((imm as u32) & 0xFF)
}

fn encode_j_instruction(opcode: u8, target: u32) -> u32 {
    (opcode as u32) << 26 | ((target as u32) & 0x03FFFFFF)
}

fn encode_instruction(instruction: Instruction) -> u32 {
    match instruction {
        Instruction::Add { rs, rt, rd } => encode_r_instruction(0b000000, 0b100000, rs, rt, rd, 0),
        Instruction::Addi { rs, rt, imm } => encode_i_instruction(0b001000, rs, rt, imm),
        Instruction::Addiu { rs, rt, imm } => encode_i_instruction(0b001001, rs, rt, imm),
        Instruction::Addu { rs, rt, rd } => encode_r_instruction(0b000000, 0b100001, rs, rt, rd, 0),
        Instruction::And { rs, rt, rd } => encode_r_instruction(0b000000, 0b100100, rs, rt, rd, 0),
        Instruction::Andi { rs, rt, imm } => encode_i_instruction(0b001100, rs, rt, imm),
        Instruction::Lui { rt, imm } => encode_i_instruction(0b001111, 0, rt, imm),
        Instruction::Nor { rs, rt, rd } => encode_r_instruction(0b000000, 0b100111, rs, rt, rd, 0),
        Instruction::Or { rs, rt, rd } => encode_r_instruction(0b000000, 0b100101, rs, rt, rd, 0),
        Instruction::Ori { rs, rt, imm } => encode_i_instruction(0b001101, rs, rt, imm),
        Instruction::Slt { rs, rt, rd } => encode_r_instruction(0b000000, 0b101010, rs, rt, rd, 0),
        Instruction::Slti { rs, rt, imm } => encode_i_instruction(0b001010, rs, rt, imm),
        Instruction::Sltiu { rs, rt, imm } => encode_i_instruction(0b001011, rs, rt, imm),
        Instruction::Sltu { rs, rt, rd } => encode_r_instruction(0b000000, 0b101011, rs, rt, rd, 0),
        Instruction::Sub { rs, rt, rd } => encode_r_instruction(0b000000, 0b100010, rs, rt, rd, 0),
        Instruction::Subu { rs, rt, rd } => encode_r_instruction(0b000000, 0b100011, rs, rt, rd, 0),
        Instruction::Xor { rs, rt, rd } => encode_r_instruction(0b000000, 0b100110, rs, rt, rd, 0),
        Instruction::Xori { rs, rt, imm } => encode_i_instruction(0b001110, rs, rt, imm),
        Instruction::Sll { rt, rd, sa } => encode_r_instruction(0b000000, 0b000000, 0, rt, rd, sa),
        Instruction::Sllv { rs, rt, rd } => encode_r_instruction(0b000000, 0b000100, rs, rt, rd, 0),
        Instruction::Sra { rt, rd, sa } => encode_r_instruction(0b000000, 0b000011, 0, rt, rd, sa),
        Instruction::Srav { rs, rt, rd } => encode_r_instruction(0b000000, 0b000111, rs, rt, rd, 0),
        Instruction::Srl { rt, rd, sa } => encode_r_instruction(0b000000, 0b000010, 0, rt, rd, sa),
        Instruction::Srlv { rs, rt, rd } => encode_r_instruction(0b000000, 0b000110, rs, rt, rd, 0),
        Instruction::Div { rs, rt } => encode_r_instruction(0b000000, 0b011010, rs, rt, 0, 0),
        Instruction::Divu { rs, rt } => encode_r_instruction(0b000000, 0b011011, rs, rt, 0, 0),
        Instruction::Mfhi { rd } => encode_r_instruction(0b000000, 0b010000, 0, 0, rd, 0),
        Instruction::Mflo { rd } => encode_r_instruction(0b000000, 0b010010, 0, 0, rd, 0),
        Instruction::Mthi { rs } => encode_r_instruction(0b000000, 0b010001, rs, 0, 0, 0),
        Instruction::Mtlo { rs } => encode_r_instruction(0b000000, 0b010011, rs, 0, 0, 0),
        Instruction::Mult { rs, rt } => encode_r_instruction(0b000000, 0b011000, rs, rt, 0, 0),
        Instruction::Multu { rs, rt } => encode_r_instruction(0b000000, 0b011001, rs, rt, 0, 0),
        Instruction::Beq { rs, rt, offset } => encode_i_instruction(0b000100, rs, rt, offset),
        Instruction::Bgez { rs, offset } => encode_i_instruction(0b000001, rs, 0b00001, offset),
        Instruction::Bgezal { rs, offset } => encode_i_instruction(0b000001, rs, 0b10001, offset),
        Instruction::Bgtz { rs, offset } => encode_i_instruction(0b000111, rs, 0, offset),
        Instruction::Blez { rs, offset } => encode_i_instruction(0b000110, rs, 0, offset),
        Instruction::Bltz { rs, offset } => encode_i_instruction(0b000001, rs, 0b00000, offset),
        Instruction::Bltzal { rs, offset } => encode_i_instruction(0b000001, rs, 0b10000, offset),
        Instruction::Bne { rs, rt, offset } => encode_i_instruction(0b000101, rs, rt, offset),
        Instruction::Break => 0b00000000000000000000000000001101,
        Instruction::J { target } => encode_j_instruction(0b000010, target),
        Instruction::Jal { target } => encode_j_instruction(0b000011, target),
        Instruction::Jalr { rs } => encode_r_instruction(0b000000, 0b001001, rs, 0, 0, 0),
        Instruction::Jr { rs } => encode_r_instruction(0b000000, 0b001000, rs, 0, 0, 0),
        Instruction::Mfc0 { rt, rd } => encode_r_instruction(0b010000, 0, 0b00000, rt, rd, 0),
        Instruction::Mtc0 { rt, rd } => encode_r_instruction(0b010000, 0, 0b00100, rt, rd, 0),
        Instruction::Syscall => 0b00000000000000000000000000001100,
        Instruction::Lb { rs, rt, offset } => encode_i_instruction(0b100000, rs, rt, offset),
        Instruction::Lbu { rs, rt, offset } => encode_i_instruction(0b100100, rs, rt, offset),
        Instruction::Lh { rs, rt, offset } => encode_i_instruction(0b100001, rs, rt, offset),
        Instruction::Lhu { rs, rt, offset } => encode_i_instruction(0b100101, rs, rt, offset),
        Instruction::Lw { rs, rt, offset } => encode_i_instruction(0b100011, rs, rt, offset),
        Instruction::Sb { rs, rt, offset } => encode_i_instruction(0b101000, rs, rt, offset),
        Instruction::Sh { rs, rt, offset } => encode_i_instruction(0b101001, rs, rt, offset),
        Instruction::Sw { rs, rt, offset } => encode_i_instruction(0b101011, rs, rt, offset),
    }
}

fn decode_instruction(instruction: u32) -> Result<Instruction, String> {
    let opcode = instruction >> 26;
    let rs: u8 = ((instruction >> 21) & 0x1F) as u8;
    let rt: u8 = ((instruction >> 16) & 0x1F) as u8;
    let rd: u8 = ((instruction >> 11) & 0x1F) as u8;
    let sa: u8 = ((instruction >> 6) & 0x1F) as u8;
    let funct = instruction & 0x3F;
    let offset: u16 = (instruction & 0xFFFF) as u16;
    let target = instruction & 0x03FFFFFF;
    let imm: u16 = (instruction & 0xFFFF) as u16;

    match opcode {
        0b000000 => match funct {
            0b100000 => Ok(Instruction::Add { rs, rt, rd }),
            0b100001 => Ok(Instruction::Addu { rs, rt, rd }),
            0b100100 => Ok(Instruction::And { rs, rt, rd }),
            0b100111 => Ok(Instruction::Nor { rs, rt, rd }),
            0b100101 => Ok(Instruction::Or { rs, rt, rd }),
            0b101010 => Ok(Instruction::Slt { rs, rt, rd }),
            0b101011 => Ok(Instruction::Sltu { rs, rt, rd }),
            0b100010 => Ok(Instruction::Sub { rs, rt, rd }),
            0b100011 => Ok(Instruction::Subu { rs, rt, rd }),
            0b100110 => Ok(Instruction::Xor { rs, rt, rd }),
            0b000000 => Ok(Instruction::Sll { rt, rd, sa }),
            0b000100 => Ok(Instruction::Sllv { rs, rt, rd }),
            0b000011 => Ok(Instruction::Sra { rt, rd, sa }),
            0b000111 => Ok(Instruction::Srav { rs, rt, rd }),
            0b000010 => Ok(Instruction::Srl { rt, rd, sa }),
            0b000110 => Ok(Instruction::Srlv { rs, rt, rd }),
            0b011010 => Ok(Instruction::Div { rs, rt }),
            0b011011 => Ok(Instruction::Divu { rs, rt }),
            0b011000 => Ok(Instruction::Mult { rs, rt }),
            0b011001 => Ok(Instruction::Multu { rs, rt }),
            0b001001 => Ok(Instruction::Jalr { rs }),
            0b001000 => Ok(Instruction::Jr { rs }),
            0b001101 => Ok(Instruction::Break),
            0b001100 => Ok(Instruction::Syscall),
            0b010000 => Ok(Instruction::Mfhi { rd }),
            0b010010 => Ok(Instruction::Mflo { rd }),
            0b010001 => Ok(Instruction::Mthi { rs }),
            0b010011 => Ok(Instruction::Mtlo { rs }),
            _ => Err("Invalid instruction".to_string()),
        },
        0b001000 => Ok(Instruction::Addi { rs, rt, imm }),
        0b001001 => Ok(Instruction::Addiu { rs, rt, imm }),
        0b001100 => Ok(Instruction::Andi { rs, rt, imm }),
        0b001111 => Ok(Instruction::Lui { rt, imm }),
        0b001101 => Ok(Instruction::Ori { rs, rt, imm }),
        0b001010 => Ok(Instruction::Slti { rs, rt, imm }),
        0b001011 => Ok(Instruction::Sltiu { rs, rt, imm }),
        0b001110 => Ok(Instruction::Xori { rs, rt, imm }),
        0b000100 => Ok(Instruction::Beq { rs, rt, offset }),
        0b000001 => match rt {
            0b00001 => Ok(Instruction::Bgez { rs, offset }),
            0b10001 => Ok(Instruction::Bgezal { rs, offset }),
            0b00000 => Ok(Instruction::Bltz { rs, offset }),
            0b10000 => Ok(Instruction::Bltzal { rs, offset }),
            _ => Err("Invalid instruction".to_string()),
        },
        0b010000 => match rs {
            0b00000 => Ok(Instruction::Mfc0 { rt, rd }),
            0b00100 => Ok(Instruction::Mtc0 { rt, rd }),
            _ => Err("Invalid instruction".to_string()),
        },
        0b000111 => Ok(Instruction::Bgtz { rs, offset }),
        0b000110 => Ok(Instruction::Blez { rs, offset }),
        0b000101 => Ok(Instruction::Bne { rs, rt, offset }),
        0b000010 => Ok(Instruction::J { target }),
        0b000011 => Ok(Instruction::Jal { target }),
        0b100000 => Ok(Instruction::Lb { rs, rt, offset }),
        0b100100 => Ok(Instruction::Lbu { rs, rt, offset }),
        0b100001 => Ok(Instruction::Lh { rs, rt, offset }),
        0b100101 => Ok(Instruction::Lhu { rs, rt, offset }),
        0b100011 => Ok(Instruction::Lw { rs, rt, offset }),
        0b101000 => Ok(Instruction::Sb { rs, rt, offset }),
        0b101001 => Ok(Instruction::Sh { rs, rt, offset }),
        0b101011 => Ok(Instruction::Sw { rs, rt, offset }),
        _ => Err("Invalid instruction".to_string()),
    }
}

fn execute_instruction(cpu: &mut CPU, mem: &mut [u8], inst: Instruction) {
    match inst {
        Instruction::Addu { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rs) + cpu.get_reg(rt)),
        Instruction::And { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rs) & cpu.get_reg(rt)),
        Instruction::Andi { rs, rt, imm } => cpu.set_reg(rt, cpu.get_reg(rs) & (imm as u32)),
        Instruction::Lui { rt, imm } => cpu.set_reg(rt, (imm as u32) << 16),
        Instruction::Nor { rs, rt, rd } => cpu.set_reg(rd, !(cpu.get_reg(rs) | cpu.get_reg(rt))),
        Instruction::Or { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rs) | cpu.get_reg(rt)),
        Instruction::Ori { rs, rt, imm } => cpu.set_reg(rt, cpu.get_reg(rs) | (imm as u32)),
        Instruction::Xor { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rs) ^ cpu.get_reg(rt)),
        Instruction::Xori { rs, rt, imm } => cpu.set_reg(rt, cpu.get_reg(rs) ^ (imm as u32)),
        Instruction::Subu { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rs) - cpu.get_reg(rt)),
        Instruction::Divu { rs, rt } => {
            cpu.lo = cpu.get_reg(rs) / cpu.get_reg(rt);
            cpu.hi = cpu.get_reg(rs) % cpu.get_reg(rt);
        }
        Instruction::Mfhi { rd } => {
            cpu.set_reg(rd, cpu.hi);
        }
        Instruction::Mflo { rd } => {
            cpu.set_reg(rd, cpu.lo);
        }
        Instruction::Mthi { rs } => {
            cpu.hi = cpu.get_reg(rs);
        }
        Instruction::Mtlo { rs } => {
            cpu.lo = cpu.get_reg(rs);
        }
        Instruction::Multu { rs, rt } => {
            let result: u64 = (cpu.get_reg(rs) as u64) * (cpu.get_reg(rt) as u64);
            cpu.lo = result as u32;
            cpu.hi = (result >> 32) as u32;
        }

        Instruction::Sll { rt, rd, sa } => cpu.set_reg(rd, cpu.get_reg(rt) << sa),
        Instruction::Sllv { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rt) << cpu.get_reg(rs)),
        Instruction::Srl { rt, rd, sa } => cpu.set_reg(rd, cpu.get_reg(rt) >> sa),
        Instruction::Srlv { rs, rt, rd } => cpu.set_reg(rd, cpu.get_reg(rt) >> cpu.get_reg(rs)),
        Instruction::Beq { rs, rt, offset } => {
            if cpu.get_reg(rs) == cpu.get_reg(rt) {
                cpu.pc += (offset as u32) * 4 - 4;
            }
        }
        Instruction::Bgez { rs, offset } => {
            if cpu.get_reg(rs) >= 0 {
                cpu.pc += (offset as u32) * 4 - 4;
            }
        }
        Instruction::Bne { rs, rt, offset } => {
            if cpu.get_reg(rs) != cpu.get_reg(rt) {
                cpu.pc += (offset as u32) * 4 - 4;
            }
        }

        Instruction::J { target } => {
            cpu.pc = (cpu.pc & 0xF0000000) | (target << 2);
            cpu.pc -= 4;
        }
        Instruction::Jal { target } => {
            cpu.set_reg(31, cpu.pc);
            cpu.pc = (cpu.pc & 0xF0000000) | (target << 2);
            cpu.pc -= 4;
        }
        Instruction::Jalr { rs } => {
            // TODO: missing something
            cpu.pc = cpu.get_reg(rs) - 4;
        }
        Instruction::Jr { rs } => {
            cpu.pc = cpu.get_reg(rs) - 4;
        }
        Instruction::Break => {
            cpu.epc = cpu.pc;
            cpu.pc = 0x3c - 4;
        }
        Instruction::Syscall => {
            cpu.epc = cpu.pc;
            cpu.pc = 0x3c - 4;
        }
        Instruction::Lbu { rs, rt, offset } => {
            let index: usize = (cpu.get_reg(rs) + (offset as u32)) as usize;
            cpu.set_reg(rt, mem[index] as u32);
        }
        Instruction::Lhu { rs, rt, offset } => {
            let index: usize = (cpu.get_reg(rs) + (offset as u32)) as usize;
            let lo: u8 = mem[index];
            let hi: u8 = mem[index + 1];
            cpu.set_reg(rt, ((hi as u32) << 8) | (lo as u32));
        }
        Instruction::Lw { rs, rt, offset } => {
            let index: usize = (cpu.get_reg(rs) + (offset as u32)) as usize;
            cpu.set_reg(
                rt,
                ((mem[index + 3] as u32) << 24)
                    | ((mem[index + 2] as u32) << 16)
                    | ((mem[index + 1] as u32) << 8)
                    | (mem[index] as u32),
            );
        }
        Instruction::Sb { rs, rt, offset } => {
            let value: u32 = cpu.get_reg(rt);
            let index: usize = (cpu.get_reg(rs) + (offset as u32)) as usize;
            mem[index] = value as u8;
        }
        Instruction::Sh { rs, rt, offset } => {
            let value: u32 = cpu.get_reg(rt);
            let index: usize = (cpu.get_reg(rs) + (offset as u32)) as usize;
            mem[index] = value as u8;
            mem[index + 1] = (value >> 8) as u8;
        }
        Instruction::Sw { rs, rt, offset } => {
            let value: u32 = cpu.get_reg(rt);
            let index: usize = (cpu.get_reg(rs) + (offset as u32)) as usize;
            mem[index] = value as u8;
            mem[index + 1] = (value >> 8) as u8;
            mem[index + 2] = (value >> 16) as u8;
            mem[index + 3] = (value >> 24) as u8;
        }
        _ => println!("Instruction not implemented"),
    }
    cpu.pc += 4;
}

fn main() {
    let program: [u32; 1] = [encode_instruction(Instruction::Addu {
        rs: 1,
        rt: 2,
        rd: 3,
    })];
    let instructions = decode_program(&program);

    for i in &instructions {
        println!("{}", i);
    }

    let mut memory: [u8; MEMORY_SIZE] = [0; MEMORY_SIZE];
    let mut cpu = CPU {
        regs: [0; CPU_REG_COUNT],
        hi: 0,
        lo: 0,
        pc: 0,
        epc: 0,
    };
    println!("Begin executing program");
    for i in instructions {
        execute_instruction(&mut cpu, &mut memory, i);
    }
    println!("Finished executing program");
}
