/*
 * Emulator of the MIPS I instruction set.
 */

use clap::{App, Arg, SubCommand};
use std::fs::File;
use std::io::prelude::*;

mod mips;

const MEMORY_SIZE: usize = 1024 * 1024;

fn main() {
    let matches = App::new("MIPS emulator")
        .subcommand(SubCommand::with_name("print").arg(Arg::with_name("filepath").required(true)))
        .subcommand(SubCommand::with_name("run").arg(Arg::with_name("filepath").required(true)))
        .get_matches();

    if let Some(matches) = matches.subcommand_matches("print") {
        if let Some(ref file) = matches.value_of("filepath") {
            let mut file = File::open(file).expect("Failed to open program");
            let mut program = Vec::<u8>::new();
            file.read_to_end(&mut program)
                .expect("Failed to read program");
            let instructions = mips::decode_program(&program);
            for i in &instructions {
                println!("{}", i);
            }
        }
    } else if let Some(matches) = matches.subcommand_matches("run") {
        if let Some(ref file) = matches.value_of("filepath") {
            let mut file = File::open(file).expect("Failed to open program");
            let mut program = Vec::<u8>::new();
            file.read_to_end(&mut program)
                .expect("Failed to read program");
            let instructions = mips::decode_program(&program);
            let mut memory: [u8; MEMORY_SIZE] = [0; MEMORY_SIZE];
            let mut cpu = mips::CPU::new();
            for i in instructions {
                mips::execute_instruction(&mut cpu, &mut memory, i);
            }
            println!("{}", cpu);
        }
    }
}
