use crate::{
    cpu::{StatusFlags, CPU},
    memory::{Memory, MemoryOperations},
    opcodes::OPCODE_MAP,
};

mod cpu;
mod memory;
mod opcodes;

const RAM_SIZE: usize = 1024 * 64;
const FUNCTIONAL_TEST: &[u8; RAM_SIZE] = include_bytes!("../tests/6502_functional_test.bin");

fn main() {
    let mem: &mut [u8] = &mut vec![0; RAM_SIZE];
    let mut memory = Memory::new(mem);
    let mut cpu = CPU::new(&mut memory);

    for i in 0..FUNCTIONAL_TEST.len() {
        //println!("{:#x?}: {:#x?}", i, FUNCTIONAL_TEST[i]);
        match cpu.write8(i, FUNCTIONAL_TEST[i]) {
            Ok(_) => {}
            Err(err) => panic!("{:?}", err),
        }
    }

    // Functional test PC starts at 0x400
    cpu.pc = 0x400;
    cpu.set_reg(cpu::Register::SP, 0xfd);

    loop {
        match cpu.tick() {
            Ok(_ticks) => {}
            Err(err) => {
                println!("Trap:{:#x?}", err);
                break;
            }
        }
        println!(
            "A: {:#04x} X: {:#04x} Y: {:#04x} SP: {:#04x} PC: {:#04x} SR: {:#04x}",
            cpu.reg(cpu::Register::A),
            cpu.reg(cpu::Register::X),
            cpu.reg(cpu::Register::Y),
            cpu.reg(cpu::Register::SP),
            cpu.pc,
            cpu.sr
        );
    }
}
