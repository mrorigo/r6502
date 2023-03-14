use std::{cell::RefCell, rc::Rc};

use crate::{
    memory::{Memory, MemoryOperations},
    opcodes::{OpCode, OPCODE_MAP},
};

pub const STACK_BASE: u16 = 0x100;

type SR = u8;

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum StatusFlags {
    C = 0,
    Z = 1,
    I = 2,
    D = 3,
    B = 4,
    _U = 5,
    V = 6,
    N = 7,
}

impl StatusFlags {
    pub fn is_set(value: u8, bit: StatusFlags) -> bool {
        (value & StatusFlags::mask(bit)) != 0
    }

    #[inline(always)]
    pub fn mask(bit: StatusFlags) -> u8 {
        1 << (bit as u8)
    }
}

#[derive(Debug, Copy, Clone)]
#[repr(u8)]
pub enum Register {
    SP = 0,
    A = 1,
    X = 2,
    Y = 3,
    MAX = 4,
}

#[derive(Copy, Clone)]
pub struct Operands {
    pub op1: u16,          // 'addr' for zeropage(xy)/absolute(xy)/indirect(xy)/relative
    pub op2: u8,           // 'arg' for most
    pub extra_cycle: bool, // true if addressing mode causes extra cycle
    pub orig: u16,         // the opcode argument
}

#[derive(Debug)]
pub enum Trap {
    AddressError(usize),
    Break(usize),
    Halt(usize),
}

pub struct CPU<'a> {
    pub bus: Rc<RefCell<Box<dyn BusOperations>>>,
    registers: [u8; Register::MAX as usize],
    pub pc: u16,
    pub prev_pc: u16,
    pub sr: SR,
    pub clock: usize,
    pub ticks: usize,
    // Last instruction opcode and operands
    pub opcode: &'a OpCode,
    pub operands: Operands,
}

/// A minimal bus consisting of only RAM
pub struct Bus {
    //pub ram: &'a mut Memory,
    pub ram: Box<Memory>,
}

/// In the view of the CPU, the Bus is basically a memory mapper for all the different peripherals connected to the CPU
pub trait BusOperations {
    fn read(&mut self, addr: usize) -> Result<u8, Trap>;
    fn write(&mut self, addr: usize, value: u8) -> Result<(), Trap>;
    // `tick()` returns true if an IRQ is raised
    fn tick(&mut self) -> bool {
        false
    }
    fn reset(&mut self) {}
}

impl Bus {
    pub fn create<'a>() -> impl BusOperations + 'a {
        const RAM_SIZE: usize = 1024 * 64; // 16-bit adressable
        let ram = Memory::new(Box::new([0u8; RAM_SIZE]));
        Bus { ram: Box::new(ram) }
    }
}

impl BusOperations for Bus {
    fn read(&mut self, addr: usize) -> Result<u8, Trap> {
        self.ram.read8(addr)
    }

    fn write(&mut self, addr: usize, value: u8) -> Result<(), Trap> {
        self.ram.write8(addr, value)
    }
}

impl CPU<'_> {
    pub fn read16(&mut self, addr: usize) -> Result<u16, Trap> {
        // let mut b1 = self.bus.as_ref().borrow_mut();
        // b1.read(addr)?;

        let hh = self.bus.borrow_mut().read(addr + 1)?;
        let ll = self.bus.borrow_mut().read(addr)?;
        Ok(((hh as u16) << 8) | (ll as u16))
    }

    pub fn write16(&mut self, addr: usize, value: u16) -> Result<(), Trap> {
        self.bus.borrow_mut().write(addr, (value & 0xff) as u8)?;
        self.bus
            .borrow_mut()
            .write(addr + 1, ((value >> 8) & 0xff) as u8)?;
        Ok(())
    }
}

impl CPU<'_> {
    pub fn new<'a>(bus: Rc<RefCell<Box<dyn BusOperations>>>) -> CPU<'a> {
        CPU {
            bus,
            registers: [0; Register::MAX as usize],
            sr: 0,
            pc: 0,
            prev_pc: 0,
            clock: 0,
            ticks: 0,
            opcode: &OPCODE_MAP[0], // BRK
            operands: Operands {
                op1: 0,
                op2: 0,
                extra_cycle: false,
                orig: 0,
            },
        }
    }

    #[inline]
    pub fn reg(&self, reg: Register) -> u8 {
        self.registers[reg as usize]
    }

    #[inline]
    pub fn set_reg(&mut self, reg: Register, value: u8) {
        self.registers[reg as usize] = value
    }

    fn do_irq(&mut self, status: u8) -> Result<(), Trap> {
        if StatusFlags::is_set(self.sr, StatusFlags::I) {
            println!("IRQ, but Interrupts disabled!");
            return Ok(());
        }

        // The interrupt sequence pushes three bytes onto the stack. First is the high byte of the return address,
        // followed by the low byte, and finally the status byte from the P processor status register
        OpCode::push_stack(self, (self.pc >> 8) as u8)?;
        OpCode::push_stack(self, (self.pc & 0xff) as u8)?;
        OpCode::push_stack(self, status)?;
        let mut bus = self.bus.borrow_mut();
        self.pc = (bus.read(0xfffe)? as u16) | ((bus.read(0xffff)? as u16) << 8);

        self.clock = self.clock.wrapping_add(7);
        Ok(())
    }

    fn irq(&mut self) -> Result<(), Trap> {
        println!("IRQ!");
        self.do_irq(self.sr & !StatusFlags::mask(StatusFlags::B))
    }

    fn brk(&mut self) -> Result<(), Trap> {
        println!("BRK!");
        self.do_irq(self.sr | StatusFlags::mask(StatusFlags::B))
    }

    pub fn get_opcode(data: u8) -> &'static OpCode {
        &OPCODE_MAP[data as usize]
    }

    // Returns the # of ticks until next clock
    pub fn tick(&mut self) -> Result<usize, Trap> {
        self.ticks = self.ticks.wrapping_add(1);
        if self.ticks > self.clock {
            self.prev_pc = self.pc;
            if self.bus.borrow_mut().tick() {
                //                println!("IGNORED IRQ!. 314={:#x?}", self.read16(0x314));
                return self.irq().map(|_| 0);
            }

            let data = self.bus.borrow_mut().read(self.pc as usize)?;
            self.opcode = &OPCODE_MAP[data as usize];

            self.operands = self.opcode.addressing_mode.decode(self)?;

            let amf = self.opcode.addressing_mode.format(&self.operands);
            println!(
                "{:#} {:#x}: {} {}",
                self.clock, self.pc, self.opcode.name, amf
            );

            (self.opcode.op_impl)(self)?;

            // Clock might also have been updaetd by instruction (branches)
            // However, we add the possible decoding extra cycle here
            self.clock = self.clock.wrapping_add(
                self.opcode
                    .cycles
                    .wrapping_add(self.operands.extra_cycle.into()),
            );

            self.pc = self.pc.wrapping_add(self.opcode.size as u16);
        }
        Ok(self.clock - self.ticks)
    }
}

#[derive(Clone, Copy)]
pub enum AddressingMode {
    Implied,
    Immediate,
    ZeroPage,
    ZeroPageX,
    ZeroPageY,
    Absolute,
    AbsoluteX,
    AbsoluteY,
    Indirect,
    IndirectX,
    IndirectY,
    Relative,
    Accumulator,
}

impl AddressingMode {
    pub fn format(&self, op: &Operands) -> String {
        match *self {
            AddressingMode::Implied => format!(""),
            AddressingMode::Immediate => format!("#{:#04X}", op.orig),
            AddressingMode::ZeroPage => format!("{:#04X}\t#{:#x}", op.orig, op.op2),
            AddressingMode::ZeroPageX => format!("{:#04X},X\t# {:#x}", op.orig, op.op2),
            AddressingMode::ZeroPageY => format!("{:#04X},Y\t# {:#x}", op.orig, op.op2),
            AddressingMode::Absolute => format!("{:#06X}", op.orig),
            AddressingMode::AbsoluteX => format!("{:#06X},X\t# = {:#x?}", op.orig, op.op2),
            AddressingMode::AbsoluteY => format!("{:#06X},Y", op.orig),
            AddressingMode::Indirect => format!("({:#06X})", op.orig),
            AddressingMode::IndirectX => format!("({:#X},X)", op.orig),
            AddressingMode::IndirectY => {
                format!("({:#X},Y)\t# ({:#X?})+Y={:#X}", op.orig, op.orig, op.op1)
            }
            AddressingMode::Relative => format!("{:#}", op.orig as i8 as i16),
            AddressingMode::Accumulator => format!(""),
        }
    }

    fn check_extra(low: u16, offs: u16) -> bool {
        let val = (low & 0xff) + offs;
        val > 0xff
    }

    pub fn decode(&self, cpu: &mut CPU) -> Result<Operands, Trap> {
        let pc1 = cpu.pc.wrapping_add(1) as usize;
        let (addr, arg, orig, extra_cycle) = match *self {
            AddressingMode::Accumulator | AddressingMode::Implied => (0, 0, 0 as u16, false),
            AddressingMode::Absolute => {
                let addr = cpu.read16(pc1)?;
                (addr, cpu.bus.borrow_mut().read(addr as usize)?, addr, false)
            }
            AddressingMode::AbsoluteX => {
                let addr = cpu.read16(pc1)?;
                let extra_cycle = AddressingMode::check_extra(addr, cpu.reg(Register::X) as u16);
                let addr2 = addr.wrapping_add(cpu.reg(Register::X) as u16);
                (
                    addr2,
                    cpu.bus.borrow_mut().read(addr2 as usize)?,
                    addr,
                    extra_cycle,
                )
            }
            AddressingMode::AbsoluteY => {
                let addr = cpu.read16(pc1)?;
                let extra_cycle = AddressingMode::check_extra(addr, cpu.reg(Register::Y) as u16);
                let addr2 = addr.wrapping_add(cpu.reg(Register::Y) as u16);
                (
                    addr2,
                    cpu.bus.borrow_mut().read(addr2 as usize)?,
                    addr,
                    extra_cycle,
                )
            }
            AddressingMode::Immediate => {
                let value = cpu.bus.borrow_mut().read(pc1)?;
                (0, value, value as u16, false)
            }
            AddressingMode::Indirect => {
                let addr = cpu.read16(pc1)?;
                let lo = cpu.bus.borrow_mut().read(addr as usize)? as u16;
                let addr2 = match (addr & 0xff) == 0xff {
                    true => addr & 0xff00,
                    false => addr.wrapping_add(1),
                };
                let hi = cpu.bus.borrow_mut().read(addr2 as usize)? as u16;
                ((hi << 8) | (lo), 0, addr, false)
            }
            AddressingMode::IndirectX => {
                let tmp = cpu.bus.borrow_mut().read(pc1)?;
                let addr = tmp.wrapping_add(cpu.reg(Register::X));
                let addr2_lo = cpu.bus.borrow_mut().read((addr & 0xff) as usize)? as u16;
                let addr2_hi =
                    cpu.bus
                        .borrow_mut()
                        .read((addr.wrapping_add(1) & 0xff) as usize)? as u16;
                let addr2 = addr2_lo | (addr2_hi << 8);
                (
                    addr2,
                    cpu.bus.borrow_mut().read(addr2 as usize)?,
                    tmp as u16,
                    false,
                )
            }
            AddressingMode::IndirectY => {
                let tmp = cpu.bus.borrow_mut().read(pc1)?; // 0xd1
                let addr_lo = cpu.bus.borrow_mut().read(tmp as usize)? as u16;
                let addr_hi =
                    cpu.bus
                        .borrow_mut()
                        .read((tmp.wrapping_add(1) & 0xff) as usize)? as u16;
                let addr = (addr_hi << 8) | addr_lo;
                let extra_cycle = AddressingMode::check_extra(addr, cpu.reg(Register::Y) as u16);
                let addr2 = addr.wrapping_add(cpu.reg(Register::Y) as u16) as u16;
                (
                    addr2,
                    cpu.bus.borrow_mut().read(addr2 as usize)?,
                    tmp as u16,
                    extra_cycle,
                )
            }
            AddressingMode::Relative => {
                let offs = cpu.bus.borrow_mut().read(pc1)? as i8 as i16;
                (
                    (cpu.pc as i16).wrapping_add(offs) as u16,
                    0,
                    offs as u16,
                    false,
                )
            }
            AddressingMode::ZeroPage => {
                let addr = cpu.bus.borrow_mut().read(pc1)? as u16;
                (
                    addr,
                    cpu.bus.borrow_mut().read(addr as usize)? as u8,
                    addr,
                    false,
                )
            }
            AddressingMode::ZeroPageX => {
                let addr = cpu.bus.borrow_mut().read(pc1)?;
                let addr2 = addr.wrapping_add(cpu.reg(Register::X));
                (
                    addr2 as u16,
                    cpu.bus.borrow_mut().read(addr2 as usize)? as u8,
                    addr as u16,
                    false,
                )
            }
            AddressingMode::ZeroPageY => {
                let addr = cpu.bus.borrow_mut().read(pc1)?;
                let addr2 = addr.wrapping_add(cpu.reg(Register::Y));
                (
                    addr2 as u16,
                    cpu.bus.borrow_mut().read(addr2 as usize)? as u8,
                    addr as u16,
                    false,
                )
            }
        };
        Ok(Operands {
            op1: addr,
            op2: arg,
            extra_cycle,
            orig,
        })
    }
}
