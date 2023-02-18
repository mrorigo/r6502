use crate::{
    memory::MemoryOperations,
    opcodes::{OpCode, OPCODE_MAP},
};

pub const STACK_BASE: u16 = 0x100;

type SR = u8;

#[repr(u8)]
pub enum StatusFlags {
    C = 0,
    Z = 1,
    I = 2,
    D = 3,
    _B = 4,
    _U = 5,
    V = 6,
    N = 7,
}

impl StatusFlags {
    pub fn is_set(value: u8, bit: StatusFlags) -> bool {
        (value & StatusFlags::mask(bit)) != 0
    }

    pub fn mask(bit: StatusFlags) -> u8 {
        1 << (bit as u8)
    }
}

#[derive(Debug)]
#[repr(u8)]
pub enum Register {
    SP = 0,
    A = 1,
    X = 2,
    Y = 3,
    MAX = 4,
}

pub struct Operands {
    pub op1: u16,          // 'addr' for zeropage(xy)/absolute(xy)/indirect(xy)/relative
    pub op2: u8,           // 'arg' for most
    pub extra_cycle: bool, // true if addressing mode causes extra cycle
}

#[derive(Debug)]
pub enum Trap {
    AddressError(usize),
    Break(usize),
    Halt(usize),
}

pub struct CPU<'a> {
    pub bus: &'a mut dyn BusOperations,
    registers: [u8; Register::MAX as usize],
    pub pc: u16,
    pub sr: SR,
    pub clock: usize,
    pub ticks: usize,
    // Last instruction opcode and operands
    pub opcode: &'a OpCode,
    pub operands: Operands,
}

/// A minimal bus consisting of only RAM
struct Bus {
    pub memory: dyn MemoryOperations,
}

/// In the view of the CPU, the Bus is basically a memory mapper for all the different peripherals connected to the CPU
pub trait BusOperations {
    fn read(&mut self, addr: usize) -> Result<u8, Trap>;
    fn write(&mut self, addr: usize, value: u8) -> Result<(), Trap>;
    // `tick()` returns true if an IRQ is raised
    fn tick(&mut self) -> bool {
        false
    }
}

impl BusOperations for Bus {
    fn read(&mut self, addr: usize) -> Result<u8, Trap> {
        self.memory.read8(addr)
    }

    fn write(&mut self, addr: usize, value: u8) -> Result<(), Trap> {
        self.memory.write8(addr, value)
    }
}

impl CPU<'_> {
    pub fn read8(&mut self, addr: usize) -> Result<u8, Trap> {
        self.bus.read(addr)
    }

    pub fn write8(&mut self, addr: usize, value: u8) -> Result<(), Trap> {
        self.bus.write(addr, value)
    }

    pub fn read16(&mut self, addr: usize) -> Result<u16, Trap> {
        let hh = self.read8(addr + 1)?;
        let ll = self.read8(addr)?;
        Ok(((hh as u16) << 8) | (ll as u16))
    }

    pub fn write16(&mut self, addr: usize, value: u16) -> Result<(), Trap> {
        self.write8(addr, (value & 0xff) as u8)?;
        self.write8(addr + 1, (value >> 8) as u8)?;
        Ok(())
    }
}

impl CPU<'_> {
    pub fn new<'a>(bus: &'a mut dyn BusOperations) -> CPU<'a> {
        CPU {
            bus,
            registers: [0; Register::MAX as usize],
            sr: 0,
            pc: 0,
            clock: 0,
            ticks: 0,
            opcode: &OPCODE_MAP[0], // BRK
            operands: Operands {
                op1: 0,
                op2: 0,
                extra_cycle: false,
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

    fn irq(&mut self) -> Result<(), Trap> {
        // The interrupt sequence pushes three bytes onto the stack. First is the high byte of the return address,
        // followed by the low byte, and finally the status byte from the P processor status register
        OpCode::push_stack(self, (self.pc >> 8) as u8)?;
        OpCode::push_stack(self, (self.pc & 0xff) as u8)?;
        OpCode::push_stack(self, self.sr)?;

        self.pc = (self.read8(0xffff)? as u16) | ((self.read8(0xfffe)? as u16) << 8);

        self.clock = self.clock.wrapping_add(7);
        Ok(())
    }

    // Returns the # of ticks until next clock
    pub fn tick(&mut self) -> Result<(), Trap> {
        self.ticks = self.ticks.wrapping_add(1);
        if self.ticks > self.clock {
            if self.bus.tick() {
                return self.irq();
            }

            let data = self.bus.read(self.pc as usize)?;
            self.opcode = &OPCODE_MAP[data as usize];

            //println!("{:#x?}: {:#x?}", self.pc, self.opcode.name);

            self.operands = self.opcode.addressing_mode.decode(self)?;

            let amf = self.opcode.addressing_mode.format(&self.operands);
            println!("{:#x}: {} {}", self.pc, self.opcode.name, amf);

            (self.opcode.op_impl)(self)?;

            self.clock = self.clock.wrapping_add(
                self.opcode
                    .cycles
                    .wrapping_add(self.operands.extra_cycle.into()),
            );

            self.pc = self.pc.wrapping_add(self.opcode.size as u16);
        }
        Ok(())
    }
}

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
    fn format(&self, op: &Operands) -> String {
        match *self {
            AddressingMode::Implied => format!(""),
            AddressingMode::Immediate => format!("#{:#04x}", op.op2),
            AddressingMode::ZeroPage => format!("{:#04x}", op.op1),
            AddressingMode::ZeroPageX => format!("{:#04x},X", op.op1),
            AddressingMode::ZeroPageY => format!("{:#04x},Y", op.op1),
            AddressingMode::Absolute => format!("{:#06x}", op.op1),
            AddressingMode::AbsoluteX => format!("{:#06x},X", op.op1),
            AddressingMode::AbsoluteY => format!("{:#06x},Y", op.op1),
            AddressingMode::Indirect => todo!(),
            AddressingMode::IndirectX => format!("({:#x},X)", op.op2),
            AddressingMode::IndirectY => format!("({:#x},Y)", op.op2),
            AddressingMode::Relative => format!("{:#x}", op.op1),
            AddressingMode::Accumulator => format!("-a-"),
        }
    }

    fn check_extra(low: u16, offs: u16) -> bool {
        let val = (low & 0xff) + offs;
        val > 0xff
    }

    fn decode(&self, cpu: &mut CPU) -> Result<Operands, Trap> {
        let pc1 = cpu.pc.wrapping_add(1) as usize;
        let (addr, arg, extra_cycle) = match *self {
            AddressingMode::Accumulator | AddressingMode::Implied => (0, 0, false),
            AddressingMode::Absolute => {
                let addr = cpu.read16(pc1)?;
                (addr, cpu.read8(addr as usize)?, false)
            }
            AddressingMode::AbsoluteX => {
                let addr = cpu.read16(pc1)?;
                let extra_cycle = AddressingMode::check_extra(addr, cpu.reg(Register::X) as u16);
                let addr2 = addr.wrapping_add(cpu.reg(Register::X) as u16);
                (addr2, cpu.read8(addr2 as usize)?, extra_cycle)
            }
            AddressingMode::AbsoluteY => {
                let addr = cpu.read16(pc1)?;
                let extra_cycle = AddressingMode::check_extra(addr, cpu.reg(Register::Y) as u16);
                let addr2 = addr.wrapping_add(cpu.reg(Register::Y) as u16);
                (addr2, cpu.read8(addr2 as usize)?, extra_cycle)
            }
            AddressingMode::Immediate => (0, cpu.read8(pc1)?, false),
            AddressingMode::Indirect => {
                let addr = cpu.read16(pc1)?;
                let lo = cpu.read8(addr as usize)? as u16;
                let addr2 = match (addr & 0xff) == 0xff {
                    true => addr & 0xff00,
                    false => addr.wrapping_add(1),
                };
                let hi = cpu.read8(addr2 as usize)? as u16;
                ((hi << 8) | (lo), 0, false)
            }
            AddressingMode::IndirectX => {
                let tmp = cpu.read8(pc1)?;
                let addr = tmp.wrapping_add(cpu.reg(Register::X));
                let addr2_lo = cpu.read8((addr & 0xff) as usize)? as u16;
                let addr2_hi = cpu.read8((addr.wrapping_add(1) & 0xff) as usize)? as u16;
                let addr2 = addr2_lo | (addr2_hi << 8);
                (addr2, cpu.read8(addr2 as usize)?, false)
            }
            AddressingMode::IndirectY => {
                let tmp = cpu.read8(pc1)?;
                let addr_lo = cpu.read8(tmp as usize)? as u16;
                let addr_hi = cpu.read8((tmp.wrapping_add(1) & 0xff) as usize)? as u16;
                let addr = (addr_hi << 8) | addr_lo;
                let extra_cycle = AddressingMode::check_extra(addr, cpu.reg(Register::Y) as u16);
                let addr2 = addr.wrapping_add(cpu.reg(Register::Y) as u16) as u16;
                (addr2, cpu.read8(addr2 as usize)?, extra_cycle)
            }
            AddressingMode::Relative => {
                let offs = cpu.read8(pc1)? as i8 as i16;
                ((cpu.pc as i16).wrapping_add(offs) as u16, 0, false)
            }
            AddressingMode::ZeroPage => {
                let addr = cpu.bus.read(pc1)? as u16;
                (addr, cpu.bus.read(addr as usize)? as u8, false)
            }
            AddressingMode::ZeroPageX => {
                let mut addr = cpu.bus.read(pc1)?;
                addr = addr.wrapping_add(cpu.reg(Register::X));
                (addr as u16, cpu.bus.read(addr as usize)? as u8, false)
            }
            AddressingMode::ZeroPageY => {
                let mut addr = cpu.bus.read(pc1)?;
                addr = addr.wrapping_add(cpu.reg(Register::Y));
                (addr as u16, cpu.bus.read(addr as usize)? as u8, false)
            }
        };
        Ok(Operands {
            op1: addr,
            op2: arg,
            extra_cycle,
        })
    }
}
