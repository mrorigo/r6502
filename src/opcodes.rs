use std::ops::Shr;

use crate::cpu::{AddressingMode, Register, StatusFlags, Trap, CPU, STACK_BASE};

#[derive(Clone, Copy)]
pub struct OpCode {
    pub name: &'static str,
    pub op_impl: fn(&mut CPU) -> Result<(), Trap>,
    pub addressing_mode: AddressingMode,
    pub register: Option<Register>,
    pub size: usize,
    pub cycles: usize,
    pub extra_cycle: bool,
}

// Since all branch operations operate on Status Register flags,
// we implement it as a macro
macro_rules! branch {
    ($name:ident, $bit:expr, $flag:expr) => {
        pub fn $name(cpu: &mut CPU) -> Result<(), Trap> {
            if StatusFlags::is_set(cpu.sr, $bit) == $flag {
                let target = cpu.operands.op1 + cpu.opcode.size as u16 - 2;
                if target == cpu.pc.wrapping_sub(2) {
                    return Err(Trap::Break(cpu.pc as usize));
                }
                cpu.pc = target;
            }
            Ok(())
        }
    };
}

macro_rules! set_or_clear_status_bit {
    ($cpu:expr,$val:expr,$bit:ident) => {
        match $val {
            true => $cpu.sr |= StatusFlags::mask(StatusFlags::$bit),
            false => $cpu.sr &= !StatusFlags::mask(StatusFlags::$bit),
        }
    };
}

macro_rules! set_or_clear_nz {
    ($cpu:expr,$val:expr) => {
        set_or_clear_status_bit!($cpu, $val & 0x80 == 0x80, N);
        set_or_clear_status_bit!($cpu, $val == 0x0, Z);
    };
}

#[allow(non_snake_case, dead_code)]
impl OpCode {
    pub fn push_stack(cpu: &mut CPU, val: u8) -> Result<(), Trap> {
        let sp = cpu.reg(Register::SP);
        cpu.bus
            .write(STACK_BASE.wrapping_add(sp as u16) as usize, val)?;
        cpu.set_reg(Register::SP, sp.wrapping_sub(1));
        Ok(())
    }

    pub fn pop_stack(cpu: &mut CPU) -> Result<u8, Trap> {
        let sp = cpu.reg(Register::SP);
        cpu.set_reg(Register::SP, sp.wrapping_add(1));
        cpu.bus
            .read(STACK_BASE.wrapping_add(sp as u16).wrapping_add(1) as usize)
    }

    pub fn ADC(cpu: &mut CPU) -> Result<(), Trap> {
        let c: i16 = StatusFlags::is_set(cpu.sr, StatusFlags::C).into();
        let ra = cpu.reg(Register::A);
        let t: i16 = (ra as i16)
            .wrapping_add(cpu.operands.op1 as i16)
            .wrapping_add(c);
        let ovflw1 = (ra >> 7) != (cpu.operands.op2 >> 7) && (ra >> 7) != ((t as u8) >> 7);
        cpu.set_reg(Register::A, t as u8);

        set_or_clear_status_bit!(cpu, ovflw1, V);
        set_or_clear_status_bit!(cpu, t >= 0, C);
        set_or_clear_nz!(cpu, t as u8);
        Ok(())
    }

    pub fn AND(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::A) & cpu.operands.op2;
        cpu.set_reg(Register::A, value);
        set_or_clear_nz!(cpu, value as u8);
        Ok(())
    }
    pub fn EOR(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::A) ^ cpu.operands.op2;
        cpu.set_reg(Register::A, value);
        set_or_clear_nz!(cpu, value as u8);
        Ok(())
    }
    pub fn ORA(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::A) | cpu.operands.op2;
        cpu.set_reg(Register::A, value);
        set_or_clear_nz!(cpu, value as u8);
        Ok(())
    }

    fn impl_ASL(cpu: &mut CPU, val: u8) -> u8 {
        set_or_clear_status_bit!(cpu, (val & 0x080) == 0x80, C);
        let nval = val.wrapping_shl(1);
        set_or_clear_nz!(cpu, nval);
        nval
    }

    pub fn ASL(cpu: &mut CPU) -> Result<(), Trap> {
        match cpu.opcode.addressing_mode {
            AddressingMode::Accumulator => {
                let nval = OpCode::impl_ASL(cpu, cpu.reg(Register::A));
                cpu.set_reg(Register::A, nval);
            }
            _ => {
                let nval = OpCode::impl_ASL(cpu, cpu.operands.op2);
                cpu.bus.write(cpu.operands.op1 as usize, nval)?;
            }
        }
        Ok(())
    }

    pub fn ASO(cpu: &mut CPU) -> Result<(), Trap> {
        let nval = OpCode::impl_ASL(cpu, cpu.operands.op2);
        cpu.bus.write(cpu.operands.op1 as usize, nval)?;
        let nval2 = cpu.reg(Register::A) | nval;
        cpu.set_reg(Register::A, nval2);
        set_or_clear_nz!(cpu, nval2);
        Ok(())
    }

    pub fn BIT(cpu: &mut CPU) -> Result<(), Trap> {
        let op2 = cpu.operands.op2;
        set_or_clear_status_bit!(cpu, (op2 & cpu.reg(Register::A)) == 0, Z);
        set_or_clear_status_bit!(cpu, (op2 & StatusFlags::mask(StatusFlags::V)) == 0, V);
        set_or_clear_status_bit!(cpu, (op2 & StatusFlags::mask(StatusFlags::N)) == 0, N);
        Ok(())
    }

    branch! {BCS, StatusFlags::C, true}
    branch! {BCC, StatusFlags::C, false}
    branch! {BEQ, StatusFlags::Z, true}
    branch! {BNE, StatusFlags::Z, false}
    branch! {BMI, StatusFlags::N, true}
    branch! {BPL, StatusFlags::N, false}
    branch! {BVS, StatusFlags::V, true}
    branch! {BVC, StatusFlags::V, false}

    fn BRK(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.prev_pc = cpu.pc;
        // let lo = cpu.bus.read(0xfffe).unwrap();
        // let hi = cpu.bus.read(0xffff).unwrap();
        // cpu.pc = ((hi as u16) << 8) | (lo as u16);
        Err(Trap::Break(cpu.pc as usize))
        //        Ok(())
    }

    pub fn CLC(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, false, C);
        Ok(())
    }
    pub fn CLD(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, false, D);
        Ok(())
    }
    pub fn CLI(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, false, I);
        Ok(())
    }
    pub fn CLV(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, false, V);
        Ok(())
    }

    fn impl_CMP(cpu: &mut CPU, reg: Register) {
        let ov = cpu.reg(reg);
        let rv = ov.wrapping_sub(cpu.operands.op2);
        set_or_clear_nz!(cpu, rv);
        set_or_clear_status_bit!(cpu, cpu.operands.op2 <= ov, C)
    }

    pub fn SBC(cpu: &mut CPU) -> Result<(), Trap> {
        let c: i16 = StatusFlags::is_set(cpu.sr, StatusFlags::C).into();
        let ra = cpu.reg(Register::A);
        let t: i16 = (ra as i16)
            .wrapping_sub(cpu.operands.op1 as i16)
            .wrapping_sub(1 - c);
        let ovflw1 = (ra >> 7) != (cpu.operands.op2 >> 7) && (ra >> 7) != ((t as u8) >> 7);
        cpu.set_reg(Register::A, t as u8);

        set_or_clear_status_bit!(cpu, ovflw1, V);
        set_or_clear_status_bit!(cpu, t >= 0, C);
        set_or_clear_nz!(cpu, t as u8);
        Ok(())
    }

    pub fn CMP(cpu: &mut CPU) -> Result<(), Trap> {
        OpCode::impl_CMP(cpu, Register::A);
        Ok(())
    }
    pub fn CPX(cpu: &mut CPU) -> Result<(), Trap> {
        OpCode::impl_CMP(cpu, Register::X);
        Ok(())
    }
    pub fn CPY(cpu: &mut CPU) -> Result<(), Trap> {
        OpCode::impl_CMP(cpu, Register::Y);
        Ok(())
    }

    pub fn ISB(cpu: &mut CPU) -> Result<(), Trap> {
        let v = cpu.operands.op2.wrapping_add(1);
        cpu.bus.write(cpu.operands.op1 as usize, v)?;
        OpCode::SBC(cpu)?;
        Ok(())
    }
    pub fn DCP(cpu: &mut CPU) -> Result<(), Trap> {
        let v = cpu.operands.op2.wrapping_sub(1);
        cpu.bus.write(cpu.operands.op1 as usize, v)?;

        OpCode::impl_CMP(cpu, Register::A);
        Ok(())
    }

    pub fn INC(cpu: &mut CPU) -> Result<(), Trap> {
        let value = match cpu.opcode.addressing_mode {
            AddressingMode::Accumulator => {
                let v = cpu.reg(Register::A).wrapping_add(1);
                cpu.set_reg(Register::A, v);
                v
            }
            _ => {
                let v = cpu.operands.op2.wrapping_add(1);
                cpu.bus.write(cpu.operands.op1 as usize, v)?;
                v
            }
        };
        set_or_clear_nz!(cpu, value);
        Ok(())
    }
    pub fn INX(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::X).wrapping_add(1);
        cpu.set_reg(Register::X, value);
        set_or_clear_nz!(cpu, value);
        Ok(())
    }
    pub fn INY(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::Y).wrapping_add(1);
        cpu.set_reg(Register::Y, value);
        set_or_clear_nz!(cpu, value);
        Ok(())
    }

    pub fn DEC(cpu: &mut CPU) -> Result<(), Trap> {
        let value = match cpu.opcode.addressing_mode {
            AddressingMode::Accumulator => {
                let v = cpu.reg(Register::A).wrapping_sub(1);
                cpu.set_reg(Register::A, v);
                v
            }
            _ => {
                let v = cpu.operands.op2.wrapping_sub(1);
                cpu.bus.write(cpu.operands.op1 as usize, v)?;
                v
            }
        };
        set_or_clear_nz!(cpu, value);
        Ok(())
    }
    pub fn DEX(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::X).wrapping_sub(1);
        cpu.set_reg(Register::X, value);
        set_or_clear_nz!(cpu, value);
        Ok(())
    }
    pub fn DEY(cpu: &mut CPU) -> Result<(), Trap> {
        let value = cpu.reg(Register::Y).wrapping_sub(1);
        cpu.set_reg(Register::Y, value);
        set_or_clear_nz!(cpu, value);
        Ok(())
    }

    pub fn HLT(cpu: &mut CPU) -> Result<(), Trap> {
        Err(Trap::Halt(cpu.pc as usize))
    }

    pub fn JMP(cpu: &mut CPU) -> Result<(), Trap> {
        let old_pc = cpu.pc;
        cpu.pc = cpu.operands.op1.wrapping_sub(cpu.opcode.size as u16);
        if cpu.pc == old_pc - 3 {
            Err(Trap::Break(old_pc as usize)) // Infinite loop
        } else {
            Ok(())
        }
    }
    pub fn JSR(cpu: &mut CPU) -> Result<(), Trap> {
        let pc2 = cpu.pc.wrapping_add(2);
        OpCode::push_stack(cpu, (pc2 >> 8) as u8)?;
        OpCode::push_stack(cpu, (pc2 & 0xff) as u8)?;
        cpu.pc = cpu.operands.op1.wrapping_sub(cpu.opcode.size as u16);
        Ok(())
    }

    pub fn LAX(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::X, cpu.operands.op2);
        cpu.set_reg(Register::A, cpu.operands.op2);
        set_or_clear_nz!(cpu, cpu.operands.op2);
        Ok(())
    }
    pub fn LDA(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::A, cpu.operands.op2);
        set_or_clear_nz!(cpu, cpu.operands.op2);
        Ok(())
    }
    pub fn LDX(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::X, cpu.operands.op2);
        set_or_clear_nz!(cpu, cpu.operands.op2);
        Ok(())
    }
    pub fn LDY(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::Y, cpu.operands.op2);
        set_or_clear_nz!(cpu, cpu.operands.op2);
        Ok(())
    }

    pub fn LSR(cpu: &mut CPU) -> Result<(), Trap> {
        let v = match cpu.opcode.addressing_mode {
            AddressingMode::Accumulator => {
                let v = cpu.reg(Register::A);
                cpu.set_reg(Register::A, v.shr(1));
                v
            }
            _ => {
                let v: u8 = cpu.operands.op2;
                cpu.bus.write(cpu.operands.op1 as usize, v.shr(1))?;
                v
            }
        };
        set_or_clear_nz!(cpu, v);
        Ok(())
    }

    pub fn SRE(cpu: &mut CPU) -> Result<(), Trap> {
        let v: u8 = cpu.operands.op2.shr(1);
        cpu.bus.write(cpu.operands.op1 as usize, v)?;
        let a = cpu.reg(Register::A);
        let nav = a ^ v;
        cpu.set_reg(Register::A, nav);
        set_or_clear_nz!(cpu, nav);
        Ok(())
    }

    pub fn NOP(_cpu: &mut CPU) -> Result<(), Trap> {
        Ok(())
    }

    pub fn PHA(cpu: &mut CPU) -> Result<(), Trap> {
        OpCode::push_stack(cpu, cpu.reg(Register::A))?;
        Ok(())
    }
    pub fn PHP(cpu: &mut CPU) -> Result<(), Trap> {
        OpCode::push_stack(cpu, cpu.sr | 0x30)?;
        Ok(())
    }
    pub fn PLA(cpu: &mut CPU) -> Result<(), Trap> {
        let v = OpCode::pop_stack(cpu)?;
        cpu.set_reg(Register::A, v);
        set_or_clear_nz!(cpu, v);
        Ok(())
    }
    pub fn PLP(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.sr = (OpCode::pop_stack(cpu)? & !0x10) | 0x20;
        Ok(())
    }

    pub fn RLA(cpu: &mut CPU) -> Result<(), Trap> {
        let ar = OpCode::impl_ROL(cpu, cpu.operands.op2);
        cpu.bus.write(cpu.operands.op2 as usize, ar)?;
        let a = cpu.reg(Register::A);
        set_or_clear_nz!(cpu, a);
        Ok(())
    }
    pub fn ROL(cpu: &mut CPU) -> Result<(), Trap> {
        match cpu.opcode.addressing_mode {
            AddressingMode::Accumulator => {
                let ar = OpCode::impl_ROL(cpu, cpu.reg(Register::A));
                cpu.set_reg(Register::A, ar);
            }
            _ => {
                let ar = OpCode::impl_ROL(cpu, cpu.operands.op2);
                cpu.bus.write(cpu.operands.op1 as usize, ar)?;
            }
        }
        Ok(())
    }
    pub fn ROR(cpu: &mut CPU) -> Result<(), Trap> {
        match cpu.opcode.addressing_mode {
            AddressingMode::Accumulator => {
                let ar = OpCode::impl_ROR(cpu, cpu.reg(Register::A));
                cpu.set_reg(Register::A, ar);
            }
            _ => {
                let ar = OpCode::impl_ROR(cpu, cpu.operands.op2);
                cpu.bus.write(cpu.operands.op1 as usize, ar)?;
            }
        }
        Ok(())
    }

    fn impl_ROR(cpu: &mut CPU, value: u8) -> u8 {
        let oc = StatusFlags::is_set(value, StatusFlags::C) as u8;

        set_or_clear_status_bit!(cpu, (value & 1) == 1, C);
        let x = (value >> 1) | (oc << 7);
        set_or_clear_nz!(cpu, x);
        x
    }

    fn impl_ROL(cpu: &mut CPU, value: u8) -> u8 {
        let oc = StatusFlags::is_set(value, StatusFlags::C) as u8;

        set_or_clear_status_bit!(cpu, (value & 1) == 1, C);
        let x = (value << 1) | oc;
        set_or_clear_nz!(cpu, x);
        x
    }

    pub fn RRA(_cpu: &mut CPU) -> Result<(), Trap> {
        todo!();
        // Ok(())
    }
    pub fn RTI(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.sr = OpCode::pop_stack(cpu)? | 0x20;
        cpu.pc = OpCode::pop_stack(cpu)? as u16 | ((OpCode::pop_stack(cpu)? as u16) << 8) - 1;

        Ok(())
    }
    pub fn RTS(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.pc = (OpCode::pop_stack(cpu)? as u16) | ((OpCode::pop_stack(cpu)? as u16) << 8);
        Ok(())
    }
    pub fn SAX(cpu: &mut CPU) -> Result<(), Trap> {
        let a = cpu.reg(Register::A);
        let x = cpu.reg(Register::X);
        let v = a & x;
        cpu.bus.write(cpu.operands.op1 as usize, v)?;
        Ok(())
    }

    pub fn SEC(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, true, C);
        Ok(())
    }
    pub fn SED(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, true, D);
        Ok(())
    }
    pub fn SEI(cpu: &mut CPU) -> Result<(), Trap> {
        set_or_clear_status_bit!(cpu, true, I);
        Ok(())
    }

    pub fn SKW(_cpu: &mut CPU) -> Result<(), Trap> {
        Ok(())
    }

    pub fn STA(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.bus
            .write(cpu.operands.op1 as usize, cpu.reg(Register::A))
    }
    pub fn STX(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.bus
            .write(cpu.operands.op1 as usize, cpu.reg(Register::X))?;
        Ok(())
    }
    pub fn STY(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.bus
            .write(cpu.operands.op1 as usize, cpu.reg(Register::Y))?;
        Ok(())
    }

    pub fn TAS(cpu: &mut CPU) -> Result<(), Trap> {
        // ANDs the contents of the A and X registers (without changing the
        // contents of either register) and transfers the result to the stack pointer.
        let a = cpu.reg(Register::A);
        let x = cpu.reg(Register::X);
        let v = a & x;
        cpu.set_reg(Register::SP, v);
        // ..then ANDs that result with the contents of the high byte of  the target address
        let v = v & cpu.operands.op2;
        cpu.bus.write(cpu.operands.op1 as usize, v)?;
        Ok(())
    }

    pub fn TAX(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::X, cpu.reg(Register::A));
        Ok(())
    }
    pub fn TAY(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::Y, cpu.reg(Register::A));
        Ok(())
    }
    pub fn TSX(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::X, cpu.reg(Register::SP));
        Ok(())
    }
    pub fn TXA(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::A, cpu.reg(Register::X));
        Ok(())
    }
    pub fn TXS(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::SP, cpu.reg(Register::X));
        Ok(())
    }
    pub fn TYA(cpu: &mut CPU) -> Result<(), Trap> {
        cpu.set_reg(Register::A, cpu.reg(Register::Y));
        Ok(())
    }

    pub fn NOT_IMPL(cpu: &mut CPU) -> Result<(), Trap> {
        panic!("Unimplemented opcode @ {:#x?}", cpu.pc)
    }
}

macro_rules! op {
    ( $op:ident, $addr_mode:expr,$reg:expr,$size:expr, $cycles:expr, $extra:expr) => {
        OpCode {
            name: stringify!($op),
            op_impl: OpCode::$op,
            addressing_mode: $addr_mode,
            register: $reg,
            size: $size,
            cycles: $cycles,
            extra_cycle: $extra,
        }
    };
}

/// A mapping from opcode to OpCode
pub const OPCODE_MAP: [OpCode; 256] = [
    op!(BRK, AddressingMode::Implied, None, 1, 7, false),
    op!(
        ORA,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        ASO,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::Relative, None, 2, 3, false), // INVALID OP $0C (NOP $XX)
    op!(
        ORA,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(ASL, AddressingMode::ZeroPage, None, 2, 5, false),
    op!(
        ASO,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(PHP, AddressingMode::Implied, None, 1, 3, false),
    op!(
        ORA,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(
        ASL,
        AddressingMode::Accumulator,
        Some(Register::A),
        1,
        2,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false), // ANC, not used by 2A03
    op!(SKW, AddressingMode::Absolute, None, 3, 4, true),
    op!(
        ORA,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(ASL, AddressingMode::Absolute, None, 3, 6, false),
    op!(
        ASO,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    /* $10 */
    op!(BPL, AddressingMode::Relative, None, 2, 2, false),
    op!(
        ORA,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        ASO,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::ZeroPageX, None, 2, 4, false), // NOP $XX,x
    op!(
        ORA,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(ASL, AddressingMode::ZeroPageX, None, 2, 6, false),
    op!(
        ASO,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(CLC, AddressingMode::Implied, None, 1, 2, false),
    op!(
        ORA,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        ASO,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(SKW, AddressingMode::AbsoluteX, None, 3, 4, true),
    op!(
        ORA,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(ASL, AddressingMode::AbsoluteX, None, 3, 7, true),
    op!(
        ASO,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    /* $20 */
    op!(JSR, AddressingMode::Absolute, None, 3, 6, false),
    op!(
        AND,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        RLA,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(
        BIT,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(
        AND,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(
        ROL,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(
        RLA,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(PLP, AddressingMode::Implied, None, 1, 4, false),
    op!(
        AND,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(
        ROL,
        AddressingMode::Accumulator,
        Some(Register::A),
        1,
        2,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false), // ANC
    op!(BIT, AddressingMode::Absolute, None, 3, 4, false),
    op!(
        AND,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(
        ROL,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    op!(
        RLA,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    /* $30 */
    op!(BMI, AddressingMode::Relative, None, 2, 2, false),
    op!(
        AND,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        RLA,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::ZeroPageX, None, 2, 4, false), // NOP $XX,X
    op!(
        AND,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(
        ROL,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(
        RLA,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(SEC, AddressingMode::Implied, None, 1, 2, false),
    op!(
        AND,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        RLA,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(SKW, AddressingMode::AbsoluteX, None, 3, 4, true),
    op!(
        AND,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(
        ROL,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(
        RLA,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    /* $40 */
    op!(RTI, AddressingMode::Implied, None, 1, 6, false),
    op!(
        EOR,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        SRE,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::Relative, None, 2, 3, false),
    op!(
        EOR,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(LSR, AddressingMode::ZeroPage, None, 2, 5, false),
    op!(
        SRE,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(PHA, AddressingMode::Implied, Some(Register::A), 1, 3, false),
    op!(
        EOR,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(
        LSR,
        AddressingMode::Accumulator,
        Some(Register::A),
        1,
        2,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false), // ALR
    op!(JMP, AddressingMode::Absolute, None, 3, 3, false),
    op!(
        EOR,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(LSR, AddressingMode::Absolute, None, 3, 6, false),
    op!(
        SRE,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    /* $50 */
    op!(BVC, AddressingMode::Relative, None, 2, 2, false),
    op!(
        EOR,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        SRE,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::ZeroPageX, None, 2, 4, false), // NOP $XX,X
    op!(
        EOR,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(
        LSR,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(
        SRE,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(CLI, AddressingMode::Implied, None, 1, 2, false),
    op!(
        EOR,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        SRE,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(SKW, AddressingMode::AbsoluteX, None, 3, 4, true),
    op!(
        EOR,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(
        LSR,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(
        SRE,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    /* $60 */
    op!(RTS, AddressingMode::Implied, None, 1, 6, false),
    op!(
        ADC,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        RRA,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::Relative, None, 2, 3, false),
    op!(
        ADC,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(ROR, AddressingMode::ZeroPage, None, 2, 5, false),
    op!(
        RRA,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(PLA, AddressingMode::Implied, Some(Register::A), 1, 4, false),
    op!(
        ADC,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(
        ROR,
        AddressingMode::Accumulator,
        Some(Register::A),
        1,
        2,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false), // ARR
    op!(JMP, AddressingMode::Indirect, None, 3, 5, false),
    op!(
        ADC,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(ROR, AddressingMode::Absolute, None, 3, 6, false),
    op!(
        RRA,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    /* $70 */
    op!(BVS, AddressingMode::Relative, None, 2, 2, false),
    op!(
        ADC,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        RRA,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::ZeroPageX, None, 2, 4, false), // NOP $XX,X
    op!(ADC, AddressingMode::ZeroPageX, None, 2, 4, false),
    op!(ROR, AddressingMode::ZeroPageX, None, 2, 6, false),
    op!(
        RRA,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(SEI, AddressingMode::Implied, None, 1, 2, false),
    op!(
        ADC,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        RRA,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(SKW, AddressingMode::AbsoluteX, None, 3, 4, true),
    op!(
        ADC,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(ROR, AddressingMode::AbsoluteX, None, 3, 7, false),
    op!(
        RRA,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    /* $80 */
    op!(NOP, AddressingMode::Immediate, None, 2, 2, false), // NOP #$XX
    op!(
        STA,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false), // "may be HLT"
    op!(
        SAX,
        AddressingMode::IndirectX,
        Some(Register::X),
        2,
        6,
        false
    ),
    op!(
        STY,
        AddressingMode::ZeroPage,
        Some(Register::Y),
        2,
        3,
        false
    ),
    op!(
        STA,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(
        STX,
        AddressingMode::ZeroPage,
        Some(Register::X),
        2,
        3,
        false
    ),
    op!(
        SAX,
        AddressingMode::ZeroPage,
        Some(Register::X),
        2,
        3,
        false
    ),
    op!(DEY, AddressingMode::Implied, Some(Register::Y), 1, 2, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(TXA, AddressingMode::Implied, Some(Register::A), 1, 2, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        STY,
        AddressingMode::Absolute,
        Some(Register::Y),
        3,
        4,
        false
    ),
    op!(
        STA,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(
        STX,
        AddressingMode::Absolute,
        Some(Register::X),
        3,
        4,
        false
    ),
    op!(
        SAX,
        AddressingMode::Absolute,
        Some(Register::X),
        3,
        4,
        false
    ),
    /* $90 */
    op!(BCC, AddressingMode::Relative, None, 2, 2, false),
    op!(
        STA,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        STY,
        AddressingMode::ZeroPageX,
        Some(Register::Y),
        2,
        4,
        false
    ),
    op!(
        STA,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(
        STX,
        AddressingMode::ZeroPageY,
        Some(Register::X),
        2,
        4,
        false
    ),
    op!(
        SAX,
        AddressingMode::ZeroPageY,
        Some(Register::X),
        2,
        4,
        false
    ),
    op!(TYA, AddressingMode::Implied, Some(Register::A), 1, 2, false),
    op!(
        STA,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        5,
        false
    ),
    op!(
        TXS,
        AddressingMode::Implied,
        Some(Register::SP),
        1,
        2,
        false
    ),
    op!(TAS, AddressingMode::Implied, None, 3, 5, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        STA,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        5,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    /* $A0 */
    op!(
        LDY,
        AddressingMode::Immediate,
        Some(Register::Y),
        2,
        2,
        false
    ),
    op!(
        LDA,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(
        LDX,
        AddressingMode::Immediate,
        Some(Register::X),
        2,
        2,
        false
    ),
    op!(
        LAX,
        AddressingMode::IndirectX,
        Some(Register::X),
        2,
        6,
        false
    ),
    op!(
        LDY,
        AddressingMode::ZeroPage,
        Some(Register::Y),
        2,
        3,
        false
    ),
    op!(
        LDA,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(
        LDX,
        AddressingMode::ZeroPage,
        Some(Register::X),
        2,
        3,
        false
    ),
    op!(
        LAX,
        AddressingMode::ZeroPage,
        Some(Register::X),
        2,
        3,
        false
    ),
    op!(TAY, AddressingMode::Implied, Some(Register::Y), 1, 2, false),
    op!(
        LDA,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(TAX, AddressingMode::Implied, Some(Register::X), 1, 2, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        LDY,
        AddressingMode::Absolute,
        Some(Register::Y),
        3,
        4,
        false
    ),
    op!(
        LDA,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(
        LDX,
        AddressingMode::Absolute,
        Some(Register::X),
        3,
        4,
        false
    ),
    op!(
        LAX,
        AddressingMode::Absolute,
        Some(Register::X),
        3,
        4,
        false
    ),
    /* $B0 */
    op!(BCS, AddressingMode::Relative, None, 2, 2, false),
    op!(
        LDA,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        LAX,
        AddressingMode::IndirectY,
        Some(Register::X),
        2,
        5,
        true
    ),
    op!(
        LDY,
        AddressingMode::ZeroPageX,
        Some(Register::Y),
        2,
        4,
        false
    ),
    op!(
        LDA,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(
        LDX,
        AddressingMode::ZeroPageY,
        Some(Register::X),
        2,
        4,
        false
    ),
    op!(
        LAX,
        AddressingMode::ZeroPageY,
        Some(Register::X),
        2,
        4,
        false
    ),
    op!(CLV, AddressingMode::Implied, None, 1, 2, false),
    op!(
        LDA,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(TSX, AddressingMode::Implied, Some(Register::X), 1, 2, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        LDY,
        AddressingMode::AbsoluteX,
        Some(Register::Y),
        3,
        4,
        true
    ),
    op!(
        LDA,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(
        LDX,
        AddressingMode::AbsoluteY,
        Some(Register::X),
        3,
        4,
        true
    ),
    op!(
        LAX,
        AddressingMode::AbsoluteY,
        Some(Register::X),
        3,
        4,
        true
    ),
    /* $C0 */
    op!(
        CPY,
        AddressingMode::Immediate,
        Some(Register::Y),
        2,
        2,
        false
    ),
    op!(
        CMP,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false), // "may be HLT"
    op!(
        DCP,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(
        CPY,
        AddressingMode::ZeroPage,
        Some(Register::Y),
        2,
        3,
        false
    ),
    op!(
        CMP,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(
        DEC,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(
        DCP,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(INY, AddressingMode::Implied, Some(Register::Y), 1, 2, false),
    op!(
        CMP,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(DEX, AddressingMode::Implied, Some(Register::X), 1, 2, false),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        CPY,
        AddressingMode::Absolute,
        Some(Register::Y),
        3,
        4,
        false
    ),
    op!(
        CMP,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(
        DEC,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    op!(
        DCP,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    /* $D0 */
    op!(BNE, AddressingMode::Relative, None, 2, 2, false),
    op!(
        CMP,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        DCP,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::ZeroPageX, None, 2, 4, false), // NOP $XX,X
    op!(
        CMP,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(
        DEC,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(
        DCP,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(CLD, AddressingMode::Implied, None, 1, 2, false),
    op!(
        CMP,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        DCP,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(SKW, AddressingMode::AbsoluteX, None, 3, 4, true),
    op!(
        CMP,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(
        DEC,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(
        DCP,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    /* $E0 */
    op!(
        CPX,
        AddressingMode::Immediate,
        Some(Register::X),
        2,
        2,
        false
    ),
    op!(
        SBC,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(NOT_IMPL, AddressingMode::Implied, None, 1, 0, false),
    op!(
        ISB,
        AddressingMode::IndirectX,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(
        CPX,
        AddressingMode::ZeroPage,
        Some(Register::X),
        2,
        3,
        false
    ),
    op!(
        SBC,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        3,
        false
    ),
    op!(
        INC,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(
        ISB,
        AddressingMode::ZeroPage,
        Some(Register::A),
        2,
        5,
        false
    ),
    op!(INX, AddressingMode::Implied, Some(Register::X), 1, 2, false),
    op!(
        SBC,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        SBC,
        AddressingMode::Immediate,
        Some(Register::A),
        2,
        2,
        false
    ),
    op!(
        CPX,
        AddressingMode::Absolute,
        Some(Register::X),
        3,
        4,
        false
    ),
    op!(
        SBC,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        4,
        false
    ),
    op!(
        INC,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    op!(
        ISB,
        AddressingMode::Absolute,
        Some(Register::A),
        3,
        6,
        false
    ),
    /* $F0 */
    op!(BEQ, AddressingMode::Relative, None, 2, 2, false),
    op!(
        SBC,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        5,
        true
    ),
    op!(HLT, AddressingMode::Implied, None, 1, 0, false),
    op!(
        ISB,
        AddressingMode::IndirectY,
        Some(Register::A),
        2,
        8,
        false
    ),
    op!(NOP, AddressingMode::ZeroPageX, None, 2, 4, false), // NOP $XX,X
    op!(
        SBC,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        4,
        false
    ),
    op!(
        INC,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(
        ISB,
        AddressingMode::ZeroPageX,
        Some(Register::A),
        2,
        6,
        false
    ),
    op!(SED, AddressingMode::Implied, None, 1, 2, false),
    op!(
        SBC,
        AddressingMode::AbsoluteY,
        Some(Register::Y),
        3,
        4,
        true
    ),
    op!(NOP, AddressingMode::Implied, None, 1, 2, false),
    op!(
        ISB,
        AddressingMode::AbsoluteY,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(SKW, AddressingMode::AbsoluteX, None, 3, 4, true),
    op!(
        SBC,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        4,
        true
    ),
    op!(
        INC,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
    op!(
        ISB,
        AddressingMode::AbsoluteX,
        Some(Register::A),
        3,
        7,
        false
    ),
];
