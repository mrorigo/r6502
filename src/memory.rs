use crate::cpu::Trap;

/// A simple representation of memory.
pub struct Memory<'a> {
    pub cells: &'a mut [u8],
    size: usize,
}

impl Memory<'_> {
    pub fn new<'a>(cells: &'a mut [u8]) -> Memory<'a> {
        let size = cells.len();
        Memory { cells, size }
    }
}

pub trait MemoryOperations {
    fn read8(&self, addr: usize) -> Result<u8, Trap>;
    fn write8(&mut self, addr: usize, value: u8) -> Result<(), Trap>;
    fn read16(&self, addr: usize) -> Result<u16, Trap>;
    fn write16(&mut self, addr: usize, value: u16) -> Result<(), Trap>;
}

impl MemoryOperations for Memory<'_> {
    fn read8(&self, addr: usize) -> Result<u8, Trap> {
        if addr > self.size {
            Err(Trap::AddressError(addr))
        } else {
            Ok(self.cells[addr as usize])
        }
    }

    fn read16(&self, addr: usize) -> Result<u16, Trap> {
        let hh = self.read8(addr + 1)?;
        let ll = self.read8(addr)?;
        Ok(((hh as u16) << 8) | (ll as u16))
    }

    fn write8(&mut self, addr: usize, value: u8) -> Result<(), Trap> {
        if addr > self.size {
            Err(Trap::AddressError(addr))
        } else {
            self.cells[addr] = value;
            Ok(())
        }
    }

    fn write16(&mut self, addr: usize, value: u16) -> Result<(), Trap> {
        self.write8(addr, (value & 0xff) as u8)?;
        self.write8(addr + 1, (value >> 8) as u8)?;
        Ok(())
    }
}
