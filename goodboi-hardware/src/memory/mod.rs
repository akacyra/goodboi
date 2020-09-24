//! Traits and types used for the Game Boy memory map unit (MMU) and cartridges.

mod cartridge;
mod mbc;
mod mmu;

pub use cartridge::Cartridge;
pub use mmu::MMU;

/// Trait for types that can have data and operations mapped to addresses in a 16-bit address space.
pub trait MemoryMapped {
    /// Reads a byte.
    ///
    /// # Arguments
    /// * `address` - Memory address to read.
    fn read_byte(&self, address: u16) -> u8;

    /// Reads a litte-endian word (two bytes).
    ///
    /// # Arguments
    /// * `address` - Memory address to start reading from.
    fn read_word(&self, address: u16) -> u16 {
        let low = self.read_byte(address) as u16;
        let high = self.read_byte(address.wrapping_add(1)) as u16;
        high << 8 | low
    }

    /// Writes a byte.
    ///
    /// # Arguments
    /// * `address` - Memory address to write.
    /// * `value` - Byte value to write.
    fn write_byte(&mut self, address: u16, value: u8);

    /// Writes a litte-endian word (two bytes).
    ///
    /// # Arguments
    /// * `address` - Memory address to start writing to.
    /// * `value` - Word value to write.
    fn write_word(&mut self, address: u16, value: u16) {
        self.write_byte(address, value as u8);
        self.write_byte(address.wrapping_add(1), (value >> 8) as u8);
    }
}
