//! Module for the Game Boy's memory management unit (MMU).
//!
//! The MMU maps the Game Boy's 16-bit address space to hardware.

use super::{Cartridge, MemoryMapped};

pub const WORK_RAM_SIZE: usize = 8 * 1024;
pub const HIGH_RAM_SIZE: usize = 127;

/// The MMU maps the Game Boy's 16-bit address space to hardware.
pub struct MMU {
    /// Cartridge is mapped to address ranges \[`0x0000`, `0x7FFF`\] and \[`0xA000`, `0xBFFF`\].
    pub cartridge: Cartridge,
    /// Work ROM is mapped to address range \[`0C0000`, `0xDFFF`\].
    pub work_ram: [u8; WORK_RAM_SIZE],
    // TODO: placeholder until more granular mapping is implemented.
    pub io_registers: [u8; 128],
    /// High RAM is mapped to address range \[`0xFF80`, `0xFFFE`\].
    pub high_ram: [u8; HIGH_RAM_SIZE],
}

impl MMU {
    /// Creates a new `MMU`.
    ///
    /// # Arguments
    /// * `cartridge` - The `Cartridge` to map memory to.
    pub fn new(cartridge: Cartridge) -> Self {
        Self {
            cartridge,
            work_ram: [0; WORK_RAM_SIZE],
            io_registers: [0; 128],
            high_ram: [0; HIGH_RAM_SIZE],
        }
    }
}

impl MemoryMapped for MMU {
    fn read_byte(&self, address: u16) -> u8 {
        match address {
            // External ROM
            0x0000..=0x7FFF => self.cartridge.read_byte(address),
            // VRAM
            0x8000..=0x9FFF => 0,
            // External RAM
            0xA000..=0xBFFF => self.cartridge.read_byte(address),
            // Work RAM
            0xC000..=0xDFFF => self.work_ram[(address - 0xC000) as usize],
            // Echo RAM
            0xE000..=0xFDFF => self.work_ram[(address - 0xE000) as usize],
            // OAM
            0xFE00..=0xFE9F => 0,
            // Not Usable
            0xFEA0..=0xFEFF => 0,
            // IO Registers
            0xFF00..=0xFF7F => self.io_registers[(address - 0xFF00) as usize],
            // HRAM
            0xFF80..=0xFFFE => self.high_ram[(address - 0xFF80) as usize],
            // IE
            0xFFFF => 0,
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        match address {
            // External ROM
            0x0000..=0x7FFF => self.cartridge.write_byte(address, value),
            // VRAM
            0x8000..=0x9FFF => (),
            // External RAM
            0xA000..=0xBFFF => self.cartridge.write_byte(address, value),
            // Work RAM
            0xC000..=0xDFFF => self.work_ram[(address - 0xC000) as usize] = value,
            // Echo RAM
            0xE000..=0xFDFF => self.work_ram[(address - 0xE000) as usize] = value,
            // OAM
            0xFE00..=0xFE9F => (),
            // Not Usable
            0xFEA0..=0xFEFF => (),
            // IO Registers
            0xFF00..=0xFF7F => self.io_registers[(address - 0xFF00) as usize] = value,
            // HRAM
            0xFF80..=0xFFFE => self.high_ram[(address - 0xFF80) as usize] = value,
            // IE
            0xFFFF => (),
        }
    }
}
