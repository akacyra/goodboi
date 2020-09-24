//! Module for Game Boy memory bank controllers (MBC).
//!
//! The Game Boy only has a 16-bit address bus, so MBCs are used in the cartridge to expand the availabe address space using memory banking.

use crate::MemoryMapped;

/// ROM banks are 16KB.
const ROM_BANK_SIZE: u32 = 16 * 1024;

/// RAM banks are 8KB.
const RAM_BANK_SIZE: u32 = 8 * 1024;

/// MBC that supports up to 2MB ROM and/or 32KB RAM.
pub struct MBC1 {
    rom: Vec<u8>,
    ram: Vec<u8>,
    num_rom_banks: u32,
    num_ram_banks: u32,
    ram_enabled: bool,
    rom_bank_register: u32,
    secondary_bank_register: u32,
    advanced_banking_mode: bool,
}

impl MBC1 {
    /// Creates a new `MBC1`.
    ///
    /// # Arguments
    /// * `rom` - Cartridge ROM.
    /// * `ram_size` - RAM size. `0` indicates that the cartridge does not have RAM.
    /// # Panics
    /// When `rom_size` is greater than 2MB or `ram_size` is greater than 32KB.
    pub fn new(rom: Vec<u8>, ram_size: u32) -> Self {
        let rom_size = rom.len();
        assert!(
            rom_size <= 2 * 1024 * 1024,
            "rom size must be less than 2MB"
        );
        assert!(ram_size <= 32 * 1024, "ram_size must be less than 32KB");
        Self {
            rom,
            ram: vec![0; ram_size as usize],
            num_rom_banks: rom_size as u32 / ROM_BANK_SIZE,
            num_ram_banks: ram_size / RAM_BANK_SIZE,
            ram_enabled: false,
            rom_bank_register: 1,
            secondary_bank_register: 0,
            advanced_banking_mode: false,
        }
    }

    fn is_large_rom(&self) -> bool {
        self.num_rom_banks > 32
    }

    fn is_large_ram(&self) -> bool {
        self.num_ram_banks > 0
    }

    fn has_ram(&self) -> bool {
        !self.ram.is_empty()
    }

    fn low_rom_bank(&self) -> u32 {
        if self.advanced_banking_mode {
            self.secondary_bank_register << 5
        } else {
            0
        }
    }

    fn high_rom_bank(&self) -> u32 {
        if self.is_large_rom() {
            (self.secondary_bank_register << 5) | self.rom_bank_register
        } else {
            self.rom_bank_register
        }
    }

    fn ram_bank(&self) -> u32 {
        if self.advanced_banking_mode {
            self.secondary_bank_register as u32
        } else {
            0
        }
    }
}

impl MemoryMapped for MBC1 {
    fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x7FFF => {
                let bank = if address < 0x4000 {
                    self.low_rom_bank()
                } else {
                    self.high_rom_bank()
                };
                let index = (bank * ROM_BANK_SIZE + address as u32 % ROM_BANK_SIZE) as usize;
                self.rom[index]
            }
            0xA000..=0xBFFF if self.has_ram() && self.ram_enabled => {
                let bank = self.ram_bank();
                let index = (bank * RAM_BANK_SIZE + address as u32 % RAM_BANK_SIZE) as usize;
                self.ram[index]
            }
            _ => 0,
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x1FFF if self.has_ram() => {
                self.ram_enabled = value & 0xF == 0xA;
            }
            0x2000..=0x3FFF => {
                let bank = value as u32 & 0x1F;
                // TODO: mask if num larger than max
                self.rom_bank_register = if bank == 0 { 1 } else { bank };
            }
            0x4000..=0x5FFF if self.is_large_rom() || self.is_large_ram() => {
                self.secondary_bank_register = value as u32 & 0x3;
            }
            0x6000..=0x7FFF if self.is_large_rom() || self.is_large_ram() => {
                self.advanced_banking_mode = value == 1;
            }
            0xA000..=0xBFFF if self.has_ram() && self.ram_enabled => {
                let bank = self.ram_bank();
                let index = (bank * RAM_BANK_SIZE + address as u32 % RAM_BANK_SIZE) as usize;
                self.ram[index] = value;
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn mbc1_simple_rom_banking() {
        let mut rom = vec![0; 4 * ROM_BANK_SIZE as usize];
        rom[0] = 0x12;
        rom[ROM_BANK_SIZE as usize] = 0x34;
        rom[3 * ROM_BANK_SIZE as usize] = 0x56;
        let mut mbc = MBC1::new(rom, 0);

        assert_eq!(mbc.read_byte(0x0000), 0x12, "low bank maps to bank 0");
        assert_eq!(
            mbc.read_byte(0x4000),
            0x34,
            "high bank maps to bank 1 at start"
        );

        mbc.write_byte(0x2000, 3);

        assert_eq!(
            mbc.read_byte(0x4000),
            0x56,
            "high bank can be changed to bank 3"
        );
    }

    #[test]
    fn mbc1_large_rom_banking() {
        let mut rom = vec![0; 64 * ROM_BANK_SIZE as usize];
        rom[63 * ROM_BANK_SIZE as usize] = 0x56;
        let mut mbc = MBC1::new(rom, 0);

        mbc.write_byte(0x2000, 0x1F);
        mbc.write_byte(0x4000, 0x01);

        assert_eq!(
            mbc.read_byte(0x4000),
            0x56,
            "high bank is selected using both bank registers"
        );
    }

    #[test]
    fn mbc1_advanced_rom_banking() {
        let mut rom = vec![0; 64 * ROM_BANK_SIZE as usize];
        rom[0] = 0x12;
        rom[32 * ROM_BANK_SIZE as usize] = 0x34;
        let mut mbc = MBC1::new(rom, 0);

        mbc.write_byte(0x6000, 1);

        assert_eq!(mbc.read_byte(0), 0x12, "low bank maps to bank 0");

        mbc.write_byte(0x4000, 0x1);

        assert_eq!(
            mbc.read_byte(0),
            0x34,
            "low bank can be switched in advanced ROM banking mode"
        );
    }

    #[test]
    fn mbc1_small_ram() {
        let mut mbc = MBC1::new(vec![], RAM_BANK_SIZE / 2);

        mbc.write_byte(0xA000, 0x12);

        assert_eq!(mbc.read_byte(0xA000), 0, "RAM is initially disabled");

        mbc.write_byte(0x0000, 0x0A);
        mbc.write_byte(0xA000, 0x12);

        assert_eq!(
            mbc.read_byte(0xA000),
            0x12,
            "RAM can be written and read after being enabled"
        );
    }

    #[test]
    fn mbc1_ram_banking() {
        let mut mbc = MBC1::new(vec![], 4 * RAM_BANK_SIZE);

        mbc.write_byte(0x0000, 0x0A);
        mbc.write_byte(0x4000, 0x01);
        mbc.write_byte(0xA000, 0x12);
        mbc.write_byte(0x6000, 1);

        assert_eq!(mbc.read_byte(0xA000), 0, "RAM bank can be swapped");

        mbc.write_byte(0xA000, 0x34);

        assert_eq!(
            mbc.read_byte(0xA000),
            0x34,
            "Mapped RAM bank can be written and read"
        );

        mbc.write_byte(0x4000, 0);

        assert_eq!(
            mbc.read_byte(0xA000),
            0x12,
            "Unmapped RAM banks are not modified"
        );
    }
}
