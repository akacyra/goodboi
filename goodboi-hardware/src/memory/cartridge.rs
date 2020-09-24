//! Module for Game Boy cartridges.

use std::{collections::HashSet, fs, path::Path};

use super::{mbc::MBC1, MemoryMapped};

/// External hardware that a `Cartridge` can have.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Hardware {
    ROM,
    RAM,
    MBC1,
}

enum MBC {
    NoMBC(NoMBC),
    MBC1(MBC1),
}

/// Contains a cartridge's metadata and memory.
pub struct Cartridge {
    title: String,
    hardware: HashSet<Hardware>,
    rom_size: usize,
    ram_size: usize,
    mbc: MBC,
}

impl Cartridge {
    /// Creates a new `Cartridge` from a ROM file.
    ///
    /// # Arguments
    /// * `path` - Path to the cartridge ROM file.
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<Self, String> {
        let rom =
            fs::read(path).map_err(|error| format!("Error reading cartridge ROM: {}", error))?;
        Self::from_rom(rom)
    }

    /// Creates a new `Cartridge` from ROM bytes.
    ///
    /// # Arguments
    /// * `rom` - ROM bytes.
    pub fn from_rom(rom: Vec<u8>) -> Result<Self, String> {
        if rom.len() < 0x150 {
            return Err("ROM does not have full cartridge header".to_string());
        }
        let title = parse_title(&rom[0x134..=0x143]);
        let hardware =
            parse_hardware(rom[0x147]).ok_or_else(|| "Invalid cartridge type byte".to_string())?;
        let rom_size =
            parse_rom_size(rom[0x148]).ok_or_else(|| "Invalid ROM size byte".to_string())?;
        if rom_size != rom.len() {
            return Err("ROM size in header and actual size do not match".to_string());
        }
        let ram_size =
            parse_ram_size(rom[0x149]).ok_or_else(|| "Invalid RAM size byte".to_string())?;
        if ram_size > 0 && !hardware.contains(&Hardware::RAM) {
            return Err(
                "RAM size in header is non-zero but cartridge type does not have RAM".to_string(),
            );
        }
        let mbc = if hardware.contains(&Hardware::ROM) {
            MBC::NoMBC(NoMBC::new(rom, ram_size as u32))
        } else if hardware.contains(&Hardware::MBC1) {
            MBC::MBC1(MBC1::new(rom, ram_size as u32))
        } else {
            return Err("Cartridge header does not have MBC type specified".to_string());
        };
        Ok(Self {
            title,
            hardware,
            rom_size,
            ram_size,
            mbc,
        })
    }

    /// Returns the title of the game in upper case ASCII.
    pub fn title(&self) -> &str {
        &self.title
    }

    /// Returns the hardware present in the cartridge.
    pub fn hardware(&self) -> &HashSet<Hardware> {
        &self.hardware
    }

    /// Returns the size of ROM in bytes.
    pub fn rom_size(&self) -> usize {
        self.rom_size
    }

    /// Returns the size of RAM in bytes.
    pub fn ram_size(&self) -> usize {
        self.ram_size
    }
}

impl MemoryMapped for Cartridge {
    fn read_byte(&self, address: u16) -> u8 {
        match &self.mbc {
            MBC::NoMBC(no_mbc) => no_mbc.read_byte(address),
            MBC::MBC1(mbc) => mbc.read_byte(address),
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        match &mut self.mbc {
            MBC::NoMBC(no_mbc) => no_mbc.write_byte(address, value),
            MBC::MBC1(mbc) => mbc.write_byte(address, value),
        }
    }
}

fn parse_title(bytes: &[u8]) -> String {
    bytes
        .iter()
        .cloned()
        .take_while(|&b| b != 0)
        .map(|b| b as char)
        .collect()
}

fn parse_hardware(value: u8) -> Option<HashSet<Hardware>> {
    let hardware = match value {
        0x00 => Some(vec![Hardware::ROM]),
        0x01 => Some(vec![Hardware::MBC1]),
        0x02 => Some(vec![Hardware::MBC1, Hardware::RAM]),
        0x08 => Some(vec![Hardware::ROM, Hardware::RAM]),
        _ => None,
    };
    hardware.map(|hw| hw.into_iter().collect())
}

fn parse_rom_size(value: u8) -> Option<usize> {
    match value {
        0x00..=0x06 => Some(1 << (15 + value)),
        _ => None,
    }
}

fn parse_ram_size(value: u8) -> Option<usize> {
    match value {
        0x00 => Some(0),
        0x01 => Some(2 * 1024),
        0x02 => Some(8 * 1024),
        _ => None,
    }
}

/// Cartridge memory that supports up to 32KB ROM and 8KB RAM, so it does not use a MBC.
pub struct NoMBC {
    rom: Vec<u8>,
    ram: Vec<u8>,
}

impl NoMBC {
    /// Creates a new `NoMBC`.
    ///
    /// # Arguments
    /// * `rom` - Cartridge ROM.
    /// * `ram_size` - RAM size. `0` indicates that the cartridge does not have RAM.
    /// # Panics
    /// When `rom_size` is greater than 32KB or `ram_size` is greater than 8KB.
    pub fn new(rom: Vec<u8>, ram_size: u32) -> Self {
        assert!(rom.len() <= 32 * 1024, "rom size must be less than 32KB");
        assert!(ram_size <= 8 * 1024, "ram_size must be less than 8KB");
        Self {
            rom,
            ram: vec![0; ram_size as usize],
        }
    }
}

impl MemoryMapped for NoMBC {
    fn read_byte(&self, address: u16) -> u8 {
        match address {
            0x0000..=0x7FFF => self.rom[address as usize],
            0xA000..=0xBFFF => {
                let offset = address as usize - 0xA000;
                if offset < self.ram.len() {
                    self.ram[offset]
                } else {
                    0
                }
            }
            _ => 0,
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        match address {
            0x0000..=0x7FFF => self.rom[address as usize] = value,
            0xA000..=0xBFFF => {
                let offset = address as usize - 0xA000;
                if offset < self.ram.len() {
                    self.ram[offset] = value;
                }
            }
            _ => (),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn no_mbc_can_read_rom() {
        let mut rom = vec![0; 10];
        rom[0] = 0x12;
        let no_mbc = NoMBC::new(rom, 0);

        assert_eq!(no_mbc.read_byte(0), 0x12);
    }

    #[test]
    fn no_mbc_can_read_and_write_ram() {
        let mut no_mbc = NoMBC::new(vec![0; 10], 10);

        no_mbc.write_byte(0xA000, 0x12);

        assert_eq!(no_mbc.read_byte(0xA000), 0x12);
    }

    #[test]
    fn cartride_parses_header_data() {
        let mut rom = vec![0; 32 * 1024];
        rom[0x134] = b'A';
        rom[0x135] = b'B';
        rom[0x136] = b'C';
        rom[0x137] = b'\0';
        rom[0x147] = 0;
        rom[0x148] = 0;
        rom[0x149] = 0;
        let cartridge = Cartridge::from_rom(rom);

        assert!(cartridge.is_ok(), "cartridge created without error");

        let cartridge = cartridge.unwrap();

        assert_eq!(cartridge.title(), "ABC", "title parsed correctly");
        assert_eq!(cartridge.hardware().len(), 1, "cartridge type parsed");
        assert!(
            cartridge.hardware.contains(&Hardware::ROM),
            "cartridge hardware is correct"
        );
        assert_eq!(cartridge.rom_size(), 32 * 1024, "ROM size parsed correctly");
        assert_eq!(cartridge.ram_size(), 0, "RAM size parsed correctly");
    }
}
