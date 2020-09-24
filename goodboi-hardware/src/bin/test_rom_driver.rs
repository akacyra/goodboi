use goodboi_hardware::cpu::CPU;
use goodboi_hardware::memory::{Cartridge, MemoryMapped, MMU};

fn main() {
    let mut args = std::env::args().skip(1);

    let path = args.next().unwrap_or_else(|| {
        println!("Usage: test_rom_driver path");
        std::process::exit(1);
    });

    let cartridge = Cartridge::from_file(path).unwrap_or_else(|error| {
        eprintln!("Error reading cartridge: {}", error);
        std::process::exit(1);
    });

    let mut mmu = MMU::new(cartridge);

    let mut cpu = CPU::new();

    cpu.pc = 0x0100;
    cpu.sp = 0xFFFE;
    cpu.af.0 = 0x01B0;
    cpu.bc.0 = 0x0013;
    cpu.de.0 = 0x00D8;
    cpu.hl.0 = 0x014D;

    mmu.write_byte(0xFF40, 0x91);
    mmu.write_byte(0xFF47, 0xFC);
    mmu.write_byte(0xFF48, 0xFF);
    mmu.write_byte(0xFF49, 0xFF);

    loop {
        cpu.step(&mut mmu);

        if mmu.read_byte(0xFF02) == 0x81 {
            let ch = mmu.read_byte(0xFF01) as char;
            print!("{}", ch);
            mmu.write_byte(0xFF02, 0x1);
        }
    }
}
