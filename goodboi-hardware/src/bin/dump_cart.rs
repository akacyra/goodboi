use goodboi_hardware::memory::Cartridge;

fn main() {
    let mut args = std::env::args().skip(1);

    let path = args.next().unwrap_or_else(|| {
        println!("Usage: cart_dump path");
        std::process::exit(1);
    });

    let cart = Cartridge::from_file(path).unwrap_or_else(|error| {
        eprintln!("Error loading cartridge: {}", error);
        std::process::exit(1);
    });

    println!("Title = \"{}\"", cart.title());
    println!("Hardware = {:?}", cart.hardware());
    println!("ROM Size = {} bytes", cart.rom_size());
    println!("RAM Size = {} bytes", cart.ram_size());
}
