use gem::gem::Gem;
use gem::uci;
use std::io;

fn main() -> io::Result<()> {
    let mut gem = Gem::new();
    let mut buffer = String::new();
    loop {
        io::stdin().read_line(&mut buffer)?;
        if let Err(e) = uci::reader::read_uci_line(&buffer, &mut gem) {
            eprintln!("{e}");
            break;
        }
        buffer.clear();
    }
    Ok(())
}
