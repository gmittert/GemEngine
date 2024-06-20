use gem::board;
use gem::gem;
use gem::perft;
use gem::uci;
use gem::hashmap;
use gem::zobrist;
use std::io;

fn main() -> io::Result<()> {
    let mut gem = gem::Gem::new();
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
