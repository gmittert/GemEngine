mod board;
mod perft;
mod uci;
mod gem;
use std::io;

fn main() -> io::Result<()> {
    let mut gem = gem::Gem{board: board::starting_board()};
    let mut buffer = String::new();
    loop {
        io::stdin().read_line(&mut buffer)?;
        eprintln!("Line: {}", buffer);
        if let Err(e) = uci::reader::read_uci_line(&buffer, &mut gem) {
            eprintln!("{}", e);
            break;
        }
        buffer.clear();
    }
    Ok(())
}
