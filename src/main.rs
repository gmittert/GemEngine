mod board;
mod perft;
mod uci;

fn main() {
    println!("Starting Chess Board:");
    let mut b = board::starting_board();
    println!("{}", b);

    println!("Perft6: {:?}", perft::perft(&mut b, 6));
}
