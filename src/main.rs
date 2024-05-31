mod board;
mod perft;

fn main() {
    println!("Starting Chess Board:");
    let mut b = board::starting_board();
    println!("{}", b);

    println!("Perft3: {:?}", perft::perft(&mut b, 7));
}
