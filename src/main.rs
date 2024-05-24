mod board;

fn main() {
    println!("Starting Chess Board:");
    let mut b = board::starting_board();
    println!("{}", b);

    println!("Perft3: {:?}", board::perft(&mut b, 3));
}
