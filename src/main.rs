use crate::board::starting_board;

mod board;

fn main() {
    println!("Starting Chess Board:");
    println!("{}", starting_board());
}
