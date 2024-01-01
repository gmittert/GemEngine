mod board;

fn main() {
    println!("Starting Chess Board:");
    let mut b = board::starting_board();
    println!("{}", b);

    let m1 = board::Move {
        from: board::e2(),
        to: board::e4(),
        turn: board::Turn::White,
        piece: board::Piece::Pawn,
        capture: None,
        is_check: false,
        is_mate: false,
    };
    println!("1.{}", m1);
    b.make_move(&m1);
    println!("{}", b);

    let m2 = board::Move {
        from: board::e7(),
        to: board::e5(),
        turn: board::Turn::Black,
        piece: board::Piece::Pawn,
        capture: None,
        is_check: false,
        is_mate: false,
    };

    println!("..{}", m2);
    b.make_move(&m2);
    println!("{}", b);
}
