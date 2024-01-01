use crate::board::posn::*;

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Piece {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[repr(u8)]
#[derive(Copy, Clone)]
pub enum Turn {
    Black,
    White,
}

pub struct Move {
    pub from: Posn,
    pub to: Posn,
    pub turn: Turn,
    pub piece: Piece,
    pub capture: Option<Piece>,
}

pub fn generate_legal_moves() -> Vec<Move> {
    vec![]
}

#[cfg(test)]
mod tests {
    use crate::board::*;
    fn perft(b: &mut Board, depth: u8) -> u64 {
        let mut nodes = 0;
        if depth == 0 {
            return 1;
        }

        let moves = generate_legal_moves();

        for m in moves {
            b.make_move(&m);
            nodes += perft(b, depth - 1);
            b.undo_move(&m);
        }
        nodes
    }

    #[test]
    fn pertf0() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 0), 1);
    }
    #[test]
    fn pertf1() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 1), 20);
    }
    #[test]
    fn pertf2() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 2), 400);
    }
    #[test]
    fn pertf3() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 3), 8902);
    }
    #[test]
    fn pertf4() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 4), 197281);
    }
}
