use crate::board::posn::*;
use std::fmt;
use std::ops::Not;

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Piece {
    Pawn,
    Rook,
    Knight,
    Bishop,
    Queen,
    King,
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Turn {
    Black,
    White,
}

impl Not for Turn {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Turn::White => Turn::Black,
            Turn::Black => Turn::White,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Move {
    pub from: Posn,
    pub to: Posn,
    pub turn: Turn,
    pub piece: Piece,
    pub capture: Option<Piece>,
    pub is_check: bool,
    pub is_mate: bool,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let is_check = if self.is_check {
            "+"
        } else if self.is_mate {
            "#"
        } else {
            ""
        };

        let icon = match (self.piece, self.turn) {
            (Piece::Rook, Turn::Black) => "♖",
            (Piece::Rook, Turn::White) => "♜",
            (Piece::Knight, Turn::Black) => "♘",
            (Piece::Knight, Turn::White) => "♞",
            (Piece::Bishop, Turn::Black) => "♗",
            (Piece::Bishop, Turn::White) => "♝",
            (Piece::Queen, Turn::Black) => "♛",
            (Piece::Queen, Turn::White) => "♛",
            (Piece::King, Turn::Black) => "♔",
            (Piece::King, Turn::White) => "♚",
            (Piece::Pawn, _) => match self.capture {
                Some(_) => {
                    return write!(
                        f,
                        "{}x{}{}{}",
                        self.from.file(),
                        self.to.file(),
                        self.to.rank(),
                        is_check
                    )
                }
                None => return write!(f, "{}{}{}", self.to.file(), self.to.rank(), is_check),
            },
        };
        let capture = match self.capture {
            Some(_) => "x",
            None => "",
        };
        write!(
            f,
            "{}{}{}{}{}",
            icon,
            capture,
            self.to.file(),
            self.to.rank(),
            is_check
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;

    pub fn generate_legal_moves(b: &Board) -> Vec<Move> {
        let mut moves = vec![];

        b.knight_moves(&mut moves);
        b.king_moves(&mut moves);
        b.pawn_moves(&mut moves);
        moves
    }

    fn perft(b: &mut Board, depth: u8) -> u64 {
        let mut nodes = 0;
        if depth == 0 {
            return 1;
        }

        let moves = generate_legal_moves(b);

        for m in moves {
            b.make_move(&m);
            println!("{}", m);
            println!("{}", b);
            nodes += perft(b, depth - 1);
            b.undo_move(&m);
        }
        nodes
    }

    #[test]
    fn perft0() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 0), 1);
    }
    #[test]
    fn perft1() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 1), 20);
    }
    /*
    #[test]
    fn perft2() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 2), 400);
    }
    #[test]
    fn perft3() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 3), 8902);
    }
    #[test]
    fn perft4() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 4), 197281);
    }
    */
}
