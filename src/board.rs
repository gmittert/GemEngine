mod moves;
mod posn;
pub use crate::board::moves::*;
pub use crate::board::posn::*;
use std::fmt;
use std::ops;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BitBoard {
    bits: u64,
}

impl BitBoard {
    pub fn from(p: &Posn) -> BitBoard {
        BitBoard {
            bits: (1 << (p.file as u8)) << (8 * p.rank as u8),
        }
    }

    pub fn make_move(&mut self, m: &Move) {
        let from = BitBoard::from(&m.from);
        let to = BitBoard::from(&m.to);

        self.bits = self.bits & !from.bits | to.bits
    }

    pub fn undo_move(&mut self, m: &Move) {
        let from = BitBoard::from(&m.from);
        let to = BitBoard::from(&m.to);

        self.bits = (self.bits & !to.bits) | from.bits
    }
}

#[derive(Debug, PartialEq)]
pub struct Board {
    black_pieces: [BitBoard; 6],
    white_pieces: [BitBoard; 6],
}

impl Board {
    pub fn make_move(&mut self, m: &Move) {
        match m.turn {
            Turn::Black => self.black_pieces[m.piece as usize].make_move(m),
            Turn::White => self.white_pieces[m.piece as usize].make_move(m),
        }
    }
    pub fn undo_move(&mut self, m: &Move) {
        match m.turn {
            Turn::Black => self.black_pieces[m.piece as usize].undo_move(m),
            Turn::White => self.white_pieces[m.piece as usize].undo_move(m),
        };
        match m.capture {
            Some(p) => match m.turn {
                Turn::Black => self.white_pieces[p as usize] |= m.to,
                Turn::White => self.black_pieces[p as usize] |= m.to,
            },
            None => (),
        };
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars: [char; 64] = ['.'; 64];

        for i in 0..64 {
            if (self.black_pieces[Piece::King as usize].bits & (1 << i)) != 0 {
                chars[i] = 'k'
            } else if (self.black_pieces[Piece::Queen as usize].bits & (1 << i)) != 0 {
                chars[i] = 'q'
            } else if (self.black_pieces[Piece::Knight as usize].bits & (1 << i)) != 0 {
                chars[i] = 'n'
            } else if (self.black_pieces[Piece::Pawn as usize].bits & (1 << i)) != 0 {
                chars[i] = 'p'
            } else if (self.black_pieces[Piece::Bishop as usize].bits & (1 << i)) != 0 {
                chars[i] = 'b'
            } else if (self.black_pieces[Piece::Rook as usize].bits & (1 << i)) != 0 {
                chars[i] = 'r'
            } else if (self.white_pieces[Piece::King as usize].bits & (1 << i)) != 0 {
                chars[i] = 'K'
            } else if (self.white_pieces[Piece::Queen as usize].bits & (1 << i)) != 0 {
                chars[i] = 'Q'
            } else if (self.white_pieces[Piece::Knight as usize].bits & (1 << i)) != 0 {
                chars[i] = 'N'
            } else if (self.white_pieces[Piece::Pawn as usize].bits & (1 << i)) != 0 {
                chars[i] = 'P'
            } else if (self.white_pieces[Piece::Bishop as usize].bits & (1 << i)) != 0 {
                chars[i] = 'B'
            } else if (self.white_pieces[Piece::Rook as usize].bits & (1 << i)) != 0 {
                chars[i] = 'R'
            }
        }
        for rank in 0..8 {
            for file in 0..8 {
                write!(f, "{}", chars[file + (8 * (7 - rank))])?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl ops::BitOr<Posn> for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: Posn) -> Self::Output {
        BitBoard {
            bits: self.bits | BitBoard::from(&rhs).bits,
        }
    }
}

impl ops::BitOr for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: BitBoard) -> Self::Output {
        BitBoard {
            bits: self.bits | rhs.bits,
        }
    }
}

impl ops::BitOrAssign<Posn> for BitBoard {
    fn bitor_assign(&mut self, rhs: Posn) {
        self.bits |= BitBoard::from(&rhs).bits;
    }
}

impl ops::BitOrAssign for BitBoard {
    fn bitor_assign(&mut self, rhs: BitBoard) {
        self.bits |= rhs.bits;
    }
}

impl ops::BitOr for Posn {
    type Output = BitBoard;
    fn bitor(self, rhs: Self) -> Self::Output {
        BitBoard::from(&self) | BitBoard::from(&rhs)
    }
}

impl ops::BitOr<BitBoard> for Posn {
    type Output = BitBoard;
    fn bitor(self, rhs: BitBoard) -> Self::Output {
        BitBoard::from(&self) | rhs
    }
}

pub fn starting_board() -> Board {
    Board {
        black_pieces: [
            a7() | b7() | c7() | d7() | e7() | f7() | g7() | h7(),
            a8() | h8(),
            b8() | g8(),
            c8() | f8(),
            BitBoard::from(&d8()),
            BitBoard::from(&e8()),
        ],

        white_pieces: [
            a2() | b2() | c2() | d2() | e2() | f2() | g2() | h2(),
            a1() | h1(),
            b1() | g1(),
            c1() | f1(),
            BitBoard::from(&d1()),
            BitBoard::from(&e1()),
        ],
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;
    #[test]
    fn formatted_start() {
        let exp =
            "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR\n";
        assert_eq!(format!("{}", crate::board::starting_board()), exp);
    }

    #[test]
    fn make_move() {
        let mut board = starting_board();
        let board2 = starting_board();
        let m = Move {
            from: e2(),
            to: e4(),
            turn: Turn::White,
            piece: Piece::Pawn,
            capture: None,
            is_check: false,
            is_mate: false,
        };
        board.make_move(&m);
        board.undo_move(&m);
        assert_eq!(board, board2);
    }
}
