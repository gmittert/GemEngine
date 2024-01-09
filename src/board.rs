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
        BitBoard { bits: 1 << p.pos }
    }

    pub fn make_move(&mut self, m: &Move) {
        let from = BitBoard::from(&m.from);
        let to = BitBoard::from(&m.to);

        self.bits = self.bits & !from.bits | to.bits;
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

    to_play: Turn,
    turn_count: u16,
}

impl Board {
    pub fn make_move(&mut self, m: &Move) {
        match m.turn {
            Turn::Black => self.black_pieces[m.piece as usize].make_move(m),
            Turn::White => self.white_pieces[m.piece as usize].make_move(m),
        }
        self.to_play = !self.to_play;
        self.turn_count += 1;
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
        self.to_play = !self.to_play;
        self.turn_count -= 1;
    }

    pub fn current_board(&self) -> BitBoard {
        let mut b = BitBoard { bits: 0 };
        for i in 0..6 {
            b |= self.black_pieces[i];
            b |= self.white_pieces[i];
        }
        b
    }

    pub fn white_pieces(&self) -> BitBoard {
        let mut b = BitBoard { bits: 0 };
        for i in 0..6 {
            b |= self.white_pieces[i];
        }
        b
    }

    pub fn black_pieces(&self) -> BitBoard {
        let mut b = BitBoard { bits: 0 };
        for i in 0..6 {
            b |= self.black_pieces[i];
        }
        b
    }

    pub fn knight_moves(&self, out: &mut Vec<Move>) {
        const NOT_A_FILE: u64 = 0xFEFE_FEFE_FEFE_FEFE;
        const NOT_A_B_FILE: u64 = 0xFCFC_FCFC_FCFC_FCFC;
        const NOT_H_FILE: u64 = 0x7F7F_7F7F_7F7F_7F7F;
        const NOT_G_H_FILE: u64 = 0x3F3F_3F3F_3F3F_3F3F;
        fn no_no_ea(b: u64) -> u64 {
            (b << 17) & NOT_A_FILE
        }
        fn no_ea_ea(b: u64) -> u64 {
            (b << 10) & NOT_A_B_FILE
        }
        fn so_ea_ea(b: u64) -> u64 {
            (b >> 6) & NOT_A_B_FILE
        }
        fn so_so_ea(b: u64) -> u64 {
            (b >> 15) & NOT_A_FILE
        }
        fn no_no_we(b: u64) -> u64 {
            (b << 15) & NOT_H_FILE
        }
        fn no_we_we(b: u64) -> u64 {
            (b << 6) & NOT_G_H_FILE
        }
        fn so_we_we(b: u64) -> u64 {
            (b >> 10) & NOT_G_H_FILE
        }
        fn so_so_we(b: u64) -> u64 {
            (b >> 17) & NOT_H_FILE
        }
        let knights = match self.to_play {
            Turn::White => self.white_pieces[Piece::Knight as usize].bits,
            Turn::Black => self.black_pieces[Piece::Knight as usize].bits,
        };

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        for i in 0..64 as u8 {
            if knights & (1 << i) != 0 {
                for pos in [
                    no_no_ea(1 << i),
                    no_ea_ea(1 << i),
                    so_ea_ea(1 << i),
                    so_so_ea(1 << i),
                    so_so_we(1 << i),
                    so_we_we(1 << i),
                    no_we_we(1 << i),
                    no_no_we(1 << i),
                ] {
                    if pos != 0 && (pos & allied_pieces == 0) {
                        out.push(Move {
                            from: Posn { pos: i },
                            to: Posn {
                                pos: pos.trailing_zeros() as u8,
                            },
                            turn: self.to_play,
                            piece: Piece::Knight,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                        });
                    }
                }
            }
        }
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

        to_play: Turn::White,
        turn_count: 1,
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
