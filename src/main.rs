use std::fmt;
use std::ops;

#[repr(u8)]
#[derive(Debug, PartialEq)]
enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

#[repr(u8)]
#[derive(Debug, PartialEq)]
enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

#[derive(Debug, PartialEq)]
struct Posn {
    rank: Rank,
    file: File,
}

#[derive(Debug, PartialEq)]
struct BitBoard {
    bits: u64,
}

impl BitBoard {
    fn from(p: Posn) -> BitBoard {
        BitBoard {
            bits: (1 << (p.file as u8)) << (8 * p.rank as u8),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Board {
    black_king: BitBoard,
    black_queen: BitBoard,
    black_knight: BitBoard,
    black_pawn: BitBoard,
    black_bishop: BitBoard,
    black_rook: BitBoard,

    white_king: BitBoard,
    white_queen: BitBoard,
    white_knight: BitBoard,
    white_pawn: BitBoard,
    white_bishop: BitBoard,
    white_rook: BitBoard,
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars: [char; 64] = ['.'; 64];

        for i in 0..64 {
            if (self.black_king.bits & (1 << i)) != 0 {
                chars[i] = 'k'
            } else if (self.black_queen.bits & (1 << i)) != 0 {
                chars[i] = 'q'
            } else if (self.black_knight.bits & (1 << i)) != 0 {
                chars[i] = 'n'
            } else if (self.black_pawn.bits & (1 << i)) != 0 {
                chars[i] = 'p'
            } else if (self.black_bishop.bits & (1 << i)) != 0 {
                chars[i] = 'b'
            } else if (self.black_rook.bits & (1 << i)) != 0 {
                chars[i] = 'r'
            } else if (self.white_king.bits & (1 << i)) != 0 {
                chars[i] = 'K'
            } else if (self.white_queen.bits & (1 << i)) != 0 {
                chars[i] = 'Q'
            } else if (self.white_knight.bits & (1 << i)) != 0 {
                chars[i] = 'N'
            } else if (self.white_pawn.bits & (1 << i)) != 0 {
                chars[i] = 'P'
            } else if (self.white_bishop.bits & (1 << i)) != 0 {
                chars[i] = 'B'
            } else if (self.white_rook.bits & (1 << i)) != 0 {
                chars[i] = 'R'
            }
        }
        for rank in 0..8 {
            for file in 0..8 {
                write!(f, "{}", chars[file + (8*(7-rank))])?;
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
            bits: self.bits | BitBoard::from(rhs).bits,
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

impl ops::BitOr for Posn {
    type Output = BitBoard;
    fn bitor(self, rhs: Self) -> Self::Output {
        BitBoard::from(self) | BitBoard::from(rhs)
    }
}

impl ops::BitOr<BitBoard> for Posn {
    type Output = BitBoard;
    fn bitor(self, rhs: BitBoard) -> Self::Output {
        BitBoard::from(self) | rhs
    }
}

macro_rules! make_posns {
    ($file:ident) => {
        paste::paste! {
        #[allow(dead_code)]
        fn [<$file:lower 1>]() -> Posn {
            Posn {
                rank: Rank::One,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 2>]() -> Posn {
            Posn {
                rank: Rank::Two,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 3>]() -> Posn {
            Posn {
                rank: Rank::Three,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 4>]() -> Posn {
            Posn {
                rank: Rank::Four,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 5>]() -> Posn {
            Posn {
                rank: Rank::Five,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 6>]() -> Posn {
            Posn {
                rank: Rank::Six,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 7>]() -> Posn {
            Posn {
                rank: Rank::Seven,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        fn [<$file:lower 8>]() -> Posn {
            Posn {
                rank: Rank::Eight,
                file: File::$file
            }
        }
        }
    };
}

make_posns!(A);
make_posns!(B);
make_posns!(C);
make_posns!(D);
make_posns!(E);
make_posns!(F);
make_posns!(G);
make_posns!(H);

fn starting_board() -> Board {
    Board {
        black_king: BitBoard::from(e8()),
        black_queen: BitBoard::from(d8()),
        black_knight: b8() | g8(),
        black_pawn: a7() | b7() | c7() | d7() | e7() | f7() | g7() | h7(),
        black_bishop: c8() | f8(),
        black_rook: a8() | h8(),

        white_king: BitBoard::from(e1()),
        white_queen: BitBoard::from(d1()),
        white_knight: b1() | g1(),
        white_pawn: a2() | b2() | c2() | d2() | e2() | f2() | g2() | h2(),
        white_bishop: c1() | f1(),
        white_rook: a1() | h1(),
    }
}

fn main() {
    println!("Starting Chess Board:");
    println!("{}", starting_board());
}

#[cfg(test)]
mod tests {
    #[test]
    fn formatted_start() {
        let exp =
            "rnbqkbnr\npppppppp\n........\n........\n........\n........\nPPPPPPPP\nRNBQKBNR\n";
        assert_eq!(format!("{}", crate::starting_board()), exp);
    }
}
