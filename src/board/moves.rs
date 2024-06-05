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

impl Piece {
    pub fn from(c: char) -> Option<Piece> {
        match c {
            'p' | 'P' => Some(Piece::Pawn),
            'r' | 'R' => Some(Piece::Rook),
            'n' | 'N' => Some(Piece::Knight),
            'b' | 'B' => Some(Piece::Bishop),
            'q' | 'Q' => Some(Piece::Queen),
            'k' | 'K' => Some(Piece::King),
            _ => None,
        }
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Color {
    Black,
    White,
}

impl Not for Color {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct AlgebraicMove {
    pub from: Posn,
    pub to: Posn,
    pub promotion: Option<Piece>,
}

impl AlgebraicMove {
    pub fn from(s: &str) -> Option<AlgebraicMove> {
        let mut chars = s.chars();
        let from_file = File::from(chars.next()?)?;
        let from_rank = Rank::from(chars.next()?)?;
        let to_file = File::from(chars.next()?)?;
        let to_rank = Rank::from(chars.next()?)?;
        let promo = chars.next().and_then(|c| Piece::from(c));
        Some(AlgebraicMove {
            from: Posn::from(from_rank, from_file),
            to: Posn::from(to_rank, to_file),
            promotion: promo,
        })
    }
}
#[cfg(test)]
mod tests {
    use crate::board::*;

    #[test]
    pub fn parse_alg_move() {
        assert_eq!(
            AlgebraicMove::from("e2e4"),
            Some(AlgebraicMove {
                from: e2(),
                to: e4(),
                promotion: None
            })
        );
        assert_eq!(
            AlgebraicMove::from("b7b8q"),
            Some(AlgebraicMove {
                from: b7(),
                to: b8(),
                promotion: Some(Piece::Queen)
           })
        );
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct Move {
    pub from: Posn,
    pub to: Posn,
    pub turn: Color,
    pub piece: Piece,
    pub capture: Option<Piece>,
    pub promotion: Option<Piece>,
    pub is_check: bool,
    pub is_mate: bool,
    pub is_en_passant: bool,
    pub is_castle_queen: bool,
    pub is_castle_king: bool,
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.turn == Color::Black {
            write!(f, "..")?;
        }
        let is_check = if self.is_check {
            "+"
        } else if self.is_mate {
            "#"
        } else {
            ""
        };
        let promo = if let Some(piece) = self.promotion {
            format!("={piece:?}")
        } else {
            format!("")
        };

        let icon = match (self.piece, self.turn) {
            (Piece::Rook, Color::Black) => "♖",
            (Piece::Rook, Color::White) => "♜",
            (Piece::Knight, Color::Black) => "♘",
            (Piece::Knight, Color::White) => "♞",
            (Piece::Bishop, Color::Black) => "♗",
            (Piece::Bishop, Color::White) => "♝",
            (Piece::Queen, Color::Black) => "♕",
            (Piece::Queen, Color::White) => "♛",
            (Piece::King, Color::Black) => {
                if self.is_castle_king {
                    return write!(f, "O-O{}", is_check);
                } else if self.is_castle_queen {
                    return write!(f, "O-O-O{}", is_check);
                }
                "♔"
            }
            (Piece::King, Color::White) => {
                if self.is_castle_king {
                    return write!(f, "O-O{}", is_check);
                } else if self.is_castle_queen {
                    return write!(f, "O-O-O{}", is_check);
                }
                "♚"
            }
            (Piece::Pawn, _) => match self.capture {
                Some(_) => {
                    return write!(
                        f,
                        "{}x{}{}{}{}",
                        self.from.file(),
                        self.to.file(),
                        self.to.rank(),
                        promo,
                        is_check
                    )
                }
                None => {
                    return write!(
                        f,
                        "{}{}{}{}",
                        self.to.file(),
                        self.to.rank(),
                        promo,
                        is_check
                    )
                }
            },
        };

        let capture = match self.capture {
            Some(_) => "x",
            None => "",
        };
        write!(
            f,
            "{}{}{}{}{}{}",
            icon,
            capture,
            self.to.file(),
            self.to.rank(),
            promo,
            is_check
        )
    }
}
