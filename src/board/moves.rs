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
pub struct Move {
    pub from: Posn,
    pub to: Posn,
    pub turn: Color,
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
            (Piece::Rook, Color::Black) => "♖",
            (Piece::Rook, Color::White) => "♜",
            (Piece::Knight, Color::Black) => "♘",
            (Piece::Knight, Color::White) => "♞",
            (Piece::Bishop, Color::Black) => "♗",
            (Piece::Bishop, Color::White) => "♝",
            (Piece::Queen, Color::Black) => "♕",
            (Piece::Queen, Color::White) => "♛",
            (Piece::King, Color::Black) => "♔",
            (Piece::King, Color::White) => "♚",
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
