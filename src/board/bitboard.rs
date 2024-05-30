use std::ops;

use crate::board::moves::*;
use crate::board::posn::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BitBoard {
    bits: u64,
}

impl BitBoard {
    pub fn empty() -> BitBoard {
        BitBoard { bits: 0 }
    }

    pub fn from(p: Posn) -> BitBoard {
        BitBoard { bits: 1 << p.pos }
    }

    pub fn make_move(&self, m: &Move) -> BitBoard {
        assert_eq!(
            true,
            self.contains(m.from),
            "Tried to make move: {m} on board {:x}",
            self.bits
        );
        let from = BitBoard::from(m.from);
        let to = BitBoard::from(m.to);

        (*self & !from) | to
    }

    pub fn undo_move(&self, m: &Move) -> BitBoard {
        let from = BitBoard::from(m.from);
        let to = BitBoard::from(m.to);

        (*self & !to) | from
    }

    pub fn contains(&self, p: Posn) -> bool {
        self.bits & (1 << p.pos) != 0
    }
}

impl Iterator for BitBoard {
    type Item = Posn;

    fn next(&mut self) -> Option<Self::Item> {
        if self.bits == 0 {
            return None;
        }
        let first_bit = self.bits.ilog2();
        self.bits &= !(1 << first_bit);
        Some(Posn {
            pos: first_bit as u8,
        })
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

impl ops::Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self::Output {
        BitBoard { bits: !self.bits }
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

impl ops::BitAnd for BitBoard {
    type Output = Self;
    fn bitand(self, rhs: BitBoard) -> Self::Output {
        BitBoard {
            bits: self.bits & rhs.bits,
        }
    }
}

impl ops::BitOrAssign<Posn> for BitBoard {
    fn bitor_assign(&mut self, rhs: Posn) {
        self.bits |= BitBoard::from(rhs).bits;
    }
}

impl ops::BitOrAssign for BitBoard {
    fn bitor_assign(&mut self, rhs: BitBoard) {
        self.bits |= rhs.bits;
    }
}

impl ops::BitAndAssign for BitBoard {
    fn bitand_assign(&mut self, rhs: BitBoard) {
        self.bits &= rhs.bits;
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

#[cfg(test)]
mod tests {
    use crate::board::*;
    #[test]
    pub fn bitboard_iter() {
        let b = a1() | a2() | b1() | b2();
        let mut b_it = b.into_iter();
        assert_eq!(Some(a2()), b_it.next());
        assert_eq!(Some(b2()), b_it.next());
        assert_eq!(Some(a1()), b_it.next());
        assert_eq!(Some(b1()), b_it.next());
        assert_eq!(None, b_it.next());
    }
}
