use std::ops;

use crate::board::posn::*;

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub struct BitBoard(pub u64);

impl BitBoard {
    pub const fn empty() -> BitBoard {
        BitBoard { 0: 0 }
    }

    pub const fn from(p: Posn) -> BitBoard {
        BitBoard { 0: p.pos }
    }

    pub const fn contains(&self, p: Posn) -> bool {
        self.0 & p.pos != 0
    }

    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    pub const fn len(&self) -> usize {
        let mut acc = self.0 as u64;
        let mut count = 0;
        while acc != 0 {
            count += 1;
            acc &= acc - 1
        }
        count
    }
}

impl std::fmt::Display for BitBoard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for rank in 0..8 {
            for file in 0..8 {
                if (self.0 & (1 << ((8 * rank) + file))) != 0 {
                    write!(f, "o")?;
                } else {
                    write!(f, ".")?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Iterator for BitBoard {
    type Item = Posn;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            return None;
        }
        if self.0 == 0x8000_0000_0000_0000 {
            let res = self.0;
            self.0 = 0;
            return Some(Posn { pos: res });
        }
        let lsb = (self.0 as i64) & -(self.0 as i64);
        self.0 &= self.0 - 1;
        Some(Posn { pos: lsb as u64 })
    }
}

impl ops::BitOr<Posn> for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: Posn) -> Self::Output {
        BitBoard {
            0: self.0 | BitBoard::from(rhs).0,
        }
    }
}

impl ops::Not for BitBoard {
    type Output = Self;
    fn not(self) -> Self::Output {
        BitBoard { 0: !self.0 }
    }
}

impl ops::BitOr for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: BitBoard) -> Self::Output {
        BitBoard { 0: self.0 | rhs.0 }
    }
}

impl ops::BitAnd for BitBoard {
    type Output = Self;
    fn bitand(self, rhs: BitBoard) -> Self::Output {
        BitBoard { 0: self.0 & rhs.0 }
    }
}

impl ops::BitOrAssign<Posn> for BitBoard {
    fn bitor_assign(&mut self, rhs: Posn) {
        self.0 |= BitBoard::from(rhs).0;
    }
}

impl ops::BitOrAssign for BitBoard {
    fn bitor_assign(&mut self, rhs: BitBoard) {
        self.0 |= rhs.0;
    }
}

impl ops::BitAndAssign for BitBoard {
    fn bitand_assign(&mut self, rhs: BitBoard) {
        self.0 &= rhs.0;
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
        assert_eq!(Some(b1()), b_it.next());
        assert_eq!(Some(a1()), b_it.next());
        assert_eq!(Some(b2()), b_it.next());
        assert_eq!(Some(a2()), b_it.next());
        assert_eq!(None, b_it.next());
    }
}
