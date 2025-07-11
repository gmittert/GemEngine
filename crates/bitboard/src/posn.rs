use std::fmt;

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

impl Rank {
    pub const fn from(s: char) -> Option<Rank> {
        match s {
            '1' => Some(Rank::One),
            '2' => Some(Rank::Two),
            '3' => Some(Rank::Three),
            '4' => Some(Rank::Four),
            '5' => Some(Rank::Five),
            '6' => Some(Rank::Six),
            '7' => Some(Rank::Seven),
            '8' => Some(Rank::Eight),
            _ => None,
        }
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", (*self as u8) + 1)
    }
}

#[repr(u8)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum File {
    H,
    G,
    F,
    E,
    D,
    C,
    B,
    A,
}

impl File {
    pub const fn from(c: char) -> Option<File> {
        let lower = c;
        match lower {
            'a' | 'A' => Some(File::A),
            'b' | 'B' => Some(File::B),
            'c' | 'C' => Some(File::C),
            'd' | 'D' => Some(File::D),
            'e' | 'E' => Some(File::E),
            'f' | 'F' => Some(File::F),
            'g' | 'G' => Some(File::G),
            'h' | 'H' => Some(File::H),
            _ => None,
        }
    }
}
#[cfg(test)]
mod tests {
    use crate::posn::*;

    #[test]
    pub fn parse_rank() {
        assert_eq!(Some(Rank::One), Rank::from('1'));
        assert_eq!(Some(Rank::Two), Rank::from('2'));
        assert_eq!(Some(Rank::Three), Rank::from('3'));
        assert_eq!(Some(Rank::Four), Rank::from('4'));
        assert_eq!(Some(Rank::Five), Rank::from('5'));
        assert_eq!(Some(Rank::Six), Rank::from('6'));
        assert_eq!(Some(Rank::Seven), Rank::from('7'));
        assert_eq!(Some(Rank::Eight), Rank::from('8'));
        assert_eq!(None, Rank::from('9'));
    }

    #[test]
    pub fn parse_file() {
        assert_eq!(Some(File::A), File::from('a'));
        assert_eq!(Some(File::A), File::from('A'));
        assert_eq!(None, File::from('9'));
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                File::A => "a",
                File::B => "b",
                File::C => "c",
                File::D => "d",
                File::E => "e",
                File::F => "f",
                File::G => "g",
                File::H => "h",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Posn {
    id: u8,
}

impl Posn {
    pub const fn from(rank: Rank, file: File) -> Posn {
        Posn {
            id: (8 * (rank as u8)) + (file as u8),
        }
    }

    pub const fn from_idx(i: u8) -> Posn {
        debug_assert!(i < 64);
        Posn { id: i }
    }

    #[inline]
    pub const fn idx(&self) -> usize {
        self.id as usize
    }

    #[inline]
    pub const fn no(&self) -> Option<Posn> {
        if self.id >= 56 {
            None
        } else {
            Some(Posn { id: self.id + 8 })
        }
    }

    #[inline]
    pub const fn so(&self) -> Option<Posn> {
        if self.id < 8 {
            None
        } else {
            Some(Posn { id: self.id - 8 })
        }
    }

    #[inline]
    pub const fn we(&self) -> Option<Posn> {
        if self.id % 8 == 7 {
            None
        } else {
            Some(Posn { id: self.id + 1 })
        }
    }

    #[inline]
    pub const fn ea(&self) -> Option<Posn> {
        if self.id % 8 == 0 {
            None
        } else {
            Some(Posn { id: self.id - 1 })
        }
    }

    #[inline]
    pub const fn rank(&self) -> Rank {
        let first_bit = self.idx();
        match (first_bit >> 3) & 0x7 {
            0 => Rank::One,
            1 => Rank::Two,
            2 => Rank::Three,
            3 => Rank::Four,
            4 => Rank::Five,
            5 => Rank::Six,
            6 => Rank::Seven,
            _ => Rank::Eight,
        }
    }

    #[inline]
    pub const fn file(&self) -> File {
        let first_bit = self.idx();
        match first_bit & 0x7 {
            0 => File::H,
            1 => File::G,
            2 => File::F,
            3 => File::E,
            4 => File::D,
            5 => File::C,
            6 => File::B,
            _ => File::A,
        }
    }
}

impl fmt::Display for Posn {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

macro_rules! make_posns {
    ($file:ident) => {
        paste::paste! {
        #[allow(dead_code)]
        pub const fn [<$file:lower 1>]() -> Posn {
            Posn::from(
                Rank::One,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 2>]() -> Posn {
            Posn::from(
                Rank::Two,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 3>]() -> Posn {
            Posn::from(
                Rank::Three,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 4>]() -> Posn{
            Posn::from(
                Rank::Four,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 5>]() -> Posn {
            Posn::from(
                Rank::Five,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 6>]() -> Posn {
            Posn::from(
                Rank::Six,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 7>]() -> Posn {
            Posn::from(
                Rank::Seven,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub const fn [<$file:lower 8>]() -> Posn{
            Posn::from(
                Rank::Eight,
                File::$file
            )
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

pub const ALL_POSNS: [Posn; 64] = [
    a1(),
    a2(),
    a3(),
    a4(),
    a5(),
    a6(),
    a7(),
    a8(),
    b1(),
    b2(),
    b3(),
    b4(),
    b5(),
    b6(),
    b7(),
    b8(),
    c1(),
    c2(),
    c3(),
    c4(),
    c5(),
    c6(),
    c7(),
    c8(),
    d1(),
    d2(),
    d3(),
    d4(),
    d5(),
    d6(),
    d7(),
    d8(),
    e1(),
    e2(),
    e3(),
    e4(),
    e5(),
    e6(),
    e7(),
    e8(),
    f1(),
    f2(),
    f3(),
    f4(),
    f5(),
    f6(),
    f7(),
    f8(),
    g1(),
    g2(),
    g3(),
    g4(),
    g5(),
    g6(),
    g7(),
    g8(),
    h1(),
    h2(),
    h3(),
    h4(),
    h5(),
    h6(),
    h7(),
    h8(),
];
