use std::{fmt, num::NonZero};

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

#[derive(Debug, Copy, Clone, PartialEq)]
#[repr(transparent)]
pub struct Posn {
    pub pos: NonZero<u64>,
}

impl Posn {
    pub const fn from(rank: Rank, file: File) -> Posn {
        Posn {
            pos: unsafe { NonZero::new_unchecked(1 << ((8 * (rank as u8)) + (file as u8))) },
        }
    }

    pub const fn from_idx(i: usize) -> Option<Posn> {
        if let Some(pos) = NonZero::new(1 << i) {
            unsafe { std::mem::transmute(Posn { pos }) }
        } else {
            None
        }
    }

    #[inline]
    pub const fn idx(&self) -> u32 {
        // On the Author's machine, even though we know a priori that posns are non zero,
        //
        // ```
        // unsafe {NonZero::new_unchecked(self.pos).ilog2()}
        // ```
        //
        // compiles down to the same thing as just calling ilog2, so we just use the ilog2 call
        // below.
        //
        // However, the #[inline] is quite necessary to not pay a 40% perf penalty for abstracting
        // this into a function.
        self.pos.ilog2()
    }

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

    pub const fn no(&self) -> Option<Posn> {
        let no = self.pos.get() << 8;
        unsafe { std::mem::transmute(no) }
    }
    pub const fn so(&self) -> Option<Posn> {
        let so = self.pos.get() >> 8;
        unsafe { std::mem::transmute(so) }
    }
    pub const fn ea(&self) -> Option<Posn> {
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let ea = (self.pos.get() >> 1) & !A_FILE;
        unsafe { std::mem::transmute(ea) }
    }
    pub const fn we(&self) -> Option<Posn> {
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let we = (self.pos.get() << 1) & !H_FILE;
        unsafe { std::mem::transmute(we) }
    }
    pub const fn nw(&self) -> Option<Posn> {
        let no = self.pos.get() << 8;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let nw = (no << 1) & !H_FILE;
        unsafe { std::mem::transmute(nw) }
    }
    pub const fn ne(&self) -> Option<Posn> {
        let no = self.pos.get() << 8;
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let ne = (no >> 1) & !A_FILE;
        unsafe { std::mem::transmute(ne) }
    }
    pub const fn sw(&self) -> Option<Posn> {
        let so = self.pos.get() >> 8;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let sw = (so << 1) & !H_FILE;
        unsafe { std::mem::transmute(sw) }
    }
    pub const fn se(&self) -> Option<Posn> {
        let so = self.pos.get() >> 8;
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let se = (so >> 1) & !A_FILE;
        unsafe { std::mem::transmute(se) }
    }
    pub const fn nnw(&self) -> Option<Posn> {
        let nno = self.pos.get() << 16;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let nnw = (nno << 1) & !H_FILE;
        unsafe { std::mem::transmute(nnw) }
    }
    pub const fn nne(&self) -> Option<Posn> {
        let nno = self.pos.get() << 16;
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let nne = (nno >> 1) & !A_FILE;
        unsafe { std::mem::transmute(nne) }
    }
    pub const fn nww(&self) -> Option<Posn> {
        let no = self.pos.get() << 8;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let nw = (no << 1) & !H_FILE;
        let nww = (nw << 1) & !H_FILE;
        unsafe { std::mem::transmute(nww) }
    }
    pub const fn nee(&self) -> Option<Posn> {
        let no = self.pos.get() << 8;
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let ne = (no >> 1) & !A_FILE;
        let nee = (ne >> 1) & !A_FILE;
        unsafe { std::mem::transmute(nee) }
    }
    pub const fn ssw(&self) -> Option<Posn> {
        let sso = self.pos.get() >> 16;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let ssw = (sso << 1) & !H_FILE;
        unsafe { std::mem::transmute(ssw) }
    }
    pub const fn sse(&self) -> Option<Posn> {
        let sso = self.pos.get() >> 16;
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let sse = (sso >> 1) & !A_FILE;
        unsafe { std::mem::transmute(sse) }
    }
    pub const fn sww(&self) -> Option<Posn> {
        let so = self.pos.get() >> 8;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        let sw = (so << 1) & !H_FILE;
        let sww = (sw << 1) & !H_FILE;
        unsafe { std::mem::transmute(sww) }
    }
    pub const fn see(&self) -> Option<Posn> {
        let so = self.pos.get() >> 8;
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        let se = (so >> 1) & !A_FILE;
        let see = (se >> 1) & !A_FILE;
        unsafe { std::mem::transmute(see) }
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
