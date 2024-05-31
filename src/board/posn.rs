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
pub struct Posn {
    pub pos: u64,
}

impl Posn {
    pub fn from(rank: Rank, file: File) -> Posn {
        Posn {
            pos: 1 << ((8 * (rank as u8)) + (file as u8)),
        }
    }

    pub fn rank(&self) -> Rank {
        let first_bit = self.pos.ilog2();
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

    pub fn file(&self) -> File {
        let first_bit = self.pos.ilog2();
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

    fn check(&self) -> Option<Posn> {
        if self.pos == 0 {
            return None;
        }
        Some(*self)
    }
    pub fn no_unchecked(&self) -> Posn {
        Posn { pos: self.pos << 8 }
    }
    pub fn so_unchecked(&self) -> Posn {
        Posn { pos: self.pos >> 8 }
    }
    pub fn ea_unchecked(&self) -> Posn {
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        Posn {
            pos: (self.pos >> 1) & !A_FILE,
        }
    }
    pub fn we_unchecked(&self) -> Posn {
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        Posn {
            pos: (self.pos << 1) & !H_FILE,
        }
    }

    pub fn no(&self) -> Option<Posn> {
        self.no_unchecked().check()
    }
    pub fn so(&self) -> Option<Posn> {
        self.so_unchecked().check()
    }
    pub fn ea(&self) -> Option<Posn> {
        self.ea_unchecked().check()
    }
    pub fn we(&self) -> Option<Posn> {
        self.we_unchecked().check()
    }
    pub fn nw(&self) -> Option<Posn> {
        self.no_unchecked().we_unchecked().check()
    }
    pub fn ne(&self) -> Option<Posn> {
        self.no_unchecked().ea_unchecked().check()
    }
    pub fn sw(&self) -> Option<Posn> {
        self.so_unchecked().we_unchecked().check()
    }
    pub fn se(&self) -> Option<Posn> {
        self.so_unchecked().ea_unchecked().check()
    }
    pub fn nnw(&self) -> Option<Posn> {
        self.no_unchecked().no_unchecked().we_unchecked().check()
    }
    pub fn nne(&self) -> Option<Posn> {
        self.no_unchecked().no_unchecked().ea_unchecked().check()
    }
    pub fn nww(&self) -> Option<Posn> {
        self.no_unchecked().we_unchecked().we_unchecked().check()
    }
    pub fn nee(&self) -> Option<Posn> {
        self.no_unchecked().ea_unchecked().ea_unchecked().check()
    }
    pub fn ssw(&self) -> Option<Posn> {
        self.so_unchecked().so_unchecked().we_unchecked().check()
    }
    pub fn sse(&self) -> Option<Posn> {
        self.so_unchecked().so_unchecked().ea_unchecked().check()
    }
    pub fn sww(&self) -> Option<Posn> {
        self.so_unchecked().we_unchecked().we_unchecked().check()
    }
    pub fn see(&self) -> Option<Posn> {
        self.so_unchecked().ea_unchecked().ea_unchecked().check()
    }
}

macro_rules! make_posns {
    ($file:ident) => {
        paste::paste! {
        #[allow(dead_code)]
        pub fn [<$file:lower 1>]() -> Posn {
            Posn::from(
                Rank::One,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 2>]() -> Posn {
            Posn::from(
                Rank::Two,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 3>]() -> Posn {
            Posn::from(
                Rank::Three,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 4>]() -> Posn{
            Posn::from(
                Rank::Four,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 5>]() -> Posn {
            Posn::from(
                Rank::Five,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 6>]() -> Posn {
            Posn::from(
                Rank::Six,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 7>]() -> Posn {
            Posn::from(
                Rank::Seven,
                File::$file
            )
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 8>]() -> Posn{
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
