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
    pub pos: u8,
}

impl Posn {
    pub fn from(rank: Rank, file: File) -> Posn {
        Posn {
            pos: (8 * rank as u8) + file as u8,
        }
    }

    pub fn rank(&self) -> Rank {
        match (self.pos >> 3) & 0x7 {
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
        match self.pos & 0x7 {
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

    pub fn no(&self) -> Option<Posn> {
        if self.rank() == Rank::Eight {
            None
        } else {
            Some(Posn { pos: self.pos + 8 })
        }
    }
    pub fn so(&self) -> Option<Posn> {
        if self.rank() == Rank::One{
            None
        } else {
            Some(Posn { pos: self.pos - 8 })
        }
    }
    pub fn ea(&self) -> Option<Posn> {
        if self.file() == File::H{
            None
        } else {
            Some(Posn { pos: self.pos - 1 })
        }
    }
    pub fn we(&self) -> Option<Posn> {
        if self.file() == File::A{
            None
        } else {
            Some(Posn { pos: self.pos + 1 })
        }
    }
    pub fn nw(&self) -> Option<Posn> {
        self.no().and_then(|p|p.we())
    }
    pub fn ne(&self) -> Option<Posn> {
        self.no().and_then(|p|p.ea())
    }
    pub fn sw(&self) -> Option<Posn> {
        self.so().and_then(|p|p.we())
    }
    pub fn se(&self) -> Option<Posn> {
        self.so().and_then(|p|p.ea())
    }
    pub fn nnw(&self) -> Option<Posn> {
        self.no().and_then(|p|p.no()).and_then(|p|p.we())
    }
    pub fn nne(&self) -> Option<Posn> {
        self.no().and_then(|p|p.no()).and_then(|p|p.ea())
    }
    pub fn nww(&self) -> Option<Posn> {
        self.no().and_then(|p|p.we()).and_then(|p|p.we())
    }
    pub fn nee(&self) -> Option<Posn> {
        self.no().and_then(|p|p.ea()).and_then(|p|p.ea())
    }
    pub fn ssw(&self) -> Option<Posn> {
        self.so().and_then(|p|p.so()).and_then(|p|p.we())
    }
    pub fn sse(&self) -> Option<Posn> {
        self.so().and_then(|p|p.so()).and_then(|p|p.ea())
    }
    pub fn sww(&self) -> Option<Posn> {
        self.so().and_then(|p|p.we()).and_then(|p|p.we())
    }
    pub fn see(&self) -> Option<Posn> {
        self.so().and_then(|p|p.ea()).and_then(|p|p.ea())
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
