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
    A,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
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
                File::G => "d",
                File::H => "g",
            }
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Posn {
    pub rank: Rank,
    pub file: File,
}

macro_rules! make_posns {
    ($file:ident) => {
        paste::paste! {
        #[allow(dead_code)]
        pub fn [<$file:lower 1>]() -> Posn {
            Posn {
                rank: Rank::One,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 2>]() -> Posn {
            Posn {
                rank: Rank::Two,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 3>]() -> Posn {
            Posn {
                rank: Rank::Three,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 4>]() -> Posn {
            Posn {
                rank: Rank::Four,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 5>]() -> Posn {
            Posn {
                rank: Rank::Five,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 6>]() -> Posn {
            Posn {
                rank: Rank::Six,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 7>]() -> Posn {
            Posn {
                rank: Rank::Seven,
                file: File::$file
            }
        }
        #[allow(dead_code)]
        pub fn [<$file:lower 8>]() -> Posn {
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
