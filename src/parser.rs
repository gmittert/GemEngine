pub mod combinators;
pub mod text;

pub use crate::parser::combinators::*;
pub use crate::parser::text::*;

pub trait Parser<'a> {
    type Item;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)>;

    fn or<O>(&self, other: O) -> Or<Self, O>
    where
        Self: Sized,
        Self: Clone,
        O: Parser<'a, Item = Self::Item>,
    {
        Or::new(self.clone(), other)
    }

    fn and_then<O>(&self, other: O) -> AndThen<Self, O>
    where
        Self: Sized,
        Self: Clone,
        O: Parser<'a>,
    {
        AndThen::new(self.clone(), other)
    }

    fn then<O>(&self, other: O) -> Then<Self, O>
    where
        Self: Sized,
        Self: Clone,
        O: Parser<'a>,
    {
        Then::new(self.clone(), other)
    }

    fn skip<O>(&self, other: O) -> Skip<Self, O>
    where
        Self: Sized,
        Self: Clone,
        O: Parser<'a>,
    {
        Skip::new(self.clone(), other)
    }
}

#[macro_export]
macro_rules! parser {
    ($name:ident, $result:ty, $parse:expr) => {
        #[derive(Clone)]
        pub struct $name {}

        impl $name {
            pub fn new() -> $name {
                $name {}
            }
        }

        impl<'a> Parser<'a> for $name {
            type Item = $result;
            fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
                $parse(text)
            }
        }
    };
}

