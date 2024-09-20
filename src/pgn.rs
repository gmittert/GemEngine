use crate::board::AlgebraicMove;
use std::collections::HashMap;

struct Pgn {
    tags: HashMap<String, String>,
    moves: Vec<AlgebraicMove>,
}

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
        O: Parser<'a, Item = Self::Item>,
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

macro_rules! parser {
    ($name:ident, $result:ty, $parse:expr) => {
        #[derive(Clone)]
        struct $name {}

        impl $name {
            fn new() -> $name {
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

fn parse_pgn(_: &str) -> Option<(Pgn, &str)> {
    None
}

#[derive(Clone)]
pub struct Or<L, R> {
    l: L,
    r: R,
}

impl<L, R> Or<L, R> {
    fn new(l: L, r: R) -> Or<L, R> {
        Or { l, r }
    }
}

impl<'a, L, R> Parser<'a> for Or<L, R>
where
    L: Parser<'a>,
    R: Parser<'a, Item = L::Item>,
{
    type Item = L::Item;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        self.l.parse(text).or(self.r.parse(text))
    }
}

#[derive(Clone)]
pub struct AndThen<L, R> {
    l: L,
    r: R,
}

impl<L, R> AndThen<L, R> {
    fn new(l: L, r: R) -> AndThen<L, R> {
        AndThen { l, r }
    }
}

impl<'a, L, R> Parser<'a> for AndThen<L, R>
where
    L: Parser<'a>,
    R: Parser<'a>,
{
    type Item = (L::Item, R::Item);
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let (v1, text) = self.l.parse(text)?;
        let (v2, text) = self.r.parse(text)?;
        Some(((v1, v2), text))
    }
}

#[derive(Clone)]
pub struct Then<L, R> {
    l: L,
    r: R,
}

impl<L, R> Then<L, R> {
    fn new(l: L, r: R) -> Then<L, R> {
        Then { l, r }
    }
}

impl<'a, L, R> Parser<'a> for Then<L, R>
where
    L: Parser<'a>,
    R: Parser<'a>,
{
    type Item = R::Item;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let (_, text) = self.l.parse(text)?;
        self.r.parse(text)
    }
}

#[derive(Clone)]
pub struct TakeWhile {
    pred: fn(char)-> bool,
}

impl TakeWhile {
    fn new(pred: fn(char) -> bool) -> TakeWhile {
        TakeWhile { pred }
    }
}

impl<'a> Parser<'a> for TakeWhile
{
    type Item = &'a str;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let idx = text.find(|c|{!(self.pred)(c)})?;
        Some(text.split_at(idx))
    }
}

#[derive(Clone)]
pub struct Skip<L, R> {
    l: L,
    r: R,
}

impl<L, R> Skip<L, R> {
    fn new(l: L, r: R) -> Skip<L, R> {
        Skip { l, r }
    }
}

impl<'a, L, R> Parser<'a> for Skip<L, R>
where
    L: Parser<'a>,
    R: Parser<'a>,
{
    type Item = L::Item;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let (val, text) = self.l.parse(text)?;
        let (_, text) = self.r.parse(text)?;
        Some((val, text))
    }
}

parser!(Whitespace, (), |text| {
    Literal::new('\n')
        .or(Literal::new(' '))
        .or(Literal::new('\t'))
        .parse(text)
        .map(|(_, rest)| ((), rest))
});

parser!(Digit, char, |text| {
    Literal::new('1')
        .or(Literal::new('2'))
        .or(Literal::new('3'))
        .or(Literal::new('4'))
        .or(Literal::new('5'))
        .or(Literal::new('6'))
        .or(Literal::new('7'))
        .or(Literal::new('8'))
        .or(Literal::new('9'))
        .parse(text)
});

struct SeparatedList<C, W> {
    contents: C,
    seperator: W,
}
impl<C, W> SeparatedList<C, W> {
    fn new(contents: C, seperator: W) -> SeparatedList<C, W> {
        SeparatedList {
            contents,
            seperator,
        }
    }
}

impl<'a, C, W> Parser<'a> for SeparatedList<C, W>
where
    C: Parser<'a>,
    W: Parser<'a>,
{
    type Item = Vec<C::Item>;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let mut remaining = text;
        let mut results = vec![];
        loop {
            let (val, rest) = self.contents.parse(remaining)?;
            results.push(val);
            remaining = rest;

            let Some((_, rest)) = self.seperator.parse(remaining) else {
                break;
            };
            remaining = rest
        }
        Some((results, remaining))
    }
}

parser!(Word, &'a str, |text: &'a str| { text.split_once(" ") });

parser!(StringLit, &'a str, |text: &'a str| {
    Literal::new('"')
        .then(TakeWhile::new(|c|c != '"'))
        .skip(Literal::new('"'))
        .parse(text)
});

parser!(TagPair, (&'a str, &'a str), |text| {
    Literal::new('[')
        .then(Word::new())
        .and_then(StringLit::new())
        .skip(Literal::new(']'))
        .parse(text)
});

#[derive(Clone)]
struct Literal {
    c: char,
}

impl Literal {
    fn new(c: char) -> Literal {
        Literal { c }
    }
}

impl<'a> Parser<'a> for Literal {
    type Item = char;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        if text.is_empty() {
            return None;
        }
        let (first, rest) = text.split_at_checked(1)?;

        if first != self.c.to_string() {
            return None;
        }
        Some((self.c, rest))
    }
}

#[derive(Clone)]
pub struct MoveText {}

#[derive(Clone)]
pub struct Move {}

impl<'a> Parser<'a> for Move {
    type Item = (u16, AlgebraicMove, &'a str);
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        None
    }
}

#[cfg(test)]
mod tests {
    use crate::pgn::*;
    #[test]
    fn literals() {
        let s = "abc";
        let parse_one = Literal::new('b').parse(s);
        assert_eq!(None, parse_one);

        let parse_one = Literal::new('a').parse(s);
        let Some((c, rest)) = parse_one else {
            assert!(parse_one.is_some());
            return;
        };
        assert_eq!(rest, "bc");
        assert_eq!(c, 'a');

        let parse_one = Literal::new('b').parse(rest);
        let Some((c, rest)) = parse_one else {
            assert!(parse_one.is_some());
            return;
        };
        assert_eq!(rest, "c");
        assert_eq!(c, 'b');

        let parse_one = Literal::new('c').parse(rest);
        let Some((c, rest)) = parse_one else {
            assert!(parse_one.is_some());
            return;
        };
        assert_eq!(rest, "");
        assert_eq!(c, 'c');

        let parse_one = Literal::new('d').parse(rest);
        assert_eq!(None, parse_one);
    }
    #[test]
    fn tag_one_pair() {
        let s = "[Round \"9\"]";
        let parsed = TagPair::new().parse(s);
        assert!(parsed.is_some());
        let ((tag, val), rest) = parsed.unwrap();
        assert_eq!(tag, "Round");
        assert_eq!(val, "9");
        assert_eq!(rest, "");
    }

    #[test]
    fn or() {
        let a = Literal::new('a');
        let b = Literal::new('b');
        let eitherab = a.or(b);

        let s = "a";
        let parsed = eitherab.parse(s);
        assert!(parsed.is_some());
        let (c, rest) = parsed.unwrap();
        assert_eq!(c, 'a');
        assert_eq!(rest, "");

        let s = "b";
        let parsed = eitherab.parse(s);
        assert!(parsed.is_some());
        let (c, rest) = parsed.unwrap();
        assert_eq!(c, 'b');
        assert_eq!(rest, "");

        let s = "c";
        let parsed = eitherab.parse(s);
        assert!(parsed.is_none());
    }

    #[test]
    fn and_then() {
        let a = Literal::new('a');
        let b = Literal::new('b');
        let andthenab = a.and_then(b);

        let s = "ab";
        let parsed = andthenab.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, ('a', 'b'));
        assert_eq!(rest, "");

        let s = "b";
        let parsed = andthenab.parse(s);
        assert!(parsed.is_none());

        let s = "a";
        let parsed = andthenab.parse(s);
        assert!(parsed.is_none());

        let s = "ba";
        let parsed = andthenab.parse(s);
        assert!(parsed.is_none());
    }

    #[test]
    fn then() {
        let a = Literal::new('a');
        let b = Literal::new('b');
        let thenab = a.then(b);

        let s = "ab";
        let parsed = thenab.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, 'b');
        assert_eq!(rest, "");

        let s = "b";
        let parsed = thenab.parse(s);
        assert!(parsed.is_none());

        let s = "a";
        let parsed = thenab.parse(s);
        assert!(parsed.is_none());

        let s = "ba";
        let parsed = thenab.parse(s);
        assert!(parsed.is_none());
    }

    #[test]
    fn skip() {
        let a = Literal::new('a');
        let b = Literal::new('b');
        let thenab = a.skip(b);

        let s = "ab";
        let parsed = thenab.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, 'a');
        assert_eq!(rest, "");

        let s = "b";
        let parsed = thenab.parse(s);
        assert!(parsed.is_none());

        let s = "a";
        let parsed = thenab.parse(s);
        assert!(parsed.is_none());

        let s = "ba";
        let parsed = thenab.parse(s);
        assert!(parsed.is_none());
    }

    #[test]
    fn tag_pairs() {
        let s = "[Round \"9\"]\n[Date \"1992.11.04\"]";
        let parser = SeparatedList::new(TagPair::new(), Whitespace::new());
        let parsed = parser.parse(s);
        assert!(parsed.is_some());
        let (tags, rest) = parsed.unwrap();
        let (tag, val) = tags[0];
        assert_eq!(tag, "Round");
        assert_eq!(val, "9");
        let (tag, val) = tags[1];
        assert_eq!(tag, "Date");
        assert_eq!(val, "1992.11.04");
        assert_eq!(rest, "");
    }
}
