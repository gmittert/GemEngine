use crate::parser;
use crate::parser::combinators::*;
use crate::parser::*;

#[derive(Clone)]
pub struct Literal {
    c: char,
}

impl Literal {
    pub fn new(c: char) -> Literal {
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
pub struct Ident {
    i: String,
}

impl Ident {
    pub fn new(i: &str) -> Ident {
        Ident { i: i.to_string() }
    }
}

impl<'a> Parser<'a> for Ident {
    type Item = &'a str;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let (_, text) = parser::ZeroOrMore::new(parser::Whitespace::new()).parse(text)?;
        if !text.starts_with(&self.i) {
            return None;
        }
        let (ident, text) = text.split_at(self.i.len());
        let (_, text) = parser::ZeroOrMore::new(parser::Whitespace::new()).parse(text)?;
        Some((ident, text))
    }
}

parser!(Whitespace, (), |text| {
    Literal::new('\n')
        .or(Literal::new(' '))
        .or(Literal::new('\t'))
        .parse(text)
        .map(|(_, rest)| ((), rest))
});

parser!(Digit, u32, |text| {
    Literal::new('1')
        .or(Literal::new('2'))
        .or(Literal::new('3'))
        .or(Literal::new('4'))
        .or(Literal::new('5'))
        .or(Literal::new('6'))
        .or(Literal::new('7'))
        .or(Literal::new('8'))
        .or(Literal::new('9'))
        .or(Literal::new('0'))
        .parse(text)
        .map(|(c, rest)| (c.to_digit(10).unwrap(), rest))
});

pub struct SeparatedList<C, W> {
    contents: C,
    seperator: W,
}
impl<C, W> SeparatedList<C, W> {
    pub fn new(contents: C, seperator: W) -> SeparatedList<C, W> {
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
        .then(TakeWhile::new(|c| c != '"'))
        .skip(Literal::new('"'))
        .parse(text)
});

parser!(NumberLit, u64, |text: &'a str| {
    OneOrMore::new(Digit::new())
        .parse(text)
        .map(|(digits, rest)| {
            let mut res: u64 = 0;
            for d in digits {
                res *= 10;
                res += d as u64;
            }
            (res, rest)
        })
});

#[cfg(test)]
mod tests {
    use crate::parser::*;

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
}
