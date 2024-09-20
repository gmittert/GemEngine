use crate::parser::Parser;
#[derive(Clone)]
pub struct Or<L, R> {
    l: L,
    r: R,
}

impl<L, R> Or<L, R> {
    pub fn new(l: L, r: R) -> Or<L, R> {
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
    pub fn new(l: L, r: R) -> AndThen<L, R> {
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
    pub fn new(l: L, r: R) -> Then<L, R> {
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
    pred: fn(char) -> bool,
}

impl TakeWhile {
    pub fn new(pred: fn(char) -> bool) -> TakeWhile {
        TakeWhile { pred }
    }
}

impl<'a> Parser<'a> for TakeWhile {
    type Item = &'a str;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let idx = text.find(|c| !(self.pred)(c))?;
        Some(text.split_at(idx))
    }
}

#[derive(Clone)]
pub struct ZeroOrMore<P> {
    parser: P,
}

impl<P> ZeroOrMore<P> {
    pub fn new(parser: P) -> ZeroOrMore<P> {
        ZeroOrMore { parser }
    }
}

impl<'a, P> Parser<'a> for ZeroOrMore<P>
where
    P: Parser<'a>,
{
    type Item = Vec<P::Item>;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let mut res = Vec::new();
        let mut text = text;
        while let Some((val, remaining)) = self.parser.parse(text) {
            res.push(val);
            text = remaining;
        }
        Some((res, text))
    }
}

#[derive(Clone)]
pub struct Optionally<P> {
    parser: P,
}

impl<P> Optionally<P> {
    pub fn new(parser: P) -> Optionally<P> {
        Optionally { parser }
    }
}

impl<'a, P> Parser<'a> for Optionally<P>
where
    P: Parser<'a>,
{
    type Item = Option<P::Item>;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        if let Some((val, remaining)) = self.parser.parse(text) {
            Some((Some(val), remaining))
        } else {
            Some((None, text))
        }
    }
}

#[derive(Clone)]
pub struct OneOrMore<P> {
    parser: P,
}

impl<P> OneOrMore<P> {
    pub fn new(parser: P) -> OneOrMore<P> {
        OneOrMore { parser }
    }
}

impl<'a, P> Parser<'a> for OneOrMore<P>
where
    P: Parser<'a>,
{
    type Item = Vec<P::Item>;
    fn parse(&self, text: &'a str) -> Option<(Self::Item, &'a str)> {
        let (fst, rest) = self.parser.parse(text)?;
        let mut res = vec![fst];
        let mut text = rest;
        while let Some((val, remaining)) = self.parser.parse(text) {
            res.push(val);
            text = remaining;
        }
        Some((res, text))
    }
}

#[derive(Clone)]
pub struct Skip<L, R> {
    l: L,
    r: R,
}

impl<L, R> Skip<L, R> {
    pub fn new(l: L, r: R) -> Skip<L, R> {
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

#[cfg(test)]
mod tests {
    use crate::parser::*;
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
    fn zero_or_more() {
        let a = Literal::new('a');
        let zero_or_more = ZeroOrMore::new(a);

        let s = "";
        let parsed = zero_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec![]);
        assert_eq!(rest, "");

        let s = "a";
        let parsed = zero_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec!['a']);
        assert_eq!(rest, "");

        let s = "aaaa";
        let parsed = zero_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec!['a', 'a', 'a', 'a']);
        assert_eq!(rest, "");

        let s = "aaaab";
        let parsed = zero_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec!['a', 'a', 'a', 'a']);
        assert_eq!(rest, "b");

        let s = "baaaab";
        let parsed = zero_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec![]);
        assert_eq!(rest, "baaaab");
    }

    #[test]
    fn one_or_more() {
        let a = Literal::new('a');
        let one_or_more = OneOrMore::new(a);

        let s = "";
        let parsed = one_or_more.parse(s);
        assert!(parsed.is_none());

        let s = "a";
        let parsed = one_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec!['a']);
        assert_eq!(rest, "");

        let s = "aaaa";
        let parsed = one_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec!['a', 'a', 'a', 'a']);
        assert_eq!(rest, "");

        let s = "aaaab";
        let parsed = one_or_more.parse(s);
        assert!(parsed.is_some());
        let (v, rest) = parsed.unwrap();
        assert_eq!(v, vec!['a', 'a', 'a', 'a']);
        assert_eq!(rest, "b");

        let s = "baaaab";
        let parsed = one_or_more.parse(s);
        assert!(parsed.is_none());
    }


}
