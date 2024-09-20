use crate::parser;
use crate::parser::Parser;
use crate::board::AlgebraicMove;
use std::collections::HashMap;

struct Pgn {
    tags: HashMap<String, String>,
    moves: Vec<AlgebraicMove>,
}

parser!(TagPair, (&'a str, &'a str), |text| {
    parser::Literal::new('[')
        .then(parser::Word::new())
        .and_then(parser::StringLit::new())
        .skip(parser::Literal::new(']'))
        .parse(text)
});


#[derive(Clone)]
pub struct MoveText {}

#[derive(Debug, PartialEq, Eq)]
pub struct Ply {
    mov: String,
    comment: Option<String>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct Move {
    move_num: u64,
    white: Ply,
    black: Option<Ply>,
}

parser!(Comment, String, |text| {
    parser::Literal::new('{')
        .then(parser::TakeWhile::new(|c| c != '}'))
        .skip(parser::Literal::new('}'))
        .parse(text)
        .map(|(s, rest)| (s.to_string(), rest))
});

parser!(MoveParser, Move, |text| {
    let ply = parser::Whitespace::new()
        .then(parser::Word::new())
        .and_then(parser::Optionally::new(Comment::new()));

    let (((move_num, (alg_move, comment)), ply2), rest) = parser::NumberLit::new()
        .skip(parser::Literal::new('.'))
        .and_then(ply.clone())
        .and_then(parser::Optionally::new(ply))
        .parse(text)?;
    let white = Ply {
        mov: alg_move.to_string(),
        comment,
    };
    let black = ply2.map(|(mov, comment)|
        Ply {
            mov: mov.to_string(),
            comment,
        });
    Some((
        Move {
            move_num,
            white,
            black,
        },
        rest,
    ))
});

#[cfg(test)]
mod tests {
    use crate::pgn::*;

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
    fn tag_pairs() {
        let s = "[Round \"9\"]\n[Date \"1992.11.04\"]";
        let parser = parser::SeparatedList::new(TagPair::new(), parser::Whitespace::new());
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

    #[test]
    fn one_move() {
        let s = "1. e3 {+0.04/8 5.0s} d5 {-0.04/7 5.0s}";
        let parser = MoveParser::new();
        let parsed = parser.parse(s);
        assert!(parsed.is_some());
        let (mov, rest) = parsed.unwrap();

        assert_eq!(rest, "");
        assert_eq!(mov.move_num, 1);
        assert_eq!(mov.white, Ply{mov: String::from("e3"), comment: Some(String::from("+0.04/8 5.0s"))});
        assert_eq!(mov.black, Some(Ply{mov: String::from("d5"), comment: Some(String::from("-0.04/7 5.0s"))}));
    }
}
