use crate::parser;
use crate::parser::Parser;

// An incomplete parser for pgn. Doesn't support recursive variation or pgn-databases, but it
// should be somewhat trivial to add if needed.

// A pgn's syntax, taken from http://www.saremba.de/chessgml/standards/pgn/pgn-complete.htm
//
// <PGN-database> ::= <PGN-game> <PGN-database> <empty>
// <PGN-game> ::= <tag-section> <movetext-section>
// <tag-section> ::= <tag-pair> <tag-section> <empty>
// <tag-pair> ::= [ <tag-name> <tag-value> ]
// <tag-name> ::= <identifier>
// <tag-value> ::= <string>
// <movetext-section> ::= <element-sequence> <game-termination>
// <element-sequence> ::= <element> <element-sequence> <recursive-variation> <element-sequence> <empty>
// <element> ::= <move-number-indication> <SAN-move> <numeric-annotation-glyph>
// <recursive-variation> ::= ( <element-sequence> )
// <game-termination> ::= 1-0 0-1 1/2-1/2 *
// <empty> ::=

pub struct PgnGame {
    pub tags: Vec<TagPair>,
    pub moves: Vec<Element>,
    pub termination: GameTermination,
}

pub struct TagPair {
    pub name: String,
    pub value: String,
}

parser!(TagSectionParser, Vec<TagPair>, |text| {
    parser::OneOrMore::new(TagPairParser::new()).parse(text)
});

parser!(TagPairParser, TagPair, |text| {
    parser::ZeroOrMore::new(parser::Whitespace::new())
        .skip(parser::Literal::new('['))
        .then(parser::Word::new())
        .and_then(parser::StringLit::new())
        .skip(parser::Literal::new(']'))
        .parse(text)
        .map(|((name, value), rest)| {
            (
                TagPair {
                    name: name.to_string(),
                    value: value.to_string(),
                },
                rest,
            )
        })
});

#[derive(PartialEq, Eq)]
pub enum GameTermination {
    WhiteWins,
    BlackWins,
    Draw,
    NoResult,
}

parser!(GameTerminationParser, GameTermination, |text| {
    let (parsed, rest) = parser::Ident::new("1-0")
        .or(parser::Ident::new("0-1"))
        .or(parser::Ident::new("1/2-1/2"))
        .or(parser::Ident::new("*"))
        .parse(text)?;

    match parsed {
        "1-0" => Some((GameTermination::WhiteWins, rest)),
        "0-1" => Some((GameTermination::BlackWins, rest)),
        "1/2-1/2" => Some((GameTermination::Draw, rest)),
        "*" => Some((GameTermination::NoResult, rest)),
        _ => None,
    }
});

#[derive(Debug, PartialEq, Eq)]
pub struct Ply {
    pub mov: String,
    pub comment: Option<String>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Element {
    pub move_num: u64,
    pub white: Ply,
    pub black: Option<Ply>,
}

parser!(Comment, String, |text| {
    parser::Literal::new('{')
        .then(parser::TakeWhile::new(|c| c != '}'))
        .skip(parser::Literal::new('}'))
        .parse(text)
        .map(|(s, rest)| (s.to_string(), rest))
});

parser!(ElementParser, Element, |text| {
    let ply = parser::ZeroOrMore::new(parser::Whitespace::new())
        .then(parser::Word::new())
        .and_then(parser::Optionally::new(Comment::new()));

    let (((move_num, (alg_move, comment)), ply2), rest) =
        parser::ZeroOrMore::new(parser::Whitespace::new())
            .then(parser::NumberLit::new())
            .skip(parser::OneOrMore::new(parser::Literal::new('.')))
            .and_then(ply.clone())
            .and_then(parser::Optionally::new(ply))
            .parse(text)?;
    let white = Ply {
        mov: alg_move.to_string(),
        comment,
    };
    let black = ply2.map(|(mov, comment)| Ply {
        mov: mov.to_string(),
        comment,
    });
    Some((
        Element {
            move_num,
            white,
            black,
        },
        rest,
    ))
});

parser!(ElementSequenceParser, Vec<Element>, |text| {
    parser::OneOrMore::new(ElementParser::new()).parse(text)
});

parser!(
    MoveTextSectionParser,
    (Vec<Element>, GameTermination),
    |text| {
        ElementSequenceParser::new()
            .and_then(GameTerminationParser::new())
            .parse(text)
    }
);
parser!(PgnGameParser, PgnGame, |text| {
    TagSectionParser::new()
        .and_then(MoveTextSectionParser::new())
        .parse(text)
        .map(|((tags, (moves, termination)), rest)| {
            (
                PgnGame {
                    tags,
                    moves,
                    termination,
                },
                rest,
            )
        })
});

#[cfg(test)]
mod tests {
    use crate::pgn::*;

    #[test]
    fn tag_one_pair() {
        let s = "[Round \"9\"]";
        let parsed = TagPairParser::new().parse(s);
        assert!(parsed.is_some());
        let (
            TagPair {
                name: tag,
                value: val,
            },
            rest,
        ) = parsed.unwrap();
        assert_eq!(tag, "Round");
        assert_eq!(val, "9");
        assert_eq!(rest, "");
    }

    #[test]
    fn tag_pairs() {
        let s = "[Round \"9\"]\n[Date \"1992.11.04\"]";
        let parser = parser::SeparatedList::new(TagPairParser::new(), parser::Whitespace::new());
        let parsed = parser.parse(s);
        assert!(parsed.is_some());
        let (tags, rest) = parsed.unwrap();
        let TagPair {
            name: tag,
            value: val,
        } = &tags[0];
        assert_eq!(tag, "Round");
        assert_eq!(val, "9");
        let TagPair {
            name: tag,
            value: val,
        } = &tags[1];
        assert_eq!(tag, "Date");
        assert_eq!(val, "1992.11.04");
        assert_eq!(rest, "");
    }

    #[test]
    fn one_move() {
        let s = "1. e3 {+0.04/8 5.0s} d5 {-0.04/7 5.0s}";
        let parser = ElementParser::new();
        let parsed = parser.parse(s);
        assert!(parsed.is_some());
        let (mov, rest) = parsed.unwrap();

        assert_eq!(rest, "");
        assert_eq!(mov.move_num, 1);
        assert_eq!(
            mov.white,
            Ply {
                mov: String::from("e3"),
                comment: Some(String::from("+0.04/8 5.0s"))
            }
        );
        assert_eq!(
            mov.black,
            Some(Ply {
                mov: String::from("d5"),
                comment: Some(String::from("-0.04/7 5.0s"))
            })
        );
    }

    #[test]
    fn parse_tags() {
        let tags = r###"        
[Event "?"]
[Site "?"]
[Date "2024.08.21"]
[Round "?"]
[White "gem"]
[Black "gem"]
[Result "1/2-1/2"]
[ECO "A00"]
[GameDuration "00:09:10"]
[GameEndTime "2024-08-21T22:35:21.457 PDT"]
[GameStartTime "2024-08-21T22:26:11.364 PDT"]
[Opening "Van't Kruijs Opening"]
[PlyCount "110"]
[TimeControl "6/move"]
    "###;
        let parsed = TagSectionParser::new().parse(tags);
        assert!(parsed.is_some());
    }
    #[test]
    fn parse_movetext() {
        let movetext = r###"        
1. e3 {+0.04/8 5.0s} d5 {-0.04/7 5.0s} 2. Nf3 {0.00/7 5.0s} Qd6 {-0.01/7 5.0s}
3. c4 {0.00/7 5.0s} dxc4 {-0.01/7 5.0s} 4. Qa4+ {+0.03/6 5.0s}
Nc6 {-0.02/7 5.0s} 5. Na3 {0.00/7 5.0s} a6 {-0.02/7 5.0s} 6. Nxc4 {+0.02/6 5.0s}
Qd7 {-0.03/7 5.0s} 7. Qb3 {+0.01/7 5.0s} Nf6 {-0.02/7 5.0s}
8. Nce5 {+0.02/7 5.0s} Nxe5 {-0.04/7 5.0s} 9. Nxe5 {+0.02/7 5.0s}
Qd5 {-0.05/7 5.0s} 10. d4 {+0.05/6 5.0s} e6 {-0.02/6 5.0s} 11. f3 {+0.04/6 5.0s}
b5 {-0.01/6 5.0s} 12. Kd2 {+0.03/6 5.0s} h5 {-0.02/7 5.0s} 13. h4 {+0.02/6 5.0s}
Bb7 {-0.02/7 5.0s} 14. Nd3 {+0.02/6 5.0s} Qd6 {-0.02/7 5.0s}
15. a4 {+0.02/6 5.0s} b4 {-0.03/7 5.0s} 16. Nc5 {+0.03/7 5.0s}
Bc8 {-0.04/7 5.0s} 17. a5 {+0.03/7 5.0s} Nd7 {-0.04/7 5.0s}
18. Ne4 {+0.04/7 5.0s} Qd5 {-0.04/7 5.0s} 19. Bc4 {+0.06/7 5.0s}
Qb7 {-0.06/7 5.0s} 20. Kd3 {+0.05/7 5.0s} Qc6 {-0.06/7 5.0s}
21. Bd2 {+0.05/7 5.0s} Rb8 {-0.06/7 5.0s} 22. Qa4 {+0.06/6 5.0s}
Qxa4 {-0.05/7 5.0s} 23. Rxa4 {+0.05/7 5.0s} Bb7 {-0.06/7 5.0s}
24. Raa1 {+0.05/7 5.0s} Rd8 {-0.06/7 5.0s} 25. Kc2 {+0.05/7 5.0s}
Rb8 {-0.06/7 5.0s} 26. Be1 {+0.06/7 5.0s} Rh6 {-0.07/7 5.0s}
27. Bg3 {+0.06/7 5.0s} Rc8 {-0.07/7 5.0s} 28. Rh3 {+0.05/7 5.0s}
f5 {-0.05/7 5.0s} 29. Ng5 {+0.05/7 5.0s} c5 {-0.05/7 5.0s}
30. Nxe6 {+0.04/7 5.0s} cxd4 {-0.03/7 5.0s} 31. Nxf8 {+0.04/7 5.0s}
Kxf8 {-0.09/7 5.0s} 32. Kb3 {+0.09/7 5.0s} dxe3 {-0.09/7 5.0s}
33. Rd1 {+0.11/7 5.0s} Ke7 {-0.11/7 5.0s} 34. Re1 {+1.03/7 5.0s}
Rf8 {-0.09/7 5.0s} 35. Rxe3+ {+1.09/7 5.0s} Kd8 {-1.11/8 5.0s}
36. Bf4 {+1.11/7 5.0s} Rg6 {-1.12/8 5.0s} 37. Bg5+ {+1.12/8 5.0s}
Kc7 {-1.11/8 5.0s} 38. Kxb4 {+1.11/7 5.0s} f4 {-1.12/7 5.0s}
39. Re1 {+1.13/7 5.0s} Kc6 {-1.14/7 5.0s} 40. Bd3 {+3.06/7 5.0s}
Rgf6 {-3.06/8 5.0s} 41. Be4+ {+3.06/7 5.0s} Kc7 {-3.11/8 5.0s}
42. Bxf6 {+3.11/7 5.0s} gxf6 {-3.10/8 5.0s} 43. Bxb7 {+3.09/8 5.0s}
Kxb7 {-3.06/8 5.0s} 44. Re7 {+3.07/8 5.0s} Kc7 {-3.08/8 5.0s}
45. Ka3 {+3.91/9 5.0s} Rg8 {-3.91/8 5.0s} 46. Rh2 {+3.10/8 5.0s}
Rb8 {-3.94/8 5.0s} 47. b4 {+3.96/8 5.0s} Rb5 {-3.95/8 5.0s}
48. Re6 {+3.98/8 5.0s} Re5 {-3.98/9 5.0s} 49. Rxa6 {+3.98/9 5.0s}
Re3+ {-3.98/8 5.0s} 50. Ka4 {+3.98/9 5.0s} Re2 {-4.00/9 5.0s}
51. Ra7+ {+3.98/9 5.0s} Kc8 {-4.00/9 5.0s} 52. Ra8+ {+3.98/9 5.0s}
Nb8 {-4.00/9 5.0s} 53. Ra7 {+4.00/8 5.0s} Nd7 {-4.00/9 5.0s}
54. Ra8+ {+3.98/9 5.0s} Nb8 {-4.00/9 5.0s} 55. Ra7 {+4.00/8 5.0s}
Nd7 {-4.00/9 5.0s, Draw by 3-fold repetition} 1/2-1/2"###;
        let parsed = MoveTextSectionParser::new().parse(movetext);
        assert!(parsed.is_some());
    }

    #[test]
    fn parse_demo() {
        let pgn = r###"        
[Event "?"]
[Site "?"]
[Date "2024.08.21"]
[Round "?"]
[White "gem"]
[Black "gem"]
[Result "1/2-1/2"]
[ECO "A00"]
[GameDuration "00:09:10"]
[GameEndTime "2024-08-21T22:35:21.457 PDT"]
[GameStartTime "2024-08-21T22:26:11.364 PDT"]
[Opening "Van't Kruijs Opening"]
[PlyCount "110"]
[TimeControl "6/move"]

1. e3 {+0.04/8 5.0s} d5 {-0.04/7 5.0s} 2. Nf3 {0.00/7 5.0s} Qd6 {-0.01/7 5.0s}
3. c4 {0.00/7 5.0s} dxc4 {-0.01/7 5.0s} 4. Qa4+ {+0.03/6 5.0s}
Nc6 {-0.02/7 5.0s} 5. Na3 {0.00/7 5.0s} a6 {-0.02/7 5.0s} 6. Nxc4 {+0.02/6 5.0s}
Qd7 {-0.03/7 5.0s} 7. Qb3 {+0.01/7 5.0s} Nf6 {-0.02/7 5.0s}
8. Nce5 {+0.02/7 5.0s} Nxe5 {-0.04/7 5.0s} 9. Nxe5 {+0.02/7 5.0s}
Qd5 {-0.05/7 5.0s} 10. d4 {+0.05/6 5.0s} e6 {-0.02/6 5.0s} 11. f3 {+0.04/6 5.0s}
b5 {-0.01/6 5.0s} 12. Kd2 {+0.03/6 5.0s} h5 {-0.02/7 5.0s} 13. h4 {+0.02/6 5.0s}
Bb7 {-0.02/7 5.0s} 14. Nd3 {+0.02/6 5.0s} Qd6 {-0.02/7 5.0s}
15. a4 {+0.02/6 5.0s} b4 {-0.03/7 5.0s} 16. Nc5 {+0.03/7 5.0s}
Bc8 {-0.04/7 5.0s} 17. a5 {+0.03/7 5.0s} Nd7 {-0.04/7 5.0s}
18. Ne4 {+0.04/7 5.0s} Qd5 {-0.04/7 5.0s} 19. Bc4 {+0.06/7 5.0s}
Qb7 {-0.06/7 5.0s} 20. Kd3 {+0.05/7 5.0s} Qc6 {-0.06/7 5.0s}
21. Bd2 {+0.05/7 5.0s} Rb8 {-0.06/7 5.0s} 22. Qa4 {+0.06/6 5.0s}
Qxa4 {-0.05/7 5.0s} 23. Rxa4 {+0.05/7 5.0s} Bb7 {-0.06/7 5.0s}
24. Raa1 {+0.05/7 5.0s} Rd8 {-0.06/7 5.0s} 25. Kc2 {+0.05/7 5.0s}
Rb8 {-0.06/7 5.0s} 26. Be1 {+0.06/7 5.0s} Rh6 {-0.07/7 5.0s}
27. Bg3 {+0.06/7 5.0s} Rc8 {-0.07/7 5.0s} 28. Rh3 {+0.05/7 5.0s}
f5 {-0.05/7 5.0s} 29. Ng5 {+0.05/7 5.0s} c5 {-0.05/7 5.0s}
30. Nxe6 {+0.04/7 5.0s} cxd4 {-0.03/7 5.0s} 31. Nxf8 {+0.04/7 5.0s}
Kxf8 {-0.09/7 5.0s} 32. Kb3 {+0.09/7 5.0s} dxe3 {-0.09/7 5.0s}
33. Rd1 {+0.11/7 5.0s} Ke7 {-0.11/7 5.0s} 34. Re1 {+1.03/7 5.0s}
Rf8 {-0.09/7 5.0s} 35. Rxe3+ {+1.09/7 5.0s} Kd8 {-1.11/8 5.0s}
36. Bf4 {+1.11/7 5.0s} Rg6 {-1.12/8 5.0s} 37. Bg5+ {+1.12/8 5.0s}
Kc7 {-1.11/8 5.0s} 38. Kxb4 {+1.11/7 5.0s} f4 {-1.12/7 5.0s}
39. Re1 {+1.13/7 5.0s} Kc6 {-1.14/7 5.0s} 40. Bd3 {+3.06/7 5.0s}
Rgf6 {-3.06/8 5.0s} 41. Be4+ {+3.06/7 5.0s} Kc7 {-3.11/8 5.0s}
42. Bxf6 {+3.11/7 5.0s} gxf6 {-3.10/8 5.0s} 43. Bxb7 {+3.09/8 5.0s}
Kxb7 {-3.06/8 5.0s} 44. Re7 {+3.07/8 5.0s} Kc7 {-3.08/8 5.0s}
45. Ka3 {+3.91/9 5.0s} Rg8 {-3.91/8 5.0s} 46. Rh2 {+3.10/8 5.0s}
Rb8 {-3.94/8 5.0s} 47. b4 {+3.96/8 5.0s} Rb5 {-3.95/8 5.0s}
48. Re6 {+3.98/8 5.0s} Re5 {-3.98/9 5.0s} 49. Rxa6 {+3.98/9 5.0s}
Re3+ {-3.98/8 5.0s} 50. Ka4 {+3.98/9 5.0s} Re2 {-4.00/9 5.0s}
51. Ra7+ {+3.98/9 5.0s} Kc8 {-4.00/9 5.0s} 52. Ra8+ {+3.98/9 5.0s}
Nb8 {-4.00/9 5.0s} 53. Ra7 {+4.00/8 5.0s} Nd7 {-4.00/9 5.0s}
54. Ra8+ {+3.98/9 5.0s} Nb8 {-4.00/9 5.0s} 55. Ra7 {+4.00/8 5.0s}
Nd7 {-4.00/9 5.0s, Draw by 3-fold repetition} 1/2-1/2
    "###;
        let parsed = PgnGameParser::new().parse(pgn);
        assert!(parsed.is_some());
    }
}
