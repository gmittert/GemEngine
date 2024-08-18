use crate::{board, shared_hashmap::SharedHashMap};
use std::{
    collections::HashMap,
    fmt,
    ops::{Add, AddAssign},
};

#[derive(Debug, PartialEq, Clone, Default)]
pub struct PerftResult {
    pub nodes: usize,
    pub captures: usize,
    pub enpassants: usize,
    pub castles: usize,
    pub promotions: usize,
    pub checks: usize,
    pub checkmates: usize,
}

impl Add for PerftResult {
    type Output = PerftResult;

    fn add(self, rhs: Self) -> Self::Output {
        PerftResult {
            nodes: self.nodes + rhs.nodes,
            captures: self.captures + rhs.captures,
            enpassants: self.enpassants + rhs.enpassants,
            castles: self.castles + rhs.castles,
            promotions: self.promotions + rhs.promotions,
            checks: self.checks + rhs.checks,
            checkmates: self.checkmates + rhs.checkmates,
        }
    }
}
impl AddAssign for PerftResult {
    fn add_assign(&mut self, rhs: Self) {
        self.nodes += rhs.nodes;
        self.captures += rhs.captures;
        self.enpassants += rhs.enpassants;
        self.castles += rhs.castles;
        self.promotions += rhs.promotions;
        self.checks += rhs.checks;
        self.checkmates += rhs.checkmates;
    }
}

impl fmt::Display for PerftResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Nodes: {}", self.nodes)?;
        writeln!(f, "Captures: {}", self.captures)?;
        writeln!(f, "En Passants: {}", self.enpassants)?;
        writeln!(f, "Castles: {}", self.castles)?;
        writeln!(f, "Promotions: {}", self.promotions)?;
        writeln!(f, "Checks: {}", self.checks)?;
        writeln!(f, "Checkmates: {}", self.checkmates)
    }
}

pub fn perft(b: &mut board::Board, depth: u8) -> PerftResult {
    if depth == 0 {
        return PerftResult {
            nodes: 1,
            ..Default::default()
        };
    }
    let mut cache: HashMap<u64, PerftResult> = HashMap::new();
    let res = perft_inner(b, depth, &mut cache).clone();
    res
}

fn perft_inner(
    b: &mut board::Board,
    depth: u8,
    cache: &mut HashMap<u64, PerftResult>,
) -> PerftResult {
    const DEPTH_KEYS: [u64; 10] = [
        0xfd4b4027ed1f23fc,
        0x1e1868cda8784d1f,
        0xe9fe0f4d017a548b,
        0xefd0a39fbd797aaa,
        0xacff95d3fc47bb52,
        0xaa24744cf0bbed33,
        0x9d9a762a6f0fdc52,
        0x3a1dde62a0f9b96a,
        0x24a16969e817cf8c,
        0x5c28ed2953879b8a,
    ];
    let hash = b.hash ^ DEPTH_KEYS[depth as usize];
    if let Some(v) = cache.get(&hash) {
        return v.clone();
    }
    let mut result = PerftResult {
        nodes: 0,
        captures: 0,
        checks: 0,
        checkmates: 0,
        enpassants: 0,
        castles: 0,
        promotions: 0,
    };
    let moves = board::generate_pseudo_legal_moves(b);
    for m in &moves {
        let preb = b.black_pieces();
        let prew = b.white_pieces();
        assert!(
            preb & prew == board::BitBoard::empty(),
            "Before making move: {m}, black ({:?}) overlapped with white ({:?})",
            preb,
            prew,
        );
        b.make_move(&m);
        let mb = b.black_pieces();
        let mw = b.white_pieces();
        assert!(
            mb & mw == board::BitBoard::empty(),
            "After making move: {m}, black ({:?}) overlapped with white ({:?})",
            mb,
            mw
        );
        if !b.in_check(!b.to_play) {
            if depth > 1 {
                let next_res = perft_inner(b, depth - 1, cache);
                result += next_res;
            } else {
                result.nodes += 1;
                if b.in_check(b.to_play) {
                    result.checks += 1;
                    if perft_inner(b, 1, cache).nodes == 0 {
                        result.checkmates += 1;
                    }
                }
                if m.capture.is_some() {
                    result.captures += 1
                }
                if m.is_en_passant {
                    result.enpassants += 1
                }
                if m.is_castle_queen || m.is_castle_king {
                    result.castles += 1
                }
                if m.promotion.is_some() {
                    result.promotions += 1
                }
            }
        }
        b.undo_move(&m);
        let postb = b.black_pieces();
        let postw = b.white_pieces();
        assert!(
            postb & postw == board::BitBoard::empty(),
            "After undoing move: {m}, black ({:?}) overlapped with white ({:?})",
            postb,
            postw,
        );
        if preb != postb {
            println!("Undo_move failed for move: {m}");
            println!("board:\n{b}");
            assert_eq!(preb, postb);
        }
        assert_eq!(prew, postw);
    }

    if let Some(v) = cache.get(&hash) {
        assert_eq!(v, &result);
    }
    cache.insert(hash, result.clone());
    return result;
}
#[cfg(test)]
mod tests {
    // Test positions cribbed from https://www.chessprogramming.org/Perft_Results
    use crate::board::*;
    use crate::perft::*;
    #[test]
    fn perft0() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 0).nodes, 1);
    }
    #[test]
    fn perft1() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 1).nodes, 20);
    }

    #[test]
    fn perft2() {
        let mut b = starting_board();
        assert_eq!(perft(&mut b, 2).nodes, 400);
    }

    #[test]
    fn perft3() {
        let mut b = starting_board();
        let res = perft(&mut b, 3);
        assert_eq!(res.nodes, 8902);
        assert_eq!(res.captures, 34);
        assert_eq!(res.checks, 12);
        assert_eq!(res.enpassants, 0);
    }

    #[test]
    fn perft4() {
        let mut b = starting_board();
        let res = perft(&mut b, 4);
        assert_eq!(res.checks, 469);
        assert_eq!(res.captures, 1576);
        assert_eq!(res.nodes, 197281);
        assert_eq!(res.checkmates, 8);
        assert_eq!(res.enpassants, 0);
    }

    #[test]
    fn perft5() {
        let mut b = starting_board();
        let res = perft(&mut b, 5);
        assert_eq!(res.nodes, 4865609);
        assert_eq!(res.checks, 27351);
        assert_eq!(res.captures, 82719);
        assert_eq!(res.checkmates, 347);
        assert_eq!(res.enpassants, 258);
    }

    #[test]
    #[ignore]
    fn perft6() {
        let mut b = starting_board();
        let res = perft(&mut b, 6);
        println!("{res}");

        assert_eq!(res.nodes, 119060324);
        assert_eq!(res.checks, 809099);
        assert_eq!(res.captures, 2812008);
        assert_eq!(res.checkmates, 10828);
        assert_eq!(res.enpassants, 5248);
        assert_eq!(res.castles, 0);
    }

    #[test]
    #[ignore]
    fn perft7() {
        let mut b = starting_board();
        let res = perft(&mut b, 7);
        println!("{res}");

        assert_eq!(res.nodes, 3_195_901_860);
        assert_eq!(res.checks, 33_103_848);
        assert_eq!(res.captures, 108_329_926);
        assert_eq!(res.checkmates, 435_767);
        assert_eq!(res.enpassants, 319_617);
        assert_eq!(res.castles, 883_453);
    }

    #[test]
    #[ignore]
    fn perft8() {
        let mut b = starting_board();
        let res = perft(&mut b, 8);
        println!("{res}");

        assert_eq!(res.nodes, 84_998_978_956);
        assert_eq!(res.checks, 968_981_593);
        assert_eq!(res.captures, 3_523_740_106);
        assert_eq!(res.checkmates, 9_852_036);
        assert_eq!(res.enpassants, 7_187_977);
        assert_eq!(res.castles, 23_605_205);
    }

    #[test]
    fn kiwipete1() {
        let mut b =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .expect("failed to parse fen");
        let res = perft(&mut b, 1);
        println!("{res}");
        assert_eq!(res.nodes, 48);
        assert_eq!(res.checks, 0);
        assert_eq!(res.captures, 8);
        assert_eq!(res.castles, 2);
        assert_eq!(res.checkmates, 0);
        assert_eq!(res.enpassants, 0);
    }
    #[test]
    fn kiwipete2() {
        let mut b =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .expect("failed to parse fen");
        let res = perft(&mut b, 2);
        println!("{res}");
        assert_eq!(res.nodes, 2039);
        assert_eq!(res.checks, 3);
        assert_eq!(res.captures, 351);
        assert_eq!(res.castles, 91);
        assert_eq!(res.checkmates, 0);
        assert_eq!(res.enpassants, 1);
    }
    #[test]
    fn kiwipete3() {
        let mut b =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .expect("failed to parse fen");
        let res = perft(&mut b, 3);
        println!("{res}");
        assert_eq!(res.nodes, 97862);
        assert_eq!(res.checks, 993);
        assert_eq!(res.captures, 17102);
        assert_eq!(res.castles, 3162);
        assert_eq!(res.checkmates, 1);
        assert_eq!(res.enpassants, 45);
        assert_eq!(res.promotions, 0);
    }
    #[test]
    fn kiwipete4() {
        let mut b =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .expect("failed to parse fen");
        let res = perft(&mut b, 4);
        println!("{res}");
        assert_eq!(res.nodes, 4085603);
        assert_eq!(res.checks, 25523);
        assert_eq!(res.captures, 757163);
        assert_eq!(res.castles, 128013);
        assert_eq!(res.checkmates, 43);
        assert_eq!(res.enpassants, 1929);
        assert_eq!(res.promotions, 15172);
    }
    #[test]
    #[ignore]
    fn kiwipete5() {
        let mut b =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .expect("failed to parse fen");
        let res = perft(&mut b, 5);
        println!("{res}");
        assert_eq!(res.nodes, 193690690);
        assert_eq!(res.checks, 3309887);
        assert_eq!(res.captures, 35043416);
        assert_eq!(res.castles, 4993637);
        assert_eq!(res.checkmates, 30171);
        assert_eq!(res.enpassants, 73365);
        assert_eq!(res.promotions, 8392);
    }
    #[test]
    #[ignore]
    fn kiwipete6() {
        let mut b =
            Board::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1")
                .expect("failed to parse fen");
        let res = perft(&mut b, 6);
        println!("{res}");
        assert_eq!(res.nodes, 8031647685);
        assert_eq!(res.checks, 92238050);
        assert_eq!(res.captures, 1558445089);
        assert_eq!(res.castles, 184513607);
        assert_eq!(res.checkmates, 360003);
        assert_eq!(res.enpassants, 3577504);
        assert_eq!(res.promotions, 56627920);
    }
    #[test]
    fn pos3_1() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 14,
            captures: 1,
            enpassants: 0,
            castles: 0,
            promotions: 0,
            checks: 2,
            checkmates: 0,
        };
        let actual = perft(&mut b, 1);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos3_2() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 191,
            captures: 14,
            enpassants: 0,
            castles: 0,
            promotions: 0,
            checks: 10,
            checkmates: 0,
        };
        let actual = perft(&mut b, 2);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos3_3() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 2812,
            captures: 209,
            enpassants: 2,
            castles: 0,
            promotions: 0,
            checks: 267,
            checkmates: 0,
        };
        let actual = perft(&mut b, 3);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos3_4() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 43238,
            captures: 3348,
            enpassants: 123,
            castles: 0,
            promotions: 0,
            checks: 1680,
            checkmates: 17,
        };
        let actual = perft(&mut b, 4);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos3_5() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 674624,
            captures: 52051,
            enpassants: 1165,
            castles: 0,
            promotions: 0,
            checks: 52950,
            checkmates: 0,
        };
        let actual = perft(&mut b, 5);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos3_6() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 11030083,
            captures: 940350,
            enpassants: 33325,
            castles: 0,
            promotions: 7552,
            checks: 452473,
            checkmates: 2733,
        };
        let actual = perft(&mut b, 6);
        assert_eq!(actual, exp);
    }
    #[test]
    #[ignore]
    fn pos3_7() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 178633661,
            captures: 14519036,
            enpassants: 294874,
            castles: 0,
            promotions: 140024,
            checks: 12797406,
            checkmates: 87,
        };
        let actual = perft(&mut b, 7);
        assert_eq!(actual, exp);
    }
    #[test]
    #[ignore]
    fn pos3_8() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 3009794393,
            captures: 267586558,
            enpassants: 8009239,
            castles: 0,
            promotions: 6578076,
            checks: 135626805,
            checkmates: 450410,
        };
        let actual = perft(&mut b, 8);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_1() {
        let mut b =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 6,
            captures: 0,
            enpassants: 0,
            castles: 0,
            promotions: 0,
            checks: 0,
            checkmates: 0,
        };
        let actual = perft(&mut b, 1);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_2() {
        let mut b =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 264,
            captures: 87,
            enpassants: 0,
            castles: 6,
            promotions: 48,
            checks: 10,
            checkmates: 0,
        };
        let actual = perft(&mut b, 2);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_3() {
        let mut b =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 9467,
            captures: 1021,
            enpassants: 4,
            castles: 0,
            promotions: 120,
            checks: 38,
            checkmates: 22,
        };
        let actual = perft(&mut b, 3);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_4() {
        let mut b =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 422333,
            captures: 131393,
            enpassants: 0,
            castles: 7795,
            promotions: 60032,
            checks: 15492,
            checkmates: 5,
        };
        let actual = perft(&mut b, 4);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_5() {
        let mut b =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 15833292,
            captures: 2046173,
            enpassants: 6512,
            castles: 0,
            promotions: 329464,
            checks: 200568,
            checkmates: 50562,
        };
        let actual = perft(&mut b, 5);
        assert_eq!(actual, exp);
    }
    #[test]
    #[ignore]
    fn pos4_6() {
        let mut b =
            Board::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 706045033,
            captures: 210369132,
            enpassants: 212,
            castles: 10882006,
            promotions: 81102984,
            checks: 26973664,
            checkmates: 81076,
        };
        let actual = perft(&mut b, 6);
        assert_eq!(actual, exp);
    }

    #[test]
    fn pos4_1b() {
        let mut b =
            Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 6,
            captures: 0,
            enpassants: 0,
            castles: 0,
            promotions: 0,
            checks: 0,
            checkmates: 0,
        };
        let actual = perft(&mut b, 1);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_2b() {
        let mut b =
            Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 264,
            captures: 87,
            enpassants: 0,
            castles: 6,
            promotions: 48,
            checks: 10,
            checkmates: 0,
        };
        let actual = perft(&mut b, 2);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_3b() {
        let mut b =
            Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 9467,
            captures: 1021,
            enpassants: 4,
            castles: 0,
            promotions: 120,
            checks: 38,
            checkmates: 22,
        };
        let actual = perft(&mut b, 3);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_4b() {
        let mut b =
            Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 422333,
            captures: 131393,
            enpassants: 0,
            castles: 7795,
            promotions: 60032,
            checks: 15492,
            checkmates: 5,
        };
        let actual = perft(&mut b, 4);
        assert_eq!(actual, exp);
    }
    #[test]
    fn pos4_5b() {
        let mut b =
            Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 15833292,
            captures: 2046173,
            enpassants: 6512,
            castles: 0,
            promotions: 329464,
            checks: 200568,
            checkmates: 50562,
        };
        let actual = perft(&mut b, 5);
        assert_eq!(actual, exp);
    }
    #[test]
    #[ignore]
    fn pos4_6b() {
        let mut b =
            Board::from_fen("r2q1rk1/pP1p2pp/Q4n2/bbp1p3/Np6/1B3NBn/pPPP1PPP/R3K2R b KQ - 0 1")
                .expect("failed to parse fen");
        let exp = PerftResult {
            nodes: 706045033,
            captures: 210369132,
            enpassants: 212,
            castles: 10882006,
            promotions: 81102984,
            checks: 26973664,
            checkmates: 81076,
        };
        let actual = perft(&mut b, 6);
        assert_eq!(actual, exp);
    }

    #[test]
    fn pos5_1() {
        let mut b = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
            .expect("failed to parse fen");
        let actual = perft(&mut b, 1);
        assert_eq!(actual.nodes, 44);
    }
    #[test]
    fn pos5_2() {
        let mut b = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
            .expect("failed to parse fen");
        let actual = perft(&mut b, 2);
        assert_eq!(actual.nodes, 1486);
    }
    #[test]
    fn pos5_3() {
        let mut b = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
            .expect("failed to parse fen");
        let actual = perft(&mut b, 3);
        assert_eq!(actual.nodes, 62379);
    }
    #[test]
    fn pos5_4() {
        let mut b = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
            .expect("failed to parse fen");
        let actual = perft(&mut b, 4);
        assert_eq!(actual.nodes, 2103487);
    }
    #[test]
    #[ignore]
    fn pos5_5() {
        let mut b = Board::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8")
            .expect("failed to parse fen");
        let actual = perft(&mut b, 5);
        assert_eq!(actual.nodes, 89941194);
    }
    #[test]
    fn pos6_1() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 1);
        assert_eq!(actual.nodes, 46);
    }
    #[test]
    fn pos6_2() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 2);
        assert_eq!(actual.nodes, 2_079);
    }
    #[test]
    fn pos6_3() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 3);
        assert_eq!(actual.nodes, 89_890);
    }
    #[test]
    fn pos6_4() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 4);
        assert_eq!(actual.nodes, 3_894_594);
    }
    #[test]
    #[ignore]
    fn pos6_5() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 5);
        assert_eq!(actual.nodes, 164_075_551);
    }
    #[test]
    #[ignore]
    fn pos6_6() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 6);
        assert_eq!(actual.nodes, 6_923_051_137);
    }
    #[test]
    #[ignore]
    fn pos6_7() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 7);
        assert_eq!(actual.nodes, 287_188_994_746);
    }
    #[test]
    #[ignore]
    fn pos6_8() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 8);
        assert_eq!(actual.nodes, 11_923_589_843_526);
    }
    #[test]
    #[ignore]
    fn pos6_9() {
        let mut b = Board::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10",
        )
        .expect("failed to parse fen");
        let actual = perft(&mut b, 9);
        assert_eq!(actual.nodes, 490_154_852_788_714);
    }
}
