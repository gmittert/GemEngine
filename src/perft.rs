use crate::board;

#[derive(Debug, PartialEq)]
pub struct PerfResult {
    nodes: usize,
    captures: usize,
    checks: usize,
    checkmates: usize,
    enpassants: usize,
    castles: usize,
    promotions: usize,
}

pub fn perft(b: &mut board::Board, depth: u8) -> PerfResult {
    let mut result = PerfResult {
        nodes: 0,
        captures: 0,
        checks: 0,
        checkmates: 0,
        enpassants: 0,
        castles: 0,
        promotions: 0,
    };
    if depth == 0 {
        result.nodes = 1;
        if b.in_check(b.to_play) {
            result.checks = 1;
            if perft(b, 1).nodes == 0 {
                result.checkmates = 1;
            }
        }
        if let Some(last_move) = b.move_list.last() {
            if last_move.capture.is_some() {
                result.captures = 1
            }
            if last_move.is_en_passant {
                result.enpassants = 1
            }
            if last_move.is_castle_queen || last_move.is_castle_king {
                result.castles = 1
            }
            if last_move.promotion.is_some() {
                result.promotions = 1
            }
        }
        return result;
    }

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
        if m.capture.is_some() && depth == 1 {
            println!("{m}\n");
            println!("{b}");
        }
        b.make_move(&m);
        if m.capture.is_some() && depth == 1 {
            println!("{b}");
        }
        let mb = b.black_pieces();
        let mw = b.white_pieces();
        assert!(
            mb & mw == board::BitBoard::empty(),
            "After making move: {m}, black ({:?}) overlapped with white ({:?})",
            mb,
            mw
        );
        //println!("{b}");
        if !b.in_check(!b.to_play) {
            let next_res = perft(b, depth - 1);
            result.nodes += next_res.nodes;
            result.captures += next_res.captures;
            result.checks += next_res.checks;
            result.checkmates += next_res.checkmates;
            result.enpassants += next_res.enpassants;
            result.castles += next_res.castles;
            result.promotions += next_res.promotions;
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
    result
}
#[cfg(test)]
mod tests {
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
    fn perft6() {
        let mut b = starting_board();
        let res = perft(&mut b, 6);

        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles: {}", res.castles);
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

        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles: {}", res.castles);
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

        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles: {}", res.castles);
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
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
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
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
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
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles: {}", res.castles);
        println!("Promotions: {}", res.promotions);
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
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
        println!("Promotions: {}", res.promotions);
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
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
        println!("Promotions: {}", res.promotions);
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
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
        println!("Promotions: {}", res.promotions);
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
        let res = perft(&mut b, 1);
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
        println!("Promotions: {}", res.promotions);
        assert_eq!(res.nodes, 14);
        assert_eq!(res.captures, 1);
        assert_eq!(res.enpassants, 0);
        assert_eq!(res.castles, 0);
        assert_eq!(res.promotions, 0);
        assert_eq!(res.checks, 2);
        assert_eq!(res.checkmates, 0);
    }
    #[test]
    fn pos3_2() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let res = perft(&mut b, 2);
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
        println!("Promotions: {}", res.promotions);
        assert_eq!(res.nodes, 191);
        assert_eq!(res.captures, 14);
        assert_eq!(res.enpassants, 0);
        assert_eq!(res.castles, 0);
        assert_eq!(res.promotions, 0);
        assert_eq!(res.checks, 10);
        assert_eq!(res.checkmates, 0);
    }
    #[test]
    fn pos3_3() {
        let mut b = Board::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1")
            .expect("failed to parse fen");
        let res = perft(&mut b, 3);
        println!("Nodes: {}", res.nodes);
        println!("Checks: {}", res.checks);
        println!("Captures: {}", res.captures);
        println!("Checkmates: {}", res.checkmates);
        println!("En Passants: {}", res.enpassants);
        println!("Castles : {}", res.castles);
        println!("Promotions: {}", res.promotions);
        assert_eq!(res.nodes, 2812);
        assert_eq!(res.captures, 3348);
        assert_eq!(res.enpassants, 123);
        assert_eq!(res.castles, 0);
        assert_eq!(res.promotions, 0);
        assert_eq!(res.checks, 1680);
        assert_eq!(res.checkmates, 17);
    }
}
