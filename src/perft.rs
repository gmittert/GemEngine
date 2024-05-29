use crate::board;

#[derive(Debug, PartialEq)]
pub struct PerfResult {
    nodes: usize,
    captures: usize,
    checks: usize,
    checkmates: usize,
    enpassants: usize,
}

pub fn perft(b: &mut board::Board, depth: u8) -> PerfResult {
    let mut result = PerfResult {
        nodes: 0,
        captures: 0,
        checks: 0,
        checkmates: 0,
        enpassants: 0,
    };
    if depth == 0 {
        result.nodes = 1;

        if b.in_check(b.to_play) {
            result.checks += 1;
            if perft(b, 1).nodes == 0 {
                result.checkmates += 1;
            }
        }
        if let Some(last_move) = b.move_list.last() {
            if last_move.capture.is_some() {
                result.captures = 1
            }
            if last_move.is_en_passant {
                result.enpassants = 1
            }
        }
        return result;
    }

    let moves = board::generate_pseudo_legal_moves(b);

    for m in &moves {
        let preb = b.black_pieces();
        let prew = b.white_pieces();
        b.make_move(&m);
        if !b.in_check(!b.to_play) {
            let next_res = perft(b, depth - 1);
            result.nodes += next_res.nodes;
            result.captures += next_res.captures;
            result.checks += next_res.checks;
            result.checkmates += next_res.checkmates;
            result.enpassants += next_res.enpassants;
        }
        b.undo_move(&m);
        let postb = b.black_pieces();
        let postw = b.white_pieces();
        assert_eq!(preb, postb);
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
        assert_eq!(res.nodes, 119060324);
        assert_eq!(res.checks, 809099);
        assert_eq!(res.captures, 2812008);
        assert_eq!(res.checkmates, 10828);
        assert_eq!(res.enpassants, 5248);
    }
}
