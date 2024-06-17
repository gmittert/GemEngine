pub use crate::board::*;
use std::fmt;
use std::ops::Neg;
use std::sync::mpsc;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy)]
pub struct Evaluation(pub i64);

// An evaluation is simply an i64 with a few caveats:
// - We limit the range to (std::i64::MIN, std::i64::MAX] to not run into negation errors
// - We treat i64::MAX as having won the game, i64::MAX - 1 as mate in 1, i64::MAX -2 as mate in
//   2 and so on.
// - We treat i64::MIN as having lost the game, i64::MIN + 1 as the opponent having mate in 1,
//   i64::MAX -2 as the opponent having mate in 2 and so on.
// - Everything else is an evalutation in centipawns
impl Evaluation {
    fn won() -> Evaluation {
        Evaluation(std::i64::MAX)
    }
    fn draw() -> Evaluation {
        Evaluation(0)
    }
    fn lost() -> Evaluation {
        Evaluation(std::i64::MIN + 1)
    }
    fn m1() -> Evaluation {
        Evaluation(std::i64::MAX - 1)
    }
    fn m2() -> Evaluation {
        Evaluation(std::i64::MAX - 2)
    }
    fn m3() -> Evaluation {
        Evaluation(std::i64::MAX - 3)
    }
    fn m4() -> Evaluation {
        Evaluation(std::i64::MAX - 4)
    }
    fn m5() -> Evaluation {
        Evaluation(std::i64::MAX - 5)
    }
    pub fn mate_in(&self) -> Option<usize> {
        if self.0 >= Self::won().0 - 100 {
            Some(Self::won().0 as usize - 100)
        } else {
            None
        }
    }
    pub fn mated_in(&self) -> Option<usize> {
        if self.0 <= Self::lost().0 + 100 {
            Some((Self::lost().0 - self.0) as usize)
        } else {
            None
        }
    }
    fn dec_mate(&self) -> Evaluation {
        if self.0 >= Self::won().0 - 100 {
            Evaluation(self.0 - 1)
        } else if self.0 <= Self::lost().0 + 100 {
            Evaluation(self.0 + 1)
        } else {
            *self
        }
    }
    fn inc_mate(&self) -> Evaluation {
        if (self.0 >= Self::won().0 - 100) && *self != Self::won() {
            Evaluation(self.0 + 1)
        } else if self.0 <= Self::lost().0 + 100 && *self != Self::lost() {
            Evaluation(self.0 - 1)
        } else {
            *self
        }
    }
}

impl Neg for Evaluation {
    type Output = Self;

    fn neg(self) -> Self::Output {
        Evaluation(-self.0)
    }
}

impl fmt::Display for Evaluation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if *self == Evaluation::won() {
            write!(f, "Won")?;
        } else if self.0 >= (Evaluation::m1().0 - 100) {
            write!(f, "M{}", Evaluation::m1().0 - self.0 + 1)?;
        } else if *self == Evaluation::lost() {
            write!(f, "Lost")?;
        } else if self.0 <= (-Evaluation::m1().0 + 100) {
            write!(f, "-M{}", self.0 + Evaluation::m1().0 + 1)?;
        } else {
            write!(f, "{}", self.0 as f64 / 100.0)?;
        }
        Ok(())
    }
}

impl Board {
    pub fn best_move(&mut self, queue: &threadpool::ThreadPool) -> (Option<Move>, Evaluation) {
        let moves = generate_pseudo_legal_moves(self);
        let mut had_legal_move = false;
        const DEFAULT_DEPTH: usize = 4;

        let (tx, rx) = mpsc::channel();
        for m in &moves {
            if Some(Piece::King) == m.capture {
                return (None, Evaluation::won());
            }
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                had_legal_move = true;
                let tx = tx.clone();
                let mut new_b = self.clone();
                let m = *m;
                queue.execute(move || {
                    let best_score = Evaluation::lost();
                    let eval = -new_b
                        .alpha_beta(Evaluation::lost(), -best_score.inc_mate(), DEFAULT_DEPTH)
                        .dec_mate();

                    let _ = tx.send((eval, m));
                });
            }
            self.undo_move(&m);
        }
        drop(tx);
        if had_legal_move {
            let mut best_move = None;
            let mut best_score = Evaluation::lost();
            while let Ok((eval, m)) = rx.recv() {
                if eval > best_score {
                    best_move = Some(m);
                    best_score = eval;
                }
            }
            (best_move, best_score)
        } else {
            // We have no legal moves. If we are in check, it's checkmate. If not, it's stalemate
            if self.in_check(self.to_play) {
                (None, Evaluation::lost())
            } else {
                (None, Evaluation::draw())
            }
        }
    }

    pub fn quiesce(&mut self, alpha: Evaluation, beta: Evaluation) -> Evaluation {
        let mut alpha = alpha;
        let stand_pat = self.eval(self.to_play);
        if stand_pat >= beta {
            return beta;
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }
        let captures = generate_pseudo_legal_moves(self)
            .into_iter()
            .filter(|x| x.capture.is_some());
        for capture in captures {
            self.make_move(&capture);
            if !self.in_check(!self.to_play) {
                let eval = -self.quiesce(-beta, -alpha.inc_mate()).dec_mate();
                if eval >= beta {
                    self.undo_move(&capture);
                    return beta;
                }
                if eval > alpha {
                    alpha = eval;
                }
            }
            self.undo_move(&capture);
        }
        alpha
    }

    pub fn alpha_beta(&mut self, alpha: Evaluation, beta: Evaluation, depth: usize) -> Evaluation {
        if depth == 0 {
            return self.quiesce(alpha, beta);
        }
        let mut alpha = alpha;
        let mut had_legal_move = false;
        let moves = generate_pseudo_legal_moves(self);
        for m in &moves {
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                had_legal_move = true;
                let eval = -self
                    .alpha_beta(-beta, -alpha.inc_mate(), depth - 1)
                    .dec_mate();
                if eval >= beta {
                    self.undo_move(&m);
                    return beta;
                }
                if eval > alpha {
                    alpha = eval;
                }
            }
            self.undo_move(&m);
        }
        if had_legal_move {
            alpha
        } else {
            // We have no legal moves. If we are in check, it's checkmate. If not, it's stalemate
            if self.in_check(self.to_play) {
                Evaluation::lost()
            } else {
                Evaluation::draw()
            }
        }
    }

    pub fn eval(&self, to_play: Color) -> Evaluation {
        // Let's start with a simple materialistic evaluation
        let king_diff: i64 = self.white_pieces[Piece::King as usize].len() as i64
            - self.black_pieces[Piece::King as usize].len() as i64;
        let queen_diff: i64 = self.white_pieces[Piece::Queen as usize].len() as i64
            - self.black_pieces[Piece::Queen as usize].len() as i64;
        let rook_diff: i64 = self.white_pieces[Piece::Rook as usize].len() as i64
            - self.black_pieces[Piece::Rook as usize].len() as i64;
        let bishop_diff: i64 = self.white_pieces[Piece::Bishop as usize].len() as i64
            - self.black_pieces[Piece::Bishop as usize].len() as i64;
        let knight_diff: i64 = self.white_pieces[Piece::Knight as usize].len() as i64
            - self.black_pieces[Piece::Knight as usize].len() as i64;
        let pawn_diff: i64 = self.white_pieces[Piece::Pawn as usize].len() as i64
            - self.black_pieces[Piece::Pawn as usize].len() as i64;

        /*
        let attacks_white = self.rook_attacks(Color::White)
            | self.queen_attacks(Color::White)
            | self.king_attacks(Color::White)
            | self.pawn_attacks(Color::White)
            | self.knight_attacks(Color::White);

        let attacks_black = self.rook_attacks(Color::Black)
            | self.queen_attacks(Color::Black)
            | self.king_attacks(Color::Black)
            | self.pawn_attacks(Color::Black)
            | self.knight_attacks(Color::Black);

        let attacks_diff = attacks_white.len() as i64 - attacks_black.len() as i64;
        */
        let attacks_diff = 0;

        let materialistic = 20000 * king_diff
            + 900 * queen_diff
            + 500 * rook_diff
            + 310 * bishop_diff
            + 300 * knight_diff
            + 100 * pawn_diff
            + attacks_diff as i64;
        match to_play {
            Color::Black => -Evaluation(materialistic),
            Color::White => Evaluation(materialistic),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board::evaluation::*;
    #[test]
    fn white_better() {
        let b = Board::from_fen("rkb1kbkr/pppppppp/8/8/8/8/PPPPPPPP/RKBQKBKR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white.0 > 0);
        assert!(eval_black.0 < 0);
        assert!(eval_black == -eval_white)
    }
    #[test]
    fn black_better() {
        let b = Board::from_fen("rkbqkbkr/pppppppp/8/8/8/8/PPPPPPPP/RKB1KBKR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white.0 < 0);
        assert!(eval_black.0 > 0);
        assert!(eval_black == -eval_white)
    }
    #[test]
    fn find_queen_take() {
        let mut b = Board::from_fen("4k3/pppppppp/8/8/7q/8/PPPPPPP1/RKBQKBKR w - - 0 1")
            .expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, _) = b.best_move(&pool);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h4());
        assert_eq!(best_move.capture, Some(Piece::Queen));
    }
    #[test]
    fn take_back_trade() {
        let mut b = Board::from_fen("rk1qkbkr/ppp2ppp/3pB3/4p3/4P3/5K2/PPPP1PPP/RKBQK2R b - - 0 1")
            .expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, _) = b.best_move(&pool);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Pawn);
        assert_eq!(best_move.from, f7());
        assert_eq!(best_move.to, e6());
        assert_eq!(best_move.capture, Some(Piece::Bishop));
    }
    #[test]
    fn m1() {
        let mut b =
            Board::from_fen("1k6/ppp5/8/8/8/8/8/K6R w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, eval) = b.best_move(&pool);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m1());
    }
    #[test]
    fn won() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, eval) = b.best_move(&pool);
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::won());
    }
    #[test]
    fn lost() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, eval) = b.best_move(&pool);
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::lost());
    }
    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("k7/2Q5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, eval) = b.best_move(&pool);
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::draw());
    }
    #[test]
    fn draw() {
        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (_, eval) = b.best_move(&pool);
        assert_eq!(eval, Evaluation::draw());

        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (_, eval) = b.best_move(&pool);
        assert_eq!(eval, Evaluation::draw());
    }
    #[test]
    fn mates() {
        let mut b =
            Board::from_fen("1k6/pppr4/8/8/8/8/8/K6R w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, eval) = b.best_move(&pool);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m3());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let (best_move, eval) = b.best_move(&pool);
        println!("Eval: {}", eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m1());

        let mut b = Board::from_fen("k5RN/7R/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let (best_move, eval) = b.best_move(&pool);
        println!("Eval: {}", eval);
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::lost());

        let mut b =
            Board::from_fen("k6N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let (best_move, eval) = b.best_move(&pool);
        println!("Eval: {}", eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(eval, Evaluation::m1());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let (best_move, eval) = b.best_move(&pool);
        println!("Eval: {}", eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        assert_eq!(best_move.piece, Piece::King);
        assert_eq!(best_move.from, b8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, -Evaluation::m2());
    }
    #[test]
    fn bishop_knight_mate() {
        let mut b =
            Board::from_fen("8/8/8/1B6/5N2/6K1/8/6k1 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let (best_move, eval) = b.best_move(&pool);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert!(eval.mate_in().is_some());
    }
    #[test]
    fn eval_flipped() {
        assert_eq!(Evaluation::won(), -Evaluation::lost());
        assert_eq!(Evaluation::lost(), -Evaluation::won());
        assert_eq!(Evaluation::won().dec_mate(), Evaluation::m1());
        assert_eq!(Evaluation::lost().dec_mate(), -Evaluation::m1());
        assert_eq!(-Evaluation::won().dec_mate(), -Evaluation::m1());
        assert_eq!(-Evaluation::lost().dec_mate(), Evaluation::m1());
    }
    #[test]
    fn eval_formatted() {
        assert_eq!("M1", format!("{}", Evaluation::m1()));
        assert_eq!("-M1", format!("{}", -Evaluation::m1()));
        assert_eq!("M2", format!("{}", Evaluation::m2()));
        assert_eq!("-M2", format!("{}", -Evaluation::m2()));
    }
}
