pub use crate::board::*;
use crate::shared_hashmap::SharedHashMap;
use std::cmp::max;
use std::fmt;
use std::ops::{Add, AddAssign, Neg, Sub};
use std::sync::{mpsc, Arc};
use std::time::{Duration, Instant};

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy, Default)]
pub struct Evaluation(pub i64);

#[derive(Default)]
pub struct TTEntry {
    eval: Evaluation,
    is_exact: bool,
    is_upper: bool,
    is_lower: bool,
    depth: u16,
    best_move: Option<Move>,
}

const PIECE_VALUES: [Evaluation; 6] = [
    Evaluation(100),   // Pawn
    Evaluation(500),   // Rook
    Evaluation(300),   // Knight
    Evaluation(310),   // Bishop
    Evaluation(900),   // Queen
    Evaluation(20000), // King
];

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
    fn _m2() -> Evaluation {
        Evaluation(std::i64::MAX - 2)
    }
    fn _m3() -> Evaluation {
        Evaluation(std::i64::MAX - 3)
    }
    fn _m4() -> Evaluation {
        Evaluation(std::i64::MAX - 4)
    }
    fn _m5() -> Evaluation {
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
            Some((self.0 - Self::lost().0) as usize)
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

impl Add for Evaluation {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Evaluation(self.0 + rhs.0)
    }
}

impl AddAssign for Evaluation {
    fn add_assign(&mut self, rhs: Self) {
        self.0 += rhs.0
    }
}

impl Sub for Evaluation {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        Evaluation(self.0 - rhs.0)
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

pub struct SearchInfo {
    pub depth: u16,
    pub time: Duration,
}

impl Board {
    pub fn search_best_move_for(
        &mut self,
        time: Duration,
        queue: &threadpool::ThreadPool,
    ) -> (Option<Move>, Evaluation, SearchInfo) {
        let start = Instant::now();
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (mut m, mut eval) = self.best_move(1, &queue, cache.clone());
        let mut depth = 2;
        while (Instant::now() - start) < time {
            (m, eval) = self.best_move(depth, &queue, cache.clone());
            depth += 1;
        }
        let info = SearchInfo {
            depth,
            time: Instant::now() - start,
        };
        (m, eval, info)
    }

    pub fn best_move<const N: usize>(
        &mut self,
        depth: u16,
        queue: &threadpool::ThreadPool,
        cache: Arc<SharedHashMap<TTEntry, N>>,
    ) -> (Option<Move>, Evaluation) {
        let moves = generate_pseudo_legal_moves(self);
        let mut had_legal_move = false;
        let target_depth = self.half_move + depth;

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
                let cloned = cache.clone();
                queue.execute(move || {
                    let best_score = Evaluation::lost();
                    let eval = -new_b
                        .alpha_beta(
                            Evaluation::lost(),
                            -best_score.inc_mate(),
                            target_depth,
                            cloned.as_ref(),
                        )
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
            // The most material this could swing is capturing a queen
            let mut big_change = PIECE_VALUES[Piece::Queen as usize];
            // While possibly promoting
            if let Some(promote) = capture.promotion {
                big_change += PIECE_VALUES[promote as usize];
            }
            // If we're so far down that this doesn't help, don't bother searching
            if stand_pat + big_change < alpha {
                continue;
            }

            self.make_move(&capture);

            let value = PIECE_VALUES[capture.capture.unwrap() as usize]
                - self.static_exchange_evaluation(capture.to, self.to_play);

            if value >= Evaluation::draw() && !self.in_check(!self.to_play) {
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

    pub fn alpha_beta<const N: usize>(
        &mut self,
        alpha: Evaluation,
        beta: Evaluation,
        target_depth: u16,
        cache: &SharedHashMap<TTEntry, N>,
    ) -> Evaluation {
        let mut best_move = None;
        let cached_val = cache.get(self.hash);
        if let Some(entry) = cached_val {
            // We can use this cache entry if:
            // - The node is deep enough
            // - The entry is exact, or the upper bound <= alpha or lowerbound >= beta
            if entry.depth >= self.half_move
                && (entry.is_exact
                    || (entry.is_upper && entry.eval < alpha)
                    || (entry.is_lower && entry.eval >= beta))
            {
                return entry.eval;
            }
            // If not, if the entry has a best move, start with it and hope that it gives us a nice
            // alpha to start with that should cause lots of cut offs.
            best_move = entry.best_move;
        }
        // If we've got deep enough, run a quiesence search to reduce horizon effects. We don't
        // want to compute taking a pawn with our queen and just stop computing there, for example.
        if self.half_move >= target_depth {
            return self.quiesce(alpha, beta);
        }
        let mut alpha = alpha;
        let mut had_legal_move = false;
        let mut moves = match best_move {
            Some(m) => vec![m],
            None => vec![],
        };
        fill_pseudo_legal_moves(&mut moves, self);
        let mut is_pv_node = false;
        for m in &moves {
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                had_legal_move = true;
                let eval = -self
                    .alpha_beta(-beta, -alpha.inc_mate(), target_depth, cache)
                    .dec_mate();

                if eval >= beta {
                    self.undo_move(&m);
                    if cached_val.is_none() {
                        cache.insert(
                            self.hash,
                            TTEntry {
                                eval: beta,
                                is_exact: false,
                                is_upper: true,
                                is_lower: false,
                                depth: self.half_move,
                                best_move: None,
                            },
                        );
                    }
                    return beta;
                }
                if eval > alpha {
                    is_pv_node = true;
                    alpha = eval;
                }
            }
            self.undo_move(&m);
        }
        let eval = if had_legal_move {
            alpha
        } else {
            // We have no legal moves. If we are in check, it's checkmate. If not, it's stalemate
            if self.in_check(self.to_play) {
                Evaluation::lost()
            } else {
                Evaluation::draw()
            }
        };
        if cached_val.is_none() {
            cache.insert(
                self.hash,
                TTEntry {
                    eval,
                    is_exact: is_pv_node,
                    is_upper: false,
                    is_lower: !is_pv_node,
                    depth: self.half_move,
                    best_move: None,
                },
            );
        } else if let Some(entry) = cached_val {

        }

        eval
    }

    pub fn eval(&self, to_play: Color) -> Evaluation {
        // Check for 3 fold repetition
        if let Some(irr) = self.last_irreversible.last() {
            if self.half_move - irr >= 12 {
                for prev_state in &self.moves[*irr as usize..] {
                    if *prev_state == self.hash {
                        return Evaluation::draw();
                    }
                }
            }
        }
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

        let materialistic = PIECE_VALUES[Piece::King as usize].0 * king_diff
            + PIECE_VALUES[Piece::Queen as usize].0 * queen_diff
            + PIECE_VALUES[Piece::Rook as usize].0 * rook_diff
            + PIECE_VALUES[Piece::Bishop as usize].0 * bishop_diff
            + PIECE_VALUES[Piece::Knight as usize].0 * knight_diff
            + PIECE_VALUES[Piece::Pawn as usize].0 * pawn_diff
            + attacks_diff as i64;
        match to_play {
            Color::Black => -Evaluation(materialistic),
            Color::White => Evaluation(materialistic),
        }
    }

    pub fn get_smallest_attacker(&self, p: Posn, side: Color) -> Option<Move> {
        self.pawn_can_capture(side, p)
            .or(self.knight_can_capture(side, p))
            .or(self.bishop_can_capture(side, p))
            .or(self.rook_can_capture(side, p))
            .or(self.queen_can_capture(side, p))
            .or(self.king_can_capture(side, p))
    }

    pub fn static_exchange_evaluation(&mut self, p: Posn, side: Color) -> Evaluation {
        let mut value = Evaluation::draw();
        if let Some(m) = self.get_smallest_attacker(p, side) {
            self.make_move(&m);
            /* Do not consider captures if they lose material, therefor max zero */
            value = max(
                Evaluation::draw(),
                PIECE_VALUES[m.capture.unwrap() as usize]
                    - self.static_exchange_evaluation(p, !side),
            );
            self.undo_move(&m);
        }
        value
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
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, _) = b.best_move(4, &pool, cache.clone());
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
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, _) = b.best_move(4, &pool, cache.clone());
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
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
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
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::won());
    }
    #[test]
    fn lost() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::lost());
    }
    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("k7/2Q5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::draw());
    }
    #[test]
    fn draw() {
        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (_, eval) = b.best_move(4, &pool, cache.clone());
        assert_eq!(eval, Evaluation::draw());

        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (_, eval) = b.best_move(4, &pool, cache.clone());
        assert_eq!(eval, Evaluation::draw());
    }
    #[test]
    fn mates() {
        let mut b =
            Board::from_fen("1k6/pppr4/8/8/8/8/8/K6R w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::_m3());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
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
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        println!("Eval: {}", eval);
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::lost());

        let mut b =
            Board::from_fen("k6N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        println!("Eval: {}", eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(eval, Evaluation::m1());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
        println!("Eval: {}", eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        assert_eq!(best_move.piece, Piece::King);
        assert_eq!(best_move.from, b8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, -Evaluation::_m2());
    }
    #[test]
    fn bishop_knight_mate() {
        let mut b =
            Board::from_fen("8/8/8/1B6/5N2/6K1/8/6k1 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone());
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
        assert_eq!("M2", format!("{}", Evaluation::_m2()));
        assert_eq!("-M2", format!("{}", -Evaluation::_m2()));
    }
    #[test]
    fn many_moves() {
        let mut board = Board::from_fen(
            "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
        )
        .expect("Invalid fen?");
        let pool = threadpool::ThreadPool::new(32);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        board.best_move(4, &pool, cache.clone());
    }

    #[test]
    fn many_moves2() {
        let mut board =
            Board::from_fen("rn2k2r/1b1p1p2/p2ppn2/1p1P3p/2P3q1/1PNBP3/P3R1PP/R4Q1K b Qkq - 0 1")
                .expect("Invalid fen?");
        let pool = threadpool::ThreadPool::new(64);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        board.best_move(4, &pool, cache.clone());
    }

    #[test]
    fn see_simple() {
        let mut board = Board::from_fen("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1")
            .expect("Invalid fen?");
        assert_eq!(
            Evaluation(100),
            board.static_exchange_evaluation(e5(), Color::White)
        );
    }

    #[test]
    fn see_med() {
        let mut board = Board::from_fen("1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1")
            .expect("Invalid fen?");

        board.make_move(&Move {
            from: d3(),
            to: e5(),
            piece: Piece::Knight,
            capture: Some(Piece::Pawn),
            promotion: None,
            is_check: false,
            is_mate: false,
            is_en_passant: false,
            is_castle_queen: false,
            is_castle_king: false,
        });
        assert_eq!(
            Evaluation(-200),
            PIECE_VALUES[Piece::Pawn as usize]
                - board.static_exchange_evaluation(e5(), Color::Black)
        );
    }

    #[test]
    fn eval_bug1() {
        let mut board =
            Board::from_fen("r1b1k1nr/pp1p3p/1qnpp3/5pp1/2PP4/2N1P3/PPQ2PPP/R3KBNR b KQkq - 2 8")
                .expect("Invalid fen?");
        board.make_move(&Move {
            from: b6(),
            to: b2(),
            piece: Piece::Queen,
            capture: Some(Piece::Pawn),
            promotion: None,
            is_check: false,
            is_mate: false,
            is_en_passant: false,
            is_castle_queen: false,
            is_castle_king: false,
        });
        let best_score = Evaluation::lost();
        let cache = SharedHashMap::new();
        let eval = -board.alpha_beta::<1024>(Evaluation::lost(), -best_score.inc_mate(), 4, &cache);
        println!("Eval: {}", eval);
        assert!(eval < Evaluation::draw());
    }
    #[test]
    fn eval_bug2() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
            .expect("bad fen?");
        board
            .make_alg_move(&AlgebraicMove {
                from: d2(),
                to: d4(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c7(),
                to: c5(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: d4(),
                to: c5(),
                promotion: None,
            })
            .expect("bad move?");

        let pool = threadpool::ThreadPool::new(32);
        let cache: Arc<SharedHashMap<TTEntry, 1024>> = Arc::new(SharedHashMap::new());
        let (m, _) = board.best_move(4, &pool, cache.clone());
        assert!(m.unwrap().to != c5());
    }
    #[test]
    fn repitition() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
            .expect("bad fen?");
        board
            .make_alg_move(&AlgebraicMove {
                from: b1(),
                to: c3(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: b8(),
                to: c6(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c3(),
                to: b1(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c6(),
                to: b8(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: b1(),
                to: c3(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: b8(),
                to: c6(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c3(),
                to: b1(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c6(),
                to: b8(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: b1(),
                to: c3(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: b8(),
                to: c6(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c3(),
                to: b1(),
                promotion: None,
            })
            .expect("bad move?");
        board
            .make_alg_move(&AlgebraicMove {
                from: c6(),
                to: b8(),
                promotion: None,
            })
            .expect("bad move?");

        let evalw = board.eval(Color::White);
        assert!(evalw == Evaluation::draw());
        let evalb = board.eval(Color::Black);
        assert!(evalb == Evaluation::draw());
    }

    #[test]
    fn check_hashing() {
        let mut board = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
            .expect("bad fen?");
        let starting_hash = board.hash;
        board
            .make_alg_move(&AlgebraicMove {
                from: b1(),
                to: c3(),
                promotion: None,
            })
            .expect("bad move?");
        assert_ne!(board.hash, starting_hash);
        board
            .make_alg_move(&AlgebraicMove {
                from: b8(),
                to: c6(),
                promotion: None,
            })
            .expect("bad move?");
        assert_ne!(board.hash, starting_hash);
        board
            .make_alg_move(&AlgebraicMove {
                from: c3(),
                to: b1(),
                promotion: None,
            })
            .expect("bad move?");
        assert_ne!(board.hash, starting_hash);
        board
            .make_alg_move(&AlgebraicMove {
                from: c6(),
                to: b8(),
                promotion: None,
            })
            .expect("bad move?");
        assert_eq!(board.hash, starting_hash);
    }
}
