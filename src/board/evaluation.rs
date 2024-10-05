use tracing::{field, trace_span, Level};

pub use crate::board::*;
use crate::shared_hashmap::SharedHashMap;
use crate::transposition_table::{NodeType, PackedTTEntry, TranspositionTable};
use std::cmp::max;
use std::fmt;
use std::ops::{Add, AddAssign, Neg, Sub};
use std::sync::{atomic::AtomicBool, atomic::Ordering, mpsc, Arc};
use std::thread::sleep;
use std::time::{Duration, Instant};

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy, Default)]
pub struct Evaluation(pub i16);

pub struct EvalResult {
    eval: Evaluation,
    seldepth: u16,
    nodes: usize,
}

const PIECE_VALUES: [Evaluation; 6] = [
    Evaluation(100),   // Pawn
    Evaluation(500),   // Rook
    Evaluation(300),   // Knight
    Evaluation(310),   // Bishop
    Evaluation(900),   // Queen
    Evaluation(10000), // King
];

// Piece square tables
/* values from Rofchade: http://www.talkchess.com/forum3/viewtopic.php?f=2&t=68311&start=19 */
const MG_VALUE: [i16; 6] = [82, 477, 337, 365, 1025, 0];
const EG_VALUE: [i16; 6] = [94, 512, 281, 297, 936, 0];

#[rustfmt::skip]
const MG_PAWN_TABLE: [i16; 64] = [
      0,   0,   0,   0,  0,    0,  0,   0,
     98, 134,  61,  95,  68, 126, 34, -11,
     -6,   7,  26,  31,  65,  56, 25, -20,
    -14,  13,   6,  21,  23,  12, 17, -23,
    -27,  -2,  -5,  12,  17,   6, 10, -25,
    -26,  -4,  -4, -10,   3,   3, 33, -12,
    -35,  -1, -20, -23, -15,  24, 38, -22,
      0,   0,   0,   0,   0,   0,  0,   0,
];

#[rustfmt::skip]
const EG_PAWN_TABLE: [i16; 64] = [
      0,   0,   0,   0,   0,   0,   0,   0, 
    178, 173, 158, 134, 147, 132, 165, 187,
     94, 100,  85,  67,  56,  53,  82,  84,
     32,  24,  13,   5,  -2,   4,  17,  17,
     13,   9,  -3,  -7,  -7,  -8,   3,  -1,
      4,   7,  -6,   1,   0,  -5,  -1,  -8,
     13,   8,   8,  10,  13,   0,   2,  -7,
      0,   0,   0,   0,   0,   0,   0,   0,
];

const MG_KNIGHT_TABLE: [i16; 64] = [
    -167, -89, -34, -49, 61, -97, -15, -107, -73, -41, 72, 36, 23, 62, 7, -17, -47, 60, 37, 65, 84,
    129, 73, 44, -9, 17, 19, 53, 37, 69, 18, 22, -13, 4, 16, 13, 28, 19, 21, -8, -23, -9, 12, 10,
    19, 17, 25, -16, -29, -53, -12, -3, -1, 18, -14, -19, -105, -21, -58, -33, -17, -28, -19, -23,
];

const EG_KNIGHT_TABLE: [i16; 64] = [
    -58, -38, -13, -28, -31, -27, -63, -99, -25, -8, -25, -2, -9, -25, -24, -52, -24, -20, 10, 9,
    -1, -9, -19, -41, -17, 3, 22, 22, 22, 11, 8, -18, -18, -6, 16, 25, 16, 17, 4, -18, -23, -3, -1,
    15, 10, -3, -20, -22, -42, -20, -10, -5, -2, -20, -23, -44, -29, -51, -23, -15, -22, -18, -50,
    -64,
];

const MG_BISHOP_TABLE: [i16; 64] = [
    -29, 4, -82, -37, -25, -42, 7, -8, -26, 16, -18, -13, 30, 59, 18, -47, -16, 37, 43, 40, 35, 50,
    37, -2, -4, 5, 19, 50, 37, 37, 7, -2, -6, 13, 13, 26, 34, 12, 10, 4, 0, 15, 15, 15, 14, 27, 18,
    10, 4, 15, 16, 0, 7, 21, 33, 1, -33, -3, -14, -21, -13, -12, -39, -21,
];

const EG_BISHOP_TABLE: [i16; 64] = [
    -14, -21, -11, -8, -7, -9, -17, -24, -8, -4, 7, -12, -3, -13, -4, -14, 2, -8, 0, -1, -2, 6, 0,
    4, -3, 9, 12, 9, 14, 10, 3, 2, -6, 3, 13, 19, 7, 10, -3, -9, -12, -3, 8, 10, 13, 3, -7, -15,
    -14, -18, -7, -1, 4, -9, -15, -27, -23, -9, -23, -5, -9, -16, -5, -17,
];

const MG_ROOK_TABLE: [i16; 64] = [
    32, 42, 32, 51, 63, 9, 31, 43, 27, 32, 58, 62, 80, 67, 26, 44, -5, 19, 26, 36, 17, 45, 61, 16,
    -24, -11, 7, 26, 24, 35, -8, -20, -36, -26, -12, -1, 9, -7, 6, -23, -45, -25, -16, -17, 3, 0,
    -5, -33, -44, -16, -20, -9, -1, 11, -6, -71, -19, -13, 1, 17, 16, 7, -37, -26,
];

const EG_ROOK_TABLE: [i16; 64] = [
    13, 10, 18, 15, 12, 12, 8, 5, 11, 13, 13, 11, -3, 3, 8, 3, 7, 7, 7, 5, 4, -3, -5, -3, 4, 3, 13,
    1, 2, 1, -1, 2, 3, 5, 8, 4, -5, -6, -8, -11, -4, 0, -5, -1, -7, -12, -8, -16, -6, -6, 0, 2, -9,
    -9, -11, -3, -9, 2, 3, -1, -5, -13, 4, -20,
];

const MG_QUEEN_TABLE: [i16; 64] = [
    -28, 0, 29, 12, 59, 44, 43, 45, -24, -39, -5, 1, -16, 57, 28, 54, -13, -17, 7, 8, 29, 56, 47,
    57, -27, -27, -16, -16, -1, 17, -2, 1, -9, -26, -9, -10, -2, -4, 3, -3, -14, 2, -11, -2, -5, 2,
    14, 5, -35, -8, 11, 2, 8, 15, -3, 1, -1, -18, -9, 10, -15, -25, -31, -50,
];

const EG_QUEEN_TABLE: [i16; 64] = [
    -9, 22, 22, 27, 27, 19, 10, 20, -17, 20, 32, 41, 58, 25, 30, 0, -20, 6, 9, 49, 47, 35, 19, 9,
    3, 22, 24, 45, 57, 40, 57, 36, -18, 28, 19, 47, 31, 34, 39, 23, -16, -27, 15, 6, 9, 17, 10, 5,
    -22, -23, -30, -16, -16, -23, -36, -32, -33, -28, -22, -43, -5, -32, -20, -41,
];

const MG_KING_TABLE: [i16; 64] = [
    -65, 23, 16, -15, -56, -34, 2, 13, 29, -1, -20, -7, -8, -4, -38, -29, -9, 24, 2, -16, -20, 6,
    22, -22, -17, -20, -12, -27, -30, -25, -14, -36, -49, -1, -27, -39, -46, -44, -33, -51, -14,
    -14, -22, -46, -44, -30, -15, -27, 1, 7, -8, -64, -43, -16, 9, 8, -15, 36, 12, -54, 8, -28, 24,
    14,
];

const EG_KING_TABLE: [i16; 64] = [
    -74, -35, -18, -18, -11, 15, 4, -17, -12, 17, 14, 17, 17, 38, 23, 11, 10, 17, 23, 15, 20, 45,
    44, 13, -8, 22, 24, 27, 26, 33, 26, 3, -18, -4, 21, 24, 27, 23, 9, -11, -19, -3, 11, 21, 23,
    16, 7, -9, -27, -11, 4, 13, 14, 4, -5, -17, -53, -34, -21, -11, -28, -14, -24, -43,
];

const MG_TABLES: [[i16; 64]; 6] = [
    MG_PAWN_TABLE,
    MG_ROOK_TABLE,
    MG_KNIGHT_TABLE,
    MG_BISHOP_TABLE,
    MG_QUEEN_TABLE,
    MG_KING_TABLE,
];

const EG_TABLES: [[i16; 64]; 6] = [
    EG_PAWN_TABLE,
    EG_ROOK_TABLE,
    EG_KNIGHT_TABLE,
    EG_BISHOP_TABLE,
    EG_QUEEN_TABLE,
    EG_KING_TABLE,
];

const fn flip(i: usize) -> usize {
    i ^ 56
}
pub const GAME_PHASE_INC: [u8; 6] = [0, 2, 1, 1, 4, 0];
pub const MG_TABLE: [[[i16; 64]; 6]; 2] = {
    let mut arr = [[[0; 64]; 6]; 2];
    let mut piece = 0;
    while piece < 6 {
        let mut pos = 0;
        while pos < 64 {
            arr[0][piece][pos] = MG_VALUE[piece] + MG_TABLES[piece][flip(63 - pos)];
            arr[1][piece][pos] = MG_VALUE[piece] + MG_TABLES[piece][63 - pos];
            pos += 1;
        }
        piece += 1;
    }
    arr
};
pub const EG_TABLE: [[[i16; 64]; 6]; 2] = {
    let mut arr = [[[0; 64]; 6]; 2];
    let mut piece = 0;
    while piece < 6 {
        let mut pos = 0;
        while pos < 64 {
            arr[0][piece][pos] = EG_VALUE[piece] + EG_TABLES[piece][flip(63 - pos)];
            arr[1][piece][pos] = EG_VALUE[piece] + EG_TABLES[piece][63 - pos];
            pos += 1;
        }
        piece += 1;
    }
    arr
};

// An evaluation is simply an i64 with a few caveats:
// - We limit the range to (std::i64::MIN, std::i64::MAX] to not run into negation errors
// - We treat i64::MAX as having won the game, i64::MAX - 1 as mate in 1, i64::MAX -2 as mate in
//   2 and so on.
// - We treat i64::MIN as having lost the game, i64::MIN + 1 as the opponent having mate in 1,
//   i64::MAX -2 as the opponent having mate in 2 and so on.
// - Everything else is an evalutation in centipawns
impl Evaluation {
    pub fn won() -> Evaluation {
        Evaluation(std::i16::MAX)
    }
    pub fn draw() -> Evaluation {
        Evaluation(0)
    }
    pub fn lost() -> Evaluation {
        Evaluation(std::i16::MIN + 1)
    }
    fn m1() -> Evaluation {
        Evaluation(std::i16::MAX - 1)
    }
    fn _m2() -> Evaluation {
        Evaluation(std::i16::MAX - 2)
    }
    fn _m3() -> Evaluation {
        Evaluation(std::i16::MAX - 3)
    }
    fn _m4() -> Evaluation {
        Evaluation(std::i16::MAX - 4)
    }
    fn _m5() -> Evaluation {
        Evaluation(std::i16::MAX - 5)
    }
    pub fn mate_in(&self) -> Option<usize> {
        if self.0 >= Self::won().0 - 100 {
            Some((Self::won().0 - self.0) as usize)
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
    pub nodes: usize,
    pub nodes_per_sec: usize,
    pub seldepth: u16,
    pub hash_full: usize,
}

impl Board {
    pub fn search_best_move_for(
        &mut self,
        time: Duration,
        queue: &threadpool::ThreadPool,
    ) -> (Option<Move>, Evaluation, SearchInfo) {
        let start = Instant::now();
        let end_time = start + time;
        let cache: TranspositionTable = Arc::new(SharedHashMap::new());
        let (mut m, mut eval) = self.best_move(1, &queue, cache.clone(), None);
        let mut depth = 2;
        loop {
            let now = Instant::now();
            if now >= end_time {
                break;
            }
            let (mp, evalp) = self.best_move(depth, &queue, cache.clone(), Some(end_time - now));
            let now = Instant::now();
            if now >= end_time {
                break;
            }
            m = mp;
            eval = evalp;
            depth += 1;
        }
        let time = Instant::now() - start;
        let info = SearchInfo {
            depth,
            seldepth: eval.seldepth,
            nodes: eval.nodes,
            nodes_per_sec: eval.nodes / time.as_secs() as usize,
            time,
            hash_full: cache.hash_usage(),
        };
        (m, eval.eval, info)
    }

    pub fn it_depth_best_move(
        &mut self,
        target_depth: u16,
        queue: &threadpool::ThreadPool,
    ) -> (Option<Move>, EvalResult) {
        let cache: TranspositionTable = Arc::new(SharedHashMap::new());
        let (mut m, mut eval) = self.best_move(1, &queue, cache.clone(), None);

        for depth in 1..target_depth {
            (m, eval) = self.best_move(depth + 1, &queue, cache.clone(), None);
        }
        (m, eval)
    }

    pub fn best_move<const N: usize>(
        &mut self,
        depth: u16,
        queue: &threadpool::ThreadPool,
        cache: Arc<SharedHashMap<N>>,
        time: Option<Duration>,
    ) -> (Option<Move>, EvalResult) {
        let mut had_legal_move = false;
        let target_depth = self.half_move + depth;
        let should_stop = Arc::new(AtomicBool::new(false));
        if let Some(sleep_for) = time {
            let should_stop = should_stop.clone();
            queue.execute(move || {
                sleep(sleep_for);
                should_stop.store(true, Ordering::Release);
            });
        }
        let mut nodes = 0;
        let mut seldepth = 0;

        let (tx, rx) = mpsc::channel();
        for a in self.pseudo_legal_moves_it() {
            let m = self.from_algeabraic(&a);
            if Some(Piece::King) == m.capture {
                return (
                    None,
                    EvalResult {
                        eval: Evaluation::won(),
                        nodes: 1,
                        seldepth: 1,
                    },
                );
            }
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                had_legal_move = true;
                let tx = tx.clone();
                let mut new_b = self.clone();
                let m = m;
                let cloned = cache.clone();
                let should_stop = should_stop.clone();
                let to_play = self.to_play;

                if let Some(irr) = self.last_irreversible.last() {
                    if self.half_move - irr >= 8 {
                        for prev_state in &self.moves[*irr as usize..] {
                            if *prev_state == self.hash {
                                tracing::event!(Level::ERROR, "Three fold!");
                                let _ = tx.send((
                                    EvalResult {
                                        eval: Evaluation::draw(),
                                        nodes: 1,
                                        seldepth: 1,
                                    },
                                    m,
                                ));
                                continue;
                            }
                        }
                    }
                }

                queue.execute(move || {
                    let best_score = Evaluation::lost();
                    let span = match to_play {
                        Color::Black => trace_span!("white", inspecting = %m, alpha = %Evaluation::lost(), beta = %-best_score.inc_mate(), eval = field::Empty).entered(),
                        Color::White => trace_span!("black", inspecting = %m, alpha = %Evaluation::lost(), beta = %-best_score.inc_mate(), eval = field::Empty).entered(),
                    };
                    // Check for 3 fold repetition
                    let eval_res = new_b
                            .alpha_beta(
                                Evaluation::lost(),
                                -best_score.inc_mate(),
                                target_depth,
                                cloned.as_ref(),
                                should_stop.clone(),
                            );
                    let eval = -eval_res.eval.dec_mate();

                    span.record("eval", format!("{eval}"));
                    drop(span);

                    let _ = tx.send((EvalResult{
                        eval,
                        nodes: eval_res.nodes,
                        seldepth: eval_res.seldepth,
                    }, m));
                });
            }
            self.undo_move(&m);
        }
        drop(tx);
        if had_legal_move {
            let mut best_move = None;
            let mut best_score = Evaluation::lost();
            while let Ok((eval, m)) = rx.recv() {
                nodes += eval.nodes;
                seldepth = max(seldepth, eval.seldepth + 1);
                if eval.eval > best_score {
                    best_move = Some(m);
                    best_score = eval.eval;
                }
            }
            (
                best_move,
                EvalResult {
                    eval: best_score,
                    nodes,
                    seldepth,
                },
            )
        } else {
            // We have no legal moves. If we are in check, it's checkmate. If not, it's stalemate
            if self.in_check(self.to_play) {
                (
                    None,
                    EvalResult {
                        eval: Evaluation::lost(),
                        nodes: 1,
                        seldepth: 1,
                    },
                )
            } else {
                (
                    None,
                    EvalResult {
                        eval: Evaluation::draw(),
                        nodes: 1,
                        seldepth: 1,
                    },
                )
            }
        }
    }

    pub fn quiesce(&mut self, alpha: Evaluation, beta: Evaluation) -> EvalResult {
        let mut alpha = alpha;
        let stand_pat = self.eval(alpha, beta, self.to_play);
        tracing::event!(Level::INFO, stand_pat = %stand_pat);
        if stand_pat >= beta {
            return EvalResult {
                eval: beta,
                seldepth: 1,
                nodes: 1,
            };
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }
        let mut nodes = 0;
        let mut seldepth = 0;
        let opponent_pieces = match self.to_play {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        let captures = self
            .pseudo_legal_moves_it()
            .filter(|x| opponent_pieces.contains(x.to));
        for capture in captures {
            let m = self.from_algeabraic(&capture);
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

            self.make_move(&m);

            let value = PIECE_VALUES[m.capture.unwrap() as usize]
                - self.static_exchange_evaluation(capture.to, self.to_play);

            if value >= Evaluation::draw() && !self.in_check(!self.to_play) {
                let span = match self.to_play {
                    Color::Black => trace_span!("quiesece white", inspecting = %m, alpha = %-beta, beta = %-alpha.inc_mate(), eval = field::Empty).entered(),
                    Color::White => trace_span!("quiesce black", inspecting = %m, alpha = %-beta, beta = %-alpha.inc_mate(), eval = field::Empty).entered(),
                };
                let eval_res = self.quiesce(-beta, -alpha.inc_mate());
                let eval = -eval_res.eval.dec_mate();
                nodes += eval_res.nodes;
                seldepth = max(seldepth, eval_res.seldepth + 1);
                span.record("eval", format!("{eval}"));
                drop(span);
                if eval >= beta {
                    self.undo_move(&m);

                    tracing::event!(Level::INFO, name = "Beta cutoff", "eval" = %eval, "beta" = %beta);
                    return EvalResult {
                        eval: beta,
                        seldepth,
                        nodes,
                    };
                }
                if eval > alpha {
                    tracing::event!(Level::INFO, name = "Raised Alpha!", "alpha" = %alpha, "eval" = %eval);
                    alpha = eval;
                }
            }
            self.undo_move(&m);
        }
        EvalResult {
            eval: alpha,
            seldepth,
            nodes,
        }
    }

    pub fn alpha_beta<const N: usize>(
        &mut self,
        alpha: Evaluation,
        beta: Evaluation,
        target_depth: u16,
        cache: &SharedHashMap<N>,
        should_stop: Arc<AtomicBool>,
    ) -> EvalResult {
        let mut seldepth = 0;
        let mut nodes = 1;

        if should_stop.load(Ordering::Acquire) {
            return EvalResult {
                eval: Evaluation::draw(),
                seldepth: 0,
                nodes: 0,
            };
        }
        let mut best_move = None;
        let cached_val = cache.get(self.hash);
        if let Some(entry) = cached_val {
            let unpacked = PackedTTEntry(entry);
            // We can use this cache entry if:
            // - The node is deep enough
            // - The entry is exact, or the upper bound <= alpha or lowerbound >= beta
            let node_type = unpacked.node_type();
            let eval = unpacked.eval();
            // If not, if the entry has a best move, start with it and hope that it gives us a nice
            // alpha to start with that should cause lots of cut offs.
            best_move = unpacked.best_move();
            if unpacked.depth() >= target_depth
                && (node_type == NodeType::Exact
                    || (node_type == NodeType::Upper && eval < alpha)
                    || (node_type == NodeType::Lower && eval >= beta))
            {
                tracing::event!(Level::INFO, name = "Retrieved from cache", "eval" = %eval);
                return EvalResult {
                    eval,
                    seldepth: 0,
                    nodes: 1,
                };
            }
        }
        // If we've got deep enough, run a quiesence search to reduce horizon effects. We don't
        // want to compute taking a pawn with our queen and just stop computing there, for example.
        if self.half_move >= target_depth {
            let span = trace_span!("quiesece", alpha = %alpha, beta = %beta, eval = field::Empty)
                .entered();
            let eval = self.quiesce(alpha, beta);
            span.record("eval", format!("{}", eval.eval));
            return eval;
        }
        let mut alpha = alpha;
        let mut had_legal_move = false;
        let moves = match best_move {
            Some(m) => vec![m],
            None => vec![],
        }
        .into_iter()
        .chain(self.pseudo_legal_moves_it());
        let mut is_pv_node = false;
        for a in moves {
            let m = self.from_algeabraic(&a);
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                had_legal_move = true;
                let span = match self.to_play {
                    Color::Black => trace_span!("white", inspecting = %m, alpha = %-beta, beta = %-alpha.inc_mate(), eval = field::Empty).entered(),
                    Color::White => trace_span!("black", inspecting = %m, alpha = %-beta, beta = %-alpha.inc_mate(), eval = field::Empty).entered(),
                };
                // Check for 3 fold repetition
                let mut is_three_fold = false;
                if let Some(irr) = self.last_irreversible.last() {
                    if self.half_move - irr >= 8 {
                        for prev_state in &self.moves[*irr as usize..] {
                            if *prev_state == self.hash {
                                tracing::event!(Level::ERROR, "Three fold!");
                                is_three_fold = true;
                                break;
                            }
                        }
                    }
                }
                let eval_res = if is_three_fold {
                    EvalResult {
                        eval: Evaluation::draw(),
                        nodes: 1,
                        seldepth: 1,
                    }
                } else {
                    self.alpha_beta(
                        -beta,
                        -alpha.inc_mate(),
                        target_depth,
                        cache,
                        should_stop.clone(),
                    )
                };
                let eval = -eval_res.eval.dec_mate();
                nodes += eval_res.nodes;
                seldepth = max(seldepth, eval_res.seldepth + 1);
                span.record("eval", format!("{eval}"));
                drop(span);

                if eval >= beta {
                    self.undo_move(&m);
                    let should_cache = if let Some(entry) = cached_val {
                        self.half_move > PackedTTEntry(entry).depth()
                    } else {
                        true
                    };

                    if should_cache {
                        cache.insert(
                            self.hash,
                            PackedTTEntry::new(beta, self.half_move, best_move, NodeType::Upper).0,
                        );
                    }
                    tracing::event!(Level::INFO, name = "Beta cutoff", "eval" = %eval, "beta" = %beta);
                    return EvalResult {
                        eval: beta,
                        nodes,
                        seldepth,
                    };
                }

                if eval > alpha {
                    tracing::event!(Level::INFO, name = "Raised Alpha!", "alpha" = %alpha, "eval" = %eval);
                    is_pv_node = true;
                    alpha = eval;
                    best_move = Some(a);
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

        let should_cache = if let Some(entry) = cached_val {
            self.half_move > PackedTTEntry(entry).depth()
        } else {
            true
        };

        if should_cache {
            cache.insert(
                self.hash,
                PackedTTEntry::new(
                    eval,
                    self.half_move,
                    best_move.map(|m| AlgebraicMove {
                        to: m.to,
                        from: m.from,
                        promotion: m.promotion,
                    }),
                    if is_pv_node {
                        NodeType::Exact
                    } else {
                        NodeType::Lower
                    },
                )
                .0,
            );
        }
        EvalResult {
            eval,
            nodes,
            seldepth,
        }
    }

    #[tracing::instrument]
    pub fn eval(&self, alpha: Evaluation, beta: Evaluation, to_play: Color) -> Evaluation {
        let mg_score: i32 = self.mg_piece_values[Color::White as usize] as i32
            - self.mg_piece_values[Color::Black as usize] as i32;
        let eg_score: i32 = self.eg_piece_values[Color::White as usize] as i32
            - self.eg_piece_values[Color::Black as usize] as i32;
        let mg_phase: i32 = self.game_phase as i32;
        let eg_phase: i32 = 24 - mg_phase;
        let phase1_eval = (((mg_score * mg_phase) + (eg_score * eg_phase)) / 24) as i16;

        // Lazily evaluate the more expensive parts. If we're already too far out of range of alpha
        // and beta, don't bother trying to compute the minutia.
        tracing::event!(Level::INFO,
        name = "Phase1 eval",
        "eval" = %phase1_eval,
        "alpha" = %alpha,
        "beta" = %beta,
        "mg_score" = %mg_score,
        "eg_score" = %eg_score,
        "mg_phase" = %mg_phase,
        "eg_phase" = %eg_phase
        );
        if alpha.0 > phase1_eval && alpha.0 - phase1_eval > 200 {
            tracing::event!(Level::INFO, name = "Alpha too high");
            return match to_play {
                Color::Black => -Evaluation(phase1_eval),
                Color::White => Evaluation(phase1_eval),
            };
        }
        if phase1_eval > beta.0 && (phase1_eval - beta.0) > 200 {
            tracing::event!(Level::INFO, name = "Beta too low");
            return match to_play {
                Color::Black => -Evaluation(phase1_eval),
                Color::White => Evaluation(phase1_eval),
            };
        }

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

        let attacks_diff = attacks_white.len() as i16 - attacks_black.len() as i16;
        let doubled_pawns =
            self.doubled_pawns(Color::White) as i16 - self.doubled_pawns(Color::Black) as i16;
        let isolated_pawns =
            self.isolated_pawns(Color::White) as i16 - self.isolated_pawns(Color::Black) as i16;
        let blocked_pawns =
            self.blocked_pawns(Color::White) as i16 - self.blocked_pawns(Color::Black) as i16;
        let phase2_eval = phase1_eval + attacks_diff as i16
            - 20 * doubled_pawns
            - 30 * isolated_pawns
            - 10 * blocked_pawns;

        tracing::event!(Level::INFO, name = "Phase2 eval", "eval" = %phase2_eval);
        match to_play {
            Color::Black => -Evaluation(phase2_eval),
            Color::White => Evaluation(phase2_eval),
        }
    }

    // The number of doubled pawns a side has
    pub fn doubled_pawns(&self, side: Color) -> u8 {
        let pawns = match side {
            Color::Black => self.black_pieces,
            Color::White => self.white_pieces,
        }[Piece::Pawn as usize];
        let bits = pawns.0;
        let files = [
            0x10101010_10101010,
            0x20202020_20202020,
            0x40404040_40404040,
            0x80808080_80808080,
            0x01010101_01010101,
            0x02020202_02020202,
            0x04040404_04040404,
            0x08080808_08080808,
        ];
        let mut doubled_pawns: u8 = 0;
        for file in files {
            let num_pawns = BitBoard(bits & file).len();
            if num_pawns >= 2 {
                doubled_pawns += num_pawns as u8;
            }
        }
        doubled_pawns
    }

    // The number of isolated pawns a side has
    pub fn isolated_pawns(&self, side: Color) -> u8 {
        let pawns = match side {
            Color::Black => self.black_pieces,
            Color::White => self.white_pieces,
        }[Piece::Pawn as usize];
        let bits = pawns.0;
        let files = [
            0x00000000_00000000,
            0x10101010_10101010,
            0x20202020_20202020,
            0x40404040_40404040,
            0x80808080_80808080,
            0x01010101_01010101,
            0x02020202_02020202,
            0x04040404_04040404,
            0x08080808_08080808,
            0x00000000_00000000,
        ];
        let mut isolated_pawns: u8 = 0;
        for file_idx in 1..9 {
            let pawns_left = bits & files[file_idx - 1];
            let pawns_right = bits & files[file_idx + 1];
            if (pawns_left | pawns_right) == 0 {
                isolated_pawns += BitBoard(bits & files[file_idx]).len() as u8;
            }
        }
        isolated_pawns
    }

    // The number of blocked pawns a side has
    pub fn blocked_pawns(&self, side: Color) -> u8 {
        let pawns = match side {
            Color::Black => self.black_pieces,
            Color::White => self.white_pieces,
        }[Piece::Pawn as usize];
        let opponent_pieces = match !side {
            Color::Black => self.black_pieces(),
            Color::White => self.white_pieces(),
        };

        let mut blocked_pawns: u8 = 0;

        for pawn in pawns {
            if let Some(in_front) = if side == Color::White {
                pawn.no()
            } else {
                pawn.so()
            } {
                if opponent_pieces.contains(in_front) || pawns.contains(in_front) {
                    blocked_pawns += 1;
                }
            }
        }

        blocked_pawns
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
        let b = Board::from_fen("rnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let eval_black = b.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(eval_white.0 > 0);
        assert!(eval_black.0 < 0);
        assert!(eval_black == -eval_white)
    }
    #[test]
    fn black_better() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let eval_black = b.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(eval_white.0 < 0);
        assert!(eval_black.0 > 0);
        assert!(eval_black == -eval_white)
    }
    #[test]
    fn find_queen_take() {
        let mut b = Board::from_fen("4k3/pppppppp/8/8/7q/8/PPPPPPP1/RNBQKBNR w - - 0 1")
            .expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, _) = b.best_move(4, &pool, cache.clone(), None);
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
        let mut b = Board::from_fen("rn1qkbnr/ppp2ppp/3pB3/4p3/4P3/5N2/PPPP1PPP/RNBQK2R b - - 0 1")
            .expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, _) = b.best_move(4, &pool, cache.clone(), None);
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
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval.eval, Evaluation::m1());
    }
    #[test]
    fn won() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(best_move.is_none());
        assert_eq!(eval.eval, Evaluation::won());
    }
    #[test]
    fn lost() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(best_move.is_none());
        assert_eq!(eval.eval, Evaluation::lost());
    }
    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("k7/2Q5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(best_move.is_none());
        assert_eq!(eval.eval, Evaluation::draw());
    }
    #[test]
    fn eval_starting_board() {
        let b = starting_board();
        let eval_white = b.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let eval_black = b.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(eval_white.0 < 100 && eval_white.0 > -100);
        assert!(eval_black.0 < 100 && eval_black.0 > -100);
        assert_eq!(eval_white, eval_black);
    }

    #[test]
    fn eval_starting_board_e4() {
        let mut b = starting_board();
        let eval_white_before = b.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        b.make_move(&Move {
            from: e2(),
            to: e4(),
            piece: Piece::Pawn,
            capture: None,
            promotion: None,
            is_check: false,
            is_mate: false,
            is_en_passant: false,
            is_castle_queen: false,
            is_castle_king: false,
        });
        let eval_white_after = b.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        dbg!(eval_white_before);
        dbg!(eval_white_after);
        assert!(eval_white_before < eval_white_after);
    }

    #[test]
    fn check_mg_table() {
        for posn in ALL_POSNS {
            for piece in [
                Piece::Pawn,
                Piece::Rook,
                Piece::Knight,
                Piece::Bishop,
                Piece::Queen,
                Piece::King,
            ] {
                assert_eq!(
                    MG_TABLE[Color::White as usize][piece as usize][posn.pos.ilog2() as usize],
                    MG_TABLE[Color::Black as usize][piece as usize][flip(posn.pos.ilog2() as usize)],
                );
            }
        }
    }
    #[test]
    fn check_eg_table() {
        for posn in ALL_POSNS {
            for piece in [
                Piece::Pawn,
                Piece::Rook,
                Piece::Knight,
                Piece::Bishop,
                Piece::Queen,
                Piece::King,
            ] {
                assert_eq!(
                    EG_TABLE[Color::White as usize][piece as usize][posn.pos.ilog2() as usize],
                    EG_TABLE[Color::Black as usize][piece as usize][flip(posn.pos.ilog2() as usize)],
                );
            }
        }
    }

    #[test]
    fn draw() {
        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (_, eval) = b.best_move(4, &pool, cache.clone(), None);
        println!("Eval: {}", eval.eval);
        assert!(eval.eval.0 < 100 && eval.eval.0 > -100);

        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (_, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(eval.eval.0 < 100 && eval.eval.0 > -100);
    }
    #[test]
    fn mates() {
        let mut b =
            Board::from_fen("1k6/pppr4/8/8/8/8/8/K6R w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval.eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval.eval, Evaluation::_m3());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        println!("Eval: {}", eval.eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval.eval, Evaluation::m1());

        let mut b = Board::from_fen("k5RN/7R/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        println!("Eval: {}", eval.eval);
        assert!(best_move.is_none());
        assert_eq!(eval.eval, Evaluation::lost());

        let mut b =
            Board::from_fen("k6N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        println!("Eval: {}", eval.eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(eval.eval, Evaluation::m1());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        println!("Eval: {}", eval.eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        assert_eq!(best_move.piece, Piece::King);
        assert_eq!(best_move.from, b8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval.eval, -Evaluation::_m2());
    }
    #[test]
    fn bishop_knight_mate() {
        let mut b =
            Board::from_fen("8/8/8/1B6/5N2/6K1/8/6k1 w - - 0 1").expect("failed to parse fen");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, eval) = b.best_move(4, &pool, cache.clone(), None);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval.eval);
        assert!(eval.eval.mate_in().is_some());
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
    fn mated_in_formatting() {
        assert_eq!(Some(1), Evaluation::m1().mate_in());
        assert_eq!(Some(1), (-Evaluation::m1()).mated_in());
        assert_eq!(Some(2), Evaluation::_m2().mate_in());
        assert_eq!(Some(2), (-Evaluation::_m2()).mated_in());
    }

    #[test]
    fn many_moves() {
        let mut board = Board::from_fen(
            "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
        )
        .expect("Invalid fen?");
        let pool = threadpool::ThreadPool::new(32);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        board.best_move(4, &pool, cache.clone(), None);
    }

    #[test]
    fn many_moves2() {
        let mut board =
            Board::from_fen("rn2k2r/1b1p1p2/p2ppn2/1p1P3p/2P3q1/1PNBP3/P3R1PP/R4Q1K b Qkq - 0 1")
                .expect("Invalid fen?");
        let pool = threadpool::ThreadPool::new(64);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        board.best_move(4, &pool, cache.clone(), None);
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
        let should_stop = Arc::new(AtomicBool::new(false));
        let eval = -board
            .alpha_beta::<1024>(
                Evaluation::lost(),
                -best_score.inc_mate(),
                4,
                &cache,
                should_stop,
            )
            .eval;
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
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (m, _) = board.best_move(4, &pool, cache.clone(), None);
        assert!(m.unwrap().to != c5());
    }
    #[test]
    fn repetition() {
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

        let evalw = board.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        assert!(evalw == Evaluation::draw());
        let evalb = board.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
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
    #[test]
    fn check_packed_tt_entry() {
        {
            let eval = Evaluation::m1();
            let depth = 0x123;
            let best_move = AlgebraicMove {
                from: b6(),
                to: e2(),
                promotion: Some(Piece::Queen),
            };
            let node_type = NodeType::Exact;
            let tt = PackedTTEntry::new(eval, depth, Some(best_move), node_type);
            println!("{:x}", tt.0);
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), Some(best_move));
            assert_eq!(tt.node_type(), node_type);
        }
        {
            let eval = -Evaluation::m1();
            let depth = 0x456;
            let best_move = AlgebraicMove {
                from: h1(),
                to: a8(),
                promotion: None,
            };
            let node_type = NodeType::Upper;
            let tt = PackedTTEntry::new(eval, depth, Some(best_move), node_type);
            println!("{:x}", tt.0);
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), Some(best_move));
            assert_eq!(tt.node_type(), node_type);
        }
        {
            let eval = Evaluation(31);
            let depth = 0x456;
            let best_move = None;
            let node_type = NodeType::Upper;
            let tt = PackedTTEntry::new(eval, depth, best_move, node_type);
            println!("{:x}", tt.0);
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), best_move);
            assert_eq!(tt.node_type(), node_type);
        }
    }

    #[test]
    fn repetition_bug() {
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
54. Ra8+ {+3.98/9 5.0s} Nb8 {-4.00/9 5.0s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let pool = threadpool::ThreadPool::new(1);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (_, move_eval) = board.best_move(5, &pool, cache.clone(), None);

        let evalw = board.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let evalb = board.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(evalw != Evaluation::draw());
        assert!(evalb != Evaluation::draw());
        assert!(move_eval.eval != Evaluation::draw());
    }
    #[test]
    fn doubled_pawns() {
        let b = Board::from_fen("8/P7/PP6/1P6/8/8/8/8 w - - 0 1").expect("failed to parse fen");
        assert_eq!(4, b.doubled_pawns(Color::White));
    }
    #[test]
    fn isolated_pawns() {
        let b = Board::from_fen("8/P1P5/P7/8/8/8/8/8 w - - 0 1").expect("failed to parse fen");
        assert_eq!(3, b.isolated_pawns(Color::White));
    }
    #[test]
    fn blocked_pawns() {
        let b = Board::from_fen("8/p1p5/P1N5/8/8/7P/7P/8 w - - 0 1").expect("failed to parse fen");
        assert_eq!(2, b.blocked_pawns(Color::White));
        assert_eq!(2, b.blocked_pawns(Color::Black));
    }

    #[test]
    #[ignore]
    fn london() {
        let pool = threadpool::ThreadPool::new(64);
        let mut board = Board::from_fen(
            "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
        )
        .expect("Invalid fen?");
        board.it_depth_best_move(6, &pool);
    }

    #[test]
    fn repetition_bug2() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2024.09.22"]
[Round "?"]
[White "gem"]
[Black "Human"]
[Result "1/2-1/2"]
[ECO "A40"]
[GameDuration "00:09:27"]
[GameEndTime "2024-09-22T13:50:09.343 PDT"]
[GameStartTime "2024-09-22T13:40:41.980 PDT"]
[Opening "Queen's pawn"]
[PlyCount "89"]
[TimeControl "inf"]

1. d4 {+0.03/8 5.0s} e6 {3.4s} 2. Nc3 {+0.02/7 5.0s} Bb4 {1.6s}
3. Qd3 {+0.05/8 5.0s} Nc6 {5.0s} 4. d5 {+0.08/7 5.0s} Ne5 {1.2s}
5. Qd4 {+0.13/7 5.0s} Bd6 {1.8s} 6. Nb5 {+0.63/6 5.0s} Ne7 {2.2s}
7. Nxd6+ {+2.17/7 5.0s} cxd6 {3.3s} 8. f4 {+2.07/7 5.0s} Nf5 {33s}
9. Qc3 {+2.75/7 5.0s} Ng4 {1.7s} 10. e4 {+2.52/7 5.0s} Qb6 {2.0s}
11. Nh3 {+2.47/7 5.0s} Nfe3 {3.6s} 12. Kd2 {+2.47/7 5.0s} O-O {5.0s}
13. Be2 {+2.49/7 5.0s} e5 {8.0s} 14. f5 {+2.53/7 5.0s} h6 {2.7s}
15. Re1 {+2.60/7 5.0s} Re8 {2.3s} 16. Bf3 {+3.18/7 5.0s} Qd4+ {5.4s}
17. Qxd4 {+3.01/7 5.0s} exd4 {4.9s} 18. Bxg4 {+3.03/8 5.0s} Nxg4 {3.0s}
19. Kd3 {+3.00/8 5.0s} b6 {55s} 20. Kxd4 {+3.02/8 5.0s} Bb7 {9.7s}
21. Bf4 {+3.02/7 5.0s} Rac8 {6.8s} 22. c3 {+2.99/7 5.0s} Ba6 {6.7s}
23. b3 {+3.00/7 5.0s} Ne5 {8.2s} 24. Re3 {+3.02/7 5.0s} Ng4 {5.8s}
25. Rg3 {+3.02/7 5.0s} Nf6 {11s} 26. Re3 {+2.99/7 5.0s} Ng4 {5.9s}
27. Rg3 {+3.02/7 5.0s} Nf6 {6.7s} 28. Re1 {+2.95/7 5.0s} Nh5 {6.8s}
29. Rge3 {+2.97/7 5.0s} Nxf4 {7.0s} 30. Nxf4 {+3.49/8 5.0s} Bb7 {13s}
31. Rg3 {+3.49/7 5.0s} Re5 {11s} 32. Nd3 {+3.55/7 5.0s} Ree8 {8.9s}
33. Rh3 {+3.50/7 5.0s} Kh7 {7.1s} 34. a4 {+3.50/7 5.0s} g6 {11s}
35. f6 {+3.50/7 5.0s} h5 {7.0s} 36. g4 {+3.50/7 5.0s} a6 {7.5s}
37. g5 {+3.99/7 5.0s} a5 {9.8s} 38. Rh4 {+3.58/7 5.0s} Ba6 {7.3s}
39. Rg1 {+3.58/7 5.0s} Bxd3 {8.0s} 40. Kxd3 {+3.58/8 5.0s} Re5 {7.9s}
41. Kd4 {+3.57/8 5.0s} Kh8 {5.8s} 42. Kd3 {+3.58/8 5.0s} Kh7 {4.9s}
43. Kd4 {+3.57/8 5.0s} Rh8 {7.3s} 44. Kc4 {+3.58/8 5.0s} Rc8+ {6.7s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let pool = threadpool::ThreadPool::new(64);
        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (best_move, move_eval) = board.best_move(6, &pool, cache.clone(), None);

        let evalw = board.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let evalb = board.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(evalw != Evaluation::draw());
        assert!(evalb != Evaluation::draw());
        assert!(move_eval.eval != Evaluation::draw());
        assert!(best_move.unwrap().to != d4());
    }

    #[test]
    fn eval_bug3() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2024.10.04"]
[Round "?"]
[White "Human"]
[Black "gem"]
[Result "1-0"]
[ECO "B01"]
[GameDuration "00:05:32"]
[GameEndTime "2024-10-04T16:26:25.384 PDT"]
[GameStartTime "2024-10-04T16:20:53.380 PDT"]
[Opening "Scandinavian defense"]
[PlyCount "45"]
[TimeControl "inf"]

1. e4 d5 {-0.19/7 5.0s} 2. exd5 {6.3s} Nf6 {-0.31/7 5.0s} 3. d4 {6.2s}
Bg4 {-0.39/7 5.0s} 4. f3 {6.8s} Bf5 {-0.21/7 5.0s} 5. g4 {8.5s}
Bg6 {-0.12/7 5.0s} 6. c4 {6.6s} h6 {-0.09/6 5.0s} 7. h4 {7.7s} c6 {+0.23/6 5.0s}
8. Nc3 {6.2s} cxd5 {+0.43/6 5.0s} 9. cxd5 {8.8s} Nxd5 {+0.53/6 5.0s}
10. h5 {6.4s} Bh7 {+0.26/7 5.0s} 11. Qb3 {6.7s} Nxc3 {+0.06/7 5.0s}
12. Qxb7 {11s} Nd7 {+0.14/6 5.0s} 13. bxc3 {9.8s} Rc8 {+0.06/6 5.0s}
14. Bd2 {9.1s} e5 {-0.08/6 5.0s} 15. dxe5 {7.9s} Rc7 {+0.04/6 5.0s}
16. Qb3 {6.5s} Nxe5 {+0.11/6 5.0s} 17. Bf4 {6.8s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let pool = threadpool::ThreadPool::new(1);
        let res = board.make_alg_move(&AlgebraicMove {
            from: c7(),
            to: d7(),
            promotion: None,
        });
        assert!(res.is_ok());

        let res = board.make_alg_move(&AlgebraicMove {
            from: f4(),
            to: e5(),
            promotion: None,
        });
        assert!(res.is_ok());

        let res = board.make_alg_move(&AlgebraicMove {
            from: d8(),
            to: h4(),
            promotion: None,
        });
        assert!(res.is_ok());

        let res = board.make_alg_move(&AlgebraicMove {
            from: h1(),
            to: h4(),
            promotion: None,
        });
        assert!(res.is_ok());

        let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
        let (Some(_), move_eval) = board.best_move(1, &pool, cache.clone(), None) else {
            assert!(false);
            return;
        };

        assert!(move_eval.eval.0 < 0);
    }
}
