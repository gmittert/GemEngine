use tracing::{field, trace_span, Level};

use crate::board::evaluation::PIECE_VALUES;
use crate::board::*;
use crate::transposition_table::{CacheResult, NodeType, TranspositionTable, DEFAULT_TT_SIZE};
use std::cmp::{max, min};
use std::sync::atomic::{AtomicU16, AtomicUsize};
use std::sync::OnceLock;
use std::sync::{atomic::AtomicBool, atomic::Ordering};
use std::thread;
use std::thread::sleep;
use std::time::{Duration, Instant};

use super::evaluation::Evaluation;

pub struct SearchInfo {
    pub depth: u16,
    pub time: Duration,
    pub nodes: usize,
    pub nodes_per_sec: usize,
    pub seldepth: u16,
    pub hash_full: usize,
}

#[derive(Debug)]
pub enum SearchResult {
    Completed {
        eval: Evaluation,
        best_move: Option<AlgebraicMove>,
        seldepth: u16,
        nodes: usize,
    },
    Aborted {
        seldepth: u16,
        nodes: usize,
    },
}

impl SearchResult {
    pub fn eval(&self) -> Option<Evaluation> {
        match self {
            SearchResult::Completed { eval, .. } => Some(*eval),
            _ => None,
        }
    }
    pub fn best_move(&self) -> Option<AlgebraicMove> {
        match self {
            SearchResult::Completed { best_move, .. } => *best_move,
            _ => None,
        }
    }
    pub fn seldepth(&self) -> u16 {
        match self {
            SearchResult::Completed { seldepth, .. } => *seldepth,
            SearchResult::Aborted { seldepth, .. } => *seldepth,
        }
    }
    pub fn nodes(&self) -> usize {
        match self {
            SearchResult::Completed { nodes, .. } => *nodes,
            SearchResult::Aborted { nodes, .. } => *nodes,
        }
    }

    #[must_use]
    fn improve(&self, other: &SearchResult) -> SearchResult {
        match (self, other) {
            (
                SearchResult::Completed {
                    seldepth: s1,
                    nodes: n1,
                    ..
                },
                SearchResult::Completed {
                    eval,
                    best_move,
                    seldepth,
                    nodes,
                },
            ) => SearchResult::Completed {
                eval: *eval,
                best_move: *best_move,
                seldepth: max(*s1, *seldepth),
                nodes: n1 + nodes,
            },
            (
                SearchResult::Completed {
                    eval,
                    best_move,
                    seldepth: s1,
                    nodes: n1,
                },
                SearchResult::Aborted { seldepth, nodes },
            ) => SearchResult::Completed {
                eval: *eval,
                best_move: *best_move,
                seldepth: max(*s1, *seldepth),
                nodes: n1 + nodes,
            },
            (
                SearchResult::Aborted {
                    seldepth: s1,
                    nodes: n1,
                    ..
                },
                SearchResult::Completed {
                    eval,
                    best_move,
                    seldepth,
                    nodes,
                },
            ) => SearchResult::Completed {
                eval: *eval,
                best_move: *best_move,
                nodes: n1 + nodes,
                seldepth: max(*s1, *seldepth),
            },
            (
                SearchResult::Aborted {
                    seldepth: s1,
                    nodes: n1,
                    ..
                },
                SearchResult::Aborted { seldepth, nodes },
            ) => SearchResult::Aborted {
                seldepth: max(*s1, *seldepth),
                nodes: n1 + nodes,
            },
        }
    }
}

#[derive(PartialEq)]
pub enum ExpectedNodeType {
    PV,
    Cut,
    All,
}

impl Board {
    pub fn search_best_move_for(
        &mut self,
        time: Duration,
        num_threads: usize,
    ) -> (Option<Move>, Evaluation, SearchInfo) {
        let start = Instant::now();
        let end_time = start + time;
        let cache = TranspositionTable::<DEFAULT_TT_SIZE>::new();
        let mut completed_search = self.best_move(1, num_threads, &cache, None);
        let mut depth = 2;
        loop {
            let now = Instant::now();
            if now >= end_time {
                break;
            }
            let evalp = self.best_move(depth, num_threads, &cache, Some(end_time - now));
            completed_search = completed_search.improve(&evalp);
            match evalp {
                SearchResult::Aborted { .. } => break,
                _ => (),
            }
            depth += 1;
        }
        let info = SearchInfo {
            depth,
            seldepth: completed_search.seldepth(),
            nodes: completed_search.nodes(),
            nodes_per_sec: completed_search.nodes() / start.elapsed().as_secs() as usize,
            time,
            hash_full: cache.hash_usage(),
        };
        (
            completed_search
                .best_move()
                .map(|m| self.from_algeabraic(&m)),
            completed_search.eval().unwrap(),
            info,
        )
    }

    pub fn it_depth_best_move(&mut self, target_depth: u16, num_threads: usize) -> SearchResult {
        let cache = TranspositionTable::<DEFAULT_TT_SIZE>::new();

        let mut res = self.best_move(1, num_threads, &cache, None);
        for depth in 1..target_depth {
            let res2 = self.best_move(depth + 1, num_threads, &cache, None);
            res = res.improve(&res2);
        }
        res
    }

    pub fn best_move<const N: usize>(
        &mut self,
        depth: u16,
        num_threads: usize,
        cache: &TranspositionTable<N>,
        time: Option<Duration>,
    ) -> SearchResult {
        let target_depth = self.half_move + depth;
        let should_stop = AtomicBool::new(false);
        let result: OnceLock<Option<(Evaluation, Option<AlgebraicMove>)>> = OnceLock::new();
        let total_seldepth = AtomicU16::new(0);
        let total_nodes = AtomicUsize::new(0);
        thread::scope(|s| {
            for _ in 0..num_threads {
                s.spawn(|| {
                    let mut new_b = self.clone();
                    let res = new_b.pvs(
                        Evaluation::lost(),
                        Evaluation::won(),
                        target_depth,
                        cache,
                        &should_stop,
                        ExpectedNodeType::PV,
                    );
                    match res {
                        SearchResult::Completed {
                            eval,
                            best_move,
                            seldepth,
                            nodes,
                        } => {
                            let _ = &total_nodes.fetch_add(nodes, Ordering::AcqRel);
                            let _ = &total_seldepth.fetch_max(seldepth, Ordering::AcqRel);
                            if result.set(Some((eval, best_move))).is_ok() {
                                let _ = &should_stop.store(true, Ordering::Relaxed);
                            }
                        }
                        SearchResult::Aborted { seldepth, nodes } => {
                            let _ = &total_nodes.fetch_add(nodes, Ordering::AcqRel);
                            let _ = &total_seldepth.fetch_max(seldepth, Ordering::AcqRel);
                        }
                    }
                });
            }
            // If we have a maximum time to wait for, instead of waiting on the result directly, we
            // watch a timer to wait for the timeout. We check every 100ms if any search has
            // completed yet so we don't want too long.
            if let Some(t) = time {
                let end_time = Instant::now() + t;
                while !should_stop.load(Ordering::Relaxed) {
                    let now = Instant::now();
                    if now >= end_time {
                        if result.set(None).is_ok() {
                            let _ = &should_stop.store(true, Ordering::Relaxed);
                        };
                        break;
                    }
                    let remaining = end_time - now;
                    sleep(min(remaining, Duration::from_millis(100)));
                }
            }
        });

        if let Some((eval, best_move)) = result.wait() {
            SearchResult::Completed {
                eval: *eval,
                best_move: *best_move,
                seldepth: total_seldepth.load(Ordering::Acquire),
                nodes: total_nodes.load(Ordering::Acquire),
            }
        } else {
            SearchResult::Aborted {
                seldepth: total_seldepth.load(Ordering::Acquire),
                nodes: total_nodes.load(Ordering::Acquire),
            }
        }
    }

    pub fn quiesce(&mut self, alpha: Evaluation, beta: Evaluation) -> SearchResult {
        let mut alpha = alpha;
        let stand_pat = self.eval(alpha, beta, self.to_play);
        tracing::event!(Level::INFO, stand_pat = stand_pat.0);
        if stand_pat >= beta {
            return SearchResult::Completed {
                eval: beta,
                seldepth: 1,
                nodes: 1,
                best_move: None,
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
                    Color::Black => trace_span!("quiesece white", inspecting = %m, alpha = -beta.0, beta = -alpha.inc_mate().0, eval = field::Empty).entered(),
                    Color::White => trace_span!("quiesce black", inspecting = %m, alpha = -beta.0, beta = -alpha.inc_mate().0, eval = field::Empty).entered(),
                };
                let eval_res = self.quiesce(-beta, -alpha.inc_mate());

                let eval = match eval_res {
                    SearchResult::Completed {
                        eval,
                        seldepth: s1,
                        nodes: n1,
                        ..
                    } => {
                        nodes += n1;
                        seldepth = max(seldepth, s1 + 1);
                        -eval.dec_mate()
                    }
                    SearchResult::Aborted { .. } => panic!("Quience shouldn't abort"),
                };
                span.record("eval", eval.0);
                drop(span);
                if eval >= beta {
                    self.undo_move(&m);

                    tracing::event!(
                        Level::INFO,
                        name = "Beta cutoff",
                        eval = eval.0,
                        beta = beta.0
                    );
                    return SearchResult::Completed {
                        eval: beta,
                        seldepth,
                        nodes,
                        best_move: None,
                    };
                }
                if eval > alpha {
                    tracing::event!(
                        Level::INFO,
                        name = "Raised Alpha!",
                        alpha = alpha.0,
                        eval = eval.0
                    );
                    alpha = eval;
                }
            }
            self.undo_move(&m);
        }
        SearchResult::Completed {
            eval: alpha,
            seldepth,
            nodes,
            best_move: None,
        }
    }

    pub fn pvs<const N: usize>(
        &mut self,
        alpha: Evaluation,
        beta: Evaluation,
        target_depth: u16,
        cache: &TranspositionTable<N>,
        should_stop: &AtomicBool,
        node_type: ExpectedNodeType,
    ) -> SearchResult {
        let mut seldepth = 0;
        let mut nodes = 1;

        if should_stop.load(Ordering::Acquire) {
            return SearchResult::Aborted { seldepth, nodes };
        }

        let cached_val = cache.get(self.hash, alpha, beta, target_depth);
        let mut hash_move = match cached_val {
            CacheResult::Exact(best_move, eval) => {
                return SearchResult::Completed {
                    eval,
                    best_move,
                    seldepth,
                    nodes,
                }
            }
            CacheResult::HashMove(m) => m,
            CacheResult::Miss => None,
        };

        // If we've got deep enough, run a quiesence search to reduce horizon effects. We don't
        // want to compute taking a pawn with our queen and just stop computing there, for example.
        if self.half_move >= target_depth {
            let span = trace_span!(
                "quiesece",
                alpha = alpha.0,
                beta = beta.0,
                eval = field::Empty
            )
            .entered();

            let res = self.quiesce(alpha, beta);
            let eval = match res {
                SearchResult::Completed { eval, .. } => eval,
                SearchResult::Aborted { .. } => panic!("Quience shouldn't abort"),
            };
            span.record("eval", eval.0);
            return res;
        }

        if hash_move.is_none()
            && node_type == ExpectedNodeType::PV
            && target_depth - self.half_move > 2
        {
            // Do internal iterative deepening.
            match self.pvs(
                alpha,
                beta,
                target_depth - 2,
                cache,
                should_stop,
                ExpectedNodeType::PV,
            ) {
                SearchResult::Completed {
                    best_move: b1,
                    seldepth: s1,
                    nodes: n1,
                    ..
                } => {
                    nodes += n1;
                    seldepth = max(seldepth, s1 + 1);
                    hash_move = b1;
                }
                SearchResult::Aborted {
                    seldepth: s1,
                    nodes: n1,
                } => {
                    nodes += n1;
                    seldepth = max(seldepth, s1 + 1);
                    return SearchResult::Aborted { seldepth, nodes };
                }
            }
        }

        let mut alpha = alpha;
        let mut had_legal_move = false;

        let recapture = if let Some((p, _)) = self.moves.last() {
            self.get_smallest_attacker(*p, self.to_play)
                .map(|m| m.algebraic_move())
        } else {
            None
        };

        let killer_moves = {
            if target_depth - self.half_move >= 16 {
                vec![]
            } else {
                let killer_idx = target_depth - self.half_move;
                let killer_moves = self.killer_moves[killer_idx as usize];
                let mut killers = vec![];
                if let Some(m0) = killer_moves[0] {
                    if self.check_killer(&m0) {
                        killers.push(m0)
                    }
                }
                if let Some(m1) = killer_moves[1] {
                    if self.check_killer(&m1) {
                        killers.push(m1)
                    }
                }
                killers
            }
        }
        .into_iter();

        let moves = hash_move
            .into_iter()
            .chain(recapture.into_iter())
            .chain(killer_moves)
            .chain(self.pseudo_legal_randomized_moves_it());
        let mut is_pv_node = false;
        let mut is_first_child = true;
        let mut best_move = None;
        for a in moves {
            let m = self.from_algeabraic(&a);
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                if best_move.is_none() {
                    best_move = Some(a);
                }
                had_legal_move = true;
                let span = match self.to_play {
                    Color::Black => trace_span!("white", piece = %m.piece, to = %m.to, alpha = -beta.0, beta = -alpha.inc_mate().0, eval = field::Empty).entered(),
                    Color::White => trace_span!("black", piece = %m.piece, to = %m.to, alpha = -beta.0, beta = -alpha.inc_mate().0, eval = field::Empty).entered(),
                };
                // Check for 3 fold repetition
                let mut is_three_fold = false;
                if let Some(irr) = self.last_irreversible.last() {
                    if self.half_move - irr >= 8 {
                        for (_, prev_state) in &self.moves[*irr as usize..] {
                            if *prev_state == self.hash {
                                is_three_fold = true;
                                break;
                            }
                        }
                    }
                }
                let eval_res = if is_three_fold {
                    SearchResult::Completed {
                        eval: Evaluation::draw(),
                        best_move: Some(a),
                        seldepth: 0,
                        nodes: 1,
                    }
                } else {
                    // PV Search: We'd ordered our hash move in front and it's likely to be the PV
                    // node. Establish an exact score for it, and search a smaller window for
                    // everything else. If a move might actually be better, research it to find the
                    // actual score.
                    if is_first_child || alpha.mate_in().is_some() || alpha.mated_in().is_some() {
                        is_first_child = false;
                        let expected_next_node = match node_type {
                            ExpectedNodeType::PV => ExpectedNodeType::PV,
                            ExpectedNodeType::Cut => ExpectedNodeType::All,
                            ExpectedNodeType::All => ExpectedNodeType::Cut,
                        };
                        self.pvs(
                            -beta,
                            -alpha.inc_mate(),
                            target_depth,
                            cache,
                            should_stop,
                            expected_next_node,
                        )
                    } else {
                        let expected_next_node = match node_type {
                            ExpectedNodeType::PV => ExpectedNodeType::Cut,
                            ExpectedNodeType::Cut => ExpectedNodeType::All,
                            ExpectedNodeType::All => ExpectedNodeType::Cut,
                        };
                        let mut score = self.pvs(
                            Evaluation(-alpha.inc_mate().0 - 1),
                            -alpha.inc_mate(),
                            target_depth,
                            cache,
                            should_stop,
                            expected_next_node,
                        );
                        match score {
                            SearchResult::Completed { eval, .. } => {
                                // The score should be in our alpha beta window. If it's not, we need to do
                                // a full search.
                                if alpha < -eval && -eval < beta {
                                    score = self.pvs(
                                        -beta,
                                        -alpha.inc_mate(),
                                        target_depth,
                                        cache,
                                        should_stop,
                                        ExpectedNodeType::PV,
                                    );
                                }
                                score
                            }
                            a => a,
                        }
                    }
                };
                let eval = match eval_res {
                    SearchResult::Completed {
                        eval,
                        seldepth: s1,
                        nodes: n1,
                        ..
                    } => {
                        nodes += n1;
                        seldepth = max(seldepth, s1 + 1);
                        -eval.dec_mate()
                    }
                    SearchResult::Aborted {
                        seldepth: s1,
                        nodes: n1,
                    } => {
                        nodes += n1;
                        seldepth = max(seldepth, s1 + 1);
                        return SearchResult::Aborted { seldepth, nodes };
                    }
                };
                span.record("eval", eval.0);
                drop(span);

                if eval >= beta {
                    self.undo_move(&m);
                    match cached_val {
                        CacheResult::Miss => {
                            cache.insert(self.hash, beta, best_move, target_depth, NodeType::Upper);
                        }
                        _ => {
                            cache.update(self.hash, beta, best_move, target_depth, NodeType::Upper);
                        }
                    };
                    if m.capture.is_none() {
                        // This is a quiet move that caused a beta cutoff, record this as a killer
                        // move! Since it's a strong move that didn't involve capturing anything,
                        // it's likely strong for a lot of other moves at this level.
                        let killer_idx = (target_depth - self.half_move) as usize;
                        if killer_idx < 16 {
                            let killer_moves = self.killer_moves[killer_idx];
                            let new_move = Some(m.algebraic_move());
                            if killer_moves[0] != new_move && killer_moves[1] != new_move {
                                self.killer_moves[killer_idx][1] = self.killer_moves[killer_idx][0];
                                self.killer_moves[killer_idx][0] = new_move
                            }
                        }
                    }
                    tracing::event!(
                        Level::INFO,
                        name = "Beta cutoff",
                        eval = eval.0,
                        beta = beta.0
                    );
                    return SearchResult::Completed {
                        eval: beta,
                        nodes,
                        seldepth,
                        best_move: Some(a),
                    };
                }

                if eval > alpha {
                    tracing::event!(
                        Level::INFO,
                        name = "Raised Alpha!",
                        alpha = alpha.0,
                        eval = eval.0
                    );
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

        let node_type = if is_pv_node {
            NodeType::Exact
        } else {
            NodeType::Lower
        };
        match cached_val {
            CacheResult::Miss => {
                cache.insert(self.hash, eval, best_move, target_depth, node_type);
            }
            _ => {
                cache.update(self.hash, eval, best_move, target_depth, node_type);
            }
        };
        SearchResult::Completed {
            eval,
            nodes,
            seldepth,
            best_move,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board::evaluation::*;
    use crate::board::search::*;

    #[test]
    fn find_queen_take() {
        let mut b = Board::from_fen("4k3/pppppppp/8/8/7q/8/PPPPPPP1/RNBQKBNR w - - 0 1")
            .expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let best_move = b.best_move(4, 1, &cache, None).best_move();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
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
        let cache = TranspositionTable::<1024>::new();
        let best_move = b.best_move(4, 1, &cache, None).best_move();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
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
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval.unwrap(), Evaluation::m1());
    }

    #[test]
    fn won() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval();
        assert!(best_move.is_none());
        assert_eq!(eval.unwrap(), -Evaluation::won());
    }

    #[test]
    fn lost() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval();
        assert!(best_move.is_none());
        assert_eq!(eval.unwrap(), Evaluation::lost());
    }

    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("k7/2Q5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval();
        assert!(best_move.is_none());
        assert_eq!(eval.unwrap(), Evaluation::draw());
    }

    #[test]
    fn draw() {
        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let eval = b.best_move(4, 1, &cache, None).eval().unwrap();
        println!("Eval: {}", eval);
        assert!(eval.0 < 100 && eval.0 > -100);

        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let eval = b.best_move(4, 1, &cache, None).eval().unwrap();
        assert!(eval.0 < 100 && eval.0 > -100);
    }
    #[test]
    fn mates() {
        let mut b =
            Board::from_fen("1k6/pppr4/8/8/8/8/8/K6R w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval().unwrap();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m3());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval().unwrap();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
        println!("Eval: {}", eval);
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m1());

        let mut b = Board::from_fen("k5RN/7R/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let eval = b.best_move(4, 1, &cache, None).eval().unwrap();
        println!("Eval: {}", eval);
        assert_eq!(eval, Evaluation::lost());

        let mut b =
            Board::from_fen("k6N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval().unwrap();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(eval, Evaluation::m1());

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval().unwrap();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::King);
        assert_eq!(best_move.from, b8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, -Evaluation::m2());
    }

    #[test]
    fn bishop_knight_mate() {
        let mut b =
            Board::from_fen("8/8/8/1B6/5N2/6K1/8/6k1 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval().unwrap();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic(&best_move);
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert!(eval.mate_in().is_some());
    }

    #[test]
    fn london() {
        let mut board = Board::from_fen(
            "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
        )
        .expect("Invalid fen?");
        board.it_depth_best_move(6, 64);
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
        let cache = TranspositionTable::<1024>::new();
        let res = board.best_move(6, 64, &cache, None);
        let best_move = res.best_move();
        let eval = res.eval().unwrap();

        let evalw = board.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let evalb = board.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(evalw != Evaluation::draw());
        assert!(evalb != Evaluation::draw());
        assert!(eval != Evaluation::draw());
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

        let cache = TranspositionTable::<1024>::new();
        let eval = board.best_move(1, 1, &cache, None).eval().unwrap();
        assert!(eval.0 < 0);
    }
    #[test]
    fn eval_bug_bad_trade() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2024.10.04"]
[Round "?"]
[White "gem"]
[Black "Human"]
[Result "0-1"]
[ECO "A05"]
[GameDuration "00:06:06"]
[GameEndTime "2024-10-04T23:38:47.250 PDT"]
[GameStartTime "2024-10-04T23:32:40.838 PDT"]
[Opening "Reti Opening"]
[PlyCount "39"]
[Termination "adjudication"]
[TimeControl "inf"]

1. Nf3 {+0.29/8 5.0s} Nf6 {8.5s} 2. d4 {+0.08/7 5.0s} g6 {7.5s}
3. Nc3 {+0.11/7 5.0s} d5 {10s} 4. e3 {+0.06/7 5.0s} Bg4 {71s}
5. Be2 {+0.31/7 5.0s} Nc6 {16s} 6. Bb5 {+0.16/7 5.0s} a6 {8.3s}
7. Bxc6+ {+0.77/6 5.0s} bxc6 {8.4s} 8. O-O {+0.38/7 5.0s} Qd6 {16s}
9. h3 {+0.47/7 5.0s} Bxf3 {6.7s} 10. Qxf3 {+0.47/7 5.0s} Bg7 {8.1s}
11. Rd1 {+0.39/7 5.0s} O-O {9.1s} 12. e4 {+1.00/6 5.0s} dxe4 {6.2s}
13. Nxe4 {+0.83/7 5.0s} Nxe4 {6.0s} 14. Qxe4 {+0.79/7 5.0s} e5 {6.1s}
15. Be3 {+1.05/6 5.0s} f5 {8.6s} 16. Qd3 {+0.50/7 5.0s} f4 {5.8s} *
17. dxe5 {-0.22/7 5.0s} Qxd3 {7.9s} 18. Rxd3 {-0.29/7 5.0s} fxe3 {6.6s}
19. Rxe3 {-0.71/7 5.0s} *
"###;
        let board = Board::from_pgn(pgn).expect("bad pgn?");
        let eval = board.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(eval.0 < 0);
    }
    #[test]
    fn mate_in_2_format() {
        let fen = "6k1/p6p/3p2p1/3P1B2/2Q3n1/N1P5/Pr1B2P1/R3RK1q w - - 1 23";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        let cache = TranspositionTable::<1024>::new();
        let eval = board.best_move(6, 1, &cache, None).eval().unwrap();
        let Some(mated_in) = eval.mated_in() else {
            assert!(false);
            return;
        };
        assert_eq!(mated_in, 4);
    }
    #[test]
    fn mate_1_disconnect() {
        let fen = "6rk/p1p5/4BNQ1/4P3/4P3/2p2P2/6R1/3R3K w - - 1 39";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        let eval = board.it_depth_best_move(7, 32).eval().unwrap();
        assert_eq!(eval, Evaluation::m1());
    }
    #[test]
    fn eval_bug4() {
        let fen = "r1b1k2r/pp1n3p/6pN/4pp2/3P3Q/8/2q1KPPP/3R1B1R w kq - 0 19";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        let cache = TranspositionTable::<1024>::new();
        let move_eval = board.best_move(6, 1, &cache, None).eval().unwrap();
        println!("move_eval: {}", move_eval);
        assert!(move_eval.0 < 0);
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
        let cache = TranspositionTable::<1024>::new();
        let move_eval = board.best_move(5, 1, &cache, None).eval().unwrap();

        let evalw = board.eval(Evaluation::lost(), Evaluation::won(), Color::White);
        let evalb = board.eval(Evaluation::lost(), Evaluation::won(), Color::Black);
        assert!(evalw != Evaluation::draw());
        assert!(evalb != Evaluation::draw());
        assert!(move_eval != Evaluation::draw());
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
        let cache = TranspositionTable::<1024>::new();
        let should_stop = AtomicBool::new(false);
        let eval = -board
            .pvs(
                Evaluation::lost(),
                -best_score.inc_mate(),
                4,
                &cache,
                &should_stop,
                ExpectedNodeType::PV,
            )
            .eval()
            .expect("Failed to resolve value");
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

        let cache = TranspositionTable::<1024>::new();
        let m = board.best_move(4, 32, &cache, None).best_move().unwrap();
        assert!(m.to != c5());
    }

    #[test]
    fn many_moves() {
        let mut board = Board::from_fen(
            "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
        )
        .expect("Invalid fen?");
        let cache = TranspositionTable::<1024>::new();
        board.best_move(4, 32, &cache, None);
    }

    #[test]
    fn many_moves2() {
        let mut board =
            Board::from_fen("rn2k2r/1b1p1p2/p2ppn2/1p1P3p/2P3q1/1PNBP3/P3R1PP/R4Q1K b Qkq - 0 1")
                .expect("Invalid fen?");
        let cache = TranspositionTable::<1024>::new();
        board.best_move(4, 64, &cache, None);
    }
}
