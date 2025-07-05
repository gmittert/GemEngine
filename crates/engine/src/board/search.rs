use tracing::{Level, field, trace_span};

use crate::board::evaluation::PIECE_VALUES;
use crate::board::*;
use crate::transposition_table::{CacheResult, DEFAULT_TT_SIZE, ScoreType, TranspositionTable};
use std::cmp::max;
use std::sync::atomic::{AtomicU16, AtomicUsize};
use std::sync::{Condvar, Mutex};
use std::sync::{atomic::AtomicBool, atomic::Ordering};
use std::time::{Duration, Instant};

use super::evaluation::Evaluation;

pub struct SearchInfo {
    pub depth: u16,
    pub time: Duration,
    pub nodes: usize,
    pub nodes_per_sec: usize,
    pub seldepth: u16,
    pub hash_full: usize,
    pub qnodes: usize,
}

#[derive(Debug)]
pub struct SearchResult {
    eval: Evaluation,
    best_move: Option<AlgebraicMove>,
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
        self.reset_stats();
        let start = Instant::now();
        let end_time = start + time;
        let cache = TranspositionTable::<DEFAULT_TT_SIZE>::new();
        let mut completed_search = self.best_move(1, num_threads, &cache, None).unwrap();
        let mut depth = 2;
        loop {
            let now = Instant::now();
            if now > end_time {
                break;
            }
            if let Some(evalp) = self.best_move(depth, num_threads, &cache, Some(end_time - now)) {
                completed_search = evalp;
            } else {
                break;
            }
            depth += 1;
        }

        let (seldepth, nodes, qnodes) = self.get_stats();
        let elapsed_ms = start.elapsed().as_millis().min(1);
        let info = SearchInfo {
            depth,
            seldepth: max(seldepth, self.half_move) - self.half_move,
            nodes,
            qnodes,
            nodes_per_sec: 1000 * self.nodes / elapsed_ms as usize,
            time,
            hash_full: 0,
        };
        (
            completed_search
                .best_move
                .map(|m| self.from_algeabraic_unchecked(&m)),
            completed_search.eval,
            info,
        )
    }

    pub fn it_depth_best_move(&mut self, target_depth: u16, num_threads: usize) -> SearchResult {
        let cache = TranspositionTable::<DEFAULT_TT_SIZE>::new();

        let mut res = self.best_move(1, num_threads, &cache, None).unwrap();
        for depth in 1..target_depth {
            res = self
                .best_move(depth + 1, num_threads, &cache, None)
                .unwrap();
        }
        res
    }

    pub fn best_move<const N: usize>(
        &mut self,
        depth: u16,
        num_threads: usize,
        cache: &TranspositionTable<N>,
        time: Option<Duration>,
    ) -> Option<SearchResult> {
        let end_time = time.map(|t| Instant::now() + t);
        let target_depth = self.half_move + depth;
        let should_stop = AtomicBool::new(false);
        let result: Mutex<Option<(Evaluation, Option<AlgebraicMove>)>> = Mutex::new(None);
        let cv = Condvar::new();
        let total_seldepth = AtomicU16::new(0);
        let total_nodes = AtomicUsize::new(self.nodes);
        let total_qnodes = AtomicUsize::new(self.qnodes);
        let res = rayon::scope(|s| {
            for _ in 0..num_threads {
                s.spawn(|_| {
                    let mut new_b = self.clone();
                    new_b.nodes = 0;
                    new_b.qnodes = 0;
                    let res = new_b.pvs(
                        Evaluation::lost(self.half_move),
                        Evaluation::won(self.half_move),
                        target_depth,
                        cache,
                        &should_stop,
                        ExpectedNodeType::PV,
                    );
                    let _ = &total_nodes.fetch_add(new_b.nodes, Ordering::AcqRel);
                    let _ = &total_qnodes.fetch_add(new_b.qnodes, Ordering::AcqRel);
                    let _ = &total_seldepth.fetch_max(new_b.seldepth, Ordering::AcqRel);
                    if let Some(res) = res {
                        let mut eval = result.lock().unwrap();
                        if eval.is_none() {
                            *eval = Some((res.eval, res.best_move));
                            cv.notify_one();
                            should_stop.store(true, Ordering::Relaxed);
                        }
                    }
                });
            }
            let mut res = result.lock().unwrap();
            // Loop for spurious wake ups
            loop {
                if let Some((eval, best_move)) = *res {
                    return Some(SearchResult { eval, best_move });
                } else if let Some(end) = end_time {
                    let now = Instant::now();
                    if now > end {
                        should_stop.store(true, Ordering::Relaxed);
                        return None;
                    }
                    let (guard, timed_out) = cv.wait_timeout(res, end - now).unwrap();
                    if timed_out.timed_out() {
                        should_stop.store(true, Ordering::Relaxed);
                        return None;
                    }
                    res = guard;
                } else {
                    res = cv.wait(res).unwrap();
                }
            }
        });
        self.seldepth = total_seldepth.load(Ordering::Acquire);
        self.nodes = total_nodes.load(Ordering::Acquire);
        self.qnodes = total_qnodes.load(Ordering::Acquire);
        res
    }

    pub fn quiesce(&mut self, alpha: Evaluation, beta: Evaluation) -> Evaluation {
        self.qnodes += 1;
        let mut alpha = alpha;
        let stand_pat = self.eval(alpha, beta, self.to_play);
        tracing::event!(Level::INFO, stand_pat = stand_pat.0);
        if stand_pat >= beta {
            return beta;
        }
        if alpha < stand_pat {
            alpha = stand_pat;
        }
        let captures = self.pseudo_legal_captures_it();
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

            let Some(m) = self.from_algeabraic(&capture) else {
                debug_assert!(false, "Invalid move pulled from cache: {capture}");
                continue;
            };
            let value = self.static_exchange_evaluation(m.to, m.capture.unwrap(), m.from, m.piece);
            if value <= Evaluation::draw() {
                continue;
            }
            self.make_move(&m);

            if !self.in_check(!self.to_play) {
                let span = match self.to_play {
                    Color::Black => trace_span!("quiesece white", inspecting = %m, alpha = -beta.0, beta = -alpha.0, eval = field::Empty).entered(),
                    Color::White => trace_span!("quiesce black", inspecting = %m, alpha = -beta.0, beta = -alpha.0, eval = field::Empty).entered(),
                };
                let eval = -self.quiesce(-beta, -alpha);

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
                    return beta;
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
        alpha
    }

    pub fn has_three_fold_repetition(&self) -> bool {
        let Some(irr) = self.last_irreversible.last() else {
            return false;
        };
        let mut count = 1;
        if self.half_move - irr >= 8 {
            for (_, prev_state) in &self.moves[*irr as usize..] {
                if *prev_state == self.hash {
                    count += 1;
                }
            }
        }
        count >= 3
    }

    pub fn eval_null_move<const N: usize>(
        &mut self,
        target_depth: u16,
        beta: Evaluation,
        cache: &TranspositionTable<N>,
        should_stop: &AtomicBool,
    ) -> Option<Evaluation> {
        #[cfg(not(feature = "nnue"))]
        if self.game_phase == 24 {
            return None;
        }
        if target_depth - self.half_move < 2 || self.in_check(self.to_play) || beta.mate() {
            return None;
        }
        self.make_null_move();

        let killer_moves = self.killer_moves;
        self.killer_moves = [[None; 2]; 16];

        let search = self.pvs(
            -beta,
            Evaluation(-(beta.0 - 1)),
            target_depth - 2,
            cache,
            should_stop,
            ExpectedNodeType::PV,
        )?;

        self.killer_moves = killer_moves;
        self.undo_null_move();
        let eval = -search.eval;
        if eval >= beta { Some(eval) } else { None }
    }

    fn search_extensions(&self, target_depth: u16) -> u16 {
        let mut additions = 0;
        if self.in_check(self.to_play) {
            additions += 1;
        }
        target_depth + additions
    }

    pub fn pvs<const N: usize>(
        &mut self,
        alpha: Evaluation,
        beta: Evaluation,
        target_depth: u16,
        cache: &TranspositionTable<N>,
        should_stop: &AtomicBool,
        node_type: ExpectedNodeType,
    ) -> Option<SearchResult> {
        self.nodes += 1;
        self.seldepth = max(self.seldepth, self.half_move);

        if should_stop.load(Ordering::Acquire) {
            return None;
        }

        let cached_val = cache.get(self.hash, alpha, beta, target_depth);
        let mut hash_move = match cached_val {
            CacheResult::Cutoff(best_move, eval) => {
                return Some(SearchResult { eval, best_move });
            }
            CacheResult::HashMove(m) => m,
            CacheResult::Miss => None,
        };

        if self.has_three_fold_repetition() {
            return Some(SearchResult {
                eval: Evaluation::draw(),
                best_move: None,
            });
        }

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

            let eval = self.quiesce(alpha, beta);
            span.record("eval", eval.0);
            return Some(SearchResult {
                eval,
                best_move: None,
            });
        }

        if let Some(eval) = self.eval_null_move(target_depth, beta, cache, should_stop) {
            return Some(SearchResult {
                eval,
                best_move: None,
            });
        }
        // We need to recheck should_stop here -- the pvs in eval_null_move could have failed due
        // to time out.
        if should_stop.load(Ordering::Acquire) {
            return None;
        }

        if hash_move.is_none()
            && node_type == ExpectedNodeType::PV
            && target_depth - self.half_move > 2
        {
            trace_span!("iid");
            // Do internal iterative deepening.
            hash_move = self
                .pvs(
                    alpha,
                    beta,
                    target_depth - 2,
                    cache,
                    should_stop,
                    ExpectedNodeType::PV,
                )?
                .best_move;
        }

        let mut alpha = alpha;
        let mut had_legal_move = false;

        let recapture = if let Some((Some(p), _)) = self.moves.last() {
            self.get_smallest_attacker(*p, self.to_play)
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
                if let Some(m0) = killer_moves[0]
                    && self.check_killer(&m0)
                {
                    killers.push(m0)
                }
                if let Some(m1) = killer_moves[1]
                    && self.check_killer(&m1)
                {
                    killers.push(m1)
                }
                killers
            }
        }
        .into_iter();

        let moves = hash_move
            .into_iter()
            .chain(recapture)
            .chain(killer_moves)
            .chain(self.pseudo_legal_randomized_moves_it());
        let mut is_pv_node = false;
        let mut is_first_child = true;
        let mut best_move = None;
        for a in moves {
            let Some(m) = self.from_algeabraic(&a) else {
                debug_assert!(false, "Invalid move pulled from cache: {a}");
                continue;
            };
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                if best_move.is_none() {
                    best_move = Some(a);
                }
                had_legal_move = true;
                let span = match self.to_play {
                    Color::Black => trace_span!("white", piece = %m.piece, to = %m.to, alpha = -beta.0, beta = -alpha.0, eval = field::Empty).entered(),
                    Color::White => trace_span!("black", piece = %m.piece, to = %m.to, alpha = -beta.0, beta = -alpha.0, eval = field::Empty).entered(),
                };
                // PV Search: We'd ordered our hash move in front and it's likely to be the PV
                // node. Establish an exact score for it, and search a smaller window for
                // everything else. If a move might actually be better, research it to find the
                // actual score.
                let eval_res = if is_first_child || alpha.mate() {
                    is_first_child = false;
                    let expected_next_node = match node_type {
                        ExpectedNodeType::PV => ExpectedNodeType::PV,
                        ExpectedNodeType::Cut => ExpectedNodeType::All,
                        ExpectedNodeType::All => ExpectedNodeType::Cut,
                    };
                    self.pvs(
                        -beta,
                        -alpha,
                        target_depth,
                        cache,
                        should_stop,
                        expected_next_node,
                    )?
                } else {
                    let expected_next_node = match node_type {
                        ExpectedNodeType::PV => ExpectedNodeType::Cut,
                        ExpectedNodeType::Cut => ExpectedNodeType::All,
                        ExpectedNodeType::All => ExpectedNodeType::Cut,
                    };
                    let mut score = self.pvs(
                        Evaluation(-alpha.0 - 1),
                        -alpha,
                        self.search_extensions(target_depth),
                        cache,
                        should_stop,
                        expected_next_node,
                    )?;

                    if alpha < -score.eval && -score.eval < beta {
                        score = self.pvs(
                            -beta,
                            -alpha,
                            self.search_extensions(target_depth),
                            cache,
                            should_stop,
                            ExpectedNodeType::PV,
                        )?;
                    }

                    score
                };
                let eval = -eval_res.eval;
                span.record("eval", eval.0);
                drop(span);

                if eval >= beta {
                    self.undo_move(&m);
                    match cached_val {
                        CacheResult::Miss => {
                            cache.insert(
                                self.hash,
                                beta,
                                best_move,
                                target_depth,
                                ScoreType::Lower,
                            );
                        }
                        _ => {
                            cache.update(
                                self.hash,
                                beta,
                                best_move,
                                target_depth,
                                ScoreType::Lower,
                            );
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
                    return Some(SearchResult {
                        eval: beta,
                        best_move: Some(a),
                    });
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
                Evaluation::lost(self.half_move)
            } else {
                Evaluation::draw()
            }
        };

        let node_type = if is_pv_node {
            ScoreType::Exact
        } else {
            ScoreType::Upper
        };
        match cached_val {
            CacheResult::Miss => {
                cache.insert(self.hash, eval, best_move, target_depth, node_type);
            }
            _ => {
                cache.update(self.hash, eval, best_move, target_depth, node_type);
            }
        };
        Some(SearchResult { eval, best_move })
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
        let best_move = b.best_move(4, 1, &cache, None).unwrap().best_move;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
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
        let best_move = b.best_move(4, 1, &cache, None).unwrap().best_move;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
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
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m1(b.half_move));
    }

    #[test]
    fn won() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_none());
        assert_eq!(eval, -Evaluation::won(b.half_move));
    }

    #[test]
    fn lost() {
        let mut b =
            Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::lost(b.half_move));
    }

    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("k7/2Q5/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::draw());
    }

    #[test]
    fn draw() {
        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let eval = b.best_move(4, 1, &cache, None).unwrap().eval;
        println!("Eval: {}", eval);
        assert!(eval.0 < 100 && eval.0 > -100);

        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let eval = b.best_move(4, 1, &cache, None).unwrap().eval;
        assert!(eval.0 < 100 && eval.0 > -100);
    }
    #[test]
    fn mates() {
        let mut b =
            Board::from_fen("1k6/pppr4/8/8/8/8/8/K6R w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m3(b.half_move));

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
        println!("Eval: {}", eval);
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m1(b.half_move));

        let mut b = Board::from_fen("k5RN/7R/8/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let eval = b.best_move(4, 1, &cache, None).unwrap().eval;
        println!("Eval: {}", eval);
        assert_eq!(eval, Evaluation::lost(b.half_move));

        let mut b =
            Board::from_fen("k6N/7R/6R1/8/8/8/8/K7 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(eval, Evaluation::m1(b.half_move));

        let mut b =
            Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 b - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::King);
        assert_eq!(best_move.from, b8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, -Evaluation::m2(b.half_move));
    }

    #[test]
    fn bishop_knight_mate() {
        let mut b =
            Board::from_fen("8/8/8/1B6/5N2/6K1/8/6k1 w - - 0 1").expect("failed to parse fen");
        let cache = TranspositionTable::<1024>::new();
        let res = b.best_move(4, 1, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        let best_move = b.from_algeabraic_unchecked(&best_move);
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert!(eval.mate_in(b.half_move).is_some());
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
        let res = board.best_move(6, 64, &cache, None).unwrap();
        let best_move = res.best_move;
        let eval = res.eval;

        let evalw = board.eval(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
            Color::White,
        );
        let evalb = board.eval(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
            Color::Black,
        );
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
        let eval = board.best_move(1, 1, &cache, None).unwrap().eval;
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
        let eval = board.eval(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
            Color::Black,
        );
        assert!(eval.0 < 0);
    }
    #[test]
    fn mate_in_2_format() {
        let fen = "6k1/p6p/3p2p1/3P1B2/2Q3n1/N1P5/Pr1B2P1/R3RK1q w - - 1 23";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        let cache = TranspositionTable::<1024>::new();
        let eval = board.best_move(6, 1, &cache, None).unwrap().eval;
        let Some(mated_in) = eval.mated_in(board.half_move) else {
            assert!(false);
            return;
        };
        assert_eq!(mated_in, 4);
    }
    #[test]
    fn mate_1_disconnect() {
        let fen = "6rk/p1p5/4BNQ1/4P3/4P3/2p2P2/6R1/3R3K w - - 1 39";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        let eval = board.it_depth_best_move(7, 32).eval;
        assert_eq!(eval, Evaluation::m1(board.half_move));
    }
    #[test]
    fn eval_bug4() {
        let fen = "r1b1k2r/pp1n3p/6pN/4pp2/3P3Q/8/2q1KPPP/3R1B1R w kq - 0 19";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        let cache = TranspositionTable::<1024>::new();
        let move_eval = board.best_move(6, 1, &cache, None).unwrap().eval;
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
54. Ra8+ {+3.98/9 5.0s} Nb8 {-4.00/9 5.0s} 55. Ra7 Nd7 *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let cache = TranspositionTable::<1024>::new();
        let move_eval = board.best_move(6, 1, &cache, None).unwrap().eval;

        let evalw = board.eval(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
            Color::White,
        );
        let evalb = board.eval(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
            Color::Black,
        );
        assert_ne!(evalw, Evaluation::draw());
        assert_ne!(evalb, Evaluation::draw());
        assert_eq!(move_eval, Evaluation::draw());
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
            is_en_passant: false,
            is_castle_queen: false,
            is_castle_king: false,
        });
        let best_score = Evaluation::lost(board.half_move);
        let cache = TranspositionTable::<1024>::new();
        let should_stop = AtomicBool::new(false);
        let eval = -board
            .pvs(
                Evaluation::lost(board.half_move),
                -best_score,
                4,
                &cache,
                &should_stop,
                ExpectedNodeType::PV,
            )
            .expect("Failed to resolve value")
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

        let cache = TranspositionTable::<1024>::new();
        let m = board
            .best_move(4, 32, &cache, None)
            .unwrap()
            .best_move
            .unwrap();
        assert!(m.to != c5());
    }

    #[test]
    fn many_moves() {
        let mut board = Board::from_fen(
            "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
        )
        .expect("Invalid fen?");
        let cache = TranspositionTable::<1024>::new();
        board.best_move(4, 32, &cache, None).unwrap();
    }

    #[test]
    fn many_moves2() {
        let mut board =
            Board::from_fen("rn2k2r/1b1p1p2/p2ppn2/1p1P3p/2P3q1/1PNBP3/P3R1PP/R4Q1K b Qkq - 0 1")
                .expect("Invalid fen?");
        let cache = TranspositionTable::<1024>::new();
        board.best_move(4, 64, &cache, None).unwrap();
    }
    #[test]
    fn eval_bug5() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2025.04.28"]
[Round "?"]
[White "gem"]
[Black "Human"]
[Result "0-1"]
[ECO "C20"]
[GameDuration "00:08:08"]
[GameEndTime "2025-04-28T23:01:03.980 PDT"]
[GameStartTime "2025-04-28T22:52:55.032 PDT"]
[Opening "King's pawn"]
[PlyCount "77"]
[Termination "adjudication"]
[TimeControl "inf"]
[Variation "Napoleon's Opening"]

1. e4 {+0.01/9 5.0s} e5 {7.5s} 2. Qf3 {+0.16/9 5.0s} Qh4 {7.2s}
3. Qe3 {+0.20/8 5.0s} Nc6 {6.6s} 4. Nf3 {+0.35/9 5.0s} Qf6 {6.4s}
5. Bb5 {+0.24/8 5.0s} Nd4 {9.4s} 6. Nxd4 {+0.14/9 5.0s} exd4 {7.2s}
7. Qd3 {+0.21/8 5.0s} c6 {7.1s} 8. Bc4 {-0.09/8 5.0s} d5 {5.7s}
9. exd5 {-0.04/9 5.0s} b5 {6.2s} 10. Bb3 {+0.39/9 5.0s} Be7 {7.0s}
11. dxc6 {+0.70/9 5.0s} Qxc6 {7.0s} 12. O-O {+0.81/8 5.0s} Nf6 {5.6s}
13. Na3 {+0.36/8 5.0s} O-O {7.5s} 14. Nxb5 {+0.84/9 5.0s} Bb7 {7.9s}
15. f3 {+0.82/8 5.0s} Qb6 {5.8s} 16. Nxd4 {+1.50/8 5.0s} Rad8 {6.8s}
17. c3 {+1.11/9 5.0s} Bc5 {7.8s} 18. Re1 {+0.86/8 5.0s} Bxd4+ {6.7s}
19. cxd4 {+0.83/10 5.0s} Rfe8 {6.8s} 20. Rxe8+ {+1.22/9 5.0s} Rxe8 {5.9s}
21. Rb1 {+1.24/10 5.0s} Nd5 {9.2s} 22. Bxd5 {+0.99/9 5.0s} Bxd5 {7.6s}
23. a3 {+0.54/9 5.0s} Qf6 {8.2s} 24. b3 {+0.29/10 5.0s} Re1+ {8.7s}
25. Kf2 {-0.62/10 5.0s} Rd1 {6.0s} 26. Qc2 {+1.83/9 5.0s} Qh4+ {6.7s}
27. Ke2 {+0.26/11 5.0s} Qe1+ {8.0s} 28. Kd3 {-2.05/11 5.0s} Bb7 {8.5s}
29. Qc5 {+1.02/10 5.0s} Qe6 {10s} 30. Qxa7 {+2.04/9 5.0s} Bc8 {8.3s}
31. Qa4 {+2.18/9 5.0s} Re1 {9.4s} 32. b4 {-1.52/10 5.1s} Qg6+ {6.5s}
33. Kc3 {-2.62/11 5.0s} Qxb1 {7.6s} 34. Bb2 {-2.91/10 5.0s} Re7 {8.7s}
35. Qa5 {-2.64/11 5.0s} Re8 {8.4s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let cache = TranspositionTable::<DEFAULT_TT_SIZE>::new();
        let res = board.best_move(8, 32, &cache, None).unwrap();

        assert!(!res.eval.mate());
    }
    #[test]
    fn eval_bug6() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2025.06.25"]
[Round "10"]
[White "gem"]
[Black "gem_prev"]
[Result "0-1"]
[ECO "E12"]
[GameDuration "00:00:19"]
[GameEndTime "2025-06-25T22:45:56.804 PDT"]
[GameStartTime "2025-06-25T22:45:37.093 PDT"]
[Opening "Queen's Indian"]
[PlyCount "70"]
[Termination "abandoned"]
[TimeControl "5+0.2"]
[Variation "4.Nc3"]

1. d4 {book} Nf6 {book} 2. c4 {book} e6 {book}
3. Nf3 {book} b6 {book} 4. Nc3 {+1.17/7 0.46s} Bb7 {-0.75/6 0.41s}
5. g3 {+1.81/6 0.41s} Bb4 {-0.89/6 0.42s} 6. Bd2 {+1.41/6 0.41s} Nc6 {-0.23/6 0.37s}
7. a3 {+0.23/5 0.38s} Bxc3 {-0.71/6 0.41s} 8. Bxc3 {+1.59/6 0.41s} Ne4 {-0.09/6 0.36s}
9. Qc2 {+0.09/5 0.36s} Nxc3 {-2.38/5 0.36s} 10. Qxc3 {+1.60/6 0.33s} Qf6 {+1.58/6 0.41s}
11. Rc1 {+2.04/6 0.36s} Nxd4 {+10.41/6 0.32s} 12. Bg2 {-6.24/6 0.34s} c5 {+5.85/5 0.35s}
13. h4 {-7.72/5 0.35s} O-O {+12.52/6 0.36s} 14. Rh3 {-7.92/5 0.29s} Bc6 {+8.27/5 0.32s}
15. Kf1 {-5.60/6 0.31s} Rad8 {+10.88/6 0.31s} 16. Rd1 {-5.19/6 0.33s} Bxf3 {+3.93/5 0.31s}
17. Bxf3 {-4.65/6 0.28s} Qe5 {+9.64/6 0.28s} 18. b4 {-8.07/5 0.27s} Nxf3 {+4.47/5 0.32s}
19. Qxf3 {-5.28/6 0.34s} d5 {+5.36/7 0.28s} 20. cxd5 {-5.91/6 0.27s} Rxd5 {+6.50/7 0.29s}
21. Rxd5 {-6.74/6 0.27s} Qxd5 {+7.14/6 0.26s} 22. Qxd5 {-5.45/6 0.27s} exd5 {+8.31/8 0.28s}
23. bxc5 {-7.02/8 0.26s} bxc5 {+9.24/8 0.25s} 24. g4 {-7.84/8 0.31s} Rb8 {+7.84/7 0.29s}
25. Rd3 {-7.91/7 0.39s} d4 {+7.91/6 0.28s} 26. Rd1 {-6.27/6 0.24s} Rb3 {+8.06/6 0.29s}
27. e3 {-7.03/6 0.23s} d3 {+7.99/7 0.26s} 28. Rc1 {-9.65/7 0.24s} Rxa3 {+9.65/6 0.25s}
29. Rxc5 {-7.44/7 0.22s} h6 {+4.45/7 0.26s} 30. Ke1 {-5.26/7 0.25s} Ra2 {+4.86/7 0.23s}
31. Rd5 {-4.07/7 0.22s} Ra1+ {+4.96/8 0.24s} 32. Kd2 {-4.07/7 0.23s} Ra2+ {+4.96/6 0.23s}
33. Ke1 {-3.85/7 0.29s} Re2+ {+5.15/6 0.23s} 34. Kf1 {-1.56/7 0.21s} Rd2 {+1.56/6 0.21s}
35. Ke1 {-1.20/7 0.21s} Re2+ {0.00/7 0.21s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let (_, eval, _) = board.search_best_move_for(Duration::from_millis(300), 16);

        assert!(!eval.mate());
    }
    #[test]
    fn eval_bug7() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2025.06.27"]
[Round "100"]
[White "gem"]
[Black "gem_prev"]
[Result "0-1"]
[ECO "B02"]
[GameDuration "00:00:42"]
[GameEndTime "2025-06-27T18:50:12.457 PDT"]
[GameStartTime "2025-06-27T18:49:30.000 PDT"]
[Opening "Alekhine's defense"]
[PlyCount "102"]
[Termination "stalled connection"]
[TimeControl "5+0.2"]

1. e4 {book} Nf6 {book} 2. e5 {book} Nd5 {book} 3. c4 {book} Nb6 {book}
4. d4 {+3.92/6 0.37s} d6 {-2.53/6 0.37s} 5. Nf3 {+2.53/5 0.37s}
g6 {-1.61/6 0.36s} 6. Nc3 {+2.06/6 0.35s} Bg7 {-0.73/6 0.35s}
7. b3 {+0.78/5 0.35s} Nc6 {-1.17/5 0.35s} 8. Qe2 {-0.07/5 0.34s}
dxe5 {+0.17/5 0.34s} 9. dxe5 {-1.17/5 0.33s} Nd4 {+1.42/5 0.33s}
10. Nxd4 {-3.29/5 0.33s} Qxd4 {-0.45/5 0.32s} 11. Bb2 {-4.64/5 0.32s}
Qxe5 {+1.42/5 0.32s} 12. Qxe5 {-3.77/5 0.31s} Bxe5 {+2.16/5 0.31s}
13. Rd1 {-4.54/5 0.31s} Be6 {+1.92/5 0.31s} 14. g3 {-5.11/5 0.30s}
Rd8 {+3.54/5 0.30s} 15. Rxd8+ {-2.49/5 0.29s} Kxd8 {+2.49/5 0.29s}
16. Bg2 {-4.64/5 0.29s} Kc8 {+3.78/5 0.29s} 17. c5 {-5.78/5 0.29s}
Nd7 {+4.96/5 0.29s} 18. c6 {-4.57/5 0.28s} bxc6 {+4.08/5 0.28s}
19. f4 {-7.26/5 0.28s} Bd4 {+5.70/5 0.28s} 20. Bxc6 {-8.21/5 0.27s}
Rd8 {+6.75/5 0.27s} 21. Bb5 {-8.41/5 0.27s} Kb7 {+8.26/5 0.27s}
22. Be2 {-9.86/5 0.27s} c6 {+8.20/5 0.27s} 23. Kd2 {-10.60/5 0.27s}
Nc5 {+9.92/5 0.26s} 24. Rf1 {-10.77/5 0.26s} Bg1+ {+11.18/5 0.26s}
25. Ke1 {-11.96/5 0.26s} Bxh2 {+11.94/5 0.26s} 26. Kf2 {-13.79/5 0.25s}
Rd2 {+12.18/5 0.25s} 27. Ba3 {-9.52/5 0.25s} Bh3 {+9.52/5 0.25s}
28. Rd1 {-8.54/5 0.25s} Rxd1 {+9.18/5 0.25s} 29. Bxd1 {-9.18/5 0.24s}
Nd3+ {+7.32/5 0.24s} 30. Kf3 {-9.18/5 0.24s} Ne1+ {+9.18/5 0.24s}
31. Kf2 {-9.18/5 0.24s} Nd3+ {+6.89/5 0.24s} 32. Kf3 {-6.89/5 0.24s}
e5 {+5.33/5 0.24s} 33. Be2 {-5.33/5 0.24s} Ne1+ {+5.39/5 0.24s}
34. Kf2 {-5.39/5 0.23s} exf4 {+6.80/5 0.23s} 35. Kxe1 {-6.80/5 0.23s}
Bxg3+ {+6.40/5 0.23s} 36. Kd2 {-6.40/5 0.23s} Bg2 {+4.73/5 0.23s}
37. Bc4 {-3.85/5 0.23s} g5 {+2.10/5 0.23s} 38. Be7 {-2.36/5 0.23s}
f6 {+1.78/5 0.23s} 39. Bxf6 {-2.24/5 0.23s} h6 {+0.03/5 0.23s}
40. Bd3 {+0.01/5 0.23s} f3 {+0.94/5 0.23s} 41. Ke3 {+0.12/5 0.22s}
Bf4+ {+1.45/5 0.22s} 42. Kf2 {-1.45/5 0.22s} g4 {+0.59/5 0.22s}
43. Ne4 {-1.25/5 0.22s} h5 {+0.52/5 0.22s} 44. Ng3 {+0.32/5 0.22s}
h4 {-2.13/5 0.22s} 45. Bxh4 {+3.20/5 0.22s} Ka8 {-4.04/5 0.22s}
46. Bf5 {+4.04/4 0.22s} Bh3 {-7.21/5 0.22s} 47. Nh5 {+5.76/5 0.22s}
Bd2 {-6.73/5 0.22s} 48. Be7 {+8.01/5 0.22s} Bc3 {-8.01/4 0.22s}
49. Kg3 {+8.01/4 0.22s} Be1+ {-4.30/5 0.22s} 50. Kf4 {+4.30/5 0.21s}
Bd2+ {-4.30/4 0.21s} 51. Kg3 {0.00/5 0.21s}
Be1+ {0.00/5 0.21s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let (_, eval, _) = board.search_best_move_for(Duration::from_millis(100), 16);

        assert!(!eval.mate());
    }

    #[test]
    fn eval_bug8() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2025.06.28"]
[Round "1"]
[White "gem_prev"]
[Black "gem"]
[Result "1-0"]
[ECO "B54"]
[GameDuration "00:00:08"]
[GameEndTime "2025-06-28T12:25:55.929 PDT"]
[GameStartTime "2025-06-28T12:25:47.608 PDT"]
[Opening "Sicilian"]
[PlyCount "25"]
[Termination "abandoned"]
[TimeControl "5+0.2"]

1. e4 {book} c5 {book} 2. Nf3 {book} d6 {book} 3. d4 {book} cxd4 {book}
4. Nxd4 {+2.93/6 0.38s} a6 {-0.78/8 1.2s} 5. Nc3 {+1.90/5 0.36s}
g6 {-1.16/8 0.52s} 6. Bc4 {+0.71/5 0.35s} Nf6 {-1.08/8 0.81s}
7. O-O {+0.80/5 0.35s} Bg7 {-2.09/7 0.31s} 8. Nd5 {+1.22/5 0.34s}
Nc6 {-1.50/7 0.49s} 9. Nxc6 {+1.50/5 0.33s} bxc6 {-1.35/7 0.40s}
10. Nxf6+ {+1.35/5 0.32s} Bxf6 {+0.11/8 0.31s} 11. Rb1 {+0.10/5 0.32s}
Qa5 {-1.58/7 0.29s} 12. b4 {-0.11/5 0.31s} Qe5 {-2.36/7 0.40s}
13. Bb2 {+0.03/5 0.31s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let (_, eval, _) = board.search_best_move_for(Duration::from_millis(400), 16);

        assert!(!eval.mate());
    }
    #[test]
    fn eval_bug9() {
        let pgn = r###"
[Event "?"]
[Site "?"]
[Date "2025.06.28"]
[Round "1"]
[White "gem_prev"]
[Black "gem"]
[Result "1-0"]
[ECO "B54"]
[GameDuration "00:00:08"]
[GameEndTime "2025-06-28T21:20:32.782 PDT"]
[GameStartTime "2025-06-28T21:20:24.196 PDT"]
[Opening "Sicilian"]
[PlyCount "31"]
[Termination "abandoned"]
[TimeControl "5+0.2"]

1. e4 {book} c5 {book} 2. Nf3 {book} d6 {book} 3. d4 {book} cxd4 {book}
4. Nxd4 {+2.93/6 0.38s} a6 {-2.18/7 0.39s} 5. Nc3 {+1.90/5 0.36s}
g6 {-1.83/7 0.38s} 6. Be3 {+2.54/6 0.36s} Bg7 {-0.97/8 0.39s}
7. Qd2 {+3.39/6 0.35s} Bd7 {-2.23/7 0.38s} 8. O-O-O {+3.55/6 0.34s}
Nc6 {+1.07/7 0.37s} 9. Nxc6 {+1.26/5 0.33s} Bxc6 {+1.15/7 0.36s}
10. Bc4 {+1.43/5 0.32s} Nf6 {+2.13/7 0.33s} 11. f3 {+1.13/5 0.32s}
b5 {+2.42/7 0.33s} 12. Bb3 {+1.36/5 0.31s} b4 {+2.10/7 0.32s}
13. Nd5 {+2.71/5 0.31s} Bxd5 {+2.42/7 0.32s} 14. Ba4+ {+3.26/5 0.30s}
Kf8 {+1.21/7 0.31s} 15. exd5 {+0.90/5 0.29s} Qa5 {+1.61/7 0.30s}
16. Bb3 {+0.74/5 0.29s} *"###;
        let mut board = Board::from_pgn(pgn).expect("bad pgn?");
        let (_, eval, _) = board.search_best_move_for(Duration::from_millis(10000), 32);
        assert!(!eval.mate());
    }
}
