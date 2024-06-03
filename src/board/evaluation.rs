pub use crate::board::*;
use std::fmt;
use std::ops::Neg;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy)]
pub struct Evaluation(i64);

// An evaluation is simply an i64 with a few caveats:
// - We limit the range to (std::i64::MIN, std::i64::MAX] to not run into negation errors
// - We treat i64::MAX as having won the game, i64::MAX - 1 as mate in 1, i64::MAX -2 as mate in
//   2 and so on.
// - We treat i64::MIN as having lost the game, i64::MIN + 1 as the opponent having mate in 1,
//   i64::MAX -2 as the opponent having mate in 2 and so on.
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
    fn is_mate(&self) -> bool {
        self.0 >= Self::won().0 - 100
    }
    fn dec_mate(&self) -> Evaluation {
        Evaluation(self.0 - 1)
    }
    fn is_mated(&self) -> bool {
        self.0 <= Self::lost().0 + 100
    }
    fn dec_mated(&self) -> Evaluation {
        Evaluation(self.0 + 1)
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
            write!(f, "{}", self.0)?;
        }
        Ok(())
    }
}

impl Board {
    pub fn best_move(&mut self) -> (Option<Move>, Evaluation) {
        let moves = generate_pseudo_legal_moves(self);
        let mut best_move = None;
        let mut best_score = None;
        const DEFAULT_DEPTH: usize = 4;

        for m in &moves {
            if Some(Piece::King) == m.capture {
                return (None, Evaluation::won());
            }
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                let score = -self.nega_max(DEFAULT_DEPTH);
                if let Some(prev_best) = best_score {
                    if score > prev_best {
                        best_score = Some(score);
                        best_move = Some(m)
                    }
                } else {
                    best_score = Some(score);
                    best_move = Some(m);
                }
            }
            self.undo_move(&m);
        }
        if let Some(score) = best_score {
            let adjusted_max = if score.is_mate() {
                score.dec_mate()
            } else if score.is_mated() {
                score.dec_mated()
            } else {
                score
            };
            (best_move.copied(), adjusted_max)
        } else {
            // We have no legal moves. If we are in check, it's checkmate. If not, it's stalemate
            if self.in_check(self.to_play) {
                (None, Evaluation::lost())
            } else {
                (None, Evaluation::draw())
            }
        }
    }

    pub fn nega_max(&mut self, depth: usize) -> Evaluation {
        if depth == 0 {
            return self.eval(self.to_play);
        }
        let mut max = None;
        let moves = generate_pseudo_legal_moves(self);
        for m in &moves {
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                let eval = -self.nega_max(depth-1);
                if let Some(prev) = max {
                    max = Some(std::cmp::max(eval, prev));
                } else {
                    max = Some(eval);
                }
            }
            self.undo_move(&m);
        }
        if let Some(max) = max {
            if max.is_mate() {
                max.dec_mate()
            } else if max.is_mated() {
                max.dec_mated()
            } else {
                max
            }
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

        let materialistic = 2000 * king_diff
            + 90 * queen_diff
            + 50 * rook_diff
            + 31 * bishop_diff
            + 30 * knight_diff
            + 1 * pawn_diff;
        match to_play {
            Color::Black => -Evaluation(materialistic),
            Color::White => Evaluation(materialistic),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;
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
        let (best_move, _) = b.best_move();
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
        let (best_move, _) = b.best_move();
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
        let mut b = Board::from_fen("1k6/ppp5/8/8/8/8/8/K6R w - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
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
        let mut b = Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 w - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::won());
    }
    #[test]
    fn lost() {
        let mut b = Board::from_fen("1k5R/ppp5/8/8/8/8/8/K7 b - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::lost());
    }
    #[test]
    fn stalemate() {
        let mut b = Board::from_fen("k7/2Q5/8/8/8/8/8/K7 b - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
        assert!(best_move.is_none());
        assert_eq!(eval, Evaluation::draw());
    }
    #[test]
    fn draw() {
        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 b - - 0 1")
            .expect("failed to parse fen");
        let (_, eval) = b.best_move();
        assert_eq!(eval, Evaluation::draw());

        let mut b = Board::from_fen("k7/8/8/8/8/8/8/K7 w - - 0 1")
            .expect("failed to parse fen");
        let (_, eval) = b.best_move();
        assert_eq!(eval, Evaluation::draw());
    }
    #[test]
    fn mates() {
        let mut b = Board::from_fen("1k6/pppr4/8/8/8/8/8/K6R w - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, h1());
        assert_eq!(best_move.to, h8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m3());

        let mut b = Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 w - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
        println!("Eval: {}", eval);
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Rook);
        assert_eq!(best_move.from, g6());
        assert_eq!(best_move.to, g8());
        assert_eq!(best_move.capture, None);
        assert_eq!(eval, Evaluation::m1());
        
        let mut b = Board::from_fen("1k5N/7R/6R1/8/8/8/8/K7 b - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
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
        let mut b = Board::from_fen("8/8/8/1B6/5N2/6K1/8/6k1 w - - 0 1")
            .expect("failed to parse fen");
        let (best_move, eval) = b.best_move();
        assert!(best_move.is_some());
        let best_move = best_move.unwrap();
        println!("Best Move: {}", best_move);
        println!("Eval: {}", eval);
        assert!(eval.is_mate());
    }
}
