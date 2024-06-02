pub use crate::board::*;

impl Board {
    pub fn best_move(&mut self) -> Option<Move> {
        let moves = generate_pseudo_legal_moves(self);
        let mut best_move = None;
        let mut best_score = std::i64::MIN;
        const DEFAULT_DEPTH: usize = 4;

        for m in &moves {
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                let score = -self.nega_max(DEFAULT_DEPTH);
                println!("Move: {}, Score: {}", m, score);
                if score > best_score {
                    best_score = score;
                    best_move = Some(m)
                }
            }
            self.undo_move(&m);
        }
        best_move.copied()
    }

    pub fn nega_max(&mut self, depth: usize) -> i64 {
        if depth == 0 {
            return self.eval(self.to_play);
        }
        let mut max = std::i64::MIN;
        let moves = generate_pseudo_legal_moves(self);
        for m in &moves {
            self.make_move(&m);
            if !self.in_check(!self.to_play) {
                max = std::cmp::max(self.nega_max(depth - 1), max);
            }
            self.undo_move(&m);
        }
        return max;
    }

    pub fn eval(&self, to_play: Color) -> i64 {
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
            Color::Black => -materialistic,
            Color::White => materialistic,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;
    #[test]
    fn white_better() {
        let b = Board::from_fen("rkb1kbkr/pppppppp/8/8/8/8/PPPPPPPP/RKBQKBKR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white > 0);
        assert!(eval_black < 0);
        assert!(eval_black == -eval_white)
    }
    #[test]
    fn black_better() {
        let b = Board::from_fen("rkbqkbkr/pppppppp/8/8/8/8/PPPPPPPP/RKB1KBKR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white < 0);
        assert!(eval_black > 0);
        assert!(eval_black == -eval_white)
    }
    #[test]
    fn find_queen_take() {
        let mut b = Board::from_fen("4k3/pppppppp/8/8/7q/8/PPPPPPP1/RKBQKBKR w - - 0 1")
            .expect("failed to parse fen");
        let best_move = b.best_move().unwrap();
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
        let best_move = b.best_move().unwrap();
        println!("Best Move: {}", best_move);
        assert_eq!(best_move.piece, Piece::Pawn);
        assert_eq!(best_move.from, f7());
        assert_eq!(best_move.to, e6());
        assert_eq!(best_move.capture, Some(Piece::Bishop));
    }
}
