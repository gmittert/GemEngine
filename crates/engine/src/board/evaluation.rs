use crate::board::*;
use std::cmp::max;
use std::fmt;
use std::ops::{Add, AddAssign, Neg, Sub};
use std::time::Duration;

use super::sliding_attacks::{compute_bishop_attacks, compute_rook_attacks};

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy, Default)]
pub struct Evaluation(pub i16);

pub const PIECE_VALUES: [Evaluation; 6] = [
    Evaluation(100),   // Pawn
    Evaluation(300),   // Knight
    Evaluation(310),   // Bishop
    Evaluation(500),   // Rook
    Evaluation(900),   // Queen
    Evaluation(10000), // King
];

// An evaluation is simply an i64 with a few caveats:
// - We limit the range to (i16::MIN, i16::MAX] to not run into negation errors
// - Evaluation maxes out at +/-20000 centipawns.
// - Beyond that, we use that to express that we found mate at <ply>
impl Evaluation {
    pub fn won(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - current_ply as i16)
    }
    pub fn draw() -> Evaluation {
        Evaluation(0)
    }
    pub fn lost(current_ply: u16) -> Evaluation {
        Evaluation(i16::MIN + 1 + current_ply as i16)
    }
    pub fn m1(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - 1 - current_ply as i16)
    }
    pub fn m2(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - 2 - current_ply as i16)
    }
    pub fn m3(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - 3 - current_ply as i16)
    }
    pub fn m4(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - 4 - current_ply as i16)
    }
    pub fn m5(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - 5 - current_ply as i16)
    }
    pub fn m6(current_ply: u16) -> Evaluation {
        Evaluation(i16::MAX - 6 - current_ply as i16)
    }
    pub fn mate(&self) -> bool {
        self.0 > 20000 || self.0 < -20000
    }

    pub fn mate_in(&self, current_ply: u16) -> Option<usize> {
        if self.0 > 20000 {
            Some((Self::won(current_ply).0 - self.0) as usize)
        } else {
            None
        }
    }
    pub fn mated_in(&self, current_ply: u16) -> Option<usize> {
        if self.0 < -20000 {
            Some((self.0 - Self::lost(current_ply).0) as usize)
        } else {
            None
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
        if self.0 > 20000 {
            write!(f, "M: {} plies", i16::MAX - self.0)?;
        } else if self.0 < -20000 {
            write!(f, "-M: {} plies", self.0 - (i16::MIN + 1))?;
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
    pub fn eval(&mut self, to_play: Color) -> Evaluation {
        while let Some(update) = self.unapplied_updates.pop() {
            nnue_features::apply(update, &mut self.white_features, &mut self.black_features);
        }

        Evaluation(match to_play {
            Color::Black => nnue::NNUE.evaluate(&self.black_features, &self.white_features),
            Color::White => nnue::NNUE.evaluate(&self.white_features, &self.black_features),
        } as i16)
    }

    pub fn get_least_valuable_piece(
        &self,
        attadef: BitBoard,
        side: Color,
    ) -> Option<(Posn, Piece)> {
        let pieces = [
            Piece::Pawn,
            Piece::Knight,
            Piece::Bishop,
            Piece::Rook,
            Piece::Queen,
            Piece::King,
        ];
        for piece in pieces {
            let subset = attadef & self.piece(side, piece);
            if !subset.is_empty() {
                return Some((subset.into_iter().next().unwrap(), piece));
            }
        }
        None
    }

    pub fn get_smallest_attacker(&self, p: Posn, side: Color) -> Option<AlgebraicMove> {
        self.pawn_can_capture(side, p)
            .or(self.knight_can_capture(side, p))
            .or(self.bishop_can_capture(side, p))
            .or(self.rook_can_capture(side, p))
            .or(self.queen_can_capture(side, p))
            .or(self.king_can_capture(side, p))
    }

    pub fn static_exchange_evaluation(
        &mut self,
        to_square: Posn,
        target: Piece,
        from_square: Posn,
        attack_piece: Piece,
    ) -> Evaluation {
        let mut gain: [Evaluation; 32] = [Evaluation::draw(); 32];
        let mut attack_piece = attack_piece;
        let mut d = 0;
        let mut to_play = self.to_play;
        let may_x_ray = self.piece(Color::White, Piece::Pawn)
            | self.piece(Color::Black, Piece::Pawn)
            | self.piece(Color::White, Piece::Bishop)
            | self.piece(Color::Black, Piece::Bishop)
            | self.piece(Color::White, Piece::Rook)
            | self.piece(Color::Black, Piece::Rook)
            | self.piece(Color::White, Piece::Queen)
            | self.piece(Color::Black, Piece::Queen);

        let mut from_set = BitBoard::from(from_square);
        let mut occ = self.pieces();
        let mut attadef = Self::king_attacks_pos(self.piece(Color::White, Piece::King), to_square)
            | Self::king_attacks_pos(self.piece(Color::Black, Piece::King), to_square)
            | Self::rook_attacks_pos(
                self.piece(Color::White, Piece::Rook) | self.piece(Color::White, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::rook_attacks_pos(
                self.piece(Color::Black, Piece::Rook) | self.piece(Color::Black, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::bishop_attacks_pos(
                self.piece(Color::White, Piece::Bishop) | self.piece(Color::White, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::bishop_attacks_pos(
                self.piece(Color::Black, Piece::Bishop) | self.piece(Color::Black, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::knight_attacks_pos(self.piece(Color::White, Piece::Knight), to_square)
            | Self::knight_attacks_pos(self.piece(Color::Black, Piece::Knight), to_square)
            | Self::pawn_attacks_pos(
                self.piece(Color::White, Piece::Pawn),
                to_square,
                Color::White,
            )
            | Self::pawn_attacks_pos(
                self.piece(Color::Black, Piece::Pawn),
                to_square,
                Color::Black,
            );
        gain[d] = PIECE_VALUES[target as usize];
        loop {
            d += 1; // next depth and side
            to_play = !to_play;
            gain[d] = PIECE_VALUES[attack_piece as usize] - gain[d - 1]; // speculative store, if defended
            attadef ^= from_set; // reset bit in set to traverse
            occ ^= from_set; // reset bit in temporary occupancy (for x-Rays)
            if !(from_set & may_x_ray).is_empty() {
                attadef |= self.consider_xrays(occ, to_square);
            }
            if let Some((from, piece)) = self.get_least_valuable_piece(attadef, to_play) {
                from_set = BitBoard::from(from);
                attack_piece = piece;
            } else {
                break;
            }
        }
        d -= 1;
        while d > 0 {
            gain[d - 1] = -max(-gain[d - 1], gain[d]);
            d -= 1;
        }
        gain[0]
    }

    pub fn consider_xrays(&self, occ: BitBoard, target: Posn) -> BitBoard {
        let rook_queen_attacks = compute_rook_attacks(target, occ);
        let bishop_queen_attacks = compute_bishop_attacks(target, occ);

        let revealed_rook_queens = (self.piece(Color::White, Piece::Queen)
            | self.piece(Color::Black, Piece::Queen)
            | self.piece(Color::White, Piece::Rook)
            | self.piece(Color::Black, Piece::Rook))
            & rook_queen_attacks
            & occ;
        let revealed_bishop_queens = (self.piece(Color::White, Piece::Queen)
            | self.piece(Color::Black, Piece::Queen)
            | self.piece(Color::White, Piece::Bishop)
            | self.piece(Color::Black, Piece::Bishop))
            & bishop_queen_attacks
            & occ;
        revealed_bishop_queens | revealed_rook_queens
    }
}

#[cfg(test)]
mod tests {
    use crate::board::evaluation::*;

    #[test]
    fn white_better() {
        let mut b = Board::from_fen("rnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white.0 > 0);
        assert!(eval_black.0 < 0);
    }

    #[test]
    fn black_better() {
        let mut b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white.0 < 0);
        assert!(eval_black.0 > 0);
    }

    #[test]
    fn eval_starting_board() {
        let mut b = starting_board();
        let eval_white = b.eval(Color::White);
        let eval_black = b.eval(Color::Black);
        assert!(eval_white.0 < 100 && eval_white.0 > -100);
        assert!(eval_black.0 < 100 && eval_black.0 > -100);
    }

    #[test]
    fn eval_starting_board_e4() {
        let mut b = starting_board();
        let eval_white_before = b.eval(Color::White);
        b.make_move(&Move {
            from: e2(),
            to: e4(),
            piece: Piece::Pawn,
            capture: None,
            promotion: None,
            is_en_passant: false,
            is_castle_queen: false,
            is_castle_king: false,
        });
        let eval_white_after = b.eval(Color::White);
        dbg!(eval_white_before);
        dbg!(eval_white_after);
        assert!(eval_white_before < eval_white_after);
    }

    #[test]
    fn eval_flipped() {
        assert_eq!(Evaluation::won(0), -Evaluation::lost(0));
        assert_eq!(Evaluation::lost(0), -Evaluation::won(0));
    }
    #[test]
    fn eval_formatted() {
        assert_eq!("M: 33 plies", format!("{}", Evaluation::m1(32)));
        assert_eq!("-M: 33 plies", format!("{}", -Evaluation::m1(32)));
        assert_eq!("M: 34 plies", format!("{}", Evaluation::m2(32)));
        assert_eq!("-M: 34 plies", format!("{}", -Evaluation::m2(32)));
    }

    #[test]
    fn mated_in_formatting() {
        assert_eq!(Some(1), Evaluation::m1(31).mate_in(31));
        assert_eq!(Some(1), (-Evaluation::m1(31)).mated_in(31));
        assert_eq!(Some(2), Evaluation::m2(31).mate_in(31));
        assert_eq!(Some(2), (-Evaluation::m2(31)).mated_in(31));
    }

    #[test]
    fn see_simple() {
        let mut board = Board::from_fen("1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - - 0 1")
            .expect("Invalid fen?");
        assert_eq!(
            Evaluation(100),
            board.static_exchange_evaluation(e5(), Piece::Pawn, e1(), Piece::Rook)
        );
    }

    #[test]
    fn see_med() {
        let mut board = Board::from_fen("1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - - 0 1")
            .expect("Invalid fen?");
        assert_eq!(
            Evaluation(-200),
            board.static_exchange_evaluation(e5(), Piece::Pawn, d3(), Piece::Knight)
        );
    }

    #[test]
    fn repetition() {
        let mut board =
            Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 23 1")
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

        assert!(board.has_draw());
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
    fn draw_50_move() {
        let pgn = r###"
[Event "?"]

1. d4 d5 2. Kd2 Kd7 3. Ke3 Ke6 4. Kf3 Kf6 5. Kg3 Kg6 6. Nf3 Nf6 7. Nc3 Nc6 8. Qd2 Qd7 9. Qd3+ Kh5 10. Kf4 Na5 11. Nb5 Nb3 12. Na3 Nc5 13. Nc4 Na6 14. Na3 Nb4 15. Nb5 Nc6 16. Nc3 Qd6+ 17. Ke3 Bh3 18. Bd2 Bg4 19. Bc1 Bf5 20. Bd2 Be6 21. Bc1 Bd7 22. Bd2 Be8 23. Bc1 Bd7 24. Na4 Be6 25. Bd2 Bf5 26. Bc3 Be6 27. Bb4 Bf5 28. Ba5 Be6 29. Be1 Bd7 30. Bb4 Bc8 31. Ba5 Bd7 32. Rb1 Bc8 33. Ra1 Be6 34. Rb1 Bc8 35. Rc1 Bf5 36. Rb1 Bg4 37. Rd1 Bf5 38. Rc1 Bh3 39. Re1 Bg4 40. Bb4 Bf5 41. Rd1 Be6 42. Rc1 Bf5 43. Re1 Bc8 44. Rb1 Bd7 45. Re1 Bh3 46. Ra1 Bg4 47. Rg1 Bc8 48. Ba5 Bg4 49. Bd2 Be6 50. Be1 Bc8 51. Bc3 Bd7 *"###;
        let board = Board::from_pgn(pgn).expect("bad pgn?");
        assert!(board.has_draw());
    }
}
