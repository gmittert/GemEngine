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
    #[cfg(feature = "nnue")]
    pub fn eval(&self, _alpha: Evaluation, _beta: Evaluation, to_play: Color) -> Evaluation {
        Evaluation(match to_play {
            Color::Black => nnue::NNUE.evaluate(&self.black_features, &self.white_features),
            Color::White => nnue::NNUE.evaluate(&self.white_features, &self.black_features),
        } as i16)
    }

    #[cfg(not(feature = "nnue"))]
    #[tracing::instrument(skip(self))]
    pub fn eval(&self, alpha: Evaluation, beta: Evaluation, to_play: Color) -> Evaluation {
        let mg_score =
            self.mg_piece_values[to_play as usize] - self.mg_piece_values[!to_play as usize];
        let eg_score =
            self.eg_piece_values[to_play as usize] - self.eg_piece_values[!to_play as usize];
        let mg_phase: i32 = self.game_phase as i32;
        let eg_phase: i32 = 24 - mg_phase;
        // It's okay to cast back to an i16 here:
        //
        // We're taking a weighted average of eg and mg scores here. The intermediate computation
        // may overflow an i16, but won't once we divide by 24.
        let phase1_eval =
            (((mg_score as i32 * mg_phase) + (eg_score as i32 * eg_phase)) / 24) as i16;

        tracing::event!(
            tracing::Level::INFO,
            name = "Phase1 eval",
            eval = phase1_eval,
            alpha = alpha.0,
            beta = beta.0,
            mg_score,
            eg_score,
            mg_phase,
            eg_phase
        );
        // Lazily evaluate the more expensive parts. If we're already too far out of range of alpha
        // and beta, don't bother trying to compute the minutia.
        if alpha.0 as i32 - phase1_eval as i32 > 200 {
            tracing::event!(tracing::Level::INFO, name = "Alpha too high");
            return Evaluation(phase1_eval);
        }
        if phase1_eval as i32 - beta.0 as i32 > 200 {
            tracing::event!(tracing::Level::INFO, name = "Beta too low");
            return Evaluation(phase1_eval);
        }

        let attacks_white = Self::rook_attacks(
            self.piece(Color::White, Piece::Rook)
            | self.piece(Color::White, Piece::Queen),
            self.pieces(),
        ) | Self::bishop_attacks(
            self.piece(Color::White, Piece::Bishop)
            | self.piece(Color::White, Piece::Queen),
            self.pieces(),
        ) | Self::king_attacks(self.piece(Color::White, Piece::King))
            | Self::pawn_attacks(self.piece(Color::White, Piece::Pawn), Color::White)
            | Self::knight_attacks(self.piece(Color::White, Piece::Knight));

        let attacks_black = Self::rook_attacks(
            self.piece(Color::Black, Piece::Rook)
            | self.piece(Color::Black, Piece::Queen),
            self.pieces(),
        ) | Self::bishop_attacks(
            self.piece(Color::Black, Piece::Bishop)
            | self.piece(Color::Black, Piece::Queen),
            self.pieces(),
        ) | Self::king_attacks(self.piece(Color::Black, Piece::King))
            | Self::pawn_attacks(self.piece(Color::Black, Piece::Pawn), Color::Black)
            | Self::knight_attacks(self.piece(Color::Black, Piece::Knight));

        let attacks_diff = attacks_white.len() as i16 - attacks_black.len() as i16;
        let doubled_pawns =
            self.doubled_pawns(Color::White) as i16 - self.doubled_pawns(Color::Black) as i16;
        let isolated_pawns =
            self.isolated_pawns(Color::White) as i16 - self.isolated_pawns(Color::Black) as i16;
        let blocked_pawns =
            self.blocked_pawns(Color::White) as i16 - self.blocked_pawns(Color::Black) as i16;
        let eval_refinements =
            attacks_diff - 10 * doubled_pawns - 15 * isolated_pawns - 10 * blocked_pawns;

        let phase2_eval = phase1_eval
            + match to_play {
                Color::Black => -eval_refinements,
                Color::White => eval_refinements,
            };

        tracing::event!(
            tracing::Level::INFO,
            name = "Phase2 eval",
            "eval" = phase2_eval
        );
        Evaluation(phase2_eval)
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
            let num_pawns = (bits & file).count_ones();
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
                isolated_pawns += (bits & files[file_idx]).count_ones() as u8;
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

        let shifted = match side {
            Color::Black => pawns.0 >> 8,
            Color::White => pawns.0 << 8,
        };

        let blocked_locations = opponent_pieces.0 | pawns.0;
        let blocked_pawns = shifted & blocked_locations;
        blocked_pawns.count_ones() as u8
    }

    pub fn get_least_valuable_piece(
        &self,
        attadef: BitBoard,
        side: Color,
    ) -> Option<(Posn, Piece)> {
        let pieces = [
            Piece::Pawn,
            Piece::Rook,
            Piece::Knight,
            Piece::Bishop,
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
                self.piece(Color::White, Piece::Rook)
                | self.piece(Color::White, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::rook_attacks_pos(
                self.piece(Color::Black, Piece::Rook)
                | self.piece(Color::Black, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::bishop_attacks_pos(
                self.piece(Color::White, Piece::Bishop)
                | self.piece(Color::White, Piece::Queen),
                self.pieces(),
                to_square,
            )
            | Self::bishop_attacks_pos(
                self.piece(Color::Black, Piece::Bishop)
                | self.piece(Color::Black, Piece::Queen),
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
        let b = Board::from_fen("rnb1kbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::White,
        );
        let eval_black = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::Black,
        );
        assert!(eval_white.0 > 0);
        assert!(eval_black.0 < 0);
    }

    #[test]
    fn black_better() {
        let b = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNB1KBNR w - - 0 1")
            .expect("failed to parse fen");
        let eval_white = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::White,
        );
        let eval_black = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::Black,
        );
        assert!(eval_white.0 < 0);
        assert!(eval_black.0 > 0);
    }

    #[test]
    fn eval_starting_board() {
        let b = starting_board();
        let eval_white = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::White,
        );
        let eval_black = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::Black,
        );
        assert!(eval_white.0 < 100 && eval_white.0 > -100);
        assert!(eval_black.0 < 100 && eval_black.0 > -100);
    }

    #[test]
    fn eval_starting_board_e4() {
        let mut b = starting_board();
        let eval_white_before = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::White,
        );
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
        let eval_white_after = b.eval(
            Evaluation::lost(b.half_move),
            Evaluation::won(b.half_move),
            Color::White,
        );
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

        assert!(board.has_three_fold_repetition());
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
}
