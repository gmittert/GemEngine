use tracing::Level;

use crate::board::*;
use std::cmp::max;
use std::fmt;
use std::ops::{Add, AddAssign, Neg, Sub};
use std::time::Duration;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy, Default)]
pub struct Evaluation(pub i16);

pub const PIECE_VALUES: [Evaluation; 6] = [
    Evaluation(100),   // Pawn
    Evaluation(500),   // Rook
    Evaluation(300),   // Knight
    Evaluation(310),   // Bishop
    Evaluation(900),   // Queen
    Evaluation(10000), // King
];

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
    pub fn m1() -> Evaluation {
        Evaluation(std::i16::MAX - 1)
    }
    pub fn m2() -> Evaluation {
        Evaluation(std::i16::MAX - 2)
    }
    pub fn m3() -> Evaluation {
        Evaluation(std::i16::MAX - 3)
    }
    pub fn m4() -> Evaluation {
        Evaluation(std::i16::MAX - 4)
    }
    pub fn m5() -> Evaluation {
        Evaluation(std::i16::MAX - 5)
    }
    pub fn m6() -> Evaluation {
        Evaluation(std::i16::MAX - 6)
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
    pub fn dec_mate(&self) -> Evaluation {
        if self.0 >= Self::won().0 - 100 {
            Evaluation(self.0 - 1)
        } else if self.0 <= Self::lost().0 + 100 {
            Evaluation(self.0 + 1)
        } else {
            *self
        }
    }
    pub fn inc_mate(&self) -> Evaluation {
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
            Level::INFO,
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
            tracing::event!(Level::INFO, name = "Alpha too high");
            return Evaluation(phase1_eval);
        }
        if phase1_eval as i32 - beta.0 as i32 > 200 {
            tracing::event!(Level::INFO, name = "Beta too low");
            return Evaluation(phase1_eval);
        }

        let attacks_white = self.rook_queen_attacks(Color::White)
            | self.bishop_queen_attacks(Color::White)
            | self.king_attacks(Color::White)
            | self.pawn_attacks(Color::White)
            | self.knight_attacks(Color::White);

        let attacks_black = self.rook_queen_attacks(Color::Black)
            | self.bishop_queen_attacks(Color::Black)
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
        let eval_refinements =
            attacks_diff as i16 - 10 * doubled_pawns - 15 * isolated_pawns - 10 * blocked_pawns;

        let phase2_eval = phase1_eval
            + match to_play {
                Color::Black => -eval_refinements,
                Color::White => eval_refinements,
            };

        tracing::event!(Level::INFO, name = "Phase2 eval", "eval" = phase2_eval);
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
        let saved = self.save_state();
        let mut stack = Vec::with_capacity(10);
        let mut side = side;
        while let Some(m) = self.get_smallest_attacker(p, side) {
            stack.push(PIECE_VALUES[m.capture.unwrap() as usize]);
            self.make_move(&m);
            side = !side;
        }

        let mut value = Evaluation::draw();
        while let Some(v) = stack.pop() {
            /* Do not consider captures if they lose material, therefor max zero */
            value = max(Evaluation::draw(), v - value);
        }
        self.restore_state(saved);
        value
    }
}

#[cfg(test)]
mod tests {
    use crate::{board::evaluation::*, transposition_table::*};

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

    #[test]
    fn mated_in_formatting() {
        assert_eq!(Some(1), Evaluation::m1().mate_in());
        assert_eq!(Some(1), (-Evaluation::m1()).mated_in());
        assert_eq!(Some(2), Evaluation::m2().mate_in());
        assert_eq!(Some(2), (-Evaluation::m2()).mated_in());
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
    fn eval_prec() {
        let e = Evaluation::m1();
        let f = e.dec_mate();
        let g = -f;
        let h = -e.dec_mate();
        let i = -(e.dec_mate());
        let j = (-e).dec_mate();
        assert_eq!(-Evaluation::m2(), g);
        assert_eq!(-Evaluation::m2(), h);
        assert_eq!(-Evaluation::m2(), i);
        assert_eq!(-Evaluation::m2(), j);
    }
}
