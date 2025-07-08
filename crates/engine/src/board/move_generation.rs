use std::num::NonZero;

use crate::board::*;
use crate::piece_attack_tables::KING_ATTACKS;
use crate::{board::sliding_attacks, piece_attack_tables::KNIGHT_ATTACKS};
use rand::{distr::Uniform, prelude::*};

#[derive(Debug, Clone, Copy)]
enum PawnMovesState {
    ReadPawn,
    Push1,
    Push2,
    PromoteQueen,
    PromoteKnight,
    PromoteRook,
    PromoteBishop,
}

pub struct PawnNonCaptures {
    pawns: BitBoard,
    pieces: BitBoard,
    from: Option<Posn>,
    color: Color,
    state: PawnMovesState,
    to: Option<Posn>,
}

impl PawnNonCaptures {
    fn new(pawns: BitBoard, pieces: BitBoard, color: Color) -> PawnNonCaptures {
        PawnNonCaptures {
            pawns,
            pieces,
            from: None,
            color,
            state: PawnMovesState::ReadPawn,
            to: None,
        }
    }
}
impl Iterator for PawnNonCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        let promo_rank = match self.color {
            Color::Black => Rank::One,
            Color::White => Rank::Eight,
        };
        loop {
            match self.state {
                PawnMovesState::ReadPawn => {
                    if let Some(pawn) = self.pawns.next() {
                        self.from = Some(pawn);
                        self.state = PawnMovesState::Push1;
                    } else {
                        return None;
                    }
                }
                PawnMovesState::Push1 => {
                    let mpush_pos = match self.color {
                        Color::White => self.from.unwrap().no(),
                        Color::Black => self.from.unwrap().so(),
                    };

                    if let Some(push_pos) = mpush_pos {
                        if !self.pieces.contains(push_pos) {
                            if push_pos.rank() == promo_rank {
                                self.to = Some(push_pos);
                                self.state = PawnMovesState::PromoteQueen;
                                continue;
                            } else {
                                let can_double_push = match self.color {
                                    Color::White => self.from.unwrap().rank() == Rank::Two,
                                    Color::Black => self.from.unwrap().rank() == Rank::Seven,
                                };
                                self.state = if can_double_push {
                                    PawnMovesState::Push2
                                } else {
                                    PawnMovesState::ReadPawn
                                };
                                return Some(AlgebraicMove {
                                    from: self.from.unwrap(),
                                    to: push_pos,
                                    promotion: None,
                                });
                            }
                        } else {
                            self.state = PawnMovesState::ReadPawn;
                        }
                    }
                }
                PawnMovesState::Push2 => {
                    let mdouble_push_pos = match self.color {
                        Color::White => self.from.unwrap().no().and_then(|x| x.no()),
                        Color::Black => self.from.unwrap().so().and_then(|x| x.so()),
                    };

                    if let Some(double_push_pos) = mdouble_push_pos {
                        if !self.pieces.contains(double_push_pos) {
                            self.state = PawnMovesState::ReadPawn;
                            return Some(AlgebraicMove {
                                from: self.from.unwrap(),
                                to: double_push_pos,
                                promotion: None,
                            });
                        } else {
                            self.state = PawnMovesState::ReadPawn;
                        }
                    }
                }
                PawnMovesState::PromoteQueen => {
                    self.state = PawnMovesState::PromoteKnight;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Queen),
                    });
                }
                PawnMovesState::PromoteRook => {
                    self.state = PawnMovesState::PromoteBishop;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Rook),
                    });
                }
                PawnMovesState::PromoteBishop => {
                    self.state = PawnMovesState::ReadPawn;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Bishop),
                    });
                }
                PawnMovesState::PromoteKnight => {
                    self.state = PawnMovesState::PromoteRook;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Knight),
                    });
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum PawnCapturesState {
    ReadPawn,
    TakeEast,
    TakeWest,
    TakeEp,
    PromoteQueen,
    PromoteRook,
    PromoteKnight,
    PromoteBishop,
    Done,
}

pub struct PawnCaptures {
    pawns: BitBoard,
    opponent_pieces: BitBoard,
    from: Option<Posn>,
    color: Color,
    state: PawnCapturesState,
    next_state: PawnCapturesState,
    to: Option<Posn>,
    ep_target: Option<File>,
}

impl PawnCaptures {
    fn new(
        pawns: BitBoard,
        opponent_pieces: BitBoard,
        color: Color,
        ep_target: Option<File>,
    ) -> PawnCaptures {
        PawnCaptures {
            pawns,
            opponent_pieces,
            from: None,
            color,
            state: PawnCapturesState::ReadPawn,
            next_state: PawnCapturesState::Done,
            to: None,
            ep_target,
        }
    }
}
impl Iterator for PawnCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        let promo_rank = match self.color {
            Color::Black => Rank::One,
            Color::White => Rank::Eight,
        };
        loop {
            match self.state {
                PawnCapturesState::ReadPawn => {
                    if let Some(pawn) = self.pawns.next() {
                        self.from = Some(pawn);
                        self.state = PawnCapturesState::TakeEast;
                    } else {
                        self.state = PawnCapturesState::Done;
                    }
                }
                PawnCapturesState::TakeEast => {
                    let mpush_pos = match self.color {
                        Color::White => self.from.unwrap().no(),
                        Color::Black => self.from.unwrap().so(),
                    };
                    let Some(take_pos) = mpush_pos.and_then(|x| x.ea()) else {
                        self.state = PawnCapturesState::TakeWest;
                        continue;
                    };
                    let can_capture = self.opponent_pieces.contains(take_pos);
                    if !can_capture {
                        self.state = PawnCapturesState::TakeWest;
                        continue;
                    };
                    if take_pos.rank() == promo_rank {
                        self.to = Some(take_pos);
                        self.next_state = PawnCapturesState::TakeWest;
                        self.state = PawnCapturesState::PromoteQueen;
                        continue;
                    }
                    self.state = PawnCapturesState::TakeWest;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: take_pos,
                        promotion: None,
                    });
                }
                PawnCapturesState::TakeWest => {
                    let mpush_pos = match self.color {
                        Color::White => self.from.unwrap().no(),
                        Color::Black => self.from.unwrap().so(),
                    };
                    let Some(take_pos) = mpush_pos.and_then(|x| x.we()) else {
                        self.state = PawnCapturesState::TakeEp;
                        continue;
                    };
                    let can_capture = self.opponent_pieces.contains(take_pos);
                    if !can_capture {
                        self.state = PawnCapturesState::TakeEp;
                        continue;
                    };
                    if take_pos.rank() == promo_rank {
                        self.to = Some(take_pos);
                        self.next_state = PawnCapturesState::TakeEp;
                        self.state = PawnCapturesState::PromoteQueen;
                        continue;
                    }
                    self.state = PawnCapturesState::TakeEp;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: take_pos,
                        promotion: None,
                    });
                }
                PawnCapturesState::TakeEp => {
                    self.state = PawnCapturesState::ReadPawn;
                    if let Some(ep_target) = self.ep_target {
                        let to = Posn::from(
                            if self.color == Color::White {
                                Rank::Six
                            } else {
                                Rank::Three
                            },
                            ep_target,
                        );
                        if (self.color == Color::White
                            && (self.from.unwrap().nw() == Some(to)
                                || self.from.unwrap().ne() == Some(to)))
                            || (self.color == Color::Black
                                && (self.from.unwrap().sw() == Some(to)
                                    || self.from.unwrap().se() == Some(to)))
                        {
                            return Some(AlgebraicMove {
                                from: self.from.unwrap(),
                                to,
                                promotion: None,
                            });
                        }
                    }
                }
                PawnCapturesState::Done => return None,
                PawnCapturesState::PromoteQueen => {
                    self.state = PawnCapturesState::PromoteRook;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Queen),
                    });
                }
                PawnCapturesState::PromoteRook => {
                    self.state = PawnCapturesState::PromoteBishop;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Rook),
                    });
                }
                PawnCapturesState::PromoteBishop => {
                    self.state = PawnCapturesState::PromoteKnight;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Bishop),
                    });
                }
                PawnCapturesState::PromoteKnight => {
                    self.state = self.next_state;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Knight),
                    });
                }
            }
        }
    }
}

pub struct KingMoves {
    kings: BitBoard,
    allies: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
    can_castle_queen: bool,
    can_castle_king: bool,
}

impl KingMoves {
    fn new(
        kings: BitBoard,
        allies: BitBoard,
        can_castle_queen: bool,
        can_castle_king: bool,
    ) -> KingMoves {
        KingMoves {
            kings,
            allies,
            attacks: BitBoard::empty(),
            from: None,
            can_castle_queen,
            can_castle_king,
        }
    }
}
impl Iterator for KingMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else if let Some(next_king) = self.kings.next() {
                self.from = Some(next_king);
                self.attacks = !self.allies & KING_ATTACKS[next_king.idx() as usize];
            } else {
                // Finished all the kings
                if self.can_castle_king {
                    self.can_castle_king = false;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.from.unwrap().ea().and_then(|x| x.ea()).unwrap(),
                        promotion: None,
                    });
                } else if self.can_castle_queen {
                    self.can_castle_queen = false;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.from.unwrap().we().and_then(|x| x.we()).unwrap(),
                        promotion: None,
                    });
                } else {
                    return None;
                }
            }
        }
    }
}

pub struct KingCaptures {
    attacks: BitBoard,
    from: Posn,
}

impl KingCaptures {
    fn new(king: Posn, enemies: BitBoard) -> KingCaptures {
        KingCaptures {
            attacks: enemies & KING_ATTACKS[king.idx() as usize],
            from: king,
        }
    }
}
impl Iterator for KingCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(attack) = self.attacks.next() {
            return Some(AlgebraicMove {
                from: self.from,
                to: attack,
                promotion: None,
            });
        }
        // Finished all the attacks
        None
    }
}

pub struct QueenCaptures {
    queens: BitBoard,
    enemies: BitBoard,
    pieces: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl QueenCaptures {
    fn new(queens: BitBoard, enemies: BitBoard, pieces: BitBoard) -> QueenCaptures {
        QueenCaptures {
            queens,
            enemies,
            pieces,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for QueenCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else if let Some(next_queen) = self.queens.next() {
                self.from = Some(next_queen);
                self.attacks = self.enemies
                    & (sliding_attacks::compute_rook_attacks(next_queen, self.pieces)
                        | sliding_attacks::compute_bishop_attacks(next_queen, self.pieces));
            } else {
                // Finished all the queens
                return None;
            }
        }
    }
}

pub struct RookCaptures {
    rooks: BitBoard,
    enemies: BitBoard,
    pieces: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl RookCaptures {
    fn new(rooks: BitBoard, enemies: BitBoard, pieces: BitBoard) -> RookCaptures {
        RookCaptures {
            rooks,
            enemies,
            pieces,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for RookCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else if let Some(next_rook) = self.rooks.next() {
                self.from = Some(next_rook);
                self.attacks =
                    self.enemies & sliding_attacks::compute_rook_attacks(next_rook, self.pieces);
            } else {
                // Finished all the rooks
                return None;
            }
        }
    }
}

pub struct BishopCaptures {
    bishops: BitBoard,
    enemies: BitBoard,
    pieces: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl BishopCaptures {
    fn new(bishops: BitBoard, enemies: BitBoard, pieces: BitBoard) -> BishopCaptures {
        BishopCaptures {
            bishops,
            enemies,
            pieces,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for BishopCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else if let Some(next_bishop) = self.bishops.next() {
                self.from = Some(next_bishop);
                self.attacks = self.enemies
                    & sliding_attacks::compute_bishop_attacks(next_bishop, self.pieces);
            } else {
                // Finished all the bishops
                return None;
            }
        }
    }
}

pub fn knight_moves_it(knights: BitBoard, allies: BitBoard) -> impl Iterator<Item = AlgebraicMove> {
    knights.into_iter().flat_map(move |from| {
        (!allies & KNIGHT_ATTACKS[from.idx() as usize]).map(move |to| AlgebraicMove {
            from,
            to,
            promotion: None,
        })
    })
}

pub fn rook_moves_it(
    rooks: BitBoard,
    allies: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    rooks.flat_map(move |from| {
        (!allies & sliding_attacks::compute_rook_attacks(from, all_pieces)).map(move |to| {
            AlgebraicMove {
                from,
                to,
                promotion: None,
            }
        })
    })
}

pub fn bishop_moves_it(
    bishops: BitBoard,
    allies: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    bishops.flat_map(move |from| {
        (!allies & sliding_attacks::compute_bishop_attacks(from, all_pieces)).map(move |to| {
            AlgebraicMove {
                from,
                to,
                promotion: None,
            }
        })
    })
}

pub fn queen_moves_it(
    queens: BitBoard,
    allies: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    queens.flat_map(move |from| {
        (!allies
            & (sliding_attacks::compute_rook_attacks(from, all_pieces)
                | sliding_attacks::compute_bishop_attacks(from, all_pieces)))
        .map(move |to| AlgebraicMove {
            from,
            to,
            promotion: None,
        })
    })
}

pub struct KnightCaptures {
    knights: BitBoard,
    enemies: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl KnightCaptures {
    fn new(knights: BitBoard, enemies: BitBoard) -> KnightCaptures {
        KnightCaptures {
            knights,
            enemies,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for KnightCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else if let Some(next_knight) = self.knights.next() {
                self.from = Some(next_knight);
                self.attacks = self.enemies & KNIGHT_ATTACKS[next_knight.idx() as usize];
            } else {
                // Finished all the knights
                return None;
            }
        }
    }
}

pub struct PsuedoLegalCaptures {
    #[allow(clippy::type_complexity)]
    iter: std::iter::Chain<
        std::iter::Chain<
            std::iter::Chain<
                std::iter::Chain<std::iter::Chain<PawnCaptures, KnightCaptures>, BishopCaptures>,
                RookCaptures,
            >,
            QueenCaptures,
        >,
        KingCaptures,
    >,
}

impl PsuedoLegalCaptures {
    fn new(board: &Board) -> PsuedoLegalCaptures {
        PsuedoLegalCaptures {
            iter: board
                .pawn_captures_it()
                .chain(board.knight_captures_it())
                .chain(board.bishop_captures_it())
                .chain(board.rook_captures_it())
                .chain(board.queen_captures_it())
                .chain(board.king_captures_it()),
        }
    }
}

impl Iterator for PsuedoLegalCaptures {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

pub struct PsuedoLegalRandomizedMoves {
    iter: (
        Box<dyn Iterator<Item = AlgebraicMove>>,
        Box<dyn Iterator<Item = AlgebraicMove>>,
        Box<dyn Iterator<Item = AlgebraicMove>>,
        Box<dyn Iterator<Item = AlgebraicMove>>,
        std::iter::Chain<PawnCaptures, PawnNonCaptures>,
        KingMoves,
    ),
    rng: ThreadRng,
    dist: Uniform<u8>,
    remaining: Vec<u8>,
}
impl PsuedoLegalRandomizedMoves {
    fn new(board: &Board) -> PsuedoLegalRandomizedMoves {
        let dist = Uniform::try_from(0..=5).unwrap();
        let knights = board.piece(board.to_play, Piece::Knight);
        let rooks = board.piece(board.to_play, Piece::Rook);
        let bishops = board.piece(board.to_play, Piece::Bishop);
        let queens = board.piece(board.to_play, Piece::Queen);
        let allies = match board.to_play {
            Color::White => board.white_pieces(),
            Color::Black => board.black_pieces(),
        };
        let all_pieces = board.pieces();
        PsuedoLegalRandomizedMoves {
            iter: (
                Box::new(knight_moves_it(knights, allies)),
                Box::new(bishop_moves_it(bishops, allies, all_pieces)),
                Box::new(rook_moves_it(rooks, allies, all_pieces)),
                Box::new(queen_moves_it(queens, allies, all_pieces)),
                board.pawn_captures_it().chain(board.pawn_non_captures_it()),
                board.king_moves_it(),
            ),
            rng: rand::rng(),
            dist,
            remaining: vec![0, 1, 2, 3, 4, 5],
        }
    }
}
impl Iterator for PsuedoLegalRandomizedMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        // We've got a somewhat cumbersome approach here. The goal is to randomly choose between
        // the different iterators, removing them from the "remaining" set when they run out.
        if self.remaining.is_empty() {
            return None;
        }
        let random = self.dist.sample(&mut self.rng);
        let idx = self.remaining[random as usize];
        let res = match idx {
            0 => self.iter.0.next(),
            1 => self.iter.1.next(),
            2 => self.iter.2.next(),
            3 => self.iter.3.next(),
            4 => self.iter.4.next(),
            _ => self.iter.5.next(),
        };
        if res.is_some() {
            return res;
        }
        // Here, we take whichever iterator index ran out, switch it to the last spot in the
        // vector, then pop it as a way of removing the n'th index from the vector without shifting
        // all the elements. Since we're using it as a set, we don't care about order.
        self.remaining[random as usize] = *self.remaining.last().unwrap();
        self.remaining.pop().unwrap();
        if !self.remaining.is_empty() {
            self.dist = Uniform::try_from(0..self.remaining.len() as u8).unwrap();
        }
        self.next()
    }
}

impl Board {
    pub fn rook_can_capture(&self, color: Color, target: Posn) -> Option<AlgebraicMove> {
        let pieces = self.piece(color, Piece::Rook);

        let attacked_from = sliding_attacks::compute_rook_attacks(target, self.pieces());
        Some(AlgebraicMove {
            from: (pieces & attacked_from).into_iter().next()?,
            to: target,
            promotion: None,
        })
    }

    // Compute the vertical and horizontal ray attacks of the rooks and queens (used with
    // bishop_queen_attacks to blend the queen attacks across two calls).
    pub fn rook_attacks(rooks: BitBoard, all_pieces: BitBoard) -> BitBoard {
        let mut acc = BitBoard::empty();
        for i in rooks {
            acc |= sliding_attacks::compute_rook_attacks(i, all_pieces);
        }
        acc
    }

    pub fn rook_attacks_pos(rooks: BitBoard, all_pieces: BitBoard, pos: Posn) -> BitBoard {
        let attacked_from = sliding_attacks::compute_rook_attacks(pos, all_pieces);
        rooks & attacked_from
    }

    pub fn bishop_can_capture(&self, color: Color, target: Posn) -> Option<AlgebraicMove> {
        let pieces = self.piece(color, Piece::Bishop);

        let attacked_from = sliding_attacks::compute_bishop_attacks(target, self.pieces());
        Some(AlgebraicMove {
            from: (pieces & attacked_from).into_iter().next()?,
            to: target,
            promotion: None,
        })
    }

    pub fn queen_can_capture(&self, color: Color, target: Posn) -> Option<AlgebraicMove> {
        let pieces = self.piece(color, Piece::Queen);

        let attacked_from = sliding_attacks::compute_bishop_attacks(target, self.pieces())
            | sliding_attacks::compute_rook_attacks(target, self.pieces());
        Some(AlgebraicMove {
            from: (pieces & attacked_from).into_iter().next()?,
            to: target,
            promotion: None,
        })
    }

    // Compute the diagonal ray attacks of the bishops and queens (used with
    // rook_queen_attacks to blend the queen attacks across two calls).
    pub fn bishop_attacks(bishops: BitBoard, all_pieces: BitBoard) -> BitBoard {
        let mut acc = BitBoard::empty();
        for i in bishops {
            acc |= sliding_attacks::compute_bishop_attacks(i, all_pieces);
        }
        acc
    }

    pub fn bishop_attacks_pos(bishops: BitBoard, all_pieces: BitBoard, pos: Posn) -> BitBoard {
        let attacked_from = sliding_attacks::compute_bishop_attacks(pos, all_pieces);
        bishops & attacked_from
    }

    pub fn queen_captures_it(&self) -> QueenCaptures {
        let color = self.to_play;
        let queens = self.piece(color, Piece::Queen);

        let enemy_pieces = match !color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        QueenCaptures::new(queens, enemy_pieces, self.pieces())
    }

    pub fn queen_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let queens = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Queen as usize];

        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in queens {
            let mut attacks = sliding_attacks::compute_bishop_attacks(i, self.pieces());
            attacks |= sliding_attacks::compute_rook_attacks(i, self.pieces());
            for pos in attacks & !allied_pieces {
                out.push(AlgebraicMove {
                    from: i,
                    to: pos,
                    promotion: None,
                });
            }
        }
    }

    pub fn rook_captures_it(&self) -> RookCaptures {
        let rooks = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let enemy_pieces = match !self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        RookCaptures::new(rooks, enemy_pieces, self.pieces())
    }

    pub fn rook_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let rooks = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in rooks {
            let attacks = sliding_attacks::compute_rook_attacks(i, self.pieces());
            for pos in attacks & !allied_pieces {
                out.push(AlgebraicMove {
                    from: i,
                    to: pos,
                    promotion: None,
                });
            }
        }
    }

    pub fn bishop_captures_it(&self) -> BishopCaptures {
        let bishops = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Bishop as usize];

        let enemy_pieces = match !self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        BishopCaptures::new(bishops, enemy_pieces, self.pieces())
    }

    pub fn bishop_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let bishops = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Bishop as usize];

        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in bishops {
            let attacks = sliding_attacks::compute_bishop_attacks(i, self.pieces());
            for pos in attacks & !allied_pieces {
                out.push(AlgebraicMove {
                    from: i,
                    to: pos,
                    promotion: None,
                });
            }
        }
    }

    pub fn king_can_capture(&self, color: Color, target: Posn) -> Option<AlgebraicMove> {
        let kings = self.piece(color, Piece::King);

        let attacked_from = KING_ATTACKS[target.idx() as usize];
        let attackers = kings & attacked_from;
        Some(AlgebraicMove {
            from: attackers.into_iter().next()?,
            to: target,
            promotion: None,
        })
    }

    pub fn king_attacks_pos(kings: BitBoard, pos: Posn) -> BitBoard {
        let attacked_from = KING_ATTACKS[pos.idx() as usize];
        kings & attacked_from
    }

    pub fn king_attacks(kings: BitBoard) -> BitBoard {
        if kings.0 == 0 {
            BitBoard::empty()
        } else {
            // There should always be one king.
            let from = Posn {
                pos: unsafe { NonZero::new_unchecked(kings.0) },
            };
            KING_ATTACKS[from.idx() as usize]
        }
    }

    pub fn king_captures_it(&self) -> KingCaptures {
        let color = self.to_play;
        let kings = self.piece(color, Piece::King);

        let enemy_pieces = match !color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };
        KingCaptures::new(
            Posn {
                pos: unsafe { NonZero::new_unchecked(kings.0) },
            },
            enemy_pieces,
        )
    }

    pub fn king_moves_it(&self) -> KingMoves {
        let color = self.to_play;
        let kings = self.piece(color, Piece::King);
        let rooks = self.piece(color, Piece::Rook);

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };
        let mut kings_it = kings;
        let from = kings_it.next().unwrap();

        // Computing the ability to castle needs the board to compute castling through check
        let can_castle_king = if self
            .move_rights
            .last()
            .map(|x| x.castling_ability.can_castle_king(self.to_play))
            .unwrap_or(false)
        {
            !self.attacked_by_side(from.ea().unwrap(), !self.to_play)
                && !self.attacked_by_side(from.ea().and_then(|x| x.ea()).unwrap(), !self.to_play)
                && !self.in_check(self.to_play)
                && !self.pieces().contains(from.ea().unwrap())
                && !self
                    .pieces()
                    .contains(from.ea().and_then(|x| x.ea()).unwrap())
                && rooks.contains(from.ea().and_then(|x| x.ea()).and_then(|x| x.ea()).unwrap())
        } else {
            false
        };
        let can_castle_queen = if self
            .move_rights
            .last()
            .map(|x| x.castling_ability.can_castle_queen(self.to_play))
            .unwrap_or(false)
        {
            !self.attacked_by_side(from.we().unwrap(), !self.to_play)
                && !self.attacked_by_side(from.we().and_then(|x| x.we()).unwrap(), !self.to_play)
                && !self.in_check(self.to_play)
                && !self.pieces().contains(from.we().unwrap())
                && !self
                    .pieces()
                    .contains(from.we().and_then(|x| x.we()).unwrap())
                && !self
                    .pieces()
                    .contains(from.we().and_then(|x| x.we()).and_then(|x| x.we()).unwrap())
                && rooks.contains(
                    from.we()
                        .and_then(|x| x.we())
                        .and_then(|x| x.we())
                        .and_then(|x| x.we())
                        .unwrap(),
                )
        } else {
            false
        };

        KingMoves::new(kings, allied_pieces, can_castle_queen, can_castle_king)
    }

    pub fn king_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let color = self.to_play;
        let kings = self.piece(color, Piece::King);
        let rooks = self.piece(color, Piece::Rook);

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };
        let from = Posn {
            pos: unsafe { NonZero::new_unchecked(kings.0) },
        };

        for m in KING_ATTACKS[from.idx() as usize] & !allied_pieces {
            out.push(AlgebraicMove {
                from,
                to: m,
                promotion: None,
            });
        }

        // Computing the ability to castle needs the board to compute castling through check
        let can_castle_king = if self
            .move_rights
            .last()
            .map(|x| x.castling_ability.can_castle_king(self.to_play))
            .unwrap_or(false)
        {
            !self.attacked_by_side(from.ea().unwrap(), !self.to_play)
                && !self.attacked_by_side(from.ea().and_then(|x| x.ea()).unwrap(), !self.to_play)
                && !self.in_check(self.to_play)
                && !self.pieces().contains(from.ea().unwrap())
                && !self
                    .pieces()
                    .contains(from.ea().and_then(|x| x.ea()).unwrap())
                && rooks.contains(from.ea().and_then(|x| x.ea()).and_then(|x| x.ea()).unwrap())
        } else {
            false
        };
        if can_castle_king {
            out.push(AlgebraicMove {
                from,
                to: from.ea().and_then(|x| x.ea()).unwrap(),
                promotion: None,
            });
        }
        let can_castle_queen = if self
            .move_rights
            .last()
            .map(|x| x.castling_ability.can_castle_queen(self.to_play))
            .unwrap_or(false)
        {
            !self.attacked_by_side(from.we().unwrap(), !self.to_play)
                && !self.attacked_by_side(from.we().and_then(|x| x.we()).unwrap(), !self.to_play)
                && !self.in_check(self.to_play)
                && !self.pieces().contains(from.we().unwrap())
                && !self
                    .pieces()
                    .contains(from.we().and_then(|x| x.we()).unwrap())
                && !self
                    .pieces()
                    .contains(from.we().and_then(|x| x.we()).and_then(|x| x.we()).unwrap())
                && rooks.contains(
                    from.we()
                        .and_then(|x| x.we())
                        .and_then(|x| x.we())
                        .and_then(|x| x.we())
                        .unwrap(),
                )
        } else {
            false
        };
        if can_castle_queen {
            out.push(AlgebraicMove {
                from,
                to: from.we().and_then(|x| x.we()).unwrap(),
                promotion: None,
            });
        }
    }

    pub fn knight_can_capture(&self, color: Color, target_pos: Posn) -> Option<AlgebraicMove> {
        let knights = self.piece(color, Piece::Knight);
        let attacked_from = KNIGHT_ATTACKS[target_pos.idx() as usize];
        let attackers = knights & attacked_from;
        Some(AlgebraicMove {
            from: attackers.into_iter().next()?,
            to: target_pos,
            promotion: None,
        })
    }

    pub fn knight_attacks_pos(knights: BitBoard, pos: Posn) -> BitBoard {
        let attacked_from = KNIGHT_ATTACKS[pos.idx() as usize];
        knights & attacked_from
    }

    pub fn knight_attacks(knights: BitBoard) -> BitBoard {
        knights.into_iter().fold(BitBoard::empty(), |acc, knight| {
            acc | KNIGHT_ATTACKS[knight.idx() as usize]
        })
    }

    pub fn knight_captures_it(&self) -> KnightCaptures {
        let knights = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Knight as usize];

        let enemy_pieces = match !self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        KnightCaptures::new(knights, enemy_pieces)
    }

    pub fn knight_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let knights = match self.to_play {
            Color::White => self.white_piece(Piece::Knight),
            Color::Black => self.black_piece(Piece::Knight),
        };
        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for knight in knights {
            out.extend(
                (KNIGHT_ATTACKS[knight.idx() as usize] & !allied_pieces)
                    .into_iter()
                    .map(|p| AlgebraicMove {
                        from: knight,
                        to: p,
                        promotion: None,
                    }),
            );
        }
    }

    pub fn pawn_attacks_pos(pawns: BitBoard, pos: Posn, color: Color) -> BitBoard {
        let attacked_from = match color {
            Color::White => [pos.se(), pos.sw()],
            Color::Black => [pos.ne(), pos.nw()],
        }
        .into_iter()
        .flatten()
        .fold(BitBoard::empty(), |acc, p| acc | p);
        pawns & attacked_from
    }
    pub fn pawn_attacks(pawns: BitBoard, color: Color) -> BitBoard {
        const A_FILE: u64 = 0x8080_8080_8080_8080;
        const H_FILE: u64 = 0x0101_0101_0101_0101;
        match color {
            Color::Black => {
                let sw_attacks = (pawns.0 & !A_FILE) >> 7;
                let se_attacks = (pawns.0 & !H_FILE) << 9;
                BitBoard(sw_attacks | se_attacks)
            }
            Color::White => {
                let nw_attacks = (pawns.0 & !A_FILE) << 9;
                let ne_attacks = (pawns.0 & !H_FILE) << 7;
                BitBoard(nw_attacks | ne_attacks)
            }
        }
    }

    pub fn pawn_captures_it(&self) -> PawnCaptures {
        let color = self.to_play;
        let pawns = self.piece(color, Piece::Pawn);
        let opponent_pieces = match color {
            Color::White => self.black_pieces(),
            Color::Black => self.white_pieces(),
        };
        let attacked_pawns = Self::pawn_attacks(opponent_pieces, !color);
        let pawns = attacked_pawns & pawns;

        let ep_target = self.move_rights.last().and_then(|r| r.ep_target);
        let ep_pos = ep_target
            .map(|f| {
                BitBoard::from(Posn::from(
                    match color {
                        Color::Black => Rank::Three,
                        Color::White => Rank::Five,
                    },
                    f,
                ))
            })
            .unwrap_or(BitBoard::empty());

        let attacked_pawns = Self::pawn_attacks(opponent_pieces, !color) | ep_pos;
        let pawns = attacked_pawns & pawns;

        PawnCaptures::new(pawns, opponent_pieces, color, ep_target)
    }

    pub fn pawn_non_captures_it(&self) -> PawnNonCaptures {
        let color = self.to_play;
        let pawns = self.piece(color, Piece::Pawn);
        let pieces = self.pieces();

        // Skip blocked pawns
        let non_blocked = match self.to_play {
            Color::Black => pawns.0 & !(pieces.0 << 8),
            Color::White => pawns.0 & !(pieces.0 >> 8),
        };

        PawnNonCaptures::new(BitBoard(non_blocked), self.pieces(), color)
    }

    pub fn pawn_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let promo_rank = match self.to_play {
            Color::Black => Rank::One,
            Color::White => Rank::Eight,
        };
        let pawns = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Pawn as usize];
        let opponent_pieces = match self.to_play {
            Color::White => self.black_pieces(),
            Color::Black => self.white_pieces(),
        };

        for pawn in pawns {
            let mpush_pos = match self.to_play {
                Color::White => pawn.no(),
                Color::Black => pawn.so(),
            };
            // Push 1
            if let Some(push_pos) = mpush_pos
                && !self.pieces().contains(push_pos)
            {
                if push_pos.rank() == promo_rank {
                    for piece in [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop] {
                        out.push(AlgebraicMove {
                            from: pawn,
                            to: push_pos,
                            promotion: Some(piece),
                        });
                    }
                } else {
                    out.push(AlgebraicMove {
                        from: pawn,
                        to: push_pos,
                        promotion: None,
                    });
                }

                // Double Push (only if we could push 1)
                let can_double_push = match self.to_play {
                    Color::White => pawn.rank() == Rank::Two,
                    Color::Black => pawn.rank() == Rank::Seven,
                };
                if can_double_push {
                    let mdouble_push_pos = match self.to_play {
                        Color::White => pawn.no().and_then(|x| x.no()),
                        Color::Black => pawn.so().and_then(|x| x.so()),
                    };

                    if let Some(double_push_pos) = mdouble_push_pos
                        && !self.pieces().contains(double_push_pos)
                    {
                        out.push(AlgebraicMove {
                            from: pawn,
                            to: double_push_pos,
                            promotion: None,
                        });
                    }
                }
            }

            for take in [
                mpush_pos.and_then(|x| x.we()),
                mpush_pos.and_then(|x| x.ea()),
            ] {
                if let Some(take_pos) = take
                    && opponent_pieces.contains(take_pos)
                {
                    if take_pos.rank() == promo_rank {
                        for piece in [Piece::Queen, Piece::Knight, Piece::Rook, Piece::Bishop] {
                            out.push(AlgebraicMove {
                                from: pawn,
                                to: take_pos,
                                promotion: Some(piece),
                            });
                        }
                    } else {
                        out.push(AlgebraicMove {
                            from: pawn,
                            to: take_pos,
                            promotion: None,
                        });
                    }
                }
            }
            // Take En Passant
            if let Some(ep_target) = self.move_rights.last().and_then(|x| x.ep_target) {
                let to = Posn::from(
                    if self.to_play == Color::White {
                        Rank::Six
                    } else {
                        Rank::Three
                    },
                    ep_target,
                );
                if (self.to_play == Color::White
                    && (pawn.nw() == Some(to) || pawn.ne() == Some(to)))
                    || (self.to_play == Color::Black
                        && (pawn.sw() == Some(to) || pawn.se() == Some(to)))
                {
                    out.push(AlgebraicMove {
                        from: pawn,
                        to,
                        promotion: None,
                    });
                }
            }
        }
    }

    pub fn pawn_can_capture(&self, color: Color, target_pos: Posn) -> Option<AlgebraicMove> {
        let pawns = self.piece(color, Piece::Pawn);

        let promo_rank = match color {
            Color::Black => Rank::One,
            Color::White => Rank::Eight,
        };
        let promotion = if target_pos.rank() == promo_rank {
            Some(Piece::Queen)
        } else {
            None
        };

        let attacked_from = match color {
            Color::White => [target_pos.se(), target_pos.sw()],
            Color::Black => [target_pos.ne(), target_pos.nw()],
        }
        .into_iter()
        .flatten()
        .fold(BitBoard::empty(), |acc, p| acc | p);
        if let Some(from) = (pawns & attacked_from).into_iter().next() {
            return Some(AlgebraicMove {
                from,
                to: target_pos,
                promotion,
            });
        }

        // En Passant
        if let Some(ep_target) = self.move_rights.last().and_then(|x| x.ep_target) {
            let double_push = Posn::from(
                if color == Color::White {
                    Rank::Five
                } else {
                    Rank::Four
                },
                ep_target,
            );

            let to = Posn::from(
                if color == Color::White {
                    Rank::Six
                } else {
                    Rank::Three
                },
                ep_target,
            );

            if let Some(e) = double_push.ea()
                && pawns.contains(e)
            {
                return Some(AlgebraicMove {
                    from: e,
                    to,
                    promotion: None,
                });
            }
            if let Some(w) = double_push.we()
                && pawns.contains(w)
            {
                return Some(AlgebraicMove {
                    from: w,
                    to,
                    promotion: None,
                });
            }
        }
        None
    }
    pub fn pseudo_legal_captures_it(&self) -> PsuedoLegalCaptures {
        PsuedoLegalCaptures::new(self)
    }
    pub fn pseudo_legal_randomized_moves_it(&self) -> PsuedoLegalRandomizedMoves {
        PsuedoLegalRandomizedMoves::new(self)
    }

    pub fn fill_pseudo_legal_moves(&mut self, moves: &mut Vec<AlgebraicMove>) {
        self.rook_moves(moves);
        self.bishop_moves(moves);
        self.queen_moves(moves);
        self.knight_moves(moves);
        self.king_moves(moves);
        self.pawn_moves(moves);
    }

    pub fn generate_pseudo_legal_moves(&mut self) -> Vec<AlgebraicMove> {
        let mut moves = Vec::with_capacity(32);
        self.fill_pseudo_legal_moves(&mut moves);
        moves
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;

    #[test]
    fn rook_moves_empty() {
        for i in 0..64 {
            let mut board = empty_board(Color::White);
            board.add_piece(Color::White, Piece::Rook, Posn::from_idx(i).unwrap());
            let mut moves = vec![];
            board.rook_moves(&mut moves);
            assert_eq!(moves.len(), 14);
            let mut before = board.clone();
            for am in &moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
            board.remove_piece(Color::White, Piece::Rook, Posn::from_idx(i).unwrap());
        }
    }

    #[test]
    fn rook_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Rook, d5());
        board.add_piece(Color::White, Piece::Pawn, d4());
        board.add_piece(Color::White, Piece::Pawn, d6());
        board.add_piece(Color::White, Piece::Pawn, c5());
        board.add_piece(Color::White, Piece::Pawn, e5());
        let mut moves = vec![];
        board.rook_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn rook_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Rook, d5());
        board.add_piece(Color::Black, Piece::Pawn, d4());
        board.add_piece(Color::Black, Piece::Pawn, d6());
        board.add_piece(Color::Black, Piece::Pawn, c5());
        board.add_piece(Color::Black, Piece::Pawn, e5());
        let mut moves = vec![];
        board.rook_moves(&mut moves);
        assert_eq!(moves.len(), 4);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn bishop_moves_empty() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Bishop, a1());
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 7);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn bishop_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Bishop, d5());
        board.add_piece(Color::White, Piece::Pawn, e6());
        board.add_piece(Color::White, Piece::Pawn, e4());
        board.add_piece(Color::White, Piece::Pawn, c6());
        board.add_piece(Color::White, Piece::Pawn, c4());
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn bishop_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Bishop, d5());
        board.add_piece(Color::Black, Piece::Pawn, e6());
        board.add_piece(Color::Black, Piece::Pawn, e4());
        board.add_piece(Color::Black, Piece::Pawn, c6());
        board.add_piece(Color::Black, Piece::Pawn, c4());
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 4);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn queen_moves_empty() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Queen, a1());
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 21);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
        board.remove_piece(Color::White, Piece::Queen, a1());

        board.add_piece(Color::White, Piece::Queen, d5());
        moves.clear();
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 27);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
        board.remove_piece(Color::White, Piece::Queen, d5());
    }

    #[test]
    fn queen_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        board.add_piece(Color::White, Piece::Queen, d5());
        board.add_piece(Color::White, Piece::Pawn, e6());
        board.add_piece(Color::White, Piece::Pawn, e4());
        board.add_piece(Color::White, Piece::Pawn, c6());
        board.add_piece(Color::White, Piece::Pawn, c4());
        board.add_piece(Color::White, Piece::Pawn, d4());
        board.add_piece(Color::White, Piece::Pawn, d6());
        board.add_piece(Color::White, Piece::Pawn, c5());
        board.add_piece(Color::White, Piece::Pawn, e5());
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn queen_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Queen, d5());
        board.add_piece(Color::Black, Piece::Pawn, e6());
        board.add_piece(Color::Black, Piece::Pawn, e4());
        board.add_piece(Color::Black, Piece::Pawn, c6());
        board.add_piece(Color::Black, Piece::Pawn, c4());
        board.add_piece(Color::Black, Piece::Pawn, d4());
        board.add_piece(Color::Black, Piece::Pawn, d6());
        board.add_piece(Color::Black, Piece::Pawn, c5());
        board.add_piece(Color::Black, Piece::Pawn, e5());
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn king_moves_empty() {
        let mut board = empty_board(Color::White);
        board.move_rights.push(MoveRights {
            castling_ability: CastlingAbility(0),
            ep_target: None,
        });
        board.white_pieces[Piece::King as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 3);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        moves.clear();
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn king_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::King, d5());
        board.add_piece(Color::White, Piece::Knight, e6());
        board.add_piece(Color::White, Piece::Knight, e4());
        board.add_piece(Color::White, Piece::Knight, c6());
        board.add_piece(Color::White, Piece::Knight, c4());
        board.add_piece(Color::White, Piece::Knight, d6());
        board.add_piece(Color::White, Piece::Knight, d4());
        board.add_piece(Color::White, Piece::Knight, c5());
        board.add_piece(Color::White, Piece::Knight, e5());
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn king_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::King, d5());
        board.add_piece(Color::Black, Piece::Knight, e6());
        board.add_piece(Color::Black, Piece::Knight, e4());
        board.add_piece(Color::Black, Piece::Knight, c6());
        board.add_piece(Color::Black, Piece::Knight, c4());
        board.add_piece(Color::Black, Piece::Knight, d6());
        board.add_piece(Color::Black, Piece::Knight, d4());
        board.add_piece(Color::Black, Piece::Knight, c5());
        board.add_piece(Color::Black, Piece::Knight, e5());
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn knight_moves_empty() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Knight, a1());
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 2);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
        board.remove_piece(Color::White, Piece::Knight, a1());

        board.add_piece(Color::White, Piece::Knight, d5());
        moves.clear();
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
        board.remove_piece(Color::White, Piece::Knight, d5());

        board.add_piece(Color::White, Piece::Knight, a5());
        moves.clear();
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 4);
        let mut before = board.clone();
        for am in &moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
        board.remove_piece(Color::White, Piece::Knight, a5());
    }

    #[test]
    fn knight_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Knight, d5());
        board.add_piece(Color::White, Piece::Pawn, e7());
        board.add_piece(Color::White, Piece::Pawn, e3());
        board.add_piece(Color::White, Piece::Pawn, c7());
        board.add_piece(Color::White, Piece::Pawn, c3());
        board.add_piece(Color::White, Piece::Pawn, f4());
        board.add_piece(Color::White, Piece::Pawn, f6());
        board.add_piece(Color::White, Piece::Pawn, b4());
        board.add_piece(Color::White, Piece::Pawn, b6());
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn knight_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.add_piece(Color::White, Piece::Knight, d5());
        board.add_piece(Color::Black, Piece::Pawn, e7());
        board.add_piece(Color::Black, Piece::Pawn, e3());
        board.add_piece(Color::Black, Piece::Pawn, c7());
        board.add_piece(Color::Black, Piece::Pawn, c3());
        board.add_piece(Color::Black, Piece::Pawn, f4());
        board.add_piece(Color::Black, Piece::Pawn, f6());
        board.add_piece(Color::Black, Piece::Pawn, b4());
        board.add_piece(Color::Black, Piece::Pawn, b6());
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for am in moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn pawn_moves_empty() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a3());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a5());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn pawn_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a3());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a3());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a5());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a6());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn pawn_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a3());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a3());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);
        let mut before = board.clone();
        for am in moves.clone() {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a5());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a6());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);
        let mut before = board.clone();
        for am in moves.clone() {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn pawn_captures() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(d3());
        board.black_pieces[Piece::Pawn as usize] = c4() | e4() | d4();
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Pawn as usize] = c4() | e4() | d4();
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);
        let mut before = board.clone();
        for am in moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn pawn_en_passants_white() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(e5());
        board.move_rights.push(MoveRights {
            castling_ability: CastlingAbility(0xff),
            ep_target: Some(File::E),
        });
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);
        let last_move = board.from_algeabraic_unchecked(moves.last().unwrap());
        assert_eq!(last_move.is_en_passant, true);
        assert_eq!(last_move.from, d5());
        assert_eq!(last_move.to, e6());
        let mut before = board.clone();
        for am in moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn pawn_en_passants_black() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];
        board.to_play = Color::Black;

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(e4());
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(d4());
        board.move_rights.push(MoveRights {
            castling_ability: CastlingAbility(0xff),
            ep_target: Some(File::E),
        });
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);
        let last_move = board.from_algeabraic_unchecked(moves.last().unwrap());
        assert_eq!(last_move.is_en_passant, true);
        assert_eq!(last_move.from, d4());
        assert_eq!(last_move.to, e3());
        let mut before = board.clone();
        for am in moves {
            let m = board.from_algeabraic_unchecked(&am);
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn in_check_rook() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(e1());
        board.black_pieces[Piece::Rook as usize] = BitBoard::from(e2());
        assert_eq!(board.in_check(Color::White), true);

        board.white_pieces[Piece::King as usize] = BitBoard::from(e1());
        board.black_pieces[Piece::Rook as usize] = BitBoard::from(g1());
        assert_eq!(board.in_check(Color::White), true);
    }

    #[test]
    fn in_check_bishop() {
        let mut board = empty_board(Color::White);
        board.black_pieces[Piece::King as usize] = BitBoard::from(e1());
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(f2());
        assert_eq!(board.in_check(Color::Black), true);
    }

    #[test]
    fn in_check_pawn() {
        let mut board = empty_board(Color::White);
        board.black_pieces[Piece::King as usize] = BitBoard::from(e4());
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(f3());
        assert_eq!(board.in_check(Color::Black), true);

        board.black_pieces[Piece::King as usize] = BitBoard::from(e4());
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(e3());
        assert_eq!(board.in_check(Color::Black), false);
    }
    #[test]
    fn castle_king_white_queen() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 w KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 w Q - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
    #[test]
    fn castle_king_white_king() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w K - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
    #[test]
    fn castle_king_black_queen() {
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 b KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for m in only_castles {
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 b q - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
    #[test]
    fn castle_king_black_king() {
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b k - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
    #[test]
    fn no_castle_king_white_queen() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 w - - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 w Kkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
    }
    #[test]
    fn no_castle_king_white_king() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w - - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w Qkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
    }
    #[test]
    fn no_castle_king_black_queen() {
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 b - - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 b KQk - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
    }
    #[test]
    fn no_castle_king_black_king() {
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b - - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b KQq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
    }
    #[test]
    fn no_castle_through_check() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/4b3/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("8/8/b7/8/8/8/8/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("8/8/7b/8/8/8/8/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let board =
                Board::from_fen("8/8/8/7b/8/8/8/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
            let mut before = board.clone();
            for am in moves {
                let m = board.from_algeabraic_unchecked(&am);
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
    #[test]
    fn no_castle_while_check() {
        {
            let mut board =
                Board::from_fen("8/8/8/8/8/8/4r3/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .clone()
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
            for am in moves {
                let before = board.clone();
                let m = board.from_algeabraic_unchecked(&am);
                board.make_move(&m);
                board.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
    #[test]
    fn no_castle_through_pieces() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R2NKB1R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3KB1R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K1BR w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R2NK2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R1N1K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/RN2K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves
                .into_iter()
                .map(|x| board.from_algeabraic_unchecked(&x))
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
    }
    #[test]
    fn pawn_promotion() {
        {
            let mut board =
                Board::from_fen("8/1P6/8/8/8/8/8/8 w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.pawn_moves(&mut moves);
            assert_eq!(moves.len(), 4);

            for am in moves {
                let before = board.clone();
                let m = board.from_algeabraic_unchecked(&am);
                board.make_move(&m);
                board.undo_move(&m);
                assert_eq!(before, board);
            }
        }
        {
            let mut board =
                Board::from_fen("8/8/8/8/8/8/4p3/8 b KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.pawn_moves(&mut moves);
            assert_eq!(moves.len(), 4);
            for am in moves {
                let before = board.clone();
                let m = board.from_algeabraic_unchecked(&am);
                board.make_move(&m);
                board.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
}
