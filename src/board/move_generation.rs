pub use crate::board::moves::*;
pub use crate::board::posn::*;
use crate::board::sliding_attacks;
pub use crate::board::*;

#[derive(Debug, Clone, Copy)]
enum PawnMovesState {
    ReadPawn,
    Push1,
    Push2,
    TakeEast,
    TakeWest,
    TakeEp,
    PromoteQueen,
    PromoteRook,
    PromoteKnight,
    PromoteBishop,
    Done,
}

pub struct PawnMoves {
    pawns: BitBoard,
    opponent_pieces: BitBoard,
    pieces: BitBoard,
    from: Option<Posn>,
    color: Color,
    state: PawnMovesState,
    next_state: PawnMovesState,
    to: Option<Posn>,
    capture: Option<Piece>,
    ep_target: Option<File>,
}

impl PawnMoves {
    fn new(
        pawns: BitBoard,
        opponent_pieces: BitBoard,
        pieces: BitBoard,
        color: Color,
        ep_target: Option<File>,
    ) -> PawnMoves {
        PawnMoves {
            pawns,
            opponent_pieces,
            pieces,
            from: None,
            color,
            state: PawnMovesState::ReadPawn,
            next_state: PawnMovesState::Done,
            to: None,
            capture: None,
            ep_target,
        }
    }
}
impl Iterator for PawnMoves {
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
                        self.state = PawnMovesState::Done;
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
                                self.capture = None;
                                self.next_state = PawnMovesState::TakeEast;
                                self.state = PawnMovesState::PromoteQueen;
                                continue;
                            } else {
                                self.state = PawnMovesState::Push2;
                                return Some(AlgebraicMove {
                                    from: self.from.unwrap(),
                                    to: push_pos,
                                    promotion: None,
                                });
                            }
                        } else {
                            self.state = PawnMovesState::TakeEast;
                        }
                    }
                }
                PawnMovesState::Push2 => {
                    let can_double_push = match self.color {
                        Color::White => self.from.unwrap().rank() == Rank::Two,
                        Color::Black => self.from.unwrap().rank() == Rank::Seven,
                    };
                    if !can_double_push {
                        self.state = PawnMovesState::TakeEast;
                        continue;
                    }

                    let mdouble_push_pos = match self.color {
                        Color::White => self.from.unwrap().no().and_then(|x| x.no()),
                        Color::Black => self.from.unwrap().so().and_then(|x| x.so()),
                    };

                    if let Some(double_push_pos) = mdouble_push_pos {
                        if !self.pieces.contains(double_push_pos) {
                            if double_push_pos.rank() == promo_rank {
                                self.to = Some(double_push_pos);
                                self.capture = None;
                                self.next_state = PawnMovesState::TakeEast;
                                self.state = PawnMovesState::PromoteQueen;
                                continue;
                            } else {
                                self.state = PawnMovesState::TakeEast;
                                return Some(AlgebraicMove {
                                    from: self.from.unwrap(),
                                    to: double_push_pos,
                                    promotion: None,
                                });
                            }
                        } else {
                            self.state = PawnMovesState::TakeEast;
                        }
                    }
                }
                PawnMovesState::TakeEast => {
                    let mpush_pos = match self.color {
                        Color::White => self.from.unwrap().no(),
                        Color::Black => self.from.unwrap().so(),
                    };
                    let Some(take_pos) = mpush_pos.and_then(|x| x.ea()) else {
                        self.state = PawnMovesState::TakeWest;
                        continue;
                    };
                    let can_capture = self.opponent_pieces.contains(take_pos);
                    if !can_capture {
                        self.state = PawnMovesState::TakeWest;
                        continue;
                    };
                    if take_pos.rank() == promo_rank {
                        self.to = Some(take_pos);
                        self.next_state = PawnMovesState::TakeWest;
                        self.state = PawnMovesState::PromoteQueen;
                        continue;
                    }
                    self.state = PawnMovesState::TakeWest;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: take_pos,
                        promotion: None,
                    });
                }
                PawnMovesState::TakeWest => {
                    let mpush_pos = match self.color {
                        Color::White => self.from.unwrap().no(),
                        Color::Black => self.from.unwrap().so(),
                    };
                    let Some(take_pos) = mpush_pos.and_then(|x| x.we()) else {
                        self.state = PawnMovesState::TakeEp;
                        continue;
                    };
                    let can_capture = self.opponent_pieces.contains(take_pos);
                    if !can_capture {
                        self.state = PawnMovesState::TakeEp;
                        continue;
                    };
                    if take_pos.rank() == promo_rank {
                        self.to = Some(take_pos);
                        self.next_state = PawnMovesState::TakeEp;
                        self.state = PawnMovesState::PromoteQueen;
                        continue;
                    }
                    self.state = PawnMovesState::TakeEp;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: take_pos,
                        promotion: None,
                    });
                }
                PawnMovesState::TakeEp => {
                    self.state = PawnMovesState::ReadPawn;
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
                PawnMovesState::Done => return None,
                PawnMovesState::PromoteQueen => {
                    self.state = PawnMovesState::PromoteRook;
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
                    self.state = PawnMovesState::PromoteKnight;
                    return Some(AlgebraicMove {
                        from: self.from.unwrap(),
                        to: self.to.unwrap(),
                        promotion: Some(Piece::Bishop),
                    });
                }
                PawnMovesState::PromoteKnight => {
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
                if self.allies.contains(attack) {
                    continue;
                }
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else {
                if let Some(next_king) = self.kings.next() {
                    self.from = Some(next_king);
                    self.attacks = [
                        next_king.no(),
                        next_king.ne(),
                        next_king.nw(),
                        next_king.ea(),
                        next_king.we(),
                        next_king.so(),
                        next_king.se(),
                        next_king.sw(),
                    ]
                    .into_iter()
                    .filter_map(|p| p)
                    .fold(BitBoard::empty(), |x, y| x | y);
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
}

pub struct QueenMoves {
    queens: BitBoard,
    allies: BitBoard,
    pieces: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl QueenMoves {
    fn new(queens: BitBoard, allies: BitBoard, pieces: BitBoard) -> QueenMoves {
        QueenMoves {
            queens,
            allies,
            pieces,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for QueenMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                if self.allies.contains(attack) {
                    continue;
                }
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else {
                if let Some(next_queen) = self.queens.next() {
                    self.from = Some(next_queen);
                    self.attacks = sliding_attacks::compute_rook_attacks(next_queen, self.pieces)
                        | sliding_attacks::compute_bishop_attacks(next_queen, self.pieces);
                } else {
                    // Finished all the queens
                    return None;
                }
            }
        }
    }
}

pub struct RookMoves {
    rooks: BitBoard,
    allies: BitBoard,
    pieces: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl RookMoves {
    fn new(rooks: BitBoard, allies: BitBoard, pieces: BitBoard) -> RookMoves {
        RookMoves {
            rooks,
            allies,
            pieces,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for RookMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                if self.allies.contains(attack) {
                    continue;
                }
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else {
                if let Some(next_rook) = self.rooks.next() {
                    self.from = Some(next_rook);
                    self.attacks = sliding_attacks::compute_rook_attacks(next_rook, self.pieces);
                } else {
                    // Finished all the rooks
                    return None;
                }
            }
        }
    }
}

pub struct BishopMoves {
    bishops: BitBoard,
    allies: BitBoard,
    pieces: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl BishopMoves {
    fn new(bishops: BitBoard, allies: BitBoard, pieces: BitBoard) -> BishopMoves {
        BishopMoves {
            bishops,
            allies,
            pieces,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for BishopMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                if self.allies.contains(attack) {
                    continue;
                }
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else {
                if let Some(next_bishop) = self.bishops.next() {
                    self.from = Some(next_bishop);
                    self.attacks =
                        sliding_attacks::compute_bishop_attacks(next_bishop, self.pieces);
                } else {
                    // Finished all the bishops
                    return None;
                }
            }
        }
    }
}

pub struct KnightMoves {
    knights: BitBoard,
    allies: BitBoard,
    attacks: BitBoard,
    from: Option<Posn>,
}

impl KnightMoves {
    fn new(knights: BitBoard, allies: BitBoard) -> KnightMoves {
        KnightMoves {
            knights,
            allies,
            from: None,
            attacks: BitBoard::empty(),
        }
    }
}
impl Iterator for KnightMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(attack) = self.attacks.next() {
                if self.allies.contains(attack) {
                    continue;
                }
                return Some(AlgebraicMove {
                    from: self.from.unwrap(),
                    to: attack,
                    promotion: None,
                });
            } else {
                if let Some(next_knight) = self.knights.next() {
                    self.from = Some(next_knight);
                    self.attacks = [
                        next_knight.see(),
                        next_knight.sse(),
                        next_knight.ssw(),
                        next_knight.sww(),
                        next_knight.nww(),
                        next_knight.nnw(),
                        next_knight.nne(),
                        next_knight.nee(),
                    ]
                    .into_iter()
                    .filter_map(|p| p)
                    .fold(BitBoard::empty(), |x, y| x | y);
                } else {
                    // Finished all the knights
                    return None;
                }
            }
        }
    }
}

pub struct PsuedoLegalMoves {
    iter: std::iter::Chain<
        std::iter::Chain<
            std::iter::Chain<
                std::iter::Chain<std::iter::Chain<KnightMoves, BishopMoves>, RookMoves>,
                QueenMoves,
            >,
            PawnMoves,
        >,
        KingMoves,
    >,
}

impl PsuedoLegalMoves {
    fn new(board: &Board) -> PsuedoLegalMoves {
        PsuedoLegalMoves {
            iter: board
                .knight_moves_it()
                .chain(board.bishop_moves_it())
                .chain(board.rook_moves_it())
                .chain(board.queen_moves_it())
                .chain(board.pawn_moves_it())
                .chain(board.king_moves_it()),
        }
    }
}

impl Iterator for PsuedoLegalMoves {
    type Item = AlgebraicMove;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

impl Board {
    pub fn queen_attacks(&self, color: Color) -> BitBoard {
        let queens = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Queen as usize];

        let mut acc = BitBoard::empty();
        for i in queens {
            acc |= sliding_attacks::compute_rook_attacks(i, self.pieces());
            acc |= sliding_attacks::compute_bishop_attacks(i, self.pieces());
        }
        acc
    }

    pub fn queen_moves_it(&self) -> QueenMoves {
        let color = self.to_play;
        let queens = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Queen as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        QueenMoves::new(queens, allied_pieces, self.pieces())
    }

    pub fn queen_moves(&self, out: &mut Vec<Move>) {
        for i in self.queen_moves_it() {
            out.push(self.from_algeabraic(&i));
        }
    }

    pub fn queen_can_capture(&self, color: Color, target: Posn) -> Option<Move> {
        let queens = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Queen as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in queens {
            let mut attacks = sliding_attacks::compute_bishop_attacks(i, self.pieces());
            if i.rank() == target.rank() || i.file() == target.file() {
                attacks |= sliding_attacks::compute_rook_attacks(i, self.pieces())
            }
            if !attacks.contains(target) {
                continue;
            }
            for pos in attacks {
                if allied_pieces.contains(pos) {
                    continue;
                }
                if target == pos {
                    return Some(Move {
                        from: i,
                        to: pos,
                        piece: Piece::Queen,
                        capture: self.query_pos(pos, !color),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
        }
        None
    }

    pub fn rook_attacks(&self, color: Color) -> BitBoard {
        let rooks = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let mut acc = BitBoard::empty();
        for i in rooks {
            acc |= sliding_attacks::compute_rook_attacks(i, self.pieces())
        }
        acc
    }

    pub fn rook_moves_it(&self) -> RookMoves {
        let rooks = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        RookMoves::new(rooks, allied_pieces, self.pieces())
    }

    pub fn rook_moves(&self, out: &mut Vec<Move>) {
        for i in self.rook_moves_it() {
            out.push(self.from_algeabraic(&i));
        }
    }

    pub fn rook_can_capture(&self, color: Color, target: Posn) -> Option<Move> {
        let rooks = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in rooks {
            if i.rank() != target.rank() && i.file() != target.file() {
                continue;
            }
            let attacks = sliding_attacks::compute_rook_attacks(i, self.pieces());
            for pos in attacks {
                if allied_pieces.contains(pos) {
                    continue;
                }
                if pos == target {
                    return Some(Move {
                        from: i,
                        to: pos,
                        piece: Piece::Rook,
                        capture: self.query_pos(pos, !color),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
        }
        None
    }

    pub fn bishop_attacks(&self, color: Color) -> BitBoard {
        let bishops = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Bishop as usize];

        let mut acc = BitBoard::empty();
        for i in bishops {
            acc |= sliding_attacks::compute_bishop_attacks(i, self.pieces())
        }
        acc
    }

    pub fn bishop_moves_it(&self) -> BishopMoves {
        let bishops = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Bishop as usize];

        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        BishopMoves::new(bishops, allied_pieces, self.pieces())
    }

    pub fn bishop_moves(&self, out: &mut Vec<Move>) {
        for i in self.bishop_moves_it() {
            out.push(self.from_algeabraic(&i));
        }
    }

    pub fn bishop_can_capture(&self, color: Color, target: Posn) -> Option<Move> {
        let bishops = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Bishop as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in bishops {
            let attacks = sliding_attacks::compute_bishop_attacks(i, self.pieces());
            if !attacks.contains(target) {
                continue;
            }
            for pos in attacks {
                if allied_pieces.contains(pos) {
                    continue;
                }
                if pos == target {
                    return Some(Move {
                        from: i,
                        to: pos,
                        piece: Piece::Bishop,
                        capture: self.query_pos(pos, !color),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
        }
        None
    }

    pub fn king_attacks(&self, color: Color) -> BitBoard {
        let kings = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::King as usize];

        kings.fold(BitBoard::empty(), |acc, p| {
            acc | [
                p.no(),
                p.ne(),
                p.ea(),
                p.se(),
                p.so(),
                p.sw(),
                p.we(),
                p.nw(),
            ]
            .into_iter()
            .filter_map(|p| p)
            .fold(BitBoard::empty(), |acc, p| acc | p)
        })
    }

    pub fn king_moves_it(&self) -> KingMoves {
        let color = self.to_play;
        let kings = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::King as usize];

        let rooks = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

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
            .and_then(|x| Some(x.castling_ability.can_castle_king(self.to_play)))
            .unwrap_or(false)
        {
            !self.in_check_pos(from.ea().unwrap(), self.to_play)
                && !self.in_check_pos(from.ea().and_then(|x| x.ea()).unwrap(), self.to_play)
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
            .and_then(|x| Some(x.castling_ability.can_castle_queen(self.to_play)))
            .unwrap_or(false)
        {
            !self.in_check_pos(from.we().unwrap(), self.to_play)
                && !self.in_check_pos(from.we().and_then(|x| x.we()).unwrap(), self.to_play)
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

    pub fn king_moves(&self, out: &mut Vec<Move>) {
        for i in self.king_moves_it() {
            out.push(self.from_algeabraic(&i));
        }
    }

    pub fn king_can_capture(&self, color: Color, target: Posn) -> Option<Move> {
        let kings = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::King as usize];

        for i in kings {
            for pos in [
                i.no(),
                i.ne(),
                i.ea(),
                i.se(),
                i.so(),
                i.sw(),
                i.we(),
                i.nw(),
            ] {
                if Some(target) == pos {
                    return Some(Move {
                        from: i,
                        to: pos.unwrap(),
                        piece: Piece::King,
                        capture: self.query_pos(pos.unwrap(), !color),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
        }
        None
    }

    pub fn knight_attacks(&self, color: Color) -> BitBoard {
        let knights = match color {
            Color::White => self.white_pieces[Piece::Knight as usize],
            Color::Black => self.black_pieces[Piece::Knight as usize],
        };

        knights.into_iter().fold(BitBoard::empty(), |acc, knight| {
            acc | [
                knight.see(),
                knight.sse(),
                knight.ssw(),
                knight.sww(),
                knight.nww(),
                knight.nnw(),
                knight.nne(),
                knight.nee(),
            ]
            .into_iter()
            .filter_map(|p| p)
            .fold(BitBoard::empty(), |acc, p| acc | p)
        })
    }

    pub fn knight_moves_it(&self) -> KnightMoves {
        let knights = match self.to_play {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Knight as usize];

        let allied_pieces = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        KnightMoves::new(knights, allied_pieces)
    }

    pub fn knight_moves(&self, out: &mut Vec<Move>) {
        for i in self.knight_moves_it() {
            out.push(self.from_algeabraic(&i));
        }
    }

    pub fn knight_can_capture(&self, color: Color, target_pos: Posn) -> Option<Move> {
        let knights = match color {
            Color::White => self.white_pieces[Piece::Knight as usize],
            Color::Black => self.black_pieces[Piece::Knight as usize],
        };

        if knights.is_empty() {
            return None;
        }

        for knight in [
            target_pos.see(),
            target_pos.sse(),
            target_pos.ssw(),
            target_pos.sww(),
            target_pos.nww(),
            target_pos.nnw(),
            target_pos.nne(),
            target_pos.nee(),
        ] {
            if let Some(p) = knight {
                if knights.contains(p) {
                    return Some(Move {
                        from: p,
                        to: target_pos,
                        piece: Piece::Knight,
                        capture: self.query_pos(target_pos, !color),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
        }
        None
    }

    pub fn pawn_attacks(&self, color: Color) -> BitBoard {
        (match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Pawn as usize])
            .fold(BitBoard::empty(), |acc, p| {
                acc | match color {
                    Color::White => [p.ne(), p.nw()],
                    Color::Black => [p.se(), p.sw()],
                }
                .into_iter()
                .filter_map(|p| p)
                .fold(BitBoard::empty(), |acc, p| acc | p)
            })
    }

    pub fn pawn_moves_it(&self) -> PawnMoves {
        let color = self.to_play;
        let pawns = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Pawn as usize];

        let opponent_pieces = match color {
            Color::White => self.black_pieces(),
            Color::Black => self.white_pieces(),
        };

        PawnMoves::new(
            pawns,
            opponent_pieces,
            self.pieces(),
            color,
            self.move_rights.last().and_then(|r| r.ep_target),
        )
    }

    pub fn pawn_moves(&self, out: &mut Vec<Move>) {
        for i in self.pawn_moves_it() {
            out.push(self.from_algeabraic(&i));
        }
    }

    pub fn pawn_can_capture(&self, color: Color, target_pos: Posn) -> Option<Move> {
        let pawns = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Pawn as usize];

        let promo_rank = match color {
            Color::Black => Rank::One,
            Color::White => Rank::Eight,
        };

        for i in pawns {
            let mpush_pos = match color {
                Color::White => i.no(),
                Color::Black => i.so(),
            };

            for take in [
                mpush_pos.and_then(|x| x.we()),
                mpush_pos.and_then(|x| x.ea()),
            ] {
                if let Some(pos) = take {
                    if pos != target_pos {
                        continue;
                    }
                    if pos.rank() == promo_rank {
                        for piece in [Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop] {
                            return Some(Move {
                                from: i,
                                to: pos,
                                piece: Piece::Pawn,
                                capture: self.query_pos(pos, !color),
                                is_check: false,
                                is_mate: false,
                                is_en_passant: false,
                                is_castle_king: false,
                                is_castle_queen: false,
                                promotion: Some(piece),
                            });
                        }
                    } else {
                        return Some(Move {
                            from: i,
                            to: pos,
                            piece: Piece::Pawn,
                            capture: self.query_pos(pos, !color),
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                            promotion: None,
                        });
                    }
                }
            }
            // En Passant
            if let Some(ep_target) = self.move_rights.last().and_then(|x| x.ep_target) {
                let to = Posn::from(
                    if color == Color::White {
                        Rank::Six
                    } else {
                        Rank::Three
                    },
                    ep_target,
                );
                if to == target_pos
                    && ((color == Color::White && (i.nw() == Some(to) || i.ne() == Some(to)))
                        || (color == Color::Black && (i.sw() == Some(to) || i.se() == Some(to))))
                {
                    return Some(Move {
                        from: i,
                        to,
                        piece: Piece::Pawn,
                        capture: Some(Piece::Pawn),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: true,
                        is_castle_king: false,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
        }
        None
    }
    pub fn pseudo_legal_moves_it(&self) -> PsuedoLegalMoves {
        PsuedoLegalMoves::new(self)
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;

    #[test]
    fn rook_moves_empty() {
        for i in 0..64 {
            let mut board = empty_board(Color::White);
            board.white_pieces[Piece::Rook as usize] = BitBoard::from(Posn { pos: 1 << i });
            let mut moves = vec![];
            board.rook_moves(&mut moves);
            assert_eq!(moves.len(), 14);
            let mut before = board.clone();
            for m in &moves {
                before.make_move(&m);
                before.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }

    #[test]
    fn rook_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Rook as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Pawn as usize] = d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.rook_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn rook_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Rook as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] = d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.rook_moves(&mut moves);
        assert_eq!(moves.len(), 4);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn bishop_moves_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 7);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn bishop_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Pawn as usize] = e6() | e4() | c6() | c4();
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn bishop_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] = e6() | e4() | c6() | c4();
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 4);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn queen_moves_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 21);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        moves.clear();
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 27);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn queen_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Pawn as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn queen_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for m in &moves {
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
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        moves.clear();
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn king_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Knight as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn king_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Knight as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn knight_moves_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 2);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(d5());
        moves.clear();
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        moves.clear();
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 4);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }
    }

    #[test]
    fn knight_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Pawn as usize] =
            e7() | e3() | c7() | c3() | f4() | f6() | b4() | b6();
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn knight_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] =
            e7() | e3() | c7() | c3() | f4() | f6() | b4() | b6();
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for m in moves {
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
        for m in moves.clone() {
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
        for m in moves.clone() {
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
        for m in moves {
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
        assert_eq!(moves.last().unwrap().is_en_passant, true);
        assert_eq!(moves.last().unwrap().from, d5());
        assert_eq!(moves.last().unwrap().to, e6());
        let mut before = board.clone();
        for m in moves {
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
        assert_eq!(moves.last().unwrap().is_en_passant, true);
        assert_eq!(moves.last().unwrap().from, d4());
        assert_eq!(moves.last().unwrap().to, e3());
        let mut before = board.clone();
        for m in moves {
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
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
            let mut before = board.clone();
            for m in moves {
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
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 w Kkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
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
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w Qkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
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
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 b KQk - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
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
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b KQq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(&mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
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
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
            let mut before = board.clone();
            for m in moves {
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
                .filter(|x| x.is_castle_king || x.is_castle_queen)
                .collect();
            assert_eq!(only_castles.len(), 0);
            for m in moves {
                let before = board.clone();
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

            for m in moves {
                let before = board.clone();
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
            for m in moves {
                let before = board.clone();
                board.make_move(&m);
                board.undo_move(&m);
                assert_eq!(before, board);
            }
        }
    }
}
