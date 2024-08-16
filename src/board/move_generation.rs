pub use crate::board::moves::*;
pub use crate::board::posn::*;
use crate::board::sliding_attacks;
pub use crate::board::*;

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

    pub fn queen_moves(&self, color: Color, out: &mut Vec<Move>) {
        let queens = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Queen as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in queens {
            let attacks = sliding_attacks::compute_rook_attacks(i, self.pieces())
                | sliding_attacks::compute_bishop_attacks(i, self.pieces());
            for pos in attacks {
                if allied_pieces.contains(pos) {
                    continue;
                }
                out.push(Move {
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

    pub fn rook_moves(&self, color: Color, out: &mut Vec<Move>) {
        let rooks = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        for i in rooks {
            let attacks = sliding_attacks::compute_rook_attacks(i, self.pieces());
            for pos in attacks {
                if allied_pieces.contains(pos) {
                    continue;
                }
                out.push(Move {
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

    pub fn bishop_moves(&self, color: Color, out: &mut Vec<Move>) {
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
            for pos in attacks {
                if allied_pieces.contains(pos) {
                    continue;
                }
                out.push(Move {
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

    pub fn king_moves(&self, color: Color, out: &mut Vec<Move>) {
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

        let all_pieces = self.white_pieces() | self.black_pieces();

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
                pos.into_iter()
                    .filter(|pos| !allied_pieces.contains(*pos))
                    .for_each(|pos| {
                        out.push(Move {
                            from: i,
                            to: pos,
                            piece: Piece::King,
                            capture: self.query_pos(pos, !color),
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                            promotion: None,
                        })
                    });
            }
            if self
                .move_rights
                .last()
                .and_then(|x| Some(x.castling_ability.can_castle_king(color)))
                .unwrap_or(false)
            {
                if !self.in_check_pos(i.ea().unwrap(), color)
                    && !self.in_check_pos(i.ea().and_then(|x| x.ea()).unwrap(), color)
                    && !self.in_check(color)
                    && !all_pieces.contains(i.ea().unwrap())
                    && !all_pieces.contains(i.ea().and_then(|x| x.ea()).unwrap())
                    && rooks.contains(i.ea().and_then(|x| x.ea()).and_then(|x| x.ea()).unwrap())
                {
                    out.push(Move {
                        from: i,
                        to: i.ea().and_then(|x| x.ea()).unwrap(),
                        piece: Piece::King,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: true,
                        is_castle_queen: false,
                        promotion: None,
                    });
                }
            }
            if self
                .move_rights
                .last()
                .and_then(|x| Some(x.castling_ability.can_castle_queen(color)))
                .unwrap_or(false)
            {
                if !self.in_check_pos(i.we().unwrap(), color)
                    && !self.in_check_pos(i.we().and_then(|x| x.we()).unwrap(), color)
                    && !self.in_check(color)
                    && !all_pieces.contains(i.we().unwrap())
                    && !all_pieces.contains(i.we().and_then(|x| x.we()).unwrap())
                    && !all_pieces
                        .contains(i.we().and_then(|x| x.we()).and_then(|x| x.we()).unwrap())
                    && rooks.contains(
                        i.we()
                            .and_then(|x| x.we())
                            .and_then(|x| x.we())
                            .and_then(|x| x.we())
                            .unwrap(),
                    )
                {
                    out.push(Move {
                        from: i,
                        to: i.we().and_then(|x| x.we()).unwrap(),
                        piece: Piece::King,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: true,
                        promotion: None,
                    });
                }
            }
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

    pub fn knight_moves(&self, color: Color, out: &mut Vec<Move>) {
        let knights = match color {
            Color::White => self.white_pieces[Piece::Knight as usize],
            Color::Black => self.black_pieces[Piece::Knight as usize],
        };
        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        knights.into_iter().for_each(|knight| {
            [
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
            .filter(|p| !allied_pieces.contains(*p))
            .for_each(|p| {
                out.push(Move {
                    from: knight,
                    to: p,
                    piece: Piece::Knight,
                    capture: self.query_pos(p, !color),
                    is_check: false,
                    is_mate: false,
                    is_en_passant: false,
                    is_castle_king: false,
                    is_castle_queen: false,
                    promotion: None,
                })
            })
        });
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

    pub fn pawn_moves(&self, color: Color, out: &mut Vec<Move>) {
        // Single Pushes
        let pawns = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Pawn as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        let opponent_pieces = match color {
            Color::White => self.black_pieces(),
            Color::Black => self.white_pieces(),
        };
        let promo_rank = match color {
            Color::Black => Rank::One,
            Color::White => Rank::Eight,
        };

        for i in pawns {
            let mpush_pos = match color {
                Color::White => i.no(),
                Color::Black => i.so(),
            };
            if let Some(push_pos) = mpush_pos {
                if !allied_pieces.contains(push_pos) && !opponent_pieces.contains(push_pos) {
                    if push_pos.rank() == promo_rank {
                        for piece in [Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop] {
                            out.push(Move {
                                from: i,
                                to: push_pos,
                                piece: Piece::Pawn,
                                capture: None,
                                is_check: false,
                                is_mate: false,
                                is_en_passant: false,
                                is_castle_king: false,
                                is_castle_queen: false,
                                promotion: Some(piece),
                            });
                        }
                    } else {
                        out.push(Move {
                            from: i,
                            to: push_pos,
                            piece: Piece::Pawn,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                            promotion: None,
                        });
                        let can_double_push = match color {
                            Color::White => i.rank() == Rank::Two,
                            Color::Black => i.rank() == Rank::Seven,
                        };
                        if can_double_push {
                            let mdouble_push_pos = match color {
                                Color::White => i.no().and_then(|x| x.no()),
                                Color::Black => i.so().and_then(|x| x.so()),
                            };
                            if let Some(double_push_pos) = mdouble_push_pos {
                                if !allied_pieces.contains(double_push_pos)
                                    && !opponent_pieces.contains(double_push_pos)
                                {
                                    out.push(Move {
                                        from: i,
                                        to: double_push_pos,
                                        piece: Piece::Pawn,
                                        capture: None,
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
                    }
                }
            }

            for take in [
                mpush_pos.and_then(|x| x.we()),
                mpush_pos.and_then(|x| x.ea()),
            ] {
                take.into_iter()
                    .filter(|pos| opponent_pieces.contains(*pos))
                    .for_each(|pos| {
                        if pos.rank() == promo_rank {
                            for piece in [Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop] {
                                out.push(Move {
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
                            out.push(Move {
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
                            })
                        }
                    });
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
                if (color == Color::White && (i.nw() == Some(to) || i.ne() == Some(to)))
                    || (color == Color::Black && (i.sw() == Some(to) || i.se() == Some(to)))
                {
                    out.push(Move {
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
                    })
                }
            }
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
            board.rook_moves(Color::White, &mut moves);
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
        board.rook_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn rook_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Rook as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] = d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.rook_moves(Color::White, &mut moves);
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
        board.bishop_moves(Color::White, &mut moves);
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
        board.bishop_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn bishop_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] = e6() | e4() | c6() | c4();
        let mut moves = vec![];
        board.bishop_moves(Color::White, &mut moves);
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
        board.queen_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 21);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        moves.clear();
        board.queen_moves(Color::White, &mut moves);
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
        board.queen_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn queen_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.queen_moves(Color::White, &mut moves);
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
        board.king_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 3);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        moves.clear();
        board.king_moves(Color::White, &mut moves);
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
        board.king_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn king_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Knight as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.king_moves(Color::White, &mut moves);
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
        board.knight_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 2);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(d5());
        moves.clear();
        board.knight_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 8);
        let mut before = board.clone();
        for m in &moves {
            before.make_move(&m);
            before.undo_move(&m);
            assert_eq!(before, board);
        }

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        moves.clear();
        board.knight_moves(Color::White, &mut moves);
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
        board.knight_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn knight_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] =
            e7() | e3() | c7() | c3() | f4() | f6() | b4() | b6();
        let mut moves = vec![];
        board.knight_moves(Color::White, &mut moves);
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
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 1);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 2);

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a5());
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 1);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn pawn_moves_blocked_ally() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a3());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a3());
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 1);

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a5());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a6());
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn pawn_moves_blocked_opponent() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a3());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a3());
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(a2());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(a4());
        board.pawn_moves(Color::White, &mut moves);
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
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a6());
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(a7());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        board.pawn_moves(Color::Black, &mut moves);
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
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 2);

        board.to_play = Color::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(d5());
        board.white_pieces[Piece::Pawn as usize] = c4() | e4() | d4();
        board.pawn_moves(Color::Black, &mut moves);
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
        board.pawn_moves(Color::White, &mut moves);
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
        board.pawn_moves(Color::Black, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::Black, &mut moves);
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
            board.king_moves(Color::Black, &mut moves);
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
            board.king_moves(Color::Black, &mut moves);
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
            board.king_moves(Color::Black, &mut moves);
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
                Board::from_fen("8/8/8/8/8/8/8/R3K3 b - - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 b Kkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w Qkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::Black, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 b KQk - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::Black, &mut moves);
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
            board.king_moves(Color::Black, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b KQq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::Black, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.king_moves(Color::White, &mut moves);
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
            board.pawn_moves(Color::White, &mut moves);
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
            board.pawn_moves(Color::Black, &mut moves);
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
