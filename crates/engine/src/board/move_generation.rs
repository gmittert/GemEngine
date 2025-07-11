use crate::board::*;
use crate::piece_attack_tables::KING_ATTACKS;
use crate::{board::sliding_attacks, piece_attack_tables::KNIGHT_ATTACKS};

pub fn pawn_captures_it(
    color: Color,
    pawns: BitBoard,
    opponent_pieces: BitBoard,
    ep_target: Option<File>,
) -> impl Iterator<Item = AlgebraicMove> {
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
    let capture_targets = ep_pos | opponent_pieces;

    // Only search pawns that can capture
    let attacked_pawns = Board::pawn_attacks(capture_targets, !color);
    let pawns = attacked_pawns & pawns;

    let promo_rank = match color {
        Color::Black => Rank::One,
        Color::White => Rank::Eight,
    };

    pawns.into_iter().flat_map(move |from| {
        let (atk_we, atk_ea) = match color {
            Color::White => (
                from.no().and_then(|p| p.we()),
                from.no().and_then(|p| p.ea()),
            ),
            Color::Black => (
                from.so().and_then(|p| p.we()),
                from.so().and_then(|p| p.ea()),
            ),
        };
        let atk_we_iter = if let Some(to) = atk_we
            && capture_targets.contains(to)
        {
            Some(to)
        } else {
            None
        };
        let atk_ea_iter = if let Some(to) = atk_ea
            && capture_targets.contains(to)
        {
            Some(to)
        } else {
            None
        };
        let captures = atk_we_iter.into_iter().chain(atk_ea_iter.into_iter());
        captures.flat_map(move |to| {
            let (skip, take) = if to.rank() == promo_rank {
                (1, 4)
            } else {
                (0, 1)
            };
            [
                AlgebraicMove {
                    from,
                    to,
                    promotion: None,
                },
                AlgebraicMove {
                    from,
                    to,
                    promotion: Some(Piece::Queen),
                },
                AlgebraicMove {
                    from,
                    to,
                    promotion: Some(Piece::Knight),
                },
                AlgebraicMove {
                    from,
                    to,
                    promotion: Some(Piece::Rook),
                },
                AlgebraicMove {
                    from,
                    to,
                    promotion: Some(Piece::Bishop),
                },
            ]
            .into_iter()
            .skip(skip)
            .take(take)
        })
    })
}

pub fn pawn_non_captures_it(
    color: Color,
    pawns: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    // Skip blocked pawns
    let non_blocked = BitBoard(match color {
        Color::Black => pawns.0 & !(all_pieces.0 << 8),
        Color::White => pawns.0 & !(all_pieces.0 >> 8),
    });

    let promo_rank = match color {
        Color::Black => Rank::One,
        Color::White => Rank::Eight,
    };

    non_blocked.into_iter().flat_map(move |from| {
        let push_pos = match color {
            Color::White => from.no(),
            Color::Black => from.so(),
        }
        .unwrap();
        let promote = push_pos.rank() == promo_rank;
        let can_double_push = match color {
            Color::White => from.rank() == Rank::Two,
            Color::Black => from.rank() == Rank::Seven,
        };
        let double_push_pos = match color {
            Color::White => Posn::from(Rank::Four, from.file()),
            Color::Black => Posn::from(Rank::Five, from.file()),
        };
        let (skip_amount, take_amount) = if can_double_push && !all_pieces.contains(double_push_pos)
        {
            (0, 2)
        } else if promote {
            (2, 4)
        } else {
            (1, 1)
        };
        [
            AlgebraicMove {
                from,
                to: double_push_pos,
                promotion: None,
            },
            AlgebraicMove {
                from,
                to: push_pos,
                promotion: None,
            },
            AlgebraicMove {
                from,
                to: push_pos,
                promotion: Some(Piece::Queen),
            },
            AlgebraicMove {
                from,
                to: push_pos,
                promotion: Some(Piece::Knight),
            },
            AlgebraicMove {
                from,
                to: push_pos,
                promotion: Some(Piece::Rook),
            },
            AlgebraicMove {
                from,
                to: push_pos,
                promotion: Some(Piece::Bishop),
            },
        ]
        .into_iter()
        .skip(skip_amount)
        .take(take_amount)
    })
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

pub fn king_moves_it(
    mut kings: BitBoard,
    allied_pieces: BitBoard,
    can_castle_king: bool,
    can_castle_queen: bool,
) -> impl Iterator<Item = AlgebraicMove> {
    let from = kings.next().unwrap();
    let castle_king = if can_castle_king {
        Some(AlgebraicMove {
            from,
            to: Posn::from(from.rank(), File::G),
            promotion: None,
        })
    } else {
        None
    };
    let castle_queen = if can_castle_queen {
        Some(AlgebraicMove {
            from,
            to: Posn::from(from.rank(), File::C),
            promotion: None,
        })
    } else {
        None
    };

    let moves = (!allied_pieces & KING_ATTACKS[from.idx() as usize]).map(move |to| AlgebraicMove {
        from,
        to,
        promotion: None,
    });
    castle_king
        .into_iter()
        .chain(castle_queen.into_iter())
        .chain(moves)
}

pub fn knight_captures_it(
    knights: BitBoard,
    enemies: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    knights.into_iter().flat_map(move |from| {
        (enemies & KNIGHT_ATTACKS[from.idx() as usize]).map(move |to| AlgebraicMove {
            from,
            to,
            promotion: None,
        })
    })
}

pub fn rook_captures_it(
    rooks: BitBoard,
    enemies: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    rooks.flat_map(move |from| {
        (enemies & sliding_attacks::compute_rook_attacks(from, all_pieces)).map(move |to| {
            AlgebraicMove {
                from,
                to,
                promotion: None,
            }
        })
    })
}

pub fn bishop_captures_it(
    bishops: BitBoard,
    enemies: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    bishops.flat_map(move |from| {
        (enemies & sliding_attacks::compute_bishop_attacks(from, all_pieces)).map(move |to| {
            AlgebraicMove {
                from,
                to,
                promotion: None,
            }
        })
    })
}

pub fn queen_captures_it(
    queens: BitBoard,
    enemies: BitBoard,
    all_pieces: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    queens.flat_map(move |from| {
        (enemies
            & (sliding_attacks::compute_rook_attacks(from, all_pieces)
                | sliding_attacks::compute_bishop_attacks(from, all_pieces)))
        .map(move |to| AlgebraicMove {
            from,
            to,
            promotion: None,
        })
    })
}

pub fn king_captures_it(
    mut king: BitBoard,
    enemies: BitBoard,
) -> impl Iterator<Item = AlgebraicMove> {
    let from = king.next().unwrap();
    (enemies & KING_ATTACKS[from.idx() as usize]).map(move |to| AlgebraicMove {
        from,
        to,
        promotion: None,
    })
}

impl Board {
    pub fn can_castle_king(&self, to_play: Color) -> bool {
        let from = match to_play {
            Color::Black => e8(),
            Color::White => e1(),
        };
        if self
            .move_rights
            .last()
            .map(|x| x.castling_ability.can_castle_king(to_play))
            .unwrap_or(false)
        {
            !self.pieces().contains(Posn::from(from.rank(), File::G))
                && !self.pieces().contains(Posn::from(from.rank(), File::F))
                && !self.in_check(to_play)
                && !self.attacked_by_side(Posn::from(from.rank(), File::F), !to_play)
        } else {
            false
        }
    }
    pub fn can_castle_queen(&self, to_play: Color) -> bool {
        let from = match to_play {
            Color::Black => e8(),
            Color::White => e1(),
        };

        // Computing the ability to castle needs the board to compute castling through check
        if self
            .move_rights
            .last()
            .map(|x| x.castling_ability.can_castle_queen(to_play))
            .unwrap_or(false)
        {
            !self.pieces().contains(Posn::from(from.rank(), File::D))
                && !self.pieces().contains(Posn::from(from.rank(), File::B))
                && !self.pieces().contains(Posn::from(from.rank(), File::C))
                && !self.in_check(to_play)
                && !self.attacked_by_side(Posn::from(from.rank(), File::D), !to_play)
                && !self.attacked_by_side(Posn::from(from.rank(), File::C), !to_play)
        } else {
            false
        }
    }
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

    pub fn king_attacks(mut kings: BitBoard) -> BitBoard {
        if kings.0 == 0 {
            BitBoard::empty()
        } else {
            // There should always be one king.
            let from = kings.next().unwrap();
            KING_ATTACKS[from.idx() as usize]
        }
    }

    pub fn king_moves(&self, out: &mut Vec<AlgebraicMove>) {
        let color = self.to_play;
        let mut kings = self.piece(color, Piece::King);
        let rooks = self.piece(color, Piece::Rook);

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };
        let from = kings.next().unwrap();

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
            Color::White => [pos.so().and_then(|p| p.ea()), pos.so().and_then(|p| p.we())],
            Color::Black => [pos.no().and_then(|p| p.ea()), pos.no().and_then(|p| p.we())],
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
                    && (pawn.no().and_then(|p| p.we()) == Some(to)
                        || pawn.no().and_then(|p| p.ea()) == Some(to)))
                    || (self.to_play == Color::Black
                        && (pawn.so().and_then(|p| p.we()) == Some(to)
                            || pawn.so().and_then(|p| p.ea()) == Some(to)))
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
            Color::White => [
                target_pos.so().and_then(|p| p.ea()),
                target_pos.so().and_then(|p| p.we()),
            ],
            Color::Black => [
                target_pos.no().and_then(|p| p.ea()),
                target_pos.no().and_then(|p| p.we()),
            ],
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

    pub fn pseudo_legal_captures_it(&self) -> impl Iterator<Item = AlgebraicMove> + use<> {
        let pawns = self.piece(self.to_play, Piece::Pawn);
        let knights = self.piece(self.to_play, Piece::Knight);
        let rooks = self.piece(self.to_play, Piece::Rook);
        let bishops = self.piece(self.to_play, Piece::Bishop);
        let queens = self.piece(self.to_play, Piece::Queen);
        let king = self.piece(self.to_play, Piece::King);
        let enemies = match self.to_play {
            Color::White => self.black_pieces(),
            Color::Black => self.white_pieces(),
        };
        let all_pieces = self.pieces();
        pawn_captures_it(
            self.to_play,
            pawns,
            enemies,
            self.move_rights.last().and_then(|x| x.ep_target),
        )
        .chain(knight_captures_it(knights, enemies))
        .chain(bishop_captures_it(bishops, enemies, all_pieces))
        .chain(rook_captures_it(rooks, enemies, all_pieces))
        .chain(queen_captures_it(queens, enemies, all_pieces))
        .chain(king_captures_it(king, enemies))
    }

    pub fn pseudo_legal_moves_it(&self) -> impl Iterator<Item=AlgebraicMove> + use<> {
        let pawns = self.piece(self.to_play, Piece::Pawn);
        let knights = self.piece(self.to_play, Piece::Knight);
        let rooks = self.piece(self.to_play, Piece::Rook);
        let bishops = self.piece(self.to_play, Piece::Bishop);
        let queens = self.piece(self.to_play, Piece::Queen);
        let kings = self.piece(self.to_play, Piece::King);
        let allies = match self.to_play {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };
        let enemies = match self.to_play {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        let all_pieces = self.pieces();
        let can_castle_king = self.can_castle_king(self.to_play);
        let can_castle_queen = self.can_castle_queen(self.to_play);
        let ep_target = self.move_rights.last().and_then(|x| x.ep_target);

        pawn_non_captures_it(self.to_play, pawns, all_pieces)
        .chain(knight_moves_it(knights, allies))
        .chain(bishop_moves_it(bishops, allies, all_pieces))
        .chain(rook_moves_it(rooks, allies, all_pieces))
        .chain(queen_moves_it(queens, allies, all_pieces))
        .chain(king_moves_it( kings, allies, can_castle_king, can_castle_queen))
        .chain(pawn_captures_it(self.to_play, pawns, enemies, ep_target))
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
            board.add_piece(Color::White, Piece::Rook, Posn::from_idx(i));
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
            board.remove_piece(Color::White, Piece::Rook, Posn::from_idx(i));
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
