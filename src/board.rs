mod moves;
mod posn;
pub use crate::board::moves::*;
pub use crate::board::posn::*;
use std::fmt;
use std::ops;

const EIGHTH_RANK: u64 = 0x0100_0000_0000_0000;
const SECOND_RANK: u64 = 0x0000_0000_0000_0100;
const H_FILE: u64 = 0x0101_0101_0101_0101;
const A_FILE: u64 = 0x8080_8080_8080_8080;
const NOT_A_FILE: u64 = 0xFEFE_FEFE_FEFE_FEFE;
const NOT_A_B_FILE: u64 = 0xFCFC_FCFC_FCFC_FCFC;
const NOT_H_FILE: u64 = 0x7F7F_7F7F_7F7F_7F7F;
const NOT_G_H_FILE: u64 = 0x3F3F_3F3F_3F3F_3F3F;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct BitBoard {
    bits: u64,
}

impl BitBoard {
    pub fn from(p: &Posn) -> BitBoard {
        BitBoard { bits: 1 << p.pos }
    }

    pub fn make_move(&mut self, m: &Move) {
        let from = BitBoard::from(&m.from);
        let to = BitBoard::from(&m.to);

        self.bits = self.bits & !from.bits | to.bits;
    }

    pub fn undo_move(&mut self, m: &Move) {
        let from = BitBoard::from(&m.from);
        let to = BitBoard::from(&m.to);

        self.bits = (self.bits & !to.bits) | from.bits
    }
}

#[derive(Debug, PartialEq)]
pub struct Board {
    black_pieces: [BitBoard; 6],
    white_pieces: [BitBoard; 6],

    to_play: Turn,
    turn_count: u16,
}

impl Board {
    pub fn make_move(&mut self, m: &Move) {
        match m.turn {
            Turn::Black => self.black_pieces[m.piece as usize].make_move(m),
            Turn::White => self.white_pieces[m.piece as usize].make_move(m),
        }
        self.to_play = !self.to_play;
        self.turn_count += 1;
    }

    pub fn undo_move(&mut self, m: &Move) {
        match m.turn {
            Turn::Black => self.black_pieces[m.piece as usize].undo_move(m),
            Turn::White => self.white_pieces[m.piece as usize].undo_move(m),
        };
        match m.capture {
            Some(p) => match m.turn {
                Turn::Black => self.white_pieces[p as usize] |= m.to,
                Turn::White => self.black_pieces[p as usize] |= m.to,
            },
            None => (),
        };
        self.to_play = !self.to_play;
        self.turn_count -= 1;
    }

    pub fn current_board(&self) -> BitBoard {
        let mut b = BitBoard { bits: 0 };
        for i in 0..6 {
            b |= self.black_pieces[i];
            b |= self.white_pieces[i];
        }
        b
    }

    pub fn query_pos(&self, p: &Posn) -> Option<Piece> {
        let pieces: [Piece; 6] = [
            Piece::Pawn,
            Piece::Rook,
            Piece::Knight,
            Piece::Bishop,
            Piece::Queen,
            Piece::King,
        ];
        for i in pieces {
            if (self.black_pieces[i as usize].bits | self.white_pieces[i as usize].bits)
                & BitBoard::from(&p).bits
                != 0
            {
                return Some(i);
            }
        }
        None
    }

    pub fn white_pieces(&self) -> BitBoard {
        let mut b = BitBoard { bits: 0 };
        for i in 0..6 {
            b |= self.white_pieces[i];
        }
        b
    }

    pub fn black_pieces(&self) -> BitBoard {
        let mut b = BitBoard { bits: 0 };
        for i in 0..6 {
            b |= self.black_pieces[i];
        }
        b
    }

    pub fn queen_moves(&self, out: &mut Vec<Move>) {
        let queens = match self.to_play {
            Turn::White => self.white_pieces,
            Turn::Black => self.black_pieces,
        }[Piece::Queen as usize]
            .bits;

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        let opponent_pieces = match self.to_play {
            Turn::Black => self.white_pieces(),
            Turn::White => self.black_pieces(),
        }
        .bits;
        for i in 0..64 as u8 {
            if queens & (1 << i) != 0 {
                // North
                let mut north_pos = 1 << i;
                while north_pos < EIGHTH_RANK {
                    north_pos <<= 8;
                    if north_pos & allied_pieces != 0 {
                        break;
                    }
                    if north_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: north_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: north_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // South
                let mut south_pos = 1 << i;
                while south_pos >= SECOND_RANK {
                    south_pos >>= 8;
                    if south_pos & allied_pieces != 0 {
                        break;
                    }
                    if south_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: south_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: south_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // East
                let mut east_pos = 1 << i;
                while east_pos & H_FILE == 0 {
                    east_pos >>= 1;
                    if east_pos & allied_pieces != 0 {
                        break;
                    }
                    if east_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: east_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: east_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // West
                let mut west_pos = 1 << i;
                while west_pos & A_FILE == 0 {
                    west_pos <<= 1;
                    if west_pos & allied_pieces != 0 {
                        break;
                    }
                    if west_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: west_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: west_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }

                // North West
                let mut nw_pos = 1 << i;
                while nw_pos < EIGHTH_RANK && (nw_pos & A_FILE) == 0 {
                    nw_pos <<= 9;
                    if nw_pos & allied_pieces != 0 {
                        break;
                    }
                    if nw_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: nw_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: nw_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // North East
                let mut ne_pos = 1 << i;
                while ne_pos < EIGHTH_RANK && (ne_pos & H_FILE) == 0 {
                    ne_pos <<= 7;
                    if ne_pos & allied_pieces != 0 {
                        break;
                    }
                    if ne_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: ne_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: ne_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // South West
                let mut sw_pos = 1 << i;
                while sw_pos >= SECOND_RANK && (sw_pos & A_FILE) == 0 {
                    sw_pos >>= 7;
                    if sw_pos & allied_pieces != 0 {
                        break;
                    }
                    if sw_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: sw_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: sw_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // South East
                let mut se_pos = 1 << i;
                while se_pos >= SECOND_RANK && (se_pos & H_FILE) == 0 {
                    se_pos >>= 9;
                    if se_pos & allied_pieces != 0 {
                        break;
                    }
                    if se_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: se_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Queen,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: se_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Queen,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
            }
        }
    }

    pub fn rook_moves(&self, out: &mut Vec<Move>) {
        let rooks = match self.to_play {
            Turn::White => self.white_pieces,
            Turn::Black => self.black_pieces,
        }[Piece::Rook as usize]
            .bits;

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        let opponent_pieces = match self.to_play {
            Turn::Black => self.white_pieces(),
            Turn::White => self.black_pieces(),
        }
        .bits;
        for i in 0..64 {
            if rooks & (1 << i) != 0 {
                // North
                let mut north_pos = 1 << i;
                while north_pos < EIGHTH_RANK {
                    north_pos <<= 8;
                    if north_pos & allied_pieces != 0 {
                        break;
                    }
                    if north_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: north_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Rook,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: north_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Rook,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // South
                let mut south_pos = 1 << i;
                while south_pos >= SECOND_RANK {
                    south_pos >>= 8;
                    if south_pos & allied_pieces != 0 {
                        break;
                    }
                    if south_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: south_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Rook,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: south_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Rook,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // East
                let mut east_pos = 1 << i;
                while east_pos & H_FILE == 0 {
                    east_pos >>= 1;
                    if east_pos & allied_pieces != 0 {
                        break;
                    }
                    if east_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: east_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Rook,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: east_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Rook,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // West
                let mut west_pos = 1 << i;
                while west_pos & A_FILE == 0 {
                    west_pos <<= 1;
                    if west_pos & allied_pieces != 0 {
                        break;
                    }
                    if west_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: west_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Rook,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: west_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Rook,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
            }
        }
    }

    pub fn bishop_moves(&self, out: &mut Vec<Move>) {
        let bishops = match self.to_play {
            Turn::White => self.white_pieces,
            Turn::Black => self.black_pieces,
        }[Piece::Bishop as usize]
            .bits;

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        let opponent_pieces = match self.to_play {
            Turn::Black => self.white_pieces(),
            Turn::White => self.black_pieces(),
        }
        .bits;
        for i in 0..64 as u8 {
            if bishops & (1 << i) != 0 {
                // North West
                let mut nw_pos = 1 << i;
                while nw_pos < EIGHTH_RANK && (nw_pos & A_FILE) == 0 {
                    nw_pos <<= 9;
                    if nw_pos & allied_pieces != 0 {
                        break;
                    }
                    if nw_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: nw_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Bishop,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: nw_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Bishop,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // North East
                let mut ne_pos = 1 << i;
                while ne_pos < EIGHTH_RANK && (ne_pos & H_FILE) == 0 {
                    ne_pos <<= 7;
                    if ne_pos & allied_pieces != 0 {
                        break;
                    }
                    if ne_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: ne_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Bishop,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: ne_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Bishop,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // South West
                let mut sw_pos = 1 << i;
                while sw_pos >= SECOND_RANK && (sw_pos & A_FILE) == 0 {
                    sw_pos >>= 7;
                    if sw_pos & allied_pieces != 0 {
                        break;
                    }
                    if sw_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: sw_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Bishop,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: sw_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Bishop,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                // South East
                let mut se_pos = 1 << i;
                while se_pos >= SECOND_RANK && (se_pos & H_FILE) == 0 {
                    se_pos >>= 9;
                    if se_pos & allied_pieces != 0 {
                        break;
                    }
                    if se_pos & opponent_pieces != 0 {
                        let to = Posn {
                            pos: se_pos.trailing_zeros() as u8,
                        };
                        out.push(Move {
                            from: Posn { pos: i },
                            to: to.clone(),
                            turn: self.to_play,
                            piece: Piece::Bishop,
                            capture: self.query_pos(&to),
                            is_check: false,
                            is_mate: false,
                        });
                        break;
                    }
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: se_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Bishop,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
            }
        }
    }

    pub fn king_moves(&self, out: &mut Vec<Move>) {
        fn no(b: u64) -> u64 {
            b << 8
        }
        fn no_ea(b: u64) -> u64 {
            (b << 7) & NOT_H_FILE
        }
        fn ea(b: u64) -> u64 {
            (b >> 1) & NOT_H_FILE
        }
        fn so_ea(b: u64) -> u64 {
            (b >> 9) & NOT_H_FILE
        }
        fn so(b: u64) -> u64 {
            b >> 8
        }
        fn so_we(b: u64) -> u64 {
            (b >> 7) & NOT_A_FILE
        }
        fn we(b: u64) -> u64 {
            (b << 1) & NOT_A_FILE
        }
        fn no_we(b: u64) -> u64 {
            (b << 9) & NOT_A_FILE
        }

        let kings = match self.to_play {
            Turn::White => self.white_pieces,
            Turn::Black => self.black_pieces,
        }[Piece::King as usize]
            .bits;

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        for i in 0..64 as u8 {
            if kings & (1 << i) != 0 {
                for pos in [
                    no(1 << i),
                    no_ea(1 << i),
                    ea(1 << i),
                    so_ea(1 << i),
                    so(1 << i),
                    so_we(1 << i),
                    we(1 << i),
                    no_we(1 << i),
                ] {
                    if pos != 0 && (pos & allied_pieces == 0) {
                        out.push(Move {
                            from: Posn { pos: i },
                            to: Posn {
                                pos: pos.trailing_zeros() as u8,
                            },
                            turn: self.to_play,
                            piece: Piece::King,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                        });
                    }
                }
            }
        }
    }

    pub fn knight_moves(&self, out: &mut Vec<Move>) {
        fn no_no_ea(b: u64) -> u64 {
            (b << 17) & NOT_A_FILE
        }
        fn no_ea_ea(b: u64) -> u64 {
            (b << 10) & NOT_A_B_FILE
        }
        fn so_ea_ea(b: u64) -> u64 {
            (b >> 6) & NOT_A_B_FILE
        }
        fn so_so_ea(b: u64) -> u64 {
            (b >> 15) & NOT_A_FILE
        }
        fn no_no_we(b: u64) -> u64 {
            (b << 15) & NOT_H_FILE
        }
        fn no_we_we(b: u64) -> u64 {
            (b << 6) & NOT_G_H_FILE
        }
        fn so_we_we(b: u64) -> u64 {
            (b >> 10) & NOT_G_H_FILE
        }
        fn so_so_we(b: u64) -> u64 {
            (b >> 17) & NOT_H_FILE
        }
        let knights = match self.to_play {
            Turn::White => self.white_pieces[Piece::Knight as usize].bits,
            Turn::Black => self.black_pieces[Piece::Knight as usize].bits,
        };

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        for i in 0..64 as u8 {
            if knights & (1 << i) != 0 {
                for pos in [
                    no_no_ea(1 << i),
                    no_ea_ea(1 << i),
                    so_ea_ea(1 << i),
                    so_so_ea(1 << i),
                    so_so_we(1 << i),
                    so_we_we(1 << i),
                    no_we_we(1 << i),
                    no_no_we(1 << i),
                ] {
                    if pos != 0 && (pos & allied_pieces == 0) {
                        out.push(Move {
                            from: Posn { pos: i },
                            to: Posn {
                                pos: pos.trailing_zeros() as u8,
                            },
                            turn: self.to_play,
                            piece: Piece::Knight,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                        });
                    }
                }
            }
        }
    }

    pub fn pawn_moves(&self, out: &mut Vec<Move>) {
        // Single Pushes
        let pawns = match self.to_play {
            Turn::White => self.white_pieces[Piece::Pawn as usize].bits,
            Turn::Black => self.black_pieces[Piece::Pawn as usize].bits,
        };

        let allied_pieces = match self.to_play {
            Turn::White => self.white_pieces(),
            Turn::Black => self.black_pieces(),
        }
        .bits;

        let opponent_pieces = match self.to_play {
            Turn::White => self.black_pieces(),
            Turn::Black => self.white_pieces(),
        }
        .bits;

        for i in 0..64 as u8 {
            if pawns & (1 << i) != 0 {
                let push_i = match self.to_play {
                    Turn::White => i + 8,
                    Turn::Black => i - 8,
                };
                let push_pos = 1 << push_i;
                if push_pos != 0
                    && (push_pos & allied_pieces == 0)
                    && (push_pos & opponent_pieces == 0)
                {
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn { pos: push_i },
                        turn: self.to_play,
                        piece: Piece::Pawn,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                    });
                }
                let can_double_push = match self.to_play {
                    Turn::White => i >= 8 && i <= 15,
                    Turn::Black => i >= 48 && i <= 55,
                };
                if can_double_push {
                    let double_push_i = match self.to_play {
                        Turn::White => i + 16,
                        Turn::Black => i - 16,
                    };
                    let double_push_pos = 1 << double_push_i;
                    if double_push_pos != 0
                        && (push_pos & allied_pieces == 0)
                        && (push_pos & opponent_pieces == 0)
                        && (double_push_pos & allied_pieces == 0)
                        && (double_push_pos & opponent_pieces == 0)
                    {
                        out.push(Move {
                            from: Posn { pos: i },
                            to: Posn { pos: double_push_i },
                            turn: self.to_play,
                            piece: Piece::Pawn,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                        });
                    }
                }
                let capture_west_pos = match self.to_play {
                    Turn::White => (1 << i << 9) & NOT_A_FILE,
                    Turn::Black => (1 << i >> 7) & NOT_A_FILE,
                };
                let capture_east_pos = match self.to_play {
                    Turn::White => (1 << i << 7) & NOT_H_FILE,
                    Turn::Black => (1 << i >> 9) & NOT_H_FILE,
                };
                if capture_west_pos & opponent_pieces != 0 {
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: capture_west_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Pawn,
                        capture: self.query_pos(&Posn {
                            pos: capture_west_pos.trailing_zeros() as u8,
                        }),
                        is_check: false,
                        is_mate: false,
                    });
                }
                if capture_east_pos & opponent_pieces != 0 {
                    out.push(Move {
                        from: Posn { pos: i },
                        to: Posn {
                            pos: capture_east_pos.trailing_zeros() as u8,
                        },
                        turn: self.to_play,
                        piece: Piece::Pawn,
                        capture: self.query_pos(&Posn {
                            pos: capture_east_pos.trailing_zeros() as u8,
                        }),
                        is_check: false,
                        is_mate: false,
                    });
                }
            }
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars: [char; 64] = ['.'; 64];

        for i in 0..64 {
            if (self.black_pieces[Piece::King as usize].bits & (1 << i)) != 0 {
                chars[i] = '♔'
            } else if (self.black_pieces[Piece::Queen as usize].bits & (1 << i)) != 0 {
                chars[i] = '♕'
            } else if (self.black_pieces[Piece::Knight as usize].bits & (1 << i)) != 0 {
                chars[i] = '♘'
            } else if (self.black_pieces[Piece::Pawn as usize].bits & (1 << i)) != 0 {
                chars[i] = '♙'
            } else if (self.black_pieces[Piece::Bishop as usize].bits & (1 << i)) != 0 {
                chars[i] = '♗'
            } else if (self.black_pieces[Piece::Rook as usize].bits & (1 << i)) != 0 {
                chars[i] = '♖'
            } else if (self.white_pieces[Piece::King as usize].bits & (1 << i)) != 0 {
                chars[i] = '♚'
            } else if (self.white_pieces[Piece::Queen as usize].bits & (1 << i)) != 0 {
                chars[i] = '♛'
            } else if (self.white_pieces[Piece::Knight as usize].bits & (1 << i)) != 0 {
                chars[i] = '♞'
            } else if (self.white_pieces[Piece::Pawn as usize].bits & (1 << i)) != 0 {
                chars[i] = '♟'
            } else if (self.white_pieces[Piece::Bishop as usize].bits & (1 << i)) != 0 {
                chars[i] = '♝'
            } else if (self.white_pieces[Piece::Rook as usize].bits & (1 << i)) != 0 {
                chars[i] = '♜'
            }
        }
        for rank in 0..8 {
            for file in 0..8 {
                write!(f, "{}", chars[(7 - file) + (8 * (7 - rank))])?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl ops::BitOr<Posn> for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: Posn) -> Self::Output {
        BitBoard {
            bits: self.bits | BitBoard::from(&rhs).bits,
        }
    }
}

impl ops::BitOr for BitBoard {
    type Output = Self;
    fn bitor(self, rhs: BitBoard) -> Self::Output {
        BitBoard {
            bits: self.bits | rhs.bits,
        }
    }
}

impl ops::BitOrAssign<Posn> for BitBoard {
    fn bitor_assign(&mut self, rhs: Posn) {
        self.bits |= BitBoard::from(&rhs).bits;
    }
}

impl ops::BitOrAssign for BitBoard {
    fn bitor_assign(&mut self, rhs: BitBoard) {
        self.bits |= rhs.bits;
    }
}

impl ops::BitOr for Posn {
    type Output = BitBoard;
    fn bitor(self, rhs: Self) -> Self::Output {
        BitBoard::from(&self) | BitBoard::from(&rhs)
    }
}

impl ops::BitOr<BitBoard> for Posn {
    type Output = BitBoard;
    fn bitor(self, rhs: BitBoard) -> Self::Output {
        BitBoard::from(&self) | rhs
    }
}

pub fn empty_board(turn: Turn) -> Board {
    Board {
        black_pieces: [
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
        ],

        white_pieces: [
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
            BitBoard { bits: 0 },
        ],

        to_play: turn,
        turn_count: 1,
    }
}

pub fn starting_board() -> Board {
    Board {
        black_pieces: [
            a7() | b7() | c7() | d7() | e7() | f7() | g7() | h7(),
            a8() | h8(),
            b8() | g8(),
            c8() | f8(),
            BitBoard::from(&d8()),
            BitBoard::from(&e8()),
        ],

        white_pieces: [
            a2() | b2() | c2() | d2() | e2() | f2() | g2() | h2(),
            a1() | h1(),
            b1() | g1(),
            c1() | f1(),
            BitBoard::from(&d1()),
            BitBoard::from(&e1()),
        ],

        to_play: Turn::White,
        turn_count: 1,
    }
}

#[cfg(test)]
mod tests {
    use crate::board::*;
    #[test]
    fn formatted_start() {
        let exp =
            "♖♘♗♕♔♗♘♖\n♙♙♙♙♙♙♙♙\n........\n........\n........\n........\n♟♟♟♟♟♟♟♟\n♜♞♝♛♚♝♞♜\n";
        assert_eq!(format!("{}", crate::board::starting_board()), exp);
    }

    #[test]
    fn make_move() {
        let mut board = starting_board();
        let board2 = starting_board();
        let m = Move {
            from: e2(),
            to: e4(),
            turn: Turn::White,
            piece: Piece::Pawn,
            capture: None,
            is_check: false,
            is_mate: false,
        };
        board.make_move(&m);
        board.undo_move(&m);
        assert_eq!(board, board2);
    }

    #[test]
    fn rook_moves_empty() {
        for i in 0..64 {
            let mut board = empty_board(Turn::White);
            board.white_pieces[Piece::Rook as usize] = BitBoard { bits: 1 << i };
            let mut moves = vec![];
            board.rook_moves(&mut moves);
            assert_eq!(moves.len(), 14);
        }
    }

    #[test]
    fn rook_moves_blocked_ally() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Rook as usize] = BitBoard::from(&d5());
        board.white_pieces[Piece::Pawn as usize] = d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.rook_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn rook_moves_blocked_opponent() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Rook as usize] = BitBoard::from(&d5());
        board.black_pieces[Piece::Pawn as usize] = d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.rook_moves(&mut moves);
        assert_eq!(moves.len(), 4);
    }

    #[test]
    fn bishop_moves_empty() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(&a1());
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 7);
    }

    #[test]
    fn bishop_moves_blocked_ally() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(&d5());
        board.white_pieces[Piece::Pawn as usize] = e6() | e4() | c6() | c4();
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn bishop_moves_blocked_opponent() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(&d5());
        board.black_pieces[Piece::Pawn as usize] = e6() | e4() | c6() | c4();
        let mut moves = vec![];
        board.bishop_moves(&mut moves);
        assert_eq!(moves.len(), 4);
    }

    #[test]
    fn queen_moves_empty() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(&a1());
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 21);

        board.white_pieces[Piece::Queen as usize] = BitBoard::from(&d5());
        moves.clear();
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 27);
    }

    #[test]
    fn queen_moves_blocked_ally() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(&d5());
        board.white_pieces[Piece::Pawn as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn queen_moves_blocked_opponent() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(&d5());
        board.black_pieces[Piece::Pawn as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.queen_moves(&mut moves);
        assert_eq!(moves.len(), 8);
    }

    #[test]
    fn king_moves_empty() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(&a1());
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 3);

        board.white_pieces[Piece::King as usize] = BitBoard::from(&d5());
        moves.clear();
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 8);
    }

    #[test]
    fn king_moves_blocked_ally() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(&d5());
        board.white_pieces[Piece::Knight as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn king_moves_blocked_opponent() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::King as usize] = BitBoard::from(&d5());
        board.black_pieces[Piece::Knight as usize] =
            e6() | e4() | c6() | c4() | d4() | d6() | c5() | e5();
        let mut moves = vec![];
        board.king_moves(&mut moves);
        assert_eq!(moves.len(), 8);
    }

    #[test]
    fn knight_moves_empty() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a1());
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 2);

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&d5());
        moves.clear();
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 8);

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a5());
        moves.clear();
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 4);
    }

    #[test]
    fn knight_moves_blocked_ally() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&d5());
        board.white_pieces[Piece::Pawn as usize] =
            e7() | e3() | c7() | c3() | f4() | f6() | b4() | b6();
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 0);
    }

    #[test]
    fn knight_moves_blocked_opponent() {
        let mut board = empty_board(Turn::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&d5());
        board.black_pieces[Piece::Pawn as usize] =
            e7() | e3() | c7() | c3() | f4() | f6() | b4() | b6();
        let mut moves = vec![];
        board.knight_moves(&mut moves);
        assert_eq!(moves.len(), 8);
    }

    #[test]
    fn pawn_moves_empty() {
        let mut board = empty_board(Turn::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a3());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a2());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);

        board.to_play = Turn::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a5());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a7());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);
    }

    #[test]
    fn pawn_moves_blocked_ally() {
        let mut board = empty_board(Turn::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a3());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a2());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a3());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a2());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        board.to_play = Turn::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a5());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(&a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a7());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(&a6());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a7());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(&a5());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn pawn_moves_blocked_opponent() {
        let mut board = empty_board(Turn::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a3());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(&a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a2());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(&a3());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&a2());
        board.black_pieces[Piece::Knight as usize] = BitBoard::from(&a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);

        board.to_play = Turn::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a5());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a4());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a7());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a6());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 0);

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&a7());
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(&a5());
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 1);
    }

    #[test]
    fn pawn_captures() {
        let mut board = empty_board(Turn::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(&d3());
        board.black_pieces[Piece::Pawn as usize] = c4() | e4() | d4();
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);

        board.to_play = Turn::Black;

        moves.clear();
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(&d5());
        board.white_pieces[Piece::Pawn as usize] = c4() | e4() | d4();
        board.pawn_moves(&mut moves);
        assert_eq!(moves.len(), 2);
    }
}
