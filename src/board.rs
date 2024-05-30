mod bitboard;
mod moves;
mod posn;
pub use crate::board::bitboard::*;
pub use crate::board::moves::*;
pub use crate::board::posn::*;
use std::fmt;

#[derive(Debug, PartialEq)]
// We only use 4 bits:
// 1 = White king side
// 2 = White queen side
// 4 = Black king side
// 8 = Black queen side
#[derive(Copy, Clone)]
pub struct CastlingAbility(u8);

impl CastlingAbility {
    pub fn from_fen(s: &str) -> Option<CastlingAbility> {
        if s == "-" {
            return Some(CastlingAbility(0));
        }
        let mut out: u8 = 0;
        for c in s.chars() {
            match c {
                'K' => out |= 1,
                'Q' => out |= 2,
                'k' => out |= 4,
                'q' => out |= 8,
                _ => return None,
            }
        }
        Some(CastlingAbility(out))
    }

    pub fn can_castle_king(&self, c: Color) -> bool {
        let CastlingAbility(inner) = &self;
        match c {
            Color::White => (inner & 0b0001) != 0,
            Color::Black => (inner & 0b0100) != 0,
        }
    }
    pub fn can_castle_queen(&self, c: Color) -> bool {
        let CastlingAbility(inner) = &self;
        match c {
            Color::White => (inner & 0b0010) != 0,
            Color::Black => (inner & 0b1000) != 0,
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub struct MoveRights {
    pub castling_ability: CastlingAbility,
    pub ep_target: Option<Posn>,
}

#[derive(Debug, PartialEq)]
pub struct Board {
    pub black_pieces: [BitBoard; 6],
    pub white_pieces: [BitBoard; 6],

    pub to_play: Color,
    pub half_move: u16,
    pub full_move: u16,
    pub move_rights: Vec<MoveRights>,
    pub move_list: Vec<Move>,
}

impl Board {
    pub fn from_fen(s: &str) -> Option<Board> {
        let mut sections = s.split(" ");
        let placement = sections.next()?;
        let to_move = sections.next()?;
        let mut out = empty_board(match to_move {
            "w" => Color::White,
            "b" => Color::Black,
            _ => return None,
        });
        let castling_ability = CastlingAbility::from_fen(sections.next()?)?;
        let ep_target = sections.next()?;
        let move_rights = MoveRights {
            castling_ability,
            ep_target: if ep_target == "-" {
                None
            } else {
                let mut ep_chars = ep_target.chars();
                let file = match ep_chars.next()? {
                    'a' => File::A,
                    'b' => File::B,
                    'c' => File::C,
                    'd' => File::D,
                    'e' => File::E,
                    'f' => File::F,
                    'g' => File::G,
                    'h' => File::H,
                    _ => return None,
                };
                let rank = match ep_chars.next()? {
                    '3' => Rank::Three,
                    '6' => Rank::Six,
                    _ => return None,
                };
                Some(Posn::from(rank, file))
            },
        };
        out.move_rights.push(move_rights);
        out.half_move = sections.next()?.parse::<u16>().ok()?;
        out.full_move = sections.next()?.parse::<u16>().ok()?;
        out.move_list = vec![];
        if sections.next() != None {
            return None;
        }

        let ranks = std::iter::zip(
            [
                Rank::Eight,
                Rank::Seven,
                Rank::Six,
                Rank::Five,
                Rank::Four,
                Rank::Three,
                Rank::Two,
                Rank::One,
            ],
            placement.split("/"),
        );
        for (rank, placement) in ranks {
            let mut files = [
                File::A,
                File::B,
                File::C,
                File::D,
                File::E,
                File::F,
                File::G,
                File::H,
            ]
            .into_iter();
            for c in placement.chars() {
                match c {
                    'p' => {
                        out.black_pieces[Piece::Pawn as usize] |= Posn::from(rank, files.next()?)
                    }
                    'n' => {
                        out.black_pieces[Piece::Knight as usize] |= Posn::from(rank, files.next()?)
                    }
                    'b' => {
                        out.black_pieces[Piece::Bishop as usize] |= Posn::from(rank, files.next()?)
                    }
                    'r' => {
                        out.black_pieces[Piece::Rook as usize] |= Posn::from(rank, files.next()?)
                    }
                    'q' => {
                        out.black_pieces[Piece::Queen as usize] |= Posn::from(rank, files.next()?)
                    }
                    'k' => {
                        out.black_pieces[Piece::King as usize] |= Posn::from(rank, files.next()?)
                    }
                    'P' => {
                        out.white_pieces[Piece::Pawn as usize] |= Posn::from(rank, files.next()?)
                    }
                    'N' => {
                        out.white_pieces[Piece::Knight as usize] |= Posn::from(rank, files.next()?)
                    }
                    'B' => {
                        out.white_pieces[Piece::Bishop as usize] |= Posn::from(rank, files.next()?)
                    }
                    'R' => {
                        out.white_pieces[Piece::Rook as usize] |= Posn::from(rank, files.next()?)
                    }
                    'Q' => {
                        out.white_pieces[Piece::Queen as usize] |= Posn::from(rank, files.next()?)
                    }
                    'K' => {
                        out.white_pieces[Piece::King as usize] |= Posn::from(rank, files.next()?)
                    }
                    '1' => {
                        for _ in 0..1 {
                            files.next()?;
                        }
                    }
                    '2' => {
                        for _ in 0..2 {
                            files.next()?;
                        }
                    }
                    '3' => {
                        for _ in 0..3 {
                            files.next()?;
                        }
                    }
                    '4' => {
                        for _ in 0..4 {
                            files.next()?;
                        }
                    }
                    '5' => {
                        for _ in 0..5 {
                            files.next()?;
                        }
                    }
                    '6' => {
                        for _ in 0..6 {
                            files.next()?;
                        }
                    }
                    '7' => {
                        for _ in 0..7 {
                            files.next()?;
                        }
                    }
                    '8' => {
                        for _ in 0..8 {
                            files.next()?;
                        }
                    }
                    _ => return None,
                };
            }
        }

        Some(out)
    }

    pub fn make_move(&mut self, m: &Move) {
        self.move_list.push(*m);
        match m.turn {
            Color::Black => {
                self.black_pieces[m.piece as usize] =
                    self.black_pieces[m.piece as usize].make_move(m);
                self.full_move += 1;
            }
            Color::White => {
                self.white_pieces[m.piece as usize] =
                    self.white_pieces[m.piece as usize].make_move(m)
            }
        };
        if m.is_castle_king {
            match m.turn {
                Color::Black => {
                    self.black_pieces[Piece::Rook as usize] =
                        self.black_pieces[Piece::Rook as usize].make_move(&Move {
                            from: h8(),
                            to: f8(),
                            turn: Color::Black,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
                Color::White => {
                    self.white_pieces[Piece::Rook as usize] =
                        self.white_pieces[Piece::Rook as usize].make_move(&Move {
                            from: h1(),
                            to: f1(),
                            turn: Color::White,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
            }
        }
        if m.is_castle_queen {
            match m.turn {
                Color::Black => {
                    self.black_pieces[Piece::Rook as usize] =
                        self.black_pieces[Piece::Rook as usize].make_move(&Move {
                            from: a8(),
                            to: d8(),
                            turn: Color::Black,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
                Color::White => {
                    self.white_pieces[Piece::Rook as usize] =
                        self.white_pieces[Piece::Rook as usize].make_move(&Move {
                            from: a1(),
                            to: d1(),
                            turn: Color::White,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
            }
        }
        self.half_move += 1;
        let ep_target =
            if m.piece == Piece::Pawn && m.from.rank() == Rank::Two && m.to.rank() == Rank::Four {
                Some(Posn::from(Rank::Three, m.from.file()))
            } else if m.piece == Piece::Pawn
                && m.from.rank() == Rank::Seven
                && m.to.rank() == Rank::Five
            {
                Some(Posn::from(Rank::Six, m.from.file()))
            } else {
                None
            };
        let CastlingAbility(mut inner) = self
            .move_rights
            .last()
            .map(|x| x.castling_ability)
            .unwrap_or(CastlingAbility(0xff));
        if m.piece == Piece::King {
            match m.turn {
                Color::Black => inner &= 0b0011,
                Color::White => inner &= 0b1100,
            }
        }
        if m.piece == Piece::Rook {
            if m.from == h1() {
                inner &= 0b1110
            } else if m.from == h8() {
                inner &= 0b1011
            } else if m.from == a1() {
                inner &= 0b1101
            } else if m.from == a8() {
                inner &= 0b0111
            }
        }
        match m.capture {
            Some(p) => {
                match m.turn {
                    Color::Black => self.white_pieces[p as usize] &= !BitBoard::from(m.to),
                    Color::White => self.black_pieces[p as usize] &= !BitBoard::from(m.to),
                };
                if p == Piece::Rook {
                    if m.to == h1() {
                        inner &= 0b1110
                    } else if m.to == h8() {
                        inner &= 0b1011
                    } else if m.to == a1() {
                        inner &= 0b1101
                    } else if m.to == a8() {
                        inner &= 0b0111
                    }
                }
            }
            None => (),
        };
        self.move_rights.push(MoveRights {
            castling_ability: CastlingAbility(inner),
            ep_target,
        });
        self.to_play = !self.to_play;
    }

    pub fn undo_move(&mut self, m: &Move) {
        match m.turn {
            Color::Black => {
                self.black_pieces[m.piece as usize] =
                    self.black_pieces[m.piece as usize].undo_move(m);
                self.full_move -= 1;
            }
            Color::White => {
                self.white_pieces[m.piece as usize] =
                    self.white_pieces[m.piece as usize].undo_move(m)
            }
        };
        match m.capture {
            Some(p) => match m.turn {
                Color::Black => {
                    if m.is_en_passant {
                        self.white_pieces[p as usize] |= m.to.no().unwrap();
                    } else {
                        self.white_pieces[p as usize] |= m.to
                    }
                }
                Color::White => {
                    if m.is_en_passant {
                        self.black_pieces[p as usize] |= m.to.so().unwrap();
                    } else {
                        self.black_pieces[p as usize] |= m.to
                    }
                }
            },
            None => (),
        };
        if m.is_castle_king {
            match m.turn {
                Color::Black => {
                    self.black_pieces[Piece::Rook as usize] =
                        self.black_pieces[Piece::Rook as usize].make_move(&Move {
                            from: f8(),
                            to: h8(),
                            turn: Color::Black,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
                Color::White => {
                    self.white_pieces[Piece::Rook as usize] =
                        self.white_pieces[Piece::Rook as usize].make_move(&Move {
                            from: f1(),
                            to: h1(),
                            turn: Color::White,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
            }
        }
        if m.is_castle_queen {
            match m.turn {
                Color::Black => {
                    self.black_pieces[Piece::Rook as usize] =
                        self.black_pieces[Piece::Rook as usize].make_move(&Move {
                            from: d8(),
                            to: a8(),
                            turn: Color::Black,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
                Color::White => {
                    self.white_pieces[Piece::Rook as usize] =
                        self.white_pieces[Piece::Rook as usize].make_move(&Move {
                            from: d1(),
                            to: a1(),
                            turn: Color::White,
                            piece: Piece::Rook,
                            capture: None,
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        });
                }
            }
        }
        self.to_play = !self.to_play;
        self.move_rights.pop();
        self.move_list.pop();
        self.half_move -= 1;
    }

    pub fn query_pos(&self, p: Posn) -> Option<Piece> {
        let pieces: [Piece; 6] = [
            Piece::Pawn,
            Piece::Rook,
            Piece::Knight,
            Piece::Bishop,
            Piece::Queen,
            Piece::King,
        ];
        for i in pieces {
            if (self.black_pieces[i as usize] | self.white_pieces[i as usize]) & BitBoard::from(p)
                != BitBoard::empty()
            {
                return Some(i);
            }
        }
        None
    }

    pub fn in_check(&self, color: Color) -> bool {
        let king_pos = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::King as usize];

        let attacked = self.queen_attacks(!color)
            | self.rook_attacks(!color)
            | self.bishop_attacks(!color)
            | self.knight_attacks(!color)
            | self.pawn_attacks(!color)
            | self.king_attacks(!color);

        king_pos & attacked != BitBoard::empty()
    }

    pub fn in_check_pos(&self, pos: Posn, color: Color) -> bool {
        let attacked = self.queen_attacks(!color)
            | self.rook_attacks(!color)
            | self.bishop_attacks(!color)
            | self.knight_attacks(!color)
            | self.pawn_attacks(!color)
            | self.king_attacks(!color);

        attacked.contains(pos)
    }

    pub fn white_pieces(&self) -> BitBoard {
        let mut b = BitBoard::empty();
        for i in 0..6 {
            b |= self.white_pieces[i];
        }
        b
    }

    pub fn black_pieces(&self) -> BitBoard {
        let mut b = BitBoard::empty();
        for i in 0..6 {
            b |= self.black_pieces[i];
        }
        b
    }

    pub fn queen_attacks(&self, color: Color) -> BitBoard {
        let queens = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Queen as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        let opponent_pieces = match color {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        queens.fold(BitBoard::empty(), |acc, i| {
            let mut ret = acc;
            for shift in [
                |p: Posn| p.no(),
                |p: Posn| p.so(),
                |p: Posn| p.ea(),
                |p: Posn| p.we(),
                |p: Posn| p.ne(),
                |p: Posn| p.se(),
                |p: Posn| p.nw(),
                |p: Posn| p.sw(),
            ] {
                let mut slide = shift(i);
                while let Some(pos) = slide {
                    if allied_pieces.contains(pos) {
                        break;
                    }
                    ret |= pos;
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
            ret
        })
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

        let opponent_pieces = match color {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        for i in queens {
            for shift in [
                |p: Posn| p.no(),
                |p: Posn| p.so(),
                |p: Posn| p.ea(),
                |p: Posn| p.we(),
                |p: Posn| p.nw(),
                |p: Posn| p.ne(),
                |p: Posn| p.sw(),
                |p: Posn| p.se(),
            ] {
                let mut slide = shift(i);
                while let Some(pos) = slide {
                    if allied_pieces.contains(pos) {
                        break;
                    }
                    out.push(Move {
                        from: i,
                        to: pos,
                        turn: color,
                        piece: Piece::Queen,
                        capture: self.query_pos(pos),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                    });
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
        }
    }

    pub fn rook_attacks(&self, color: Color) -> BitBoard {
        let rooks = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Rook as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        let opponent_pieces = match color {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        rooks.fold(BitBoard::empty(), |acc, i| {
            let mut ret = acc;
            for shift in [
                |p: Posn| p.no(),
                |p: Posn| p.so(),
                |p: Posn| p.ea(),
                |p: Posn| p.we(),
            ] {
                let mut slide = shift(i);
                while let Some(pos) = slide {
                    if allied_pieces.contains(pos) {
                        break;
                    }
                    ret |= pos;
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
            ret
        })
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

        let opponent_pieces = match color {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        for i in rooks {
            for shift in [
                |p: Posn| p.no(),
                |p: Posn| p.so(),
                |p: Posn| p.ea(),
                |p: Posn| p.we(),
            ] {
                let mut slide = shift(i);
                while let Some(pos) = slide {
                    if allied_pieces.contains(pos) {
                        break;
                    }
                    out.push(Move {
                        from: i,
                        to: pos,
                        turn: color,
                        piece: Piece::Rook,
                        capture: self.query_pos(pos),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                    });
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
        }
    }

    pub fn bishop_attacks(&self, color: Color) -> BitBoard {
        let bishops = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::Bishop as usize];

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

        let opponent_pieces = match color {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        bishops.fold(BitBoard::empty(), |acc, i| {
            let mut ret = acc;
            for shift in [
                |p: Posn| p.nw(),
                |p: Posn| p.ne(),
                |p: Posn| p.sw(),
                |p: Posn| p.se(),
            ] {
                let mut slide = shift(i);
                while let Some(pos) = slide {
                    if allied_pieces.contains(pos) {
                        break;
                    }
                    ret |= pos;
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
            ret
        })
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

        let opponent_pieces = match color {
            Color::Black => self.white_pieces(),
            Color::White => self.black_pieces(),
        };
        for i in bishops {
            for shift in [
                |p: Posn| p.nw(),
                |p: Posn| p.ne(),
                |p: Posn| p.sw(),
                |p: Posn| p.se(),
            ] {
                let mut slide = shift(i);
                while let Some(pos) = slide {
                    if allied_pieces.contains(pos) {
                        break;
                    }
                    out.push(Move {
                        from: i,
                        to: pos,
                        turn: color,
                        piece: Piece::Bishop,
                        capture: self.query_pos(pos),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
                    });
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
        }
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

        let allied_pieces = match color {
            Color::White => self.white_pieces(),
            Color::Black => self.black_pieces(),
        };

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
                            turn: color,
                            piece: Piece::King,
                            capture: self.query_pos(pos),
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
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
                    && !allied_pieces.contains(i.ea().unwrap())
                    && !allied_pieces.contains(i.ea().and_then(|x| x.ea()).unwrap())
                {
                    out.push(Move {
                        from: i,
                        to: i.ea().and_then(|x| x.ea()).unwrap(),
                        turn: color,
                        piece: Piece::King,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: true,
                        is_castle_queen: false,
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
                    && !allied_pieces.contains(i.we().unwrap())
                    && !allied_pieces.contains(i.we().and_then(|x| x.we()).unwrap())
                    && !allied_pieces.contains(i.we().and_then(|x| x.we()).and_then(|x| x.we()).unwrap())
                {
                    out.push(Move {
                        from: i,
                        to: i.we().and_then(|x| x.we()).unwrap(),
                        turn: color,
                        piece: Piece::King,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: true,
                    });
                }
            }
        }
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
                    turn: color,
                    piece: Piece::Knight,
                    capture: self.query_pos(p),
                    is_check: false,
                    is_mate: false,
                    is_en_passant: false,
                    is_castle_king: false,
                    is_castle_queen: false,
                })
            })
        });
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

        for i in pawns {
            let mpush_pos = match color {
                Color::White => i.no(),
                Color::Black => i.so(),
            };
            if let Some(push_pos) = mpush_pos {
                if !allied_pieces.contains(push_pos) && !opponent_pieces.contains(push_pos) {
                    out.push(Move {
                        from: i,
                        to: push_pos,
                        turn: color,
                        piece: Piece::Pawn,
                        capture: None,
                        is_check: false,
                        is_mate: false,
                        is_en_passant: false,
                        is_castle_king: false,
                        is_castle_queen: false,
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
                                    turn: color,
                                    piece: Piece::Pawn,
                                    capture: None,
                                    is_check: false,
                                    is_mate: false,
                                    is_en_passant: false,
                                    is_castle_king: false,
                                    is_castle_queen: false,
                                });
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
                        out.push(Move {
                            from: i,
                            to: pos,
                            turn: color,
                            piece: Piece::Pawn,
                            capture: self.query_pos(pos),
                            is_check: false,
                            is_mate: false,
                            is_en_passant: false,
                            is_castle_king: false,
                            is_castle_queen: false,
                        })
                    });
            }
            // En Passant
            if let Some(ep_target) = self.move_rights.last().and_then(|x| x.ep_target) {
                if (color == Color::White
                    && (i.nw() == Some(ep_target) || i.ne() == Some(ep_target)))
                    || (color == Color::Black
                        && (i.sw() == Some(ep_target) || i.se() == Some(ep_target)))
                {
                    out.push(Move {
                        from: i,
                        to: ep_target,
                        turn: color,
                        piece: Piece::Pawn,
                        capture: Some(Piece::Pawn),
                        is_check: false,
                        is_mate: false,
                        is_en_passant: true,
                        is_castle_king: false,
                        is_castle_queen: false,
                    })
                }
            }
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars: [char; 64] = ['.'; 64];

        for i in 0..64 as usize {
            if self.black_pieces[Piece::King as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♔'
            } else if self.black_pieces[Piece::Queen as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♕'
            } else if self.black_pieces[Piece::Knight as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♘'
            } else if self.black_pieces[Piece::Pawn as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♙'
            } else if self.black_pieces[Piece::Bishop as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♗'
            } else if self.black_pieces[Piece::Rook as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♖'
            } else if self.white_pieces[Piece::King as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♚'
            } else if self.white_pieces[Piece::Queen as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♛'
            } else if self.white_pieces[Piece::Knight as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♞'
            } else if self.white_pieces[Piece::Pawn as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♟'
            } else if self.white_pieces[Piece::Bishop as usize].contains(Posn { pos: i as u8 }) {
                chars[i] = '♝'
            } else if self.white_pieces[Piece::Rook as usize].contains(Posn { pos: i as u8 }) {
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

pub fn empty_board(turn: Color) -> Board {
    Board {
        black_pieces: [
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
        ],

        white_pieces: [
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
            BitBoard::empty(),
        ],

        to_play: turn,
        half_move: 0,
        full_move: 1,
        move_rights: vec![MoveRights {
            ep_target: None,
            castling_ability: CastlingAbility(0xff),
        }],
        move_list: vec![],
    }
}

pub fn starting_board() -> Board {
    Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        .expect("Failed to parse starting fen")
}

pub fn generate_pseudo_legal_moves(b: &Board) -> Vec<Move> {
    let mut moves = vec![];

    b.rook_moves(b.to_play, &mut moves);
    b.bishop_moves(b.to_play, &mut moves);
    b.queen_moves(b.to_play, &mut moves);
    b.knight_moves(b.to_play, &mut moves);
    b.king_moves(b.to_play, &mut moves);
    b.pawn_moves(b.to_play, &mut moves);
    moves
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
            turn: Color::White,
            piece: Piece::Pawn,
            capture: None,
            is_check: false,
            is_mate: false,
            is_en_passant: false,
            is_castle_king: false,
            is_castle_queen: false,
        };
        board.make_move(&m);
        board.undo_move(&m);
        assert_eq!(board, board2);
    }

    #[test]
    fn rook_moves_empty() {
        for i in 0..64 {
            let mut board = empty_board(Color::White);
            board.white_pieces[Piece::Rook as usize] = BitBoard::from(Posn { pos: i });
            let mut moves = vec![];
            board.rook_moves(Color::White, &mut moves);
            assert_eq!(moves.len(), 14);
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
    }

    #[test]
    fn bishop_moves_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Bishop as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.bishop_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 7);
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
    }

    #[test]
    fn queen_moves_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Queen as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.queen_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 21);

        board.white_pieces[Piece::Queen as usize] = BitBoard::from(d5());
        moves.clear();
        board.queen_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 27);
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

        board.white_pieces[Piece::King as usize] = BitBoard::from(d5());
        moves.clear();
        board.king_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 8);
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
    }

    #[test]
    fn knight_moves_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a1());
        let mut moves = vec![];
        board.knight_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 2);

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(d5());
        moves.clear();
        board.knight_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 8);

        board.white_pieces[Piece::Knight as usize] = BitBoard::from(a5());
        moves.clear();
        board.knight_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 4);
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
    }

    #[test]
    fn pawn_en_passants_white() {
        let mut board = empty_board(Color::White);
        let mut moves = vec![];

        board.white_pieces[Piece::Pawn as usize] = BitBoard::from(d5());
        board.black_pieces[Piece::Pawn as usize] = BitBoard::from(e5());
        board.move_rights.push(MoveRights {
            castling_ability: CastlingAbility(0xff),
            ep_target: Some(e6()),
        });
        board.pawn_moves(Color::White, &mut moves);
        assert_eq!(moves.len(), 2);
        assert_eq!(moves.last().unwrap().is_en_passant, true);
        assert_eq!(moves.last().unwrap().from, d5());
        assert_eq!(moves.last().unwrap().to, e6());
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
            ep_target: Some(e3()),
        });
        board.pawn_moves(Color::Black, &mut moves);
        assert_eq!(moves.len(), 2);
        assert_eq!(moves.last().unwrap().is_en_passant, true);
        assert_eq!(moves.last().unwrap().from, d4());
        assert_eq!(moves.last().unwrap().to, e3());
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
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K3 w Q - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
        }
    }
    #[test]
    fn castle_king_white_king() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/4K2R w K - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
        }
    }
    #[test]
    fn castle_king_black_queen() {
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 w KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::Black, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
        }
        {
            let board =
                Board::from_fen("r3k3/8/8/8/8/8/8/8 w q - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::Black, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_queen, true);
        }
    }
    #[test]
    fn castle_king_black_king() {
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b KQkq - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::Black, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
        }
        {
            let board =
                Board::from_fen("4k2r/8/8/8/8/8/8/8 b k - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::Black, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king).collect();
            assert_eq!(only_castles.len(), 1);
            assert_eq!(only_castles.last().unwrap().is_castle_king, true);
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
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/b7/8/8/8/8/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
        }
        {
            let board =
                Board::from_fen("8/8/7b/8/8/8/8/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
        {
            let board =
                Board::from_fen("8/8/8/7b/8/8/8/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
    }
    #[test]
    fn no_castle_while_check() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/4r3/R3K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
    }
    #[test]
    fn no_castle_through_pieces() {
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R2NKB1R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 0);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3KB1R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R3K1BR w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_queen);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R2NK2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/R1N1K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
        {
            let board =
                Board::from_fen("8/8/8/8/8/8/8/RN2K2R w KQkQ - 0 1").expect("Failed to parse fen");
            let mut moves = vec![];
            board.king_moves(Color::White, &mut moves);
            let only_castles: Vec<Move> = moves.into_iter().filter(|x| x.is_castle_king || x.is_castle_queen).collect();
            assert_eq!(only_castles.len(), 1);
            assert!(only_castles[0].is_castle_king);
        }
    }
}
