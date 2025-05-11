pub mod evaluation;
mod move_generation;
pub mod search;
mod sliding_attacks;
use crate::piece_square_tables::EG_TABLE;
use crate::piece_square_tables::GAME_PHASE_INC;
use crate::piece_square_tables::MG_TABLE;

use crate::parser::Parser;
use crate::pgn;
use crate::pgn::GameTermination;
use crate::zobrist::ZOBRIST_KEYS;
use bitboard::BitBoard;
use bitboard::moves::{AlgebraicMove, Color, Move, Piece};
use bitboard::posn::*;
use std::fmt;
use std::num::NonZero;

#[derive(Debug, PartialEq)]
// We only use 4 bits:
// 1 = White king side
// 2 = White queen side
// 4 = Black king side
// 8 = Black queen side
#[derive(Copy, Clone)]
pub struct CastlingAbility(u8);

impl Default for CastlingAbility {
    fn default() -> Self {
        CastlingAbility(0xff)
    }
}

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

    pub fn hash(self) -> u64 {
        let mut hash = 0;
        if self.can_castle_king(Color::White) {
            hash ^= ZOBRIST_KEYS.white_king_castle;
        }
        if self.can_castle_king(Color::Black) {
            hash ^= ZOBRIST_KEYS.black_king_castle;
        }
        if self.can_castle_queen(Color::White) {
            hash ^= ZOBRIST_KEYS.white_queen_castle;
        }
        if self.can_castle_queen(Color::Black) {
            hash ^= ZOBRIST_KEYS.black_queen_castle;
        }
        hash
    }
}

#[derive(Debug, PartialEq, Copy, Clone, Default)]
pub struct MoveRights {
    pub castling_ability: CastlingAbility,
    pub ep_target: Option<File>,
}

impl MoveRights {
    pub fn hash(self) -> u64 {
        let mut hash = self.castling_ability.hash();
        if let Some(file) = self.ep_target {
            hash ^= ZOBRIST_KEYS.en_passant_targets[file as usize];
        }
        hash
    }
}

#[derive(Debug, Clone)]
pub struct Board {
    pub black_pieces: [BitBoard; 6],
    pub white_pieces: [BitBoard; 6],

    pub to_play: Color,
    pub half_move: u16,
    pub full_move: u16,
    pub hash: u64,

    pub mg_piece_values: [i16; 2],
    pub eg_piece_values: [i16; 2],
    // A progression of the game (out of 24)
    pub game_phase: u8,

    pub last_irreversible: Vec<u16>,
    pub moves: Vec<(Posn, u64)>,
    pub move_rights: Vec<MoveRights>,
    pub killer_moves: [[Option<AlgebraicMove>; 2]; 16],

    // Metadata about the current search
    pub seldepth: u16,
    pub nodes: usize,
}

pub struct BoardState {
    pub black_pieces: [BitBoard; 6],
    pub white_pieces: [BitBoard; 6],

    pub to_play: Color,
    pub half_move: u16,
    pub full_move: u16,
    pub hash: u64,

    pub mg_piece_values: [i16; 2],
    pub eg_piece_values: [i16; 2],
    pub game_phase: u8,

    pub last_irreversible_len: usize,
    pub moves_len: usize,
    pub move_rights_len: usize,
}

impl PartialEq for Board {
    fn eq(&self, other: &Self) -> bool {
        self.black_pieces == other.black_pieces
            && self.white_pieces == other.white_pieces
            && self.to_play == other.to_play
            && self.half_move == other.half_move
            && self.full_move == other.full_move
            && self.move_rights == other.move_rights
            && self.hash == other.hash
            && self.game_phase == other.game_phase
            && self.mg_piece_values == other.mg_piece_values
            && self.eg_piece_values == other.eg_piece_values
    }
}

impl Board {
    #[inline(always)]
    pub fn piece(&self, color: Color, piece: Piece) -> BitBoard {
        match color {
            Color::Black => self.black_piece(piece),
            Color::White => self.white_piece(piece),
        }
    }

    #[inline(always)]
    pub fn black_piece(&self, piece: Piece) -> BitBoard {
        self.black_pieces[piece as usize]
    }

    #[inline(always)]
    pub fn white_piece(&self, piece: Piece) -> BitBoard {
        self.white_pieces[piece as usize]
    }

    pub fn from_pgn(s: &str) -> Option<Board> {
        let (pgn_game, _) = pgn::PgnGameParser::new().parse(s)?;
        if pgn_game.termination != GameTermination::NoResult {
            return None;
        }
        let mut out = starting_board();
        for pgn::Element {
            white: white_move,
            black: black_move,
            ..
        } in pgn_game.moves
        {
            out.make_san_move(&white_move.mov).ok()?;
            if let Some(black_move) = black_move {
                out.make_san_move(&black_move.mov).ok()?;
            }
        }
        Some(out)
    }
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
        if castling_ability.can_castle_king(Color::White) {
            out.hash ^= ZOBRIST_KEYS.white_king_castle;
        }
        if castling_ability.can_castle_queen(Color::White) {
            out.hash ^= ZOBRIST_KEYS.white_queen_castle;
        }
        if castling_ability.can_castle_king(Color::Black) {
            out.hash ^= ZOBRIST_KEYS.black_king_castle;
        }
        if castling_ability.can_castle_queen(Color::Black) {
            out.hash ^= ZOBRIST_KEYS.black_queen_castle;
        }
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
                Some(file)
            },
        };
        out.move_rights.push(move_rights);
        out.half_move = sections.next()?.parse::<u16>().ok()?;
        out.full_move = sections.next()?.parse::<u16>().ok()?;
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
                    'p' | 'n' | 'b' | 'r' | 'q' | 'k' => {
                        out.add_piece(
                            Color::Black,
                            Piece::from(c).unwrap(),
                            Posn::from(rank, files.next()?),
                        );
                    }
                    'P' | 'N' | 'B' | 'R' | 'Q' | 'K' => {
                        out.add_piece(
                            Color::White,
                            Piece::from(c).unwrap(),
                            Posn::from(rank, files.next()?),
                        );
                    }
                    '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                        for _ in 0..c.to_digit(10).unwrap() {
                            files.next()?;
                        }
                    }
                    _ => return None,
                };
            }
        }

        Some(out)
    }

    pub fn make_alg_move(&mut self, m: &AlgebraicMove) -> Result<(), String> {
        self.make_move(&self.from_algeabraic(m));
        Ok(())
    }

    pub fn make_san_move(&mut self, m: &str) -> Result<(), String> {
        if m == "O-O" {
            let mv = match self.to_play {
                Color::Black => Move {
                    from: e8(),
                    to: g8(),
                    piece: Piece::King,
                    capture: None,
                    promotion: None,
                    is_check: false,
                    is_mate: false,
                    is_en_passant: false,
                    is_castle_queen: false,
                    is_castle_king: true,
                },
                Color::White => Move {
                    from: e1(),
                    to: g1(),
                    piece: Piece::King,
                    capture: None,
                    promotion: None,
                    is_check: false,
                    is_mate: false,
                    is_en_passant: false,
                    is_castle_queen: false,
                    is_castle_king: true,
                },
            };
            self.make_move(&mv);
            return Ok(());
        } else if m == "O-O-O" {
            let mv = match self.to_play {
                Color::Black => Move {
                    from: e8(),
                    to: b8(),
                    piece: Piece::King,
                    capture: None,
                    promotion: None,
                    is_check: false,
                    is_mate: false,
                    is_en_passant: false,
                    is_castle_queen: true,
                    is_castle_king: false,
                },
                Color::White => Move {
                    from: e1(),
                    to: b1(),
                    piece: Piece::King,
                    capture: None,
                    promotion: None,
                    is_check: false,
                    is_mate: false,
                    is_en_passant: false,
                    is_castle_queen: true,
                    is_castle_king: false,
                },
            };
            self.make_move(&mv);
            return Ok(());
        }
        let mut stream = m.chars().rev();
        let mut curr = stream.next();
        let mut promotion = None;
        while let Some(unwrapped) = curr {
            match unwrapped {
                '+' | '#' => curr = stream.next(),
                'K' => promotion = Some(Piece::King),
                'Q' => promotion = Some(Piece::Queen),
                'R' => promotion = Some(Piece::Rook),
                'B' => promotion = Some(Piece::Bishop),
                'N' => promotion = Some(Piece::Knight),
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                    break;
                }
                _ => return Err("Invalid character in san move".to_string()),
            }
        }
        let rank = Rank::from(curr.ok_or("Failed to read target rank from move")?)
            .ok_or("Failed to parse rank")?;
        let file = File::from(
            stream
                .next()
                .ok_or("Failed to read target file from move")?,
        )
        .ok_or("Failed to parse file")?;
        let mut piece = Piece::Pawn;
        let mut disamb_file = None;
        let mut disamb_rank = None;

        while let Some(curr) = stream.next() {
            match curr {
                'K' => piece = Piece::King,
                'Q' => piece = Piece::Queen,
                'R' => piece = Piece::Rook,
                'B' => piece = Piece::Bishop,
                'N' => piece = Piece::Knight,
                'x' => (),
                '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                    disamb_rank =
                        Some(Rank::from(curr).ok_or("Failed to parse disambiguation rank")?)
                }
                'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {
                    disamb_file =
                        Some(File::from(curr).ok_or("Failed to parse disambiguation file")?)
                }
                _ => return Err("Invalid character in san move".to_string()),
            };
        }

        let to = Posn::from(rank, file);
        let mv = self
            .find_from(to, piece, promotion, disamb_file, disamb_rank)
            .ok_or("Failed to find from square")?;
        self.make_move(&mv);
        Ok(())
    }

    pub fn find_from(
        &self,
        to: Posn,
        p: Piece,
        promotion: Option<Piece>,
        disamb_file: Option<File>,
        disamb_rank: Option<Rank>,
    ) -> Option<Move> {
        let mut moves = Vec::new();
        match p {
            Piece::Pawn => self.pawn_moves(&mut moves),
            Piece::Rook => self.rook_moves(&mut moves),
            Piece::Knight => self.knight_moves(&mut moves),
            Piece::Bishop => self.bishop_moves(&mut moves),
            Piece::Queen => self.queen_moves(&mut moves),
            Piece::King => self.king_moves(&mut moves),
        }
        for m in moves {
            if let Some(needed_file) = disamb_file {
                if m.from.file() != needed_file {
                    continue;
                }
            }
            if let Some(needed_rank) = disamb_rank {
                if m.from.rank() != needed_rank {
                    continue;
                }
            }
            if m.to == to && m.promotion == promotion {
                return Some(self.from_algeabraic(&m));
            }
        }
        None
    }

    pub fn add_piece(&mut self, c: Color, p: Piece, pos: Posn) {
        match c {
            Color::Black => {
                self.black_pieces[p as usize] |= pos;
            }
            Color::White => {
                self.white_pieces[p as usize] |= pos;
            }
        };
        let eg_val = EG_TABLE[c as usize][p as usize][pos.idx() as usize];
        let mg_val = MG_TABLE[c as usize][p as usize][pos.idx() as usize];
        self.eg_piece_values[c as usize] += eg_val;
        self.mg_piece_values[c as usize] += mg_val;
        self.game_phase += GAME_PHASE_INC[p as usize];
        self.hash ^= ZOBRIST_KEYS.get_key(c, p, pos);
    }
    pub fn remove_piece(&mut self, c: Color, p: Piece, pos: Posn) {
        match c {
            Color::Black => {
                self.black_pieces[p as usize] &= !BitBoard::from(pos);
            }
            Color::White => {
                self.white_pieces[p as usize] &= !BitBoard::from(pos);
            }
        };
        let eg_val = EG_TABLE[c as usize][p as usize][pos.idx() as usize];
        let mg_val = MG_TABLE[c as usize][p as usize][pos.idx() as usize];
        self.eg_piece_values[c as usize] -= eg_val;
        self.mg_piece_values[c as usize] -= mg_val;
        self.game_phase -= GAME_PHASE_INC[p as usize];
        self.hash ^= ZOBRIST_KEYS.get_key(c, p, pos);
    }
    pub fn from_algeabraic(&self, m: &AlgebraicMove) -> Move {
        let piece = (self.query_pos(m.from, Color::White))
            .or(self.query_pos(m.from, Color::Black))
            .unwrap();
        let mut capture = self
            .query_pos(m.to, Color::White)
            .or(self.query_pos(m.to, Color::Black));
        let is_castle_king = piece == Piece::King
            && ((m.from == e1() && m.to == g1()) || (m.from == e8() && m.to == g8()));
        let is_castle_queen = piece == Piece::King
            && ((m.from == e1() && m.to == c1()) || (m.from == e8() && m.to == c8()));
        let ep_target = self.move_rights.last().and_then(|x| x.ep_target);
        let is_en_passant = if let Some(file) = ep_target {
            piece == Piece::Pawn
                && m.to.file() == file
                && m.from.file() != file
                && capture.is_none()
        } else {
            false
        };
        if is_en_passant {
            capture = Some(Piece::Pawn)
        }
        Move {
            from: m.from,
            to: m.to,
            piece,
            capture,
            is_check: false,
            is_mate: false,
            is_castle_king,
            is_castle_queen,
            promotion: m.promotion,
            is_en_passant,
        }
    }

    /// Killer moves may not always apply.
    pub fn check_killer(&self, m: &AlgebraicMove) -> bool {
        // We better have a piece on the from square.
        let Some(piece) = self.query_pos(m.from, self.to_play) else {
            return false;
        };
        match piece {
            Piece::Pawn => return self.pawn_moves_it().any(|x| x == *m),
            Piece::Rook => self.rook_moves_it().any(|x| x == *m),
            Piece::Knight => self.knight_moves_it().any(|x| x == *m),
            Piece::Bishop => self.bishop_moves_it().any(|x| x == *m),
            Piece::Queen => self.queen_moves_it().any(|x| x == *m),
            Piece::King => self.king_moves_it().any(|x| x == *m),
        }
    }

    pub fn move_piece(&mut self, c: Color, p: Piece, from: Posn, to: Posn) {
        self.remove_piece(c, p, from);
        self.add_piece(c, p, to);
    }

    pub fn save_state(&self) -> BoardState {
        BoardState {
            black_pieces: self.black_pieces,
            white_pieces: self.white_pieces,

            to_play: self.to_play,
            half_move: self.half_move,
            full_move: self.full_move,
            hash: self.hash,

            mg_piece_values: self.mg_piece_values,
            eg_piece_values: self.eg_piece_values,
            game_phase: self.game_phase,

            last_irreversible_len: self.last_irreversible.len(),
            moves_len: self.moves.len(),
            move_rights_len: self.move_rights.len(),
        }
    }

    pub fn restore_state(&mut self, state: BoardState) {
        self.black_pieces = state.black_pieces;
        self.white_pieces = state.white_pieces;
        self.to_play = state.to_play;
        self.half_move = state.half_move;
        self.full_move = state.full_move;
        self.hash = state.hash;
        self.mg_piece_values = state.mg_piece_values;
        self.eg_piece_values = state.eg_piece_values;
        self.game_phase = state.game_phase;

        self.last_irreversible.truncate(state.last_irreversible_len);
        self.moves.truncate(state.moves_len);
        self.move_rights.truncate(state.move_rights_len);
    }

    pub fn make_null_move(&mut self) {
        self.half_move += 1;

        if self.to_play == Color::Black {
            self.full_move += 1;
        }
        self.to_play = !self.to_play;
        self.hash ^= ZOBRIST_KEYS.black_turn;
    }

    pub fn undo_null_move(&mut self) {
        self.to_play = !self.to_play;
        self.hash ^= ZOBRIST_KEYS.black_turn;

        if self.to_play == Color::Black {
            self.full_move -= 1;
        }
        self.half_move -= 1;
    }

    pub fn make_move(&mut self, m: &Move) {
        self.half_move += 1;

        self.moves.push((m.to, self.hash));
        if m.is_castle_king || m.is_castle_queen || m.capture.is_some() || m.piece == Piece::Pawn {
            self.last_irreversible.push(self.half_move);
        }

        self.move_piece(self.to_play, m.piece, m.from, m.to);
        if m.is_castle_king {
            let (from, to) = match self.to_play {
                Color::Black => (h8(), f8()),
                Color::White => (h1(), f1()),
            };
            self.move_piece(self.to_play, Piece::Rook, from, to)
        }
        if m.is_castle_queen {
            let (from, to) = match self.to_play {
                Color::Black => (a8(), d8()),
                Color::White => (a1(), d1()),
            };
            self.move_piece(self.to_play, Piece::Rook, from, to)
        }

        let ep_target =
            if m.piece == Piece::Pawn && m.from.rank() == Rank::Two && m.to.rank() == Rank::Four {
                Some(m.from.file())
            } else if m.piece == Piece::Pawn
                && m.from.rank() == Rank::Seven
                && m.to.rank() == Rank::Five
            {
                Some(m.from.file())
            } else {
                None
            };
        let move_rights = *self.move_rights.last().unwrap_or(&MoveRights::default());
        let CastlingAbility(mut inner) = move_rights.castling_ability;
        if m.piece == Piece::King {
            match self.to_play {
                Color::Black => {
                    inner &= 0b0011;
                }
                Color::White => {
                    inner &= 0b1100;
                }
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
        if let Some(piece) = m.capture {
            self.remove_piece(
                !self.to_play,
                piece,
                match (m.is_en_passant, self.to_play) {
                    (true, Color::Black) => m.to.no(),
                    (true, Color::White) => m.to.so(),
                    _ => Some(m.to),
                }
                .unwrap(),
            );
            if piece == Piece::Rook {
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
        if let Some(piece) = m.promotion {
            self.add_piece(self.to_play, piece, m.to);
            self.remove_piece(self.to_play, Piece::Pawn, m.to);
        }
        let new_move_rights = MoveRights {
            castling_ability: CastlingAbility(inner),
            ep_target,
        };
        self.hash ^= move_rights.hash();
        self.hash ^= new_move_rights.hash();
        self.move_rights.push(new_move_rights);

        if self.to_play == Color::Black {
            self.full_move += 1;
        }
        self.to_play = !self.to_play;
        self.hash ^= ZOBRIST_KEYS.black_turn;
    }

    pub fn undo_move(&mut self, m: &Move) {
        self.to_play = !self.to_play;
        self.moves.pop();
        if m.is_castle_king || m.is_castle_queen || m.capture.is_some() || m.piece == Piece::Pawn {
            self.last_irreversible.pop();
        }

        if let Some(piece) = m.promotion {
            self.remove_piece(self.to_play, piece, m.to);
            self.add_piece(self.to_play, Piece::Pawn, m.to);
        }
        self.move_piece(self.to_play, m.piece, m.to, m.from);
        if let Some(piece) = m.capture {
            self.add_piece(
                !self.to_play,
                piece,
                match (m.is_en_passant, self.to_play) {
                    (true, Color::Black) => m.to.no(),
                    (true, Color::White) => m.to.so(),
                    _ => Some(m.to),
                }
                .unwrap(),
            );
        };
        if m.is_castle_king {
            let (to, from) = match self.to_play {
                Color::Black => (h8(), f8()),
                Color::White => (h1(), f1()),
            };
            self.move_piece(self.to_play, Piece::Rook, from, to)
        }
        if m.is_castle_queen {
            let (to, from) = match self.to_play {
                Color::Black => (a8(), d8()),
                Color::White => (a1(), d1()),
            };
            self.move_piece(self.to_play, Piece::Rook, from, to)
        }
        self.hash ^= ZOBRIST_KEYS.black_turn;

        if let Some(prev_rights) = self.move_rights.pop() {
            self.hash ^= prev_rights.hash();
            if let Some(new_rights) = self.move_rights.last() {
                self.hash ^= new_rights.hash();
            }
        }
        if self.to_play == Color::Black {
            self.full_move -= 1;
        }
        self.half_move -= 1;
    }

    pub fn query_pos(&self, p: Posn, color: Color) -> Option<Piece> {
        let pieces: [Piece; 6] = [
            Piece::Pawn,
            Piece::Rook,
            Piece::Knight,
            Piece::Bishop,
            Piece::Queen,
            Piece::King,
        ];
        for i in pieces {
            if self.piece(color, i).contains(p) {
                return Some(i);
            }
        }
        None
    }

    pub fn in_check(&self, color: Color) -> bool {
        let king_pos = self.piece(color, Piece::King);
        self.attacked_by_side(
            Posn {
                pos: unsafe { NonZero::new_unchecked(king_pos.0) },
            },
            !color,
        )
    }

    pub fn attacked_by_side(&self, pos: Posn, color: Color) -> bool {
        !self.knight_attacks_pos(pos, color).is_empty()
            || !self.rook_queen_attacks_pos(pos, color).is_empty()
            || !self.bishop_queen_attacks_pos(pos, color).is_empty()
            || !self.pawn_attacks_pos(pos, color).is_empty()
            || !self.king_attacks_pos(pos, color).is_empty()
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

    pub fn pieces(&self) -> BitBoard {
        let mut b = BitBoard::empty();
        for i in 0..6 {
            b |= self.black_pieces[i];
            b |= self.white_pieces[i];
        }
        b
    }

    pub fn reset_stats(&mut self) {
        self.seldepth = 0;
        self.nodes = 0;
    }
    pub fn get_stats(&mut self) -> (u16, usize) {
        (self.seldepth, self.nodes)
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars: [char; 64] = ['.'; 64];

        for i in 0..64 as usize {
            if self
                .black_piece(Piece::King)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♔'
            } else if self
                .black_piece(Piece::Queen)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♕'
            } else if self
                .black_piece(Piece::Knight)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♘'
            } else if self
                .black_piece(Piece::Pawn)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♙'
            } else if self
                .black_piece(Piece::Bishop)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♗'
            } else if self
                .black_piece(Piece::Rook)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♖'
            } else if self
                .white_piece(Piece::King)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♚'
            } else if self
                .white_piece(Piece::Queen)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♛'
            } else if self
                .white_piece(Piece::Knight)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♞'
            } else if self
                .white_piece(Piece::Pawn)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♟'
            } else if self
                .white_piece(Piece::Bishop)
                .contains(Posn::from_idx(i).unwrap())
            {
                chars[i] = '♝'
            } else if self
                .white_piece(Piece::Rook)
                .contains(Posn::from_idx(i).unwrap())
            {
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
        hash: ZOBRIST_KEYS.white_king_castle
            ^ ZOBRIST_KEYS.white_queen_castle
            ^ ZOBRIST_KEYS.black_king_castle
            ^ ZOBRIST_KEYS.black_queen_castle
            ^ if turn == Color::Black {
                ZOBRIST_KEYS.black_turn
            } else {
                0
            },
        last_irreversible: vec![],
        moves: vec![],
        mg_piece_values: [0, 0],
        eg_piece_values: [0, 0],
        game_phase: 0,
        killer_moves: [[None, None]; 16],
        nodes: 0,
        seldepth: 0,
    }
}

pub fn starting_board() -> Board {
    Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
        .expect("Failed to parse starting fen")
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
            piece: Piece::Pawn,
            capture: None,
            is_check: false,
            is_mate: false,
            is_en_passant: false,
            is_castle_king: false,
            is_castle_queen: false,
            promotion: None,
        };
        board.make_move(&m);
        board.undo_move(&m);
        assert_eq!(board, board2);
    }

    #[test]
    fn from_pgn() {
        let pgn = r###"        
[Event "?"]
[Site "?"]
[Date "2024.08.21"]
[Round "?"]
[White "gem"]
[Black "gem"]
[Result "1/2-1/2"]
[ECO "A00"]
[GameDuration "00:09:10"]
[GameEndTime "2024-08-21T22:35:21.457 PDT"]
[GameStartTime "2024-08-21T22:26:11.364 PDT"]
[Opening "Van't Kruijs Opening"]
[PlyCount "110"]
[TimeControl "6/move"]

1. e3 {+0.04/8 5.0s} d5 {-0.04/7 5.0s} 2. Nf3 {0.00/7 5.0s} Qd6 {-0.01/7 5.0s}
3. c4 {0.00/7 5.0s} dxc4 {-0.01/7 5.0s} 4. Qa4+ {+0.03/6 5.0s}
Nc6 {-0.02/7 5.0s} 5. Na3 {0.00/7 5.0s} a6 {-0.02/7 5.0s} 6. Nxc4 {+0.02/6 5.0s}
Qd7 {-0.03/7 5.0s} 7. Qb3 {+0.01/7 5.0s} Nf6 {-0.02/7 5.0s}
8. Nce5 {+0.02/7 5.0s} Nxe5 {-0.04/7 5.0s} 9. Nxe5 {+0.02/7 5.0s}
Qd5 {-0.05/7 5.0s} 10. d4 {+0.05/6 5.0s} e6 {-0.02/6 5.0s} 11. f3 {+0.04/6 5.0s}
b5 {-0.01/6 5.0s} 12. Kd2 {+0.03/6 5.0s} h5 {-0.02/7 5.0s} 13. h4 {+0.02/6 5.0s}
Bb7 {-0.02/7 5.0s} 14. Nd3 {+0.02/6 5.0s} Qd6 {-0.02/7 5.0s}
15. a4 {+0.02/6 5.0s} b4 {-0.03/7 5.0s} 16. Nc5 {+0.03/7 5.0s}
Bc8 {-0.04/7 5.0s} 17. a5 {+0.03/7 5.0s} Nd7 {-0.04/7 5.0s}
18. Ne4 {+0.04/7 5.0s} Qd5 {-0.04/7 5.0s} 19. Bc4 {+0.06/7 5.0s}
Qb7 {-0.06/7 5.0s} 20. Kd3 {+0.05/7 5.0s} Qc6 {-0.06/7 5.0s}
21. Bd2 {+0.05/7 5.0s} Rb8 {-0.06/7 5.0s} 22. Qa4 {+0.06/6 5.0s}
Qxa4 {-0.05/7 5.0s} 23. Rxa4 {+0.05/7 5.0s} Bb7 {-0.06/7 5.0s}
24. Raa1 {+0.05/7 5.0s} Rd8 {-0.06/7 5.0s} 25. Kc2 {+0.05/7 5.0s}
Rb8 {-0.06/7 5.0s} 26. Be1 {+0.06/7 5.0s} Rh6 {-0.07/7 5.0s}
27. Bg3 {+0.06/7 5.0s} Rc8 {-0.07/7 5.0s} 28. Rh3 {+0.05/7 5.0s}
f5 {-0.05/7 5.0s} 29. Ng5 {+0.05/7 5.0s} c5 {-0.05/7 5.0s}
30. Nxe6 {+0.04/7 5.0s} cxd4 {-0.03/7 5.0s} 31. Nxf8 {+0.04/7 5.0s}
Kxf8 {-0.09/7 5.0s} 32. Kb3 {+0.09/7 5.0s} dxe3 {-0.09/7 5.0s}
33. Rd1 {+0.11/7 5.0s} Ke7 {-0.11/7 5.0s} 34. Re1 {+1.03/7 5.0s}
Rf8 {-0.09/7 5.0s} 35. Rxe3+ {+1.09/7 5.0s} Kd8 {-1.11/8 5.0s}
36. Bf4 {+1.11/7 5.0s} Rg6 {-1.12/8 5.0s} 37. Bg5+ {+1.12/8 5.0s}
Kc7 {-1.11/8 5.0s} 38. Kxb4 {+1.11/7 5.0s} f4 {-1.12/7 5.0s}
39. Re1 {+1.13/7 5.0s} Kc6 {-1.14/7 5.0s} 40. Bd3 {+3.06/7 5.0s}
Rgf6 {-3.06/8 5.0s} 41. Be4+ {+3.06/7 5.0s} Kc7 {-3.11/8 5.0s}
42. Bxf6 {+3.11/7 5.0s} gxf6 {-3.10/8 5.0s} 43. Bxb7 {+3.09/8 5.0s}
Kxb7 {-3.06/8 5.0s} 44. Re7 {+3.07/8 5.0s} Kc7 {-3.08/8 5.0s}
45. Ka3 {+3.91/9 5.0s} Rg8 {-3.91/8 5.0s} 46. Rh2 {+3.10/8 5.0s}
Rb8 {-3.94/8 5.0s} 47. b4 {+3.96/8 5.0s} Rb5 {-3.95/8 5.0s}
48. Re6 {+3.98/8 5.0s} Re5 {-3.98/9 5.0s} 49. Rxa6 {+3.98/9 5.0s}
Re3+ {-3.98/8 5.0s} 50. Ka4 {+3.98/9 5.0s} Re2 {-4.00/9 5.0s}
51. Ra7+ {+3.98/9 5.0s} Kc8 {-4.00/9 5.0s} 52. Ra8+ {+3.98/9 5.0s}
Nb8 {-4.00/9 5.0s} 53. Ra7 {+4.00/8 5.0s} Nd7 {-4.00/9 5.0s}
54. Ra8+ {+3.98/9 5.0s} Nb8 {-4.00/9 5.0s} 55. Ra7 {+4.00/8 5.0s}
Nd7 {-4.00/9 5.0s, Draw by 3-fold repetition} *
    "###;
        let board = Board::from_pgn(pgn);
        assert!(board.is_some());
    }

    #[test]
    fn save_restore() {
        let mut board = starting_board();
        let saved = board.save_state();
        board
            .make_alg_move(&AlgebraicMove {
                from: e2(),
                to: e4(),
                promotion: None,
            })
            .unwrap();
        board
            .make_alg_move(&AlgebraicMove {
                from: e7(),
                to: e5(),
                promotion: None,
            })
            .unwrap();
        board
            .make_alg_move(&AlgebraicMove {
                from: g1(),
                to: f3(),
                promotion: None,
            })
            .unwrap();
        board
            .make_alg_move(&AlgebraicMove {
                from: g8(),
                to: f6(),
                promotion: None,
            })
            .unwrap();
        board
            .make_alg_move(&AlgebraicMove {
                from: f3(),
                to: e5(),
                promotion: None,
            })
            .unwrap();
        board
            .make_alg_move(&AlgebraicMove {
                from: b8(),
                to: c6(),
                promotion: None,
            })
            .unwrap();
        board
            .make_alg_move(&AlgebraicMove {
                from: d7(),
                to: c6(),
                promotion: None,
            })
            .unwrap();
        board.restore_state(saved);
        assert_eq!(board, starting_board());
    }
}
