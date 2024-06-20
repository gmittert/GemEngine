pub mod bitboard;
pub mod evaluation;
mod move_generation;
mod moves;
mod posn;
pub use crate::board::bitboard::*;
pub use crate::board::moves::*;
pub use crate::board::posn::*;
use crate::zobrist::ZOBRIST_KEYS;
use std::fmt;

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
    pub move_rights: Vec<MoveRights>,
    pub hash: u64,
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
    }
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
        let piece = self
            .query_pos(m.from)
            .ok_or(format!("Failed to find a piece on position: {}", m.from))?;
        let capture = self.query_pos(m.to);
        let is_castle_king = piece == Piece::King
            && ((m.from == e1() && m.to == g1()) || (m.from == e8() && m.to == g8()));
        let is_castle_queen = piece == Piece::King
            && ((m.from == e1() && m.to == c1()) || (m.from == e8() && m.to == c8()));
        let ep_target = self.move_rights.last().and_then(|x| x.ep_target);
        let is_en_passant = if let Some(file) = ep_target {
            piece == Piece::Pawn && m.to.file() == file
        } else {
            false
        };
        let m = Move {
            from: m.from,
            to: m.to,
            piece,
            turn: self.to_play,
            capture,
            is_check: false,
            is_mate: false,
            is_castle_king,
            is_castle_queen,
            promotion: m.promotion,
            is_en_passant,
        };
        self.make_move(&m);
        Ok(())
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
        self.hash ^= ZOBRIST_KEYS.get_key(c, p, pos);
    }
    pub fn move_piece(&mut self, c: Color, p: Piece, from: Posn, to: Posn) {
        self.remove_piece(c, p, from);
        self.add_piece(c, p, to);
    }

    pub fn make_move(&mut self, m: &Move) {
        self.move_piece(m.turn, m.piece, m.from, m.to);
        if m.is_castle_king {
            let (from, to) = match m.turn {
                Color::Black => (h8(), f8()),
                Color::White => (h1(), f1()),
            };
            self.move_piece(m.turn, Piece::Rook, from, to)
        }
        if m.is_castle_queen {
            let (from, to) = match m.turn {
                Color::Black => (a8(), d8()),
                Color::White => (a1(), d1()),
            };
            self.move_piece(m.turn, Piece::Rook, from, to)
        }

        self.half_move += 1;
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
            match m.turn {
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
                !m.turn,
                piece,
                match (m.is_en_passant, m.turn) {
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
            self.add_piece(m.turn, piece, m.to);
            self.remove_piece(m.turn, Piece::Pawn, m.to);
        }
        let new_move_rights = MoveRights {
            castling_ability: CastlingAbility(inner),
            ep_target,
        };
        self.hash ^= move_rights.hash();
        self.hash ^= new_move_rights.hash();
        self.move_rights.push(new_move_rights);

        self.to_play = !self.to_play;
        self.hash ^= ZOBRIST_KEYS.black_turn;
    }

    pub fn undo_move(&mut self, m: &Move) {
        if let Some(piece) = m.promotion {
            self.remove_piece(m.turn, piece, m.to);
            self.add_piece(m.turn, Piece::Pawn, m.to);
        }
        self.move_piece(m.turn, m.piece, m.to, m.from);
        if let Some(piece) = m.capture {
            self.add_piece(
                !m.turn,
                piece,
                match (m.is_en_passant, m.turn) {
                    (true, Color::Black) => m.to.no(),
                    (true, Color::White) => m.to.so(),
                    _ => Some(m.to),
                }
                .unwrap(),
            );
        };
        if m.is_castle_king {
            let (to, from) = match m.turn {
                Color::Black => (h8(), f8()),
                Color::White => (h1(), f1()),
            };
            self.move_piece(m.turn, Piece::Rook, from, to)
        }
        if m.is_castle_queen {
            let (to, from) = match m.turn {
                Color::Black => (a8(), d8()),
                Color::White => (a1(), d1()),
            };
            self.move_piece(m.turn, Piece::Rook, from, to)
        }
        self.to_play = !self.to_play;
        self.hash ^= ZOBRIST_KEYS.black_turn;

        if let Some(prev_rights) = self.move_rights.pop() {
            self.hash ^= prev_rights.hash();
            if let Some(new_rights) = self.move_rights.last() {
                self.hash ^= new_rights.hash();
            }
        }
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
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut chars: [char; 64] = ['.'; 64];

        for i in 0..64 as usize {
            if self.black_pieces[Piece::King as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♔'
            } else if self.black_pieces[Piece::Queen as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♕'
            } else if self.black_pieces[Piece::Knight as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♘'
            } else if self.black_pieces[Piece::Pawn as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♙'
            } else if self.black_pieces[Piece::Bishop as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♗'
            } else if self.black_pieces[Piece::Rook as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♖'
            } else if self.white_pieces[Piece::King as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♚'
            } else if self.white_pieces[Piece::Queen as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♛'
            } else if self.white_pieces[Piece::Knight as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♞'
            } else if self.white_pieces[Piece::Pawn as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♟'
            } else if self.white_pieces[Piece::Bishop as usize].contains(Posn { pos: 1 << i }) {
                chars[i] = '♝'
            } else if self.white_pieces[Piece::Rook as usize].contains(Posn { pos: 1 << i }) {
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
            promotion: None,
        };
        board.make_move(&m);
        board.undo_move(&m);
        assert_eq!(board, board2);
    }
}
