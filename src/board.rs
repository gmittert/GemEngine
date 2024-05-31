mod bitboard;
mod move_generation;
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

#[derive(Debug, Clone, PartialEq)]
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
                            promotion: None,
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
                            promotion: None,
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
                            promotion: None,
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
                            promotion: None,
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
                if m.is_en_passant {
                    match m.turn {
                        Color::Black => {
                            self.white_pieces[p as usize] &= !BitBoard::from(m.to.no().unwrap())
                        }
                        Color::White => {
                            self.black_pieces[p as usize] &= !BitBoard::from(m.to.so().unwrap())
                        }
                    }
                } else {
                    match m.turn {
                        Color::Black => self.white_pieces[p as usize] &= !BitBoard::from(m.to),
                        Color::White => self.black_pieces[p as usize] &= !BitBoard::from(m.to),
                    };
                }
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
        if let Some(piece) = m.promotion {
            match m.turn {
                Color::Black => {
                    self.black_pieces[Piece::Pawn as usize] &= !BitBoard::from(m.to);
                    self.black_pieces[piece as usize] |= BitBoard::from(m.to);
                }
                Color::White => {
                    self.white_pieces[Piece::Pawn as usize] &= !BitBoard::from(m.to);
                    self.white_pieces[piece as usize] |= BitBoard::from(m.to);
                }
            };
        }
        self.move_rights.push(MoveRights {
            castling_ability: CastlingAbility(inner),
            ep_target,
        });
        self.to_play = !self.to_play;
    }

    pub fn undo_move(&mut self, m: &Move) {
        if let Some(piece) = m.promotion {
            match m.turn {
                Color::Black => {
                    self.black_pieces[Piece::Pawn as usize] |= BitBoard::from(m.to);
                    self.black_pieces[piece as usize] &= !BitBoard::from(m.to);
                }
                Color::White => {
                    self.white_pieces[Piece::Pawn as usize] |= BitBoard::from(m.to);
                    self.white_pieces[piece as usize] &= !BitBoard::from(m.to);
                }
            };
        }
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
                            promotion: None,
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
                            promotion: None,
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
                            promotion: None,
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
                            promotion: None,
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
            promotion: None,
        };
        board.make_move(&m);
        board.undo_move(&m);
        assert_eq!(board, board2);
    }
}
