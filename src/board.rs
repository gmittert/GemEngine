mod bitboard;
mod moves;
mod posn;
pub use crate::board::bitboard::*;
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

#[derive(Debug, PartialEq)]
pub struct Board {
    black_pieces: [BitBoard; 6],
    white_pieces: [BitBoard; 6],

    to_play: Color,
    turn_count: u16,
}

impl Board {
    pub fn make_move(&mut self, m: &Move) {
        match m.turn {
            Color::Black => self.black_pieces[m.piece as usize].make_move(m),
            Color::White => self.white_pieces[m.piece as usize].make_move(m),
        }
        self.to_play = !self.to_play;
        self.turn_count += 1;
    }

    pub fn undo_move(&mut self, m: &Move) {
        match m.turn {
            Color::Black => self.black_pieces[m.piece as usize].undo_move(m),
            Color::White => self.white_pieces[m.piece as usize].undo_move(m),
        };
        match m.capture {
            Some(p) => match m.turn {
                Color::Black => self.white_pieces[p as usize] |= m.to,
                Color::White => self.black_pieces[p as usize] |= m.to,
            },
            None => (),
        };
        self.to_play = !self.to_play;
        self.turn_count -= 1;
    }

    pub fn current_board(&self) -> BitBoard {
        let mut b = BitBoard::empty();
        for i in 0..6 {
            b |= self.black_pieces[i];
            b |= self.white_pieces[i];
        }
        b
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
        let other_pieces = match color {
            Color::White => self.black_pieces,
            Color::Black => self.white_pieces,
        }[Piece::King as usize];
        let king_pos = match color {
            Color::White => self.white_pieces,
            Color::Black => self.black_pieces,
        }[Piece::King as usize];

        let pieces: [Piece; 6] = [
            Piece::Pawn,
            Piece::Rook,
            Piece::Knight,
            Piece::Bishop,
            Piece::Queen,
            Piece::King,
        ];
        todo!();
        true
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
                    });
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
        }
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
                    });
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
        }
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
                    });
                    if opponent_pieces.contains(pos) {
                        break;
                    }
                    slide = shift(pos);
                }
            }
        }
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
                            capture: None,
                            is_check: false,
                            is_mate: false,
                        })
                    });
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
                knight.nne(),
                knight.see(),
                knight.sse(),
                knight.ssw(),
                knight.sww(),
                knight.nww(),
                knight.nnw(),
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
                knight.nne(),
                knight.nee(),
                knight.see(),
                knight.sse(),
                knight.ssw(),
                knight.sww(),
                knight.nww(),
                knight.nnw(),
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
                    capture: None,
                    is_check: false,
                    is_mate: false,
                })
            })
        });
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
                        })
                    });
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
            BitBoard::from(d8()),
            BitBoard::from(e8()),
        ],

        white_pieces: [
            a2() | b2() | c2() | d2() | e2() | f2() | g2() | h2(),
            a1() | h1(),
            b1() | g1(),
            c1() | f1(),
            BitBoard::from(d1()),
            BitBoard::from(e1()),
        ],

        to_play: Color::White,
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
            turn: Color::White,
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
            let mut board = empty_board(Color::White);
            board.white_pieces[Piece::Rook as usize] = BitBoard::from(Posn{pos: i });
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
}
