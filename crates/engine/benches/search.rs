use bitboard::{
    moves::{Color, Move, Piece},
    posn::{e1, e2, e4, g1},
};
use criterion::{Criterion, criterion_group};
use engine::board;

pub fn move_piece(c: &mut Criterion) {
    c.bench_function("move_piece", |b| {
        let mut board = board::starting_board();
        b.iter(|| {
            board.move_piece(Color::White, Piece::Pawn, e2(), e4());
        })
    });
    c.bench_function("move_piece_castle", |b| {
        let mut board = board::Board::from_fen(
            "r1bqkb1r/1ppp1ppp/p1n2n2/4p3/B3P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 2 5",
        )
        .expect("bad fen?");
        let castles = Move {
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
        };
        b.iter(|| {
            board.make_move(&castles);
        })
    });
}

criterion_group!(search_benches, move_piece);
