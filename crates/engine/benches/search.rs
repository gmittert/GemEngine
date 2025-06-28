use bitboard::{
    moves::{Color, Piece},
    posn::{e2, e4},
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
}

criterion_group!(search_benches, move_piece);
