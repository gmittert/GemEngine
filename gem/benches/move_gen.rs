use bitboard::moves::AlgebraicMove;
use criterion::{black_box, criterion_group, Criterion};
use gem::board::Board;

pub fn knights(c: &mut Criterion) {
    c.bench_function("knights", |b| {
        let fen = "rnbqkbnr/pppppppp/N2N1N2/7N/1N2N3/3N2N1/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = vec![];
            board.knight_moves(&mut out);
            black_box(out);
        })
    });
    c.bench_function("knights_it", |b| {
        let fen = "rnbqkbnr/pppppppp/N2N1N2/7N/1N2N3/3N2N1/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let out: Vec<AlgebraicMove> = board.knight_moves_it().collect();
            black_box(out);
        })
    });
}

criterion_group!(iterative_compare, knights);
