use criterion::{Criterion, black_box, criterion_group};
use engine::board::Board;

pub fn knights(c: &mut Criterion) {
    c.bench_function("knights", |b| {
        let fen = "rnbqkbnr/pppppppp/N2N1N2/7N/1N2N3/3N2N1/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = Vec::with_capacity(16);
            board.knight_moves(&mut out);
            for m in out {
                black_box(m);
            }
        })
    });
    c.bench_function("knights_it", |b| {
        let fen = "rnbqkbnr/pppppppp/N2N1N2/7N/1N2N3/3N2N1/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            for m in board.knight_moves_it() {
                black_box(m);
            }
        })
    });
}

pub fn rooks(c: &mut Criterion) {
    c.bench_function("rooks", |b| {
        let fen = "rnbqkbnr/pppppppp/1R3R2/3R4/R6R/2R1R3/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = Vec::with_capacity(16);
            board.rook_moves(&mut out);
            for m in out {
                black_box(m);
            }
        })
    });
    c.bench_function("rooks_it", |b| {
        let fen = "rnbqkbnr/pppppppp/1R3R2/3R4/R6R/2R1R3/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            for m in board.rook_moves_it() {
                black_box(m);
            }
        })
    });
}

pub fn bishops(c: &mut Criterion) {
    c.bench_function("bishops", |b| {
        let fen = "rnbqkbnr/pppppppp/1B3B2/3B4/B6B/2B1B3/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = Vec::with_capacity(16);
            board.bishop_moves(&mut out);
            for m in out {
                black_box(m);
            }
        })
    });
    c.bench_function("bishops_it", |b| {
        let fen = "rnbqkbnr/pppppppp/1B3B2/3B4/B6B/2B1B3/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            for m in board.bishop_moves_it() {
                black_box(m);
            }
        })
    });
}

pub fn queens(c: &mut Criterion) {
    c.bench_function("queens", |b| {
        let fen = "rnbqkbnr/pppppppp/1Q3Q2/3Q4/Q6Q/2Q1Q3/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = Vec::with_capacity(16);
            board.queen_moves(&mut out);
            for m in out {
                black_box(m);
            }
        })
    });
    c.bench_function("queens_it", |b| {
        let fen = "rnbqkbnr/pppppppp/1Q3Q2/3Q4/Q6Q/2Q1Q3/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            for m in board.queen_moves_it() {
                black_box(m);
            }
        })
    });
}

pub fn pawns(c: &mut Criterion) {
    c.bench_function("pawns", |b| {
        let fen = "r1b1k1r1/p2pqp1p/1pn2n1b/P1pPp1p1/1P1QP2P/N1P2P1N/5KP1/R1B2BR1 w q - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = Vec::with_capacity(16);
            board.pawn_moves(&mut out);
            black_box(out);
        })
    });
    c.bench_function("pawns_it", |b| {
        let fen = "r1b1k1r1/p2pqp1p/1pn2n1b/P1pPp1p1/1P1QP2P/N1P2P1N/5KP1/R1B2BR1 w q - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let i = board.pawn_captures_it().chain(board.pawn_non_captures_it());
            for m in i {
                black_box(m);
            }
        })
    });
}

pub fn kings(c: &mut Criterion) {
    c.bench_function("kings", |b| {
        let fen = "r1b1k1r1/p2pqp1p/1pn2n1b/P1pPp1p1/1P1QP2P/N1P2P1N/5KP1/R1B2BR1 w q - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            let mut out = Vec::with_capacity(8);
            board.king_moves(&mut out);
            black_box(out);
        })
    });
    c.bench_function("kings_it", |b| {
        let fen = "r1b1k1r1/p2pqp1p/1pn2n1b/P1pPp1p1/1P1QP2P/N1P2P1N/5KP1/R1B2BR1 w q - 0 1";
        let board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            for m in board.king_moves_it() {
                black_box(m);
            }
        })
    });
}

criterion_group!(
    iterative_compare,
    knights,
    rooks,
    bishops,
    queens,
    pawns,
    kings
);
