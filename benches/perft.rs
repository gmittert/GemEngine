use criterion::{criterion_group, Criterion};
use gem::board;
use gem::perft;

pub fn shallow_perft(c: &mut Criterion) {
    c.bench_function("perft3", |b| {
        let mut board = board::starting_board();
        b.iter(|| {
            perft::perft(&mut board, 3);
        })
    });
    c.bench_function("perft4", |b| {
        let mut board = board::starting_board();
        b.iter(|| {
            perft::perft(&mut board, 4);
        })
    });
}

pub fn deep_perft(c: &mut Criterion) {
    c.bench_function("perft5", |b| {
        let mut board = board::starting_board();
        b.iter(|| {
            perft::perft(&mut board, 5);
        })
    });
}

criterion_group!(shallow_perfts, shallow_perft);

criterion_group! {
    name=deep_perfts;
    config=Criterion::default().measurement_time(core::time::Duration::from_secs(75));
    targets=deep_perft
}
