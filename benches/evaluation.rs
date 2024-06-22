use criterion::{criterion_group, BenchmarkId, Criterion};
use gem::board;

pub fn start(c: &mut Criterion) {
    let mut group = c.benchmark_group("start");
    for num_cpus in [1, 2, 4, 8, 16, 32, 64].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_cpus),
            num_cpus,
            |b, &num_cpus| {
                let mut board = board::starting_board();
                let pool = threadpool::ThreadPool::new(num_cpus);
                b.iter(|| {
                    board.best_move(&pool);
                })
            },
        );
    }
    group.finish()
}

pub fn london(c: &mut Criterion) {
    let mut group = c.benchmark_group("london");
    for num_cpus in [1, 2, 4, 8, 16, 32, 64].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_cpus),
            num_cpus,
            |b, &num_cpus| {
                let mut board = board::Board::from_fen(
                    "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
                )
                .expect("Invalid fen?");
                let pool = threadpool::ThreadPool::new(num_cpus);
                b.iter(|| {
                    board.best_move(&pool);
                })
            },
        );
    }
    group.finish()
}

criterion_group!(evaluation, start, london);
