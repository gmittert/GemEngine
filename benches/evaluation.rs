use std::sync::Arc;

use criterion::{criterion_group, BenchmarkId, Criterion};
use gem::{
    board::{self},
    shared_hashmap::SharedHashMap,
};

pub fn eval_fn(c: &mut Criterion) {
    c.bench_function("eval_start", |b| {
        let board = board::starting_board();
        b.iter(|| {
            board.eval(
                board::evaluation::Evaluation::lost(),
                board::evaluation::Evaluation::won(),
                board::Color::White,
            );
        })
    });
}

pub fn start(c: &mut Criterion) {
    let mut group = c.benchmark_group("start");
    for num_cpus in [1, 2, 4, 8, 16, 32, 64].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_cpus),
            num_cpus,
            |b, &num_cpus| {
                let mut board = board::starting_board();
                let pool = threadpool::ThreadPool::new(num_cpus);
                let cache: Arc<SharedHashMap<1024>> = Arc::new(SharedHashMap::new());
                b.iter(|| {
                    board.best_move(4, &pool, cache.clone(), None);
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
                let pool = threadpool::ThreadPool::new(num_cpus);
                let mut board = board::Board::from_fen(
                    "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
                )
                .expect("Invalid fen?");
                b.iter(|| {
                    board.it_depth_best_move(4, &pool);
                })
            },
        );
    }
    group.finish()
}

criterion_group!(evaluation, start, london, eval_fn);
