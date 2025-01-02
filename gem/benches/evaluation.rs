use std::sync::Arc;

use criterion::{criterion_group, BenchmarkId, Criterion};
use gem::{
    board::{self, e5, Board},
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

pub fn static_exchange(c: &mut Criterion) {
    c.bench_function("static_exchange", |b| {
        let fen = "r1b1r1k1/pp2q1pp/2nb1p2/2pppQ2/2NP1Bn1/2PB1N2/PP1KRPPP/4R3 w - - 2 15";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            board.static_exchange_evaluation(e5(), board::Color::White);
        })
    });
    c.bench_function("static_exchange_short", |b| {
        let fen = "rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 2";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            board.static_exchange_evaluation(e5(), board::Color::White);
        })
    });
}

criterion_group!(evaluation, start, london, eval_fn, static_exchange);
