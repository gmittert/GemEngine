use std::time::{Duration, Instant};

use bitboard::moves::{Color, Piece};
use bitboard::posn::{d4, e5};
use criterion::{BenchmarkId, Criterion, criterion_group};
use engine::{
    board::{self, Board},
    transposition_table::TranspositionTable,
};

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Nodes;

impl criterion::measurement::Measurement for Nodes {
    type Intermediate = usize;

    type Value = usize;

    fn start(&self) -> Self::Intermediate {
        0
    }

    fn end(&self, i: Self::Intermediate) -> Self::Value {
        i
    }

    fn add(&self, v1: &Self::Value, v2: &Self::Value) -> Self::Value {
        v1 + v2
    }

    fn zero(&self) -> Self::Value {
        0
    }

    fn to_f64(&self, value: &Self::Value) -> f64 {
        *value as f64
    }

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &NodeFormatter
    }
}

struct NodeFormatter;

impl criterion::measurement::ValueFormatter for NodeFormatter {
    fn scale_values(&self, typical_value: f64, values: &mut [f64]) -> &'static str {
        let (factor, unit) = if typical_value < 10f64.powi(3) {
            (10f64.powi(0), "nodes")
        } else if typical_value < 10f64.powi(6) {
            (10f64.powi(-3), "kilonodes")
        } else if typical_value < 10f64.powi(9) {
            (10f64.powi(-6), "meganodes")
        } else {
            (10f64.powi(-9), "giganodes")
        };

        for val in values {
            *val *= factor;
        }

        unit
    }

    fn scale_throughputs(
        &self,
        _typical_value: f64,
        _throughput: &criterion::Throughput,
        _values: &mut [f64],
    ) -> &'static str {
        todo!()
    }

    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "nodes"
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct NodeThroughput;

impl criterion::measurement::Measurement for NodeThroughput {
    type Intermediate = (usize, Duration);

    type Value = (usize, Duration);

    fn start(&self) -> Self::Intermediate {
        (0, Duration::from_secs(0))
    }

    fn end(&self, i: Self::Intermediate) -> Self::Value {
        i
    }

    fn add(&self, (v1a, v1b): &Self::Value, (v2a, v2b): &Self::Value) -> Self::Value {
        (v1a + v2a, *v1b + *v2b)
    }

    fn zero(&self) -> Self::Value {
        (0, Duration::from_secs(0))
    }

    fn to_f64(&self, (count, duration): &Self::Value) -> f64 {
        (*count as f64) / duration.as_secs_f64()
    }

    fn formatter(&self) -> &dyn criterion::measurement::ValueFormatter {
        &NodeThroughputFormatter
    }
}

struct NodeThroughputFormatter;

impl criterion::measurement::ValueFormatter for NodeThroughputFormatter {
    fn scale_values(&self, typical_value: f64, values: &mut [f64]) -> &'static str {
        let (factor, unit) = if typical_value < 10f64.powi(3) {
            (10f64.powi(0), "nodes/s")
        } else if typical_value < 10f64.powi(6) {
            (10f64.powi(-3), "kilonodes/s")
        } else if typical_value < 10f64.powi(9) {
            (10f64.powi(-6), "meganodes/s")
        } else {
            (10f64.powi(-9), "giganodes/s")
        };

        for val in values {
            *val *= factor;
        }

        unit
    }

    fn scale_throughputs(
        &self,
        _typical_value: f64,
        _throughput: &criterion::Throughput,
        _values: &mut [f64],
    ) -> &'static str {
        todo!()
    }

    fn scale_for_machines(&self, _values: &mut [f64]) -> &'static str {
        "nodes/s"
    }
}

pub fn eval_fn(c: &mut Criterion) {
    c.bench_function("eval_start", |b| {
        let board = board::starting_board();
        b.iter(|| {
            board.eval(
                board::evaluation::Evaluation::lost(0),
                board::evaluation::Evaluation::won(0),
                Color::White,
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
                let cache = TranspositionTable::<1024>::new();
                b.iter(|| {
                    board.best_move(4, num_cpus, &cache, None);
                })
            },
        );
    }
    group.finish()
}

pub fn london(c: &mut Criterion) {
    let mut group = c.benchmark_group("london");
    for depth in 0..10 {
        for num_cpus in [1, 2, 4, 8, 16, 32, 64].iter() {
            group.bench_with_input(
                BenchmarkId::from_parameter(format!("{}ply/{}cpu", depth, num_cpus)),
                num_cpus,
                |b, &num_cpus| {
                    let mut board = board::Board::from_fen(
                        "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
                    )
                    .expect("Invalid fen?");
                    b.iter(|| {
                        board.it_depth_best_move(depth, num_cpus);
                    })
                },
            );
        }
    }
    group.finish()
}

pub fn london_nodes(c: &mut Criterion<Nodes>) {
    let mut group = c.benchmark_group("london_nodes");
    for num_cpus in [1, 2, 4, 8, 16, 32, 64].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_cpus),
            num_cpus,
            |b, &num_cpus| {
                let mut board = board::Board::from_fen(
                    "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
                )
                .expect("Invalid fen?");
                b.iter_custom(|iters| {
                    let mut nodes = 0;
                    for _i in 0..iters {
                        let _ = board.it_depth_best_move(4, num_cpus);
                        nodes += board.nodes;
                    }
                    nodes
                })
            },
        );
    }
    group.finish()
}

pub fn london_node_throughput(c: &mut Criterion<NodeThroughput>) {
    let mut group = c.benchmark_group("london_node_throughput");
    for num_cpus in [1, 2, 4, 8, 16, 32, 64].iter() {
        group.bench_with_input(
            BenchmarkId::from_parameter(num_cpus),
            num_cpus,
            |b, &num_cpus| {
                let mut board = board::Board::from_fen(
                    "r1b1kb1r/pp5p/1qn1pp2/3p2pn/2pP4/1PP1PNB1/P1QN1PPP/R3KB1R b KQkq - 0 11",
                )
                .expect("Invalid fen?");
                b.iter_custom(|iters| {
                    let mut nodes = 0;
                    let begin = Instant::now();
                    for _i in 0..iters {
                        let _ = board.it_depth_best_move(4, num_cpus);
                        nodes += board.nodes;
                    }
                    (nodes, begin.elapsed())
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
            board.static_exchange_evaluation(e5(), Piece::Pawn, d4(), Piece::Pawn);
        })
    });
    c.bench_function("static_exchange_short", |b| {
        let fen = "rnbqkbnr/pppp1ppp/8/4p3/3P4/8/PPP1PPPP/RNBQKBNR w KQkq - 0 2";
        let mut board = Board::from_fen(fen).expect("bad fen?");
        b.iter(|| {
            board.static_exchange_evaluation(e5(), Piece::Pawn, d4(), Piece::Pawn);
        })
    });
}

criterion_group!(
    name = evaluation;
    config = Criterion::default();
    targets = start, london, eval_fn, static_exchange
);
criterion_group!(
    name = node_efficiency;
    config = Criterion::default().warm_up_time(Duration::from_nanos(1)).with_measurement(Nodes);
    targets = london_nodes
);
criterion_group!(
    name = node_throughput;
    config = Criterion::default().with_measurement(NodeThroughput);
    targets = london_node_throughput
);
