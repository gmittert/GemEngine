use bitboard::{
    moves::AlgebraicMove,
    posn::{e2, e4},
};
use criterion::{Criterion, black_box, criterion_group};
use engine::{
    board::evaluation::Evaluation,
    transposition_table::SharedHashMap,
    transposition_table::{PackedTTEntry, ScoreType, TranspositionTable},
};

pub fn create(c: &mut Criterion) {
    c.bench_function("create_tt", |b| {
        b.iter(|| {
            let cache = TranspositionTable::new();
            black_box(cache);
        })
    });
    c.bench_function("clear_tt", |b| {
        let cache = TranspositionTable::new();
        b.iter(|| {
            cache.clear();
        })
    });
    c.bench_function("pack_entry", |b| {
        b.iter(|| {
            let entry = PackedTTEntry::new(
                Evaluation(124),
                4,
                Some(AlgebraicMove {
                    from: e2(),
                    to: e4(),
                    promotion: None,
                }),
                ScoreType::Exact,
            );
            black_box(entry);
        })
    });
    c.bench_function("fill_random", |b| {
        const N: usize = 64 * 1024;
        let cache = SharedHashMap::new();
        let mut state = 0u32;
        b.iter(|| {
            for i in 0..N {
                cache.insert(
                    state as u64,
                    PackedTTEntry {
                        eval: Evaluation(state as i16),
                        depth: 1u16 + (i / (4 * 1024)) as u16,
                        ..Default::default()
                    },
                );
                // From Knuth / HWLewis
                state = state.wrapping_mul(1_664_525).wrapping_add(1_013_904_223);
            }
        })
    });
    c.bench_function("fill_linear", |b| {
        const N: usize = 256 * 1024 * 1024 / 16;
        let cache = SharedHashMap::new();
        b.iter(|| {
            for i in 0..N {
                cache.insert(
                    i as u64,
                    PackedTTEntry {
                        eval: Evaluation(i as i16),
                        ..Default::default()
                    },
                );
            }
        })
    });
    c.bench_function("fill_spaced", |b| {
        const N: usize = 256 * 1024 * 1024 / 16;
        let cache = SharedHashMap::new();
        b.iter(|| {
            for i in 0..1024 {
                for j in 0..N / 1024 {
                    cache.insert(
                        (j * 1024 + i) as u64,
                        PackedTTEntry {
                            eval: Evaluation(j as i16 * 1024 + i as i16),
                            ..Default::default()
                        },
                    )
                }
            }
        })
    });
    c.bench_function("fill_spaced 4k", |b| {
        const N: usize = 256 * 1024 * 1024 / 16;
        let cache = SharedHashMap::new();
        let interval_size = 4096usize;
        b.iter(|| {
            for i in 0..interval_size {
                for j in 0..N / interval_size {
                    let pos = (j * interval_size + i) as u64;
                    cache.insert(
                        pos,
                        PackedTTEntry {
                            eval: Evaluation(pos as i16),
                            ..Default::default()
                        },
                    );
                }
            }
        })
    });
    c.bench_function("fill_spaced 2MiB", |b| {
        const N: usize = 256 * 1024 * 1024 / 16;
        let cache = SharedHashMap::new();
        let interval_size = 2 * 1024 * 1024;
        b.iter(|| {
            for i in 0..interval_size {
                for j in 0..N / interval_size {
                    let pos = (j * interval_size + i) as u64;
                    cache.insert(
                        pos,
                        PackedTTEntry {
                            eval: Evaluation(pos as i16),
                            ..Default::default()
                        },
                    );
                }
            }
        })
    });
}

criterion_group!(shared_hashmap, create);
