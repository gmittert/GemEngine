use bitboard::{
    moves::AlgebraicMove,
    posn::{e2, e4},
};
use criterion::{Criterion, black_box, criterion_group};
use engine::{
    board::evaluation::Evaluation, shared_hashmap::SharedHashMap, transposition_table::{PackedTTEntry, ScoreType, TranspositionTable}
};

pub fn create(c: &mut Criterion) {
    c.bench_function("create_tt", |b| {
        b.iter(|| {
            let cache = TranspositionTable::<{ 256 * 1024 * 1024 / 16 }>::new();
            black_box(cache);
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
    c.bench_function("fill_linear", |b| {
        const N: usize = 256 * 1024 * 1024 / 16 ;
        let cache = SharedHashMap::<u64, N>::new();
        b.iter(|| {
            for i in 0..N {
                cache.insert(i as u64, i as u64);
            }
        })
    });
}

criterion_group!(shared_hashmap, create);
