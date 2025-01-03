use criterion::{black_box, criterion_group, Criterion};
use gem::{shared_hashmap::SharedHashMap, transposition_table::TranspositionTable};

pub fn create(c: &mut Criterion) {
    c.bench_function("create_tt", |b| {
        b.iter(|| {
            let cache: TranspositionTable = SharedHashMap::new();
            black_box(cache);
        })
    });
}

criterion_group!(shared_hashmap, create);
