mod evaluation;
mod move_gen;
mod perft;
mod shared_hashmap;

criterion::criterion_main!(
    perft::shallow_perfts,
    perft::deep_perfts,
    evaluation::evaluation,
    evaluation::node_efficiency,
    evaluation::node_throughput,
    shared_hashmap::shared_hashmap,
    move_gen::iterative_compare,
);
