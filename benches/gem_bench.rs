mod evaluation;
mod perft;

criterion::criterion_main!(
    perft::shallow_perfts,
    perft::deep_perfts,
    evaluation::evaluation
);
