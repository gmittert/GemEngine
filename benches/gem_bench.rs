mod perft;
mod evaluation;

criterion::criterion_main!(perft::shallow_perfts, perft::deep_perfts, evaluation::evaluation);
