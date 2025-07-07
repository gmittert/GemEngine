// A script to check loading and evaluating a few positions from a trained network.
use engine::board::{Board, evaluation::Evaluation};

fn eval_fen(fen: &str) -> Evaluation {
    let mut board = Board::from_fen(fen).unwrap();
    board.eval(board.to_play)
}

fn main() {
    let mut trainer = nnue_train::get_trainer();
    trainer
        .optimiser
        .load_weights_from_file("checkpoints/1_simple-50/optimiser_state/weights.bin")
        .expect("Failed to load weights for trainer");

    let starting_fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
    println!("starting Eval: {}", 400.0 * trainer.eval(starting_fen));
    println!("starting Eval nn: {}", eval_fen(starting_fen));

    let london_fen = "r1bqk2r/pp3ppp/2n1pn2/2pp4/3P4/2P1PNP1/PP1N1PP1/R2QKB1R b KQkq - 0 8";
    println!("london Eval: {}", 400.0 * trainer.eval(london_fen));
    println!("london Eval nn: {}", eval_fen(london_fen));

    let black_blunder_fen = "r1b1k2r/pp3ppp/2n1pn2/2pp4/1P1P4/4PNP1/P2N1PP1/R2QKB1R b KQkq - 0 10";
    println!(
        "black blunder Eval: {}",
        400.0 * trainer.eval(black_blunder_fen)
    );
    println!("black blunder Eval nn: {}", eval_fen(black_blunder_fen));

    let white_blunder_fen = "r1b1k2r/pp3ppp/2n1pn2/2pp4/q2P4/2P1PNP1/PP1N1PP1/R3KB1R w KQkq - 0 10";
    println!(
        "white blunder Eval: {}",
        400.0 * trainer.eval(white_blunder_fen)
    );
    println!("white blunder Eval nn: {}", eval_fen(white_blunder_fen));

    let worse_fen = "r1b1k2r/pp3ppp/2n1pn2/3p4/1q1P4/4PNP1/P2N1PP1/R3KB1R w KQkq - 0 13";
    println!("worse Eval: {}", 400.0 * trainer.eval(worse_fen));
    println!("worse Eval nn: {}", eval_fen(worse_fen));

    let eg_fen = "7k/8/8/8/8/8/8/KQ6 w - - 0 1";
    println!("king_queen eval: {}", 400.0 * trainer.eval(eg_fen));
    println!("king_queen eval quantized: {}", eval_fen(eg_fen));

    let mate_fen = "6k1/3Q4/5K2/8/8/8/8/8 w - - 0 1";
    println!("m1 eval: {}", 400.0 * trainer.eval(mate_fen));
    println!("m1 eval quantized: {}", eval_fen(mate_fen));
}
