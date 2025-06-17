// A script to check loading and evaluating a few positions from a trained network.
//
// A network based off the bullet' simple example
use bitboard::moves::{Color, Piece};
use gem::board::Board;
use nnue::{HIDDEN_SIZE, QA, QB, SCALE};

static NNUE: Network = unsafe {
    std::mem::transmute(*include_bytes!(
        "../../checkpoints/1_simple-50/quantised.bin"
    ))
};

/// Accumulator code cribbed from the bullet examples in
/// https://github.com/jw1912/bullet/blob/main/examples/simple.rs
#[derive(Clone, Copy)]
#[repr(C, align(64))]
pub struct Accumulator {
    vals: [i16; HIDDEN_SIZE],
}

impl Accumulator {
    /// Initialised with bias so we can just efficiently
    /// operate on it afterwards.
    pub fn new(net: &Network) -> Self {
        net.feature_bias
    }

    /// Add a feature to an accumulator.
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i += *d
        }
    }

    /// Remove a feature from an accumulator.
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i -= *d
        }
    }
}

/// A bullet quantized network
#[repr(C)]
pub struct Network {
    /// Column-Major `HIDDEN_SIZE x 768` matrix.
    feature_weights: [Accumulator; 768],
    /// Vector with dimension `HIDDEN_SIZE`.
    feature_bias: Accumulator,
    /// Column-Major `1 x (2 * HIDDEN_SIZE)` matrix, we use it like this to make the code nicer in
    /// `Network::evaluate`.
    output_weights: [i16; 2 * HIDDEN_SIZE],
    /// Scalar output bias.
    output_bias: i16,
}

#[inline]
/// Clipped ReLU - Activation Function.
/// Note that this takes the i16s in the accumulator to i32s.
fn crelu(x: i16) -> i32 {
    i32::from(x).clamp(0, i32::from(QA))
}

impl Network {
    /// Calculates the output of the network, starting from the already calculated hidden layer.
    pub fn evaluate(&self, us: &Accumulator, them: &Accumulator) -> i32 {
        // Initialise output with bias.
        let mut output = i32::from(self.output_bias);

        // Side-To-Move Accumulator -> Output.
        for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HIDDEN_SIZE]) {
            output += crelu(input) * i32::from(weight);
        }

        // Not-Side-To-Move Accumulator -> Output.
        for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HIDDEN_SIZE..]) {
            output += crelu(input) * i32::from(weight);
        }

        // Apply eval scale.
        output *= SCALE;

        // Remove quantisation.
        output /= i32::from(QA) * i32::from(QB);

        output
    }
}

fn fill_accumulator(stm: &mut Accumulator, nstm: &mut Accumulator, board: &Board) {
    let to_move = board.to_play;
    for color in [Color::White, Color::Black] {
        for piece in [
            Piece::Pawn,
            Piece::Knight,
            Piece::Bishop,
            Piece::Rook,
            Piece::Queen,
            Piece::King,
        ] {
            let squares = board.piece(color, piece);
            for square in squares {
                let c = usize::from(color != to_move);
                let pc = 64 * usize::from(piece as usize);

                // Bulletformat/chessboard considers a1 to be 0, Gem considers h1 to be 0;
                let mut sq = square.idx() as usize ^ 7;

                // Bulletformat/chessboard, which the net uses, uses side-to-move relative
                // bitboards.
                if to_move == Color::Black {
                    sq ^= 56;
                }

                let stm_feature = [0, 384][c] + pc + sq;
                let nstm_feature = [384, 0][c] + pc + (sq ^ 56);
                stm.add_feature(stm_feature, &NNUE);
                nstm.add_feature(nstm_feature, &NNUE);
            }
        }
    }
}

fn eval_fen(fen: &str) -> i32 {
    let mut stm = Accumulator::new(&NNUE);
    let mut nstm = Accumulator::new(&NNUE);

    let board = Board::from_fen(&fen).unwrap();
    fill_accumulator(&mut stm, &mut nstm, &board);
    NNUE.evaluate(&stm, &nstm)
}

fn main() {
    let mut trainer = nnue::get_trainer();
    trainer.load_weights_from_file("checkpoints/1_simple-50/optimiser_state/weights.bin");

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
}
