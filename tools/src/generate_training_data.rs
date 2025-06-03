use clap::Parser;
use std::fs::File;
use std::io::BufReader;
use std::io::BufRead;
use std::io::BufWriter;
use std::io::Write;
use gem::board::Board;

use serde::{Deserialize, Serialize};

/// A script to parse and transform input training data formatted like:
/// ```
/// {
///   "fen":          // the position FEN only contains pieces, active color, castling rights, and en passant square.
///   "evals": [      // a list of evaluations, ordered by number of PVs.
///       "knodes":   // number of kilo-nodes searched by the engine
///       "depth":    // depth reached by the engine
///       "pvs": [    // list of principal variations
///         "cp":     // centipawn evaluation. Omitted if mate is certain.
///         "mate":   // mate evaluation. Omitted if mate is not certain.
///         "line":   // principal variation, in UCI format.
/// }
/// ```
///
/// Such as the data found on https://database.lichess.org/#evals
#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Input .jsonl file
    #[arg(short, long)]
    input: String,

    /// Output training data
    #[arg(short, long, default_value_t = String::from("data.bin"))]
    output: String,
}

#[derive(Serialize, Deserialize)]
struct Pv{
    cp: Option<i16>,
    mate: Option<i16>,
    line: String,
}

#[derive(Serialize, Deserialize)]
struct Eval {
    pvs: Vec<Pv>,
    knodes: usize,
    depth: usize,
}

#[derive(Serialize, Deserialize)]
struct Sample {
    fen: String,
    evals: Vec<Eval>
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    let f = File::open(args.input)?;
    let mut reader = BufReader::new(f);

    let output = File::create(&args.output)?;
    let mut writer = BufWriter::new(output);

    let total_lines = 232637106;
    let mut curr_line = 0usize;

    let mut line = String::new();
    eprintln!("");
    while let Ok(_len) = reader.read_line(&mut line) {
        let s: Sample = serde_json::from_str(&line)?;
        line.clear();
        let Some(board) = Board::from_fen(&s.fen) else {
            println!("Failed to parse fen: {}", s.fen);
            continue;
        };
        curr_line += 1;
        if curr_line % (total_lines/100) == 0 {
            eprint!("\rProgress: {}%", 100*curr_line/total_lines)
        }
        let mut max_depth = 0;
        for eval in &s.evals {
            max_depth = max_depth.max(eval.depth);
        }
        for eval in s.evals.iter() {
            if eval.depth == max_depth {
                let cp_eval = if let Some(cp) = eval.pvs[0].cp {
                    cp
                } else if let Some(mate) = eval.pvs[0].mate {
                    if mate > 0 {
                        std::i16::MAX
                    } else {
                        std::i16::MIN + 1
                    }
                } else {
                    panic!("Expected one of either mate or cp");
                } as u64;
                let features = gem::nn_features::FeatureSet::from(&board);
                writer.write(features.as_bytes())?;
                writer.write(cp_eval.to_ne_bytes().as_slice())?;
                break;
            }
        }
    }

    Ok(())
}
