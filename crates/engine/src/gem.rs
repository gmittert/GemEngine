use std::time::Duration;

use bitboard::moves::Color;

use crate::{
    board::{self, Board},
    uci::{self, *},
};

struct GemOptions {
    num_threads: usize,
}

const DEFAULT_THREADS: usize = 4;

impl GemOptions {
    fn default() -> GemOptions {
        GemOptions {
            num_threads: DEFAULT_THREADS,
        }
    }

    fn report() {
        uci::option(EngineOption {
            name: String::from("Threads"),
            ty: EngineOptionType::Spin,
            default: Some(format!("{DEFAULT_THREADS}")),
            min: Some(1),
            max: Some(1024),
        });
    }

    fn set_option(&mut self, name: &str, value: Option<&str>) -> Result<String, String> {
        match name {
            "NumThreads" => {
                if let Some(Ok(threads)) = value.map(|x| x.parse()) {
                    self.num_threads = threads;
                    Ok(String::from("NumThreads"))
                } else {
                    Err("Bad argument for NumThreads".to_string())
                }
            }
            _ => Err(format!("No such Option: {name}")),
        }
    }
}

pub struct Gem {
    board: Board,
    options: GemOptions,
}

impl Default for Gem {
    fn default() -> Self {
        Self::new()
    }
}

impl Gem {
    pub fn new() -> Gem {
        let _ = rayon::ThreadPoolBuilder::new()
            .num_threads(32)
            .build_global();
        Gem {
            board: board::starting_board(),
            options: GemOptions::default(),
        }
    }
}

impl UciEngine for Gem {
    fn uci(&mut self) -> Result<(), String> {
        id("gem", "Gwen Mittertreiner");

        GemOptions::report();
        uci_ok();
        Ok(())
    }

    fn debug(&mut self, on: bool) -> Result<(), String> {
        if on {
            use tracing_perfetto::PerfettoLayer;
            use tracing_subscriber::prelude::*;

            let layer = PerfettoLayer::new(std::sync::Mutex::new(std::io::BufWriter::new(
                std::fs::File::create("gem.pb").unwrap(),
            )))
            .with_debug_annotations(true)
            .with_filter_by_marker(|_| true);
            tracing_subscriber::registry().with(layer).init();
        }
        Ok(())
    }

    fn is_ready(&mut self) -> Result<(), String> {
        ready_ok();
        Ok(())
    }

    fn set_option(&mut self, name: &str, value: Option<&str>) -> Result<(), String> {
        self.options
            .set_option(name, value)
            .map(|name| match name.as_str() {
                "NumThreads" => (),
                _ => panic!("Bad option set"),
            })
    }

    fn register(&mut self) -> Result<(), String> {
        registration(RegistrationStatus::Ok);
        Ok(())
    }

    fn uci_new_game(&mut self) -> Result<(), String> {
        self.board = crate::board::starting_board();
        Ok(())
    }

    fn position(
        &mut self,
        fen: &str,
        moves: Vec<bitboard::moves::AlgebraicMove>,
    ) -> Result<(), String> {
        self.board = Board::from_fen(fen).ok_or(format!("Failed to parse fen: {fen}"))?;
        for m in &moves {
            self.board.make_alg_move(m)?;
        }
        Ok(())
    }

    fn go(&mut self, options: crate::uci::GoOptions) -> Result<(), String> {
        // Hueristic to how long we should search for:
        // We do something pretty simple: remaining time/20 + increment/2.

        let search_ms = if let Some(move_time) = options.move_time {
            move_time as u64
        } else if let Some(btime) = options.btime
            && self.board.to_play == Color::Black
        {
            let inc_ms = options.binc.unwrap_or(0);
            (btime / 20 + inc_ms / 2) as u64
        } else if let Some(wtime) = options.wtime
            && self.board.to_play == Color::White
        {
            let inc_ms = options.winc.unwrap_or(0);
            (wtime / 20 + inc_ms / 2) as u64
        } else {
            5000
        };

        let (m, eval, info) = self
            .board
            .search_best_move_for(Duration::from_millis(search_ms), self.options.num_threads);
        let Some(best_move) = m else {
            return Err(format!("Failed to find best move on board: {}", self.board));
        };
        let info = uci::Info {
            score: Some(Score {
                eval,
                is_upper_bound: true,
                is_lower_bound: false,
            }),
            depth: Some(info.depth.into()),
            time: info.time.as_millis().try_into().ok(),
            seldepth: Some(info.seldepth as usize),
            nodes_per_sec: Some(info.nodes_per_sec),
            nodes: Some(info.nodes),
            hash_full: Some(info.hash_full),
            ..Default::default()
        };
        uci::info(self.board.half_move, info);
        uci::best_move(best_move, None);
        Ok(())
    }

    fn stop(&mut self) -> Result<(), String> {
        Ok(())
    }

    fn ponder_hit(&mut self) -> Result<(), String> {
        Ok(())
    }

    fn quit(&mut self) -> Result<(), String> {
        Err("Quitting!".to_string())
    }
}

#[cfg(test)]
mod tests {
    use crate::{gem::Gem, uci};

    #[test]
    fn uci_integration_test() {
        let mut gem = Gem::new();
        let buffer = "position startpos moves d2d4 g8f6 b1c3 d7d5 c1f4 c8d7 g1f3 b8c6 e2e3 e7e6 a2a3 f6e4 c3e4 d5e4 f3g5 f8e7 g5e4 e8g8 f1c4 c6a5 c4d3 f7f5 e4g3 g7g5 f4e5 a5c6 e1g1 c6e5 d4e5 d8e8 c2c3 d7a4 d1e2 a8d8 d3c4 e8d7 e3e4 b7b5 c4a2 g8h8 b2b3 b5b4 b3a4 f5f4 g3h5 b4c3 e2g4 d7a4 h2h4 f4f3 g2f3 a4a3 a2e6 a3b2 h4g5 e7c5 a1b1 b2d2 g5g6 c5f2 f1f2 d2d1 g1h2 h7g6 g4g5 h8h7 f2g2 d1h1 h2h1 d8d1 b1d1 f8g8 h5f6 h7g7 g5g6 g7h8";
        assert!(uci::reader::read_uci_line(&buffer, &mut gem).is_ok());
        let buffer = "go infinite";
        assert!(uci::reader::read_uci_line(&buffer, &mut gem).is_ok());
    }
}
