use std::time::Duration;

use crate::{
    board::{self, Board},
    uci::{self, *},
};

struct GemOptions {
    num_threads: usize,
}

const DEFAULT_THREADS: usize = 64;

impl GemOptions {
    fn default() -> GemOptions {
        GemOptions {
            num_threads: DEFAULT_THREADS,
        }
    }

    fn report() {
        uci::option(EngineOption {
            name: String::from("NumThreads"),
            ty: EngineOptionType::Spin,
            default: Some(String::from("64")),
            min: Some(1),
            max: Some(512),
        });
    }

    fn set_option(&mut self, name: &str, value: Option<&str>) -> Result<String, String> {
        match name {
            "NumThreads" => {
                if let Some(Ok(threads)) = value.map(|x| x.parse()) {
                    self.num_threads = threads;
                    Ok(String::from("NumThreads"))
                } else {
                    Err(format!("Bad argument for NumThreads"))
                }
            }
            _ => Err(format!("No such Option: {name}")),
        }
    }
}

pub struct Gem {
    board: Board,
    work_queue: threadpool::ThreadPool,
    options: GemOptions,
}

impl Gem {
    pub fn new() -> Gem {
        Gem {
            board: board::starting_board(),
            work_queue: threadpool::ThreadPool::new(DEFAULT_THREADS),
            options: GemOptions::default(),
        }
    }
}

impl UciEngine for Gem {
    fn uci(&mut self) -> Result<(), String> {
        id("gem", "Gwen Mittertreiner");
        // TODO: Implement options
        GemOptions::report();
        uci_ok();
        Ok(())
    }

    fn debug(&mut self, _on: bool) -> Result<(), String> {
        // TODO: Implement debug
        Ok(())
    }

    fn is_ready(&mut self) -> Result<(), String> {
        ready_ok();
        Ok(())
    }

    fn set_option(&mut self, name: &str, value: Option<&str>) -> Result<(), String> {
        self.options
            .set_option(name, value)
            .and_then(|name| match name.as_str() {
                "NumThreads" => {
                    self.work_queue.set_num_threads(self.options.num_threads);
                    Ok(())
                }
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
        moves: Vec<crate::board::AlgebraicMove>,
    ) -> Result<(), String> {
        self.board = Board::from_fen(fen).ok_or(format!("Failed to parse fen: {}", fen))?;
        for m in &moves {
            self.board.make_alg_move(m)?;
        }
        Ok(())
    }

    fn go(&mut self, _options: crate::uci::GoOptions) -> Result<(), String> {
        let (m, eval, info) = self
            .board
            .search_best_move_for(Duration::from_secs(5), &self.work_queue);
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
            ..Default::default()
        };
        uci::info(info);
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
        Err(format!("Quitting!"))
    }
}
