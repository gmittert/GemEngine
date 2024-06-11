use crate::{
    board::Board,
    uci::{self, *},
};

pub struct Gem {
    pub board: Board,
}

impl UciEngine for Gem {
    fn uci(&mut self) -> Result<(), String> {
        id("gem", "Gwen Mittertreiner");
        // TODO: Implement options
        uci_ok();
        Ok(())
    }

    fn debug(&mut self, on: bool) -> Result<(), String> {
        // TODO: Implement debug
        Ok(())
    }

    fn is_ready(&mut self) -> Result<(), String> {
        ready_ok();
        Ok(())
    }

    fn set_option(&mut self, name: &str, value: Option<&str>) -> Result<(), String> {
        // TODO setup options
        Ok(())
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

    fn go(&mut self, options: crate::uci::GoOptions) -> Result<(), String> {
        let (m, eval) = self.board.best_move();
        let Some(best_move) = m else {
            return Err(format!("Failed to find best move on board: {}", self.board));
        };
        let info = uci::Info {
            score: Some(Score {
                eval,
                is_upper_bound: false,
                is_lower_bound: false,
            }),
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
