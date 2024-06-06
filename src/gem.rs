use crate::{board::Board, uci::*};

pub struct Gem {
    pub board: Board
}

impl UciEngine for Gem {
    fn uci(&mut self)  -> Result<(), String> {
        id("gem", "Gwen Mittertreiner");
        // TODO: Implement options
        uci_ok();
        Ok(())
    }

    fn debug(&mut self, on: bool)  -> Result<(), String>{
        // TODO: Implement debug
        Ok(())
    }

    fn is_ready(&mut self)  -> Result<(), String>{
        ready_ok();
        Ok(())
    }

    fn set_option(&mut self, name: &str, value: Option<&str>)  -> Result<(), String>{
        // TODO setup options
        Ok(())
    }

    fn register(&mut self)  -> Result<(), String>{
        registration(RegistrationStatus::Ok);
        Ok(())
    }

    fn uci_new_game(&mut self)  -> Result<(), String>{
        self.board = crate::board::starting_board();
        Ok(())
    }

    fn position(&mut self, fen: &str, moves: Vec<crate::board::AlgebraicMove>)  -> Result<(), String>{
        self.board = Board::from_fen(fen).ok_or(format!("Failed to parse fen: {}", fen))?;
        Ok(())
    }

    fn go(&mut self, options: crate::uci::GoOptions)  -> Result<(), String>{
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
