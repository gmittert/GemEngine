use crate::{board::AlgebraicMove, uci::UciEngine};

use super::GoOptions;

fn read_uci_line(line: String, dispatch: &dyn UciEngine) -> Result<(), String> {
    let mut words = line.split_whitespace();
    let Some(command) = words.next() else {
        return Err(format!("Invalid uci command: {line}"));
    };
    match command {
        "uci" => Ok(dispatch.uci()),
        "debug" => {
            let Some(mode) = words.next() else {
                return Err(format!("Missing debug mode in: {line}"));
            };
            match mode {
                "on" => Ok(dispatch.debug(true)),
                "off" => Ok(dispatch.debug(true)),
                _ => Err(format!("Invalid debug mode in: {line}")),
            }
        }
        "isready" => Ok(dispatch.is_ready()),
        "setoption" => {
            let Some(name_literal) = words.next() else {
                return Err(format!("Invalid setoption line: {line}"));
            };
            if name_literal != "name" {
                return Err(format!("Missing 'name' parameter in: {line}"));
            }
            let Some(option_name) = words.next() else {
                return Err(format!("Missing 'name' value in : {line}"));
            };
            let Some(option_literal) = words.next() else {
                return Ok(dispatch.set_option(option_name, None));
            };
            if option_literal != "option" {
                return Err(format!("Invalid 'option' parameter in: {line}"));
            }
            let Some(option) = words.next() else {
                return Err(format!("Missing 'option' value in : {line}"));
            };
            Ok(dispatch.set_option(option_name, Some(option)))
        }
        "register" => {
            // todo, actually implement the parameters to this
            Ok(dispatch.register())
        }
        "ucinewgame" => Ok(dispatch.uci_new_game()),
        "position" => {
            let Some(position_type) = words.next() else {
                return Err(format!("Missing position arguments: {line}"));
            };

            let fen = match position_type {
                "fen" => {
                    let Some(fen) = words.next() else {
                        return Err(format!("Missing position arguments: {line}"));
                    };
                    fen
                }
                "startpos" => "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
                _ => return Err(format!("Invalid position type argument in: {line}")),
            };
            let Some(moves_literal) = words.next() else {
                return Err(format!("Missing position moves: {line}"));
            };
            if moves_literal != "moves" {
                return Err(format!("Missing position moves: {line}"));
            };
            let mut moves = vec![];
            while let Some(m) = words.next() {
                let parsed = AlgebraicMove::from(m).ok_or(format!("Invalid move: {}", m))?;
                moves.push(parsed);
            }
            Ok(dispatch.position(fen, moves))
        }
        "go" => {
            let mut options = GoOptions {
                ..Default::default()
            };
            while let Some(option) = words.next() {
                match option {
                    "searchmoves" => {
                        let mut moves = vec![];
                        while let Some(m) = words.next() {
                            let parsed = AlgebraicMove::from(m).ok_or(format!("Invalid move: {}", m))?;
                            moves.push(parsed);
                        }
                        options.search_moves = Some(moves)
                    }
                    "ponder" => options.ponder = true,
                    "wtime" => {
                        let time = words.next().ok_or(format!("Missing wtime value in: {}", line))?;
                        let ms: usize = time.parse().map_err(|e|format!("Invalid wtime value: {}", e))?;
                        options.wtime = Some(ms);
                    }
                    "btime" => {
                        let time = words.next().ok_or(format!("Missing btime value in: {}", line))?;
                        let ms: usize = time.parse().map_err(|e|format!("Invalid btime value: {}", e))?;
                        options.wtime = Some(ms);
                    }
                    "winc" => {
                        let time = words.next().ok_or(format!("Missing wincvalue in: {}", line))?;
                        let ms: usize = time.parse().map_err(|e|format!("Invalid winc value: {}", e))?;
                        options.winc = Some(ms);
                    }
                    "binc" => {
                        let time = words.next().ok_or(format!("Missing binc value in: {}", line))?;
                        let ms: usize = time.parse().map_err(|e|format!("Invalid binc value: {}", e))?;
                        options.btime = Some(ms);
                    }
                    "movestogo" => {
                        let moves = words.next().ok_or(format!("Missing movestogo value in: {}", line))?;
                        let moves: usize = moves.parse().map_err(|e|format!("Invalid movestogo value: {}", e))?;
                        options.moves_to_go = Some(moves);
                    }
                    "depth" => {
                        let depth = words.next().ok_or(format!("Missing depth value in: {}", line))?;
                        let depth: usize = depth.parse().map_err(|e|format!("Invalid depth value: {}", e))?;
                        options.depth = Some(depth);
                    }
                    "nodes" => {
                        let nodes = words.next().ok_or(format!("Missing nodes value in: {}", line))?;
                        let nodes: usize = nodes.parse().map_err(|e|format!("Invalid nodes value: {}", e))?;
                        options.nodes = Some(nodes);
                    }
                    "mate" => {
                        let mate = words.next().ok_or(format!("Missing mate value in: {}", line))?;
                        let mate : usize = mate.parse().map_err(|e|format!("Invalid mate value: {}", e))?;
                        options.mate = Some(mate);
                    }
                    "movetime" => {
                        let time = words.next().ok_or(format!("Missing movetime value in: {}", line))?;
                        let time : usize = time.parse().map_err(|e|format!("Invalid movetime value: {}", e))?;
                        options.move_time = Some(time);
                    }
                    "infinite" => {
                        options.infinite = true;
                    }
                    _ => return Err(format!("Invalid go option: {}", option)),
                }
            }
            Ok(dispatch.go(options))
        }
        "stop" => Ok(dispatch.stop()),
        "ponderhit" => Ok(dispatch.ponder_hit()),
        "quit" => Ok(dispatch.quit()),
        _ => Err(format!("Invalid uci command: {line}")),
    }
}
