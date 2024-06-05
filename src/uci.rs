use crate::board::{evaluation::Evaluation, AlgebraicMove, Move};
pub mod reader;

#[derive(Default, PartialEq)]
pub struct GoOptions {
    // Restrict search to only these moves
    search_moves: Option<Vec<AlgebraicMove>>,
    ponder: bool,
    // White's remaining ms on the clock
    wtime: Option<usize>,
    // Black's remaining ms on the clock
    btime: Option<usize>,
    // White's increment per move in ms
    winc: Option<usize>,
    // Black's increment per move in ms
    binc: Option<usize>,
    // there are x moves to the next time control,
    moves_to_go: Option<usize>,
    // Only search `depth` plies deep
    depth: Option<usize>,
    // Check only `nodes` deep
    nodes: Option<usize>,
    // Search for a mate in `mate` moves
    mate: Option<usize>,
    // Search for exactly x mseconds
    move_time: Option<usize>,
    // Search until recieving "stop" command
    infinite: bool,
}

pub struct Score {
    eval: Evaluation,
    is_upper_bound: bool,
    is_lower_bound: bool,
}

enum EngineOptionType {
    Chec,
    Spin,
    Combo(Vec<String>),
    Button,
    String,
}

pub struct EngineOption {
    name: String,
    ty: EngineOptionType,
    default: Option<String>,
    min: Option<i64>,
    max: Option<i64>,
}

pub struct Info {
    // search depth in plies
    depth: Option<usize>,
    // selective search depth in plies,
    seldepth: Option<usize>,
    // the time searched in ms
    time: Option<usize>,
    // number of  nodes searched
    nodes: Option<usize>,
    // The best line(s) found
    pv: Option<Vec<Vec<Move>>>,

    // The score from the engine's point of view in centipawns
    score: Option<Score>,
    // currently searching this move
    curr_move: Option<Move>,

    // currently searching move number x, for the first move x should be 1 not 0.
    curr_move_number: Option<usize>,
    // the hash is `hash_full` permill full
    hash_full: Option<usize>,
    // x nodes per second searched
    nodes_per_sec: Option<usize>,
    // x positions where found in the endgame table bases
    table_base_hits: Option<usize>,
    // x positions where found in the shredder endgame databases
    shredder_ending_hits: Option<usize>,
    // the cpu usage of the engine is x permill.
    cpuload: Option<usize>,
    // any string str which will be displayed
    string: Option<String>,

    // move <move1> is refuted by the line <move2> ... <movei>
    refutation: Option<Vec<Move>>,

    // the current lines engine is calculating on each cpu
    curr_lines: Option<Vec<Move>>,
}

pub enum RegistrationStatus {
    Ok,
    Err,
    Checking,
}

pub trait UciEngine{
    //	After receiving the uci command the engine must
    //	- identify itself with the "id" command
    //	- send the "option" commands to tell the GUI which engine settings the engine supports if any
    //	- the engine should send "uciok" to acknowledge the uci mode
    fn uci(&self);

    // switch the debug mode of the engine on and off. In debug mode the engine should send
    // additional infos to the GUI, e.g. with the "info string" command, to help debugging, e.g.
    // the commands that the engine has received etc. This mode should be switched off by default
    // and this command can be sent any time, also when the engine is thinking.
    fn debug(&self, on: bool);

    // Allow the GUI to synchronize with the engine. The engine should respond with ready_ok() to
    // indicate to the GUI that it has completed processing any remaining input
    fn is_ready(&self);

    // Allow various configuration options exposed by the engine to be set
    fn set_option(&self, name: &str, value: Option<&str>);

    //this is the command to try to register an engine or to tell the engine that registration
    //will be done later. This command should always be sent if the engine	has sent "registration error"
    //at program startup.
    //The following tokens are allowed:
    //* later
    //   the user doesn't want to register the engine now.
    //* name <x>
    //   the engine should be registered with the name <x>
    //* code <y>
    //   the engine should be registered with the code <y>
    //Example:
    //   "register later"
    //   "register name Stefan MK code 4359874324"

    fn register(&self);

    // this is sent to the engine when the next search (started with "position" and "go") will be from
    // a different game. This can be a new game the engine should play or a new game it should analyse but
    // also the next position from a testsuite with positions only.
    // If the GUI hasn't sent a "ucinewgame" before the first "position" command, the engine shouldn't
    // expect any further ucinewgame commands as the GUI is probably not supporting the ucinewgame command.
    // So the engine should not rely on this command even though all new GUIs should support it.
    // As the engine's reaction to "ucinewgame" can take some time the GUI should always send "isready"
    // after "ucinewgame" to wait for the engine to finish its operation.
    fn uci_new_game(&self);

    // set up the position described in fenstring on the internal board and
    // play the moves on the internal chess board.
    // if the game was played  from the start position the string "startpos" will be sent
    // Note: no "new" command is needed. However, if this position is from a different game than
    // the last position sent to the engine, the GUI should have sent a "ucinewgame" inbetween.
    fn position(&self, fen: &str, moves: Vec<AlgebraicMove>);

    // Start calculating on the current position set up with the "position" command.
    fn go(&self, options: GoOptions);

    // Stop calculating as soon as possible,
    //
    // The engine should send "bestmove" and possibly the "ponder" token when finishing the search
    fn stop(&self);

    // The user has played the expected move. This will be sent if the engine was told to ponder on the same move
    // the user has played. The engine should continue searching but switch from pondering to normal search.
    fn ponder_hit(&self);

    // quit the program
    fn quit(&self);
}

pub trait UciWriter{
    // Identify the engine to the GUI
    fn id(&self, name: str, author: str);

    // Let the GUI know that we have identified ourself and are in UCI mode
    fn uci_ok(&self);

    // After recieiving a "is_ready", the engine should respond with "ready_ok()" after processing
    // all input.
    fn ready_ok(&self);

    // The engine has stopped searching and found the move <move> best in this position. The engine
    // can optionally send the move it likes to ponder on.
    fn best_move(&self, m: Move, ponder: Option<Move>);

    // Inform the gui of if registration is needed
    fn registration(&self, status: RegistrationStatus);

    // The engine wishes to send information to the GUI.
    fn info(&self, info_block: Info);

    // This command tells the GUI which parameters can be changed in the engine.
    fn option(&self, name: str);
}
