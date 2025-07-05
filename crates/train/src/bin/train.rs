// A simple network cribbed from the bullet simple example
use bullet_lib::{
    game::formats::sfbinpack::{
        TrainingDataEntry,
        chess::{r#move::MoveType, piecetype::PieceType},
    },
    trainer::{
        schedule::{TrainingSchedule, TrainingSteps, lr, wdl},
        settings::LocalSettings,
    },
    value::loader::SfBinpackLoader,
};
use nnue::{SCALE, SUPERBATCHES, WDL_PROPORTION};

fn main() {
    let initial_lr = 0.001;
    let final_lr = 0.001 * 0.3f32.powi(5);

    let mut trainer = nnue_train::get_trainer();

    let schedule = TrainingSchedule {
        net_id: "1_simple".to_string(),
        eval_scale: SCALE as f32,
        steps: TrainingSteps {
            batch_size: 16_384,
            batches_per_superbatch: 6104,
            start_superbatch: 1,
            end_superbatch: SUPERBATCHES,
        },
        wdl_scheduler: wdl::ConstantWDL {
            value: WDL_PROPORTION,
        },
        lr_scheduler: lr::CosineDecayLR {
            initial_lr,
            final_lr,
            final_superbatch: SUPERBATCHES,
        },
        save_rate: 10,
    };

    let settings = LocalSettings {
        threads: 16,
        test_set: None,
        output_directory: "checkpoints",
        batch_queue_size: 32,
    };
    // loading from a SF binpack
    let data_loader = {
        // Just hardcode for now.
        let file_path = "data/data_d9_2021_09_02.binpack";
        let buffer_size_mb = 1024;
        let threads = 4;
        fn filter(entry: &TrainingDataEntry) -> bool {
            entry.ply >= 16
                && !entry.pos.is_checked(entry.pos.side_to_move())
                && entry.score.unsigned_abs() <= 10000
                && entry.mv.mtype() == MoveType::Normal
                && entry.pos.piece_at(entry.mv.to()).piece_type() == PieceType::None
        }

        SfBinpackLoader::new(file_path, buffer_size_mb, threads, filter)
    };

    //trainer.load_weights_from_file("checkpoints/1_simple-160/optimiser_state/weights.bin");
    trainer.run(&schedule, &settings, &data_loader);
    let starting_eval = trainer.eval("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    println!("starting Eval: {}", 400.0 * starting_eval);

    let london_eval =
        trainer.eval("r1bqk2r/pp3ppp/2n1pn2/2pp4/3P4/2P1PNP1/PP1N1PP1/R2QKB1R b KQkq - 0 8");
    println!("london Eval: {}", 400.0 * london_eval);

    let black_blunder_eval =
        trainer.eval("r1b1k2r/pp3ppp/2n1pn2/2pp4/1P1P4/4PNP1/P2N1PP1/R2QKB1R b KQkq - 0 10");
    println!("black blunder Eval: {}", 400.0 * black_blunder_eval);

    let white_blunder_eval =
        trainer.eval("r1b1k2r/pp3ppp/2n1pn2/2pp4/q2P4/2P1PNP1/PP1N1PP1/R3KB1R w KQkq - 0 10");
    println!("white blunder Eval: {}", 400.0 * white_blunder_eval);

    let worse_eval =
        trainer.eval("r1b1k2r/pp3ppp/2n1pn2/3p4/1q1P4/4PNP1/P2N1PP1/R3KB1R w KQkq - 0 13");
    println!("worse Eval: {}", 400.0 * worse_eval);
}
