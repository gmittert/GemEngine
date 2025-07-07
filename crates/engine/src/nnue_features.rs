use bitboard::{
    moves::{Color, Piece},
    posn::Posn,
};
use nnue::Accumulator;

// Bulletformat/chessboard considers a1 to be 0, Gem considers h1 to be 0;
// Since our nnue is trained using chessboard, we need to flip the board with (^ 7).
//
// TODO: retrain using our own board representation.
pub fn apply(update: FeatureUpdate, white: &mut Accumulator, black: &mut Accumulator) {
    match update {
        FeatureUpdate::MoveOne {
            c,
            from,
            to,
            from_piece,
            to_piece,
        } => {
            let from_pc = 64 * from_piece as usize;
            let to_pc = 64 * to_piece as usize;
            // Bulletformat/chessboard considers a1 to be 0, Gem considers h1 to be 0;
            // Since our nnue is trained using chessboard, we need to flip the board.
            //
            // TODO: retrain using our own board representation.
            let from_sq = from.idx() as usize ^ 7;
            let to_sq = to.idx() as usize ^ 7;
            let perspective = usize::from(c == Color::White);

            white.add_remove_feature(
                [384, 0][perspective] + to_pc + to_sq,
                [384, 0][perspective] + from_pc + from_sq,
                &nnue::NNUE,
            );
            black.add_remove_feature(
                [0, 384][perspective] + to_pc + (to_sq ^ 56),
                [0, 384][perspective] + from_pc + (from_sq ^ 56),
                &nnue::NNUE,
            );
        }
        FeatureUpdate::MoveTwo {
            c,
            from1,
            to1,
            piece1,
            from2,
            to2,
            piece2,
        } => {
            let pc1 = 64 * piece1 as usize;
            let pc2 = 64 * piece2 as usize;
            let from_sq1 = from1.idx() as usize ^ 7;
            let to_sq1 = to1.idx() as usize ^ 7;
            let from_sq2 = from2.idx() as usize ^ 7;
            let to_sq2 = to2.idx() as usize ^ 7;
            let perspective = usize::from(c == Color::White);

            white.add2_remove2_feature(
                [384, 0][perspective] + pc1 + to_sq1,
                [384, 0][perspective] + pc2 + to_sq2,
                [384, 0][perspective] + pc1 + from_sq1,
                [384, 0][perspective] + pc2 + from_sq2,
                &nnue::NNUE,
            );
            black.add2_remove2_feature(
                [0, 384][perspective] + pc1 + (to_sq1 ^ 56),
                [0, 384][perspective] + pc2 + (to_sq2 ^ 56),
                [0, 384][perspective] + pc1 + (from_sq1 ^ 56),
                [0, 384][perspective] + pc2 + (from_sq2 ^ 56),
                &nnue::NNUE,
            );
        }
        FeatureUpdate::Capture {
            c,
            from_piece,
            from,
            to_piece,
            to,
            capture_piece,
            capture_pos,
        } => {
            let from_pc = 64 * from_piece as usize;
            let to_pc = 64 * to_piece as usize;
            let capture_pc = 64 * capture_piece as usize;
            let from_sq = from.idx() as usize ^ 7;
            let to_sq = to.idx() as usize ^ 7;
            let capture_sq = capture_pos.idx() as usize ^ 7;
            let perspective = usize::from(c == Color::White);

            white.add1_remove2_feature(
                [384, 0][perspective] + to_pc + to_sq,
                [0, 384][perspective] + capture_pc + capture_sq,
                [384, 0][perspective] + from_pc + from_sq,
                &nnue::NNUE,
            );
            black.add1_remove2_feature(
                [0, 384][perspective] + to_pc + (to_sq ^ 56),
                [384, 0][perspective] + capture_pc + (capture_sq ^ 56),
                [0, 384][perspective] + from_pc + (from_sq ^ 56),
                &nnue::NNUE,
            );
        }
    }
}

pub fn unapply(update: FeatureUpdate, white: &mut Accumulator, black: &mut Accumulator) {
    match update {
        FeatureUpdate::MoveOne {
            c,
            from,
            to,
            from_piece,
            to_piece,
        } => {
            let from_pc = 64 * from_piece as usize;
            let to_pc = 64 * to_piece as usize;
            let from_sq = from.idx() as usize ^ 7;
            let to_sq = to.idx() as usize ^ 7;
            let perspective = usize::from(c == Color::White);

            white.add_remove_feature(
                [384, 0][perspective] + from_pc + from_sq,
                [384, 0][perspective] + to_pc + to_sq,
                &nnue::NNUE,
            );
            black.add_remove_feature(
                [0, 384][perspective] + from_pc + (from_sq ^ 56),
                [0, 384][perspective] + to_pc + (to_sq ^ 56),
                &nnue::NNUE,
            );
        }
        FeatureUpdate::MoveTwo {
            c,
            from1,
            to1,
            piece1,
            from2,
            to2,
            piece2,
        } => {
            let pc1 = 64 * piece1 as usize;
            let pc2 = 64 * piece2 as usize;
            let from_sq1 = from1.idx() as usize ^ 7;
            let to_sq1 = to1.idx() as usize ^ 7;
            let from_sq2 = from2.idx() as usize ^ 7;
            let to_sq2 = to2.idx() as usize ^ 7;
            let perspective = usize::from(c == Color::White);

            white.add2_remove2_feature(
                [384, 0][perspective] + pc1 + from_sq1,
                [384, 0][perspective] + pc2 + from_sq2,
                [384, 0][perspective] + pc1 + to_sq1,
                [384, 0][perspective] + pc2 + to_sq2,
                &nnue::NNUE,
            );
            black.add2_remove2_feature(
                [0, 384][perspective] + pc1 + (from_sq1 ^ 56),
                [0, 384][perspective] + pc2 + (from_sq2 ^ 56),
                [0, 384][perspective] + pc1 + (to_sq1 ^ 56),
                [0, 384][perspective] + pc2 + (to_sq2 ^ 56),
                &nnue::NNUE,
            );
        }
        FeatureUpdate::Capture {
            c,
            from_piece,
            from,
            to_piece,
            to,
            capture_piece,
            capture_pos,
        } => {
            let from_pc = 64 * from_piece as usize;
            let to_pc = 64 * to_piece as usize;
            let capture_pc = 64 * capture_piece as usize;
            let from_sq = from.idx() as usize ^ 7;
            let to_sq = to.idx() as usize ^ 7;
            let capture_sq = capture_pos.idx() as usize ^ 7;
            let perspective = usize::from(c == Color::White);

            white.add2_remove1_feature(
                [0, 384][perspective] + capture_pc + capture_sq,
                [384, 0][perspective] + from_pc + from_sq,
                [384, 0][perspective] + to_pc + to_sq,
                &nnue::NNUE,
            );
            black.add2_remove1_feature(
                [384, 0][perspective] + capture_pc + (capture_sq ^ 56),
                [0, 384][perspective] + from_pc + (from_sq ^ 56),
                [0, 384][perspective] + to_pc + (to_sq ^ 56),
                &nnue::NNUE,
            );
        }
    }
}

#[derive(Debug, Clone)]
pub enum FeatureUpdate {
    MoveOne {
        c: Color,
        from: Posn,
        to: Posn,
        from_piece: Piece,
        to_piece: Piece,
    },
    MoveTwo {
        c: Color,
        from1: Posn,
        to1: Posn,
        piece1: Piece,
        from2: Posn,
        to2: Posn,
        piece2: Piece,
    },
    Capture {
        c: Color,
        from_piece: Piece,
        from: Posn,
        to_piece: Piece,
        to: Posn,
        capture_piece: Piece,
        capture_pos: Posn,
    },
}
