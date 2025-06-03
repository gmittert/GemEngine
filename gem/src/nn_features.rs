use bitboard::{
    moves::{Color, Piece},
    posn::Posn,
};

use crate::board::Board;

// We'll start with a simple feature set: a one-hot encoding of each square by piece by color.
pub struct FeatureSet {
    // 64 Squares x 2 colors x 6 pieces = 768
    // 64 bit * 12 = 768 bytes
    features: [u64; 12],
}

impl FeatureSet {
    pub fn new() -> FeatureSet {
        FeatureSet { features: [0; 12] }
    }

    pub fn from(board: &Board) -> FeatureSet {
        let mut empty = FeatureSet::new();
        for piece in [
            Piece::Pawn,
            Piece::Rook,
            Piece::Knight,
            Piece::Bishop,
            Piece::Queen,
            Piece::King,
        ] {
            for color in [Color::Black, Color::White] {
                for posn in board.piece(color, piece) {
                    empty.set(posn, piece, color);
                }
            }
        }
        empty
    }
    pub fn as_bytes(&self) -> &[u8] {
        let len = self.features.len() * 8;
        let ptr = self.features.as_ptr();
        unsafe{ std::slice::from_raw_parts(ptr.cast::<u8>(), len)}
    }

    pub fn idx(pos: Posn, piece: Piece, color: Color) -> usize {
        let pos_idx = pos.idx() as usize + 1;
        let piece_idx = piece as usize + 1;
        let color_idx = color as usize + 1;
        (pos_idx * piece_idx * color_idx) - 1
    }

    pub fn set(&mut self, pos: Posn, piece: Piece, color: Color) {
        let idx = FeatureSet::idx(pos, piece, color);
        let word_idx = idx / 64;
        let bit_idx = idx % 64;
        self.features[word_idx] |= 1 << (bit_idx);
    }

    pub fn reset(&mut self, pos: Posn, piece: Piece, color: Color) {
        let idx = FeatureSet::idx(pos, piece, color);
        let word_idx = idx / 64;
        let bit_idx = idx % 64;
        self.features[word_idx] &= !(1 << (bit_idx));
    }
}
