use bitboard::BitBoard;
use bitboard::posn::Posn;
use magics::BBITS;
use magics::BISHOP_MAGICS;
use magics::BISHOP_MASK;
use magics::RBITS;
use magics::ROOK_MAGICS;
use magics::ROOK_MASK;

// See build.rs for where this is generated
include!(concat!(env!("OUT_DIR"), "/magics.rs"));

pub fn compute_rook_attacks(from: Posn, board: BitBoard) -> BitBoard {
    let mask = ROOK_MASK[from.idx() as usize];
    let num_bits = RBITS[from.idx() as usize];
    let magic = ROOK_MAGICS[from.idx() as usize];
    let key = u64::wrapping_mul(board.0 & mask, magic) >> (64 - num_bits);
    BitBoard(ROOK_SLIDING_TABLE[from.idx() as usize][key as usize])
}

pub fn compute_bishop_attacks(from: Posn, board: BitBoard) -> BitBoard {
    let mask = BISHOP_MASK[from.idx() as usize];
    let num_bits = BBITS[from.idx() as usize];
    let magic = BISHOP_MAGICS[from.idx() as usize];
    let key = u64::wrapping_mul(board.0 & mask, magic) >> (64 - num_bits);
    BitBoard(BISHOP_SLIDING_TABLE[from.idx() as usize][key as usize])
}

#[cfg(test)]
mod tests {
    use sliding_attacks::{compute_bishop_attacks, compute_rook_attacks};

    use crate::board::*;
    #[test]
    pub fn rook_slides_empty() {
        for rank in [
            Rank::Two,
            Rank::Three,
            Rank::Four,
            Rank::Five,
            Rank::Six,
            Rank::Seven,
            Rank::Eight,
        ] {
            for file in [
                File::A,
                File::B,
                File::C,
                File::D,
                File::E,
                File::F,
                File::G,
            ] {
                let mut board = empty_board(Color::White);
                let pos = Posn::from(rank, file);
                board.white_pieces[Piece::Rook as usize] = BitBoard::from(pos);
                board.black_pieces[Piece::King as usize] = BitBoard::from(h1());
                let computed = compute_rook_attacks(pos, board.pieces());
                assert!(!computed.contains(h1()));
            }
        }
    }
    #[test]
    pub fn bishop_slides_empty() {
        let mut board = empty_board(Color::White);
        board.white_pieces[Piece::Rook as usize] = BitBoard::from(d5());
        let computed = compute_bishop_attacks(d5(), board.pieces());
        assert!(computed.contains(a2()));
        assert!(computed.contains(b3()));
        assert!(computed.contains(c4()));
        assert!(computed.contains(e6()));
        assert!(computed.contains(f7()));
    }
}
