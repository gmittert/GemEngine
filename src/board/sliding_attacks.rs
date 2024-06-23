use crate::board::magics;
use crate::board::magics::BBITS;
use crate::board::magics::BISHOP_MAGICS;
use crate::board::magics::RBITS;
use crate::board::magics::ROOK_MAGICS;
use crate::board::BitBoard;
use crate::board::Posn;
use lazy_static::lazy_static;
use std::arch::asm;

use super::File;
use super::Rank;
use super::ALL_POSNS;

fn pext(mask: u64, bits: u64) -> u64 {
    let mut x: u64;
    unsafe {
        asm!(
            "pext {x}, {bits}, {mask}",
            x = lateout(reg) x,
            bits = in(reg) bits,
            mask = in(reg) mask,
        )
    }
    x
}

fn pdep(mask: u64, bits: u64) -> u64 {
    let mut x: u64;
    unsafe {
        asm!(
            "pdep {x}, {bits}, {mask}",
            x = lateout(reg) x,
            bits = in(reg) bits,
            mask = in(reg) mask,
        )
    }
    x
}

static BISHOP_MASK: [u64; 64] = {
    let mut arr = [0; 64];
    let mut i = 0;
    while i < 64 {
        let pos = Posn { pos: 1 << i };

        let mut acc = 0;

        let mut slide = pos.ne();
        while let Some(pos) = slide {
            if pos.ne().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.ne();
        }

        let mut slide = pos.se();
        while let Some(pos) = slide {
            if pos.se().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.se();
        }

        let mut slide = pos.nw();
        while let Some(pos) = slide {
            if pos.nw().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.nw();
        }

        let mut slide = pos.sw();
        while let Some(pos) = slide {
            if pos.sw().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.sw();
        }

        arr[i] = acc;
        i += 1;
    }
    arr
};

static ROOK_MASK: [u64; 64] = {
    let mut arr = [0; 64];
    let mut i = 0;
    while i < 64 {
        let pos = Posn { pos: 1 << i };

        let mut acc = 0;

        let mut slide = pos.no();
        while let Some(pos) = slide {
            if pos.no().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.no();
        }

        let mut slide = pos.so();
        while let Some(pos) = slide {
            if pos.so().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.so();
        }

        let mut slide = pos.ea();
        while let Some(pos) = slide {
            if pos.ea().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.ea();
        }

        let mut slide = pos.we();
        while let Some(pos) = slide {
            if pos.we().is_none() {
                break;
            }
            acc |= pos.pos;
            slide = pos.we();
        }

        arr[i] = acc;
        i += 1;
    }
    arr
};

lazy_static! {
    static ref ROOK_SLIDING_TABLE: [[u64; 4096]; 64] = {
        let mut out = Box::new([[0; 4096]; 64]);
        for pos in ALL_POSNS {
            let num_bits = RBITS[pos.pos.ilog2() as usize];
            for idx in 0..(1 << num_bits) {
                let mask = ROOK_MASK[pos.pos.ilog2() as usize];
                let occupants = pdep(mask, idx);
                let board = BitBoard(occupants);

                let mut acc = BitBoard::empty();
                for shift in [
                    |p: Posn| p.no(),
                    |p: Posn| p.so(),
                    |p: Posn| p.ea(),
                    |p: Posn| p.we(),
                ] {
                    let mut slide = shift(pos);
                    while let Some(pos) = slide {
                        acc |= pos;
                        if board.contains(pos) {
                            break;
                        }
                        slide = shift(pos);
                    }
                }
                let key = u64::wrapping_mul(occupants, ROOK_MAGICS[pos.pos.ilog2() as usize])
                    >> (64 - num_bits);
                out[pos.pos.ilog2() as usize][key as usize] = acc.0;
            }
        }
        *Box::leak(out)
    };
    static ref BISHOP_SLIDING_TABLE: [[u64; 512]; 64] = {
        let mut out = Box::new([[0; 512]; 64]);

        for pos in ALL_POSNS {
            let num_bits = BBITS[pos.pos.ilog2() as usize];
            for idx in 0..(1 << num_bits) {
                let mask = BISHOP_MASK[pos.pos.ilog2() as usize];
                let occupants = pdep(mask, idx);
                let board = BitBoard(occupants);

                let mut acc = BitBoard::empty();
                for shift in [
                    |p: Posn| p.ne(),
                    |p: Posn| p.se(),
                    |p: Posn| p.nw(),
                    |p: Posn| p.sw(),
                ] {
                    let mut slide = shift(pos);
                    while let Some(pos) = slide {
                        acc |= pos;
                        if board.contains(pos) {
                            break;
                        }
                        slide = shift(pos);
                    }
                }
                let key = u64::wrapping_mul(occupants, BISHOP_MAGICS[pos.pos.ilog2() as usize])
                    >> (64 - num_bits);
                out[pos.pos.ilog2() as usize][key as usize] = acc.0;
            }
        }
        *Box::leak(out)
    };
}

pub fn compute_rook_attacks(from: Posn, board: BitBoard) -> BitBoard {
    let mask = ROOK_MASK[from.pos.ilog2() as usize];
    let num_bits = RBITS[from.pos.ilog2() as usize];
    let magic = ROOK_MAGICS[from.pos.ilog2() as usize];
    let key = u64::wrapping_mul(board.0 & mask, magic) >> (64 - num_bits);
    BitBoard(ROOK_SLIDING_TABLE[from.pos.ilog2() as usize][key as usize])
}

pub fn compute_bishop_attacks(from: Posn, board: BitBoard) -> BitBoard {
    let mask = BISHOP_MASK[from.pos.ilog2() as usize];
    let num_bits = BBITS[from.pos.ilog2() as usize];
    let magic = BISHOP_MAGICS[from.pos.ilog2() as usize];
    let key = u64::wrapping_mul(board.0 & mask, magic) >> (64 - num_bits);
    BitBoard(BISHOP_SLIDING_TABLE[from.pos.ilog2() as usize][key as usize])
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
        let pos = d5();
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
