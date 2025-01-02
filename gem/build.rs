use bitboard::posn::{Posn, ALL_POSNS};
use bitboard::BitBoard;
use magics::{BBITS, BISHOP_MAGICS, BISHOP_MASK, RBITS, ROOK_MAGICS, ROOK_MASK};
use std::arch::asm;
use std::fs::File;
use std::io::Write;
use std::{env, path::Path};

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

static mut ROOK_SLIDING_TABLE: [[u64; 4096]; 64] = [[0; 4096]; 64];
static mut BISHOP_SLIDING_TABLE: [[u64; 512]; 64] = [[0; 512]; 64];

fn main() -> std::io::Result<()> {
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
            unsafe { ROOK_SLIDING_TABLE[pos.pos.ilog2() as usize][key as usize] = acc.0 };
        }
    }

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
            unsafe {
                BISHOP_SLIDING_TABLE[pos.pos.ilog2() as usize][key as usize] = acc.0;
            }
        }
    }
    let out_dir = env::var("OUT_DIR").unwrap();
    let dest_path = Path::new(&out_dir).join("magics.rs");

    let rook_sliding_table = format!(
        "pub static ROOK_SLIDING_TABLE:[[u64; 4096]; 64] = {:?};\n",
        unsafe { ROOK_SLIDING_TABLE },
    );
    let bishop_sliding_table = format!(
        "pub static BISHOP_SLIDING_TABLE:[[u64; 512]; 64] = {:?};\n",
        unsafe { BISHOP_SLIDING_TABLE },
    );

    let mut file = File::create(dest_path)?;
    file.write_all(rook_sliding_table.as_bytes())?;
    file.write_all(bishop_sliding_table.as_bytes())
}
