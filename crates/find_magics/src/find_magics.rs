use bitboard::posn::*;
use rand::prelude::*;
use std::{arch::asm, io};

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

fn find_rook_magic(pos: Posn, rng: &mut StdRng) {
    let mask = magics::ROOK_MASK[pos.idx() as usize];
    let num_bits = magics::RBITS[pos.idx() as usize];

    let mut bitset: [u64; 1 << 9];
    'outer: loop {
        bitset = [0; 1 << 9];
        let c1: u64 = rng.random();
        let c2: u64 = rng.random();
        let c3: u64 = rng.random();
        let candidate: u64 = c1 & c2 & c3;
        let transformed = u64::wrapping_mul(candidate, mask);
        if transformed.leading_ones() < num_bits as u32 {
            continue;
        }

        for occ in 0..(1 << num_bits) {
            let occupants = pdep(mask, occ);
            let key = u64::wrapping_mul(occupants, candidate) >> (64 - num_bits);
            let offset = 1 << (key % 64);
            let idx = (key / 64) as usize;
            if bitset[idx] & offset != 0 {
                continue 'outer;
            }
            bitset[idx] |= offset;
        }

        println!("Found Magic for rook pos {pos}");
        println!("Mask: {mask:#x}");
        println!("Magic: {candidate:#x}");

        break;
    }
}

fn find_bishop_magic(pos: Posn, rng: &mut StdRng) {
    let mask = magics::BISHOP_MASK[pos.idx() as usize];
    let num_bits = magics::BBITS[pos.idx() as usize];

    let mut bitset: [u64; 1 << 6];
    'outer: loop {
        bitset = [0; 1 << 6];
        let c1: u64 = rng.random();
        let c2: u64 = rng.random();
        let c3: u64 = rng.random();
        let candidate: u64 = c1 & c2 & c3;
        let transformed = u64::wrapping_mul(candidate, mask);
        if transformed.leading_ones() < num_bits as u32 {
            continue;
        }

        for occ in 0..(1 << num_bits) {
            let occupants = pdep(mask, occ);
            let key = u64::wrapping_mul(occupants, candidate) >> (64 - num_bits);
            let offset = 1 << (key % 64);
            let idx = (key / 64) as usize;
            if bitset[idx] & offset != 0 {
                continue 'outer;
            }
            bitset[idx] |= offset;
        }

        println!("Found Magic for bishop pos {pos}");
        println!("Mask: {mask:#x}");
        println!("Magic: {candidate:#x}");

        break;
    }
}

fn main() -> io::Result<()> {
    let mut rng = rand::SeedableRng::seed_from_u64(0xDE4326423);

    for pos in ALL_POSNS {
        find_rook_magic(pos, &mut rng);
    }
    for pos in ALL_POSNS {
        find_bishop_magic(pos, &mut rng);
    }
    Ok(())
}
