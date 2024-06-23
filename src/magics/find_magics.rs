use gem::board::*;
use rand::prelude::*;
use std::{arch::asm, io};

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

static RBITS: [u8; 64] = [
  12, 11, 11, 11, 11, 11, 11, 12,
  11, 10, 10, 10, 10, 10, 10, 11,
  11, 10, 10, 10, 10, 10, 10, 11,
  11, 10, 10, 10, 10, 10, 10, 11,
  11, 10, 10, 10, 10, 10, 10, 11,
  11, 10, 10, 10, 10, 10, 10, 11,
  11, 10, 10, 10, 10, 10, 10, 11,
  12, 11, 11, 11, 11, 11, 11, 12
];

static BBITS: [u8; 64] = [
  6, 5, 5, 5, 5, 5, 5, 6,
  5, 5, 5, 5, 5, 5, 5, 5,
  5, 5, 7, 7, 7, 7, 5, 5,
  5, 5, 7, 9, 9, 7, 5, 5,
  5, 5, 7, 9, 9, 7, 5, 5,
  5, 5, 7, 7, 7, 7, 5, 5,
  5, 5, 5, 5, 5, 5, 5, 5,
  6, 5, 5, 5, 5, 5, 5, 6
];

fn find_rook_magic(pos: Posn, rng: &mut StdRng) {
    let mask = ROOK_MASK[pos.pos.ilog2() as usize];
    let num_bits = RBITS[pos.pos.ilog2() as usize];

    let mut bitset: [u64; 1 << 9];
    'outer: loop {
        bitset = [0; 1 << 9];
        let c1: u64 = rng.gen();
        let c2: u64 = rng.gen();
        let c3: u64 = rng.gen();
        let candidate: u64 = c1 & c2 & c3;
        let transformed = u64::wrapping_mul(candidate, mask);
        if transformed.leading_ones() < num_bits as u32{
            continue;
        }

        for occ in 0..(1 << num_bits) {
            let occupants = pdep(mask, occ);
            let key = u64::wrapping_mul(occupants, candidate) >> (64 - num_bits);
            let offset = 1 << (key % 64);
            let idx = (key / 64) as usize;
            if bitset[idx] & offset != 0{
                continue 'outer;
            }
            bitset[idx] |= offset;
        }

        println!("Found Magic for rook pos {}", pos);
        println!("Mask: {:#x}", mask);
        println!("Magic: {:#x}", candidate);

        break;
    }
}

fn find_bishop_magic(pos: Posn, rng: &mut StdRng) {
    let mask = BISHOP_MASK[pos.pos.ilog2() as usize];
    let num_bits = BBITS[pos.pos.ilog2() as usize];

    let mut bitset: [u64; 1 << 6];
    'outer: loop {
        bitset = [0; 1 << 6];
        let c1: u64 = rng.gen();
        let c2: u64 = rng.gen();
        let c3: u64 = rng.gen();
        let candidate: u64 = c1 & c2 & c3;
        let transformed = u64::wrapping_mul(candidate, mask);
        if transformed.leading_ones() < num_bits as u32{
            continue;
        }

        for occ in 0..(1 << num_bits) {
            let occupants = pdep(mask, occ);
            let key = u64::wrapping_mul(occupants, candidate) >> (64 - num_bits);
            let offset = 1 << (key % 64);
            let idx = (key / 64) as usize;
            if bitset[idx] & offset != 0{
                continue 'outer;
            }
            bitset[idx] |= offset;
        }

        println!("Found Magic for bishop pos {}", pos);
        println!("Mask: {:#x}", mask);
        println!("Magic: {:#x}", candidate);

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
