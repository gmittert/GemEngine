use std::array;

use crate::board::{AlgebraicMove, Move};


pub struct TranspositionData {
    zobrist_key: u64,
    best_move: AlgebraicMove,
    depth: u8,

}
#[derive(Debug)]
pub struct TranspositionTable<const N: usize> {
    // 1GiB
    data: Box<[(u64, T); N]>,
    hits: usize,
    misses: usize,
    conflicts: usize,

    accepted: usize,
    rejected: usize,
}
impl<T, const N: usize> HashMap<T, N>
where
    T: Default,
{
    pub fn print_stats(&self) {
        println!("HashMap Stats: ");
        println!("Hits: {}", self.hits);
        println!("Misses: {}", self.misses);
        println!("Conflicts: {}", self.conflicts);
        println!(
            "Hit Rate: {}",
            self.hits as f64 / (self.hits + self.misses + self.conflicts) as f64
        );
        println!("");
        println!("Accepted: {}", self.accepted);
        println!("Rejected: {}", self.rejected);
        println!(
            "Acceptance Rate: {}",
            self.accepted as f64 / (self.accepted + self.rejected) as f64
        );
    }
    pub fn new() -> HashMap<T, N> {
        HashMap {
            data: Box::new(array::from_fn(|_| (0, T::default()))),
            hits: 0,
            misses: 0,
            conflicts: 0,
            accepted: 0,
            rejected: 0,
        }
    }
    pub fn get(&mut self, k: u64) -> Option<&T> {
        let pos: usize = k as usize % N;
        let (kp, v) = &self.data[pos];
        if *kp == 0 {
            self.misses += 1;
            return None;
        }
        if *kp != k {
            self.conflicts += 1;
            return None;
        }
        self.hits += 1;
        Some(v)
    }

    pub fn insert(&mut self, k: u64, v: T) -> bool {
        let pos: usize = k as usize % N;
        let (kp, _) = &self.data[pos];
        if *kp != 0 {
            self.rejected += 1;
            return false;
        }
        self.accepted += 1;
        self.data[pos] = (k, v);
        true
    }
}
