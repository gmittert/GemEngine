use std::num::NonZero;
use std::ops;

use crate::board::evaluation::Evaluation;
use bitboard::moves::{AlgebraicMove, Piece};
use bitboard::posn::{File, Posn, Rank};
use tracing::Level;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy, Default)]
pub enum ScoreType {
    #[default]
    Upper = 0,
    Lower,
    Exact,
}

// Data Layout:
//    0..4 From file
//    4..8 From rank
//    8..12 To file
//    12..16 To rank
#[derive(Clone, Copy, PartialEq, Eq, Default, Debug)]
pub struct PackedTTEntry {
    pub eval: Evaluation,
    pub depth: u16,
    pub promo: Option<Piece>,
    pub node_type: ScoreType,
    pub data: Option<NonZero<u16>>,
}

impl PackedTTEntry {
    fn empty(&self) -> bool {
        (unsafe { std::mem::transmute::<PackedTTEntry, u64>(*self) }) == 0
    }
}

impl ops::BitXor<u64> for PackedTTEntry {
    type Output = u64;
    fn bitxor(self, rhs: u64) -> Self::Output {
        (unsafe { std::mem::transmute::<PackedTTEntry, u64>(self) }) ^ rhs
    }
}

impl ops::BitXor<PackedTTEntry> for u64 {
    type Output = u64;
    fn bitxor(self, rhs: PackedTTEntry) -> Self::Output {
        (unsafe { std::mem::transmute::<PackedTTEntry, u64>(rhs) }) ^ self
    }
}

use std::cell::UnsafeCell;

#[derive(Debug, Copy, Clone)]
pub struct SharedHashMapEntry {
    key_xor_v: u64,
    value: PackedTTEntry,
}

#[derive(Debug)]
#[repr(align(0x200000))]
pub struct SharedHashMapInner {
    data: UnsafeCell<[SharedHashMapEntry; DEFAULT_TT_SIZE]>,
}

#[derive(Debug)]
pub struct SharedHashMap {
    data: Box<SharedHashMapInner>,
}

/// A multi level "lockless" hashmap that supports insertion with a "keep deepest" replacement
/// policy.
///
/// In the pursuit of speed over correctness, we don't use atomics at all. Instead, we store the
/// value along side the key xor'd with the value. If the key we're looking up doesn't xor
/// correctly, we return that it's not found.
///
/// To improve cache usage, we split the total size into 4 adjacent buckets, each larger than the
/// last. We search from each one in turn, starting from the smallest to the largest.
unsafe impl Send for SharedHashMap {}
unsafe impl Sync for SharedHashMap {}
impl Default for SharedHashMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SharedHashMap {
    pub fn new() -> SharedHashMap {
        // We use the nightly "new_zeroed" because doing a regular box new causes a stack overflow
        // on non release builds.
        let data = Box::<SharedHashMapInner>::new_zeroed();
        let data = unsafe { data.assume_init() };

        SharedHashMap { data }
    }

    pub fn clear(&self) {
        (&mut unsafe { *self.data.data.get() }).fill(SharedHashMapEntry {
            key_xor_v: 0,
            value: PackedTTEntry::default(),
        })
    }

    // How deep we are able to search corresponds to how sparsely our hashmap will be filled, which
    // corresponds to how long we are able to search for. We don't know how long we are going to
    // search for a priori, this means we don't know how big our hashmap should be.
    //
    // Too big, and our use of modulo to place keys will randomly space them out through the, e.g.
    // 256MiB of memory. Every insert is going to eat overhead creating pages, then paging them
    // into cache.
    //
    // Too small, and we don't fit all the thing we want to cache.
    //
    // Instead, we attempt to somewhat naturally let the data choose the amount of pages it needs
    // by having a multi-level modulus attempting to somewhat densely pack entries into the front
    // of the map until it gets filled up.
    //
    // We break our table size into seval smaller tables each larger than the previous which we
    // check in turn.
    const LAYER_COUNT: usize = 4;
    // Ensure that the sizes are powers of two to ensure modulo is efficient.
    const SIZES: [usize; 8] = [
        0x4000, // 2 MiB
        0x4000, // 2 MiB
        0x8000, // 4 MiB
        0x10000, // 8 MiB
        0x20000, // 16 MiB
        0x40000, // 32 MiB
        0x80000, // 64 MiB
        0x100000, // 128 MiB
    ];
    const OFFSETS: [usize; 8] = [
        0,
        0x4000,
        0x8000,
        0x10000,
        0x20000,
        0x40000,
        0x80000,
        0x100000,
    ];

    pub fn get(&self, k: u64) -> Option<PackedTTEntry> {
        if k == 0 {
            return None;
        }

        for i in 0..Self::LAYER_COUNT {
            let base = Self::OFFSETS[i];
            let size = Self::SIZES[i];
            let pos = base + (k as usize % size);
            let v = &unsafe { &*self.data.data.get() }[pos].value;
            if v.empty() {
                return None;
            }
            let key_xor_v = unsafe { &*self.data.data.get() }[pos].key_xor_v;
            let existing_key = key_xor_v ^ *v;
            if existing_key != k {
                continue;
            }
            return Some(*v);
        }
        None
    }

    /// Store a new key value pair in the hashmap. `insert` will not overwrite an existing entry,
    /// and will return a failure if something already exists in the desired hashmap position,
    /// regardless of if the key matches.
    pub fn insert(&self, k: u64, v: PackedTTEntry) {
        // We reserve values of 0 to indicate uninitialized cells. For our use of this hashmap
        // where we store PackedTTEntries which contain a move, these can never be 0. (Consider
        // that a move contains a from an to position, and h1 is the only position that's encoded
        // as 0. Since a move can't be both to and from h1, at least one of "to" or "from" must be
        // non zero).
        let mut insert_k = k;
        let mut insert_v = v;
        let mut insert_kxv = k ^ v;

        for i in 0..Self::LAYER_COUNT {
            let base = Self::OFFSETS[i];
            let size = Self::SIZES[i];
            let pos = base + (insert_k as usize % size);

            let data = unsafe { &mut *self.data.data.get() };
            let existing_val = data[pos].value;
            let existing_kxv = data[pos].key_xor_v;
            let existing_key = existing_kxv ^ existing_val;

            // If it's empty, or the same key of a deeper depth, we can write in the new value.
            if existing_val.empty()
                || (existing_key == insert_k && insert_v.depth > existing_val.depth())
            {
                data[pos].value = insert_v;
                data[pos].key_xor_v = insert_kxv;
                return;
            }

            // If our depth is deeper that what's here, we can kick the entry and find a new spot
            // for it.
            if insert_v.depth() > existing_val.depth() {
                std::mem::swap(&mut insert_kxv, &mut data[pos].key_xor_v);
                std::mem::swap(&mut insert_v, &mut data[pos].value);
                insert_k = insert_kxv ^ insert_v;
            }
        }
    }
}

impl PackedTTEntry {
    pub fn new(
        eval: Evaluation,
        depth: u16,
        best_move: Option<AlgebraicMove>,
        node_type: ScoreType,
    ) -> PackedTTEntry {
        let (data, promo) = match best_move {
            Some(AlgebraicMove {
                from,
                to,
                promotion,
            }) => {
                let mut data: u16 = 0;
                data |= from.file() as u16;
                data |= (from.rank() as u16) << 4;
                data |= (to.file() as u16) << 8;
                data |= (to.rank() as u16) << 12;
                (Some(NonZero::new(data).unwrap()), promotion)
            }
            None => (None, None),
        };
        PackedTTEntry {
            eval,
            depth,
            promo,
            node_type,
            data,
        }
    }
    pub fn eval(&self) -> Evaluation {
        self.eval
    }

    pub fn depth(&self) -> u16 {
        self.depth
    }
    pub fn best_move(&self) -> Option<AlgebraicMove> {
        let data = self.data?.get();
        let from_file = unsafe { std::mem::transmute::<u8, File>((data & 0xf) as u8) };
        let from_rank = unsafe { std::mem::transmute::<u8, Rank>(((data >> 4) & 0xf) as u8) };
        let from = Posn::from(from_rank, from_file);

        let to_file = unsafe { std::mem::transmute::<u8, File>(((data >> 8) & 0xf) as u8) };
        let to_rank = unsafe { std::mem::transmute::<u8, Rank>(((data >> 12) & 0xf) as u8) };
        let to = Posn::from(to_rank, to_file);

        Some(AlgebraicMove {
            to,
            from,
            promotion: self.promo,
        })
    }

    pub fn node_type(&self) -> ScoreType {
        self.node_type
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CacheResult {
    // We have already computed this exact position at a depth equal to or greater than required.
    // We know it's exact value, or have computed that it's definitely a cut off.
    //
    // In this case, we can return the cached value immediately.
    Cutoff(Option<AlgebraicMove>, Evaluation),
    // We've already computed this position, but it's not to a deep enough depth, or the evaluation
    // is within the alpha beta window.
    //
    // In this case, the move is probably a good starting point and will hopefully cause lots of
    // cut offs in the sibling nodes.
    HashMove(Option<AlgebraicMove>),
    // We don't have this position in the cache.
    Miss,
}

// 256MB with 16 bytes per entry
pub const DEFAULT_TT_SIZE: usize = 256 * 1024 * 1024 / 16;
pub struct TranspositionTable(SharedHashMap);
impl Default for TranspositionTable {
    fn default() -> Self {
        Self::new()
    }
}

impl TranspositionTable {
    pub fn new() -> TranspositionTable {
        TranspositionTable(SharedHashMap::new())
    }
    pub fn clear(&self) {
        self.0.clear();
    }
    pub fn get(
        &self,
        hash: u64,
        alpha: Evaluation,
        beta: Evaluation,
        target_depth: u16,
    ) -> CacheResult {
        if let Some(entry) = self.0.get(hash) {
            // We can use this cache entry if:
            // - The node is deep enough
            // - The entry is exact, or the upper bound <= alpha and lowerbound >= beta
            let node_type = entry.node_type();
            let eval = entry.eval();

            if entry.depth() >= target_depth && node_type == ScoreType::Exact {
                tracing::event!(Level::INFO, name = "Exact Cutoff",);
                CacheResult::Cutoff(entry.best_move(), eval)
            } else if entry.depth() >= target_depth && node_type == ScoreType::Upper && eval < alpha
            {
                tracing::event!(Level::INFO, name = "Upperbound Cutoff",);
                CacheResult::Cutoff(entry.best_move(), eval)
            } else if entry.depth() >= target_depth && node_type == ScoreType::Lower && eval > beta
            {
                tracing::event!(Level::INFO, name = "Lowerbound Cutoff",);
                CacheResult::Cutoff(entry.best_move(), eval)
            } else {
                // If not, if the entry has a best move, start with it and hope that it gives us a nice
                // alpha to start with that should cause lots of cut offs.
                tracing::event!(Level::INFO, name = "Cache Hash",);
                CacheResult::HashMove(entry.best_move())
            }
        } else {
            tracing::event!(Level::INFO, name = "Cache Miss",);
            CacheResult::Miss
        }
    }

    pub fn insert(
        &self,
        hash: u64,
        eval: Evaluation,
        best_move: Option<AlgebraicMove>,
        target_depth: u16,
        node_type: ScoreType,
    ) {
        tracing::event!(Level::INFO, name = "inserting", eval = eval.0, hash = hash, node_type=?ScoreType::Upper);
        self.0.insert(
            hash,
            PackedTTEntry::new(eval, target_depth, best_move, node_type),
        );
    }
}

#[cfg(test)]
mod tests {
    use bitboard::{moves::Piece, posn::*};
    use std::sync::atomic::AtomicBool;

    use crate::board::search::ExpectedNodeType;

    use super::*;
    use super::{PackedTTEntry, SharedHashMap};

    #[test]
    fn insert_get() {
        let map: SharedHashMap = SharedHashMap::new();
        for i in 0..1024 {
            let val = map.get(i);
            assert_eq!(val, None);
        }
        for i in 2..1026 {
            map.insert(
                i,
                PackedTTEntry {
                    eval: Evaluation(i as i16),
                    ..Default::default()
                },
            );
        }
        for i in 2..1026 {
            let val = map.get(i);
            assert_eq!(val.unwrap().eval(), Evaluation(i as i16));
        }
    }

    #[test]
    fn insert_overwrite_same_key() {
        // We expect inserts of the same key to automatically update the one value
        let map = SharedHashMap::new();
        for i in 0..10 {
            map.insert(
                255,
                PackedTTEntry {
                    eval: Evaluation(i as i16),
                    depth: i,
                    ..Default::default()
                },
            );
        }
        let Some(val) = map.get(255) else {
            assert!(false);
            return;
        };
        assert_eq!(val.depth(), 9);
        assert_eq!(val.eval(), Evaluation(9 as i16));
    }

    #[test]
    fn insert_overwrite_least_depth() {
        let map = SharedHashMap::new();
        for i in 0..DEFAULT_TT_SIZE {
            map.insert(
                i as u64,
                PackedTTEntry {
                    eval: Evaluation(i as i16),
                    depth: 2,
                    ..Default::default()
                },
            );
        }
        // We expect inserts of the a different key to overwrite something of lesser depth
        map.insert(
            255,
            PackedTTEntry {
                eval: Evaluation(123 as i16),
                depth: 3,
                ..Default::default()
            },
        );

        let Some(val) = map.get(255) else {
            assert!(false);
            return;
        };

        assert_eq!(val.depth(), 3);
        assert_eq!(val.eval(), Evaluation(123 as i16));
    }

    #[test]
    fn concurrent() {
        let map = SharedHashMap::new();
        rayon::scope(|s| {
            s.spawn(|_| {
                for i in 10000..20000 {
                    map.insert(
                        i,
                        PackedTTEntry {
                            eval: Evaluation(i as i16),
                            ..Default::default()
                        },
                    );
                }
            });
            s.spawn(|_| {
                for i in 20000..30000 {
                    map.insert(
                        i,
                        PackedTTEntry {
                            eval: Evaluation(i as i16),
                            ..Default::default()
                        },
                    );
                }
            });
            s.spawn(|_| {
                for i in 30000..40000 {
                    map.insert(
                        i,
                        PackedTTEntry {
                            eval: Evaluation(i as i16),
                            ..Default::default()
                        },
                    );
                }
            });
        });
        let mut inserted_vals = 0;
        for i in 0..40000 {
            if let Some(val) = map.get(i) {
                inserted_vals += 1;
                assert_eq!(val.eval.0, i as i16);
            }
        }
        assert_eq!(inserted_vals, 30000);
    }

    #[test]
    fn pack_unpack() {
        let eval = Evaluation(124);
        let depth = 8;
        let m = Some(AlgebraicMove {
            from: e2(),
            to: e4(),
            promotion: None,
        });
        let node_type = ScoreType::Exact;
        let entry = PackedTTEntry::new(eval, depth, m, node_type);

        assert_eq!(eval, entry.eval);
        assert_eq!(depth, entry.depth);
        assert_eq!(m, entry.best_move());
        assert_eq!(node_type, entry.node_type());
    }

    #[test]
    fn check_packed_tt_entry() {
        {
            let eval = Evaluation::m1(32);
            let depth = 0x123;
            let best_move = AlgebraicMove {
                from: b6(),
                to: e2(),
                promotion: Some(Piece::Queen),
            };
            let node_type = ScoreType::Exact;
            let tt = PackedTTEntry::new(eval, depth, Some(best_move), node_type);
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), Some(best_move));
            assert_eq!(tt.node_type(), node_type);
        }
        {
            let eval = -Evaluation::m1(32);
            let depth = 0x456;
            let best_move = AlgebraicMove {
                from: h1(),
                to: a8(),
                promotion: None,
            };
            let node_type = ScoreType::Upper;
            let tt = PackedTTEntry::new(eval, depth, Some(best_move), node_type);
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), Some(best_move));
            assert_eq!(tt.node_type(), node_type);
        }
        {
            let eval = Evaluation(31);
            let depth = 0x456;
            let best_move = None;
            let node_type = ScoreType::Upper;
            let tt = PackedTTEntry::new(eval, depth, best_move, node_type);
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), best_move);
            assert_eq!(tt.node_type(), node_type);
        }
    }

    #[test]
    fn expected_caching() {
        let mut board = crate::board::starting_board();
        let cache = TranspositionTable::new();
        let should_stop = AtomicBool::new(false);
        board.pvs(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
            board.half_move,
            2,
            &cache,
            &should_stop,
            ExpectedNodeType::PV,
        );

        let moves = board.generate_pseudo_legal_moves();
        for m in moves {
            let m = board.from_algeabraic_unchecked(&m);
            board.make_move(&m);
            let cache_result = cache.get(board.hash, Evaluation(-1), Evaluation(1), 1);
            // For each result, we should have a cache entry, and it should have a move associated
            // with it.
            match cache_result {
                CacheResult::Cutoff(algebraic_move, _) => assert!(algebraic_move.is_some()),
                CacheResult::HashMove(algebraic_move) => assert!(algebraic_move.is_some()),
                CacheResult::Miss => assert_ne!(cache_result, CacheResult::Miss),
            }
            board.undo_move(&m);
        }
    }
}
