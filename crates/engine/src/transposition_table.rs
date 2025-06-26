use std::num::NonZero;

use crate::shared_hashmap::Encodable;
use crate::{board::evaluation::Evaluation, shared_hashmap::SharedHashMap};
use bitboard::moves::{AlgebraicMove, Piece};
use bitboard::posn::{File, Posn, Rank};
use tracing::Level;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy)]
pub enum ScoreType {
    Upper,
    Lower,
    Exact,
}

// Data Layout:
//    0..4 From file
//    4..8 From rank
//    8..12 To file
//    12..16 To rank
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PackedTTEntry {
    eval: Evaluation,
    depth: u16,
    promo: Option<Piece>,
    node_type: ScoreType,
    data: Option<NonZero<u16>>,
}

impl Encodable for PackedTTEntry {
    fn from_u64(v: u64) -> Self {
        unsafe { std::mem::transmute(v) }
    }

    fn to_u64(&self) -> u64 {
        unsafe { std::mem::transmute(*self) }
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
pub struct TranspositionTable<const N: usize>(SharedHashMap<PackedTTEntry, N>);
impl<const N: usize> Default for TranspositionTable<N> {
    fn default() -> Self {
        Self::new()
    }
}

impl<const N: usize> TranspositionTable<N> {
    pub fn new() -> TranspositionTable<N> {
        TranspositionTable::<N>(SharedHashMap::new())
    }
    pub fn hash_usage(&self) -> usize {
        self.0.hash_usage()
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

    pub fn update(
        &self,
        hash: u64,
        eval: Evaluation,
        best_move: Option<AlgebraicMove>,
        depth: u16,
        node_type: ScoreType,
    ) {
        let mut expected = self.0.get(hash).unwrap();
        while depth > expected.depth() {
            if let Err(v) = self.0.update(
                hash,
                expected,
                PackedTTEntry::new(eval, depth, best_move, node_type),
            ) {
                expected = v;
                continue;
            }
            break;
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
            println!("{:x}", tt.to_u64());
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
            println!("{:x}", tt.to_u64());
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
            println!("{:x}", tt.to_u64());
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), best_move);
            assert_eq!(tt.node_type(), node_type);
        }
    }

    #[test]
    fn expected_caching() {
        let mut board = crate::board::starting_board();
        let cache = TranspositionTable::<DEFAULT_TT_SIZE>::new();
        let should_stop = AtomicBool::new(false);
        board.pvs(
            Evaluation::lost(board.half_move),
            Evaluation::won(board.half_move),
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
