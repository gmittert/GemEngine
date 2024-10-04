use std::sync::Arc;

use crate::{board::{evaluation::Evaluation, AlgebraicMove, Posn}, shared_hashmap::SharedHashMap};

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy)]
pub enum NodeType {
    Upper,
    Lower,
    Exact,
}

// Layout:
//     0..15 Evaluation
//    16..31 Depth
//    32..35 From file
//    36..39 From rank
//    40..43 To file
//    44..47 To rank
//    48..55 promotion
//    56..56 has best move
//    57..58 Node Type
//    59..63 5 bits unused
pub struct PackedTTEntry(pub u64);
impl PackedTTEntry {
    pub fn new(
        eval: Evaluation,
        depth: u16,
        best_move: Option<AlgebraicMove>,
        node_type: NodeType,
    ) -> PackedTTEntry {
        let mut data = 0;
        data |= (eval.0 as u16) as u64;
        data |= (depth as u64) << 16;
        if let Some(m) = best_move {
            data |= (m.from.file() as u64) << 32;
            data |= (m.from.rank() as u64) << 36;
            data |= (m.to.file() as u64) << 40;
            data |= (m.to.rank() as u64) << 44;
            data |= (m.promotion.map_or(7, |p| p as u64)) << 48;
            data |= 1 << 56;
        }
        data |= (node_type as u64) << 57;

        PackedTTEntry(data)
    }
    pub fn eval(&self) -> Evaluation {
        Evaluation(self.0 as i16)
    }

    pub fn depth(&self) -> u16 {
        (self.0 >> 16) as u16
    }
    pub fn best_move(&self) -> Option<AlgebraicMove> {
        let best_move_bit = (self.0 >> 56) & 0x1;
        if best_move_bit == 0 {
            return None;
        }
        let from_rank = unsafe { std::mem::transmute(((self.0 >> 36) & 0xf) as u8) };
        let from_file = unsafe { std::mem::transmute(((self.0 >> 32) & 0xf) as u8) };
        let from = Posn::from(from_rank, from_file);

        let to_rank = unsafe { std::mem::transmute(((self.0 >> 44) & 0xf) as u8) };
        let to_file = unsafe { std::mem::transmute(((self.0 >> 40) & 0xf) as u8) };
        let to = Posn::from(to_rank, to_file);

        let promo_bits: u8 = (self.0 >> 48) as u8;
        let promotion = if promo_bits == 7 {
            None
        } else {
            Some(unsafe { std::mem::transmute(promo_bits) })
        };
        Some(AlgebraicMove {
            to,
            from,
            promotion,
        })
    }

    pub fn node_type(&self) -> NodeType {
        let bits: u8 = (self.0 >> 57) as u8;
        unsafe { std::mem::transmute(bits) }
    }
}

pub type TranspositionTable = Arc<SharedHashMap<{1024 * 1024}>>;
