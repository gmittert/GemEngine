use std::num::NonZero;

use crate::shared_hashmap::Encodable;
use crate::{board::evaluation::Evaluation, shared_hashmap::SharedHashMap};
use bitboard::moves::{AlgebraicMove, Piece};
use bitboard::posn::Posn;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy)]
pub enum NodeType {
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
    node_type: NodeType,
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
        node_type: NodeType,
    ) -> PackedTTEntry {
        let (data, promo) = match best_move {
            Some(AlgebraicMove{ from, to, promotion }) => {
                let mut data: u16 = 0;
                data |= from.file() as u16;
                data |= (from.rank() as u16) << 4;
                data |= (to.file() as u16) << 8;
                data |= (to.rank() as u16) << 12;
                (Some(NonZero::new(data).unwrap()), promotion)
            }
            None => (None, None)
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
        let from_file = unsafe { std::mem::transmute(((data >> 0) & 0xf) as u8) };
        let from_rank = unsafe { std::mem::transmute(((data >> 4) & 0xf) as u8) };
        let from = Posn::from(from_rank, from_file);

        let to_file = unsafe { std::mem::transmute(((data >> 8) & 0xf) as u8) };
        let to_rank = unsafe { std::mem::transmute(((data >> 12) & 0xf) as u8) };
        let to = Posn::from(to_rank, to_file);

        Some(AlgebraicMove {
            to,
            from,
            promotion: self.promo,
        })
    }

    pub fn node_type(&self) -> NodeType {
        self.node_type
    }
}

// 256MB with 16 bytes per entry
pub type TranspositionTable = SharedHashMap<PackedTTEntry, { 256 * 1024 * 1024 / 16 }>;

#[cfg(test)]
mod tests {
    use bitboard::{moves::Piece, posn::*};

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
        let node_type = NodeType::Exact;
        let entry = PackedTTEntry::new(eval, depth, m, node_type);

        assert_eq!(eval, entry.eval);
        assert_eq!(depth, entry.depth);
        assert_eq!(m, entry.best_move());
        assert_eq!(node_type, entry.node_type());
    }

    #[test]
    fn check_packed_tt_entry() {
        {
            let eval = Evaluation::m1();
            let depth = 0x123;
            let best_move = AlgebraicMove {
                from: b6(),
                to: e2(),
                promotion: Some(Piece::Queen),
            };
            let node_type = NodeType::Exact;
            let tt = PackedTTEntry::new(eval, depth, Some(best_move), node_type);
            println!("{:x}", tt.to_u64());
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), Some(best_move));
            assert_eq!(tt.node_type(), node_type);
        }
        {
            let eval = -Evaluation::m1();
            let depth = 0x456;
            let best_move = AlgebraicMove {
                from: h1(),
                to: a8(),
                promotion: None,
            };
            let node_type = NodeType::Upper;
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
            let node_type = NodeType::Upper;
            let tt = PackedTTEntry::new(eval, depth, best_move, node_type);
            println!("{:x}", tt.to_u64());
            assert_eq!(tt.eval(), eval);
            assert_eq!(tt.depth(), depth);
            assert_eq!(tt.best_move(), best_move);
            assert_eq!(tt.node_type(), node_type);
        }
    }
}
