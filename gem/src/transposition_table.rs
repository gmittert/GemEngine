use crate::shared_hashmap::Encodable;
use crate::{board::evaluation::Evaluation, shared_hashmap::SharedHashMap};
use bitboard::moves::AlgebraicMove;
use bitboard::posn::Posn;

#[derive(PartialEq, Eq, Ord, PartialOrd, Debug, Clone, Copy)]
pub enum NodeType {
    Upper,
    Lower,
    Exact,
}

// Data Layout:
//    32..35 From file
//    36..39 From rank
//    40..43 To file
//    44..47 To rank
//    48..55 promotion
//    56..56 has best move
//    57..58 Node Type
//    59..63 5 bits unused
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct PackedTTEntry {
    eval: Evaluation,
    depth: u16,
    data: u32,
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
        let mut data: u32 = 0;
        if let Some(m) = best_move {
            data |= m.from.file() as u32;
            data |= (m.from.rank() as u32) << 4;
            data |= (m.to.file() as u32) << 8;
            data |= (m.to.rank() as u32) << 12;
            data |= (m.promotion.map_or(7, |p| p as u32)) << 16;
            data |= 1 << 24;
        }
        data |= (node_type as u32) << 25;

        PackedTTEntry { eval, depth, data }
    }
    pub fn eval(&self) -> Evaluation {
        self.eval
    }

    pub fn depth(&self) -> u16 {
        self.depth
    }
    pub fn best_move(&self) -> Option<AlgebraicMove> {
        let best_move_bit = (self.data >> 24) & 0x1;
        if best_move_bit == 0 {
            return None;
        }
        let from_file = unsafe { std::mem::transmute(((self.data >> 0) & 0xf) as u8) };
        let from_rank = unsafe { std::mem::transmute(((self.data >> 4) & 0xf) as u8) };
        let from = Posn::from(from_rank, from_file);

        let to_file = unsafe { std::mem::transmute(((self.data >> 8) & 0xf) as u8) };
        let to_rank = unsafe { std::mem::transmute(((self.data >> 12) & 0xf) as u8) };
        let to = Posn::from(to_rank, to_file);

        let promo_bits: u8 = (self.data >> 16) as u8;
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
        let bits: u8 = (self.data >> 25) as u8;
        unsafe { std::mem::transmute(bits) }
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
