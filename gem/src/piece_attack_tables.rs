use bitboard::{posn::Posn, BitBoard};

pub static KNIGHT_ATTACKS: [BitBoard; 64] = {
    let mut arr = [BitBoard::empty(); 64];
    let mut i = 0;
    while i < 64 {
        let knight = Posn::from_idx(i).unwrap();
        let see = if let Some(p) = knight.see() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let sse = if let Some(p) = knight.sse() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let sww = if let Some(p) = knight.sww() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let ssw = if let Some(p) = knight.ssw() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let nee = if let Some(p) = knight.nee() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let nne = if let Some(p) = knight.nne() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let nww = if let Some(p) = knight.nww() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let nnw = if let Some(p) = knight.nnw() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };

        arr[i] = BitBoard(see.0 | sse.0 | sww.0 | ssw.0 | nee.0 | nne.0 | nww.0 | nnw.0);
        i += 1;
    }
    arr
};

pub static KING_ATTACKS: [BitBoard; 64] = {
    let mut arr = [BitBoard::empty(); 64];
    let mut i = 0;
    while i < 64 {
        let king = Posn::from_idx(i).unwrap();
        let no = if let Some(p) = king.no() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let nw = if let Some(p) = king.nw() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let we = if let Some(p) = king.we() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let sw = if let Some(p) = king.sw() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let so = if let Some(p) = king.so() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let se = if let Some(p) = king.se() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let ea = if let Some(p) = king.ea() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };
        let ne = if let Some(p) = king.ne() {
            BitBoard::from(p)
        } else {
            BitBoard::empty()
        };

        arr[i] = BitBoard(no.0 | nw.0 | we.0 | sw.0 | so.0 | se.0 | ea.0 | ne.0);
        i += 1;
    }
    arr
};
