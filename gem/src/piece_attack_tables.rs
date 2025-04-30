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
