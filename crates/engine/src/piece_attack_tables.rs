use bitboard::BitBoard;

pub static KNIGHT_ATTACKS: [BitBoard; 64] = {
    let mut arr = [BitBoard::empty(); 64];
    let mut i = 0;
    while i < 64 {
        let knight = BitBoard(1 << i);
        let see = knight.so().ea().ea();
        let sse = knight.so().so().ea();
        let sww = knight.so().we().we();
        let ssw = knight.so().so().we();

        let nee = knight.no().ea().ea();
        let nne = knight.no().no().ea();
        let nww = knight.no().we().we();
        let nnw = knight.no().no().we();

        arr[i] = BitBoard(see.0 | sse.0 | sww.0 | ssw.0 | nee.0 | nne.0 | nww.0 | nnw.0);
        i += 1;
    }
    arr
};

pub static KING_ATTACKS: [BitBoard; 64] = {
    let mut arr = [BitBoard::empty(); 64];
    let mut i = 0;
    while i < 64 {
        let king = BitBoard(1 << i);
        let no = king.no();
        let nw = king.no().we();
        let we = king.we();
        let sw = king.so().we();
        let so = king.so();
        let se = king.so().ea();
        let ea = king.ea();
        let ne = king.no().ea();

        arr[i] = BitBoard(no.0 | nw.0 | we.0 | sw.0 | so.0 | se.0 | ea.0 | ne.0);
        i += 1;
    }
    arr
};
