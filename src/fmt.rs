use std::fmt;

use crate::bitset::Bitset;

pub fn fmt_pieces(board: &mut [u8; 64], pieces: Bitset, ch: char) {
    for index in pieces.indices() {
        assert_eq!(board[index as usize], b'.');
        board[index as usize] = u8::try_from(ch).unwrap();
    }
}

pub fn fmt_board(board: &[u8; 64], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let board_str = std::str::from_utf8(board).unwrap();
    for i in (8..=64).step_by(8) {
        write!(f, "{}\n", &board_str[i-8..i])?;
    }
    Ok(())
}
