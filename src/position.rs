pub const WHITE_KING_STARTING_INDEX: u8 = 7*8 + 4;
pub const WHITE_ROOK_LEFT_STARTING_INDEX: u8 = 7*8;
pub const WHITE_ROOK_RIGHT_STARTING_INDEX: u8 = 7*8 + 7;

pub const BLACK_KING_STARTING_INDEX: u8 = 4;
pub const BLACK_ROOK_LEFT_STARTING_INDEX: u8 = 0;
pub const BLACK_ROOK_RIGHT_STARTING_INDEX: u8 = 7;

pub fn index_to_position(index: u8) -> (u8, u8) {
    debug_assert!(index < 64);
    (index%8, index/8)
}

pub fn position_to_index(x: u8, y: u8) -> u8 {
    debug_assert!(x < 8);
    debug_assert!(y < 8);
    8*y + x
}
