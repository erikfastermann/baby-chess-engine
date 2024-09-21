use crate::result::Result;

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

pub fn human_position_to_index(raw_position: &[u8]) -> Result<u8> {
    if raw_position.len() != 2 {
        return Err(format!(
            "expected square, got '{}'",
            String::from_utf8_lossy(raw_position),
        ).into());
    }
    let (raw_x, raw_y) = (raw_position[0], raw_position[1]);
    if !(b'a'..=b'h').contains(&raw_x) {
        return Err(format!("expected file, got '{}'", char::from(raw_x)).into());
    }
    if !(b'1'..=b'8').contains(&raw_y) {
        return Err(format!("expected rank, got '{}'", char::from(raw_y)).into());
    }
    let (x, y) = (raw_x - b'a', 7 - (raw_y - b'1'));
    Ok(position_to_index(x, y))
}
