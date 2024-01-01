use crate::position;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub fn other(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }

    pub fn king_starting_index(&self) -> u8 {
        match self {
            Color::White => position::WHITE_KING_STARTING_INDEX,
            Color::Black => position::BLACK_KING_STARTING_INDEX,
        }
    }

    pub fn first_row(&self) -> u8 {
        match self {
            Color::White => 7,
            Color::Black => 0,
        }
    }

    pub fn rook_left_starting_index(&self) -> u8 {
        match self {
            Color::White => position::WHITE_ROOK_LEFT_STARTING_INDEX,
            Color::Black => position::BLACK_ROOK_LEFT_STARTING_INDEX,
        }
    }

    pub fn rook_right_starting_index(&self) -> u8 {
        match self {
            Color::White => position::WHITE_ROOK_RIGHT_STARTING_INDEX,
            Color::Black => position::BLACK_ROOK_RIGHT_STARTING_INDEX,
        }
    }
}
