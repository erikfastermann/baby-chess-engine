use crate::{mov::Move, position};

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

    pub fn direction(&self) -> i8 {
        match self {
            Color::White => -1,
            Color::Black => 1,
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

    pub fn en_passant_left(&self, en_passant_index: u8) -> Option<Move> {
        match self {
            Color::White => Move::white_en_passant_left(en_passant_index),
            Color::Black => Move::black_en_passant_left(en_passant_index),
        }
    }

    pub fn en_passant_right(&self, en_passant_index: u8) -> Option<Move> {
        match self {
            Color::White => Move::white_en_passant_right(en_passant_index),
            Color::Black => Move::black_en_passant_right(en_passant_index),
        }
    }
}