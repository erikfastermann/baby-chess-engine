use std::fmt;

use crate::{bitset::{Bitset, ROW_0, ROW_7}, position};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Color {
    White = 0,
    Black = 1,
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Color {
    pub fn to_usize(self) -> usize {
        self as usize
    }

    pub fn other(self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }

    pub fn castle_left(self) -> Castle {
        match self {
            Color::White => Castle {
                king_from: position::WHITE_KING_STARTING_INDEX,
                king_to: position::WHITE_KING_STARTING_INDEX - 2,
                rook_from: position::WHITE_ROOK_LEFT_STARTING_INDEX,
                rook_to: position::WHITE_ROOK_LEFT_STARTING_INDEX + 3,
            },
            Color::Black => Castle {
                king_from: position::BLACK_KING_STARTING_INDEX,
                king_to: position::BLACK_KING_STARTING_INDEX - 2,
                rook_from: position::BLACK_ROOK_LEFT_STARTING_INDEX,
                rook_to: position::BLACK_ROOK_LEFT_STARTING_INDEX + 3,
            },
        }
    }

    pub fn castle_right(self) -> Castle {
        match self {
            Color::White => Castle {
                king_from: position::WHITE_KING_STARTING_INDEX,
                king_to: position::WHITE_KING_STARTING_INDEX + 2,
                rook_from: position::WHITE_ROOK_RIGHT_STARTING_INDEX,
                rook_to: position::WHITE_ROOK_RIGHT_STARTING_INDEX - 2,
            },
            Color::Black => Castle {
                king_from: position::BLACK_KING_STARTING_INDEX,
                king_to: position::BLACK_KING_STARTING_INDEX + 2,
                rook_from: position::BLACK_ROOK_RIGHT_STARTING_INDEX,
                rook_to: position::BLACK_ROOK_RIGHT_STARTING_INDEX - 2,
            },
        }
    }

    pub fn first_row_bitset(self) -> Bitset {
        match self {
            Color::White => ROW_7,
            Color::Black => ROW_0,
        }
    }

    pub fn en_passant_row(self) -> u8 {
        self.nth_row(3)
    }

    pub fn nth_row(self, index: u8) -> u8 {
        assert!(index < 8);
        match self {
            Color::White => 7 - index,
            Color::Black => index,
        }
    }
}

pub struct Castle {
    pub king_from: u8,
    pub king_to: u8,
    pub rook_from: u8,
    pub rook_to: u8,
}
