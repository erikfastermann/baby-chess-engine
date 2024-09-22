use std::fmt;

use crate::{color::{Castle, Color}, piece::Piece, position::{self, index_to_position, position_to_index}, result::Result};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum UserMove {
    Normal { from: u8, to: u8 },
    Promotion { piece: Piece, from: u8, to: u8 },
}

impl fmt::Display for UserMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_long_algebraic_notation())
    }
}

impl fmt::Debug for UserMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl UserMove {
    pub fn from_long_algebraic_notation(notation: &str) -> Result<Self> {
        if notation.len() != 4 && notation.len() != 5 {
            return Err(format!("expected chess notation, got '{}'", notation).into());
        }
        let notation = notation.as_bytes();
        let from = position::human_position_to_index(&notation[..2])?;
        let to = position::human_position_to_index(&notation[2..4])?;
        if notation.len() == 5 {
            let symbol = char::from(notation[4]);
            let promote_to = Piece::from_symbol(symbol)?;
            if !promote_to.can_promote_to() {
                return Err(format!("cannot promote to {:?}", promote_to).into());
            }
            Ok(Self::Promotion { piece: promote_to, from, to })
        } else {
            Ok(Self::Normal { from, to })            
        }
    }

    pub fn to_long_algebraic_notation(&self) -> String {
        match *self {
            UserMove::Normal { from, to } => Self::long_algebraic_notation_normal(from, to),
            UserMove::Promotion { piece, from, to } => {
                assert!(piece.can_promote_to());
                let mut s = Self::long_algebraic_notation_normal(from, to);
                s.push(piece.to_symbol());
                s
            },
        }
    }

    fn long_algebraic_notation_normal(from_index: u8, to_index: u8) -> String {
        let (from_x, from_y) = crate::fmt::index_to_chess_position(from_index);
        let (to_x, to_y) = crate::fmt::index_to_chess_position(to_index);
        format!("{from_x}{from_y}{to_x}{to_y}")
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Move(u32);

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:?}: moved={:?} move={}",
            self.kind(),
            self.piece(),
            self.to_user_move().to_long_algebraic_notation(),
        )
    }
}

#[derive(Debug)]
pub enum MoveKind {
    NonCapture = 0,
    Capture = 1,
    EnPassant = 2,
    Castle = 3,
    Promotion = 4,
    PromotionCapture = 5,
    PawnDouble = 6,
}

impl Move {
    const PIECE_OFFSET: u32 = 3;
    const FROM_OFFSET: u32 = Self::PIECE_OFFSET + 3;
    const TO_OFFSET: u32 = Self::FROM_OFFSET + 6;
    const PROMOTION_PIECE_OFFSET: u32 = Self::TO_OFFSET + 6;

    pub const UNINITIALIZED: Self = Self(0);

    fn new(kind: MoveKind, piece: Piece, from: u8, to: u8, promotion_piece: Piece) -> Self {
        let n = kind as u32
            | piece.to_u32() << Self::PIECE_OFFSET
            | u32::from(from) << Self::FROM_OFFSET
            | u32::from(to) << Self::TO_OFFSET
            | promotion_piece.to_u32() << Self::PROMOTION_PIECE_OFFSET;
        Self(n)
    }

    pub fn non_capture(piece: Piece, from: u8, to: u8) -> Self {
        Self::new(MoveKind::NonCapture, piece, from, to, Piece::None)
    }

    pub fn capture(piece: Piece, from: u8, to: u8) -> Self {
        Self::new(MoveKind::Capture, piece, from, to, Piece::None)
    }

    pub fn promotion(from: u8, to: u8, promotion_piece: Piece) -> Self {
        Self::new(MoveKind::Promotion, Piece::Pawn, from, to, promotion_piece)
    }

    pub fn promotion_capture(from: u8, to: u8, promotion_piece: Piece) -> Self {
        Self::new(MoveKind::PromotionCapture, Piece::Pawn, from, to, promotion_piece)
    }

    pub fn pawn_double(from: u8, to: u8) -> Self {
        Self::new(MoveKind::PawnDouble, Piece::Pawn, from, to, Piece::None)
    }

    pub fn castle(castle: Castle) -> Self {
        Self::new(
            MoveKind::Castle,
            Piece::King,
            castle.king_from,
            castle.king_to,
            Piece::None,
        )
    }

    fn en_passant(from: u8, to: u8) -> Self {
        Self::new(MoveKind::EnPassant, Piece::Pawn, from, to, Piece::None)
    }

    pub fn en_passant_left(color: Color, en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        debug_assert!(y == 3 || y == 4);
        if x == 0 {
            None
        } else {
            let next_y = match color {
                Color::White => y - 1,
                Color::Black => y + 1,
            };
            Some(Self::en_passant(
                position_to_index(x-1, y),
                position_to_index(x, next_y),
            ))
        }
    }

    pub fn en_passant_right(color: Color, en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 7 {
            None
        } else {
            let next_y = match color {
                Color::White => y - 1,
                Color::Black => y + 1,
            };
            Some(Self::en_passant(
                position_to_index(x+1, y),
                position_to_index(x, next_y),
            ))
        }
    }

    pub fn kind(self) -> MoveKind {
        match self.0 & 0b111 {
            0 => MoveKind::NonCapture,
            1 => MoveKind::Capture,
            2 => MoveKind::EnPassant,
            3 => MoveKind::Castle,
            4 => MoveKind::Promotion,
            5 => MoveKind::PromotionCapture,
            6 => MoveKind::PawnDouble,
            // Extra case so the compiler does not generate unnecessary branch.
            7 => MoveKind::NonCapture,
            _ => unreachable!(),
        }
    }

    pub fn piece(self) -> Piece {
        Piece::from_u32_fast((self.0 >> Self::PIECE_OFFSET) & 0b111)
    }

    pub fn from(self) -> u8 {
        ((self.0 >> Self::FROM_OFFSET) & 0b11_1111) as u8
    }

    pub fn to(self) -> u8 {
        ((self.0 >> Self::TO_OFFSET) & 0b11_1111) as u8
    }

    pub fn promotion_piece(self) -> Piece {
        Piece::from_u32_fast((self.0 >> Self::PROMOTION_PIECE_OFFSET) & 0b111)
    }

    pub fn is_promotion(self) -> bool {
        matches!(self.kind(), MoveKind::Promotion|MoveKind::PromotionCapture)
    }

    pub fn is_capture(self) -> bool {
        matches!(
            self.kind(),
            MoveKind::Capture
                | MoveKind::PromotionCapture
                | MoveKind::EnPassant,
        )
    }

    pub fn to_user_move(self) -> UserMove {
        if self.is_promotion() {
            UserMove::Promotion {
                piece: self.promotion_piece(),
                from: self.from(),
                to: self.to(),
            }
        } else {
            UserMove::Normal {
                from: self.from(),
                to: self.to(),
            }
        }
    }

    pub fn as_castle(self) -> Option<Castle> {
        if self == Move::castle(Color::White.castle_left()) {
            Some(Color::White.castle_left())
        } else if self == Move::castle(Color::White.castle_right()) {
            Some(Color::White.castle_right())
        } else if self == Move::castle(Color::Black.castle_left()) {
            Some(Color::Black.castle_left())
        } else if self == Move::castle(Color::Black.castle_right()) {
            Some(Color::Black.castle_right())
        } else {
            None
        }
    }
}
