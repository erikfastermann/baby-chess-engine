use std::fmt;

use crate::{piece::Piece, position::{index_to_position, position_to_index}, result::Result};

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Move {
    Normal { from: u8, to: u8 },
    Promotion { piece: Piece, from: u8, to: u8 },
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_long_algebraic_notation())
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl Move {
    pub fn white_en_passant_left(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 0 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x-1, y),
                to: position_to_index(x, y-1),
            })
        }
    }

    pub fn white_en_passant_right(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 7 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x+1, y),
                to: position_to_index(x, y-1),
            })
        }
    }

    pub fn black_en_passant_left(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 0 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x-1, y),
                to: position_to_index(x, y+1),
            })
        }
    }

    pub fn black_en_passant_right(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 7 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x+1, y),
                to: position_to_index(x, y+1),
            })
        }
    }

    pub fn from_long_algebraic_notation(notation: &str) -> Result<Self> {
        if notation.len() != 4 && notation.len() != 5 {
            return Err(format!("expected chess notation, got '{}'", notation).into());
        }
        let notation = notation.as_bytes();
        let from = Self::chess_position_to_index(&notation[..2])?;
        let to = Self::chess_position_to_index(&notation[2..4])?;
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
            Move::Normal { from, to } => Self::long_algebraic_notation_normal(from, to),
            Move::Promotion { piece, from, to } => {
                assert!(piece.can_promote_to());
                let mut s = Self::long_algebraic_notation_normal(from, to);
                s.push(piece.to_symbol());
                s
            },
        }
    }

    fn index_to_chess_position(index: u8) -> (char, char) {
        let (x, y) = index_to_position(index);
        ((b'a' + x).into(), char::from_digit((8-y).into(), 10).unwrap())
    }

    #[allow(dead_code)]
    fn fmt_index(index: u8) -> String {
        let (x, y) = Self::index_to_chess_position(index);
        format!("{x}{y}")
    }

    fn chess_position_to_index(raw_position: &[u8]) -> Result<u8> {
        assert_eq!(raw_position.len(), 2);
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

    fn long_algebraic_notation_normal(from_index: u8, to_index: u8) -> String {
        let (from_x, from_y) = Self::index_to_chess_position(from_index);
        let (to_x, to_y) = Self::index_to_chess_position(to_index);
        format!("{from_x}{from_y}{to_x}{to_y}")
    }
}
