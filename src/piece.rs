use crate::result::Result;

pub static PROMOTION_PIECES: [Piece; 4] = [
    Piece::Queen,
    Piece::Rook,
    Piece::Knight,
    Piece::Bishop,
];

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Piece {
    King = 0,
    Queen = 1,
    Rook = 2,
    Bishop = 3,
    Knight = 4,
    Pawn = 5,
}

impl Piece {
    pub fn from_symbol(symbol: char) -> Result<Self> {
        match symbol {
            'p' => Ok(Piece::Pawn),
            'b' => Ok(Piece::Bishop),
            'n' => Ok(Piece::Knight),
            'r' => Ok(Piece::Rook),
            'q' => Ok(Piece::Queen),
            'k' => Ok(Piece::King),
            _ => Err(format!("unknown symbol '{}'", char::from(symbol)).into()),
        }
    }

    pub fn to_symbol(self) -> char {
        match self {
            Piece::Pawn => 'p',
            Piece::Bishop => 'b',
            Piece::Knight => 'n',
            Piece::Rook => 'r',
            Piece::Queen => 'q',
            Piece::King => 'k',
        }
    }

    pub fn to_usize(self) -> usize {
        self as usize
    }

    pub fn can_promote_to(&self) -> bool {
        PROMOTION_PIECES.contains(self)
    }
}
