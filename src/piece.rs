use crate::result::Result;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Piece {
    Pawn,
    Bishop,
    Knight,
    Rook,
    Queen,
    King,
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

    pub fn can_promote_to(&self) -> bool {
        matches!(*self, Piece::Queen | Piece::Rook | Piece::Knight | Piece::Bishop)
    }
}
