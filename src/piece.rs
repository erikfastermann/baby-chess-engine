use crate::result::Result;

pub static PROMOTION_PIECES: [Piece; 4] = [
    Piece::Queen,
    Piece::Rook,
    Piece::Knight,
    Piece::Bishop,
];

pub static STARTING_EMPTY_SQUARES: [Piece; 48] = [Piece::None; 48];

pub static STARTING_PAWNS: [Piece; 8] = [Piece::Pawn; 8];

pub static STARTING_PIECES_FIRST_RANK: [Piece; 8] = {
    use Piece::*;
    [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
};

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Piece {
    None = 0,
    King = 1,
    Queen = 2,
    Rook = 3,
    Bishop = 4,
    Knight = 5,
    Pawn = 6,
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
            Piece::None => '.',
        }
    }

    pub fn to_usize(self) -> usize {
        self as usize
    }

    pub fn can_promote_to(&self) -> bool {
        PROMOTION_PIECES.contains(self)
    }
}
