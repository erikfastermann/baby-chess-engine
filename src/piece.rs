use crate::{result::Result, config};

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
        match Self::from_symbol_option(symbol) {
            Some(piece) => Ok(piece),
            _ => Err(format!("unknown symbol '{}'", char::from(symbol)).into()),
        }
    }

    pub fn from_symbol_option(symbol: char) -> Option<Self> {
        match symbol {
            'p' => Some(Piece::Pawn),
            'b' => Some(Piece::Bishop),
            'n' => Some(Piece::Knight),
            'r' => Some(Piece::Rook),
            'q' => Some(Piece::Queen),
            'k' => Some(Piece::King),
            _ => None,
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

    pub fn from_u32_fast(n: u32) -> Self {
        match n & 0b111 {
            0 => Piece::None,
            1 => Piece::King,
            2 => Piece::Queen,
            3 => Piece::Rook,
            4 => Piece::Bishop,
            5 => Piece::Knight,
            6 => Piece::Pawn,
            // Extra case so the compiler does not generate unnecessary branch.
            7 => Piece::None,
            _ => unreachable!(),
        }
    }

    pub fn to_u32(self) -> u32 {
        self as u32
    }

    pub fn to_usize(self) -> usize {
        self as usize
    }

    pub fn can_promote_to(&self) -> bool {
        PROMOTION_PIECES.contains(self)
    }

    pub fn value(&self) -> i32 {
        match self {
            Piece::None => 0,
            Piece::King => config::SCORE_KING,
            Piece::Queen => config::SCORE_QUEEN,
            Piece::Rook => config::SCORE_ROOK,
            Piece::Bishop => config::SCORE_BISHOP,
            Piece::Knight => config::SCORE_KNIGHT,
            Piece::Pawn => config::SCORE_PAWN,
        }
    }
}
