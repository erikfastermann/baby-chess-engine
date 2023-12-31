use std::fmt;

use crate::{piece::Piece, bitset::{Bitset, ROW_0, ROW_7}, position::{self, index_to_position}, color::Color, config, moves::Moves};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Board {
    pub black: PlayerBoard,
    pub white: PlayerBoard,
    pub en_passant_index: Option<u8>,
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = [b'.'; 64];

        crate::fmt::fmt_pieces(&mut board, self.white.king(), Piece::King.to_symbol().to_ascii_uppercase());
        crate::fmt::fmt_pieces(&mut board, self.white.queens(), Piece::Queen.to_symbol().to_ascii_uppercase());
        crate::fmt::fmt_pieces(&mut board, self.white.rooks(), Piece::Rook.to_symbol().to_ascii_uppercase());
        crate::fmt::fmt_pieces(&mut board, self.white.bishops(), Piece::Bishop.to_symbol().to_ascii_uppercase());
        crate::fmt::fmt_pieces(&mut board, self.white.knights(), Piece::Knight.to_symbol().to_ascii_uppercase());
        crate::fmt::fmt_pieces(&mut board, self.white.pawns(), Piece::Pawn.to_symbol().to_ascii_uppercase());

        crate::fmt::fmt_pieces(&mut board, self.black.king(), Piece::King.to_symbol().to_ascii_lowercase());
        crate::fmt::fmt_pieces(&mut board, self.black.queens(), Piece::Queen.to_symbol().to_ascii_lowercase());
        crate::fmt::fmt_pieces(&mut board, self.black.rooks(), Piece::Rook.to_symbol().to_ascii_lowercase());
        crate::fmt::fmt_pieces(&mut board, self.black.bishops(), Piece::Bishop.to_symbol().to_ascii_lowercase());
        crate::fmt::fmt_pieces(&mut board, self.black.knights(), Piece::Knight.to_symbol().to_ascii_lowercase());
        crate::fmt::fmt_pieces(&mut board, self.black.pawns(), Piece::Pawn.to_symbol().to_ascii_lowercase());

        crate::fmt::fmt_board(&board, f)
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

// TODO:
// Currently we still calculate castling moves
// even if they are not strictly allowed.
impl Board {
    pub fn start() -> Self {      
        let mut board = Self {
            white: PlayerBoard {
                bitsets: [Bitset::zero(); 6],
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
            },
            black: PlayerBoard {
                bitsets: [Bitset::zero(); 6],
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
            },
            en_passant_index: None,
        };

        *board.white.rooks_mut() = Bitset::from_index(position::WHITE_ROOK_LEFT_STARTING_INDEX)
            | Bitset::from_index(position::WHITE_ROOK_RIGHT_STARTING_INDEX);
        *board.white.knights_mut() = Bitset::from_position(1, 7) | Bitset::from_position(6, 7);
        *board.white.bishops_mut() = Bitset::from_position(2, 7) | Bitset::from_position(5, 7);
        *board.white.queens_mut() = Bitset::from_position(3, 7);
        *board.white.king_mut() = Bitset::from_index(position::WHITE_KING_STARTING_INDEX);
        *board.white.pawns_mut() = (0..8).map(|x| Bitset::from_position(x, 6))
            .fold(Bitset::zero(), |pawns, pawn| pawns | pawn);

        *board.black.rooks_mut() = Bitset::from_index(position::BLACK_ROOK_LEFT_STARTING_INDEX)
            | Bitset::from_index(position::BLACK_ROOK_RIGHT_STARTING_INDEX);
        *board.black.knights_mut() = Bitset::from_position(1, 0) | Bitset::from_position(6, 0);
        *board.black.bishops_mut() = Bitset::from_position(2, 0) | Bitset::from_position(5, 0);
        *board.black.queens_mut() = Bitset::from_position(3, 0);
        *board.black.king_mut() = Bitset::from_index(position::BLACK_KING_STARTING_INDEX);
        *board.black.pawns_mut() = (0..8).map(|x| Bitset::from_position(x, 1))
            .fold(Bitset::zero(), |pawns, pawn| pawns | pawn);

        board.debug_check();
        board
    }

    pub fn bitset(&self) -> Bitset {
        self.white.bitset() | self.black.bitset()
    }

    pub fn debug_check(&self) {
        self.white.debug_check();
        self.black.debug_check();
        debug_assert_eq!(
            self.white.bitset().count() + self.black.bitset().count(),
            self.bitset().count(),
        );
        debug_assert!(self.bitset().count() <= 32);
    }

    fn is_end_game(&self) -> bool {
        if self.white.queens().is_empty() && self.black.queens().is_empty() {
            return true;
        }

        let white_minor_pieces = self.white.knights().count() + self.white.bishops().count();
        let white_major_pieces = self.white.rooks().count() + self.white.queens().count();

        let black_minor_pieces = self.black.knights().count() + self.black.bishops().count();
        let black_major_pieces = self.black.rooks().count() + self.black.queens().count();

        let no_major_pieces = white_major_pieces == 0 && black_major_pieces == 0;
        let max_one_minor_piece = white_minor_pieces <= 1 && black_minor_pieces <= 1;
        if no_major_pieces && max_one_minor_piece {
            true
        } else {
            false
        }
    }

    pub fn score(&self, next_move: Color) -> i32 {
        let is_end_game = self.is_end_game();
        let white = self.white.score(&WHITE_PIECE_SQUARE_TABLES, is_end_game);
        let black = self.black.score(black_piece_square_tables(), is_end_game);
        let score = white - black;
        let perspective = match next_move {
            Color::White => 1,
            Color::Black => -1,
        };
        score * perspective
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerBoard {
    bitsets: [Bitset; 6],
    pub can_castle: CanCastle,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CanCastle {
    // TODO: bitflags
    left: bool,
    right: bool,
}

impl PlayerBoard {
    pub fn pawns(&self) -> Bitset {
        self.bitsets[Piece::Pawn.to_usize()]
    }

    pub fn pawns_mut(&mut self) -> &mut Bitset {
        &mut self.bitsets[Piece::Pawn.to_usize()]
    }

    pub fn bishops(&self) -> Bitset {
        self.bitsets[Piece::Bishop.to_usize()]
    }

    pub fn bishops_mut(&mut self) -> &mut Bitset {
        &mut self.bitsets[Piece::Bishop.to_usize()]
    }

    pub fn knights(&self) -> Bitset {
        self.bitsets[Piece::Knight.to_usize()]
    }

    pub fn knights_mut(&mut self) -> &mut Bitset {
        &mut self.bitsets[Piece::Knight.to_usize()]
    }

    pub fn rooks(&self) -> Bitset {
        self.bitsets[Piece::Rook.to_usize()]
    }

    pub fn rooks_mut(&mut self) -> &mut Bitset {
        &mut self.bitsets[Piece::Rook.to_usize()]
    }

    pub fn queens(&self) -> Bitset {
        self.bitsets[Piece::Queen.to_usize()]
    }

    pub fn queens_mut(&mut self) -> &mut Bitset {
        &mut self.bitsets[Piece::Queen.to_usize()]
    }

    pub fn king(&self) -> Bitset {
        self.bitsets[Piece::King.to_usize()]
    }

    pub fn king_mut(&mut self) -> &mut Bitset {
        &mut self.bitsets[Piece::King.to_usize()]
    }

    fn debug_check(&self) {
        let count = self.pawns().count()
            + self.bishops().count()
            + self.knights().count()
            + self.rooks().count()
            + self.queens().count()
            + self.king().count();
        debug_assert_eq!(count, self.bitset().count());

        debug_assert_eq!((self.pawns() & ROW_0).count(), 0);
        debug_assert_eq!((self.pawns() & ROW_7).count(), 0);

        debug_assert_eq!(self.king().count(), 1);
        debug_assert!(self.pawns().count() <= 8);
        debug_assert!(self.bitset().count() <= 16);
    }

    pub fn bitset(&self) -> Bitset {
        let mut bitset = Bitset::zero();
        for i in 0..self.bitsets.len() {
            bitset |= self.bitsets[i];
        }
        bitset
    }

    pub fn captured(&mut self, index: u8) {
        for i in 0..self.bitsets.len() {
            self.bitsets[i].clear(index);
        }
    }

    fn score(&self, tables: &PieceSquareTables, is_end_game: bool) -> i32 {
        fn position(piece: Bitset, table: &[i32; 64]) -> i32 {
            piece.indices().map(|index| table[index as usize]).sum::<i32>()
        }

        let position = if is_end_game {
            position(self.pawns(), &tables.pawns.end_game)
                + position(self.knights(), &tables.knights.end_game)
                + position(self.bishops(), &tables.bishops.end_game)
                + position(self.rooks(), &tables.rooks.end_game)
                + position(self.queens(), &tables.queens.end_game)
                + position(self.king(), &tables.king.end_game)
        } else {
            position(self.pawns(), &tables.pawns.mid_game)
                + position(self.knights(), &tables.knights.mid_game)
                + position(self.bishops(), &tables.bishops.mid_game)
                + position(self.rooks(), &tables.rooks.mid_game)
                + position(self.queens(), &tables.queens.mid_game)
                + position(self.king(), &tables.king.mid_game)
        };

        let material = self.pawns().count() * config::SCORE_PAWN
            + self.knights().count() * config::SCORE_KNIGHT
            + self.bishops().count() * config::SCORE_BISHOP
            + self.rooks().count() * config::SCORE_ROOK
            + self.queens().count() * config::SCORE_QUEEN
            + self.king().count() * config::SCORE_KING;

        position + material
    }

    pub fn which_piece(&self, index: u8) -> Option<Piece> {
        if self.pawns().has(index) {
            Some(Piece::Pawn)
        } else if self.bishops().has(index) {
            Some(Piece::Bishop)
        } else if self.knights().has(index) {
            Some(Piece::Knight)
        } else if self.rooks().has(index) {
            Some(Piece::Rook)
        } else if self.queens().has(index) {
            Some(Piece::Queen)
        } else if self.king().has(index) {
            Some(Piece::King)
        } else {
            None
        }
    }

    pub fn remove_piece(&mut self, piece: Piece, index: u8) {
        match piece {
            Piece::Pawn => self.pawns_mut().checked_clear(index),
            Piece::Bishop => self.bishops_mut().checked_clear(index),
            Piece::Knight => self.knights_mut().checked_clear(index),
            Piece::Rook => self.rooks_mut().checked_clear(index),
            Piece::Queen => self.queens_mut().checked_clear(index),
            Piece::King => self.king_mut().checked_clear(index),
        }
    }

    pub fn place_piece(&mut self, piece: Piece, index: u8) {
        match piece {
            Piece::Pawn => self.pawns_mut().checked_set(index),
            Piece::Bishop => self.bishops_mut().checked_set(index),
            Piece::Knight => self.knights_mut().checked_set(index),
            Piece::Rook => self.rooks_mut().checked_set(index),
            Piece::Queen => self.queens_mut().checked_set(index),
            Piece::King => self.king_mut().checked_set(index),
        }
    }

    pub fn has_check(&self, enemy: &Self, self_color: Color) -> bool {
        let pawns_without_en_passant = match self_color {
            Color::White => Moves::white_pawns_without_en_passant(self, enemy),
            Color::Black => Moves::black_pawns_without_en_passant(self, enemy),
        };
        Moves::without_pawns_castle(self, enemy)
            .combine(&pawns_without_en_passant)
            .captures
            .overlaps(enemy.king())
    }

    pub fn has_en_passant_left(&self, enemy: &Self, en_passant_index: u8) -> bool {
        assert!(enemy.pawns().has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x == 0 {
            return false;
        }

        let left_neighbour_pawn = Bitset::from_position(x-1, y) & self.pawns();
        !left_neighbour_pawn.is_empty()
    }

    pub fn has_en_passant_right(&self, enemy: &Self, en_passant_index: u8) -> bool {
        assert!(enemy.pawns().has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x == 7 {
            return false;
        }
        let right_neighbour_pawn = Bitset::from_position(x+1, y) & self.pawns();
        !right_neighbour_pawn.is_empty()
    }

    pub fn can_castle_any(&self) -> bool {
        self.can_castle_left() || self.can_castle_right()
    }

    pub fn can_castle_left(&self) -> bool {
        self.can_castle.left
    }

    pub fn can_castle_right(&self) -> bool {
        self.can_castle.right
    }

    pub fn disable_castle(&mut self) {
        self.can_castle.left = false;
        self.can_castle.right = false;
    }

    pub fn disable_castle_left(&mut self) {
        self.can_castle.left = false;
    }

    pub fn disable_castle_right(&mut self) {
        self.can_castle.right = false;
    }

    pub fn reset_castle(&mut self, old: CanCastle) {
        self.can_castle.left = old.left;
        self.can_castle.right = old.right;
    }
}

struct PieceSquareTable {
    mid_game: [i32; 64],
    end_game: [i32; 64],
}

struct PieceSquareTables {
    pawns: PieceSquareTable,
    knights: PieceSquareTable,
    bishops: PieceSquareTable,
    rooks: PieceSquareTable,
    queens: PieceSquareTable,
    king: PieceSquareTable,
}

static WHITE_PIECE_SQUARE_TABLES: PieceSquareTables = PieceSquareTables {
    pawns: PieceSquareTable {
        mid_game: config::SCORE_PAWNS_TABLE,
        end_game: config::SCORE_PAWNS_TABLE,
    },
    knights: PieceSquareTable {
        mid_game: config::SCORE_KNIGHTS_TABLE,
        end_game: config::SCORE_KNIGHTS_TABLE,
    },
    bishops: PieceSquareTable {
        mid_game: config::SCORE_BISHOPS_TABLE,
        end_game: config::SCORE_BISHOPS_TABLE,
    },
    rooks: PieceSquareTable {
        mid_game: config::SCORE_ROOKS_TABLE,
        end_game: config::SCORE_ROOKS_TABLE,
    },
    queens: PieceSquareTable {
        mid_game: config::SCORE_QUEENS_TABLE,
        end_game: config::SCORE_QUEENS_TABLE,
    },
    king: PieceSquareTable {
        mid_game: config::SCORE_KING_TABLE_MIDDLE_GAME,
        end_game: config::SCORE_KING_TABLE_END_GAME,
    },
};

static mut BLACK_PIECE_SQUARE_TABLES: PieceSquareTables = PieceSquareTables {
    pawns: PieceSquareTable {
        mid_game: config::SCORE_PAWNS_TABLE,
        end_game: config::SCORE_PAWNS_TABLE,
    },
    knights: PieceSquareTable {
        mid_game: config::SCORE_KNIGHTS_TABLE,
        end_game: config::SCORE_KNIGHTS_TABLE,
    },
    bishops: PieceSquareTable {
        mid_game: config::SCORE_BISHOPS_TABLE,
        end_game: config::SCORE_BISHOPS_TABLE,
    },
    rooks: PieceSquareTable {
        mid_game: config::SCORE_ROOKS_TABLE,
        end_game: config::SCORE_ROOKS_TABLE,
    },
    queens: PieceSquareTable {
        mid_game: config::SCORE_QUEENS_TABLE,
        end_game: config::SCORE_QUEENS_TABLE,
    },
    king: PieceSquareTable {
        mid_game: config::SCORE_KING_TABLE_MIDDLE_GAME,
        end_game: config::SCORE_KING_TABLE_END_GAME,
    },
};

fn black_piece_square_tables() -> &'static PieceSquareTables {
    unsafe { &BLACK_PIECE_SQUARE_TABLES }
}

pub unsafe fn init_black_piece_square_tables() {
    reverse_piece_square_tables(&mut BLACK_PIECE_SQUARE_TABLES)
}

fn reverse_piece_square_tables(tables: &mut PieceSquareTables) {
    tables.pawns.mid_game.reverse();
    tables.pawns.end_game.reverse();

    tables.knights.mid_game.reverse();
    tables.knights.end_game.reverse();

    tables.bishops.mid_game.reverse();
    tables.bishops.end_game.reverse();

    tables.rooks.mid_game.reverse();
    tables.rooks.end_game.reverse();

    tables.queens.mid_game.reverse();
    tables.queens.end_game.reverse();

    tables.king.mid_game.reverse();
    tables.king.end_game.reverse();
}

#[cfg(test)]
mod tests {
    use crate::init::init;

    use super::*;

    #[test]
    fn test_has_check() {
        unsafe { init() };

        let mut board = Board::start();
        assert!(!board.white.has_check(&board.black, Color::White));
        assert!(!board.black.has_check(&board.white, Color::Black));

        let black_queen = board.black.queens().first_index();
        let white_queen = board.white.queens().first_index();
        board.black.queens_mut().mov(black_queen, white_queen);
        board.white.captured(board.white.queens().first_index());
        assert!(!board.white.has_check(&board.black, Color::White));
        assert!(board.black.has_check(&board.white, Color::Black));
    }
}
