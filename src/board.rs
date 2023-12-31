use std::fmt;

use crate::{piece::{Piece, PROMOTION_PIECES}, bitset::{Bitset, ROW_0, ROW_7}, position::{self, index_to_position, position_to_index}, color::Color, config, moves::{Moves, SimpleMovesBuffer, SimpleMoves, SpecialMovesBuffer}, mov::Move};

// TODO: non piece repr
type PieceBoard = [Piece; 64];

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Board {
    // TODO: color
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

    pub fn player_board(&self, color: Color) -> &PlayerBoard {
        match color {
            Color::White => &self.white,
            Color::Black => &self.black,
        }
    }

    pub fn player_board_mut(&mut self, color: Color) -> &mut PlayerBoard {
        match color {
            Color::White => &mut self.white,
            Color::Black => &mut self.black,
        }
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

    pub fn reset_with(&mut self, old: &Self) {
        self.white = old.white;
        self.black = old.black;
        self.en_passant_index = old.en_passant_index;
    }

    pub fn apply_move_unchecked(&mut self, color: Color, mov: Move) {
        self.en_passant_index = match mov {
            Move::Normal { from, to } => self.apply_move_normal(color, from, to),
            Move::Promotion { piece, from, to } => {
                self.apply_move_promotion(color, piece, from, to);
                None
            },
        };
    }

    fn apply_move_normal(&mut self, color: Color, from: u8, to: u8) -> Option<u8> {
        assert!(self.player_board(color).bitset().has(from));
        if self.apply_move_en_passant(color, from, to) {
            None
        } else if self.apply_move_castle(color, from, to) {
            None
        } else {
            let piece = self.player_board(color).which_piece(from).unwrap();
            self.player_board_mut(color).remove_piece(piece, from);
            self.player_board_mut(color.other()).captured(to);
            self.player_board_mut(color).place_piece(piece, to);

            self.disable_castle(color, from, to);
            Self::possible_en_passant(piece, from, to)
        }
    }

    fn apply_move_en_passant(&mut self, color: Color, from: u8, to: u8) -> bool {
        let Some(en_passant_index) = self.en_passant_index else {
            return false;
        };
        if !self.player_board(color).pawns().has(from) {
            return false;
        }
        let mov = Move::Normal { from, to };
        let is_en_passant = if Some(mov) == color.en_passant_left(en_passant_index) {
            debug_assert!(self.player_board(color).has_en_passant_left(self.player_board(color.other()), en_passant_index));
            true
        } else if Some(mov) == color.en_passant_right(en_passant_index) {
            debug_assert!(self.player_board(color).has_en_passant_right(self.player_board(color.other()), en_passant_index));
            true
        } else {
            false
        };
        if !is_en_passant {
            return false;
        }
        self.player_board_mut(color).pawns_mut().mov(from, to);
        self.player_board_mut(color.other()).pawns_mut().checked_clear(en_passant_index);
        true
    }

    fn disable_castle(&mut self, color: Color, from: u8, to: u8) {
        if from == color.king_starting_index() || to == color.king_starting_index() {
            self.player_board_mut(color).disable_castle();
        } else if from == color.rook_left_starting_index() || to == color.rook_left_starting_index() {
            self.player_board_mut(color).disable_castle_left();
        } else if from == color.rook_right_starting_index() || to == color.rook_right_starting_index() {
            self.player_board_mut(color).disable_castle_right();
        }
    }

    fn apply_move_castle(&mut self, color: Color, from: u8, to: u8) -> bool {
        if from != color.king_starting_index() || !self.player_board(color).can_castle_any() {
            return false;
        }
        if to == from-2 {
            assert!(self.player_board(color).can_castle_left());
            self.castle_left_apply(color);
            true
        } else if to == from+2 {
            assert!(self.player_board(color).can_castle_right());
            self.castle_right_apply(color);
            true
        } else {
            false
        }
    }

    fn castle_left_apply(&mut self, color: Color) {
        let king_from = color.king_starting_index();
        let king_to = color.king_starting_index() - 2;
        self.player_board_mut(color).king_mut().mov(king_from, king_to);
        self.player_board_mut(color).rooks_mut().mov(
            color.rook_left_starting_index(),
            position_to_index(3, color.first_row(),
        ));
        self.player_board_mut(color).disable_castle();
    }
    
    fn castle_right_apply(&mut self, color: Color) {
        let king_from = color.king_starting_index();
        let king_to = color.king_starting_index() + 2;
        self.player_board_mut(color).king_mut().mov(king_from, king_to);
        self.player_board_mut(color).rooks_mut().mov(
            color.rook_right_starting_index(),
            position_to_index(5, color.first_row(),
        ));
        self.player_board_mut(color).disable_castle();
    }

    fn apply_move_promotion(&mut self, color: Color, piece: Piece, from: u8, to: u8) {
        debug_assert!(self.player_board(color).pawns().has(from));
        self.player_board_mut(color).pawns_mut().checked_clear(from);
        self.player_board_mut(color.other()).captured(to);
        self.player_board_mut(color).place_piece(piece, to);
    }

    fn possible_en_passant(piece: Piece, from: u8, to: u8) -> Option<u8> {
        if piece != Piece::Pawn {
            return None;
        }
        let (from_x, from_y) = index_to_position(from);
        let (to_x, to_y) = index_to_position(to);
        if from_x == to_x && (from_y+2 == to_y || from_y.checked_sub(2) == Some(to_y)) {
            Some(to)
        } else {
            None
        }
    }

    pub fn fill_special_moves<'a>(
        &mut self,
        color: Color,
        en_passant_index: Option<u8>,
        buffer: &'a mut SpecialMovesBuffer,
    ) -> &'a [Move] {
        let all_pieces = self.bitset();
        let enemy_pieces = self.player_board(color.other()).bitset();

        let mut builder = SpecialMovesBuilder::new(buffer);

        for from in self.player_board(color).pawns().indices() {
            let pawn_promotion = match color {
                Color::White => Moves::white_pawn_promotion(all_pieces, from, enemy_pieces),
                Color::Black => Moves::black_pawn_promotion(all_pieces, from, enemy_pieces),
            };
            builder.push_promotion_moves(from, pawn_promotion);
            let pawn_double = match color {
                Color::White => Moves::white_pawn_double(all_pieces, from, enemy_pieces),
                Color::Black => Moves::black_pawn_double(all_pieces, from, enemy_pieces),
            };
            builder.push_normal_moves(from, pawn_double);
        }

        if self.can_castle_left(color) {
            builder.push_move(Move::castle_left(color));
        }
        if self.can_castle_right(color) {
            builder.push_move(Move::castle_right(color));
        }

        if let Some(en_passant_index) = en_passant_index {
            self.fill_en_passant(color, en_passant_index, &mut builder);
        }

        builder.to_slice()
    }

    fn can_castle_right(&mut self, color: Color) -> bool {
        let all_pieces = self.bitset();
        let king_index = self.player_board(color).king().first_index();
    
        let king_starting_index = color.king_starting_index();
        let row = color.first_row();
    
        let allowed = king_index == king_starting_index
            && self.player_board(color).can_castle_right()
            && self.player_board(color).rooks().has(color.rook_right_starting_index())
            && !all_pieces.has(position_to_index(5, row))
            && !all_pieces.has(position_to_index(6, row));
        if !allowed {
            return false;
        }
    
        let we_old_king = self.player_board(color).king();
        for shift in 0..=2 {
            self.player_board_mut(color).king_mut().mov(king_starting_index, king_starting_index+shift);
            let check = self.player_board(color.other())
                .has_check(self.player_board(color), color.other());
            *self.player_board_mut(color).king_mut() = we_old_king;
            if check {
                return false;
            }
        }
        true
    }

    fn can_castle_left(&mut self, color: Color) -> bool {
        let all_pieces = self.bitset();
        let king_index = self.player_board(color).king().first_index();
    
        let king_starting_index = color.king_starting_index();
        let row = color.first_row();
    
        let allowed = king_index == king_starting_index
            && self.player_board(color).can_castle_left()
            && self.player_board(color).rooks().has(color.rook_left_starting_index())
            && !all_pieces.has(position_to_index(3, row))
            && !all_pieces.has(position_to_index(2, row))
            && !all_pieces.has(position_to_index(1, row));
        if !allowed {
            return false;
        }
    
        let we_old_king = self.player_board(color).king();
        for shift in 0..=2 {
            self.player_board_mut(color).king_mut().mov(king_starting_index, king_starting_index-shift);
            let check = self.player_board(color.other())
                .has_check(self.player_board(color), color.other());
            *self.player_board_mut(color).king_mut() = we_old_king;
            if check {
                return false;
            }
        }
        true
    }

    fn fill_en_passant(
        &self,
        color: Color,
        en_passant_index: u8,
        builder: &mut SpecialMovesBuilder,
    ) {
        if self.player_board(color).has_en_passant_left(self.player_board(color.other()), en_passant_index) {
            let mov = match color {
                Color::White => Move::white_en_passant_left(en_passant_index),
                Color::Black => Move::black_en_passant_left(en_passant_index),
            };
            builder.push_move(mov.unwrap());
        }

        if self.player_board(color).has_en_passant_right(self.player_board(color.other()), en_passant_index) {
            let mov = match color {
                Color::White => Move::white_en_passant_right(en_passant_index),
                Color::Black => Move::black_en_passant_right(en_passant_index),
            };
            builder.push_move(mov.unwrap());
        }
    }
}

struct SpecialMovesBuilder<'a> {
    buffer: &'a mut SpecialMovesBuffer,
    index: usize,
}

impl <'a> SpecialMovesBuilder<'a> {
    fn new(buffer: &'a mut SpecialMovesBuffer) -> Self {
        Self {
            buffer,
            index: 0,
        }
    }

    fn push_move(&mut self, mov: Move) {
        self.buffer[self.index] = mov;
        self.index += 1;
    }

    fn push_normal_moves(&mut self, from: u8, moves: Moves) {
        for to in moves.moves.indices() {
            self.buffer[self.index] = Move::Normal { from, to };
            self.index += 1;
        }
        for to in moves.captures.indices() {
            self.buffer[self.index] = Move::Normal { from, to };
            self.index += 1;
        }
    }

    fn push_promotion_moves(&mut self, from: u8, moves: Moves) {
        for to in moves.moves.indices() {
            for piece in PROMOTION_PIECES {
                self.buffer[self.index] = Move::Promotion { piece, from, to };
                self.index += 1;
            }
        }
        for to in moves.captures.indices() {
            for piece in PROMOTION_PIECES {
                self.buffer[self.index] = Move::Promotion { piece, from, to };
                self.index += 1;
            }
        }
    }

    fn to_slice(self) -> &'a [Move] {
        &self.buffer[..self.index]
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

    pub fn piece_mut(&mut self, piece: Piece) -> &mut Bitset {
        &mut self.bitsets[piece.to_usize()]
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

    pub fn fill_piece_board(&self, piece_board: &mut PieceBoard) {
        for index in self.pawns().indices() {
            piece_board[index as usize] = Piece::Pawn;
        }
        for index in self.knights().indices() {
            piece_board[index as usize] = Piece::Knight;
        }
        for index in self.bishops().indices() {
            piece_board[index as usize] = Piece::Bishop;
        }
        for index in self.rooks().indices() {
            piece_board[index as usize] = Piece::Rook;
        }
        for index in self.queens().indices() {
            piece_board[index as usize] = Piece::Queen;
        }
        for index in self.king().indices() {
            piece_board[index as usize] = Piece::King;
        }
    }

    pub fn fill_simple_moves<'a>(
        &self,
        enemy: &Self,
        we_color: Color,
        from_to: &'a mut SimpleMovesBuffer,
    ) -> (&'a SimpleMoves, &'a SimpleMoves) {
        let enemy_pieces = enemy.bitset();
        let all_pieces = self.bitset() | enemy_pieces;

        let mut builder = SimpleMovesBuilder::new(from_to);

        for from in self.pawns().indices() {
            match we_color {
                Color::White => builder.push(
                    from,
                    Moves::white_pawn_normal_single_without_en_passant(all_pieces, from, enemy_pieces),
                ),
                Color::Black => builder.push(
                    from,
                    Moves::black_pawn_normal_single_without_en_passant(all_pieces, from, enemy_pieces),
                ),
            };
        }
        for from in self.knights().indices() {
            builder.push(from, Moves::knight(all_pieces, from, enemy_pieces));
        }
        for from in self.bishops().indices() {
            builder.push(from, Moves::bishop(all_pieces, from, enemy_pieces));
        }
        for from in self.rooks().indices() {
            builder.push(from, Moves::rook(all_pieces, from, enemy_pieces));
        }
        for from in self.queens().indices() {
            builder.push(from, Moves::queen(all_pieces, from, enemy_pieces));
        }
        builder.push(
            self.king().first_index(),
            Moves::king(all_pieces, self.king().first_index(), enemy_pieces),
        );
    
        builder.to_moves_captures_slice()
    }
}

struct SimpleMovesBuilder<'a> {
    from_to: &'a mut SimpleMovesBuffer,
    moves_index: usize,
    captures_index: usize,
}

impl <'a> SimpleMovesBuilder<'a> {
    fn new(from_to: &'a mut SimpleMovesBuffer) -> Self {
        let len = from_to.len();
        Self {
            from_to,
            moves_index: 0,
            captures_index: len - 1,
        }
    }

    fn push(&mut self, from: u8, moves: Moves) {
        for to in moves.moves.indices() {
            self.from_to[self.moves_index] = (from, to);
            self.moves_index += 1;
        }
        for to in moves.captures.indices() {
            self.from_to[self.captures_index] = (from, to);
            self.captures_index -= 1;
        }
    }

    fn to_moves_captures_slice(self) -> (&'a SimpleMoves, &'a SimpleMoves) {
        assert!(self.moves_index <= self.captures_index);
        (&self.from_to[..self.moves_index], &self.from_to[self.captures_index+1..])
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
    use crate::{init::init, moves::{SPECIAL_MOVES_BUFFER_LEN, SIMPLE_MOVES_BUFFER_LEN}};

    use super::*;

    // TODO: recursive

    #[test]
    fn test_count_moves_start() {
        unsafe { init() };

        let mut board = Board::start();
        assert_eq!(20, count_moves_single(&mut board, Color::White));
    }

    fn count_moves_single(board: &mut Board, color: Color) -> usize {
        let special_moves_count = {
            let mut special_moves_buffer = [Move::Normal { from: 0, to: 0 }; SPECIAL_MOVES_BUFFER_LEN];
            board.fill_special_moves(color, board.en_passant_index, &mut special_moves_buffer).len()
        };

        let simple_moves_count = {
            let mut simple_moves_buffer = [(0, 0); SIMPLE_MOVES_BUFFER_LEN];
            let (simple_moves, simple_captures) = board.player_board(color).fill_simple_moves(
                board.player_board(color.other()),
                color,
                &mut simple_moves_buffer,
            );
            simple_moves.len() + simple_captures.len()
        };

        special_moves_count + simple_moves_count
    }

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
