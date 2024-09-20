use std::fmt;

use crate::{piece::{Piece, STARTING_EMPTY_SQUARES, STARTING_PAWNS, STARTING_PIECES_FIRST_RANK}, bitset::{Bitset, ROW_0, ROW_7}, position::{index_to_position, position_to_index}, color::Color, config, moves::{Moves, SearchMovesBuilder}, mov::Move, result::Result};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Board {
    black: PlayerBoard,
    white: PlayerBoard,
    pub color: Color,
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

        crate::fmt::fmt_board(&board, f)?;
        write!(f, "Next Move: {}\n", self.color)?;
        // TODO: Could display in more fen like notation as the skipped field.
        write!(f, "En Passant: {:?}\n", self.en_passant_index.map(|index| crate::fmt::fmt_index(index)))
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
                bitsets: [Bitset::zero(); 7],
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
                piece_board: [
                    STARTING_EMPTY_SQUARES.as_slice(),
                    STARTING_PAWNS.as_slice(),
                    STARTING_PIECES_FIRST_RANK.as_slice(),
                ].concat().try_into().unwrap(),
            },
            black: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
                piece_board: [
                    STARTING_PIECES_FIRST_RANK.as_slice(),
                    STARTING_PAWNS.as_slice(),
                    STARTING_EMPTY_SQUARES.as_slice(),
                ].concat().try_into().unwrap(),
            },
            color: Color::White,
            en_passant_index: None,
        };
        board.white.fill_bitsets();
        board.black.fill_bitsets();

        board.debug_check();
        board
    }

    pub fn from_fen(fen: &str) -> Result<Self> {
        let fen = fen.trim();
        let mut fen_iter = fen.split_ascii_whitespace();
        let board_raw = fen_iter.next().ok_or(InvalidFenError)?;
        let color_raw = fen_iter.next().ok_or(InvalidFenError)?;
        let castle_raw = fen_iter.next().ok_or(InvalidFenError)?;
        let en_passant_raw = fen_iter.next().ok_or(InvalidFenError)?;
        fen_iter.next().ok_or(InvalidFenError)?; // TODO: Handle 50 move repetition.
        fen_iter.next().ok_or(InvalidFenError)?;
        if fen_iter.next().is_some() {
            return Err(InvalidFenError.into());
        }

        let color = match color_raw {
            "w" => Color::White,
            "b" => Color::Black,
            _ => return Err(InvalidFenError.into()),
        };
        if castle_raw != "-"
            && castle_raw.contains(|ch| !['k', 'q', 'K', 'Q'].contains(&ch)) {
                return Err(InvalidFenError.into());
        }
        let en_passant_index = if en_passant_raw != "-" {
            let en_passant_skipped_index = Move::chess_position_to_index(
                en_passant_raw.as_bytes(),
            )?;
            let (x, y) = index_to_position(en_passant_skipped_index);
            let en_passant_index = match y {
                2 => position_to_index(x, 3),
                5 => position_to_index(x, 4),
                _ => return Err(InvalidFenError.into()),
            };
            // Checking the pawn exists is done later.
            Some(en_passant_index)
        } else {
            None
        };

        let mut board = Self {
            black: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                piece_board: [Piece::None; 64],
                can_castle: CanCastle {
                    left: castle_raw.contains('q'),
                    right: castle_raw.contains('k'),
                },
            },
            white: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                piece_board: [Piece::None; 64],
                can_castle: CanCastle {
                    left: castle_raw.contains('Q'),
                    right: castle_raw.contains('K'),
                },
            },
            color,
            en_passant_index,
        };

        if board_raw.split('/').count() != 8 {
            return Err(InvalidFenError.into());
        }
        for (rank_index, rank) in board_raw.split('/').enumerate() {
            let mut file_index = 0;
            for piece_or_spaces in rank.chars() {
                if file_index > 7 {
                    return Err(InvalidFenError.into());
                }
                match Piece::from_symbol_option(piece_or_spaces.to_ascii_lowercase()) {
                    Some(piece) => {
                        let is_white = piece_or_spaces.is_uppercase();
                        let index = rank_index*8 + file_index;
                        if is_white {
                            board.white.piece_board[index] = piece;
                        } else {
                            board.black.piece_board[index] = piece;
                        }
                        file_index += 1;
                    },
                    None => {
                        let spaces = piece_or_spaces.to_digit(10).ok_or(InvalidFenError)?;
                        if spaces == 0 {
                            return Err(InvalidFenError.into());
                        }
                        file_index += usize::try_from(spaces).unwrap();
                    },
                }
            }
            if file_index != 8 {
                return Err(InvalidFenError.into());
            }
        }
        board.white.fill_bitsets();
        board.black.fill_bitsets();

        board.check()?;
        Ok(board)
    }

    pub fn player_board(&self, color: Color) -> &PlayerBoard {
        match color {
            Color::White => &self.white,
            Color::Black => &self.black,
        }
    }

    fn player_board_mut(&mut self, color: Color) -> &mut PlayerBoard {
        match color {
            Color::White => &mut self.white,
            Color::Black => &mut self.black,
        }
    }

    pub fn we(&self) -> &PlayerBoard {
        self.player_board(self.color)
    }

    pub fn we_mut(&mut self) -> &mut PlayerBoard {
        self.player_board_mut(self.color)
    }

    pub fn enemy(&self) -> &PlayerBoard {
        self.player_board(self.color.other())
    }

    pub fn enemy_mut(&mut self) -> &mut PlayerBoard {
        self.player_board_mut(self.color.other())
    }

    pub fn bitset(&self) -> Bitset {
        self.white.bitset() | self.black.bitset()
    }

    pub fn debug_check(&self) {
        if cfg!(debug_assertions) {
            if let Err(err) = self.check_fast() {
                panic!("{err}");
            }
        }
    }

    fn check(&self) -> Result<()> {
        self.check_fast()?;
        if let Some(en_passant_index) = self.en_passant_index {
            if !self.player_board(self.color.other()).pawns().has(en_passant_index) {
                return Err("en passant position does not match with a pawn".into());
            }
            if self.color.other().en_passant_row() != index_to_position(en_passant_index).1 {
                return Err("en passant on invalid rank".into());
            }
        }
        let has_check = self.player_board(self.color).has_check(
            self.player_board(self.color.other()),
            self.color,
        );
        if has_check {
            return Err("other color is currently in check".into());
        }
        Ok(())
    }

    fn check_fast(&self) -> Result<()> {
        self.white.check()?;
        self.black.check()?;
        assert_eq!(
            self.white.bitset().count() + self.black.bitset().count(),
            self.bitset().count(),
        );
        assert!(self.bitset().count() <= 32);
        Ok(())
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

    pub fn score(&self) -> i32 {
        let is_end_game = self.is_end_game();
        let white = self.white.score(&WHITE_PIECE_SQUARE_TABLES, is_end_game);
        let black = self.black.score(black_piece_square_tables(), is_end_game);
        let score = white - black;
        let perspective = match self.color {
            Color::White => 1,
            Color::Black => -1,
        };
        score * perspective
    }

    pub fn reset_with(&mut self, old: &Self) {
        self.white = old.white;
        self.black = old.black;
        self.color = old.color;
        self.en_passant_index = old.en_passant_index;
    }

    pub fn apply_simple(&mut self, from: u8, to: u8) -> Piece {
        debug_assert_eq!(self.en_passant_index, None);
        self.we_mut().move_piece(from, to);
        let captured_piece = self.enemy().which_piece(to);
        self.enemy_mut().set_piece_none(to);
        self.color = self.color.other();
        captured_piece
    }

    pub fn un_apply_simple(&mut self, from: u8, to: u8, captured_piece: Piece) {
        debug_assert_eq!(self.en_passant_index, None);
        self.color = self.color.other();
        if captured_piece != Piece::None {
            self.enemy_mut().place_piece(to, captured_piece);
        }
        self.we_mut().move_piece(to, from);
    }

    pub fn apply_move_unchecked(&mut self, mov: Move) {
        self.en_passant_index = match mov {
            Move::Normal { from, to } => self.apply_move_normal(from, to),
            Move::Promotion { piece, from, to } => {
                self.apply_move_promotion(piece, from, to);
                None
            },
        };
        self.color = self.color.other();
    }

    fn apply_move_normal(&mut self, from: u8, to: u8) -> Option<u8> {
        assert!(self.we().bitset().has(from));
        if self.apply_move_en_passant(from, to) {
            None
        } else if self.apply_move_castle(from, to) {
            None
        } else {
            let piece = self.we().which_piece(from);
            self.we_mut().move_piece(from, to);
            self.enemy_mut().set_piece_none(to);

            self.disable_castle(from, to);
            Self::possible_en_passant(piece, from, to)
        }
    }

    fn apply_move_en_passant(&mut self, from: u8, to: u8) -> bool {
        let Some(en_passant_index) = self.en_passant_index else {
            return false;
        };
        if !self.we().pawns().has(from) {
            return false;
        }
        let mov = Move::Normal { from, to };
        let is_en_passant = if Some(mov) == Move::en_passant_left(self.color, en_passant_index) {
            debug_assert!(self.we()
                .has_en_passant_left(self.enemy(), en_passant_index));
            true
        } else if Some(mov) == Move::en_passant_right(self.color, en_passant_index) {
            debug_assert!(self.we()
                .has_en_passant_right(self.enemy(), en_passant_index));
            true
        } else {
            false
        };
        if !is_en_passant {
            return false;
        }
        self.we_mut().move_piece(from, to);
        self.enemy_mut().remove_piece(en_passant_index);
        true
    }

    fn disable_castle(&mut self, from: u8, to: u8) {
        if from == self.color.king_starting_index() || to == self.color.king_starting_index() {
            self.we_mut().disable_castle();
        } else if from == self.color.rook_left_starting_index() || to == self.color.rook_left_starting_index() {
            self.we_mut().disable_castle_left();
        } else if from == self.color.rook_right_starting_index() || to == self.color.rook_right_starting_index() {
            self.we_mut().disable_castle_right();
        }
    }

    fn apply_move_castle(&mut self, from: u8, to: u8) -> bool {
        if from != self.color.king_starting_index() || !self.we().can_castle_any() {
            return false;
        }
        if to == from-2 {
            assert!(self.we().can_castle_left());
            self.castle_left_apply();
            true
        } else if to == from+2 {
            assert!(self.we().can_castle_right());
            self.castle_right_apply();
            true
        } else {
            false
        }
    }

    fn castle_left_apply(&mut self) {
        let king_from = self.color.king_starting_index();
        let king_to = self.color.king_starting_index() - 2;
        let rook_left_starting_index = self.color.rook_left_starting_index();
        let first_row = self.color.first_row();

        self.we_mut().move_piece(king_from, king_to);
        self.we_mut().move_piece(
            rook_left_starting_index,
            position_to_index(3, first_row),
        );
        self.we_mut().disable_castle();
    }
    
    fn castle_right_apply(&mut self) {
        let king_from = self.color.king_starting_index();
        let king_to = self.color.king_starting_index() + 2;
        let rook_right_starting_index = self.color.rook_right_starting_index();
        let first_row = self.color.first_row();

        self.we_mut().move_piece(king_from, king_to);
        self.we_mut().move_piece(
            rook_right_starting_index,
            position_to_index(5, first_row),
        );
        self.we_mut().disable_castle();
    }

    fn apply_move_promotion(&mut self, piece: Piece, from: u8, to: u8) {
        debug_assert!(self.we().pawns().has(from));
        self.we_mut().remove_piece(from);
        self.enemy_mut().set_piece_none(to);
        self.we_mut().place_piece(to, piece);
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

    pub fn fill_special_moves<'a>(&mut self, builder: &'a mut SearchMovesBuilder) {
        let all_pieces = self.bitset();
        let enemy_pieces = self.enemy().bitset();

        for from in self.we().pawns().indices() {
            let pawn_promotion = match self.color {
                Color::White => Moves::white_pawn_promotion(all_pieces, from, enemy_pieces),
                Color::Black => Moves::black_pawn_promotion(all_pieces, from, enemy_pieces),
            };
            builder.push_special_promotion_moves(from, pawn_promotion);
            let pawn_double = match self.color {
                Color::White => Moves::white_pawn_double(all_pieces, from, enemy_pieces),
                Color::Black => Moves::black_pawn_double(all_pieces, from, enemy_pieces),
            };
            builder.push_special_normal_moves(from, pawn_double);
        }

        if self.can_castle_left() {
            builder.push_special_move(Move::castle_left(self.color));
        }
        if self.can_castle_right() {
            builder.push_special_move(Move::castle_right(self.color));
        }

        if let Some(en_passant_index) = self.en_passant_index {
            self.fill_en_passant(en_passant_index, builder);
        }
    }

    fn can_castle_right(&mut self) -> bool {
        let all_pieces = self.bitset();
        let king_index = self.we().king().first_index();
    
        let king_starting_index = self.color.king_starting_index();
        let row = self.color.first_row();
    
        let allowed = king_index == king_starting_index
            && self.we().can_castle_right()
            && self.we().rooks().has(self.color.rook_right_starting_index())
            && !all_pieces.has(position_to_index(5, row))
            && !all_pieces.has(position_to_index(6, row));
        if !allowed {
            return false;
        }
    
        for shift in 0..=2 {
            let (from, to) = (king_starting_index, king_starting_index+shift);
            self.we_mut().move_piece(from, to);
            let check = self.enemy()
                .has_check(self.we(), self.color.other());
            self.we_mut().move_piece(to, from);
            if check {
                return false;
            }
        }
        true
    }

    fn can_castle_left(&mut self) -> bool {
        let all_pieces = self.bitset();
        let king_index = self.we().king().first_index();
    
        let king_starting_index = self.color.king_starting_index();
        let row = self.color.first_row();
    
        let allowed = king_index == king_starting_index
            && self.we().can_castle_left()
            && self.we().rooks().has(self.color.rook_left_starting_index())
            && !all_pieces.has(position_to_index(3, row))
            && !all_pieces.has(position_to_index(2, row))
            && !all_pieces.has(position_to_index(1, row));
        if !allowed {
            return false;
        }
    
        for shift in 0..=2 {
            let (from, to) = (king_starting_index, king_starting_index-shift);
            self.we_mut().move_piece(from, to);
            let check = self.enemy().has_check(self.we(), self.color.other());
            self.we_mut().move_piece(to, from);
            if check {
                return false;
            }
        }
        true
    }

    fn fill_en_passant(&self, en_passant_index: u8, builder: &mut SearchMovesBuilder) {
        if self.we().has_en_passant_left(self.enemy(), en_passant_index) {
            builder.push_special_move(Move::en_passant_left(self.color, en_passant_index).unwrap());
        }

        if self.we().has_en_passant_right(self.enemy(), en_passant_index) {
            builder.push_special_move(Move::en_passant_right(self.color, en_passant_index).unwrap());
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerBoard {
    bitsets: [Bitset; 7],
    piece_board: [Piece; 64], // TODO: could be packed smaller
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

    pub fn bishops(&self) -> Bitset {
        self.bitsets[Piece::Bishop.to_usize()]
    }

    pub fn knights(&self) -> Bitset {
        self.bitsets[Piece::Knight.to_usize()]
    }

    pub fn rooks(&self) -> Bitset {
        self.bitsets[Piece::Rook.to_usize()]
    }

    pub fn queens(&self) -> Bitset {
        self.bitsets[Piece::Queen.to_usize()]
    }

    pub fn king(&self) -> Bitset {
        self.bitsets[Piece::King.to_usize()]
    }

    fn piece_mut(&mut self, piece: Piece) -> &mut Bitset {
        &mut self.bitsets[piece.to_usize()]
    }

    pub fn which_piece(&self, index: u8) -> Piece {
        self.piece_board[index as usize]
    }

    fn piece_board_set(&mut self, index: u8, piece: Piece) {
        self.piece_board[index as usize] = piece;
    }

    fn check(&self) -> Result<()> {
        assert_eq!(self.bitsets[Piece::None.to_usize()].count(), 0);
        let count = self.pawns().count()
            + self.bishops().count()
            + self.knights().count()
            + self.rooks().count()
            + self.queens().count()
            + self.king().count();
        assert_eq!(count, self.bitset().count());

        if (self.pawns() & ROW_0).count() != 0 || (self.pawns() & ROW_7).count() != 0 {
            return Err("pawns on the 1st or 8th rank".into());
        }
        if self.king().count() != 1 {
            return Err("missing or multiple kings for one side".into());
        };
        let pawn_count = self.pawns().count();
        if pawn_count > 8 {
            return Err("more than 8 pawns for one side".into());
        };
        if self.bitset().count() > 16 {
            return Err("more than 16 pieces for one side".into());
        };

        let max_bishops_knights_rooks: i32 = 10 - pawn_count;
        if self.bishops().count() > max_bishops_knights_rooks
            || self.knights().count() > max_bishops_knights_rooks
            || self.rooks().count() > max_bishops_knights_rooks {
                return Err("too many bishops, knights or rooks for one side".into());
        }
        if self.queens().count() > 9-pawn_count {
            return Err("too many queens for one side".into());
        }

        Ok(())
    }

    pub fn bitset(&self) -> Bitset {
        let mut bitset = Bitset::zero();
        for i in 0..self.bitsets.len() {
            bitset |= self.bitsets[i];
        }
        bitset
    }

    pub fn move_piece(&mut self, from: u8, to: u8) {
        let piece = self.which_piece(from);
        debug_assert_ne!(piece, Piece::None);
        self.piece_mut(piece).mov(from, to);
        self.piece_board_set(from, Piece::None);
        debug_assert_eq!(self.which_piece(to), Piece::None);
        self.piece_board_set(to, piece);
    }

    pub fn set_piece_none(&mut self, index: u8) {
        let piece = self.which_piece(index);
        self.piece_mut(piece).clear(index);
        self.piece_board_set(index, Piece::None);
    }

    pub fn remove_piece(&mut self, index: u8) {
        debug_assert_ne!(self.which_piece(index), Piece::None);
        self.set_piece_none(index);
    }

    pub fn place_piece(&mut self, index: u8, piece: Piece) {
        debug_assert_eq!(self.which_piece(index), Piece::None);
        debug_assert_ne!(piece, Piece::None);
        self.piece_mut(piece).set(index);
        self.piece_board_set(index, piece);
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

    fn fill_bitsets(&mut self) {
        for index in 0..64 {
            let piece = self.which_piece(index);
            if piece != Piece::None {
                self.piece_mut(piece).set(index);
            }
        }
    }

    pub fn fill_simple_moves<'a>(
        &self,
        enemy: &Self,
        we_color: Color,
        builder: &'a mut SearchMovesBuilder,
    ) {
        let enemy_pieces = enemy.bitset();
        let all_pieces = self.bitset() | enemy_pieces;

        for from in self.pawns().indices() {
            match we_color {
                Color::White => builder.push_simple_moves(
                    from,
                    Moves::white_pawn_normal_single_without_en_passant(all_pieces, from, enemy_pieces),
                ),
                Color::Black => builder.push_simple_moves(
                    from,
                    Moves::black_pawn_normal_single_without_en_passant(all_pieces, from, enemy_pieces),
                ),
            };
        }
        for from in self.knights().indices() {
            builder.push_simple_moves(from, Moves::knight(all_pieces, from, enemy_pieces));
        }
        for from in self.bishops().indices() {
            builder.push_simple_moves(from, Moves::bishop(all_pieces, from, enemy_pieces));
        }
        for from in self.rooks().indices() {
            builder.push_simple_moves(from, Moves::rook(all_pieces, from, enemy_pieces));
        }
        for from in self.queens().indices() {
            builder.push_simple_moves(from, Moves::queen(all_pieces, from, enemy_pieces));
        }
        builder.push_simple_moves(
            self.king().first_index(),
            Moves::king(all_pieces, self.king().first_index(), enemy_pieces),
        );
    }
}

#[derive(Debug)]
pub struct InvalidFenError;

impl fmt::Display for InvalidFenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

impl std::error::Error for InvalidFenError {}

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
        board.black.move_piece(black_queen, white_queen);
        board.white.remove_piece(white_queen);
        assert!(!board.white.has_check(&board.black, Color::White));
        assert!(board.black.has_check(&board.white, Color::Black));
    }

    #[test]
    fn test_fen_en_passant() {
        unsafe { init() };

        let board = Board::from_fen(
            "r1bqkbnr/ppp1pppp/2n5/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
        ).unwrap();
        assert_eq!(
            board.en_passant_index.unwrap(),
            Move::chess_position_to_index(b"d5").unwrap(),
        )
    }
}
