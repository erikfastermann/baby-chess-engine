use std::fmt;

use crate::{bitset::{Bitset, ROW_0, ROW_7}, color::Color, mov::{Move, MoveKind}, moves::{Moves, MovesBuilder}, piece::{Piece, PROMOTION_PIECES, STARTING_EMPTY_SQUARES, STARTING_PAWNS, STARTING_PIECES_FIRST_RANK}, position::{self, index_to_position, position_to_index}, result::Result};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct PositionBoard {
    black: PlayerBoard,
    white: PlayerBoard,
    color: Color,
    en_passant_index: Option<u8>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Board {
    pub black: PlayerBoard,
    pub white: PlayerBoard,
    pub color: Color,
    pub en_passant_index: Option<u8>,
    pub moves_since_capture_or_pawn: u8,
}

pub const MAX_MOVES_SINCE_CAPTURE_OR_PAWN: u8 = 50;

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

impl Board {
    pub fn start() -> Self {
        let mut board = Self {
            white: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
            },
            black: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
            },
            color: Color::White,
            en_passant_index: None,
            moves_since_capture_or_pawn: 0,
        };
        board.white.fill_bitsets(&[
                STARTING_EMPTY_SQUARES.as_slice(),
                STARTING_PAWNS.as_slice(),
                STARTING_PIECES_FIRST_RANK.as_slice(),
            ].concat().try_into().unwrap(),
        );
        board.black.fill_bitsets(&[
                STARTING_PIECES_FIRST_RANK.as_slice(),
                STARTING_PAWNS.as_slice(),
                STARTING_EMPTY_SQUARES.as_slice(),
            ].concat().try_into().unwrap(),
        );

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
        let moves_since_capture_or_pawn_raw = fen_iter.next().ok_or(InvalidFenError)?;
        fen_iter.next().ok_or(InvalidFenError)?;
        if fen_iter.next().is_some() {
            return Err(InvalidFenError.into());
        }

        if board_raw.split('/').count() != 8 {
            return Err(InvalidFenError.into());
        }
        let mut white_pieces = [Piece::None; 64];
        let mut black_pieces = [Piece::None; 64];
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
                            white_pieces[index] = piece;
                        } else {
                            black_pieces[index] = piece;
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
            let en_passant_skipped_index = position::human_position_to_index(
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
        let moves_since_capture_or_pawn: u8 = moves_since_capture_or_pawn_raw.parse()?;

        let mut board = Self {
            black: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                can_castle: CanCastle {
                    left: castle_raw.contains('q'),
                    right: castle_raw.contains('k'),
                },
            },
            white: PlayerBoard {
                bitsets: [Bitset::zero(); 7],
                can_castle: CanCastle {
                    left: castle_raw.contains('Q'),
                    right: castle_raw.contains('K'),
                },
            },
            color,
            en_passant_index,
            moves_since_capture_or_pawn,
        };
        board.white.fill_bitsets(&white_pieces);
        board.black.fill_bitsets(&black_pieces);
        board.check()?;
        Ok(board)
    }

    pub fn to_position_board(self) -> PositionBoard {
        PositionBoard {
            black: self.black,
            white: self.white,
            color: self.color,
            en_passant_index: self.en_passant_index,
        }
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

    pub fn check_fast(&self) -> Result<()> {
        self.white.check()?;
        self.black.check()?;
        assert_eq!(
            self.white.bitset().count() + self.black.bitset().count(),
            self.bitset().count(),
        );
        assert!(self.bitset().count() <= 32);
        if self.moves_since_capture_or_pawn > MAX_MOVES_SINCE_CAPTURE_OR_PAWN {
            return Err("more than 50 moves without capturing a piece or moving a pawn".into());
        }
        Ok(())
    }

    pub fn is_draw_fast(&self) -> bool {
        self.moves_since_capture_or_pawn >= MAX_MOVES_SINCE_CAPTURE_OR_PAWN
    }

    pub fn reset_with(&mut self, old: &Self) {
        self.white = old.white;
        self.black = old.black;
        self.color = old.color;
        self.en_passant_index = old.en_passant_index;
        self.moves_since_capture_or_pawn = old.moves_since_capture_or_pawn;
    }

    pub fn apply_move_unchecked(&mut self, mov: Move) -> UndoMove {
        let undo = self.apply_move_unchecked_inner(mov);
        self.color = self.color.other();
        if matches!(mov.kind(), MoveKind::PawnDouble) {
            self.en_passant_index = Some(mov.to());
        } else {
            self.en_passant_index = None;
        }
        if mov.is_capture() || mov.piece() == Piece::Pawn {
            self.moves_since_capture_or_pawn = 0;
        } else {
            self.moves_since_capture_or_pawn += 1;
        }
        undo
    }

    fn apply_move_unchecked_inner(&mut self, mov: Move) -> UndoMove {
        match mov.kind() {
            MoveKind::NonCapture => {
                self.we_mut()
                    .piece_mut(mov.piece())
                    .mov(mov.from(), mov.to());
                UndoMove::NonCapture {
                    piece: mov.piece(),
                    from: mov.from(),
                    to: mov.to(),
                    we_can_castle: self.move_disable_castle(mov.piece(), mov.from()),
                    en_passant_index: self.en_passant_index,
                }
            },
            MoveKind::Capture => {
                let captured_piece = self.enemy().which_piece(mov.to());
                self.enemy_mut()
                    .piece_mut(captured_piece)
                    .clear(mov.to());
                self.we_mut()
                    .piece_mut(mov.piece())
                    .mov(mov.from(), mov.to());
                UndoMove::Capture {
                    piece: mov.piece(),
                    from: mov.from(),
                    to: mov.to(),
                    captured_piece,
                    we_can_castle: self.move_disable_castle(mov.piece(), mov.from()),
                    enemy_can_castle: self.capture_disable_castle(mov.to()),
                    en_passant_index: self.en_passant_index,
                }
            },
            MoveKind::EnPassant => {
                let en_passant_index = self.en_passant_index.unwrap();
                self.we_mut().piece_mut(Piece::Pawn).mov(mov.from(), mov.to());
                self.enemy_mut().piece_mut(Piece::Pawn).clear(en_passant_index);
                UndoMove::EnPassant {
                    from: mov.from(),
                    to: mov.to(),
                    en_passant_index: self.en_passant_index,
                }
            },
            MoveKind::Castle => {
                let we_can_castle = if mov == Move::castle_left(self.color) {
                    self.castle_left_apply()
                } else {
                    debug_assert_eq!(mov, Move::castle_right(self.color));
                    self.castle_right_apply()
                };
                UndoMove::Castle {
                    from: mov.from(),
                    to: mov.to(),
                    we_can_castle,
                    en_passant_index: self.en_passant_index,
                }
            },
            MoveKind::Promotion => {
                self.we_mut().piece_mut(Piece::Pawn).clear(mov.from());
                self.we_mut().piece_mut(mov.promotion_piece()).set(mov.to());
                UndoMove::Promotion {
                    from: mov.from(),
                    to: mov.to(),
                    promotion_piece: mov.promotion_piece(),
                    en_passant_index: self.en_passant_index,
                }
            },
            MoveKind::PromotionCapture => {
                let captured_piece = self.enemy().which_piece(mov.to());
                self.enemy_mut()
                    .piece_mut(captured_piece)
                    .clear(mov.to());
                self.we_mut().piece_mut(Piece::Pawn).clear(mov.from());
                self.we_mut().piece_mut(mov.promotion_piece()).set(mov.to());
                UndoMove::PromotionCapture {
                    from: mov.from(),
                    to: mov.to(),
                    promotion_piece: mov.promotion_piece(),
                    captured_piece,
                    enemy_can_castle: self.capture_disable_castle(mov.to()),
                    en_passant_index: self.en_passant_index,
                }
            },
            MoveKind::PawnDouble => {
                self.we_mut()
                    .piece_mut(Piece::Pawn)
                    .mov(mov.from(), mov.to());
                UndoMove::PawnDouble {
                    from: mov.from(),
                    to: mov.to(),
                    en_passant_index: self.en_passant_index,
                }
            },
        }
    }

    fn move_disable_castle(&mut self, piece: Piece, from: u8) -> CanCastle {
        let old_can_castle = self.we().can_castle;
        if piece == Piece::King {
            self.we_mut().disable_castle();
        } else if piece == Piece::Rook {
            if from == self.color.rook_left_starting_index() {
                self.we_mut().disable_castle_left();
            } else if from == self.color.rook_right_starting_index() {
                self.we_mut().disable_castle_right();
            }
        }
        old_can_castle
    }

    fn capture_disable_castle(&mut self, to: u8) -> CanCastle {
        let old_can_castle = self.enemy().can_castle;
        if to == self.color.other().rook_left_starting_index() {
            self.enemy_mut().disable_castle_left();
        } else if to == self.color.other().rook_right_starting_index() {
            self.enemy_mut().disable_castle_right();
        }
        old_can_castle
    }

    fn castle_left_apply(&mut self) -> CanCastle {
        let king_from = self.color.king_starting_index();
        let king_to = self.color.king_starting_index() - 2;
        let rook_left_starting_index = self.color.rook_left_starting_index();
        let first_row = self.color.first_row();

        self.we_mut().piece_mut(Piece::King).mov(king_from, king_to);
        self.we_mut().piece_mut(Piece::Rook).mov(
            rook_left_starting_index,
            position_to_index(3, first_row),
        );
        let old_can_castle = self.we().can_castle;
        self.we_mut().disable_castle();
        old_can_castle
    }

    fn castle_right_apply(&mut self) -> CanCastle {
        let king_from = self.color.king_starting_index();
        let king_to = self.color.king_starting_index() + 2;
        let rook_right_starting_index = self.color.rook_right_starting_index();
        let first_row = self.color.first_row();

        self.we_mut().piece_mut(Piece::King).mov(king_from, king_to);
        self.we_mut().piece_mut(Piece::Rook).mov(
            rook_right_starting_index,
            position_to_index(5, first_row),
        );
        let old_can_castle = self.we().can_castle;
        self.we_mut().disable_castle();
        old_can_castle
    }

    pub fn fill_special_moves<'a>(&mut self, builder: &'a mut MovesBuilder) {
        let all_pieces = self.bitset();
        let enemy_pieces = self.enemy().bitset();

        for from in self.we().pawns().indices() {
            let pawn_promotion = match self.color {
                Color::White => Moves::white_pawn_promotion(all_pieces, from, enemy_pieces),
                Color::Black => Moves::black_pawn_promotion(all_pieces, from, enemy_pieces),
            };
            for to in pawn_promotion.moves.indices() {
                for promotion_piece in PROMOTION_PIECES {
                    builder.push_special_move(Move::promotion(from, to, promotion_piece));
                }
            }
            for to in pawn_promotion.captures.indices() {
                for promotion_piece in PROMOTION_PIECES {
                    builder.push_special_move(Move::promotion_capture(from, to, promotion_piece));
                }
            }

            let pawn_double = match self.color {
                Color::White => Moves::white_pawn_double(all_pieces, from, enemy_pieces),
                Color::Black => Moves::black_pawn_double(all_pieces, from, enemy_pieces),
            };
            for to in pawn_double.moves.indices() {
                builder.push_special_move(Move::pawn_double(from, to));
            }
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
            self.we_mut().piece_mut(Piece::King).mov(from, to);
            let check = self.enemy()
                .has_check(self.we(), self.color.other());
            self.we_mut().piece_mut(Piece::King).mov(to, from);
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
            self.we_mut().piece_mut(Piece::King).mov(from, to);
            let check = self.enemy().has_check(self.we(), self.color.other());
            self.we_mut().piece_mut(Piece::King).mov(to, from);
            if check {
                return false;
            }
        }
        true
    }

    fn fill_en_passant(&self, en_passant_index: u8, builder: &mut MovesBuilder) {
        // TODO: Check nothing can be captured with the en passant move.
        if self.we().has_en_passant_left(self.enemy(), en_passant_index) {
            builder.push_special_move(Move::en_passant_left(self.color, en_passant_index).unwrap());
        }

        if self.we().has_en_passant_right(self.enemy(), en_passant_index) {
            builder.push_special_move(Move::en_passant_right(self.color, en_passant_index).unwrap());
        }
    }
}

#[repr(u8)]
pub enum UndoMove {
    NonCapture {
        piece: Piece,
        from: u8,
        to: u8,
        we_can_castle: CanCastle,
        en_passant_index: Option<u8>,
    },
    Capture {
        piece: Piece,
        from: u8,
        to: u8,
        captured_piece: Piece,
        we_can_castle: CanCastle,
        enemy_can_castle: CanCastle,
        en_passant_index: Option<u8>,
    },
    EnPassant { from: u8, to: u8, en_passant_index: Option<u8>, },
    Castle { from: u8, to: u8, we_can_castle: CanCastle, en_passant_index: Option<u8>, },
    Promotion { from: u8, to: u8, promotion_piece: Piece, en_passant_index: Option<u8>, },
    PromotionCapture {
        from: u8,
        to: u8,
        promotion_piece: Piece,
        captured_piece: Piece,
        enemy_can_castle: CanCastle,
        en_passant_index: Option<u8>,
    },
    PawnDouble { from: u8, to: u8, en_passant_index: Option<u8>, }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct PlayerBoard {
    bitsets: [Bitset; 7],
    pub can_castle: CanCastle,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CanCastle {
    // TODO: bitflags
    left: bool,
    right: bool,
}

pub const SCORE_MIN: i32 = i32::MAX * -1;
pub const SCORE_MAX: i32 = i32::MAX;

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
        let piece = u32::from(self.pawns().has(index))*Piece::Pawn.to_u32()
            + u32::from(self.knights().has(index))*Piece::Knight.to_u32()
            + u32::from(self.bishops().has(index))*Piece::Bishop.to_u32()
            + u32::from(self.rooks().has(index))*Piece::Rook.to_u32()
            + u32::from(self.queens().has(index))*Piece::Queen.to_u32()
            + u32::from(self.king().has(index))*Piece::King.to_u32();
        Piece::from_u32_fast(piece)
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

    pub fn has_check(&self, enemy: &Self, self_color: Color) -> bool {
        Moves::without_en_passant_castle(self, enemy, self_color)
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

    fn fill_bitsets(&mut self, pieces: &[Piece; 64]) {
        for (index, piece) in pieces.iter().copied().enumerate() {
            let index = u8::try_from(index).unwrap();
            if piece != Piece::None {
                self.piece_mut(piece).set(index);
            }
        }
    }

    pub fn fill_simple_moves<'a>(
        &self,
        enemy: &Self,
        we_color: Color,
        builder: &'a mut MovesBuilder,
    ) {
        let enemy_pieces = enemy.bitset();
        let all_pieces = self.bitset() | enemy_pieces;

        for from in self.pawns().indices() {
            match we_color {
                Color::White => builder.push_simple_moves(
                    from,
                    Piece::Pawn,
                    Moves::white_pawn_normal_single_without_en_passant(all_pieces, from, enemy_pieces),
                ),
                Color::Black => builder.push_simple_moves(
                    from,
                    Piece::Pawn,
                    Moves::black_pawn_normal_single_without_en_passant(all_pieces, from, enemy_pieces),
                ),
            };
        }
        for from in self.knights().indices() {
            builder.push_simple_moves(
                from,
                Piece::Knight,
                Moves::knight(all_pieces, from, enemy_pieces),
            );
        }
        for from in self.bishops().indices() {
            builder.push_simple_moves(
                from,
                Piece::Bishop,
                Moves::bishop(all_pieces, from, enemy_pieces),
            );
        }
        for from in self.rooks().indices() {
            builder.push_simple_moves(
                from,
                Piece::Rook,
                Moves::rook(all_pieces, from, enemy_pieces),
            );
        }
        for from in self.queens().indices() {
            builder.push_simple_moves(
                from,
                Piece::Queen,
                Moves::queen(all_pieces, from, enemy_pieces),
            );
        }
        builder.push_simple_moves(
            self.king().first_index(),
            Piece::King,
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
        board.black.piece_mut(Piece::Queen).mov(black_queen, white_queen);
        board.white.piece_mut(Piece::Queen).clear(white_queen);
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
            position::human_position_to_index(b"d5").unwrap(),
        )
    }
}
