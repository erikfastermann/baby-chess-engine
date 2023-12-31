use crate::{color::Color, board::{Board, PlayerBoard}, bitset::Bitset, moves::Moves, position::position_to_index, mov::Move};

pub trait Side {
    fn color() -> Color;

    fn board(&self) -> &Board;
    fn we(&self) -> &PlayerBoard;
    fn enemy(&self) -> &PlayerBoard;

    fn board_mut(&mut self) -> &mut Board;
    fn we_mut(&mut self) -> &mut PlayerBoard;
    fn enemy_mut(&mut self) -> &mut PlayerBoard;

    fn pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;
    fn pawn_double(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;
    fn pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves;
}

pub struct WhiteSide<'a> {
    board: &'a mut Board,
}

impl <'a> WhiteSide<'a> {
    pub fn new(board: &'a mut Board) -> Self {
        Self { board }
    }
}

impl <'a> Side for WhiteSide<'a> {
    fn color() -> Color {
        Color::White
    }

    fn board_mut(&mut self) -> &mut Board {
        self.board
    }

    fn we_mut(&mut self) -> &mut PlayerBoard {
        &mut self.board.white
    }

    fn enemy_mut(&mut self) -> &mut PlayerBoard {
        &mut self.board.black
    }

    fn board(&self) -> &Board {
        self.board
    }

    fn we(&self) -> &PlayerBoard {
        &self.board.white
    }

    fn enemy(&self) -> &PlayerBoard {
        &self.board.black
    }

    fn pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::white_pawn_normal_single_without_en_passant(all_pieces, index, enemy_pieces)
    }

    fn pawn_double(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::white_pawn_double(all_pieces, index, enemy_pieces)
    }

    fn pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::white_pawn_promotion(all_pieces, index, enemy_pieces)
    }

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves {
        Moves::white_pawns_without_en_passant(we, enemy)
    }
}

pub struct BlackSide<'a> {
    board: &'a mut Board,
}

impl <'a> BlackSide<'a> {
    pub fn new(board: &'a mut Board) -> Self {
        Self { board }
    }
}

impl <'a> Side for BlackSide<'a> {
    fn color() -> Color {
        Color::Black
    }

    fn board_mut(&mut self) -> &mut Board {
        self.board
    }

    fn we_mut(&mut self) -> &mut PlayerBoard {
        &mut self.board.black
    }

    fn enemy_mut(&mut self) -> &mut PlayerBoard {
        &mut self.board.white
    }

    fn board(&self) -> &Board {
        self.board
    }

    fn we(&self) -> &PlayerBoard {
        &self.board.black
    }

    fn enemy(&self) -> &PlayerBoard {
        &self.board.white
    }

    fn pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::black_pawn_normal_single_without_en_passant(all_pieces, index, enemy_pieces)
    }

    fn pawn_double(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::black_pawn_double(all_pieces, index, enemy_pieces)
    }

    fn pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::black_pawn_promotion(all_pieces, index, enemy_pieces)
    }

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves {
        Moves::black_pawns_without_en_passant(we, enemy)
    }
}

pub fn castle_left_apply<S: Side>(side: &mut S) -> Move {
    let king_from = S::color().king_starting_index();
    let king_to = S::color().king_starting_index() - 2;
    side.we_mut().king.mov(king_from, king_to);
    side.we_mut().rooks.mov(
        S::color().rook_left_starting_index(),
        position_to_index(3, S::color().first_row(),
    ));
    side.we_mut().disable_castle();
    Move::Normal { from: king_from, to: king_to }
}

pub fn castle_right_apply<S: Side>(side: &mut S) -> Move {
    let king_from = S::color().king_starting_index();
    let king_to = S::color().king_starting_index() + 2;
    side.we_mut().king.mov(king_from, king_to);
    side.we_mut().rooks.mov(
        S::color().rook_right_starting_index(),
        position_to_index(5, S::color().first_row(),
    ));
    side.we_mut().disable_castle();
    Move::Normal { from: king_from, to: king_to }
}