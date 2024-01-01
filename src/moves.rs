use std::{iter::zip, cmp::Ordering};

use crate::{bitset::{Bitset, self, ROW_0, COLUMN_0, COLUMN_7, ROW_6, ROW_1, ROW_7}, position::{position_to_index, index_to_position}, board::{PlayerBoard, Board}, mov::{Move, SearchMove}, piece::{PROMOTION_PIECES, Piece}};

static mut DIAGONALS_LEFT: [Bitset; 64] = [bitset::ZERO; 64];
static mut DIAGONALS_RIGHT: [Bitset; 64] = [bitset::ZERO; 64];
static mut KING_MOVES: [Bitset; 64] = [bitset::ZERO; 64];
static mut KNIGHT_MOVES: [Bitset; 64] = [bitset::ZERO; 64];

fn diagonal_left(index: u8) -> Bitset {
    unsafe { DIAGONALS_LEFT[usize::from(index)] }
}

fn diagonal_right(index: u8) -> Bitset {
    unsafe { DIAGONALS_RIGHT[usize::from(index)] }
}

fn king_move(index: u8) -> Bitset {
    unsafe { KING_MOVES[usize::from(index)] }
}

fn knight_move(index: u8) -> Bitset {
    unsafe { KNIGHT_MOVES[usize::from(index)] }
}

pub unsafe fn init_diagonals() {
    let (left, right) = diagonals();
    for index in 0..64 {
        debug_assert_eq!(left.iter().filter(|s| s.has(index)).count(), 1);
        debug_assert_eq!(right.iter().filter(|s| s.has(index)).count(), 1);
        DIAGONALS_LEFT[usize::from(index)] = *left.iter().find(|s| s.has(index)).unwrap();
        DIAGONALS_RIGHT[usize::from(index)] = *right.iter().find(|s| s.has(index)).unwrap();
    }
}

fn positions_to_bitset(positions: impl Iterator<Item = (u8, u8)>) -> Bitset {
    positions.fold(Bitset::zero(), |s, (x, y)| s.with(position_to_index(x, y)))
}

fn diagonals() -> (Vec<Bitset>, Vec<Bitset>) {
    let left_upper = (0..8).map(|y| zip((0..8).rev(), (0..y+1).rev()));
    let left_lower = (0..7).rev().map(|x| zip((0..x+1).rev(), (0..8).rev()));

    let left = left_upper.chain(left_lower)
        .map(|iter| positions_to_bitset(iter))
        .collect::<Vec<_>>();

    let right_upper = (0..8).map(|y| zip(0..8, (0..y+1).rev()));
    let right_lower = (1..8).map(|x| zip(x..8, (0..8).rev()));

    let right = right_upper.chain(right_lower)
        .map(|iter| positions_to_bitset(iter))
        .collect::<Vec<_>>();

    debug_assert_eq!(left.iter().copied().map(Bitset::count).sum::<i32>(), 64);
    debug_assert_eq!(right.iter().copied().map(Bitset::count).sum::<i32>(), 64);

    (left, right)
}

pub unsafe fn init_king_moves() {
    king_moves(&mut KING_MOVES);
}

fn king_moves(moves: &mut [Bitset; 64]) {
    for index in 0..64 {
        let (x, y) = index_to_position(index);
        let (x, y) = (i16::from(x), i16::from(y));
        let positions = itertools::iproduct!(x-1..=x+1, y-1..=y+1)
            .filter(|(x, y)| *x >= 0 && *x < 8 && *y >= 0 && *y < 8)
            .filter(|(moved_x, moved_y)| !(*moved_x == x && *moved_y == y))
            .map(|(x, y)| (u8::try_from(x).unwrap(), u8::try_from(y).unwrap()));
        moves[usize::from(index)] = positions_to_bitset(positions)
    }
}

pub unsafe fn init_knight_moves() {
    knight_moves(&mut KNIGHT_MOVES);
}

fn knight_moves(moves: &mut [Bitset; 64]) {
    let position_diffs: &[(i16, i16)] = &[
        (-1, 2), (1, 2), (2, -1), (2, 1), (-1, -2), (1, -2), (-2, -1), (-2, 1),
    ];
    for index in 0..64 {
        let (x, y) = index_to_position(index);
        let (x, y) = (i16::from(x), i16::from(y));
        let positions = position_diffs.iter()
            .copied()
            .map(|(diff_x, diff_y)| (x+diff_x, y+diff_y))
            .filter(|(x, y)| *x >= 0 && *x < 8 && *y >= 0 && *y < 8)
            .map(|(x, y)| (u8::try_from(x).unwrap(), u8::try_from(y).unwrap()));
        moves[usize::from(index)] = positions_to_bitset(positions)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Moves {
    pub moves: Bitset,
    pub captures: Bitset,
}

impl Moves {
    fn empty() -> Self {
        Self {
            moves: Bitset::zero(),
            captures: Bitset::zero(),
        }
    }

    fn row(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let (_, y) = index_to_position(index);
        let before = Bitset::before(index);
        let after = Bitset::after(index);
        let row = ROW_0 << (y*8);
        let row_pieces = all_pieces & row;
        let range_from = (row_pieces & before).try_last_index()
            .map(|min| Bitset::after(min))
            .unwrap_or(Bitset::ones());
        let range_to = (row_pieces & after).try_first_index()
            .map(|max| Bitset::before(max))
            .unwrap_or(Bitset::ones());
        let range = range_from & range_to & row;
        let moves = range.without(index);
        let captures = ((range << 1) | (range >> 1)) & !range & row & enemy_pieces;
        Self { moves, captures }
    }

    #[allow(dead_code)]
    fn row_alt(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let (x, y) = index_to_position(index);

        let mut moves = Bitset::zero();
        let mut captures = Bitset::zero();

        let mut check_x: i16 = i16::from(x) - 1;
        while check_x >= 0 && !all_pieces.has(position_to_index(check_x.try_into().unwrap(), y)) {
            moves.set(position_to_index(check_x.try_into().unwrap(), y));
            check_x -= 1;
        }
        if check_x >= 0 && enemy_pieces.has(position_to_index(check_x.try_into().unwrap(), y)) {
            captures.set(position_to_index(check_x.try_into().unwrap(), y));
        }

        let mut check_x = x + 1;
        while check_x < 8 && !all_pieces.has(position_to_index(check_x, y)) {
            moves.set(position_to_index(check_x, y));
            check_x += 1;
        }
        if check_x < 8 && enemy_pieces.has(position_to_index(check_x, y)) {
            captures.set(position_to_index(check_x, y));
        }

        Self { moves, captures }
    }

    fn column(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let (x, _) = index_to_position(index);
        let before = Bitset::before(index);
        let after = Bitset::after(index);
        let column = COLUMN_0 << x;
        let column_pieces = column & all_pieces;
        let range_from = (column_pieces & before).try_last_index()
            .map(|min| Bitset::after(min))
            .unwrap_or(Bitset::ones());
        let range_to = (column_pieces & after).try_first_index()
            .map(|max| Bitset::before(max))
            .unwrap_or(Bitset::ones());
        let range = range_from & range_to & column;
        let moves = range.without(index);
        let captures = ((range << 8) | (range >> 8)) & !range & column & enemy_pieces;
        Self { moves, captures }
    }

    #[allow(dead_code)]
    fn column_alt(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let (x, y) = index_to_position(index);

        let mut moves = Bitset::zero();
        let mut captures = Bitset::zero();

        let mut check_y: i16 = i16::from(y) - 1;
        while check_y >= 0 && !all_pieces.has(position_to_index(x, check_y.try_into().unwrap())) {
            moves.set(position_to_index(x, check_y.try_into().unwrap()));
            check_y -= 1;
        }
        if check_y >= 0 && enemy_pieces.has(position_to_index(x, check_y.try_into().unwrap())) {
            captures.set(position_to_index(x, check_y.try_into().unwrap()));
        }

        let mut check_y = y + 1;
        while check_y < 8 && !all_pieces.has(position_to_index(x, check_y)) {
            moves.set(position_to_index(x, check_y));
            check_y += 1;
        }
        if check_y < 8 && enemy_pieces.has(position_to_index(x, check_y)) {
            captures.set(position_to_index(x, check_y));
        }

        Self { moves, captures }
    }

    fn diagonal_left(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let before = Bitset::before(index);
        let after = Bitset::after(index);
        let diagonal = diagonal_left(index);
        let diagonal_pieces = diagonal & all_pieces;
        let range_from = (diagonal_pieces & before).try_last_index()
            .map(|min| Bitset::after(min))
            .unwrap_or(Bitset::ones());
        let range_to = (diagonal_pieces & after).try_first_index()
            .map(|max| Bitset::before(max))
            .unwrap_or(Bitset::ones());
        let range = range_from & range_to & diagonal;
        let moves = range.without(index);

        let mut captures_builder = Bitset::zero();
        let (up_x, up_y) = index_to_position(range.try_first_index().unwrap());
        if up_x > 0 && up_y > 0 {
            captures_builder.set(position_to_index(up_x-1, up_y-1));
        }
        let (down_x, down_y) = index_to_position(range.try_last_index().unwrap());
        if down_x < 7 && down_y < 7 {
            captures_builder.set(position_to_index(down_x+1, down_y+1));
        }

        Self { moves: moves, captures: captures_builder & enemy_pieces }
    }

    fn diagonal_right(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let before = Bitset::before(index);
        let after = Bitset::after(index);
        let diagonal = diagonal_right(index);
        let diagonal_pieces = diagonal & all_pieces;
        let range_from = (diagonal_pieces & before).try_last_index()
            .map(|min| Bitset::after(min))
            .unwrap_or(Bitset::ones());
        let range_to = (diagonal_pieces & after).try_first_index()
            .map(|max| Bitset::before(max))
            .unwrap_or(Bitset::ones());
        let range = range_from & range_to & diagonal;
        let moves = range.without(index);

        let mut captures_builder = Bitset::zero();
        let (up_x, up_y) = index_to_position(range.try_first_index().unwrap());
        if up_x < 7 && up_y > 0 {
            captures_builder.set(position_to_index(up_x+1, up_y-1));
        }
        let (down_x, down_y) = index_to_position(range.try_last_index().unwrap());
        if down_x > 0 && down_y < 7 {
            captures_builder.set(position_to_index(down_x-1, down_y+1));
        }

        Self { moves: moves, captures: captures_builder & enemy_pieces }
    }

    pub fn bishop(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::diagonal_left(all_pieces, index, enemy_pieces)
            .combine(&Self::diagonal_right(all_pieces, index, enemy_pieces))
    }

    pub fn rook(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::row(all_pieces, index, enemy_pieces)
            .combine(&Self::column(all_pieces, index, enemy_pieces))
    }

    pub fn queen(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::bishop(all_pieces, index, enemy_pieces)
            .combine(&Self::rook(all_pieces, index, enemy_pieces))
    }

    fn white_pawn_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let forward = pawn >> 8;
        let moves = forward & !all_pieces;
        let captures = {
            let a = (forward & !COLUMN_0) >> 1;
            let b = (forward & !COLUMN_7) << 1;
            (a | b) & enemy_pieces
        };
        Self { moves, captures }
    }

    pub fn white_pawn_double(all_pieces: Bitset, index: u8, _: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let double: Bitset = ((((pawn & ROW_6) >> 8) & !all_pieces) >> 8) & !all_pieces;
        Self { moves: double, captures: Bitset::zero() }
    }

    fn white_pawn_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_single_without_en_passant(all_pieces, index, enemy_pieces)
            .combine(&Self::white_pawn_double(all_pieces, index, enemy_pieces))
    }

    pub fn white_pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(!ROW_0)
    }

    pub fn white_pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(ROW_0)
    }

    fn black_pawn_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let forward = pawn << 8;
        let moves = forward & !all_pieces;
        let captures = {
            let a = (forward & !COLUMN_0) >> 1;
            let b = (forward & !COLUMN_7) << 1;
            (a | b) & enemy_pieces
        };
        Self { moves, captures }
    }

    pub fn black_pawn_double(all_pieces: Bitset, index: u8, _: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let double = ((((pawn & ROW_1) << 8) & !all_pieces) << 8) & !all_pieces;
        Self { moves: double, captures: Bitset::zero() }
    }

    fn black_pawn_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_single_without_en_passant(all_pieces, index, enemy_pieces)
            .combine(&Self::black_pawn_double(all_pieces, index, enemy_pieces))
    }

    pub fn black_pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(!ROW_7)
    }

    pub fn black_pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(ROW_7)
    }

    pub fn king(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let possible_moves = king_move(index);
        Self {
            moves: possible_moves & !all_pieces,
            captures: possible_moves & enemy_pieces,
        }
    }

    pub fn knight(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let possible_moves = knight_move(index);
        Self {
            moves: possible_moves & !all_pieces,
            captures: possible_moves & enemy_pieces,
        }
    }

    pub fn without_pawns_castle(we: &PlayerBoard, enemy: &PlayerBoard) -> Self {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.queens().indices().map(|index| Moves::queen(all_pieces, index, enemy_pieces))
            .chain(we.rooks().indices().map(|index| Moves::rook(all_pieces, index, enemy_pieces)))
            .chain(we.bishops().indices().map(|index| Moves::bishop(all_pieces, index, enemy_pieces)))
            .chain(we.knights().indices().map(|index| Moves::knight(all_pieces, index, enemy_pieces)))
            .chain(we.king().indices().map(|index| Moves::king(all_pieces, index, enemy_pieces)))
            .fold(Moves::empty(), |a, b| a.combine(&b))
    }

    pub fn white_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Self {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.pawns().indices()
            .map(|index| Moves::white_pawn_without_en_passant(all_pieces, index, enemy_pieces))
            .fold(Moves::empty(), |a, b| a.combine(&b))
    }

    pub fn black_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Self {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.pawns().indices()
            .map(|index| Moves::black_pawn_without_en_passant(all_pieces, index, enemy_pieces))
            .fold(Moves::empty(), |a, b| a.combine(&b))
    }

    pub fn combine(&self, other: &Self) -> Self {
        Self {
            moves: self.moves | other.moves,
            captures: self.captures | other.captures,
        }
    }

    fn and(&self, bitset: Bitset) -> Self {
        Self {
            moves: self.moves & bitset,
            captures: self.captures & bitset,
        }
    }
}

const MAX_MOVES: usize = 218; // https://www.chessprogramming.org/Chess_Position

pub struct SearchMovesBuilder {
    buffer: [SearchMove; MAX_MOVES],
    index: usize,
}

impl SearchMovesBuilder {
    pub fn new() -> Self {
        Self {
            buffer: [SearchMove::Simple { from: 0, to: 0 }; MAX_MOVES],
            index: 0,
        }
    }

    pub fn push_simple_moves(&mut self, from: u8, moves: Moves) {
        for to in moves.moves.indices() {
            self.buffer[self.index] = SearchMove::Simple { from, to };
            self.index += 1;
        }
        for to in moves.captures.indices() {
            self.buffer[self.index] = SearchMove::Simple { from, to };
            self.index += 1;
        }
    }

    pub fn push_special_move(&mut self, mov: Move) {
        self.buffer[self.index] = SearchMove::Special(mov);
        self.index += 1;
    }

    pub fn push_special_normal_moves(&mut self, from: u8, moves: Moves) {
        for to in moves.moves.indices() {
            self.buffer[self.index] = SearchMove::Special(Move::Normal { from, to });
            self.index += 1;
        }
        for to in moves.captures.indices() {
            self.buffer[self.index] = SearchMove::Special(Move::Normal { from, to });
            self.index += 1;
        }
    }

    pub fn push_special_promotion_moves(&mut self, from: u8, moves: Moves) {
        for to in moves.moves.indices() {
            for piece in PROMOTION_PIECES {
                self.buffer[self.index] = SearchMove::Special(Move::Promotion { piece, from, to });
                self.index += 1;
            }
        }
        for to in moves.captures.indices() {
            for piece in PROMOTION_PIECES {
                self.buffer[self.index] = SearchMove::Special(Move::Promotion { piece, from, to });
                self.index += 1;
            }
        }
    }

    fn as_slice_mut(&mut self) -> &mut [SearchMove] {
        &mut self.buffer[..self.index]
    }

    pub fn fill<'a>(&'a mut self, board: &mut Board) -> &[SearchMove] {
        board.we().fill_simple_moves(board.enemy(), board.color, self);
        board.fill_special_moves(self);
        self.as_slice_mut()
    }

    pub fn sort<'a>(&'a mut self, board: &Board) -> &'a [SearchMove] {
        self.as_slice_mut().sort_by(|a, b| {
            compare_search_move_score_guess(board, *a, *b).reverse()
        });
        self.as_slice_mut()
    }
}

fn compare_search_move_score_guess(board: &Board, a: SearchMove, b: SearchMove) -> Ordering {
    // TODO: score
    if a.is_promotion() || b.is_promotion() {
        a.is_promotion().cmp(&b.is_promotion())
    } else {
        let a_capture = board.enemy().which_piece(a.to());
        let b_capture = board.enemy().which_piece(b.to());
        (a_capture != Piece::None).cmp(&(b_capture != Piece::None))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_line_row() {
        for (x, y, enemy_x, enemy_y) in itertools::iproduct!(0..8, 0..8, 0..8, 0..8) {
            if x == enemy_x && y == enemy_y {
                continue;
            }
            let index = position_to_index(x, y);
            let enemy_index = position_to_index(enemy_x, enemy_y);
            let all_pieces = Bitset::zero().with(index).with(enemy_index);
            let enemy_pieces = Bitset::zero().with(enemy_index);
            assert_eq!(
                Moves::row(all_pieces, index, enemy_pieces),
                Moves::row_alt(all_pieces, index, enemy_pieces),
            );
        }
    }

    #[test]
    fn test_column_row() {
        for (x, y, enemy_x, enemy_y) in itertools::iproduct!(0..8, 0..8, 0..8, 0..8) {
            if x == enemy_x && y == enemy_y {
                continue;
            }
            let index = position_to_index(x, y);
            let enemy_index = position_to_index(enemy_x, enemy_y);
            let all_pieces = Bitset::zero().with(index).with(enemy_index);
            let enemy_pieces = Bitset::zero().with(enemy_index);
            assert_eq!(
                Moves::column(all_pieces, index, enemy_pieces),
                Moves::column_alt(all_pieces, index, enemy_pieces),
            );
        }
    }
}
