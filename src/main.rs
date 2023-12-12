use std::{fmt, ops::{BitOr, Not, BitAnd, Shl, Shr}};

fn main() {
    let all_columns = COLUMN_0|COLUMN_1|COLUMN_2|COLUMN_3|COLUMN_4|COLUMN_5|COLUMN_6|COLUMN_7;
    debug_assert_eq!(all_columns.count(), 64);
    debug_assert_eq!(ROW_2_TO_7.count(), 48);
    let board = Board::start();
    println!("{}", board);

    let (_, mov) = board.white_moves(6, None);
    println!("{mov}");
}

fn index_to_position(index: u8) -> (u8, u8) {
    debug_assert!(index < 64);
    (index%8, index/8)
}

fn position_to_index(x: u8, y: u8) -> u8 {
    debug_assert!(x < 8);
    debug_assert!(y < 8);
    8*y + x
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct Bitset(u64);

impl BitOr for Bitset {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl Not for Bitset {
    type Output = Self;

    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl BitAnd for Bitset {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self::Output {
        Self(self.0 & rhs.0)
    }
}

impl Shl<u8> for Bitset {
    type Output = Self;

    fn shl(self, rhs: u8) -> Self::Output {
        debug_assert!(rhs < 64);
        Self(self.0 << rhs)
    }
}

impl Shr<u8> for Bitset {
    type Output = Self;

    fn shr(self, rhs: u8) -> Self::Output {
        debug_assert!(rhs < 64);
        Self(self.0 >> rhs)
    }
}

impl fmt::Display for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = [b'.'; 64];
        fmt_pieces(&mut board, self.clone(), b'#');
        fmt_board(&board, f)
    }
}

impl fmt::Debug for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl Bitset {
    fn zero() -> Self {
        Self(0)
    }

    fn from_position(x: u8, y: u8) -> Self {
        Self::zero().set(position_to_index(x, y))
    }

    fn is_empty(self) -> bool {
        self.0 == 0
    }

    fn count(self) -> i32 {
        self.0.count_ones() as i32
    }

    fn first_index(self) -> u8 {
        self.0.trailing_zeros() as u8
    }

    fn has(self, index: u8) -> bool {
        debug_assert!(index < 64);
        (self.0 & (1 << index)) != 0
    }

    fn clear(self, index: u8) -> Bitset {
        debug_assert!(index < 64);
        Self(self.0 & !(1 << index))
    }

    fn set(self, index: u8) -> Bitset {
        debug_assert!(index < 64 || index == u8::MAX);
        Self(self.0 | (1 << index))
    }

    fn indices(self) -> Indices {
        Indices(self)
    }
}

struct Indices(Bitset);

impl Iterator for Indices {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0.is_empty() {
            None
        } else {
            let index = self.0.first_index();
            self.0 = self.0.clear(index);
            Some(index)
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct PlayerBoard {
    pawns: Bitset,
    bishops: Bitset,
    knights: Bitset,
    rooks: Bitset,
    queens: Bitset,
    king: Bitset,
}

impl PlayerBoard {
    fn bitset(&self) -> Bitset {
        self.pawns | self.bishops | self.knights | self.rooks | self.queens | self.king
    }

    fn captured(&mut self, index: u8) {
        self.pawns = self.pawns.clear(index);
        self.bishops = self.bishops.clear(index);
        self.knights = self.knights.clear(index);
        self.rooks = self.rooks.clear(index);
        self.queens = self.queens.clear(index);
        self.king = self.king.clear(index);
    }

    fn score(&self) -> f64 {
        let score = self.pawns.count()
            + self.bishops.count()*3
            + self.knights.count()*3
            + self.rooks.count()*5
            + self.queens.count()*9
            + self.king.count()*1000;
        score.into()
    }
}

const FLAG_WHITE_CAN_CASTLE: u32 = 1;
const FLAG_BLACK_CAN_CASTLE: u32 = 1 << 1;

#[derive(Clone, Copy, PartialEq, Eq)]
struct Board {
    black: PlayerBoard,
    white: PlayerBoard,
    flags: u32,
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut board = [b'.'; 64];

        fmt_pieces(&mut board, self.white.king, b'K');
        fmt_pieces(&mut board, self.white.queens, b'Q');
        fmt_pieces(&mut board, self.white.rooks, b'R');
        fmt_pieces(&mut board, self.white.bishops, b'B');
        fmt_pieces(&mut board, self.white.knights, b'N');
        fmt_pieces(&mut board, self.white.pawns, b'P');

        fmt_pieces(&mut board, self.black.king, b'k');
        fmt_pieces(&mut board, self.black.queens, b'q');
        fmt_pieces(&mut board, self.black.rooks, b'r');
        fmt_pieces(&mut board, self.black.bishops, b'b');
        fmt_pieces(&mut board, self.black.knights, b'n');
        fmt_pieces(&mut board, self.black.pawns, b'p');

        fmt_board(&board, f)
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

fn fmt_pieces(board: &mut [u8; 64], pieces: Bitset, ch: u8) {
    for index in pieces.indices() {
        assert_eq!(board[index as usize], b'.');
        board[index as usize] = ch;
    }
}

fn fmt_board(board: &[u8; 64], f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let board_str = std::str::from_utf8(board).unwrap();
    for i in (8..=64).step_by(8) {
        write!(f, "{}\n", &board_str[i-8..i])?;
    }
    Ok(())
}

const ROW_0: Bitset = Bitset(0xff);
const ROW_1: Bitset = Bitset(0xff << 8);
const ROW_2: Bitset = Bitset(0xff << 16);
const ROW_3: Bitset = Bitset(0xff << 24);
const ROW_4: Bitset = Bitset(0xff << 32);
const ROW_5: Bitset = Bitset(0xff << 40);
const ROW_6: Bitset = Bitset(0xff << 48);
const ROW_7: Bitset = Bitset(0xff << 56);

const ROW_2_TO_7: Bitset = Bitset(u64::MAX ^ ROW_0.0 ^ ROW_1.0);
const ROW_0_TO_5: Bitset = Bitset(u64::MAX ^ ROW_6.0 ^ ROW_7.0);

const COLUMN_0: Bitset = Bitset(1 | (1 << 8) | (1 << 16) | (1 << 24) | (1 << 32) | (1 << 40) | (1 << 48) | (1 << 56));
const COLUMN_1: Bitset = Bitset(COLUMN_0.0 << 1);
const COLUMN_2: Bitset = Bitset(COLUMN_0.0 << 2);
const COLUMN_3: Bitset = Bitset(COLUMN_0.0 << 3);
const COLUMN_4: Bitset = Bitset(COLUMN_0.0 << 4);
const COLUMN_5: Bitset = Bitset(COLUMN_0.0 << 5);
const COLUMN_6: Bitset = Bitset(COLUMN_0.0 << 6);
const COLUMN_7: Bitset = Bitset(COLUMN_0.0 << 7);

const COLUMN_0_TO_6: Bitset = Bitset(u64::MAX ^ COLUMN_7.0);
const COLUMN_1_TO_7: Bitset = Bitset(u64::MAX ^ COLUMN_0.0);

impl Board {
    fn start() -> Self {
        let black_pawns = (0..8).map(|x| Bitset::from_position(x, 1))
            .fold(Bitset::zero(), |pawns, pawn| pawns | pawn);
        let white_pawns = (0..8).map(|x| Bitset::from_position(x, 6))
            .fold(Bitset::zero(), |pawns, pawn| pawns | pawn);
        let board = Self {
            black: PlayerBoard {
                rooks: Bitset::from_position(0, 0) | Bitset::from_position(7, 0),
                knights: Bitset::from_position(1, 0) | Bitset::from_position(6, 0),
                bishops: Bitset::from_position(2, 0) | Bitset::from_position(5, 0),
                queens: Bitset::from_position(3, 0),
                king: Bitset::from_position(4, 0),
                pawns: black_pawns,
            },
            white: PlayerBoard {
                rooks: Bitset::from_position(0, 7) | Bitset::from_position(7, 7),
                knights: Bitset::from_position(1, 7) | Bitset::from_position(6, 7),
                bishops: Bitset::from_position(2, 7) | Bitset::from_position(5, 7),
                queens: Bitset::from_position(3, 7),
                king: Bitset::from_position(4, 7),
                pawns: white_pawns,
            },
            flags: FLAG_BLACK_CAN_CASTLE | FLAG_WHITE_CAN_CASTLE,
        };
        debug_assert_eq!(board.bitset().count(), 32);
        board
    }

    fn bitset(&self) -> Bitset {
        self.white.bitset() | self.black.bitset()
    }

    fn debug_check(&self) {
        debug_assert_eq!((self.white.pawns & ROW_0).count(), 0);
        debug_assert_eq!((self.white.pawns & ROW_7).count(), 0);
        debug_assert_eq!((self.black.pawns & ROW_0).count(), 0);
        debug_assert_eq!((self.black.pawns & ROW_7).count(), 0);

        debug_assert!(self.white.pawns.count() <= 8);
        debug_assert!(self.black.pawns.count() <= 8);

        debug_assert_eq!(self.white.king.count(), 1);
        debug_assert_eq!(self.black.king.count(), 1);

        debug_assert!(self.white.bitset().count() <= 16);
        debug_assert!(self.black.bitset().count() <= 16);
        debug_assert!(self.bitset().count() <= 32);
    }

    fn score(&self) -> f64 {
        self.black.score() - self.white.score()
    }

    fn white_moves(&self, remainder: usize, en_passant_index: Option<u8>) -> (f64, Move) {
        self.debug_check();
        if remainder <= 1 {
            return (self.score(), Move::None);
        }
        let remainder = remainder-1;
        let mut score = Score::new();
        self.white_pawns(&mut score, remainder, en_passant_index);
        score.finalize()
    }

    fn black_moves(&self, remainder: usize, en_passant_index: Option<u8>) -> (f64, Move) {
        self.debug_check();
        if remainder <= 1 {
            return (self.score(), Move::None);
        }
        let remainder = remainder-1;
        let mut score = Score::new();
        self.black_pawns(&mut score, remainder, en_passant_index);
        score.finalize()
    }

    fn white_pawns(&self, score: &mut Score, remainder: usize, en_passant_index: Option<u8>) {
        let all_pieces = self.bitset();
        let mut next_board = self.clone();

        self.white_pawns_move_without_promote(score, remainder, &mut next_board, all_pieces);
        self.white_pawns_capture_without_promote(score, remainder, &mut next_board);

        self.white_pawns_move_with_promote(score, remainder, &mut next_board, all_pieces);
        self.white_pawns_capture_with_promote(score, remainder, &mut next_board);

        if let Some(en_passant_index) = en_passant_index {
            self.white_en_passant(score, remainder, &mut next_board, en_passant_index);
        }
    }

    fn white_pawn_move(
        &self,
        remainder: usize,
        next_board: &mut Self,
        from: u8,
        to: u8,
        en_passant_index: Option<u8>,
    ) -> (f64, Move) {
        next_board.white.pawns = next_board.white.pawns.clear(from).set(to);
        let (avg, _) = next_board.black_moves(remainder, en_passant_index);
        next_board.white.pawns = self.white.pawns;
        (avg, Move::Pawn { from, to })
    }

    fn white_pawn_capture(
        &self,
        remainder: usize,
        next_board: &mut Self,
        captured: u8,
        from: u8,
        to: u8,
    ) -> (f64, Move) {
        next_board.black.captured(captured);
        next_board.white.pawns = next_board.white.pawns.clear(from).set(to);
        let (avg, _) = next_board.black_moves(remainder, None);
        next_board.white.pawns = self.white.pawns;
        next_board.black = self.black;
        (avg, Move::Pawn { from, to })
    }

    fn white_pawn_promote(
        &self,
        remainder: usize,
        next_board: &mut Self,
        from: u8,
        to: u8,
    ) -> (f64, Move) {
        next_board.white.pawns = next_board.white.pawns.clear(from);
        // TODO: support other figures
        next_board.white.queens = next_board.white.queens.set(to);
        let (avg, _) = next_board.black_moves(remainder, None);
        next_board.white.queens = self.white.queens;
        next_board.white.pawns = self.white.pawns;
        (avg, Move::Pawn { from, to })
    }

    fn white_pawn_capture_promote(
        &self,
        remainder: usize,
        next_board: &mut Self,
        from: u8,
        to: u8,
    ) -> (f64, Move) {
        next_board.black.captured(to);
        next_board.white.pawns = next_board.white.pawns.clear(from);
        // TODO: support other figures
        next_board.white.queens = next_board.white.queens.set(to);
        let (avg, _) = next_board.black_moves(remainder, None);
        next_board.white.queens = self.white.queens;
        next_board.white.pawns = self.white.pawns;
        next_board.black = self.black;
        (avg, Move::Pawn { from, to })
    }

    fn white_pawns_move_without_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self, all_pieces: Bitset) {
        let single_step = ((self.white.pawns & ROW_2_TO_7) >> 8) & !all_pieces;
        for index in single_step.indices() {
            score.update_white(self.white_pawn_move(remainder, next_board, index+8, index, None));
        }

        let double_step = {
            let a = ((self.white.pawns & ROW_6) >> 8) & !all_pieces;
            (a >> 8) & !all_pieces
        };
        for index in double_step.indices() {
            score.update_white(self.white_pawn_move(remainder, next_board, index+16, index, Some(index)));
        }
    }

    fn white_pawns_capture_without_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self) {
        let black_pieces = self.black.bitset();

        let capture_right = ((self.white.pawns & COLUMN_0_TO_6 & ROW_2_TO_7) >> 7) & black_pieces;
        for index in capture_right.indices() {
            score.update_white(self.white_pawn_capture(remainder, next_board, index, index+7, index));
        }

        let capture_left = ((self.white.pawns & COLUMN_1_TO_7 & ROW_2_TO_7) >> 9) & black_pieces;
        for index in capture_left.indices() {
            score.update_white(self.white_pawn_capture(remainder, next_board, index, index+9, index));
        }
    }

    fn white_pawns_move_with_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self, all_pieces: Bitset) {
        let step = ((self.white.pawns & ROW_1) >> 8) & !all_pieces;
        for index in step.indices() {
            score.update_white(self.white_pawn_promote(remainder, next_board, index+8, index));
        }
    }

    fn white_pawns_capture_with_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self) {
        let black_pieces = self.black.bitset();

        let capture_right = ((self.white.pawns & COLUMN_0_TO_6 & ROW_1) >> 7) & black_pieces;
        for index in capture_right.indices() {
            score.update_white(self.white_pawn_capture_promote(remainder, next_board, index+7, index));
        }

        let capture_left = ((self.white.pawns & COLUMN_1_TO_7 & ROW_1) >> 9) & black_pieces;
        for index in capture_left.indices() {
            score.update_white(self.white_pawn_capture_promote(remainder, next_board, index+9, index));
        }
    }

    fn white_en_passant(&self, score: &mut Score, remainder: usize, next_board: &mut Self, en_passant_index: u8) {
        debug_assert!(next_board.black.pawns.has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x > 0 {
            let left_neighbour_pawn = Bitset::from_position(x-1, y) & self.white.pawns;
            if !left_neighbour_pawn.is_empty() {
                score.update_white(self.white_pawn_capture(
                    remainder,
                    next_board,
                    en_passant_index,
                    left_neighbour_pawn.first_index(),
                    position_to_index(x, y-1),
                ));
            }
        }
        
        if x < 7 {
            let right_neighbour_pawn = Bitset::from_position(x+1, y) & self.white.pawns;
            if !right_neighbour_pawn.is_empty() {
                score.update_white(self.white_pawn_capture(
                    remainder,
                    next_board,
                    en_passant_index,
                    right_neighbour_pawn.first_index(),
                    position_to_index(x, y-1),
                ));
            }
        }
    }

    fn black_pawns(&self, score: &mut Score, remainder: usize, en_passant_index: Option<u8>) {
        let all_pieces = self.bitset();
        let mut next_board = self.clone();

        self.black_pawns_move_without_promote(score, remainder, &mut next_board, all_pieces);
        self.black_pawns_capture_without_promote(score, remainder, &mut next_board);

        self.black_pawns_move_with_promote(score, remainder, &mut next_board, all_pieces);
        self.black_pawns_capture_with_promote(score, remainder, &mut next_board);

        if let Some(en_passant_index) = en_passant_index {
            self.black_en_passant(score, remainder, &mut next_board, en_passant_index);
        }
    }

    fn black_pawn_move(
        &self,
        remainder: usize,
        next_board: &mut Self,
        from: u8,
        to: u8,
        en_passant_index: Option<u8>,
    ) -> (f64, Move) {
        next_board.black.pawns = next_board.black.pawns.clear(from).set(to);
        let (avg, _) = next_board.white_moves(remainder, en_passant_index);
        next_board.black.pawns = self.black.pawns;
        (avg, Move::Pawn { from, to })
    }

    fn black_pawn_capture(
        &self,
        remainder: usize,
        next_board: &mut Self,
        captured: u8,
        from: u8,
        to: u8,
    ) -> (f64, Move) {
        next_board.white.captured(captured);
        next_board.black.pawns = next_board.black.pawns.clear(from).set(to);
        let (avg, _) = next_board.white_moves(remainder, None);
        next_board.black.pawns = self.black.pawns;
        next_board.white = self.white;
        (avg, Move::Pawn { from, to })
    }

    fn black_pawn_promote(
        &self,
        remainder: usize,
        next_board: &mut Self,
        from: u8,
        to: u8,
    ) -> (f64, Move) {
        next_board.black.pawns = next_board.black.pawns.clear(from);
        // TODO: support other figures
        next_board.black.queens = next_board.black.queens.set(to);
        let (avg, _) = next_board.white_moves(remainder, None);
        next_board.black.queens = self.black.queens;
        next_board.black.pawns = self.black.pawns;
        (avg, Move::Pawn { from, to })
    }

    fn black_pawn_capture_promote(
        &self,
        remainder: usize,
        next_board: &mut Self,
        from: u8,
        to: u8,
    ) -> (f64, Move) {
        next_board.white.captured(to);
        next_board.black.pawns = next_board.black.pawns.clear(from);
        // TODO: support other figures
        next_board.black.queens = next_board.black.queens.set(to);
        let (avg, _) = next_board.white_moves(remainder, None);

        next_board.black.queens = self.black.queens;
        next_board.black.pawns = self.black.pawns;
        next_board.white = self.white;
        (avg, Move::Pawn { from, to })
    }

    fn black_pawns_move_without_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self, all_pieces: Bitset) {
        let single_step = ((self.black.pawns & ROW_0_TO_5) << 8) & !all_pieces;
        for index in single_step.indices() {
            score.update_black(self.black_pawn_move(remainder, next_board, index-8, index, None));
        }

        let double_step = {
            let a = ((self.black.pawns & ROW_1) << 8) & !all_pieces;
            (a << 8) & !all_pieces
        };
        for index in double_step.indices() {
            score.update_black(self.black_pawn_move(remainder, next_board, index-16, index, Some(index)));
        }
    }

    fn black_pawns_capture_without_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self) {
        let white_pieces = self.black.bitset();

        let capture_right = ((self.black.pawns & COLUMN_0_TO_6 & ROW_0_TO_5) << 9) & white_pieces;
        for index in capture_right.indices() {
            score.update_black(self.black_pawn_capture(remainder, next_board, index, index-9, index));
        }

        let capture_left = ((self.black.pawns & COLUMN_1_TO_7 & ROW_0_TO_5) << 7) & white_pieces;
        for index in capture_left.indices() {
            score.update_black(self.black_pawn_capture(remainder, next_board, index, index-7, index));
        }
    }

    fn black_pawns_move_with_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self, all_pieces: Bitset) {
        let step = ((self.black.pawns & ROW_6) << 8) & !all_pieces;
        for index in step.indices() {
            score.update_black(self.black_pawn_promote(remainder, next_board, index-8, index));
        }
    }

    fn black_pawns_capture_with_promote(&self, score: &mut Score, remainder: usize, next_board: &mut Self) {
        let white_pieces = self.black.bitset();

        let capture_right = ((self.black.pawns & COLUMN_0_TO_6 & ROW_6) << 9) & white_pieces;
        for index in capture_right.indices() {
            score.update_black(self.black_pawn_capture_promote(remainder, next_board, index-9, index));
        }

        let capture_left = ((self.black.pawns & COLUMN_1_TO_7 & ROW_6) << 7) & white_pieces;
        for index in capture_left.indices() {
            score.update_black(self.black_pawn_capture_promote(remainder, next_board, index-7, index));
        }
    }

    fn black_en_passant(&self, score: &mut Score, remainder: usize, next_board: &mut Self, en_passant_index: u8) {
        debug_assert!(next_board.white.pawns.has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x > 0 {
            let left_neighbour_pawn = Bitset::from_position(x-1, y) & self.black.pawns;
            if !left_neighbour_pawn.is_empty() {
                score.update_black(self.black_pawn_capture(
                    remainder,
                    next_board,
                    en_passant_index,
                    left_neighbour_pawn.first_index(),
                    position_to_index(x, y+1),
                ));
            }
        }

        if x < 7 {
            let right_neighbour_pawn = Bitset::from_position(x+1, y) & self.black.pawns;
            if !right_neighbour_pawn.is_empty() {
                score.update_black(self.black_pawn_capture(
                    remainder,
                    next_board,
                    en_passant_index,
                    right_neighbour_pawn.first_index(),
                    position_to_index(x, y+1),
                ));
            }
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy)]
enum Move {
    // TODO: white, black
    None,
    King { from: u8, to: u8 },
    Queen { from: u8, to: u8 },
    Rook { from: u8, to: u8 },
    Bishop { from: u8, to: u8 },
    Knight { from: u8, to: u8 },
    Pawn { from: u8, to: u8 },
    CastleLong,
    CastleShort
}

fn index_to_chess_position(index: u8) -> (char, char) {
    let (x, y) = index_to_position(index);
    ((b'a' + x).into(), char::from_digit((8-y).into(), 10).unwrap())
}

fn fmt_move(f: &mut fmt::Formatter<'_>, figure: &str, from_index: u8, to_index: u8) -> fmt::Result {
    let (from_x, from_y) = index_to_chess_position(from_index);
    let (to_x, to_y) = index_to_chess_position(to_index);
    write!(f, "{figure}: {from_x}{from_y} -> {to_x}{to_y}")
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Move::Pawn { from, to } => {
                fmt_move(f, "Pawn", from, to)
            },
            _ => todo!(),
        }
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}


struct Score {
    sum: f64,
    count: u32,
    best_avg: f64,
    best_move: Move,
}

impl Score {
    fn new() -> Self {
        // TODO: start with first legal move, if available
        Self {
            sum: 0.0,
            count: 0,
            best_avg: 0.0,
            best_move: Move::None,
        }
    }

    fn update_white(&mut self, (avg, mov): (f64, Move)) {
        self.sum += avg;
        self.count += 1;
        if avg < self.best_avg {
            self.best_avg = avg;
            self.best_move = mov;
        }
    }

    fn update_black(&mut self, (avg, mov): (f64, Move)) {
        self.sum += avg;
        self.count += 1;
        if avg > self.best_avg {
            self.best_avg = avg;
            self.best_move = mov;
        }
    }

    fn finalize(self) -> (f64, Move) {
        (self.sum / f64::from(self.count), self.best_move)
    }
}