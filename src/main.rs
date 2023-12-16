use std::{fmt::{self, Binary, Write}, ops::{BitOr, Not, BitAnd, Shl, Shr}, iter::zip, error::Error, io::{self, prelude::*}};

use itertools::Itertools;

type Result<T> = std::result::Result<T, Box<dyn Error>>;

const DEFAULT_REMAINDER: usize = 5;

fn main() -> Result<()> {
    unsafe { init() };
    UCI::new().read_eval_print_loop()
}

struct UCI {
    board: Board,
    next_move_color: Color,
    en_passant_index: Option<u8>,
}

impl UCI {
    fn new() -> Self {
        Self {
            board: Board::start(),
            next_move_color: Color::White,
            en_passant_index: None,
        }
    }

    fn read_eval_print_loop(&mut self) -> Result<()> {
        for command in io::stdin().lock().lines() {
            let command = command?;
            for response in self.read_eval_print(&command)? {
                println!("{response}");
            }
        }
        Err("unexpected end of input loop".into())
    }

    fn read_eval_print(&mut self, command: &str) -> Result<Vec<String>> {
        // TODO: complete UCI implementation

        let split = command.split(' ').collect::<Vec<_>>();
        let responses = match split.as_slice() {
            ["uci"] => vec![
                "id name Baby Chess Engine".to_string(),
                "uciok".to_string(),
            ],
            ["isready"] => vec!["readyok".to_string()],
            ["ucinewgame"] => {
                self.reset();
                Vec::new()
            },
            ["position", "startpos", "moves", raw_moves @ ..] => {
                // TODO: fen
                self.apply_moves(raw_moves)?;
                Vec::new()
            },
            ["go", ..] => {
                let mov = self.calculate_move()?;
                vec![format!("bestmove {}", mov.to_long_algebraic_notation()?)]
            }
            ["stop"] => vec![],
            ["quit"] => return Err("quit".into()),
            _ => return Err(format!("unknown command '{}'", command).into())
        };
        Ok(responses)
    }

    fn reset(&mut self) {
        self.board = Board::start();
        self.next_move_color = Color::White;
    }

    fn apply_moves(&mut self, raw_moves: &[&str]) -> Result<()> {
        self.reset();
        for raw_move in raw_moves {
            let mov = Move::from_long_algebraic_notation(raw_move)?;
            let (color, en_passant_index) = self.board.apply_move(mov)?;
            self.next_move_color = color.other();
            self.en_passant_index = en_passant_index;
        }
        Ok(())
    }

    fn calculate_move(&mut self) -> Result<Move> {
        let score = match self.next_move_color {
            Color::White => self.board.white_moves(DEFAULT_REMAINDER, self.en_passant_index),
            Color::Black => self.board.black_moves(DEFAULT_REMAINDER, self.en_passant_index),
        };
        Ok(score.best_move)
    }
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

impl Binary for Bitset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

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
        fmt_pieces(&mut board, self.clone(), '#');
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

    fn ones() -> Self {
        Self(u64::MAX)
    }

    fn from_position(x: u8, y: u8) -> Self {
        Self::zero().with(position_to_index(x, y))
    }

    fn before(index: u8) -> Self {
        debug_assert!(index < 64);
        Self(u64::MAX.checked_shr((64-index).into()).unwrap_or(0))
    }

    fn after(index: u8) -> Self {
        debug_assert!(index < 64);
        Self(u64::MAX.checked_shl((index+1).into()).unwrap_or(0))
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

    fn try_first_index(self) -> Option<u8> {
        let trailing_zeros = self.0.trailing_zeros() as u8;
        if trailing_zeros < 64 { Some(trailing_zeros) } else { None }
    }

    fn try_last_index(self) -> Option<u8> {
        63u8.checked_sub(self.0.leading_zeros() as u8)
    }

    fn has(self, index: u8) -> bool {
        debug_assert!(index < 64);
        (self.0 & (1 << index)) != 0
    }

    fn checked_clear(&mut self, index: u8) {
        assert!(self.has(index));
        self.clear(index);
    }

    fn clear(&mut self, index: u8) {
        *self = self.without(index);
    }

    fn without(self, index: u8) -> Self {
        debug_assert!(index < 64);
        Self(self.0 & !(1 << index))
    }

    fn checked_set(&mut self, index: u8) {
        assert!(!self.has(index));
        self.set(index);
    }

    fn set(&mut self, index: u8) {
        *self = self.with(index);
    }

    fn with(self, index: u8) -> Self {
        debug_assert!(index < 64);
        Self(self.0 | (1 << index))
    }

    fn mov(&mut self, from: u8, to: u8) {
        self.clear(from);
        self.set(to);
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
            self.0.clear(index);
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
    fn debug_check(&self) {
        let count = self.pawns.count()
            + self.bishops.count()
            + self.knights.count()
            + self.rooks.count()
            + self.queens.count()
            + self.king.count();
        debug_assert_eq!(count, self.bitset().count());

        debug_assert_eq!((self.pawns & ROW_0).count(), 0);
        debug_assert_eq!((self.pawns & ROW_7).count(), 0);

        debug_assert_eq!(self.king.count(), 1);
        debug_assert!(self.pawns.count() <= 8);
        debug_assert!(self.bitset().count() <= 16);
    }

    fn bitset(&self) -> Bitset {
        self.pawns | self.bishops | self.knights | self.rooks | self.queens | self.king
    }

    fn captured(&mut self, index: u8) {
        self.pawns.clear(index);
        self.bishops.clear(index);
        self.knights.clear(index);
        self.rooks.clear(index);
        self.queens.clear(index);
        self.king.clear(index);
    }

    fn score(&self) -> f64 {
        let score = self.pawns.count()
            + self.bishops.count()*3
            + self.knights.count()*3
            + self.rooks.count()*5
            + self.queens.count()*9;
        score.into()
    }

    fn which_piece(&self, index: u8) -> Option<Piece> {
        if self.pawns.has(index) {
            Some(Piece::Pawn)
        } else if self.bishops.has(index) {
            Some(Piece::Bishop)
        } else if self.knights.has(index) {
            Some(Piece::Knight)
        } else if self.rooks.has(index) {
            Some(Piece::Rook)
        } else if self.queens.has(index) {
            Some(Piece::Queen)
        } else if self.king.has(index) {
            Some(Piece::King)
        } else {
            None
        }
    }

    fn remove_piece(&mut self, piece: Piece, index: u8) {
        match piece {
            Piece::Pawn => self.pawns.checked_clear(index),
            Piece::Bishop => self.bishops.checked_clear(index),
            Piece::Knight => self.knights.checked_clear(index),
            Piece::Rook => self.rooks.checked_clear(index),
            Piece::Queen => self.queens.checked_clear(index),
            Piece::King => self.king.checked_clear(index),
        }
    }

    fn place_piece(&mut self, piece: Piece, index: u8) {
        match piece {
            Piece::Pawn => self.pawns.checked_set(index),
            Piece::Bishop => self.bishops.checked_set(index),
            Piece::Knight => self.knights.checked_set(index),
            Piece::Rook => self.rooks.checked_set(index),
            Piece::Queen => self.queens.checked_set(index),
            Piece::King => self.king.checked_set(index),
        }
    }

    fn normal_move(&mut self, enemy: &mut Self, from: u8, to: u8) -> Option<u8> {
        let piece = self.which_piece(from).unwrap();
        self.remove_piece(piece, from);
        enemy.captured(to);
        self.place_piece(piece, to);
        self.possible_en_passant(Move::Normal { from, to })
    }

    fn possible_en_passant(&self, mov: Move) -> Option<u8> {
        let Move::Normal { from, to } = mov else {
            return None;
        };
        if !self.pawns.has(from) {
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

    fn promotion_move(&mut self, enemy: &mut Self, piece: Piece, from: u8, to: u8) {
        self.pawns.checked_clear(from);
        enemy.captured(to);
        self.place_piece(piece, to);
    }
}

static mut DIAGONALS_LEFT: [Bitset; 64] = [Bitset(0); 64];
static mut DIAGONALS_RIGHT: [Bitset; 64] = [Bitset(0); 64];
static mut KING_MOVES: [Bitset; 64] = [Bitset(0); 64];
static mut KNIGHT_MOVES: [Bitset; 64] = [Bitset(0); 64];

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

unsafe fn init() {
    debug_assert_eq!(ROW_2_TO_7.count(), 48);
    init_diagonals();
    init_king_moves();
    init_knight_moves();
}

fn positions_to_bitset(positions: impl Iterator<Item = (u8, u8)>) -> Bitset {
    positions.fold(Bitset::zero(), |s, (x, y)| s.with(position_to_index(x, y)))
}

unsafe fn init_diagonals() {
    let (left, right) = diagonals();
    for index in 0..64 {
        debug_assert_eq!(left.iter().filter(|s| s.has(index)).count(), 1);
        debug_assert_eq!(right.iter().filter(|s| s.has(index)).count(), 1);
        DIAGONALS_LEFT[usize::from(index)] = *left.iter().find(|s| s.has(index)).unwrap();
        DIAGONALS_RIGHT[usize::from(index)] = *right.iter().find(|s| s.has(index)).unwrap();
    }
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

unsafe fn init_king_moves() {
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

unsafe fn init_knight_moves() {
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
struct Moves {
    moves: Bitset,
    captures: Bitset,
}

impl Moves {
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

    fn bishop(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let Self { moves: left_without, captures: left_with } =
            Self::diagonal_left(all_pieces, index, enemy_pieces);
        let Self { moves: right_without, captures: right_with } =
            Self::diagonal_right(all_pieces, index, enemy_pieces);
        Self { moves: left_without|right_without, captures: left_with|right_with }
    }

    fn rook(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let Self { moves: row_without, captures: row_with } =
            Self::row(all_pieces, index, enemy_pieces);
        let Self { moves: column_without, captures: column_with } =
            Self::column(all_pieces, index, enemy_pieces);
        Self { moves: row_without|column_without, captures: row_with|column_with }
    }

    fn queen(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let Self { moves: bishop_without, captures: bishop_with } =
            Self::bishop(all_pieces, index, enemy_pieces);
        let Self { moves: rook_without, captures: rook_with } =
            Self::rook(all_pieces, index, enemy_pieces);
        Self { moves: bishop_without|rook_without, captures: bishop_with|rook_with }
    }

    fn king(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let possible_moves = king_move(index);
        Self {
            moves: possible_moves & !all_pieces,
            captures: possible_moves & enemy_pieces,
        }
    }

    fn knight(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let possible_moves = knight_move(index);
        Self {
            moves: possible_moves & !all_pieces,
            captures: possible_moves & enemy_pieces,
        }
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

        fmt_pieces(&mut board, self.white.king, Piece::King.to_symbol().to_ascii_uppercase());
        fmt_pieces(&mut board, self.white.queens, Piece::Queen.to_symbol().to_ascii_uppercase());
        fmt_pieces(&mut board, self.white.rooks, Piece::Rook.to_symbol().to_ascii_uppercase());
        fmt_pieces(&mut board, self.white.bishops, Piece::Bishop.to_symbol().to_ascii_uppercase());
        fmt_pieces(&mut board, self.white.knights, Piece::Knight.to_symbol().to_ascii_uppercase());
        fmt_pieces(&mut board, self.white.pawns, Piece::Pawn.to_symbol().to_ascii_uppercase());

        fmt_pieces(&mut board, self.black.king, Piece::King.to_symbol().to_ascii_lowercase());
        fmt_pieces(&mut board, self.black.queens, Piece::Queen.to_symbol().to_ascii_lowercase());
        fmt_pieces(&mut board, self.black.rooks, Piece::Rook.to_symbol().to_ascii_lowercase());
        fmt_pieces(&mut board, self.black.bishops, Piece::Bishop.to_symbol().to_ascii_lowercase());
        fmt_pieces(&mut board, self.black.knights, Piece::Knight.to_symbol().to_ascii_lowercase());
        fmt_pieces(&mut board, self.black.pawns, Piece::Pawn.to_symbol().to_ascii_lowercase());

        fmt_board(&board, f)
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

fn fmt_pieces(board: &mut [u8; 64], pieces: Bitset, ch: char) {
    for index in pieces.indices() {
        assert_eq!(board[index as usize], b'.');
        board[index as usize] = u8::try_from(ch).unwrap();
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
const ROW_6: Bitset = Bitset(0xff << 48);
const ROW_7: Bitset = Bitset(0xff << 56);

const ROW_2_TO_7: Bitset = Bitset(u64::MAX ^ ROW_0.0 ^ ROW_1.0);
const ROW_0_TO_5: Bitset = Bitset(u64::MAX ^ ROW_6.0 ^ ROW_7.0);

const COLUMN_0: Bitset = Bitset(1 | (1 << 8) | (1 << 16) | (1 << 24) | (1 << 32) | (1 << 40) | (1 << 48) | (1 << 56));
const COLUMN_7: Bitset = Bitset(COLUMN_0.0 << 7);

const COLUMN_0_TO_6: Bitset = Bitset(u64::MAX ^ COLUMN_7.0);
const COLUMN_1_TO_7: Bitset = Bitset(u64::MAX ^ COLUMN_0.0);

impl Board {
    fn default_flags() -> u32 {
        FLAG_BLACK_CAN_CASTLE | FLAG_WHITE_CAN_CASTLE
    }

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
            flags: Self::default_flags(),
        };
        debug_assert_eq!(board.bitset().count(), 32);
        board
    }

    fn apply_move(&mut self, mov: Move) -> Result<(Color, Option<u8>)> {
        // TODO: check move is legal
        match mov {
            Move::None => Err("cannot apply none move".into()),
            Move::Normal { from, to } => {
                if self.white.bitset().has(from) {
                    let en_passant_index = self.white.normal_move(&mut self.black, from, to);
                    Ok((Color::White, en_passant_index))
                } else if self.black.bitset().has(from) {
                    let en_passant_index = self.black.normal_move(&mut self.white, from, to);
                    Ok((Color::Black, en_passant_index))
                } else {
                    Err(format!(
                        "illegal move {}: from empty square",
                        Move::long_algebraic_notation_normal(from, to),
                    ).into())
                }
            },
            Move::Promotion { piece, from, to } => {
                if self.white.pawns.has(from) {
                    self.white.promotion_move(&mut self.black, piece, from, to);
                    Ok((Color::White, None))
                } else if self.black.pawns.has(from) {
                    self.black.promotion_move(&mut self.white, piece, from, to);
                    Ok((Color::Black, None))
                } else {
                    Err(format!(
                        "illegal move {}: from square is not a pawn",
                        Move::long_algebraic_notation_normal(from, to),
                    ).into())
                }
            },
        }
    }

    fn bitset(&self) -> Bitset {
        self.white.bitset() | self.black.bitset()
    }

    fn debug_check(&self) {
        self.white.debug_check();
        self.black.debug_check();
        debug_assert_eq!(
            self.white.bitset().count() + self.black.bitset().count(),
            self.bitset().count(),
        );
        debug_assert!(self.bitset().count() <= 32);
    }

    fn white_apply_moves(
        &self,
        context: &mut Context,
        get_piece_bitset: impl Fn(&PlayerBoard) -> &Bitset,
        get_piece_bitset_mut: impl Fn(&mut PlayerBoard) -> &mut Bitset,
        get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
    ) {
        let all_pieces = self.bitset();
        let enemy_pieces = self.black.bitset();

        for from in get_piece_bitset(&self.white).indices() {
            let Moves { moves, captures } = get_moves(all_pieces, from, enemy_pieces);
            for to in moves.indices() {
                get_piece_bitset_mut(&mut context.next.white).mov(from, to);
                context.score.update_white(
                    context.next.black_moves(context.remainder, None),
                    Move::Normal { from, to },
                );
                *get_piece_bitset_mut(&mut context.next.white) = *get_piece_bitset(&self.white);
            }
            for to in captures.indices() {
                context.next.black.captured(to);
                get_piece_bitset_mut(&mut context.next.white).mov(from, to);
                context.score.update_white(
                    context.next.black_moves(context.remainder, None),
                    Move::Normal { from, to },
                );
                *get_piece_bitset_mut(&mut context.next.white) = *get_piece_bitset(&self.white);
                context.next.black = self.black;
            }
        }
    }

    fn black_apply_moves(
        &self,
        context: &mut Context,
        get_piece_bitset: impl Fn(&PlayerBoard) -> &Bitset,
        get_piece_bitset_mut: impl Fn(&mut PlayerBoard) -> &mut Bitset,
        get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
    ) {
        let all_pieces = self.bitset();
        let enemy_pieces = self.white.bitset();

        for from in get_piece_bitset(&self.black).indices() {
            let Moves { moves, captures } = get_moves(all_pieces, from, enemy_pieces);
            for to in moves.indices() {
                get_piece_bitset_mut(&mut context.next.black).mov(from, to);
                context.score.update_black(
                    context.next.white_moves(context.remainder, None),
                    Move::Normal { from, to },
                );
                *get_piece_bitset_mut(&mut context.next.black) = *get_piece_bitset(&self.black);
            }
            for to in captures.indices() {
                context.next.white.captured(to);
                get_piece_bitset_mut(&mut context.next.black).mov(from, to);
                context.score.update_black(
                    context.next.white_moves(context.remainder, None),
                    Move::Normal { from, to },
                );
                *get_piece_bitset_mut(&mut context.next.black) = *get_piece_bitset(&self.black);
                context.next.white = self.white;
            }
        }
    }

    fn white_moves(&self, remainder: usize, en_passant_index: Option<u8>) -> Score {
        if self.white.king.is_empty() {
            return Score::checkmate_white();
        }

        self.debug_check();
        if remainder == 0 {
            return Score::board(self);
        }
        let remainder = remainder-1;

        let mut context = Context::new(self, remainder);
        self.white_pawns(&mut context, en_passant_index);
        self.white_queens(&mut context);
        self.white_rooks(&mut context);
        self.white_bishops(&mut context);
        self.white_knights(&mut context);
        self.white_king(&mut context);
        context.score.finalize()
    }

    fn black_moves(&self, remainder: usize, en_passant_index: Option<u8>) -> Score {
        if self.black.king.is_empty() {
            return Score::checkmate_black();
        }

        self.debug_check();
        if remainder == 0 {
            return Score::board(self);
        }
        let remainder = remainder-1;

        let mut context = Context::new(self, remainder);
        self.black_pawns(&mut context, en_passant_index);
        self.black_queens(&mut context);
        self.black_rooks(&mut context);
        self.black_bishops(&mut context);
        self.black_knights(&mut context);
        self.black_king(&mut context);
        context.score.finalize()
    }

    fn white_king(&self, context: &mut Context) {
        self.white_apply_moves(
            context,
            |player_board| &player_board.king,
            |player_board| &mut player_board.king,
            Moves::king,
        );
    }

    fn black_king(&self, context: &mut Context) {
        self.black_apply_moves(
            context,
            |player_board| &player_board.king,
            |player_board| &mut player_board.king,
            Moves::king,
        );
    }

    fn white_knights(&self, context: &mut Context) {
        self.white_apply_moves(
            context,
            |player_board| &player_board.knights,
            |player_board| &mut player_board.knights,
            Moves::knight,
        );
    }

    fn black_knights(&self, context: &mut Context) {
        self.black_apply_moves(
            context,
            |player_board| &player_board.knights,
            |player_board| &mut player_board.knights,
            Moves::knight,
        );
    }

    fn white_bishops(&self, context: &mut Context) {
        self.white_apply_moves(
            context,
            |player_board| &player_board.bishops,
            |player_board| &mut player_board.bishops,
            Moves::bishop,
        );
    }

    fn black_bishops(&self, context: &mut Context) {
        self.black_apply_moves(
            context,
            |player_board| &player_board.bishops,
            |player_board| &mut player_board.bishops,
            Moves::bishop,
        );
    }

    fn white_rooks(&self, context: &mut Context) {
        self.white_apply_moves(
            context,
            |player_board| &player_board.rooks,
            |player_board| &mut player_board.rooks,
            Moves::rook,
        );
    }

    fn black_rooks(&self, context: &mut Context) {
        self.black_apply_moves(
            context,
            |player_board| &player_board.rooks,
            |player_board| &mut player_board.rooks,
            Moves::rook,
        );
    }

    fn white_queens(&self, context: &mut Context) {
        self.white_apply_moves(
            context,
            |player_board| &player_board.queens,
            |player_board| &mut player_board.queens,
            Moves::queen,
        );
    }

    fn black_queens(&self, context: &mut Context) {
        self.black_apply_moves(
            context,
            |player_board| &player_board.queens,
            |player_board| &mut player_board.queens,
            Moves::queen,
        );
    }

    fn white_pawns(&self, context: &mut Context, en_passant_index: Option<u8>) {
        self.white_pawns_move_without_promote(context);
        self.white_pawns_capture_without_promote(context);

        self.white_pawns_move_with_promote(context);
        self.white_pawns_capture_with_promote(context);

        if let Some(en_passant_index) = en_passant_index {
            self.white_en_passant(context, en_passant_index);
        }
    }

    fn white_pawn_move(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
        en_passant_index: Option<u8>,
    ) {
        context.next.white.pawns = context.next.white.pawns.without(from).with(to);
        context.score.update_white(
            context.next.black_moves(context.remainder, en_passant_index),
            Move::Normal { from, to },
        );
        context.next.white.pawns = self.white.pawns;
    }

    fn white_pawn_capture(
        &self,
        context: &mut Context,
        captured: u8,
        from: u8,
        to: u8,
    ) {
        context.next.black.captured(captured);
        context.next.white.pawns = context.next.white.pawns.without(from).with(to);
        context.score.update_white(
            context.next.black_moves(context.remainder, None), 
            Move::Normal { from, to },
        );
        context.next.white.pawns = self.white.pawns;
        context.next.black = self.black;
    }

    fn white_pawn_promote(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
    ) {
        context.next.white.pawns.clear(from);
        self.white_pawn_promote_figures(context, from, to);
        context.next.white.pawns = self.white.pawns;
    }

    fn white_pawn_capture_promote(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
    ) {
        context.next.black.captured(to);
        context.next.white.pawns.clear(from);
        self.white_pawn_promote_figures(context, from, to);
        context.next.white.pawns = self.white.pawns;
        context.next.black = self.black;
    }

    fn white_pawn_promote_figures(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
    ) {
        context.next.white.queens.set(to);
        context.score.update_white(
            context.next.black_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Queen, from, to },
        );
        context.next.white.queens = self.white.queens;

        context.next.white.rooks.set(to);
        context.score.update_white(
            context.next.black_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Rook, from, to },
        );
        context.next.white.rooks = self.white.rooks;

        context.next.white.knights.set(to);
        context.score.update_white(
            context.next.black_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Knight, from, to },
        );
        context.next.white.knights = self.white.knights;

        context.next.white.bishops.set(to);
        context.score.update_white(
            context.next.black_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Bishop, from, to },
        );
        context.next.white.bishops = self.white.bishops;
    }

    fn white_pawns_move_without_promote(&self, context: &mut Context) {
        let all_pieces = self.bitset();

        let single_step = ((self.white.pawns & ROW_2_TO_7) >> 8) & !all_pieces;
        for index in single_step.indices() {
            self.white_pawn_move(context, index+8, index, None);
        }

        let double_step = {
            let a = ((self.white.pawns & ROW_6) >> 8) & !all_pieces;
            (a >> 8) & !all_pieces
        };
        for index in double_step.indices() {
            self.white_pawn_move(context, index+16, index, Some(index));
        }
    }

    fn white_pawns_capture_without_promote(&self, context: &mut Context) {
        let black_pieces = self.black.bitset();

        let capture_right = ((self.white.pawns & COLUMN_0_TO_6 & ROW_2_TO_7) >> 7) & black_pieces;
        for index in capture_right.indices() {
            self.white_pawn_capture(context, index, index+7, index);
        }

        let capture_left = ((self.white.pawns & COLUMN_1_TO_7 & ROW_2_TO_7) >> 9) & black_pieces;
        for index in capture_left.indices() {
            self.white_pawn_capture(context, index, index+9, index);
        }
    }

    fn white_pawns_move_with_promote(&self, context: &mut Context) {
        let all_pieces = self.bitset();
        let step = ((self.white.pawns & ROW_1) >> 8) & !all_pieces;
        for index in step.indices() {
            self.white_pawn_promote(context, index+8, index);
        }
    }

    fn white_pawns_capture_with_promote(&self, context: &mut Context) {
        let black_pieces = self.black.bitset();

        let capture_right = ((self.white.pawns & COLUMN_0_TO_6 & ROW_1) >> 7) & black_pieces;
        for index in capture_right.indices() {
            self.white_pawn_capture_promote(context, index+7, index);
        }

        let capture_left = ((self.white.pawns & COLUMN_1_TO_7 & ROW_1) >> 9) & black_pieces;
        for index in capture_left.indices() {
            self.white_pawn_capture_promote(context, index+9, index);
        }
    }

    fn white_en_passant(&self, context: &mut Context, en_passant_index: u8) {
        debug_assert!(context.next.black.pawns.has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x > 0 {
            let left_neighbour_pawn = Bitset::from_position(x-1, y) & self.white.pawns;
            if !left_neighbour_pawn.is_empty() {
                self.white_pawn_capture(
                    context,
                    en_passant_index,
                    left_neighbour_pawn.first_index(),
                    position_to_index(x, y-1),
                );
            }
        }

        if x < 7 {
            let right_neighbour_pawn = Bitset::from_position(x+1, y) & self.white.pawns;
            if !right_neighbour_pawn.is_empty() {
                self.white_pawn_capture(
                    context,
                    en_passant_index,
                    right_neighbour_pawn.first_index(),
                    position_to_index(x, y-1),
                );
            }
        }
    }

    fn black_pawns(&self, context: &mut Context, en_passant_index: Option<u8>) {
        self.black_pawns_move_without_promote(context);
        self.black_pawns_capture_without_promote(context);

        self.black_pawns_move_with_promote(context);
        self.black_pawns_capture_with_promote(context);

        if let Some(en_passant_index) = en_passant_index {
            self.black_en_passant(context, en_passant_index);
        }
    }

    fn black_pawn_move(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
        en_passant_index: Option<u8>,
    ) {
        context.next.black.pawns = context.next.black.pawns.without(from).with(to);
        context.score.update_black(
            context.next.white_moves(context.remainder, en_passant_index),
            Move::Normal { from, to },
        );
        context.next.black.pawns = self.black.pawns;
    }

    fn black_pawn_capture(
        &self,
        context: &mut Context,
        captured: u8,
        from: u8,
        to: u8,
    ) {
        context.next.white.captured(captured);
        context.next.black.pawns = context.next.black.pawns.without(from).with(to);
        context.score.update_black(
            context.next.white_moves(context.remainder, None),
            Move::Normal { from, to },
        );
        context.next.black.pawns = self.black.pawns;
        context.next.white = self.white;
    }

    fn black_pawn_promote(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
    ) {
        context.next.black.pawns.clear(from);
        self.black_pawn_promote_figures(context, from, to);
        context.next.black.pawns = self.black.pawns;
    }

    fn black_pawn_capture_promote(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
    ) {
        context.next.white.captured(to);
        context.next.black.pawns.clear(from);
        self.black_pawn_promote_figures(context, from, to);
        context.next.black.pawns = self.black.pawns;
        context.next.white = self.white;
    }

    fn black_pawn_promote_figures(
        &self,
        context: &mut Context,
        from: u8,
        to: u8,
    ) {
        context.next.black.queens.set(to);
        context.score.update_black(
            context.next.white_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Queen, from, to },
        );
        context.next.black.queens = self.black.queens;

        context.next.black.rooks.set(to);
        context.score.update_black(
            context.next.white_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Rook, from, to },
        );
        context.next.black.rooks = self.black.rooks;

        context.next.black.knights.set(to);
        context.score.update_black(
            context.next.white_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Knight, from, to },
        );
        context.next.black.knights = self.black.knights;

        context.next.black.bishops.set(to);
        context.score.update_black(
            context.next.white_moves(context.remainder, None), 
            Move::Promotion { piece: Piece::Bishop, from, to },
        );
        context.next.black.bishops = self.black.bishops;
    }

    fn black_pawns_move_without_promote(&self, context: &mut Context) {
        let all_pieces = self.bitset();

        let single_step = ((self.black.pawns & ROW_0_TO_5) << 8) & !all_pieces;
        for index in single_step.indices() {
            self.black_pawn_move(context, index-8, index, None);
        }

        let double_step = {
            let a = ((self.black.pawns & ROW_1) << 8) & !all_pieces;
            (a << 8) & !all_pieces
        };
        for index in double_step.indices() {
            self.black_pawn_move(context, index-16, index, Some(index));
        }
    }

    fn black_pawns_capture_without_promote(&self, context: &mut Context) {
        let white_pieces = self.white.bitset();

        let capture_right = ((self.black.pawns & COLUMN_0_TO_6 & ROW_0_TO_5) << 9) & white_pieces;
        for index in capture_right.indices() {
            self.black_pawn_capture(context, index, index-9, index);
        }

        let capture_left = ((self.black.pawns & COLUMN_1_TO_7 & ROW_0_TO_5) << 7) & white_pieces;
        for index in capture_left.indices() {
            self.black_pawn_capture(context, index, index-7, index);
        }
    }

    fn black_pawns_move_with_promote(&self, context: &mut Context) {
        let all_pieces = self.bitset();
        let step = ((self.black.pawns & ROW_6) << 8) & !all_pieces;
        for index in step.indices() {
            self.black_pawn_promote(context, index-8, index);
        }
    }

    fn black_pawns_capture_with_promote(&self, context: &mut Context) {
        let white_pieces = self.black.bitset();

        let capture_right = ((self.black.pawns & COLUMN_0_TO_6 & ROW_6) << 9) & white_pieces;
        for index in capture_right.indices() {
            self.black_pawn_capture_promote(context, index-9, index);
        }

        let capture_left = ((self.black.pawns & COLUMN_1_TO_7 & ROW_6) << 7) & white_pieces;
        for index in capture_left.indices() {
            self.black_pawn_capture_promote(context, index-7, index);
        }
    }

    fn black_en_passant(&self, context: &mut Context, en_passant_index: u8) {
        debug_assert!(context.next.white.pawns.has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x > 0 {
            let left_neighbour_pawn = Bitset::from_position(x-1, y) & self.black.pawns;
            if !left_neighbour_pawn.is_empty() {
                self.black_pawn_capture(
                    context,
                    en_passant_index,
                    left_neighbour_pawn.first_index(),
                    position_to_index(x, y+1),
                );
            }
        }

        if x < 7 {
            let right_neighbour_pawn = Bitset::from_position(x+1, y) & self.black.pawns;
            if !right_neighbour_pawn.is_empty() {
                self.black_pawn_capture(
                    context,
                    en_passant_index,
                    right_neighbour_pawn.first_index(),
                    position_to_index(x, y+1),
                );
            }
        }
    }
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Piece {
    Pawn,
    Bishop,
    Knight,
    Rook,
    Queen,
    King,
}

impl Piece {
    fn from_symbol(symbol: char) -> Result<Self> {
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

    fn to_symbol(self) -> char {
        match self {
            Piece::Pawn => 'p',
            Piece::Bishop => 'b',
            Piece::Knight => 'n',
            Piece::Rook => 'r',
            Piece::Queen => 'q',
            Piece::King => 'k',
        }
    }

    fn can_promote_to(&self) -> bool {
        matches!(*self, Piece::Queen | Piece::Rook | Piece::Knight | Piece::Bishop)
    }
}

#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq)]
enum Move {
    None,
    Normal { from: u8, to: u8 },
    Promotion { piece: Piece, from: u8, to: u8 },
}

impl Move {
    fn from_long_algebraic_notation(notation: &str) -> Result<Self> {
        if notation.len() != 4 && notation.len() != 5 {
            return Err(format!("expected chess notation, got '{}'", notation).into());
        }
        let notation = notation.as_bytes();
        let from = Self::chess_position_to_index(&notation[..2])?;
        let to = Self::chess_position_to_index(&notation[2..])?;
        if notation.len() == 5 {
            let symbol = char::from(notation[4]);
            let promote_to = Piece::from_symbol(symbol)?;
            if !promote_to.can_promote_to() {
                return Err(format!("cannot promote to {:?}", promote_to).into());
            }
            Ok(Self::Promotion { piece: promote_to, from, to })
        } else {
            Ok(Self::Normal { from, to })            
        }
    }

    fn to_long_algebraic_notation(&self) -> Result<String> {
        match *self {
            Move::None => Err("cannot convert none move to long algebraic notation".into()),
            Move::Normal { from, to } => Ok(Self::long_algebraic_notation_normal(from, to)),
            Move::Promotion { piece, from, to } => {
                assert!(piece.can_promote_to());
                let mut s = Self::long_algebraic_notation_normal(from, to);
                s.push(piece.to_symbol());
                Ok(s)
            },
        }
    }

    fn index_to_chess_position(index: u8) -> (char, char) {
        let (x, y) = index_to_position(index);
        ((b'a' + x).into(), char::from_digit((8-y).into(), 10).unwrap())
    }

    fn chess_position_to_index(raw_position: &[u8]) -> Result<u8> {
        assert_eq!(raw_position.len(), 2);
        let (raw_x, raw_y) = (raw_position[0], raw_position[1]);
        if !(b'a'..=b'h').contains(&raw_x) {
            return Err(format!("expected file, got '{}'", char::from(raw_x)).into());
        }
        if !(b'1'..=b'8').contains(&raw_y) {
            return Err(format!("expected rank, got '{}'", char::from(raw_y)).into());
        }
        let (x, y) = (raw_x - b'a', 7 - (raw_y - b'1'));
        Ok(position_to_index(x, y))
    }

    fn long_algebraic_notation_normal(from_index: u8, to_index: u8) -> String {
        let (from_x, from_y) = Self::index_to_chess_position(from_index);
        let (to_x, to_y) = Self::index_to_chess_position(to_index);
        format!("{from_x}{from_y}{to_x}{to_y}")
    }
}

enum Color {
    White,
    Black,
}

impl Color {
    fn other(&self) -> Self {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

struct Context {
    score: Score,
    remainder: usize,
    next: Board,
}

impl Context {
    fn new(board: &Board, remainder: usize) -> Self {
        Self {
            score: Score::zero(),
            remainder,
            next: board.clone(),
        }
    }
}

struct Score {
    best_points: f64,
    best_move: Move,
}

impl Score {
    fn zero() -> Self {
        Self {
            best_points: 0.0,
            best_move: Move::None,
        }
    }

    fn board(board: &Board) -> Self {
        Self {
            best_points: board.black.score() - board.white.score(),
            best_move: Move::None,
        }
    }

    fn checkmate_black() -> Self {
        Self {
            best_points: -150.0,
            best_move: Move::None,
        }
    }

    fn checkmate_white() -> Self {
        Self {
            best_points: 150.0,
            best_move: Move::None,
        }
    }

    fn update_white(&mut self, other: Score, mov: Move) {
        if self.best_move == Move::None || other.best_points < self.best_points {
            self.best_points = other.best_points;
            self.best_move = mov;
        }
    }

    fn update_black(&mut self, other: Score, mov: Move) {
        if self.best_move == Move::None || other.best_points > self.best_points {
            self.best_points = other.best_points;
            self.best_move = mov;
        }
    }

    fn finalize(self) -> Score {
        self
    }
}

#[cfg(test)]
mod tests {
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
