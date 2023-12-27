use std::{fmt::{self, Binary}, ops::{BitOr, Not, BitAnd, Shl, Shr}, iter::zip, error::{Error, self}, io::{self, prelude::*}, cmp::Ordering, collections::HashMap};

type Result<T> = std::result::Result<T, Box<dyn Error>>;

const DEFAULT_DEPTH: usize = 4;

fn main() -> Result<()> {
    unsafe { init() };
    UCI::new().read_eval_print_loop()
}

struct UCI {
    game: Game,
}

impl UCI {
    fn new() -> Self {
        Self { game: Game::new() }
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
            ["ucinewgame"] | ["position", "startpos"] => {
                self.game.reset();
                Vec::new()
            },
            ["position", "startpos", "moves", raw_moves @ ..] => {
                // TODO: fen
                self.apply_moves(raw_moves)?;
                Vec::new()
            },
            ["go", ..] => {
                let mov = self.game.best_move()?;
                vec![format!("bestmove {}", mov.to_long_algebraic_notation())]
            }
            ["stop"] => vec![],
            ["quit"] => return Err("quit".into()),
            _ => return Err(format!("unknown command '{}'", command).into())
        };
        Ok(responses)
    }

    fn apply_moves(&mut self, raw_moves: &[&str]) -> Result<()> {
        self.game.reset();
        for raw_move in raw_moves {
            let mov = Move::from_long_algebraic_notation(raw_move)?;
            self.game.apply_move(mov)?;
        }
        Ok(())
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

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
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

    fn from_index(index: u8) -> Self {
        Self::zero().with(index)
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

    fn overlaps(self, other: Self) -> bool {
        (self & other) != Bitset::zero()
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

#[derive(Debug)]
enum EndOfGameError {
    Checkmate,
    Other,
}

impl fmt::Display for EndOfGameError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self, f)
    }
}

impl error::Error for EndOfGameError {}

#[derive(Clone)]
struct Game {
    board: Board,
    next_move_color: Color,
    full_position_counts: HashMap<Board, usize>,
}

// TODO:
// - Currently we disallow repeating moves entirely.
//   When we are losing it might be nice to repeat moves to get a draw.
// - better usage of side
//   (current blocker is typing the Moves::iter_pawns_without_en_passant impl Iterator)
impl Game {
    fn new() -> Self {
        let mut game = Self {
            board: Board::start(),
            next_move_color: Color::White,
            full_position_counts: HashMap::new(),
        };
        game.full_position_counts.insert(game.board.clone(), 1);
        game
    }

    fn with_move_unchecked(&self, mov: Move) -> (Self, bool) {
        // TODO: Don't copy the position counts HashMap every time
        let mut new_game = self.clone();
        let repeats = new_game.apply_move_unchecked(mov);
        (new_game, repeats)
    }

    fn reset(&mut self) {
        self.board = Board::start();
        self.next_move_color = Color::White;
        self.full_position_counts.clear();
    }

    fn has_check(&self) -> bool {
        match self.next_move_color {
            Color::White => self.board.black.has_check(&self.board.white, Color::Black),
            Color::Black => self.board.black.has_check(&self.board.black, Color::White),
        }
    }

    fn en_passant_left_move<S: Side>(side: &mut S) -> Option<Move> {
        let Some(en_passant_index) = side.board().en_passant_index else {
            return None;
        };

        if side.we().has_en_passant_left(side.enemy(), en_passant_index) {
            S::color().en_passant_left(en_passant_index)
        } else {
            None
        }
    }

    fn en_passant_right_move<S: Side>(side: &mut S) -> Option<Move> {
        let Some(en_passant_index) = side.board().en_passant_index else {
            return None;
        };

        if side.we().has_en_passant_right(side.enemy(), en_passant_index) {
            S::color().en_passant_right(en_passant_index)
        } else {
            None
        }
    }

    fn castle_right_move<S: Side>(side: &mut S) -> Option<Move> {
        if can_castle_right(side) {
            Some(Move::Normal {
                from: S::color().king_starting_index(),
                to: S::color().king_starting_index()+2,
            })
        } else {
            None
        }
    }

    fn castle_left_move<S: Side>(side: &mut S) -> Option<Move> {
        if can_castle_left(side) {
            Some(Move::Normal {
                from: S::color().king_starting_index(),
                to: S::color().king_starting_index()-2,
            })
        } else {
            None
        }
    }

    fn iter_moves_without_non_en_passant_pawns<S: Side>(side: &mut S) -> impl Iterator<Item = Move> {
        // TODO: clone
        let mut enemy = side.enemy().clone();
        Moves::iter_without_pawns_castle(side.we(), &mut enemy)
            .chain(Self::en_passant_left_move(side).into_iter())
            .chain(Self::en_passant_right_move(side).into_iter())
            .chain(Self::castle_right_move(side).into_iter())
            .chain(Self::castle_left_move(side).into_iter())
    }

    fn iter_white_moves_with_checks(side: &mut impl Side) -> impl Iterator<Item = Move> {
        // TODO: clone
        let mut enemy = side.enemy().clone();
        Moves::iter_white_pawns_without_en_passant(side.we(), &mut enemy)
            .chain(Self::iter_moves_without_non_en_passant_pawns(side))
    }

    fn iter_white_games_moves<'a>(&'a self, side: &'a mut impl Side) -> impl Iterator<Item = (Move, Game)> + 'a {
        Self::iter_white_moves_with_checks(side)
            .map(|mov| (mov, self.with_move_unchecked(mov)))
            .filter(|(_, (_, repeats))| !repeats)
            .map(|(mov, (game, _))| (mov, game))
            .filter(|(_, game)| !game.board.black.has_check(&game.board.white, Color::Black))
    }

    fn iter_black_moves_with_checks(side: &mut impl Side) -> impl Iterator<Item = Move> {
        // TODO: clone
        let mut enemy = side.enemy().clone();
        Moves::iter_black_pawns_without_en_passant(side.we(), &mut enemy)
            .chain(Self::iter_moves_without_non_en_passant_pawns(side))
    }

    fn iter_black_games_moves<'a>(&'a self, side: &'a mut impl Side) -> impl Iterator<Item = (Move, Game)> + 'a {
        Self::iter_black_moves_with_checks(side)
            .map(|mov| (mov, self.with_move_unchecked(mov)))
            .filter(|(_, (_, repeats))| !repeats)
            .map(|(mov, (game, _))| (mov, game))
            .filter(|(_, game)| !game.board.white.has_check(&game.board.black, Color::White))
    }

    fn apply_move(&mut self, mov: Move) -> Result<()> {
        let is_legal = match self.next_move_color {
            Color::White => {
                let mut side = WhiteSide::new(&mut self.board, 0);
                // Need extra assignment to make the compiler happy
                let is_legal = self.iter_white_games_moves(&mut side)
                    .any(|(legal_move, _)| legal_move == mov);
                is_legal
            },
            Color::Black => {
                let mut side = BlackSide::new(&mut self.board, 0);
                // Need extra assignment to make the compiler happy
                let is_legal = self.iter_black_games_moves(&mut side)
                    .any(|(legal_move, _)| legal_move == mov);
                is_legal
            },
        };
        if is_legal {
            // TODO: reject repeats
            let _ = self.apply_move_unchecked(mov);
            Ok(())
        } else {
            Err(format!("illegal move {mov}").into())
        }
    }

    fn apply_move_unchecked(&mut self, mov: Move) -> bool {
        self.board.en_passant_index = match self.next_move_color {
            Color::White => {
                let mut side = WhiteSide::new(&mut self.board, 0);
                let en_passant_index = Self::apply_move_side_unchecked(&mut side, mov);
                self.board = side.board().clone();
                en_passant_index
            },
            Color::Black => {
                let mut side = BlackSide::new(&mut self.board, 0);
                let en_passant_index = Self::apply_move_side_unchecked(&mut side, mov);
                self.board = side.board().clone();
                en_passant_index
            },
        };
        self.next_move_color = self.next_move_color.other();

        let count = self.full_position_counts
            .entry(self.board.clone())
            .or_insert(0);
        *count += 1;
        *count >= 3
    }

    fn apply_move_side_unchecked(side: &mut impl Side, mov: Move) -> Option<u8> {
        match mov {
            Move::Normal { from, to } => Self::apply_move_normal(side, from, to),
            Move::Promotion { piece, from, to } => {
                Self::apply_move_promotion(side, piece, from, to);
                None
            },
        }
    }

    fn apply_move_normal(side: &mut impl Side, from: u8, to: u8) -> Option<u8> {
        assert!(side.we().bitset().has(from));
        if Self::apply_move_en_passant(side, from, to) {
            None
        } else if Self::apply_move_castle(side, from, to) {
            None
        } else {
            let piece = side.we().which_piece(from).unwrap();
            side.we().remove_piece(piece, from);
            side.enemy().captured(to);
            side.we().place_piece(piece, to);

            Self::disable_castle(side, from, to);
            Self::possible_en_passant(piece, from, to)
        }
    }

    fn apply_move_en_passant<S: Side>(side: &mut S, from: u8, to: u8) -> bool {
        let Some(en_passant_index) = side.board().en_passant_index else {
            return false;
        };
        if !side.we().pawns.has(from) {
            return false;
        }
        let mov = Move::Normal { from, to };
        let is_en_passant = if Some(mov) == S::color().en_passant_left(en_passant_index) {
            assert!(side.we().has_en_passant_left(side.enemy(), en_passant_index));
            true
        } else if Some(mov) == S::color().en_passant_right(en_passant_index) {
            assert!(side.we().has_en_passant_right(side.enemy(), en_passant_index));
            true
        } else {
            false
        };
        if !is_en_passant {
            return false;
        }
        side.we().pawns.mov(from, to);
        side.enemy().pawns.checked_clear(en_passant_index);
        true
    }

    fn disable_castle<S: Side>(side: &mut S, from: u8, to: u8) {
        if from == S::color().king_starting_index() || to == S::color().king_starting_index() {
            side.we().disable_castle();
        } else if from == S::color().rook_left_starting_index() || to == S::color().rook_left_starting_index() {
            side.we().disable_castle_left();
        } else if from == S::color().rook_right_starting_index() || to == S::color().rook_right_starting_index() {
            side.we().disable_castle_right();
        }
    }

    fn apply_move_castle<S: Side>(side: &mut S, from: u8, to: u8) -> bool {
        if from != S::color().king_starting_index() || !side.we().can_castle_any() {
            return false;
        }
        if to == from-2 {
            assert!(side.we().can_castle_left());
            castle_left_apply(side);
            true
        } else if to == from+2 {
            assert!(side.we().can_castle_right());
            castle_right_apply(side);
            true
        } else {
            false
        }
    }

    fn apply_move_promotion(side: &mut impl Side, piece: Piece, from: u8, to: u8) {
        assert!(side.we().pawns.has(from));
        side.we().pawns.checked_clear(from);
        side.enemy().captured(to);
        side.we().place_piece(piece, to);
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

    fn best_move(&self) -> Result<Move> {
        let result = match self.next_move_color {
            Color::White => {
                let mut board = self.board.clone();
                let mut side = WhiteSide::new(&mut board, 0);
                self.iter_white_games_moves(&mut side)
                    .map(|(mov, game)| (mov, search_black(&game.board)))
                    .max_by(|(_, a), (_, b)| a.cmp_white(&b))
            },
            Color::Black => {
                let mut board = self.board.clone();
                let mut side = BlackSide::new(&mut board, 0);
                self.iter_black_games_moves(&mut side)
                    .map(|(mov, game)| (mov, search_white(&game.board)))
                    .max_by(|(_, a), (_, b)| a.cmp_black(&b))
            },
        };

        match result {
            Some((mov, _)) => Ok(mov),
            None => {
                if self.has_check() {
                    Err(EndOfGameError::Checkmate.into())
                } else {
                    Err(EndOfGameError::Other.into())
                }
            },
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct PlayerBoard {
    pawns: Bitset,
    bishops: Bitset,
    knights: Bitset,
    rooks: Bitset,
    queens: Bitset,
    king: Bitset,

    can_castle: CanCastle,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
struct CanCastle {
    // TODO: bitflags
    left: bool,
    right: bool,
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

    fn has_check(&self, enemy: &Self, self_color: Color) -> bool {
        let pawns_without_en_passant = match self_color {
            Color::White => Moves::white_pawns_without_en_passant(self, enemy),
            Color::Black => Moves::black_pawns_without_en_passant(self, enemy),
        };
        Moves::without_pawns_castle(self, enemy)
            .combine(&pawns_without_en_passant)
            .captures
            .overlaps(enemy.king)
    }

    fn has_en_passant_left(&self, enemy: &Self, en_passant_index: u8) -> bool {
        assert!(enemy.pawns.has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x == 0 {
            return false;
        }

        let left_neighbour_pawn = Bitset::from_position(x-1, y) & self.pawns;
        !left_neighbour_pawn.is_empty()
    }

    fn has_en_passant_right(&self, enemy: &Self, en_passant_index: u8) -> bool {
        assert!(enemy.pawns.has(en_passant_index));
        let (x, y) = index_to_position(en_passant_index);

        if x == 7 {
            return false;
        }
        let right_neighbour_pawn = Bitset::from_position(x+1, y) & self.pawns;
        !right_neighbour_pawn.is_empty()
    }

    fn can_castle_any(&self) -> bool {
        self.can_castle_left() || self.can_castle_right()
    }

    fn can_castle_left(&self) -> bool {
        self.can_castle.left
    }

    fn can_castle_right(&self) -> bool {
        self.can_castle.right
    }

    fn disable_castle(&mut self) {
        self.can_castle.left = false;
        self.can_castle.right = false;
    }

    fn disable_castle_left(&mut self) {
        self.can_castle.left = false;
    }

    fn disable_castle_right(&mut self) {
        self.can_castle.right = false;
    }

    fn reset_castle(&mut self, old: CanCastle) {
        self.can_castle.left = old.left;
        self.can_castle.right = old.right;
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

    fn bishop(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::diagonal_left(all_pieces, index, enemy_pieces)
            .combine(&Self::diagonal_right(all_pieces, index, enemy_pieces))
    }

    fn rook(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::row(all_pieces, index, enemy_pieces)
            .combine(&Self::column(all_pieces, index, enemy_pieces))
    }

    fn queen(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
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

    fn white_pawn_double(all_pieces: Bitset, index: u8, _: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let double: Bitset = ((((pawn & ROW_6) >> 8) & !all_pieces) >> 8) & !all_pieces;
        Self { moves: double, captures: Bitset::zero() }
    }

    fn white_pawn_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_single_without_en_passant(all_pieces, index, enemy_pieces)
            .combine(&Self::white_pawn_double(all_pieces, index, enemy_pieces))
    }

    fn white_pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(!ROW_0)
    }

    fn white_pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_normal_single_without_en_passant(all_pieces, index, enemy_pieces)
            .combine(&Self::white_pawn_double(all_pieces, index, enemy_pieces))
    }

    fn white_pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
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

    fn black_pawn_double(all_pieces: Bitset, index: u8, _: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let double = ((((pawn & ROW_1) << 8) & !all_pieces) << 8) & !all_pieces;
        Self { moves: double, captures: Bitset::zero() }
    }

    fn black_pawn_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_single_without_en_passant(all_pieces, index, enemy_pieces)
            .combine(&Self::black_pawn_double(all_pieces, index, enemy_pieces))
    }

    fn black_pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(!ROW_7)
    }

    fn black_pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_normal_single_without_en_passant(all_pieces, index, enemy_pieces)
            .combine(&Self::black_pawn_double(all_pieces, index, enemy_pieces))
    }

    fn black_pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_single_without_en_passant(all_pieces, index, enemy_pieces).and(ROW_7)
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

    fn without_pawns_castle(we: &PlayerBoard, enemy: &PlayerBoard) -> Self {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.queens.indices().map(|index| Moves::queen(all_pieces, index, enemy_pieces))
            .chain(we.rooks.indices().map(|index| Moves::rook(all_pieces, index, enemy_pieces)))
            .chain(we.bishops.indices().map(|index| Moves::bishop(all_pieces, index, enemy_pieces)))
            .chain(we.knights.indices().map(|index| Moves::knight(all_pieces, index, enemy_pieces)))
            .chain(we.king.indices().map(|index| Moves::king(all_pieces, index, enemy_pieces)))
            .fold(Moves::empty(), |a, b| a.combine(&b))
    }

    fn white_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Self {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.pawns.indices()
            .map(|index| Moves::white_pawn_without_en_passant(all_pieces, index, enemy_pieces))
            .fold(Moves::empty(), |a, b| a.combine(&b))
    }

    fn black_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Self {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.pawns.indices()
            .map(|index| Moves::black_pawn_without_en_passant(all_pieces, index, enemy_pieces))
            .fold(Moves::empty(), |a, b| a.combine(&b))
    }

    fn iter_without_pawns_castle(we: &PlayerBoard, enemy: &PlayerBoard) -> impl Iterator<Item = Move> {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        we.queens.indices().map(move |from| (from, Moves::queen(all_pieces, from, enemy_pieces)))
            .chain(we.rooks.indices().map(move |from| (from, Moves::rook(all_pieces, from, enemy_pieces))))
            .chain(we.bishops.indices().map(move |from| (from, Moves::bishop(all_pieces, from, enemy_pieces))))
            .chain(we.knights.indices().map(move |from| (from, Moves::knight(all_pieces, from, enemy_pieces))))
            .chain(we.king.indices().map(move |from| (from, Moves::king(all_pieces, from, enemy_pieces))))
            .flat_map(|(from, moves)| moves.iter_normal(from))
    }

    fn iter_pawns_without_en_passant(
        we: &PlayerBoard,
        enemy: &PlayerBoard,
        normal: impl Fn(Bitset, u8, Bitset) -> Self + 'static,
        promotion: impl Fn(Bitset, u8, Bitset) -> Self + 'static,
    ) -> impl Iterator<Item = Move> {
        let enemy_pieces = enemy.bitset();
        let all_pieces = we.bitset() | enemy_pieces;
        let normal = we.pawns.indices()
            .map(move |from| (from, normal(all_pieces, from, enemy_pieces)))
            .flat_map(|(from, moves)| moves.iter_normal(from));
        let promotions = we.pawns.indices()
            .map(move |from| (from, promotion(all_pieces, from, enemy_pieces)))
            .flat_map(|(from, moves)| moves.iter_promotions(from));
        normal.chain(promotions)
    }

    fn iter_white_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> impl Iterator<Item = Move> {
        Self::iter_pawns_without_en_passant(
            we,
            enemy,
            Moves::white_pawn_normal_without_en_passant,
            Moves::white_pawn_promotion,
        )
    }

    fn iter_black_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> impl Iterator<Item = Move> {
        Self::iter_pawns_without_en_passant(
            we,
            enemy,
            Moves::black_pawn_normal_without_en_passant,
            Moves::black_pawn_promotion,
        )
    }

    fn iter_normal(&self, from: u8) -> impl Iterator<Item = Move> {
        self.moves.indices().chain(self.captures.indices()).map(move |to| Move::Normal { from, to })
    }

    fn iter_promotions(&self, from: u8) -> impl Iterator<Item = Move> {
        self.moves.indices()
            .chain(self.captures.indices())
            .flat_map(move |to| Self::iter_single_promotions(from, to))
    }

    fn iter_single_promotions(from: u8, to: u8) -> impl Iterator<Item = Move> {
        [Piece::Queen, Piece::Rook, Piece::Knight, Piece::Bishop].into_iter()
            .map(move |piece| Move::Promotion { piece, from, to })
    }

    fn combine(&self, other: &Self) -> Self {
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

#[derive(Clone, PartialEq, Eq, Hash)]
struct Board {
    black: PlayerBoard,
    white: PlayerBoard,
    en_passant_index: Option<u8>,
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

const COLUMN_0: Bitset = Bitset(1 | (1 << 8) | (1 << 16) | (1 << 24) | (1 << 32) | (1 << 40) | (1 << 48) | (1 << 56));
const COLUMN_7: Bitset = Bitset(COLUMN_0.0 << 7);

const WHITE_KING_STARTING_INDEX: u8 = 7*8 + 4;
const WHITE_ROOK_LEFT_STARTING_INDEX: u8 = 7*8;
const WHITE_ROOK_RIGHT_STARTING_INDEX: u8 = 7*8 + 7;

const BLACK_KING_STARTING_INDEX: u8 = 4;
const BLACK_ROOK_LEFT_STARTING_INDEX: u8 = 0;
const BLACK_ROOK_RIGHT_STARTING_INDEX: u8 = 7;

// TODO:
// Currently we still calculate castling moves
// even if they are not strictly allowed.
impl Board {
    fn start() -> Self {
        let black_pawns = (0..8).map(|x| Bitset::from_position(x, 1))
            .fold(Bitset::zero(), |pawns, pawn| pawns | pawn);
        let white_pawns = (0..8).map(|x| Bitset::from_position(x, 6))
            .fold(Bitset::zero(), |pawns, pawn| pawns | pawn);
        let board = Self {
            black: PlayerBoard {
                rooks: Bitset::from_index(BLACK_ROOK_LEFT_STARTING_INDEX) | Bitset::from_index(BLACK_ROOK_RIGHT_STARTING_INDEX),
                knights: Bitset::from_position(1, 0) | Bitset::from_position(6, 0),
                bishops: Bitset::from_position(2, 0) | Bitset::from_position(5, 0),
                queens: Bitset::from_position(3, 0),
                king: Bitset::from_index(BLACK_KING_STARTING_INDEX),
                pawns: black_pawns,
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
            },
            white: PlayerBoard {
                rooks: Bitset::from_index(WHITE_ROOK_LEFT_STARTING_INDEX) | Bitset::from_index(WHITE_ROOK_RIGHT_STARTING_INDEX),
                knights: Bitset::from_position(1, 7) | Bitset::from_position(6, 7),
                bishops: Bitset::from_position(2, 7) | Bitset::from_position(5, 7),
                queens: Bitset::from_position(3, 7),
                king: Bitset::from_index(WHITE_KING_STARTING_INDEX),
                pawns: white_pawns,
                can_castle: CanCastle {
                    left: true,
                    right: true,
                },
            },
            en_passant_index: None,
        };
        board.debug_check();
        board
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
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Move {
    Normal { from: u8, to: u8 },
    Promotion { piece: Piece, from: u8, to: u8 },
}

impl fmt::Display for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_long_algebraic_notation())
    }
}

impl fmt::Debug for Move {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self, f)
    }
}

impl Move {
    fn white_en_passant_left(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 0 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x-1, y),
                to: position_to_index(x, y-1),
            })
        }
    }

    fn white_en_passant_right(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 7 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x+1, y),
                to: position_to_index(x, y-1),
            })
        }
    }

    fn black_en_passant_left(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 0 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x-1, y),
                to: position_to_index(x, y+1),
            })
        }
    }

    fn black_en_passant_right(en_passant_index: u8) -> Option<Self> {
        let (x, y) = index_to_position(en_passant_index);
        if x == 7 {
            None
        } else {
            Some(Self::Normal {
                from: position_to_index(x+1, y),
                to: position_to_index(x, y+1),
            })
        }
    }

    fn from_long_algebraic_notation(notation: &str) -> Result<Self> {
        if notation.len() != 4 && notation.len() != 5 {
            return Err(format!("expected chess notation, got '{}'", notation).into());
        }
        let notation = notation.as_bytes();
        let from = Self::chess_position_to_index(&notation[..2])?;
        let to = Self::chess_position_to_index(&notation[2..4])?;
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

    fn to_long_algebraic_notation(&self) -> String {
        match *self {
            Move::Normal { from, to } => Self::long_algebraic_notation_normal(from, to),
            Move::Promotion { piece, from, to } => {
                assert!(piece.can_promote_to());
                let mut s = Self::long_algebraic_notation_normal(from, to);
                s.push(piece.to_symbol());
                s
            },
        }
    }

    fn index_to_chess_position(index: u8) -> (char, char) {
        let (x, y) = index_to_position(index);
        ((b'a' + x).into(), char::from_digit((8-y).into(), 10).unwrap())
    }

    #[allow(dead_code)]
    fn fmt_index(index: u8) -> String {
        let (x, y) = Self::index_to_chess_position(index);
        format!("{x}{y}")
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    fn direction(&self) -> i8 {
        match self {
            Color::White => -1,
            Color::Black => 1,
        }
    }

    fn king_starting_index(&self) -> u8 {
        match self {
            Color::White => WHITE_KING_STARTING_INDEX,
            Color::Black => BLACK_KING_STARTING_INDEX,
        }
    }

    fn first_row(&self) -> u8 {
        match self {
            Color::White => 7,
            Color::Black => 0,
        }
    }

    fn rook_left_starting_index(&self) -> u8 {
        match self {
            Color::White => WHITE_ROOK_LEFT_STARTING_INDEX,
            Color::Black => BLACK_ROOK_LEFT_STARTING_INDEX,
        }
    }

    fn rook_right_starting_index(&self) -> u8 {
        match self {
            Color::White => WHITE_ROOK_RIGHT_STARTING_INDEX,
            Color::Black => BLACK_ROOK_RIGHT_STARTING_INDEX,
        }
    }

    fn en_passant_left(&self, en_passant_index: u8) -> Option<Move> {
        match self {
            Color::White => Move::white_en_passant_left(en_passant_index),
            Color::Black => Move::black_en_passant_left(en_passant_index),
        }
    }

    fn en_passant_right(&self, en_passant_index: u8) -> Option<Move> {
        match self {
            Color::White => Move::white_en_passant_right(en_passant_index),
            Color::Black => Move::black_en_passant_right(en_passant_index),
        }
    }
}

struct Score {
    best_points: f64,
}

impl Score {
    fn zero() -> Self {
        Self {
            best_points: 0.0,
        }
    }

    fn board(board: &Board) -> Self {
        Self {
            best_points: board.black.score() - board.white.score(),
        }
    }

    fn checkmate_black() -> Self {
        Self {
            best_points: -150.0,
        }
    }

    fn checkmate_white() -> Self {
        Self {
            best_points: 150.0,
        }
    }

    fn cmp_black(&self, rhs: &Self) -> Ordering {
        self.best_points.total_cmp(&rhs.best_points)
    }

    fn cmp_white(&self, rhs: &Self) -> Ordering {
        rhs.best_points.total_cmp(&self.best_points)
    }

    fn update_white(&mut self, other: Score) {
        if other.best_points < self.best_points {
            self.best_points = other.best_points;
        }
    }

    fn update_black(&mut self, other: Score) {
        if other.best_points > self.best_points {
            self.best_points = other.best_points;
        }
    }
}

trait Side {
    fn color() -> Color;

    fn search(&mut self);

    fn board(&mut self) -> &mut Board;
    fn we(&mut self) -> &mut PlayerBoard;
    fn enemy(&mut self) -> &mut PlayerBoard;

    fn pawn_normal_single_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;
    fn pawn_double(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;
    fn pawn_promotion(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves;
}

struct WhiteSide<'a> {
    board: &'a mut Board,
    score: Score,
    depth: usize,
}

impl <'a> WhiteSide<'a> {
    fn new(board: &'a mut Board, depth: usize) -> Self {
        Self {
            board,
            score: Score::zero(),
            depth,
        }
    }
}

impl <'a> Side for WhiteSide<'a> {
    fn color() -> Color {
        Color::White
    }

    fn search(&mut self) {
        if self.board.black.king.is_empty() {
            self.score.update_white(Score::checkmate_black());
        } else if self.depth == 0 {
            self.score.update_white(Score::board(&self.board));
        } else {
            let mut other = BlackSide::new(self.board, self.depth - 1);
            search(&mut other);
            self.score.update_white(other.score);
        }
    }

    fn board(&mut self) -> &mut Board {
        self.board
    }

    fn we(&mut self) -> &mut PlayerBoard {
        &mut self.board.white
    }

    fn enemy(&mut self) -> &mut PlayerBoard {
        &mut self.board.black
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

struct BlackSide<'a> {
    board: &'a mut Board,
    score: Score,
    depth: usize,
}

impl <'a> BlackSide<'a> {
    fn new(board: &'a mut Board, depth: usize) -> Self {
        Self {
            board,
            score: Score::zero(),
            depth,
        }
    }
}

impl <'a> Side for BlackSide<'a> {
    fn color() -> Color {
        Color::Black
    }

    fn search(&mut self) {
        if self.board.white.king.is_empty() {
            self.score.update_black(Score::checkmate_white());
        } else if self.depth == 0 {
            self.score.update_black(Score::board(self.board));
        } else {
            let mut other = WhiteSide::new(self.board, self.depth - 1);
            search(&mut other);
            self.score.update_black(other.score);
        }
    }

    fn board(&mut self) -> &mut Board {
        self.board
    }

    fn we(&mut self) -> &mut PlayerBoard {
        &mut self.board.black
    }

    fn enemy(&mut self) -> &mut PlayerBoard {
        &mut self.board.white
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

fn castle_left_apply<S: Side>(side: &mut S) {
    side.we().king.mov(
        S::color().king_starting_index(),
        S::color().king_starting_index() - 2,
    );
    side.we().rooks.mov(
        S::color().rook_left_starting_index(),
        position_to_index(3, S::color().first_row(),
    ));
    side.we().disable_castle();
}

fn castle_right_apply<S: Side>(side: &mut S) {
    side.we().king.mov(
        S::color().king_starting_index(),
        S::color().king_starting_index() + 2,
    );
    side.we().rooks.mov(
        S::color().rook_right_starting_index(),
        position_to_index(5, S::color().first_row(),
    ));
    side.we().disable_castle();
}

fn search_white(board: &Board) -> Score {
    let mut board = board.clone();
    let mut side = WhiteSide::new(&mut board, DEFAULT_DEPTH);
    search(&mut side);
    side.score
}

fn search_black(board: &Board) -> Score {
    let mut board = board.clone();
    let mut side = BlackSide::new(&mut board, DEFAULT_DEPTH);
    search(&mut side);
    side.score
}

fn apply_moves_with<S: Side>(
    side: &mut S,
    old_enemy: &PlayerBoard,
    get_piece_bitset: impl Fn(&mut PlayerBoard) -> &mut Bitset,
    get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
    next: impl Fn(&mut S, u8, u8),
) {
    let enemy_pieces = side.enemy().bitset();
    let all_pieces = enemy_pieces | side.we().bitset();

    let we_old_selected_piece = *get_piece_bitset(side.we());
    for from in get_piece_bitset(side.we()).indices() {
        let Moves { moves, captures } = get_moves(all_pieces, from, enemy_pieces);
        for to in moves.indices() {
            get_piece_bitset(side.we()).mov(from, to);
            next(side, from, to);
            *get_piece_bitset(side.we()) = we_old_selected_piece;
        }
        for to in captures.indices() {
            side.enemy().captured(to);
            get_piece_bitset(side.we()).mov(from, to);
            next(side, from, to);
            *get_piece_bitset(side.we()) = we_old_selected_piece;
            *side.enemy() = *old_enemy;
        }
    }
}

fn apply_moves<S: Side>(
    side: &mut S,
    old_enemy: &PlayerBoard,
    get_piece_bitset: impl Fn(&mut PlayerBoard) -> &mut Bitset,
    get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
) {
    apply_moves_with(
        side,
        old_enemy,
        get_piece_bitset,
        get_moves,
        |side, _, _| side.search(),
    );
}

fn search(side: &mut impl Side) {
    side.board().debug_check();
    let old_enemy = side.enemy().clone();
    let en_passant_index = side.board().en_passant_index;
    side.board().en_passant_index = None;

    search_pawns(side, &old_enemy, en_passant_index);
    search_queens(side, &old_enemy);
    search_rooks(side, &old_enemy);
    search_bishops(side, &old_enemy);
    search_knights(side, &old_enemy);
    search_king(side, &old_enemy);
    search_castle(side);

    side.board().en_passant_index = en_passant_index;
}

fn search_king(side: &mut impl Side, old_enemy: &PlayerBoard) {
    apply_moves(
        side,
        old_enemy,
        |player_board| &mut player_board.king,
        Moves::king,
    );
}

fn search_knights(side: &mut impl Side, old_enemy: &PlayerBoard) {
    apply_moves(
        side,
        old_enemy,
        |player_board| &mut player_board.knights,
        Moves::knight,
    );
}

fn search_bishops(side: &mut impl Side, old_enemy: &PlayerBoard) {
    apply_moves(
        side,
        old_enemy,
        |player_board| &mut player_board.bishops,
        Moves::bishop,
    );
}

fn search_rooks(side: &mut impl Side, old_enemy: &PlayerBoard) {
    apply_moves(
        side,
        old_enemy,
        |player_board| &mut player_board.rooks,
        Moves::rook,
    );
}

fn search_queens(side: &mut impl Side, old_enemy: &PlayerBoard) {
    apply_moves(
        side,
        old_enemy,
        |player_board| &mut player_board.queens,
        Moves::queen,
    );
}

fn search_castle(side: &mut impl Side) {
    if can_castle_right(side) {
        search_castle_right(side);
    }
    if can_castle_left(side) {
        search_castle_left(side);
    }
}

fn can_castle_right<S: Side>(side: &mut S) -> bool {
    let all_pieces = side.board().bitset();
    let king_index = side.we().king.first_index();

    let king_starting_index = S::color().king_starting_index();
    let row = S::color().first_row();

    let allowed = king_index == king_starting_index
        && side.we().can_castle_right()
        && side.we().rooks.has(S::color().rook_right_starting_index())
        && !all_pieces.has(position_to_index(5, row))
        && !all_pieces.has(position_to_index(6, row));
    if !allowed {
        return false;
    }

    // TODO: clone
    let we = side.we().clone();
    let we_old_king = side.we().king;
    for shift in 0..=2 {
        side.we().king.mov(king_starting_index, king_starting_index+shift);
        let check = side.enemy().has_check(&we, S::color().other());
        side.we().king = we_old_king;
        if check {
            return false;
        }
    }
    true
}

fn can_castle_left<S: Side>(side: &mut S) -> bool {
    let all_pieces = side.we().bitset();
    let king_index = side.we().king.first_index();

    let king_starting_index = S::color().king_starting_index();
    let row = S::color().first_row();

    let allowed = king_index == king_starting_index
        && side.we().can_castle_left()
        && side.we().rooks.has(S::color().rook_left_starting_index())
        && !all_pieces.has(position_to_index(3, row))
        && !all_pieces.has(position_to_index(2, row))
        && !all_pieces.has(position_to_index(1, row));
    if !allowed {
        return false;
    }

    // TODO: clone
    let we = side.we().clone();
    let we_old_king = side.we().king;
    for shift in 0..=2 {
        side.we().king.mov(king_starting_index, king_starting_index-shift);
        let check = side.enemy().has_check(&we, S::color().other());
        side.we().king = we_old_king;
        if check {
            return false;
        }
    }
    true
}

fn search_castle_reset(
    side: &mut impl Side,
    old_can_castle: CanCastle,
    we_old_king: Bitset,
    we_old_rooks: Bitset,
) {
    side.we().reset_castle(old_can_castle);
    side.we().king = we_old_king;
    side.we().rooks = we_old_rooks;
}

fn search_castle_right<S: Side>(side: &mut S) {
    let old_can_castle = side.we().can_castle;
    let we_old_king = side.we().king;
    let we_old_rooks = side.we().rooks;
    castle_right_apply(side);
    side.search();
    search_castle_reset(side, old_can_castle, we_old_king, we_old_rooks);
}

fn search_castle_left<S: Side>(side: &mut S) {
    let old_can_castle = side.we().can_castle;
    let we_old_king = side.we().king;
    let we_old_rooks = side.we().rooks;
    castle_left_apply(side);
    side.search();
    search_castle_reset(side, old_can_castle, we_old_king, we_old_rooks);
}

fn search_pawns<S: Side>(side: &mut S, old_enemy: &PlayerBoard, en_passant_index: Option<u8>) {
    apply_moves(
        side,
        old_enemy,
        |player_board| &mut player_board.pawns,
        S::pawn_normal_single_without_en_passant,
    );

    apply_moves_with(
        side,
        old_enemy,
        |player_board| &mut player_board.pawns,
        S::pawn_double,
        |side, _, to| {
            side.board().en_passant_index = Some(to);
            side.search();
            side.board().en_passant_index = None;
        },
    );

    apply_moves_with(
        side,
        old_enemy,
        |player_board| &mut player_board.pawns,
        S::pawn_promotion,
        |side, _, to| {
            side.we().pawns.clear(to);
            search_pawn_promote_figures(side, to);
        },
    );

    if let Some(en_passant_index) = en_passant_index {
        search_en_passant(side, old_enemy, en_passant_index);
    }
}

fn search_pawn_promote_figures(side: &mut impl Side, to: u8) {
    side.we().queens.set(to);
    side.search();
    side.we().queens.clear(to);

    side.we().rooks.set(to);
    side.search();
    side.we().rooks.clear(to);

    side.we().knights.set(to);
    side.search();
    side.we().knights.clear(to);

    side.we().bishops.set(to);
    side.search();
    side.we().bishops.clear(to);
}

fn search_en_passant_single(
    side: &mut impl Side,
    old_enemy: &PlayerBoard,
    captured: u8,
    from: u8,
    to: u8,
) {
    side.enemy().captured(captured);
    side.we().pawns.mov(from, to);
    side.search();
    side.we().pawns.mov(to, from);
    *side.enemy() = *old_enemy;
}

fn search_en_passant<S: Side>(side: &mut S, old_enemy: &PlayerBoard, en_passant_index: u8) {
    let (x, y) = index_to_position(en_passant_index);
    let next_y = ((y as i8) + S::color().direction()) as u8;

    if side.we().has_en_passant_left(side.enemy(), en_passant_index) {
        search_en_passant_single(
            side,
            old_enemy,
            en_passant_index,
            position_to_index(x-1, y),
            position_to_index(x, next_y),
        );
    }

    if side.we().has_en_passant_right(side.enemy(), en_passant_index) {
        search_en_passant_single(
            side,
            old_enemy,
            en_passant_index,
            position_to_index(x+1, y),
            position_to_index(x, next_y),
        );
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

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

    #[test]
    fn test_has_check() {
        unsafe { init() };

        let mut board = Board::start();
        assert!(!board.white.has_check(&board.black, Color::White));
        assert!(!board.black.has_check(&board.white, Color::Black));

        board.black.queens.mov(
            board.black.queens.first_index(),
            board.white.queens.first_index(),
        );
        board.white.captured(board.white.queens.first_index());
        assert!(!board.white.has_check(&board.black, Color::White));
        assert!(board.black.has_check(&board.white, Color::Black));
    }

    #[test]
    fn test_iter_moves_start() {
        unsafe { init() };

        let mut game = Game::new();
        let mut white_side = WhiteSide::new(&mut game.board, 0);
        let white_moves: Vec<_> = game.iter_white_games_moves(&mut white_side)
            .map(|(mov, _)| mov)
            .collect();
        assert_eq!(
            white_moves.iter().copied().collect::<HashSet<_>>().len(),
            white_moves.len(),
        );
        assert_eq!(white_moves.len(), 20);

        game.next_move_color = game.next_move_color.other();
        let mut black_side = BlackSide::new(&mut game.board, 0);
        let black_moves: Vec<_> = game.iter_black_games_moves(&mut black_side)
            .map(|(mov, _)| mov)
            .collect();
        assert_eq!(
            black_moves.iter().copied().collect::<HashSet<_>>().len(),
            black_moves.len(),
        );
        assert_eq!(black_moves.len(), 20);

        assert_ne!(white_moves, black_moves);
    }

    #[test]
    fn test_white_en_passant() {
        unsafe { init() };

        let mut game = Game::new();
        let raw_moves = [
            "e2e4",
            "b8a6",
            "e4e5",
            "d7d5",
            "e5d6",
        ];
        for raw_mov in raw_moves {
            let mov = Move::from_long_algebraic_notation(raw_mov).unwrap();
            game.apply_move(mov).unwrap();
        }
        game.board.debug_check();
    }
}
