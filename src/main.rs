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
// - using side and dedup
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

    fn iter_white_games_moves(&self) -> impl Iterator<Item = (Move, Game)> + '_ {
        self.iter_white_moves_with_checks()
            .map(|mov| (mov, self.with_move_unchecked(mov)))
            .filter(|(_, (_, repeats))| !repeats)
            .map(|(mov, (game, _))| (mov, game))
            .filter(|(_, game)| !game.board.black.has_check(&game.board.white, Color::Black))
    }

    fn iter_white_moves_with_checks(&self) -> impl Iterator<Item = Move> {
        Moves::iter_without_pawns_castle(&self.board.white, &self.board.black)
            .chain(Moves::iter_white_pawns_without_en_passant(&self.board.white, &self.board.black))
            .chain(self.white_en_passant_left_move().into_iter())
            .chain(self.white_en_passant_right_move().into_iter())
            .chain(self.white_castle_right_move().into_iter())
            .chain(self.white_castle_left_move().into_iter())
    }

    fn white_en_passant_left_move(&self) -> Option<Move> {
        let Some(en_passant_index) = self.board.en_passant_index else {
            return None;
        };

        if self.board.white.has_en_passant_left(&self.board.black, en_passant_index) {
            Move::white_en_passant_left(en_passant_index)
        } else {
            None
        }
    }

    fn white_en_passant_right_move(&self) -> Option<Move> {
        let Some(en_passant_index) = self.board.en_passant_index else {
            return None;
        };

        if self.board.white.has_en_passant_right(&self.board.black, en_passant_index) {
            Move::white_en_passant_right(en_passant_index)
        } else {
            None
        }
    }

    fn white_castle_right_move(&self) -> Option<Move> {
        // TODO: using side
        if self.board.white.can_castle_right() {
            Some(Move::Normal {
                from: WHITE_KING_STARTING_INDEX,
                to: WHITE_KING_STARTING_INDEX+2
            })
        } else {
            None
        }
    }

    fn white_castle_left_move(&self) -> Option<Move> {
        // TODO: using side
        if self.board.white.can_castle_left() {
            Some(Move::Normal {
                from: WHITE_KING_STARTING_INDEX,
                to: WHITE_KING_STARTING_INDEX-2
            })
        } else {
            None
        }
    }

    fn iter_black_games_moves(&self) -> impl Iterator<Item = (Move, Game)> + '_ {
        self.iter_black_moves_with_checks()
            .map(|mov| (mov, self.with_move_unchecked(mov)))
            .filter(|(_, (_, repeats))| !repeats)
            .map(|(mov, (game, _))| (mov, game))
            .filter(|(_, game)| !game.board.white.has_check(&game.board.black, Color::White))
    }

    fn iter_black_moves_with_checks(&self) -> impl Iterator<Item = Move> {
        Moves::iter_without_pawns_castle(&self.board.black, &self.board.white)
            .chain(Moves::iter_black_pawns_without_en_passant(&self.board.black, &self.board.white))
            .chain(self.black_en_passant_left_move().into_iter())
            .chain(self.black_en_passant_right_move().into_iter())
            .chain(self.black_castle_right_move().into_iter())
            .chain(self.black_castle_left_move().into_iter())
    }

    fn black_en_passant_left_move(&self) -> Option<Move> {
        let Some(en_passant_index) = self.board.en_passant_index else {
            return None;
        };

        if self.board.black.has_en_passant_left(&self.board.white, en_passant_index) {
            Move::black_en_passant_left(en_passant_index)
        } else {
            None
        }
    }

    fn black_en_passant_right_move(&self) -> Option<Move> {
        let Some(en_passant_index) = self.board.en_passant_index else {
            return None;
        };

        if self.board.black.has_en_passant_right(&self.board.white, en_passant_index) {
            Move::black_en_passant_right(en_passant_index)
        } else {
            None
        }
    }

    fn black_castle_right_move(&self) -> Option<Move> {
        // TODO: using side
        if self.board.black.can_castle_right() {
            Some(Move::Normal {
                from: BLACK_KING_STARTING_INDEX,
                to: BLACK_KING_STARTING_INDEX+2
            })
        } else {
            None
        }
    }

    fn black_castle_left_move(&self) -> Option<Move> {
        // TODO: using side
        if self.board.black.can_castle_left() {
            Some(Move::Normal {
                from: BLACK_KING_STARTING_INDEX,
                to: BLACK_KING_STARTING_INDEX-2
            })
        } else {
            None
        }
    }

    fn apply_move(&mut self, mov: Move) -> Result<()> {
        let is_legal = match self.next_move_color {
            Color::White => self.iter_white_games_moves().any(|(legal_move, _)| legal_move == mov),
            Color::Black => self.iter_black_games_moves().any(|(legal_move, _)| legal_move == mov),
        };
        if is_legal {
            let _ = self.apply_move_unchecked(mov);
            Ok(())
        } else {
            Err(format!("illegal move {mov}").into())
        }
    }

    fn apply_move_unchecked(&mut self, mov: Move) -> bool {
        self.board.en_passant_index = match mov {
            Move::Normal { from, to } => self.apply_move_normal(from, to),
            Move::Promotion { piece, from, to } => {
                self.apply_move_promotion(piece, from, to);
                None
            },
        };
        self.next_move_color = self.next_move_color.other();

        let count = self.full_position_counts
            .entry(self.board.clone())
            .or_insert(0);
        *count += 1;
        *count >= 3
    }

    fn apply_move_normal(&mut self, from: u8, to: u8) -> Option<u8> {
        match self.next_move_color {
            Color::White => {
                assert!(self.board.white.bitset().has(from));
                if self.white_apply_move_en_passant(from, to) {
                    None
                } else if self.white_apply_move_castle(from, to) {
                    None
                } else {
                    let en_passant_index = self.board.white.normal_move(&mut self.board.black, from, to);
                    self.white_disable_castle(from, to);
                    en_passant_index
                }
            },
            Color::Black => {
                assert!(self.board.black.bitset().has(from));
                if self.black_apply_move_en_passant(from, to) {
                    None
                } else if self.black_apply_move_castle(from, to) {
                    None
                } else {
                    let en_passant_index = self.board.black.normal_move(&mut self.board.white, from, to);
                    self.black_disable_castle(from, to);
                    en_passant_index
                }
            }
        }
    }

    fn white_apply_move_en_passant(&mut self, from: u8, to: u8) -> bool {
        let Some(en_passant_index) = self.board.en_passant_index else {
            return false;
        };
        if !self.board.white.pawns.has(from) {
            return false;
        }
        let mov = Move::Normal { from, to };
        let is_en_passant = if Some(mov) == Move::white_en_passant_left(en_passant_index) {
            assert!(self.board.white.has_en_passant_left(&self.board.black, en_passant_index));
            true
        } else if Some(mov) == Move::white_en_passant_right(en_passant_index) {
            assert!(self.board.white.has_en_passant_right(&self.board.black, en_passant_index));
            true
        } else {
            false
        };
        if !is_en_passant {
            return false;
        }
        self.board.white.pawns.mov(from, to);
        self.board.black.pawns.checked_clear(en_passant_index);
        true
    }

    fn black_apply_move_en_passant(&mut self, from: u8, to: u8) -> bool {
        let Some(en_passant_index) = self.board.en_passant_index else {
            return false;
        };
        if !self.board.black.pawns.has(from) {
            return false;
        }
        let mov = Move::Normal { from, to };
        let is_en_passant = if Some(mov) == Move::black_en_passant_left(en_passant_index) {
            assert!(self.board.black.has_en_passant_left(&self.board.white, en_passant_index));
            true
        } else if Some(mov) == Move::black_en_passant_right(en_passant_index) {
            assert!(self.board.black.has_en_passant_right(&self.board.white, en_passant_index));
            true
        } else {
            false
        };
        if !is_en_passant {
            return false;
        }
        self.board.black.pawns.mov(from, to);
        self.board.white.pawns.checked_clear(en_passant_index);
        true
    }

    fn white_disable_castle(&mut self, from: u8, to: u8) {
        if from == WHITE_KING_STARTING_INDEX || to == WHITE_KING_STARTING_INDEX {
            self.board.white.disable_castle();
        } else if from == WHITE_ROOK_LEFT_STARTING_INDEX || to == WHITE_ROOK_LEFT_STARTING_INDEX {
            self.board.white.disable_castle_left();
        } else if from == WHITE_ROOK_RIGHT_STARTING_INDEX || to == WHITE_ROOK_RIGHT_STARTING_INDEX {
            self.board.white.disable_castle_right();
        }
    }

    fn black_disable_castle(&mut self, from: u8, to: u8) {
        if from == BLACK_KING_STARTING_INDEX || to == BLACK_KING_STARTING_INDEX {
            self.board.black.disable_castle();
        } else if from == BLACK_ROOK_LEFT_STARTING_INDEX || to == BLACK_ROOK_LEFT_STARTING_INDEX {
            self.board.black.disable_castle_left();
        } else if from == BLACK_ROOK_RIGHT_STARTING_INDEX || to == BLACK_ROOK_RIGHT_STARTING_INDEX {
            self.board.black.disable_castle_right();
        }
    }

    fn white_apply_move_castle(&mut self, from: u8, to: u8) -> bool {
        if from != WHITE_KING_STARTING_INDEX || !self.board.white.can_castle_any() {
            return false;
        }
        if to == from-2 {
            assert!(self.board.white.can_castle_left());
            self.board.white_castle_left_apply();
            true
        } else if to == from+2 {
            assert!(self.board.white.can_castle_right());
            self.board.white_castle_right_apply();
            true
        } else {
            false
        }
    }

    fn black_apply_move_castle(&mut self, from: u8, to: u8) -> bool {
        if from != BLACK_KING_STARTING_INDEX || !self.board.black.can_castle_any() {
            return false;
        }
        if to == from-2 {
            assert!(self.board.black.can_castle_left());
            self.board.black_castle_left_apply();
            true
        } else if to == from+2 {
            assert!(self.board.black.can_castle_right());
            self.board.black_castle_right_apply();
            true
        } else {
            false
        }
    }

    fn apply_move_promotion(&mut self, piece: Piece, from: u8, to: u8) {
        match self.next_move_color {
            Color::White => {
                assert!(self.board.white.pawns.has(from));
                self.board.white.promotion_move(&mut self.board.black, piece, from, to);
            },
            Color::Black => {
                assert!(self.board.black.pawns.has(from));
                self.board.black.promotion_move(&mut self.board.white, piece, from, to);
            }
        }
    }

    fn best_move(&self) -> Result<Move> {
        let result = match self.next_move_color {
            Color::White => self.iter_white_games_moves()
                .map(|(mov, game)| (mov, search_black(&game.board)))
                .max_by(|(_, a), (_, b)| a.cmp_white(&b)),
            Color::Black => self.iter_black_games_moves()
                .map(|(mov, game)| (mov, search_white(&game.board)))
                .max_by(|(_, a), (_, b)| a.cmp_black(&b)),
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

    // TODO: bitflags
    can_castle_left: bool,
    can_castle_right: bool,
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

        Self::possible_en_passant(piece, from, to)
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

    fn promotion_move(&mut self, enemy: &mut Self, piece: Piece, from: u8, to: u8) {
        self.pawns.checked_clear(from);
        enemy.captured(to);
        self.place_piece(piece, to);
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
        self.can_castle_left
    }

    fn can_castle_right(&self) -> bool {
        self.can_castle_right
    }

    fn disable_castle(&mut self) {
        self.can_castle_left = false;
        self.can_castle_right = false;
    }

    fn disable_castle_left(&mut self) {
        self.can_castle_left = false;
    }

    fn disable_castle_right(&mut self) {
        self.can_castle_right = false;
    }

    fn reset_castle(&mut self, old: &Self) {
        self.can_castle_left = old.can_castle_left;
        self.can_castle_right = old.can_castle_right;
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

    fn white_pawn_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let captures = {
            let forward = pawn >> 8;
            let a = (forward & !COLUMN_0) >> 1;
            let b = (forward & !COLUMN_7) << 1;
            (a | b) & enemy_pieces
        };
        let moves = {
            let single = (pawn >> 8) & !all_pieces;
            let double = ((((pawn & ROW_6) >> 8) & !all_pieces) >> 8) & !all_pieces;
            single | double
        };
        Self { moves, captures }
    }

    fn white_pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_without_en_passant(all_pieces, index, enemy_pieces).and(!ROW_0)
    }

    fn white_pawn_promotion_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::white_pawn_without_en_passant(all_pieces, index, enemy_pieces).and(ROW_0)
    }

    fn black_pawn_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        let pawn = Bitset::from_index(index);
        let captures = {
            let forward = pawn << 8;
            let a = (forward & !COLUMN_0) >> 1;
            let b = (forward & !COLUMN_7) << 1;
            (a | b) & enemy_pieces
        };
        let moves = {
            let single = (pawn << 8) & !all_pieces;
            let double = ((((pawn & ROW_1) << 8) & !all_pieces) << 8) & !all_pieces;
            single | double
        };
        Self { moves, captures }
    }

    fn black_pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_without_en_passant(all_pieces, index, enemy_pieces).and(!ROW_7)
    }

    fn black_pawn_promotion_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Self {
        Self::black_pawn_without_en_passant(all_pieces, index, enemy_pieces).and(ROW_7)
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
            Moves::white_pawn_promotion_without_en_passant,
        )
    }

    fn iter_black_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> impl Iterator<Item = Move> {
        Self::iter_pawns_without_en_passant(
            we,
            enemy,
            Moves::black_pawn_normal_without_en_passant,
            Moves::black_pawn_promotion_without_en_passant,
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
                can_castle_left: true,
                can_castle_right: true,
            },
            white: PlayerBoard {
                rooks: Bitset::from_index(WHITE_ROOK_LEFT_STARTING_INDEX) | Bitset::from_index(WHITE_ROOK_RIGHT_STARTING_INDEX),
                knights: Bitset::from_position(1, 7) | Bitset::from_position(6, 7),
                bishops: Bitset::from_position(2, 7) | Bitset::from_position(5, 7),
                queens: Bitset::from_position(3, 7),
                king: Bitset::from_index(WHITE_KING_STARTING_INDEX),
                pawns: white_pawns,
                can_castle_left: true,
                can_castle_right: true,
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

    // TODO
    fn white_castle_right_apply(&mut self) {
        let (king_from, king_to) = (WHITE_KING_STARTING_INDEX, WHITE_KING_STARTING_INDEX+2);
        let (rook_from, rook_to) = (WHITE_ROOK_RIGHT_STARTING_INDEX, position_to_index(5, 7));
        self.white.king.mov(king_from, king_to);
        self.white.rooks.mov(rook_from, rook_to);
        self.white.disable_castle();
    }

    // TODO
    fn white_castle_left_apply(&mut self) {
        let (king_from, king_to) = (WHITE_KING_STARTING_INDEX, WHITE_KING_STARTING_INDEX-2);
        let (rook_from, rook_to) = (WHITE_ROOK_LEFT_STARTING_INDEX, position_to_index(3, 7));
        self.white.king.mov(king_from, king_to);
        self.white.rooks.mov(rook_from, rook_to);
        self.white.disable_castle();
    }

    // TODO
    fn black_castle_right_apply(&mut self) {
        let (king_from, king_to) = (BLACK_KING_STARTING_INDEX, BLACK_KING_STARTING_INDEX+2);
        let (rook_from, rook_to) = (BLACK_ROOK_RIGHT_STARTING_INDEX, position_to_index(5, 0));
        self.black.king.mov(king_from, king_to);
        self.black.rooks.mov(rook_from, rook_to);
        self.black.disable_castle();
    }

    // TODO
    fn black_castle_left_apply(&mut self) {
        let (king_from, king_to) = (BLACK_KING_STARTING_INDEX, BLACK_KING_STARTING_INDEX-2);
        let (rook_from, rook_to) = (BLACK_ROOK_LEFT_STARTING_INDEX, position_to_index(3, 0));
        self.black.king.mov(king_from, king_to);
        self.black.rooks.mov(rook_from, rook_to);
        self.black.disable_castle();
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
        let white_moves: Vec<_> = game.iter_white_games_moves()
            .map(|(mov, _)| mov)
            .collect();
        assert_eq!(
            white_moves.iter().copied().collect::<HashSet<_>>().len(),
            white_moves.len(),
        );
        assert_eq!(white_moves.len(), 20);

        game.next_move_color = game.next_move_color.other();
        let black_moves: Vec<_> = game.iter_black_games_moves()
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

trait Side {
    fn color() -> Color;

    fn search(&mut self);

    fn current(&self) -> &Board;
    fn current_we(&self) -> &PlayerBoard;
    fn current_enemy(&self) -> &PlayerBoard;

    fn next(&mut self) -> &mut Board;
    fn next_we(&mut self) -> &mut PlayerBoard;
    fn next_enemy(&mut self) -> &mut PlayerBoard;

    fn pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;
    fn pawn_promotion_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves;

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves;
}

struct WhiteSide {
    current: Board,
    next: Board,
    score: Score,
    depth: usize,
}

impl WhiteSide {
    fn new(board: &Board, depth: usize) -> Self {
        Self {
            current: board.clone(),
            next: board.clone(),
            score: Score::zero(),
            depth,
        }
    }
}

impl Side for WhiteSide {
    fn color() -> Color {
        Color::White
    }

    fn search(&mut self) {
        if self.current.white.king.is_empty() {
            self.score.update_white(Score::checkmate_white());
        } else if self.depth == 0 {
            self.score.update_white(Score::board(self.current()));
        } else {
            let mut other = BlackSide::new(&self.next, self.depth - 1);
            search(&mut other);
            self.score.update_white(other.score);
        }
    }

    fn current(&self) -> &Board {
        &self.current
    }

    fn current_we(&self) -> &PlayerBoard {
        &self.current.white
    }

    fn current_enemy(&self) -> &PlayerBoard {
        &self.current.black
    }

    fn next(&mut self) -> &mut Board {
        &mut self.next
    }

    fn next_we(&mut self) -> &mut PlayerBoard {
        &mut self.next.white
    }

    fn next_enemy(&mut self) -> &mut PlayerBoard {
        &mut self.next.black
    }

    fn pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::white_pawn_normal_without_en_passant(all_pieces, index, enemy_pieces)
    }

    fn pawn_promotion_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::white_pawn_promotion_without_en_passant(all_pieces, index, enemy_pieces)
    }

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves {
        Moves::white_pawns_without_en_passant(we, enemy)
    }
}

struct BlackSide {
    current: Board,
    next: Board,
    score: Score,
    depth: usize,
}

impl BlackSide {
    fn new(board: &Board, depth: usize) -> Self {
        Self {
            current: board.clone(),
            next: board.clone(),
            score: Score::zero(),
            depth,
        }
    }
}

impl Side for BlackSide {
    fn color() -> Color {
        Color::Black
    }

    fn search(&mut self) {
        if self.current.black.king.is_empty() {
            self.score.update_black(Score::checkmate_black());
        } else if self.depth == 0 {
            self.score.update_black(Score::board(self.current()));
        } else {
            let mut other = WhiteSide::new(&self.next, self.depth - 1);
            search(&mut other);
            self.score.update_black(other.score);
        }
    }

    fn current(&self) -> &Board {
        &self.current
    }

    fn current_we(&self) -> &PlayerBoard {
        &self.current.black
    }

    fn current_enemy(&self) -> &PlayerBoard {
        &self.current.white
    }

    fn next(&mut self) -> &mut Board {
        &mut self.next
    }

    fn next_we(&mut self) -> &mut PlayerBoard {
        &mut self.next.black
    }

    fn next_enemy(&mut self) -> &mut PlayerBoard {
        &mut self.next.white
    }

    fn pawn_normal_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::black_pawn_normal_without_en_passant(all_pieces, index, enemy_pieces)
    }

    fn pawn_promotion_without_en_passant(all_pieces: Bitset, index: u8, enemy_pieces: Bitset) -> Moves {
        Moves::black_pawn_promotion_without_en_passant(all_pieces, index, enemy_pieces)
    }

    fn all_pawns_without_en_passant(we: &PlayerBoard, enemy: &PlayerBoard) -> Moves {
        Moves::black_pawns_without_en_passant(we, enemy)
    }
}

fn search_white(board: &Board) -> Score {
    let mut side = WhiteSide::new(board, DEFAULT_DEPTH);
    search(&mut side);
    side.score
}

fn search_black(board: &Board) -> Score {
    let mut side = BlackSide::new(board, DEFAULT_DEPTH);
    search(&mut side);
    side.score
}

fn apply_moves_with<S: Side>(
    side: &mut S,
    get_piece_bitset: impl Fn(&PlayerBoard) -> &Bitset,
    get_piece_bitset_mut: impl Fn(&mut PlayerBoard) -> &mut Bitset,
    get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
    next: impl Fn(&mut S, u8, u8),
) {
    let enemy_pieces = side.current_enemy().bitset();
    let all_pieces = enemy_pieces | side.current_we().bitset();

    for from in get_piece_bitset(side.current_we()).indices() {
        let Moves { moves, captures } = get_moves(all_pieces, from, enemy_pieces);
        for to in moves.indices() {
            get_piece_bitset_mut(side.next_we()).mov(from, to);
            next(side, from, to);
            *get_piece_bitset_mut(side.next_we()) = *get_piece_bitset(side.current_we());
        }
        for to in captures.indices() {
            side.next_enemy().captured(to);
            get_piece_bitset_mut(side.next_we()).mov(from, to);
            next(side, from, to);
            *get_piece_bitset_mut(side.next_we()) = *get_piece_bitset(side.current_we());
            *side.next_enemy() = *side.current_enemy();
        }
    }
}

fn apply_moves<S: Side>(
    side: &mut S,
    get_piece_bitset: impl Fn(&PlayerBoard) -> &Bitset,
    get_piece_bitset_mut: impl Fn(&mut PlayerBoard) -> &mut Bitset,
    get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
) {
    apply_moves_with(
        side,
        get_piece_bitset,
        get_piece_bitset_mut,
        get_moves,
        |side, _, _| side.search(),
    );
}

fn search(side: &mut impl Side) {
    side.current().debug_check();
    search_pawns(side);
    search_queens(side);
    search_rooks(side);
    search_bishops(side);
    search_knights(side);
    search_king(side);
    search_castle(side);
}

fn search_king(side: &mut impl Side) {
    apply_moves(
        side,
        |player_board| &player_board.king,
        |player_board| &mut player_board.king,
        Moves::king,
    );
}

fn search_knights(side: &mut impl Side) {
    apply_moves(
        side,
        |player_board| &player_board.knights,
        |player_board| &mut player_board.knights,
        Moves::knight,
    );
}

fn search_bishops(side: &mut impl Side) {
    apply_moves(
        side,
        |player_board| &player_board.bishops,
        |player_board| &mut player_board.bishops,
        Moves::bishop,
    );
}

fn search_rooks(side: &mut impl Side) {
    apply_moves(
        side,
        |player_board| &player_board.rooks,
        |player_board| &mut player_board.rooks,
        Moves::rook,
    );
}

fn search_queens(side: &mut impl Side) {
    apply_moves(
        side,
        |player_board| &player_board.queens,
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
    let all_pieces = side.current().bitset();
    let king_index = side.current_we().king.first_index();

    let king_starting_index = S::color().king_starting_index();
    let row = S::color().first_row();

    let allowed = king_index == king_starting_index
        && side.current_we().can_castle_right()
        && side.current_we().rooks.has(S::color().rook_right_starting_index())
        && !all_pieces.has(position_to_index(5, row))
        && !all_pieces.has(position_to_index(6, row));
    if !allowed {
        return false;
    }

    // TODO: clone
    let current_we = side.current_we().clone();
    for shift in 0..=2 {
        side.next_we().king.mov(king_starting_index, king_starting_index+shift);
        let check = side.next_enemy().has_check(&current_we, S::color().other());
        side.next_we().king = side.current_we().king;
        if check {
            return false;
        }
    }
    true
}

fn can_castle_left<S: Side>(side: &mut S) -> bool {
    let all_pieces = side.current().bitset();
    let king_index = side.current_we().king.first_index();

    let king_starting_index = S::color().king_starting_index();
    let row = S::color().first_row();

    let allowed = king_index == king_starting_index
        && side.current_we().can_castle_left()
        && side.current_we().rooks.has(S::color().rook_left_starting_index())
        && !all_pieces.has(position_to_index(3, row))
        && !all_pieces.has(position_to_index(2, row))
        && !all_pieces.has(position_to_index(1, row));
    if !allowed {
        return false;
    }

    // TODO: clone
    let current_we = side.current_we().clone();
    for shift in 0..=2 {
        side.next_we().king.mov(king_starting_index, king_starting_index-shift);
        let check = side.next_enemy().has_check(&current_we, S::color().other());
        side.next_we().king = side.current_we().king;
        if check {
            return false;
        }
    }
    true
}

fn search_castle_reset(side: &mut impl Side) {
    // TODO: clone
    let current_we = side.current_we().clone();
    side.next_we().reset_castle(&current_we);
    side.next_we().rooks = side.current_we().rooks;
    side.next_we().king = side.current_we().king;
}

fn search_castle_right<S: Side>(side: &mut S) {
    side.next_we().king.mov(
        S::color().king_starting_index(),
        S::color().king_starting_index() + 2,
    );
    side.next_we().rooks.mov(
        S::color().rook_right_starting_index(),
        position_to_index(5, S::color().first_row(),
    ));
    side.next_we().disable_castle();

    side.search();
    search_castle_reset(side);
}

fn search_castle_left<S: Side>(side: &mut S) {
    side.next_we().king.mov(
        S::color().king_starting_index(),
        S::color().king_starting_index() - 2,
    );
    side.next_we().rooks.mov(
        S::color().rook_left_starting_index(),
        position_to_index(3, S::color().first_row(),
    ));
    side.next_we().disable_castle();

    side.search();
    search_castle_reset(side);
}

fn search_pawns<S: Side>(side: &mut S) {
    // TODO: double step save en passant index

    apply_moves(
        side,
        |player_board| &player_board.pawns,
        |player_board| &mut player_board.pawns,
        S::pawn_normal_without_en_passant,
    );

    apply_moves_with(
        side,
        |player_board| &player_board.pawns,
        |player_board| &mut player_board.pawns,
        S::pawn_promotion_without_en_passant,
        |side, _, to| {
            side.next_we().pawns.clear(to);
            search_pawn_promote_figures(side, to);
        },
    );

    if let Some(en_passant_index) = side.current().en_passant_index {
        search_en_passant(side, en_passant_index);
    }
}

fn search_pawn_promote_figures(side: &mut impl Side, to: u8) {
    side.next_we().queens.set(to);
    side.search();
    side.next_we().queens = side.current_we().queens;

    side.next_we().rooks.set(to);
    side.search();
    side.next_we().rooks = side.current_we().rooks;

    side.next_we().knights.set(to);
    side.search();
    side.next_we().knights = side.current_we().knights;

    side.next_we().bishops.set(to);
    side.search();
    side.next_we().bishops = side.current_we().bishops;
}

fn search_en_passant_single(
    side: &mut impl Side,
    captured: u8,
    from: u8,
    to: u8,
) {
    side.next_enemy().captured(captured);
    side.next_we().pawns.mov(from, to);
    side.search();
    side.next_we().pawns = side.current_we().pawns;
    *side.next_enemy() = *side.current_enemy();
}

fn search_en_passant<S: Side>(side: &mut S, en_passant_index: u8) {
    let (x, y) = index_to_position(en_passant_index);
    let next_y = ((y as i8) + S::color().direction()) as u8;

    if side.current_we().has_en_passant_left(side.current_enemy(), en_passant_index) {
        search_en_passant_single(
            side,
            en_passant_index,
            position_to_index(x-1, y),
            position_to_index(x, next_y),
        );
    }

    if side.current_we().has_en_passant_right(side.current_enemy(), en_passant_index) {
        search_en_passant_single(
            side,
            en_passant_index,
            position_to_index(x+1, y),
            position_to_index(x, next_y),
        );
    }
}
