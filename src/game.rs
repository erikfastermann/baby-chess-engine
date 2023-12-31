use std::{collections::HashMap, fmt, error};

use crate::{board::Board, color::Color, mov::Move, visit::{visit, Visitor}, side::{WhiteSide, BlackSide, Side, castle_left_apply, castle_right_apply}, piece::Piece, position::index_to_position, search::{search_white, search_black, self}, config, result::Result};

const SCORE_MIN: i32 = i32::MAX * -1;
const SCORE_MAX: i32 = i32::MAX;

#[derive(Clone)]
pub struct Game {
    board: Board,
    next_move_color: Color,
    full_position_counts: HashMap<Board, usize>,
}

// TODO:
// Currently we disallow repeating moves entirely.
// When we are losing it might be nice to repeat moves to get a draw.
impl Game {
    pub fn new() -> Self {
        let mut game = Self {
            board: Board::start(),
            next_move_color: Color::White,
            full_position_counts: HashMap::new(),
        };
        game.full_position_counts.insert(game.board.clone(), 1);
        game
    }

    fn legal_moves(&self) -> Vec<Move> {
        let mut moves = Vec::new();
        let mut moves_builder = LegalMovesBuilder {
            moves: &mut moves,
            full_position_counts: &self.full_position_counts,
        };
        let mut board = self.board.clone();
        match self.next_move_color {
            Color::White => {
                let mut side = WhiteSide::new(&mut board);
                visit(&mut side, &mut moves_builder)
            },
            Color::Black => {
                let mut side = BlackSide::new(&mut board);
                visit(&mut side, &mut moves_builder)
            }
        };
        moves
    }

    pub fn reset(&mut self) {
        self.board = Board::start();
        self.next_move_color = Color::White;
        self.full_position_counts.clear();
    }

    fn reset_with(&mut self, other: &Self) {
        self.board = other.board.clone();
        self.next_move_color = other.next_move_color;
        // TODO: don't copy the hashmap every time
        self.full_position_counts.clear();
        self.full_position_counts.extend(
            other.full_position_counts.iter()
                .map(|(board, count)| (board.clone(), *count)),
        );
    }

    fn has_check(&self) -> bool {
        match self.next_move_color {
            Color::White => self.board.black.has_check(&self.board.white, Color::Black),
            Color::Black => self.board.black.has_check(&self.board.black, Color::White),
        }
    }

    // TODO: use board logic here

    pub fn apply_move(&mut self, mov: Move) -> Result<()> {
        let is_legal = self.legal_moves().contains(&mov);
        if is_legal {
            let _ = self.apply_move_unchecked(mov);
            Ok(())
        } else {
            Err(format!("illegal move {mov}").into())
        }
    }

    fn apply_move_unchecked(&mut self, mov: Move) {
        self.board.en_passant_index = match self.next_move_color {
            Color::White => {
                let mut side = WhiteSide::new(&mut self.board);
                let en_passant_index = Self::apply_move_side_unchecked(&mut side, mov);
                en_passant_index
            },
            Color::Black => {
                let mut side = BlackSide::new(&mut self.board);
                let en_passant_index = Self::apply_move_side_unchecked(&mut side, mov);
                en_passant_index
            },
        };
        self.next_move_color = self.next_move_color.other();

        let count = self.full_position_counts
            .entry(self.board.clone())
            .or_insert(0);
        *count += 1;
        assert!(*count < 3);
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
            side.we_mut().remove_piece(piece, from);
            side.enemy_mut().captured(to);
            side.we_mut().place_piece(piece, to);

            Self::disable_castle(side, from, to);
            Self::possible_en_passant(piece, from, to)
        }
    }

    fn apply_move_en_passant<S: Side>(side: &mut S, from: u8, to: u8) -> bool {
        let Some(en_passant_index) = side.board().en_passant_index else {
            return false;
        };
        if !side.we().pawns().has(from) {
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
        side.we_mut().pawns_mut().mov(from, to);
        side.enemy_mut().pawns_mut().checked_clear(en_passant_index);
        true
    }

    fn disable_castle<S: Side>(side: &mut S, from: u8, to: u8) {
        if from == S::color().king_starting_index() || to == S::color().king_starting_index() {
            side.we_mut().disable_castle();
        } else if from == S::color().rook_left_starting_index() || to == S::color().rook_left_starting_index() {
            side.we_mut().disable_castle_left();
        } else if from == S::color().rook_right_starting_index() || to == S::color().rook_right_starting_index() {
            side.we_mut().disable_castle_right();
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
        assert!(side.we().pawns().has(from));
        side.we_mut().pawns_mut().checked_clear(from);
        side.enemy_mut().captured(to);
        side.we_mut().place_piece(piece, to);
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

    pub fn best_move(&self) -> Result<Move> {
        let mut next_game = self.clone();

        let mut max_score = None;
        let mut best_move = None;
        for mov in self.legal_moves() {
            next_game.apply_move_unchecked(mov);
            let score = -search::search(
                &mut next_game.board,
                self.next_move_color.other(),
                config::DEFAULT_DEPTH,
                SCORE_MIN,
                SCORE_MAX,
            );
            next_game.reset_with(&self);

            if Some(score) > max_score {
                max_score = Some(score);
                best_move = Some(mov);
            }
        }

        match best_move {
            Some(mov) => Ok(mov),
            None => {
                if self.has_check() {
                    Err(EndOfGameError::Checkmate.into())
                } else {
                    Err(EndOfGameError::Other.into())
                }
            },
        }
    }

    // TODO: remove
    pub fn best_move_old(&self) -> Result<Move> {
        let mut next_game = self.clone();

        let mut max_score = None;
        let mut best_move = None;
        for mov in self.legal_moves() {
            next_game.apply_move_unchecked(mov);
            let score = match self.next_move_color.other() {
                Color::White => -search_white(
                    &mut next_game.board,
                    config::DEFAULT_DEPTH,
                    SCORE_MIN,
                    SCORE_MAX,
                ),
                Color::Black => -search_black(
                    &mut next_game.board,
                    config::DEFAULT_DEPTH,
                    SCORE_MIN,
                    SCORE_MAX,
                ),
            };
            next_game.reset_with(&self);

            if Some(score) > max_score {
                max_score = Some(score);
                best_move = Some(mov);
            }
        }

        match best_move {
            Some(mov) => Ok(mov),
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

struct LegalMovesBuilder<'a> {
    moves: &'a mut Vec<Move>,
    full_position_counts: &'a HashMap<Board, usize>,
}

impl <'a> Visitor for LegalMovesBuilder<'a> {
    fn visit<S: Side>(&mut self, side: &mut S, mov: Move) -> bool {
        let has_check = side.enemy().has_check(
            side.we(),
            S::color().other(),
        );
        if has_check {
            return true;
        }
        let repeats = self.full_position_counts
            .get(side.board())
            .is_some_and(|count| *count >= 2);
        if repeats {
            return true;
        }
        self.moves.push(mov);
        true
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

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::init::init;

    use super::*;

    #[test]
    fn test_moves_start() {
        unsafe { init() };

        let mut game = Game::new();
        let white_moves: Vec<_> = game.legal_moves();
        assert_eq!(
            white_moves.iter().copied().collect::<HashSet<_>>().len(),
            white_moves.len(),
        );
        assert_eq!(white_moves.len(), 20);

        game.next_move_color = game.next_move_color.other();
        let black_moves: Vec<_> = game.legal_moves();
        assert_eq!(
            black_moves.iter().copied().collect::<HashSet<_>>().len(),
            black_moves.len(),
        );
        assert_eq!(black_moves.len(), 20);

        assert_ne!(white_moves, black_moves);
    }

    #[test]
    fn test_moves_start_recursive_old() {
        unsafe { init() };

        static SEARCH_COUNTS: &[u64] = &[
            1,
            20,
            400,
            8_902,
            197_281,
        ];

        let game = Game::new();
        let mut found_boards = HashMap::new();
        for (depth, expected_count) in SEARCH_COUNTS.iter().copied().enumerate() {
            found_boards.clear();
            let count = count_moves_old(&game, depth, &mut found_boards);
            assert_eq!(count, expected_count);
            assert_eq!(
                found_boards.values().sum::<u64>(),
                expected_count,
            );
        }
    }

    fn count_moves_old(game: &Game, depth: usize, found_boards: &mut HashMap<Board, u64>) -> u64 {
        if depth == 0 {
            *found_boards.entry(game.board.clone()).or_insert(0) += 1;
            return 1;
        }

        let mut next_game = game.clone();
        let mut count = 0;
        for mov in game.legal_moves() {
            next_game.apply_move_unchecked(mov);
            count += count_moves_old(&next_game, depth - 1, found_boards);
            next_game.reset_with(game);
        }
        count
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