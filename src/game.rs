use std::{collections::HashMap, fmt, error};

use crate::{board::Board, color::Color, mov::Move, search, config, result::Result, moves::FullMovesBuffer};

const SCORE_MIN: i32 = i32::MAX * -1;
const SCORE_MAX: i32 = i32::MAX;

#[derive(Clone)]
pub struct Game {
    board: Board,
    full_position_counts: HashMap<Board, usize>,
}

// TODO:
// Currently we disallow repeating moves entirely.
// When we are losing it might be nice to repeat moves to get a draw.
impl Game {
    pub fn new() -> Self {
        let mut game = Self {
            board: Board::start(),
            full_position_counts: HashMap::new(),
        };
        game.full_position_counts.insert(game.board.clone(), 1);
        game
    }

    fn legal_moves(&self) -> Vec<Move> {
        let mut next_game = self.clone();
        let mut legal_moves = Vec::new();

        let mut moves_buffer = FullMovesBuffer::new();
        let moves = moves_buffer.fill(&mut next_game.board);

        let special = moves.special.iter()
            .filter(|mov| !next_game.unchecked_move_has_check_or_repetition(self, **mov));
        legal_moves.extend(special);

        let simple = moves.simple.iter()
            .map(|(from, to)| Move::Normal { from: *from, to: *to })
            .filter(|mov| !next_game.unchecked_move_has_check_or_repetition(self, *mov));
        legal_moves.extend(simple);

        legal_moves
    }

    fn unchecked_move_has_check_or_repetition(&mut self, old: &Self, mov: Move) -> bool {
        self.apply_move_unchecked(mov);
        let has_check_or_repetition = self.has_check(self.board.color)
            || self.full_position_counts.get(&self.board).is_some_and(|count| *count >= 3);
        self.reset_with(old);
        has_check_or_repetition
    }

    pub fn reset(&mut self) {
        self.board = Board::start();
        self.full_position_counts.clear();
    }

    fn reset_with(&mut self, other: &Self) {
        // TODO: reset single move
        self.board = other.board.clone();
        // TODO: don't copy the hashmap every time
        self.full_position_counts.clear();
        self.full_position_counts.extend(
            other.full_position_counts.iter()
                .map(|(board, count)| (board.clone(), *count)),
        );
    }

    fn has_check(&self, we: Color) -> bool {
        self.board.player_board(we)
            .has_check(self.board.player_board(we.other()), we)
    }

    pub fn apply_move(&mut self, mov: Move) -> Result<()> {
        let is_legal = self.legal_moves().contains(&mov);
        if is_legal {
            self.apply_move_unchecked(mov);
            Ok(())
        } else {
            Err(format!("illegal move {mov}").into())
        }
    }

    fn apply_move_unchecked(&mut self, mov: Move) {
        self.board.apply_move_unchecked(mov);

        let count = self.full_position_counts
            .entry(self.board.clone())
            .or_insert(0);
        *count += 1;
        assert!(*count < 3);
    }

    pub fn best_move(&self) -> Result<Move> {
        let mut next_game = self.clone();

        let mut max_score = None;
        let mut best_move = None;
        for mov in self.legal_moves() {
            next_game.apply_move_unchecked(mov);
            let score = -search::search(
                &mut next_game.board,
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
                if self.has_check(self.board.color.other()) {
                    Err(EndOfGameError::Checkmate.into())
                } else {
                    Err(EndOfGameError::Other.into())
                }
            },
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

#[cfg(test)]
mod test {
    use std::collections::HashSet;

    use crate::init::init;

    use super::*;

    #[test]
    fn test_move_counts_start() {
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
            let count = count_moves(&game, depth, &mut found_boards);
            assert_eq!(count, expected_count);
            assert_eq!(
                found_boards.values().sum::<u64>(),
                expected_count,
            );
        }
    }

    fn count_moves(game: &Game, depth: usize, found_boards: &mut HashMap<Board, u64>) -> u64 {
        if depth == 0 {
            *found_boards.entry(game.board.clone()).or_insert(0) += 1;
            return 1;
        }

        let mut next_game = game.clone();
        let mut count = 0;
        for mov in game.legal_moves() {
            next_game.apply_move_unchecked(mov);
            count += count_moves(&next_game, depth - 1, found_boards);
            next_game.reset_with(game);
        }
        count
    }

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

        game.board.color = game.board.color.other();
        let black_moves: Vec<_> = game.legal_moves();
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