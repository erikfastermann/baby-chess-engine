use std::{collections::HashMap, error, fmt};

use crate::{board::{Board, PositionBoard}, color::Color, config, mov::{Move, UserMove}, moves::MovesBuilder, result::Result, search::Searcher};

#[derive(Clone)]
pub struct Game {
    board: Board,
    full_position_counts: HashMap<PositionBoard, usize>,
}

impl Game {
    pub fn new() -> Self {
        let mut game = Self {
            board: Board::start(),
            full_position_counts: HashMap::new(),
        };
        game.full_position_counts.insert(game.board.clone().to_position_board(), 1);
        game
    }

    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut game = Self {
            board: Board::from_fen(fen)?,
            full_position_counts: HashMap::new(),
        };
        // We don't know the previous positions, so we just insert the current one.
        game.full_position_counts.insert(game.board.clone().to_position_board(), 1);
        Ok(game)
    }

    fn legal_moves(&self) -> Vec<Move> {
        let is_draw = self.board.is_draw_fast()
            || self.full_position_counts.values().copied().any(|n| n >= 3);
        if is_draw {
            return Vec::new();
        }
        let mut next_game = self.clone();
        let mut builder = MovesBuilder::new();
        let moves = builder.fill(&mut next_game.board);
        moves.iter()
            .copied()
            .filter(|mov| !next_game.move_has_check(&self.board, *mov))
            .collect()
    }

    fn move_has_check(&mut self, old_board: &Board, mov: Move) -> bool {
        self.board.apply_move_unchecked(mov);
        let has_check = self.has_check(self.board.color);
        self.board.reset_with(old_board);
        has_check
    }

    pub fn reset(&mut self) {
        self.board = Board::start();
        self.full_position_counts.clear();
    }

    fn reset_with(&mut self, other: &Self) {
        self.board = other.board.clone();
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

    fn is_draw(&self, next_board: &mut Board, mov: Move) -> bool {
        next_board.apply_move_unchecked(mov);
        let count = self.full_position_counts.get(&next_board.clone().to_position_board())
            .copied()
            .unwrap_or(0);
        let is_draw = next_board.is_draw_fast() || count > 2;
        next_board.reset_with(&self.board);
        is_draw
    }

    pub fn apply_move(&mut self, user_move: UserMove) -> Result<()> {
        let mov = self.legal_moves()
            .iter()
            .find(|mov| mov.to_move() == user_move)
            .copied();
        if let Some(mov) = mov {
            self.apply_move_unchecked(mov);
            Ok(())
        } else {
            Err(format!("illegal move {user_move}").into())
        }
    }

    fn apply_move_unchecked(&mut self, mov: Move) {
        self.board.apply_move_unchecked(mov);
        let count = self.full_position_counts
            .entry(self.board.clone().to_position_board())
            .or_insert(0);
        *count += 1;
    }

    pub fn best_move(&self) -> Result<(UserMove, i32)> {
        let mut next_board = self.board.clone();
        // TODO: Include repetitions if it might be favorable for us.
        let moves = {
            let legal_moves = self.legal_moves();
            let moves: Vec<_> = legal_moves
                .iter()
                .copied()
                .filter(|mov| !self.is_draw(&mut next_board, *mov))
                .collect();
            if moves.is_empty() { legal_moves } else { moves }
        };

        let mut moves_with_score = Vec::new();
        for mov in moves {
            next_board.apply_move_unchecked(mov);
            let score = -Searcher::new(config::DEFAULT_DEPTH - 1).run(&mut next_board);
            next_board.reset_with(&self.board);
            moves_with_score.push((mov, score));
        }

        moves_with_score.sort_by(|(_, score_a), (_, score_b)| {
            score_a.cmp(score_b).reverse()
        });
        dbg!(&moves_with_score);
        let best_move = moves_with_score.first().copied();
        match best_move {
            Some((mov, score)) => Ok((mov.to_move(), score)),
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

    use crate::{board::SCORE_MAX, init::init, piece::Piece, position};

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
            let mov = UserMove::from_long_algebraic_notation(raw_mov).unwrap();
            game.apply_move(mov).unwrap();
        }
        game.board.debug_check();
    }

    #[test]
    fn test_mate() {
        unsafe { init() };

        const HARD_MATE_FEN: &str = "8/1N2N3/2r5/3qp2R/QP2kp1K/5R2/6B1/6B1 w - - 0 0";
        let game = Game::from_fen(HARD_MATE_FEN).unwrap();
        let mov = game.best_move().unwrap();
        assert!(mov.1 >= SCORE_MAX-4)
    }

    #[test]
    fn test_promote() {
        unsafe { init() }

        const PROMOTE_FEN: &str = "r3kb1r/ppPqpppp/n3b2n/8/8/5N2/PPPP1PPP/RNBQKB1R w KQkq - 11 10";
        let game = Game::from_fen(PROMOTE_FEN).unwrap();
        assert!(game.legal_moves().contains(&Move::promotion(
            position::human_position_to_index(b"c7").unwrap(),
            position::human_position_to_index(b"c8").unwrap(),
            Piece::Rook,
        )));

        const CAPTURE_PROMOTE_FEN: &str = "rnbqkb1r/ppP1pppp/7n/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 1 5";
        let game = Game::from_fen(CAPTURE_PROMOTE_FEN).unwrap();
        assert!(game.legal_moves().contains(&Move::promotion_capture(
            position::human_position_to_index(b"c7").unwrap(),
            position::human_position_to_index(b"d8").unwrap(),
            Piece::Queen,
        )));
    }

    #[test]
    fn test_50_move_rule() {
        const DRAW_FEN: &str = "N7/K7/8/8/8/8/7k/7n w - - 50 50";
        let draw = Game::from_fen(DRAW_FEN).unwrap();
        assert_eq!(draw.legal_moves(), &[]);
    }
}
