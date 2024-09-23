use crate::{board::{Board, PositionBoard, MAX_MOVES_SINCE_CAPTURE_OR_PAWN}, config, eval, mov::Move, moves::MovesBuilder};

const MOVES_HASHES_SIZE: usize = MAX_MOVES_SINCE_CAPTURE_OR_PAWN as usize + config::MAX_DEPTH;

pub struct Searcher {
    max_depth: usize,
    // Because of the 50 move rule, it is ok to only consider the last 100 half-move hashes,
    // because repetitions are impossible afterwards.
    last_moves_hashes: [u64; MOVES_HASHES_SIZE],
}

impl Searcher {
    pub fn new(max_depth: usize, previous_positions: &[PositionBoard]) -> Self {
        assert!(max_depth <= config::MAX_DEPTH);
        assert!(previous_positions.len() > 0);
        let mut last_moves_hashes = [0u64; MOVES_HASHES_SIZE];
        let position_iter = previous_positions.iter().rev().zip(
            last_moves_hashes[..usize::from(MAX_MOVES_SINCE_CAPTURE_OR_PAWN)].iter_mut().rev(),
        );
        for (position, hash) in position_iter {
            *hash = position.zobrist_hash();
        }
        Self {
            max_depth,
            last_moves_hashes,
        }
    }

    pub fn run(&mut self, board: &mut Board) -> (Move, i32) {
        let hash = self.last_moves_hashes[usize::from(MAX_MOVES_SINCE_CAPTURE_OR_PAWN) - 1];
        assert_eq!(board.clone().to_position_board().zobrist_hash(), hash);
        let move_score = self.search(
            board,
            0,
            eval::SCORE_MIN,
            eval::SCORE_MAX,
        ).unwrap();
        assert_ne!(move_score.0, Move::UNINITIALIZED);
        move_score
    }

    fn hash_move(&mut self, board: &Board, mov: Move, depth: usize) {
        let index = usize::from(MAX_MOVES_SINCE_CAPTURE_OR_PAWN) + depth;
        let current_hash = self.last_moves_hashes[index - 1];
        debug_assert_eq!(board.clone().to_position_board().zobrist_hash(), current_hash);
        let incremental_hash = board.clone()
            .to_position_board()
            .incremental_zobrist_hash_unchecked(mov);
        self.last_moves_hashes[index] = current_hash ^ incremental_hash;
    }

    fn is_threefold_repetition(&self, depth: usize) -> bool {
        // If hash collisions occur this might be inaccurate,
        // but should be really unlikely.
        let current_index = usize::from(MAX_MOVES_SINCE_CAPTURE_OR_PAWN) + depth - 1;
        let current_hash = self.last_moves_hashes[current_index];
        let mut count = 0;
        for hash in self.last_moves_hashes[..=current_index-2].iter()
            .copied()
            .rev()
            .step_by(2) {
                count += usize::from(hash == current_hash);
        }
        count >= 2
    }

    fn search(
        &mut self,
        board: &mut Board,
        depth: usize,
        mut alpha: i32,
        beta: i32,
    ) -> Option<(Move, i32)> {
        board.debug_check();
        let enemy_in_check = board.we().has_check(board.enemy(), board.color);
        if enemy_in_check {
            return None;
        }
        if board.is_draw_fast() || self.is_threefold_repetition(depth) {
            return Some((Move::UNINITIALIZED, 0));
        }
        if depth == self.max_depth {
            let score = self.quiescence_search(board, depth, alpha, beta).unwrap();
            return Some((Move::UNINITIALIZED, score));
        }

        let mut moves_buffer = MovesBuilder::new(false);
        moves_buffer.fill(board);
        let moves = moves_buffer.sort(board);
        let old_board = board.clone();
        let mut valid_move = false;

        let mut best_move = Move::UNINITIALIZED;
        for mov in moves.iter().copied() {
            self.hash_move(board, mov, depth);
            board.apply_move_unchecked(mov);
            let score = self.search(board, depth + 1, -beta, -alpha)
                .map(|(_, score)| -score);
            // TODO: Undo move.
            board.reset_with(&old_board);

            let Some(score) = score else {
                continue;
            };
            valid_move = true;
            if score >= beta {
                return Some((Move::UNINITIALIZED, beta));
            }
            if score > alpha {
                alpha = score;
                best_move = mov;
            }
        }

        if !valid_move {
            let we_in_check = board.enemy().has_check(board.we(), board.color.other());
            if we_in_check {
                Some((Move::UNINITIALIZED, eval::SCORE_MIN + depth as i32))
            } else {
                Some((Move::UNINITIALIZED, 0))
            }
        } else {
            Some((best_move, alpha))
        }
    }

    fn quiescence_search(
        &self,
        board: &mut Board,
        depth: usize,
        mut alpha: i32,
        beta: i32,
    ) -> Option<i32> {
        board.debug_check();

        let enemy_in_check = board.we().has_check(board.enemy(), board.color);
        if enemy_in_check {
            return None;
        }
        // No need to check for a draw, since only captures are considered.

        let stand_score = eval::score(board);
        if stand_score >= beta {
            return Some(beta);
        }
        if stand_score > alpha {
            alpha = stand_score;
        }

        if depth == config::MAX_DEPTH_QUIESCENCE {
            return Some(alpha);
        }

        let mut moves_buffer = MovesBuilder::new(true);
        moves_buffer.fill(board);
        let moves = moves_buffer.sort(board);
        let old_board = board.clone();

        for mov in moves.iter().copied() {
            board.apply_move_unchecked(mov);
            let score = self.quiescence_search(board, depth + 1, -beta, -alpha)
                .map(|score| -score);
            // TODO: Undo move.
            board.reset_with(&old_board);

            let Some(score) = score else {
                continue;
            };
            if score >= beta {
                return Some(beta);
            }
            if score > alpha {
                alpha = score;
            }
        }

        Some(alpha)
    }
}
