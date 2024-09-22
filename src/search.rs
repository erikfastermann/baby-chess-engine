use crate::{board::{Board, PositionBoard, MAX_MOVES_SINCE_CAPTURE_OR_PAWN}, config, eval, mov::Move, moves::MovesBuilder};

const MOVES_HASHES_SIZE: usize = MAX_MOVES_SINCE_CAPTURE_OR_PAWN as usize + config::MAX_DEPTH;

pub struct Searcher {
    max_depth: usize,
    // Because of the 50 move rule, it is ok to only consider the last 50 move hashes,
    // because repetitions are impossible afterwards.
    last_moves_hashes: [u64; MOVES_HASHES_SIZE],
}

// TODO: Consider repetitions.
impl Searcher {
    pub fn new(max_depth: usize, previous_positions: &[PositionBoard]) -> Self {
        assert!(max_depth <= config::MAX_DEPTH);
        Self {
            max_depth,
            last_moves_hashes: [0; MOVES_HASHES_SIZE], // TODO
        }
    }

    pub fn run(&self, board: &mut Board) -> (Move, i32) {
        let move_score = self.search(
            board,
            0,
            eval::SCORE_MIN,
            eval::SCORE_MAX,
        ).unwrap();
        assert_ne!(move_score.0, Move::UNINITIALIZED);
        move_score
    }

    fn search(&self, board: &mut Board, depth: usize, mut alpha: i32, beta: i32) -> Option<(Move, i32)> {
        board.debug_check();
        let enemy_in_check = board.we().has_check(board.enemy(), board.color);
        if enemy_in_check {
            return None;
        }
        if board.is_draw_fast() {
            return Some((Move::UNINITIALIZED, 0));
        }
        if depth == self.max_depth {
            return Some((Move::UNINITIALIZED, eval::score(board)));
        }

        let mut moves_buffer = MovesBuilder::new();
        moves_buffer.fill(board);
        let moves = moves_buffer.sort(board);
        let old_board = board.clone();
        let mut valid_move = false;

        let mut best_move = Move::UNINITIALIZED;
        for mov in moves.iter().copied() {
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
}
