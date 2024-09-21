use crate::{board::{Board, SCORE_MAX, SCORE_MIN}, mov::Move, moves::MovesBuilder};

pub struct Searcher {
    max_depth: usize,
}

// TODO: Consider repetitions.
impl Searcher {
    pub fn new(max_depth: usize) -> Self {
        Self { max_depth }
    }

    pub fn run(&self, board: &mut Board) -> (Move, i32) {
        let move_score = self.search(board, 0, SCORE_MIN, SCORE_MAX).unwrap();
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
            return Some((Move::UNINITIALIZED, board.score()));
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
                Some((Move::UNINITIALIZED, SCORE_MIN + depth as i32))
            } else {
                Some((Move::UNINITIALIZED, 0))
            }
        } else {
            Some((best_move, alpha))
        }
    }
}
