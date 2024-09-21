use std::cmp::max;

use crate::{board::Board, moves::MovesBuilder, mov::Move};

pub fn search(board: &mut Board, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.we().king().is_empty() {
        return board.score();
    }
    if board.is_draw_fast() {
        return 0;
    }

    let old_board = board.clone();

    let mut moves_buffer = MovesBuilder::new();
    moves_buffer.fill(board);
    let moves = moves_buffer.sort(board);

    // TODO: check found moves count

    for mov in moves.iter().copied() {
        let score = search_move(board, &old_board, mov, depth, alpha, beta);
        if score >= beta {
            return beta;
        }
        alpha = max(alpha, score);
    }

    alpha
}

pub fn search_move(
    board: &mut Board,
    old_board: &Board,
    mov: Move,
    depth: usize,
    alpha: i32,
    beta: i32,
) -> i32 {
    board.apply_move_unchecked(mov);
    let score = -search(board, depth - 1, -beta, -alpha);
    // TODO: Undo move.
    board.reset_with(old_board);
    score
}
