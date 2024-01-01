use std::cmp::max;

use crate::{board::Board, moves::FullMovesBuffer};

pub fn search(board: &mut Board, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.we().king().is_empty() {
        return board.score();
    }
    let old_board = board.clone();

    let mut moves_buffer = FullMovesBuffer::new();
    let moves = moves_buffer.fill(board);

    // TODO: check found moves count
    // TODO: sort moves

    // Special Moves

    for mov in moves.special {
        board.apply_move_unchecked(*mov);
        let score = -search(board, depth - 1, -beta, -alpha);
        // TODO: un apply move
        board.reset_with(&old_board);
        if score >= beta {
            return beta;
        }
        alpha = max(alpha, score);
    }

    // Simple Moves

    let en_passant_index = board.en_passant_index;
    board.en_passant_index = None;

    for (from, to) in moves.simple {
        let captured_piece = board.apply_simple(*from, *to);
        let score = -search(board, depth - 1, -beta, -alpha);
        board.un_apply_simple(*from, *to, captured_piece);
        if score >= beta {
            board.en_passant_index = en_passant_index;
            return beta;
        }
        alpha = max(alpha, score);
    }

    board.en_passant_index = en_passant_index;
    alpha
}
