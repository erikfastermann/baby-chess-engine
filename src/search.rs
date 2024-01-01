use std::cmp::max;

use crate::{board::Board, moves::SearchMovesBuilder, mov::SearchMove};

pub fn search(board: &mut Board, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.we().king().is_empty() {
        return board.score();
    }
    let old_board = board.clone();

    let mut moves_buffer = SearchMovesBuilder::new();
    moves_buffer.fill(board);
    let moves = moves_buffer.sort(board);

    // TODO: check found moves count
    // TODO: sort moves

    for mov in moves {
        let score = search_move(board, &old_board, *mov, depth, alpha, beta);
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
    mov: SearchMove,
    depth: usize,
    alpha: i32,
    beta: i32,
) -> i32 {
    match mov {
        SearchMove::Simple { from, to } => {
            let en_passant_index = board.en_passant_index;
            board.en_passant_index = None;
            let captured_piece = board.apply_simple(from, to);
            let score = -search(board, depth - 1, -beta, -alpha);
            board.un_apply_simple(from, to, captured_piece);
            board.en_passant_index = en_passant_index;
            score
        },
        SearchMove::Special(mov) => {
            board.apply_move_unchecked(mov);
            let score = -search(board, depth - 1, -beta, -alpha);
            // TODO: un apply move
            board.reset_with(old_board);
            score
        },
    }
}
