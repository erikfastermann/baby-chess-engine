use std::cmp::max;

use crate::{mov::Move, board::Board, color::Color, moves::{SPECIAL_MOVES_BUFFER_LEN, SIMPLE_MOVES_BUFFER_LEN}};

pub fn search(board: &mut Board, color: Color, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.player_board(color).king().is_empty() {
        return board.score(color);
    }

    // TODO: split function

    // TODO: check found moves count

    // Special Moves

    let mut special_moves_buffer = [Move::Normal { from: 0, to: 0 }; SPECIAL_MOVES_BUFFER_LEN];
    let special_moves = board.fill_special_moves(color, &mut special_moves_buffer);

    let old_board = board.clone();
    for mov in special_moves {
        board.apply_move_unchecked(color, *mov);
        let score = -search(board, color.other(), depth - 1, -beta, -alpha);
        // TODO: un apply move
        board.reset_with(&old_board);
        if score >= beta {
            return beta;
        }
        alpha = max(alpha, score);
    }

    // Simple Moves

    let mut simple_moves_buffer = [(0, 0); SIMPLE_MOVES_BUFFER_LEN];
    let (simple_moves, simple_captures) = board.player_board(color).fill_simple_moves(
        board.player_board(color.other()),
        color,
        &mut simple_moves_buffer,
    );

    let en_passant_index = board.en_passant_index;
    board.en_passant_index = None;

    // Captures

    for (from, to) in simple_captures {
        board.player_board_mut(color).move_piece(*from, *to);
        let captured_piece = board.player_board(color.other()).which_piece(*to);
        board.player_board_mut(color.other()).remove_piece(*to);

        let score = -search(board, color.other(), depth - 1, -beta, -alpha);

        board.player_board_mut(color.other()).place_piece(*to, captured_piece);
        board.player_board_mut(color).move_piece(*to, *from);

        if score >= beta {
            board.en_passant_index = en_passant_index;
            return beta;
        }
        alpha = max(alpha, score);
    }

    // Moves

    for (from, to) in simple_moves {
        board.player_board_mut(color).move_piece(*from, *to);
        let score = -search(board, color.other(), depth - 1, -beta, -alpha);
        board.player_board_mut(color).move_piece(*to, *from);

        if score >= beta {
            board.en_passant_index = en_passant_index;
            return beta;
        }
        alpha = max(alpha, score);
    }

    board.en_passant_index = en_passant_index;
    alpha
}
