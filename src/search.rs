use std::cmp::max;

use crate::{board::Board, color::Color, moves::FullMovesBuffer, piece::Piece};

pub fn search(board: &mut Board, color: Color, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.player_board(color).king().is_empty() {
        return board.score(color);
    }
    let old_board = board.clone();

    let mut moves_buffer = FullMovesBuffer::new();
    let moves = moves_buffer.fill(board, color);

    // TODO: check found moves count

    // Special Moves

    for mov in moves.special {
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

    // TODO: sort moves

    let en_passant_index = board.en_passant_index;
    board.en_passant_index = None;

    for (from, to) in moves.simple {
        board.player_board_mut(color).move_piece(*from, *to);
        let captured_piece = board.player_board(color.other()).which_piece(*to);
        board.player_board_mut(color.other()).set_piece_none(*to);

        let score = -search(board, color.other(), depth - 1, -beta, -alpha);

        if captured_piece != Piece::None {
            board.player_board_mut(color.other()).place_piece(*to, captured_piece);
        }
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
