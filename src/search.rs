use std::cmp::max;

use crate::{board::Board, moves::FullMovesBuffer, piece::Piece};

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
        board.we_mut().move_piece(*from, *to);
        let captured_piece = board.enemy().which_piece(*to);
        board.enemy_mut().set_piece_none(*to);

        board.color = board.color.other();
        let score = -search(board, depth - 1, -beta, -alpha);
        board.color = board.color.other();

        if captured_piece != Piece::None {
            board.enemy_mut().place_piece(*to, captured_piece);
        }
        board.we_mut().move_piece(*to, *from);

        if score >= beta {
            board.en_passant_index = en_passant_index;
            return beta;
        }
        alpha = max(alpha, score);
    }

    board.en_passant_index = en_passant_index;
    alpha
}
