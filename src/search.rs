use std::cmp::max;

use crate::{visit::{Visitor, visit}, side::{Side, WhiteSide, BlackSide}, mov::Move, board::Board, color::Color, moves::{SPECIAL_MOVES_BUFFER_LEN, SIMPLE_MOVES_BUFFER_LEN}, piece::Piece};

struct WhiteSearcher {
    depth: usize,
    alpha: i32,
    beta: i32,
}

impl WhiteSearcher {
    fn new(depth: usize, alpha: i32, beta: i32) -> Self {
        Self { depth, alpha, beta }
    }
}

impl Visitor for WhiteSearcher {
    fn visit<S: Side>(&mut self, side: &mut S, _: Move) -> bool {
        let score = -search_black(
            side.board_mut(),
            self.depth,
            -self.beta,
            -self.alpha,
        );
        if score >= self.beta {
            self.alpha = self.beta;
            return false;
        }
        self.alpha = max(self.alpha, score);
        return true;
    }
}

struct BlackSearcher {
    depth: usize,
    alpha: i32,
    beta: i32,
}

impl BlackSearcher {
    fn new(depth: usize, alpha: i32, beta: i32) -> Self {
        Self { depth, alpha, beta }
    }
}

impl Visitor for BlackSearcher {
    fn visit<S: Side>(&mut self, side: &mut S, _: Move) -> bool {
        let score = -search_white(
            side.board_mut(),
            self.depth,
            -self.beta,
            -self.alpha,
        );
        if score >= self.beta {
            self.alpha = self.beta;
            return false;
        }
        self.alpha = max(self.alpha, score);
        return true;
    }
}

pub fn search_white(board: &mut Board, depth: usize, alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.white.king().is_empty() {
        board.score(Color::White)
    } else {
        let mut side = WhiteSide::new(board);
        let mut searcher = WhiteSearcher::new(depth - 1, alpha, beta);
        visit(&mut side, &mut searcher);
        searcher.alpha
    }
}

pub fn search_black(board: &mut Board, depth: usize, alpha: i32, beta: i32) -> i32 {
    if depth == 0 ||  board.black.king().is_empty() {
        board.score(Color::Black)
    } else {
        let mut side = BlackSide::new(board);
        let mut searcher = BlackSearcher::new(depth - 1, alpha, beta);
        visit(&mut side, &mut searcher);
        searcher.alpha
    }
}

pub fn search(board: &mut Board, color: Color, depth: usize, mut alpha: i32, beta: i32) -> i32 {
    if depth == 0 || board.player_board(color).king().is_empty() {
        return board.score(color);
    }

    // TODO: split function

    // TODO: check found moves count

    // Special Moves

    let mut special_moves_buffer = [Move::Normal { from: 0, to: 0 }; SPECIAL_MOVES_BUFFER_LEN];
    let special_moves = board.fill_special_moves(color, board.en_passant_index, &mut special_moves_buffer);

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
    // TODO: non piece
    let mut we_piece_board = [Piece::King; 64];
    board.player_board(color).fill_piece_board(&mut we_piece_board);
    let mut enemy_piece_board = [Piece::King; 64];
    board.player_board(color.other()).fill_piece_board(&mut enemy_piece_board);

    let en_passant_index = board.en_passant_index;
    board.en_passant_index = None;

    // Captures

    for (from, to) in simple_captures {
        let we_piece = we_piece_board[*from as usize];
        let enemy_piece = enemy_piece_board[*to as usize];

        board.player_board_mut(color).piece_mut(we_piece).mov(*from, *to);
        board.player_board_mut(color.other()).piece_mut(enemy_piece).clear(*to);

        let score = -search(board, color.other(), depth - 1, -beta, -alpha);

        board.player_board_mut(color.other()).piece_mut(enemy_piece).set(*to);
        board.player_board_mut(color).piece_mut(we_piece).mov(*to, *from);

        if score >= beta {
            board.en_passant_index = en_passant_index;
            return beta;
        }
        alpha = max(alpha, score);
    }

    // Moves

    for (from, to) in simple_moves {
        let piece = we_piece_board[*from as usize];
        board.player_board_mut(color).piece_mut(piece).mov(*from, *to);

        let score = -search(board, color.other(), depth - 1, -beta, -alpha);

        board.player_board_mut(color).piece_mut(piece).mov(*to, *from);

        if score >= beta {
            board.en_passant_index = en_passant_index;
            return beta;
        }
        alpha = max(alpha, score);
    }

    board.en_passant_index = en_passant_index;
    alpha
}
