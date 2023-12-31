use std::cmp::max;

use crate::{visit::{Visitor, visit}, side::{Side, WhiteSide, BlackSide}, mov::Move, board::Board, color::Color};

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
    if depth == 0 || board.white.king.is_empty() {
        board.score(Color::White)
    } else {
        let mut side = WhiteSide::new(board);
        let mut searcher = WhiteSearcher::new(depth - 1, alpha, beta);
        visit(&mut side, &mut searcher);
        searcher.alpha
    }
}

pub fn search_black(board: &mut Board, depth: usize, alpha: i32, beta: i32) -> i32 {
    if depth == 0 ||  board.black.king.is_empty() {
        board.score(Color::Black)
    } else {
        let mut side = BlackSide::new(board);
        let mut searcher = BlackSearcher::new(depth - 1, alpha, beta);
        visit(&mut side, &mut searcher);
        searcher.alpha
    }
}
