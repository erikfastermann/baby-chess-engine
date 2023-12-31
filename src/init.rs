use crate::{moves, board};

pub unsafe fn init() {
    moves::init_diagonals();
    moves::init_king_moves();
    moves::init_knight_moves();
    board::init_black_piece_square_tables();
}
