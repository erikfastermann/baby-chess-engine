use crate::result::Result;

mod bitset;
mod board;
mod color;
mod config;
mod fmt;
mod game;
mod init;
mod mov;
mod moves;
mod piece;
mod position;
mod result;
mod search;
mod side;
mod uci;
mod visit;

fn main() -> Result<()> {
    unsafe { init::init() };
    uci::UCI::new().read_eval_print_loop()
}
