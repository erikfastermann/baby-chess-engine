use crate::result::Result;

mod bitset;
mod board;
mod color;
mod config;
mod eval;
mod fmt;
mod game;
mod init;
mod mov;
mod moves;
mod piece;
mod position;
mod result;
mod search;
mod uci;

fn main() -> Result<()> {
    unsafe { init::init() };
    uci::UCI::new().read_eval_print_loop()
}
