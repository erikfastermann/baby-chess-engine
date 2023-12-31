use std::io::{self, BufRead};

use crate::{game::Game, mov::Move, result::Result};

pub struct UCI {
    game: Game,
}

impl UCI {
    pub fn new() -> Self {
        Self { game: Game::new() }
    }

    pub fn read_eval_print_loop(&mut self) -> Result<()> {
        for command in io::stdin().lock().lines() {
            let command = command?;
            for response in self.read_eval_print(&command)? {
                println!("{response}");
            }
        }
        Err("unexpected end of input loop".into())
    }

    fn read_eval_print(&mut self, command: &str) -> Result<Vec<String>> {
        // TODO: complete UCI implementation

        let split = command.split(' ').collect::<Vec<_>>();
        let responses = match split.as_slice() {
            ["uci"] => vec![
                "id name Baby Chess Engine".to_string(),
                "uciok".to_string(),
            ],
            ["isready"] => vec!["readyok".to_string()],
            ["ucinewgame"] | ["position", "startpos"] => {
                self.game.reset();
                Vec::new()
            },
            ["position", "startpos", "moves", raw_moves @ ..] => {
                // TODO: fen
                self.apply_moves(raw_moves)?;
                Vec::new()
            },
            ["go", ..] => {
                let mov = self.game.best_move()?;
                vec![format!("bestmove {}", mov.to_long_algebraic_notation())]
            }
            ["stop"] => vec![],
            ["quit"] => return Err("quit".into()),
            _ => return Err(format!("unknown command '{}'", command).into())
        };
        Ok(responses)
    }

    fn apply_moves(&mut self, raw_moves: &[&str]) -> Result<()> {
        self.game.reset();
        for raw_move in raw_moves {
            let mov = Move::from_long_algebraic_notation(raw_move)?;
            self.game.apply_move(mov)?;
        }
        Ok(())
    }
}
