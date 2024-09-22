use crate::{bitset::Bitset, board::{Board, PlayerBoard}, color::Color, config, moves::{self, Moves}};

pub const SCORE_MIN: i32 = i32::MAX * -1;
pub const SCORE_MAX: i32 = i32::MAX;

pub fn score(board: &Board) -> i32 {
    let is_end_game = is_end_game(board);
    let white = score_player(&board.white, &WHITE_PIECE_SQUARE_TABLES, is_end_game)
        + score_king(&board.white, &board.black, board.color, is_end_game)
        + score_activity(&board.white, &board.black);
    let black = score_player(&board.black, black_piece_square_tables(), is_end_game)
        + score_king(&board.black, &board.white, board.color.other(), is_end_game)
        + score_activity(&board.black, &board.white);
    let score = white - black;
    let perspective = match board.color {
        Color::White => 1,
        Color::Black => -1,
    };
    score * perspective
}

fn is_end_game(board: &Board) -> bool {
    if board.white.queens().is_empty() && board.black.queens().is_empty() {
        return true;
    }

    let white_minor_pieces = board.white.knights().count() + board.white.bishops().count();
    let white_major_pieces = board.white.rooks().count() + board.white.queens().count();

    let black_minor_pieces = board.black.knights().count() + board.black.bishops().count();
    let black_major_pieces = board.black.rooks().count() + board.black.queens().count();

    let is_white_end_game = white_major_pieces == 0
        || (white_major_pieces == 1 && white_minor_pieces <= 2)
        || (white_major_pieces == 2 && white_minor_pieces == 0);
    let is_black_end_game = black_major_pieces == 0
        || (black_major_pieces == 1 && black_minor_pieces <= 2)
        || (black_major_pieces == 2 && black_minor_pieces == 0);
    is_white_end_game && is_black_end_game
}

fn score_player(board: &PlayerBoard, tables: &PieceSquareTables, is_end_game: bool) -> i32 {
    fn position(piece: Bitset, table: &[i32; 64]) -> i32 {
        piece.indices().map(|index| table[index as usize]).sum::<i32>()
    }

    let position = if is_end_game {
        position(board.pawns(), &tables.pawns.end_game)
            + position(board.knights(), &tables.knights.end_game)
            + position(board.bishops(), &tables.bishops.end_game)
            + position(board.rooks(), &tables.rooks.end_game)
            + position(board.queens(), &tables.queens.end_game)
            + position(board.king(), &tables.king.end_game)
    } else {
        position(board.pawns(), &tables.pawns.mid_game)
            + position(board.knights(), &tables.knights.mid_game)
            + position(board.bishops(), &tables.bishops.mid_game)
            + position(board.rooks(), &tables.rooks.mid_game)
            + position(board.queens(), &tables.queens.mid_game)
            + position(board.king(), &tables.king.mid_game)
    };

    let material = board.pawns().count() * config::SCORE_PAWN
        + board.knights().count() * config::SCORE_KNIGHT
        + board.bishops().count() * config::SCORE_BISHOP
        + board.rooks().count() * config::SCORE_ROOK
        + board.queens().count() * config::SCORE_QUEEN
        + board.king().count() * config::SCORE_KING;

    position + material
}

fn score_king(
    we: &PlayerBoard,
    enemy: &PlayerBoard,
    color: Color,
    is_end_game: bool,
) -> i32 {
    let we_pieces = we.bitset();
    let all_pieces = enemy.bitset() | we_pieces;
    let king = we.king().first_index();
    let king_area = moves::king_area(king);
    let mut score = 0;

    for index in enemy.queens().indices() {
        let enemy_queen = Moves::queen(all_pieces, index, we_pieces);
        score += i32::from(enemy_queen.moves_captures().overlaps(king_area))
            * config::SCORE_ATTACK_KING_WITH_QUEEN;
    }
    for index in enemy.rooks().indices() {
        let enemy_rook = Moves::rook(all_pieces, index, we_pieces);
        score += i32::from(enemy_rook.moves_captures().overlaps(king_area))
            * config::SCORE_ATTACK_KING_WITH_ROOK;
    }
    for index in enemy.bishops().indices() {
        let enemy_bishop = Moves::bishop(all_pieces, index, we_pieces);
        score += i32::from(enemy_bishop.moves_captures().overlaps(king_area))
            * config::SCORE_ATTACK_KING_WITH_BISHOP;
    }
    for index in enemy.knights().indices() {
        let enemy_knight = Moves::knight(all_pieces, index, we_pieces);
        score += i32::from(enemy_knight.moves_captures().overlaps(king_area))
            * config::SCORE_ATTACK_KING_WITH_KNIGHT;
    }

    if !is_end_game && color.first_row_bitset().has(king) {
        score += (we.pawns() & king_area).count() * config::SCORE_KING_PAWN_SHIELD;
    }

    score
}

fn score_activity(we: &PlayerBoard, enemy: &PlayerBoard) -> i32 {
    let enemy_pieces = enemy.bitset();
    let all_pieces = we.bitset() | enemy_pieces;
    let mut score = 0;

    for index in we.queens().indices() {
        let queen = Moves::queen(all_pieces, index, enemy_pieces);
        score += queen.moves_captures().count()
            * config::SCORE_ACTIVITY_REACHABLE_FIELDS_QUEEN;
    }
    for index in we.rooks().indices() {
        let rook = Moves::rook(all_pieces, index, enemy_pieces);
        score += rook.moves_captures().count()
            * config::SCORE_ACTIVITY_REACHABLE_FIELDS_ROOK;
    }
    for index in we.bishops().indices() {
        let bishop = Moves::bishop(all_pieces, index, enemy_pieces);
        score += bishop.moves_captures().count()
            * config::SCORE_ACTIVITY_REACHABLE_FIELDS_BISHOP;
    }
    for index in we.knights().indices() {
        let knight = Moves::knight(all_pieces, index, enemy_pieces);
        score += knight.moves_captures().count()
            * config::SCORE_ACTIVITY_REACHABLE_FIELDS_KNIGHT;
    }

    score
}

struct PieceSquareTable {
    mid_game: [i32; 64],
    end_game: [i32; 64],
}

struct PieceSquareTables {
    pawns: PieceSquareTable,
    knights: PieceSquareTable,
    bishops: PieceSquareTable,
    rooks: PieceSquareTable,
    queens: PieceSquareTable,
    king: PieceSquareTable,
}

static WHITE_PIECE_SQUARE_TABLES: PieceSquareTables = PieceSquareTables {
    pawns: PieceSquareTable {
        mid_game: config::SCORE_PAWNS_TABLE,
        end_game: config::SCORE_PAWNS_TABLE,
    },
    knights: PieceSquareTable {
        mid_game: config::SCORE_KNIGHTS_TABLE,
        end_game: config::SCORE_KNIGHTS_TABLE,
    },
    bishops: PieceSquareTable {
        mid_game: config::SCORE_BISHOPS_TABLE,
        end_game: config::SCORE_BISHOPS_TABLE,
    },
    rooks: PieceSquareTable {
        mid_game: config::SCORE_ROOKS_TABLE,
        end_game: config::SCORE_ROOKS_TABLE,
    },
    queens: PieceSquareTable {
        mid_game: config::SCORE_QUEENS_TABLE,
        end_game: config::SCORE_QUEENS_TABLE,
    },
    king: PieceSquareTable {
        mid_game: config::SCORE_KING_TABLE_MIDDLE_GAME,
        end_game: config::SCORE_KING_TABLE_END_GAME,
    },
};

static mut BLACK_PIECE_SQUARE_TABLES: PieceSquareTables = PieceSquareTables {
    pawns: PieceSquareTable {
        mid_game: config::SCORE_PAWNS_TABLE,
        end_game: config::SCORE_PAWNS_TABLE,
    },
    knights: PieceSquareTable {
        mid_game: config::SCORE_KNIGHTS_TABLE,
        end_game: config::SCORE_KNIGHTS_TABLE,
    },
    bishops: PieceSquareTable {
        mid_game: config::SCORE_BISHOPS_TABLE,
        end_game: config::SCORE_BISHOPS_TABLE,
    },
    rooks: PieceSquareTable {
        mid_game: config::SCORE_ROOKS_TABLE,
        end_game: config::SCORE_ROOKS_TABLE,
    },
    queens: PieceSquareTable {
        mid_game: config::SCORE_QUEENS_TABLE,
        end_game: config::SCORE_QUEENS_TABLE,
    },
    king: PieceSquareTable {
        mid_game: config::SCORE_KING_TABLE_MIDDLE_GAME,
        end_game: config::SCORE_KING_TABLE_END_GAME,
    },
};

fn black_piece_square_tables() -> &'static PieceSquareTables {
    unsafe { &BLACK_PIECE_SQUARE_TABLES }
}

pub unsafe fn init_black_piece_square_tables() {
    reverse_piece_square_tables(&mut BLACK_PIECE_SQUARE_TABLES)
}

fn reverse_piece_square_tables(tables: &mut PieceSquareTables) {
    tables.pawns.mid_game.reverse();
    tables.pawns.end_game.reverse();

    tables.knights.mid_game.reverse();
    tables.knights.end_game.reverse();

    tables.bishops.mid_game.reverse();
    tables.bishops.end_game.reverse();

    tables.rooks.mid_game.reverse();
    tables.rooks.end_game.reverse();

    tables.queens.mid_game.reverse();
    tables.queens.end_game.reverse();

    tables.king.mid_game.reverse();
    tables.king.end_game.reverse();
}
