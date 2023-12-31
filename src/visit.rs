use crate::{side::{Side, castle_right_apply, castle_left_apply}, board::{PlayerBoard, CanCastle}, bitset::Bitset, moves::Moves, mov::Move, position::{position_to_index, index_to_position}, piece::Piece};

pub trait Visitor {
    fn visit<S: Side>(&mut self, side: &mut S, mov: Move) -> bool;
}

pub fn visit<S: Side>(side: &mut S, visitor: &mut impl Visitor) {
    side.board().debug_check();

    let old_enemy = side.enemy().clone();
    let en_passant_index = side.board().en_passant_index;
    side.board_mut().en_passant_index = None;

    if !pawns(side, visitor, &old_enemy, en_passant_index) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }
    if !knights(side, visitor, &old_enemy) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }
    if !bishops(side, visitor, &old_enemy) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }
    if !rooks(side, visitor, &old_enemy) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }
    if !queens(side, visitor, &old_enemy) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }
    if !castle(side, visitor) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }
    if !king(side, visitor, &old_enemy) {
        side.board_mut().en_passant_index = en_passant_index;
        return;
    }

    side.board_mut().en_passant_index = en_passant_index;
}

fn apply_moves_with<S: Side>(
    side: &mut S,
    old_enemy: &PlayerBoard,
    get_piece_bitset: impl Fn(&mut PlayerBoard) -> &mut Bitset,
    get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
    mut visitor: impl FnMut(&mut S, u8, u8) -> bool,
) -> bool {
    let enemy_pieces = side.enemy().bitset();
    let all_pieces = enemy_pieces | side.we().bitset();

    let we_old_selected_piece = *get_piece_bitset(side.we_mut());
    for from in get_piece_bitset(side.we_mut()).indices() {
        let Moves { moves, captures } = get_moves(all_pieces, from, enemy_pieces);

        for to in captures.indices() {
            side.enemy_mut().captured(to);
            get_piece_bitset(side.we_mut()).mov(from, to);
            let cont = visitor(side, from, to);
            *get_piece_bitset(side.we_mut()) = we_old_selected_piece;
            *side.enemy_mut() = *old_enemy;
            if !cont {
                return false;
            }
        }

        for to in moves.indices() {
            get_piece_bitset(side.we_mut()).mov(from, to);
            let cont = visitor(side, from, to);
            *get_piece_bitset(side.we_mut()) = we_old_selected_piece;
            if !cont {
                return false;
            }
        }
    }

    true
}

fn apply_moves<S: Side>(
    side: &mut S,
    visitor: &mut impl Visitor,
    old_enemy: &PlayerBoard,
    get_piece_bitset: impl Fn(&mut PlayerBoard) -> &mut Bitset,
    get_moves: impl Fn(Bitset, u8, Bitset) -> Moves,
) -> bool {
    apply_moves_with(
        side,
        old_enemy,
        get_piece_bitset,
        get_moves,
        |side, from, to| visitor.visit(side, Move::Normal { from, to }),
    )
}

fn king<S: Side>(side: &mut S, visitor: &mut impl Visitor, old_enemy: &PlayerBoard) -> bool {
    apply_moves(
        side,
        visitor,
        old_enemy,
        |player_board| &mut player_board.king,
        Moves::king,
    )
}

fn knights<S: Side>(side: &mut S, visitor: &mut impl Visitor, old_enemy: &PlayerBoard) -> bool {
    apply_moves(
        side,
        visitor,
        old_enemy,
        |player_board| &mut player_board.knights,
        Moves::knight,
    )
}

fn bishops<S: Side>(side: &mut S, visitor: &mut impl Visitor, old_enemy: &PlayerBoard) -> bool {
    apply_moves(
        side,
        visitor,
        old_enemy,
        |player_board| &mut player_board.bishops,
        Moves::bishop,
    )
}

fn rooks<S: Side>(side: &mut S, visitor: &mut impl Visitor, old_enemy: &PlayerBoard) -> bool {
    apply_moves(
        side,
        visitor,
        old_enemy,
        |player_board| &mut player_board.rooks,
        Moves::rook,
    )
}

fn queens<S: Side>(side: &mut S, visitor: &mut impl Visitor, old_enemy: &PlayerBoard) -> bool {
    apply_moves(
        side,
        visitor,
        old_enemy,
        |player_board| &mut player_board.queens,
        Moves::queen,
    )
}

fn castle<S: Side>(side: &mut S, visitor: &mut impl Visitor) -> bool {
    if can_castle_right(side) {
        if !castle_right(side, visitor) {
            return false;
        }
    }
    if can_castle_left(side) {
        if !castle_left(side, visitor) {
            return false;
        }
    }
    true
}

fn can_castle_right<S: Side>(side: &mut S) -> bool {
    let all_pieces = side.board().bitset();
    let king_index = side.we().king.first_index();

    let king_starting_index = S::color().king_starting_index();
    let row = S::color().first_row();

    let allowed = king_index == king_starting_index
        && side.we().can_castle_right()
        && side.we().rooks.has(S::color().rook_right_starting_index())
        && !all_pieces.has(position_to_index(5, row))
        && !all_pieces.has(position_to_index(6, row));
    if !allowed {
        return false;
    }

    let we_old_king = side.we().king;
    for shift in 0..=2 {
        side.we_mut().king.mov(king_starting_index, king_starting_index+shift);
        let check = side.enemy().has_check(side.we(), S::color().other());
        side.we_mut().king = we_old_king;
        if check {
            return false;
        }
    }
    true
}

fn can_castle_left<S: Side>(side: &mut S) -> bool {
    let all_pieces = side.we().bitset();
    let king_index = side.we().king.first_index();

    let king_starting_index = S::color().king_starting_index();
    let row = S::color().first_row();

    let allowed = king_index == king_starting_index
        && side.we().can_castle_left()
        && side.we().rooks.has(S::color().rook_left_starting_index())
        && !all_pieces.has(position_to_index(3, row))
        && !all_pieces.has(position_to_index(2, row))
        && !all_pieces.has(position_to_index(1, row));
    if !allowed {
        return false;
    }

    let we_old_king = side.we().king;
    for shift in 0..=2 {
        side.we_mut().king.mov(king_starting_index, king_starting_index-shift);
        let check = side.enemy().has_check(side.we(), S::color().other());
        side.we_mut().king = we_old_king;
        if check {
            return false;
        }
    }
    true
}

fn castle_reset(
    side: &mut impl Side,
    old_can_castle: CanCastle,
    we_old_king: Bitset,
    we_old_rooks: Bitset,
) {
    side.we_mut().reset_castle(old_can_castle);
    side.we_mut().king = we_old_king;
    side.we_mut().rooks = we_old_rooks;
}

fn castle_right<S: Side>(side: &mut S, visitor: &mut impl Visitor) -> bool {
    let old_can_castle = side.we().can_castle;
    let we_old_king = side.we().king;
    let we_old_rooks = side.we().rooks;
    let mov = castle_right_apply(side);
    let cont = visitor.visit(side, mov);
    castle_reset(side, old_can_castle, we_old_king, we_old_rooks);
    cont
}

fn castle_left<S: Side>(side: &mut S, visitor: &mut impl Visitor) -> bool {
    let old_can_castle = side.we_mut().can_castle;
    let we_old_king = side.we_mut().king;
    let we_old_rooks = side.we_mut().rooks;
    let mov = castle_left_apply(side);
    let cont = visitor.visit(side, mov);
    castle_reset(side, old_can_castle, we_old_king, we_old_rooks);
    cont
}

fn pawns<S: Side>(
    side: &mut S,
    visitor: &mut impl Visitor,
    old_enemy: &PlayerBoard,
    en_passant_index: Option<u8>,
) -> bool {
    let cont = apply_moves(
        side,
        visitor,
        old_enemy,
        |player_board| &mut player_board.pawns,
        S::pawn_normal_single_without_en_passant,
    );
    if !cont {
        return false;
    }

    let cont = apply_moves_with(
        side,
        old_enemy,
        |player_board| &mut player_board.pawns,
        S::pawn_double,
        |side, from, to| {
            side.board_mut().en_passant_index = Some(to);
            let cont = visitor.visit(side, Move::Normal { from, to });
            side.board_mut().en_passant_index = None;
            cont
        },
    );
    if !cont {
        return false;
    }

    let cont = apply_moves_with(
        side,
        old_enemy,
        |player_board| &mut player_board.pawns,
        S::pawn_promotion,
        |side, from, to| {
            side.we_mut().pawns.clear(to);
            pawn_promote_figures(side, visitor, from, to)
        },
    );
    if !cont {
        return false;
    }

    if let Some(en_passant_index) = en_passant_index {
        en_passant(side, visitor, old_enemy, en_passant_index)
    } else {
        true
    }
}

fn pawn_promote_figures<S: Side>(side: &mut S, visitor: &mut impl Visitor, from: u8, to: u8) -> bool {
    side.we_mut().queens.set(to);
    let cont = visitor.visit(side, Move::Promotion { piece: Piece::Queen, from, to });
    side.we_mut().queens.clear(to);
    if !cont {
        return false;
    }

    side.we_mut().rooks.set(to);
    let cont = visitor.visit(side, Move::Promotion { piece: Piece::Rook, from, to });
    side.we_mut().rooks.clear(to);
    if !cont {
        return false;
    }

    side.we_mut().knights.set(to);
    let cont = visitor.visit(side, Move::Promotion { piece: Piece::Knight, from, to });
    side.we_mut().knights.clear(to);
    if !cont {
        return false;
    }

    side.we_mut().bishops.set(to);
    let cont = visitor.visit(side, Move::Promotion { piece: Piece::Bishop, from, to });
    side.we_mut().bishops.clear(to);
    cont
}

fn en_passant_single<S: Side>(
    side: &mut S,
    visitor: &mut impl Visitor,
    old_enemy: &PlayerBoard,
    captured: u8,
    from: u8,
    to: u8,
) -> bool {
    side.enemy_mut().pawns.clear(captured);
    side.we_mut().pawns.mov(from, to);
    let cont = visitor.visit(side, Move::Normal { from, to });
    side.we_mut().pawns.mov(to, from);
    *side.enemy_mut() = *old_enemy;
    cont
}

fn en_passant<S: Side>(
    side: &mut S,
    visitor: &mut impl Visitor,
    old_enemy: &PlayerBoard,
    en_passant_index: u8,
) -> bool {
    let (x, y) = index_to_position(en_passant_index);
    let next_y = ((y as i8) + S::color().direction()) as u8;

    if side.we().has_en_passant_left(side.enemy(), en_passant_index) {
        let cont = en_passant_single(
            side,
            visitor,
            old_enemy,
            en_passant_index,
            position_to_index(x-1, y),
            position_to_index(x, next_y),
        );
        if !cont {
            return false;
        }
    }

    if side.we().has_en_passant_right(side.enemy(), en_passant_index) {
        let cont = en_passant_single(
            side,
            visitor,
            old_enemy,
            en_passant_index,
            position_to_index(x+1, y),
            position_to_index(x, next_y),
        );
        if !cont {
            return false;
        }
    }

    true
}
