# Baby Chess Engine

A simple chess engine written in Rust.

## Notes

- UCI-compatible
- minimax search with alpha-beta pruning
- the score of a position is evaluated by material and simple piece square tables
- only using Bitsets (Bitboards) to calculate and apply the moves
- not using Magic Bitboards to calculate queen, bishop and rook movements,
  but instead relying only on bit operations including counting leading and trailing zeros
- playing strength is not that great
  -> winrate against the Lichess Stockfish Level 4 bot as white is about 50%
- accidental stalemates occur often
