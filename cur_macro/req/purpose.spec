spec game_macro = This module SHALL implement a procedural macro identified as "game".

art Processing {
  spec const_def = If game receives input tokens that match the format "Visibility IDENTIFIER = GamePattern", it SHALL return tokens that define a const item with the given visibility and identifier as a reference to the Game derived from GamePattern.
  spec game_pattern_syntax = A Game SHALL be derived from a GamePattern via one of the following derivations [
    try: GamePattern? => Game::Union(vec![Branch::Sequence(vec![]), GAME.into_branches()])
    single_sequence: (GamePattern) => GAME
    sequence: (GamePattern_1, GamePattern_2) => Game::Sequence(vec![GAME_1.into_steps(), GAME_2.into_steps()])
    multiple_sequence: (GamePattern_1, GamePattern_2, GamePattern_3) => Game::Sequence(vec![GAME_1.into_steps(), GAME_2.into_steps(), GAME_3.into_steps()])
    union: GamePattern_1 | GamePattern_2 => Game::Union(vec![GAME_1.into_branches(), GAME_2.into_branches()])
    multiple_union: GamePattern_1 | GamePattern_2 | GamePattern_3 => Game::Union(vec![GAME_1.into_branches(), GAME_2.into_branches(), GAME_3.into_branches()])
    name: IDENTIFIER @ GamePattern => Game::Item("IDENTIFIER", Box::new(GAME))
    repeat: [GamePattern; Quantifier] => [
      exactly_zero: 0 => Game::Sequence(vec![])
      exactly_one: 1 => GAME,
      exact: INT_LITERAL => Game::Sequence(vec![GAME.into_steps(), ..])
      any: .. => Game::Repetition(GAME.into_pattern())
      at_least: INT_LITERAL.. => Game::Sequence(vec![GAME.into_steps(), .., GAME.into_pattern().into_steps()])
      less_than: ..INT_LITERAL => Game::Sequence(vec![Step::Union(vec![Branch::Sequence(vec![]), GAME.into_branches()]), ..])
      less_than: ..=INT_LITERAL => Game::Sequence(vec![Step::Union(vec![Branch::Sequence(vec![]), GAME.into_branches()]), ..])
      exclusive: INT_LITERAL..INT_LITERAL => Game::Sequence(vec![GAME.into_steps(), .., Step::Union(vec![Branch::Sequence(vec![]), GAME.into_branches()]), ..])
      inclusive: INT_LITERAL..=INT_LITERAL => Game::Sequence(vec![GAME.into_steps(), .., Step::Union(vec![Branch::Sequence(vec![]), GAME.into_branches()]), ..])
    ]
    path: SimplePath => SimplePath.clone()
  ]
}
