spec game_macro = This module SHALL implement a procedural macro identified as "game".

art Processing {
  spec const_def = If game receives input tokens that match the format "Visibility IDENTIFIER = GamePattern", it SHALL return tokens that define a const item `IDENTIFIER` with the given visibility that is a reference to the Game derived from GamePattern.
  spec game_pattern_syntax = A Game SHALL be derived from a GamePattern via one of the following derivations [
    empty: [] => Game::Sequence(vec![])
    char: CHAR_LITERAL => Game::Single(Scent::Char(CHAR_LITERAL))
    empty_string: "" => Game::Sequence(vec![])
    single_char_string: "1" => Game::Sequence(vec![Step::Single(Scent::Char(1))])
    multiple_char_string: "123" => Game::Sequence(vec![Step::Single(Scent::Char(1)), Step::Single(Scent::Char(2)), Step::Single(Scent::Char(3))])
    byte: BYTE_LITERAL => Game::Single(Scent::Char(char::from(BYTE_LITERAL)))
    byte_string: b"123" => Game::Sequence(vec![Step::Single(Scent::Char(1)), Step::Single(Scent::Char(2)), Step::Single(Scent::Char(3))])
    group: (GamePattern) => DerivedGame
    wildcard: WildcardPattern => Game::Single(Scent::Range('\u{0}', '\u{10ffff}'))
    # TODO: Add remaining possible derivations.
    raw_string: RAW_STRING_LITERAL
    raw_byte_string: RAW_BYTE_STRING_LITERAL
    range: CHAR_LITERAL..=CHAR_LITERAL
    sequence: [(GamePattern (, GamePattern)*)? ,?]
    repeat: [GamePattern; Quantifier]
    try: GamePattern?
    path: SimplePath
  ]
}
