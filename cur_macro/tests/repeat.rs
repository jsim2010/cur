use cur::*;

// Processing::game_pattern_syntax[repeat[exactly_zero]]
#[test]
fn exactly_zero() {
    game!(ZERO_REPEAT = ['a'; 0]);

    assert_eq!(*ZERO_REPEAT, Game::Sequence(vec![]));
}

// Processing::game_pattern_syntax[repeat[exactly_one]]
#[test]
fn exactly_one() {
    game!(ONE_REPEAT = ['a'; 1]);

    assert_eq!(*ONE_REPEAT, Game::Single(Scent::Char('a')));
}

// Processing::game_pattern_syntax[repeat[exact]]
#[test]
fn exact() {
    game!(THREE_REPEATS = ['a'; 3]);

    assert_eq!(
        *THREE_REPEATS,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a'))
        ])
    );
}

// Processing::game_pattern_syntax[repeat[any]]
#[test]
fn any() {
    game!(ANY = ['a'; ..]);

    assert_eq!(*ANY, Game::Repetition(Pattern::Single(Scent::Char('a'))));
}

// Processing::game_pattern_syntax[repeat[at_least]]
#[test]
fn start_or_more() {
    game!(THREE_OR_MORE = ['a'; 3..]);

    assert_eq!(
        *THREE_OR_MORE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Repetition(Pattern::Single(Scent::Char('a')))
        ])
    );
}

// Processing::game_pattern_syntax[repeat[less_than]]
#[test]
fn less_than_end() {
    game!(LESS_THAN_FOUR = ['a'; ..4]);

    assert_eq!(
        *LESS_THAN_FOUR,
        Game::Sequence(vec![
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
        ])
    );
}

// Processing::game_pattern_syntax[repeat[no_more]]
#[test]
fn end_or_less() {
    game!(THREE_OR_LESS = ['a'; ..=3]);

    assert_eq!(
        *THREE_OR_LESS,
        Game::Sequence(vec![
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
        ])
    );
}

// Processing::game_pattern_syntax[repeat[exclusive]]
#[test]
fn start_to_end() {
    game!(TWO_TO_FIVE = ['a'; 2..5]);

    assert_eq!(
        *TWO_TO_FIVE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
        ])
    );
}

// Processing::game_pattern_syntax[repeat[inclusive]]
#[test]
fn start_to_end_inclusive() {
    game!(TWO_TO_FOUR_INCLUSIVE = ['a'; 2..=4]);

    assert_eq!(
        *TWO_TO_FOUR_INCLUSIVE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
        ])
    );
}

// Processing::game_pattern_syntax[repeat[exclusive]]
#[test]
fn repeat_union() {
    game!(REPEAT_UNION = ['a' | 'b'; 1..3]);

    assert_eq!(
        *REPEAT_UNION,
        Game::Sequence(vec![
            Step::Union(vec![
                Branch::Single(Scent::Char('a')),
                Branch::Single(Scent::Char('b'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a')),
                Branch::Single(Scent::Char('b'))
            ])
        ])
    );
}

// Processing::game_pattern_syntax[repeat[at_least]]
#[test]
fn repeat_sequence() {
    game!(REPEAT_SEQUENCE = [('a', 'b'); 1..]);

    assert_eq!(
        *REPEAT_SEQUENCE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b')),
            Step::Repetition(Pattern::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b'))
            ]),)
        ])
    );
}
