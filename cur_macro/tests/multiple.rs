use cur::*;

// Processing::game_pattern_syntax[try]
#[test]
fn try_expr() {
    game!(TRY = 'a'?);

    assert_eq!(
        *TRY,
        Game::Union(vec![
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('a'))
        ])
    );
}

// Processing::game_pattern_syntax[single_sequence]
#[test]
fn single_sequence() {
    game!(SINGLE_SEQUENCE = ('a'));

    assert_eq!(*SINGLE_SEQUENCE, Game::Single(Scent::Char('a')),);
}

// Processing::game_pattern_syntax[sequence]
#[test]
fn sequence() {
    game!(SEQUENCE = ('a', 'b'));

    assert_eq!(
        *SEQUENCE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b'))
        ])
    );
}

// Processing::game_pattern_syntax[multiple_sequence]
#[test]
fn multiple_sequence() {
    game!(MULTIPLE_SEQUENCE = ('a', 'b', 'c'));

    assert_eq!(
        *MULTIPLE_SEQUENCE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b')),
            Step::Single(Scent::Char('c'))
        ])
    );
}

// Processing::game_pattern_syntax[try]
#[test]
fn try_sequence() {
    game!(TRY_SEQUENCE = ('a', 'b')?);

    assert_eq!(
        *TRY_SEQUENCE,
        Game::Union(vec![
            Branch::Sequence(vec![]),
            Branch::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b'))
            ]),
        ])
    );
}

// Processing::game_pattern_syntax[sequence]
#[test]
fn sequence_with_try() {
    game!(SEQUENCE_WITH_TRY = ('a', 'b'?));

    assert_eq!(
        *SEQUENCE_WITH_TRY,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('b')),
            ]),
        ]),
    );
}

// Processing::game_pattern_syntax[union]
#[test]
fn union() {
    game!(UNION = 'a' | 'b');

    assert_eq!(
        *UNION,
        Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Char('b'))
        ])
    );
}

// Processing::game_pattern_syntax[multiple_union]
#[test]
fn multiple_union() {
    game!(MULTIPLE_UNION = 'a' | 'b' | 'c');

    assert_eq!(
        *MULTIPLE_UNION,
        Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Char('b')),
            Branch::Single(Scent::Char('c'))
        ])
    );
}

// Processing::game_pattern_syntax[try]
#[test]
fn try_union() {
    game!(TRY_UNION = ('a' | 'b')?);

    assert_eq!(
        *TRY_UNION,
        Game::Union(vec![
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Char('b')),
        ]),
    );
}

// Processing::game_pattern_syntax[union]
#[test]
fn union_with_try() {
    game!(UNION_WITH_TRY = 'a' | 'b'?);

    assert_eq!(
        *UNION_WITH_TRY,
        Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('b')),
        ]),
    );
}

// Processing::game_pattern_syntax[union]
#[test]
fn sequence_before_union() {
    game!(UNION = ('a', 'b') | 'c');

    assert_eq!(
        *UNION,
        Game::Union(vec![
            Branch::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b')),
            ]),
            Branch::Single(Scent::Char('c')),
        ]),
    );
}

// Processing::game_pattern_syntax[sequence]
#[test]
fn union_before_sequence() {
    game!(SEQUENCE = ('a', 'b' | 'c'));

    assert_eq!(
        *SEQUENCE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Single(Scent::Char('b')),
                Branch::Single(Scent::Char('c')),
            ]),
        ]),
    );
}

// Processing::game_pattern_syntax[name]
#[test]
fn name() {
    game!(CHAR = name @ 'a');

    assert_eq!(
        *CHAR,
        Game::Item("name", Box::new(Game::Single(Scent::Char('a')))),
    );
}

// Processing::game_pattern_syntax[name]
#[test]
fn named_try() {
    game!(NAMED_TRY = name @ 'a'?);

    assert_eq!(
        *NAMED_TRY,
        Game::Item(
            "name",
            Box::new(Game::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]))
        ),
    );
}

// Processing::game_pattern_syntax[name]
#[test]
fn named_sequence() {
    game!(NAMED_SEQUENCE = name @ ('a', 'b'));

    assert_eq!(
        *NAMED_SEQUENCE,
        Game::Item(
            "name",
            Box::new(Game::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b')),
            ]))
        ),
    );
}

// Processing::game_pattern_syntax[name]
#[test]
fn named_union() {
    game!(NAMED_UNION = name @ 'a' | 'b');

    assert_eq!(
        *NAMED_UNION,
        Game::Item(
            "name",
            Box::new(Game::Union(vec![
                Branch::Single(Scent::Char('a')),
                Branch::Single(Scent::Char('b')),
            ]))
        ),
    );
}
