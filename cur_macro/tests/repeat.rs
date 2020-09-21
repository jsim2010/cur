use cur::*;

/// `A?` is replaced by a `Game::Union` of nothing and `A`.
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

/// `(A | B)? is replaced by a single `Game::Union`.
#[test]
fn try_union() {
    game!(TRY_UNION = ('a' | 'b')?);

    assert_eq!(
        *TRY_UNION,
        Game::Union(vec![
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Char('b'))
        ])
    );
}

/// `A | B?` is replaced by a single `Game::Union`.
#[test]
fn union_with_try() {
    game!(UNION_WITH_TRY = 'a' | 'b'?);

    assert_eq!(
        *UNION_WITH_TRY,
        Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('b'))
        ])
    );
}

/// `(A + B)?` is replaced by a `Game::Union`.
#[test]
fn try_sequence() {
    game!(TRY_SEQUENCE = ('a' + 'b')?);

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

/// `A + B?` is replaced by a `Game::Sequence`.
#[test]
fn sequence_with_try() {
    game!(SEQUENCE_WITH_TRY = 'a' + 'b'?);

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

/// Index with usize.
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

#[test]
fn exactly_one() {
    game!(ONE_REPEAT = ['a'; 1]);

    assert_eq!(*ONE_REPEAT, Game::Single(Scent::Char('a')));
}

#[test]
fn exactly_zero() {
    game!(ZERO_REPEAT = ['a'; 0]);

    assert_eq!(*ZERO_REPEAT, Game::Sequence(vec![]));
}

/// Index with RangeFull.
#[test]
fn zero_or_more() {
    game!(ZERO_OR_MORE = ['a'; ..]);

    assert_eq!(
        *ZERO_OR_MORE,
        Game::Repetition(Pattern::Single(Scent::Char('a')))
    );
}

/// Index with RangeFrom.
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

/// Index with RangeTo.
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

/// Index with RangeToInclusive.
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

/// Index with Range.
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

/// Index with RangeToInclusive.
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

/// BitOr with Index.
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

/// Add with Index.
#[test]
fn repeat_sequence() {
    game!(REPEAT_SEQUENCE = [['a', 'b']; 1..]);

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
