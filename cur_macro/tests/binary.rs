use cur::*;

/// `A | B` is replaced by a `Game::Union`.
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

/// `A | B | C` is replaced by a single `Game::Union`.
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

/// `A + B` is replaced by a `Game::Sequence`.
#[test]
fn sequence() {
    game!(SEQUENCE = 'a' + 'b');

    assert_eq!(
        *SEQUENCE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b'))
        ])
    );
}

/// `A + B + C` is replaced by a single `Game::Sequence`.
#[test]
fn multiple_sequence() {
    game!(MULTIPLE_SEQUENCE = 'a' + 'b' + 'c');

    assert_eq!(
        *MULTIPLE_SEQUENCE,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b')),
            Step::Single(Scent::Char('c'))
        ])
    );
}

/// `A + B | C is replaced by a `Game::Union`.
#[test]
fn union_over_sequence() {
    game!(UNION = 'a' + 'b' | 'c');

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

/// `A + (B | C)` is replaced by a `Game::Sequence`.
#[test]
fn sequence_over_union() {
    game!(SEQUENCE = 'a' + ('b' | 'c'));

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
