use cur::*;

/// BitOr is replaced by [`Game::Union`].
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

/// Multiple BitOrs are replaced by a single [`Game::Union`].
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

/// Add is replaced by [`Game::Sequence`].
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

/// Multiple Adds is replaced by a single [`Game::Sequence`].
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
