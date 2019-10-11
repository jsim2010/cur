use cur::Game;
use cur_macro::game;

/// BitOr is replaced by [`Game::Union`].
#[test]
fn union() {
    #[game]
    const UNION: Game = 'a' | 'b';

    assert_eq!(UNION, Game::Union(&[Game::Char('a'), Game::Char('b')]));
}

/// Multiple BitOrs are replaced by a single [`Game::Union`].
#[test]
fn multiple_union() {
    #[game]
    const MULTIPLE_UNION: Game = 'a' | 'b' | 'c';

    assert_eq!(
        MULTIPLE_UNION,
        Game::Union(&[Game::Char('a'), Game::Char('b'), Game::Char('c')])
    );
}

/// Add is replaced by [`Game::Sequence`].
#[test]
fn sequence() {
    #[game]
    const SEQUENCE: Game = 'a' + 'b';

    assert_eq!(
        SEQUENCE,
        Game::Sequence(&[Game::Char('a'), Game::Char('b')])
    );
}

/// Multiple Adds is replaced by a single [`Game::Sequence`].
#[test]
fn multiple_sequence() {
    #[game]
    const MULTIPLE_SEQUENCE: Game = 'a' + 'b' + 'c';

    assert_eq!(
        MULTIPLE_SEQUENCE,
        Game::Sequence(&[Game::Char('a'), Game::Char('b'), Game::Char('c')])
    );
}
