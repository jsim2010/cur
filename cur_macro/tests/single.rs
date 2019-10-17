use cur::{Game, Scent};
use cur_macro::game;

/// [`None`] is replaced by an empty [`Game::Sequence`].
#[test]
fn none() {
    #[game]
    const EMPTY: Game = None;

    assert_eq!(EMPTY, Game::Sequence(&[]));
}

/// [`char`] is replaced by [`Game::Single`].
#[test]
fn char() {
    #[game]
    const CHAR: Game = 'a';

    assert_eq!(CHAR, Game::Single(Scent::Char('a')));
}

/// [`&str`] with a single char is replaced by [`Game::Single`].
#[test]
fn single_char_str() {
    #[game]
    const SINGLE_CHAR: Game = "a";

    assert_eq!(SINGLE_CHAR, Game::Single(Scent::Char('a')));
}

/// [`&str`] is replaced by [`Game::Sequence`] of [`Game::Single`]s.
#[test]
fn string() {
    #[game]
    const STR: Game = "abc";

    assert_eq!(
        STR,
        Game::Sequence(&[Game::Single(Scent::Char('a')), Game::Single(Scent::Char('b')), Game::Single(Scent::Char('c'))])
    );
}

/// Parenthesis is replaced by [`Game`] of the expression inside.
#[test]
fn parenthesis() {
    #[game]
    const PARENTHESIS: Game = ('a');

    assert_eq!(PARENTHESIS, Game::Single(Scent::Char('a')));
}

/// Range of [`chars`].
#[test]
fn range() {
    #[game]
    const RANGE: Game = 'a'..'z';

    assert_eq!(RANGE, Game::Single(Scent::Range('a', 'z')));
}

/// Type ascription is replaced by [`Game::Item`].
#[test]
fn type_ascription() {
    #[game]
    const ARTICLE: Game = 'a': id;

    assert_eq!(ARTICLE, Game::Item("id", &Game::Single(Scent::Char('a'))));
}
