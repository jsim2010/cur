use cur::Game;
use cur_macro::game;

/// [`None`] is replaced by an empty [`Game::Sequence`].
#[test]
fn none() {
    #[game]
    const EMPTY: Game = None;

    assert_eq!(EMPTY, Game::Sequence(&[]));
}

/// [`char`] is replaced by [`Game::Char`].
#[test]
fn char() {
    #[game]
    const CHAR: Game = 'a';

    assert_eq!(CHAR, Game::Char('a'));
}

/// [`&str`] with a single char is replaced by [`Game::Char`].
#[test]
fn single_char_str() {
    #[game]
    const SINGLE_CHAR: Game = "a";

    assert_eq!(SINGLE_CHAR, Game::Char('a'));
}

/// [`&str`] is replaced by [`Game::Sequence`] of [`Game::Char`]s.
#[test]
fn string() {
    #[game]
    const STR: Game = "abc";

    assert_eq!(
        STR,
        Game::Sequence(&[Game::Char('a'), Game::Char('b'), Game::Char('c')])
    );
}

/// Parenthesis is replaced by [`Game`] of the expression inside.
#[test]
fn parenthesis() {
    #[game]
    const PARENTHESIS: Game = ('a');

    assert_eq!(PARENTHESIS, Game::Char('a'));
}

/// Range of [`chars`].
#[test]
fn range() {
    #[game]
    const RANGE: Game = 'a'..'z';

    assert_eq!(RANGE, Game::Range('a', 'z'));
}

/// Type ascription is replaced by [`Game::Item`].
#[test]
fn type_ascription() {
    #[game]
    const ARTICLE: Game = 'a': id;

    assert_eq!(ARTICLE, Game::Item("id", &Game::Char('a')));
}
