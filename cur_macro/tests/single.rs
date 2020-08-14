use cur::prelude::*;

/// [`None`] is replaced by an empty [`Game::Sequence`].
#[test]
fn none() {
    game!(EMPTY = None);

    assert_eq!(*EMPTY, Game::Sequence(vec![]));
}

/// [`char`] is replaced by [`Game::Single`].
#[test]
fn char() {
    game!(CHAR = 'a');

    assert_eq!(*CHAR, Game::Single(Scent::Char('a')));
}

/// [`&str`] with a single char is replaced by [`Game::Single`].
#[test]
fn single_char_str() {
    game!(SINGLE_CHAR = "a");

    assert_eq!(*SINGLE_CHAR, Game::Single(Scent::Char('a')));
}

/// [`&str`] is replaced by [`Game::Sequence`] of [`Game::Single`]s.
#[test]
fn string() {
    game!(STR = "abc");

    assert_eq!(
        *STR,
        Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b')),
            Step::Single(Scent::Char('c'))
        ])
    );
}

/// Parenthesis is replaced by [`Game`] of the expression inside.
#[test]
fn parenthesis() {
    game!(PARENTHESIS = ('a'));

    assert_eq!(*PARENTHESIS, Game::Single(Scent::Char('a')));
}

/// Range of [`chars`].
#[test]
fn range() {
    game!(RANGE = 'a'..'z');

    assert_eq!(*RANGE, Game::Single(Scent::Range('a', 'z')));
}

/// Type ascription is replaced by [`Game::Item`].
#[test]
fn type_ascription() {
    game!(TYPE = 'a': id);

    assert_eq!(*TYPE, Game::Item("id", &Game::Single(Scent::Char('a'))));
}
