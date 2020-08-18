use cur::*;

/// `None` is replaced by an empty `Game::Sequence`.
#[test]
fn none() {
    game!(EMPTY = None);

    assert_eq!(*EMPTY, Game::Sequence(vec![]));
}

/// A single `char` is replaced by a matching `Game::Single`.
#[test]
fn char() {
    game!(CHAR = 'a');

    assert_eq!(*CHAR, Game::Single(Scent::Char('a')));
}

/// A `&str` of a single char is replaced by a matching `Game::Single`.
#[test]
fn single_char_str() {
    game!(SINGLE_CHAR = "a");

    assert_eq!(*SINGLE_CHAR, Game::Single(Scent::Char('a')));
}

/// A `&str` is replaced by a `Game::Sequence`.
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

/// A parenthesis around a single `Game` is replaced by the `Game`.
#[test]
fn parenthesis() {
    game!(PARENTHESIS = ('a'));

    assert_eq!(*PARENTHESIS, Game::Single(Scent::Char('a')));
}

/// A `RangeFull` is replaced by a `Game::Single` matching any `char`.
#[test]
fn range_full() {
    game!(ANY = ..);

    assert_eq!(*ANY, Game::Single(Scent::Range('\u{0}', '\u{10ffff}')));
}

/// A `RangeFrom` is replaced by a `Game::Single` matching any `char` at or after the start.
#[test]
fn range_from() {
    game!(FROM = 'A'..);

    assert_eq!(*FROM, Game::Single(Scent::Range('A', '\u{10ffff}')));
}

/// A `RangeTo` is replaced by a `Game::Single` matching any `char` before the end.
#[test]
fn range_to() {
    game!(TO = ..'Z');

    assert_eq!(*TO, Game::Single(Scent::Range('\u{0}', 'Y')));
}

/// A `RangeToInclusive` is replaced by a `Game::Single` matching any `char` before or including the end.
#[test]
fn range_to_inclusive() {
    game!(TO_INCLUSIVE = ..='Z');

    assert_eq!(*TO_INCLUSIVE, Game::Single(Scent::Range('\u{0}', 'Z')));
}
/// A `Range` is replaced by a `Game::Single` matching any `char` at or after the start and before the end.
#[test]
fn range() {
    game!(RANGE = 'a'..'z');

    assert_eq!(*RANGE, Game::Single(Scent::Range('a', 'y')));
}

/// A `RangeInclusive` is replaced by a `Game::Single` matching any `char` inclusively between the start and end.
#[test]
fn range_inclusvie() {
    game!(RANGE_INCLUSIVE = 'a'..='z');

    assert_eq!(*RANGE_INCLUSIVE, Game::Single(Scent::Range('a', 'z')));
}

/// A type expression is replaced by a `Game::Item`.
#[test]
fn type() {
    game!(TYPE = 'a': id);

    assert_eq!(*TYPE, Game::Item("id", &Game::Single(Scent::Char('a'))));
}
