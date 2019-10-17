use cur::{Cur, Game, Scent};

macro_rules! assert_passes {
    ($cur:ident; $( $x:expr ),+) => {
        $( assert!($cur.is_match($x), "{:?} expected to match {}", $cur, $x); )+
    }
}

macro_rules! assert_fails {
    ($cur:ident; $( $x:expr ),+) => {
        $( assert!(!$cur.is_match($x), "{:?} expected to not match {}", $cur, $x); )+
    }
}

/// [`Scent::Char`] shall match a single matching [`char`].
#[test]
fn char() {
    let cur = Cur::new(Game::Single(Scent::Char('a')));

    assert_passes!(cur; "a");
    assert_fails!(cur; "", "b", "ab");
}

/// [`Scent::Range`] shall match a single [`char`] that is inclusively within the given range.
#[test]
fn range() {
    let cur = Cur::new(Game::Single(Scent::Range('b', 'd')));

    assert_passes!(cur; "b", "c", "d");
    assert_fails!(cur; "", "a", "e");
}

/// [`Game::Union`] of [`Scent::Char`]s shall match a single [`char`] that is one of the given [`char`]s.
#[test]
fn union_of_chars() {
    let cur = Cur::new(Game::Union(&[
        Game::Single(Scent::Char('a')),
        Game::Single(Scent::Char('b')),
        Game::Single(Scent::Char('c')),
    ]));

    assert_passes!(cur; "a", "b", "c");
    assert_fails!(cur; "", "d", "ab", "ad", "da");
}

/// An empty [`Game::Sequence`] shall match an empty string.
#[test]
fn empty_sequence() {
    let cur = Cur::new(Game::Sequence(&[]));

    assert_passes!(cur; "");
    assert_fails!(cur; "a");
}

/// [`Game::Sequence`] of [`Scent::Char`]s shall indicate a string with matching [`char`]s in matching order.
#[test]
fn sequence_of_chars() {
    let cur = Cur::new(Game::Sequence(&[
        Game::Single(Scent::Char('a')),
        Game::Single(Scent::Char('b')),
        Game::Single(Scent::Char('c')),
    ]));

    assert_passes!(cur; "abc");
    assert_fails!(cur; "", "a", "ab", "aaa", "abcd", "dabc");
}

/// [`Game::Union`] of [`Game`]s where at least 1 [`Game`] is not [`Scent::Char`] shall match a [`&str`] equal to at least 1 of the [`Game`]s.
#[test]
fn union_sequences() {
    let cur = Cur::new(Game::Union(&[
        Game::Sequence(&[Game::Single(Scent::Char('a')), Game::Single(Scent::Char('b')), Game::Single(Scent::Char('c'))]),
        Game::Sequence(&[Game::Single(Scent::Char('d')), Game::Single(Scent::Char('e'))]),
        Game::Single(Scent::Char('f')),
    ]));

    assert_passes!(cur; "abc", "de", "f");
    assert_fails!(cur; "", "ab", "d", "fd", "df");
}

/// [`Game::Repetition`] of a [`Scent::Char`] shall match any repetition of the given [`char`].
#[test]
fn repetition() {
    let cur = Cur::new(Game::Repetition(&Game::Single(Scent::Char('a'))));

    assert_passes!(cur; "", "a", "aa", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
    assert_fails!(cur; "b", "ba", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaa");
}

/// [`Game::Union`] with [`Game::Repetition`] shall match when one of the branches matches.
#[test]
fn union_with_repetition() {
    let cur = Cur::new(Game::Union(&[
        Game::Repetition(&Game::Single(Scent::Char('a'))),
        Game::Single(Scent::Char('b')),
    ]));

    assert_passes!(cur; "", "a", "aa", "b");
    assert_fails!(cur; "ab", "c");
}

/// [`Game::Sequence`] with [`Game::Repetition`] followed by the repeated [`Game`].
#[test]
fn sequence_any_repetition_and_repeat() {
    let cur = Cur::new(Game::Sequence(&[
        Game::Repetition(&Game::Single(Scent::Char('a'))),
        Game::Single(Scent::Char('a')),
    ]));

    assert_passes!(cur; "a", "aa", "aaa");
    assert_fails!(cur; "", "b", "ab", "ba");
}