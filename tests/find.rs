#![allow(clippy::cognitive_complexity)] // This is unavoidable in certain tests.
use cur::*;

macro_rules! assert_find {
    ($cur:ident; $land:expr; $start:expr, $finish:expr, $s:expr) => {
        if let Some(find) = $cur.find($land) {
            assert_eq!(find.start(), $start);
            assert_eq!(find.finish(), $finish);
            assert_eq!(find.as_ref(), $s);
        }
    };
}

/// An empty [`Game::Sequence`] shall always find an empty [`Find`].
#[test]
fn empty() {
    let game = Game::Sequence(vec![]);
    let cur = Cur::new(&game);

    assert_find!(cur; ""; 0, 0, "");
    assert_find!(cur; "a"; 0, 0, "");
}

/// [`Scent::Char`] shall find the first [`Find`].
#[test]
fn char() {
    let game = Game::Single(Scent::Char('a'));
    let cur = Cur::new(&game);

    assert_find!(cur; "a"; 0, 1, "a");
    assert_find!(cur; "ab"; 0, 1, "a");
    assert_find!(cur; "ba"; 1, 2, "a");
    assert!(cur.find("").is_none());
    assert!(cur.find("b").is_none());
}

/// [`Scent::Range`] shall find the first [`Find`].
#[test]
fn range() {
    let cur = Cur::new(&Game::Single(Scent::Range('b', 'd')));

    assert_find!(cur; "b"; 0, 1, "b");
    assert_find!(cur; "cc"; 0, 1, "c");
    assert_find!(cur; "de"; 0, 1, "d");
    assert_find!(cur; "ad"; 1, 2, "d");
    assert!(cur.find("").is_none());
    assert!(cur.find("a").is_none());
    assert!(cur.find("e").is_none());
}

/// [`Game::Union`] of [`Scent::Char`]s shall find the first [`Find`].
#[test]
fn union_of_chars() {
    let game = Game::Union(vec![
        Branch::Single(Scent::Char('a')),
        Branch::Single(Scent::Char('b')),
        Branch::Single(Scent::Char('c')),
    ]);
    let cur = Cur::new(&game);

    assert_find!(cur; "a"; 0, 1, "a");
    assert_find!(cur; "bb"; 0, 1, "b");
    assert_find!(cur; "cd"; 0, 1, "c");
    assert_find!(cur; "ea"; 1, 2, "a");
    assert!(cur.find("").is_none());
    assert!(cur.find("d").is_none());
}

/// [`Game::Sequence`] of [`Scent::Char`]s shall find the first [`Find`].
#[test]
fn sequence_of_chars() {
    let game = Game::Sequence(vec![
        Step::Single(Scent::Char('a')),
        Step::Single(Scent::Char('b')),
        Step::Single(Scent::Char('c')),
    ]);
    let cur = Cur::new(&game);

    assert_find!(cur; "abc"; 0, 3, "abc");
    assert_find!(cur; "abcd"; 0, 3, "abc");
    assert_find!(cur; "dabc"; 1, 4, "abc");
    assert!(cur.find("").is_none());
    assert!(cur.find("a").is_none());
    assert!(cur.find("ab").is_none());
    assert!(cur.find("aaa").is_none());
}

/// [`Game::Union`] of [`Game`]s  where at least 1 [`Game`] is a [`Game::Sequence`] shall find the first [`Find`].
#[test]
fn union_sequences() {
    let game = Game::Union(vec![
        Branch::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b')),
            Step::Single(Scent::Char('c')),
        ]),
        Branch::Sequence(vec![
            Step::Single(Scent::Char('d')),
            Step::Single(Scent::Char('e')),
        ]),
        Branch::Single(Scent::Char('f')),
    ]);
    let cur = Cur::new(&game);

    assert_find!(cur; "abc"; 0, 3, "abc");
    assert_find!(cur; "de"; 0, 2, "de");
    assert_find!(cur; "f"; 0, 1, "f");
    assert_find!(cur; "fd"; 0, 1, "f");
    assert_find!(cur; "df"; 1, 2, "f");
    assert!(cur.find("").is_none());
    assert!(cur.find("ab").is_none());
    assert!(cur.find("d").is_none());
}

/// [`Game::Repetition`] shall find a [`Find`] with start and end of 0.
#[test]
fn min_repetition() {
    let cur = Cur::new(&Game::Repetition(Pattern::Single(Scent::Char('a'))));

    assert_find!(cur; ""; 0, 0, "");
    assert_find!(cur; "a"; 0, 0, "");
    assert_find!(cur; "aa"; 0, 0, "");
    assert_find!(cur; "b"; 0, 0, "");
}

/// [`Game::Sequence`] with [`Game::Repetition`] followed by the repeated [`Game`].
#[test]
fn sequence_repetition_and_repeat() {
    let game = Game::Sequence(vec![
        Step::Single(Scent::Char('b')),
        Step::Repetition(Pattern::Single(Scent::Char('a'))),
        Step::Single(Scent::Char('b')),
    ]);
    let cur = Cur::new(&game);

    assert_find!(cur; "bb"; 0, 2, "bb");
    assert_find!(cur; "bab"; 0, 3, "bab");
    assert_find!(cur; "baab"; 0, 4, "baab");
    assert_find!(cur; "bba"; 0, 2, "bb");
    assert_find!(cur; "abab"; 1, 4, "bab");
    assert!(cur.find("").is_none());
    assert!(cur.find("a").is_none());
    assert!(cur.find("b").is_none());
    assert!(cur.find("ba").is_none());
    assert!(cur.find("baa").is_none());
}
