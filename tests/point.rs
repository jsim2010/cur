use cur::{Cur, Find, Scent};

/// [`Scent::Absent`] shall point a [`Find`] with index and length of 0.
#[test]
fn empty() {
    let cur = Cur::with_scent(Scent::Absent);

    assert_eq!(cur.point(""), Some(Find::new(0, 0)));
    assert_eq!(cur.point("a"), Some(Find::new(0, 0)));
}

/// [`Scent::Atom`] shall point the first [`Find`].
#[test]
fn char() {
    let cur = Cur::with_scent(Scent::Atom('a'));

    assert_eq!(cur.point("a"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("ab"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("ba"), Some(Find::new(1, 2)));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("b"), None);
}

/// [`Scent::Range`] shall point the first [`Find`].
#[test]
fn range() {
    let cur = Cur::with_scent(Scent::Range('b', 'd'));

    assert_eq!(cur.point("b"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("cc"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("de"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("ad"), Some(Find::new(1, 2)));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("a"), None);
    assert_eq!(cur.point("e"), None);
}

/// [`Scent::Union`] of [`Scent::Atom`]s shall point the first [`Find`].
#[test]
fn union_of_chars() {
    let cur = Cur::with_scent(Scent::Union(&[
        Scent::Atom('a'),
        Scent::Atom('b'),
        Scent::Atom('c'),
    ]));

    assert_eq!(cur.point("a"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("bb"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("cd"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("ea"), Some(Find::new(1, 2)));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("d"), None);
}

/// [`Scent::Sequence`] of [`Scent::Atom`]s shall point the first [`Find`].
#[test]
fn sequence_of_chars() {
    let cur = Cur::with_scent(Scent::Sequence(&[
        Scent::Atom('a'),
        Scent::Atom('b'),
        Scent::Atom('c'),
    ]));

    assert_eq!(cur.point("abc"), Some(Find::new(0, 3)));
    assert_eq!(cur.point("abcd"), Some(Find::new(0, 3)));
    assert_eq!(cur.point("dabc"), Some(Find::new(1, 4)));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("a"), None);
    assert_eq!(cur.point("ab"), None);
    assert_eq!(cur.point("aaa"), None);
}

/// [`Scent::Union`] of [`Scent`]s  where at least 1 [`Scent`] is a [`Scent::Sequence`] shall point the first [`Find`].
#[test]
fn union_sequences() {
    let cur = Cur::with_scent(Scent::Union(&[
        Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]),
        Scent::Sequence(&[Scent::Atom('d'), Scent::Atom('e')]),
        Scent::Atom('f'),
    ]));

    assert_eq!(cur.point("abc"), Some(Find::new(0, 3)));
    assert_eq!(cur.point("de"), Some(Find::new(0, 2)));
    assert_eq!(cur.point("f"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("fd"), Some(Find::new(0, 1)));
    assert_eq!(cur.point("df"), Some(Find::new(1, 2)));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("ab"), None);
    assert_eq!(cur.point("d"), None);
}

/// [`Scent::Repetition`] shall point a [`Find`] with start and end of 0.
#[test]
fn min_repetition() {
    let cur = Cur::with_scent(Scent::Repetition(&Scent::Atom('a')));

    assert_eq!(cur.point(""), Some(Find::new(0, 0)));
    assert_eq!(cur.point("a"), Some(Find::new(0, 0)));
    assert_eq!(cur.point("aa"), Some(Find::new(0, 0)));
    assert_eq!(cur.point("b"), Some(Find::new(0, 0)));
}

/// [`Scent::Sequence`] with [`Scent::Repetition`] followed by the repeated [`Scent`].
#[test]
fn sequence_repetition_and_repeat() {
    let cur = Cur::with_scent(Scent::Sequence(&[
        Scent::Atom('b'),
        Scent::Repetition(&Scent::Atom('a')),
        Scent::Atom('b'),
    ]));

    assert_eq!(cur.point("bb"), Some(Find::new(0, 2)));
    assert_eq!(cur.point("bab"), Some(Find::new(0, 3)));
    assert_eq!(cur.point("baab"), Some(Find::new(0, 4)));
    assert_eq!(cur.point("bba"), Some(Find::new(0, 2)));
    assert_eq!(cur.point("abab"), Some(Find::new(1, 4)));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("a"), None);
    assert_eq!(cur.point("b"), None);
    assert_eq!(cur.point("ba"), None);
    assert_eq!(cur.point("baa"), None);
}
