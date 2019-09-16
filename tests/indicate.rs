use cur::{Cast, Cur, Scent};

/// [`Scent::Absent`] shall indicate an empty string.
#[test]
fn empty() {
    let cur = Cur::with_scent(Scent::Absent);

    assert!(cur.indicate(""));
    assert!(!cur.indicate("a"));
}

/// [`Scent::Atom`] shall indicate a string with a single matching [`char`].
#[test]
fn char() {
    let cur = Cur::with_scent(Scent::Atom('a'));

    assert!(cur.indicate("a"));
    assert!(!cur.indicate(""));
    assert!(!cur.indicate("b"));
    assert!(!cur.indicate("ab"));
}

/// [`Scent::Range`] shall indicate a string with a single [`char`] that is within the given range.
#[test]
fn range() {
    let cur = Cur::with_scent(Scent::Range('b', 'd'));

    assert!(cur.indicate("b"));
    assert!(cur.indicate("c"));
    assert!(cur.indicate("d"));
    assert!(!cur.indicate(""));
    assert!(!cur.indicate("a"));
    assert!(!cur.indicate("e"));
}

/// [`Scent::Union`] of [`Scent::Atom`]s shall indicate a string with a single [`char`] that matches one of the given [`char`]s.
#[test]
fn union_of_chars() {
    let cur = Cur::with_scent(Scent::Union(&[
        Scent::Atom('a'),
        Scent::Atom('b'),
        Scent::Atom('c'),
    ]));

    assert!(cur.indicate("a"));
    assert!(cur.indicate("b"));
    assert!(cur.indicate("c"));
    assert!(!cur.indicate(""));
    assert!(!cur.indicate("d"));
    assert!(!cur.indicate("ab"));
    assert!(!cur.indicate("ad"));
    assert!(!cur.indicate("da"));
}

/// [`Scent::Sequence`] of [`Scent::Atom`]s shall indicate a string with matching [`char`]s in matching order.
#[test]
fn sequence_of_chars() {
    let cur = Cur::with_scent(Scent::Sequence(&[
        Scent::Atom('a'),
        Scent::Atom('b'),
        Scent::Atom('c'),
    ]));

    assert!(cur.indicate("abc"));
    assert!(!cur.indicate(""));
    assert!(!cur.indicate("a"));
    assert!(!cur.indicate("ab"));
    assert!(!cur.indicate("aaa"));
    assert!(!cur.indicate("abcd"));
    assert!(!cur.indicate("dabc"));
}

/// [`Scent::Union`] of [`Scent`]s where at least 1 [`Scent`] is not [`Scent::Atom`] or [`Scent::Absent`] shall indicate a string matching at least 1 of the [`Scent`]s.
#[test]
fn union_sequences() {
    let cur = Cur::with_scent(Scent::Union(&[
        Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]),
        Scent::Sequence(&[Scent::Atom('d'), Scent::Atom('e')]),
        Scent::Atom('f'),
    ]));

    assert!(cur.indicate("abc"));
    assert!(cur.indicate("de"));
    assert!(cur.indicate("f"));
    assert!(!cur.indicate(""));
    assert!(!cur.indicate("ab"));
    assert!(!cur.indicate("d"));
    assert!(!cur.indicate("fd"));
    assert!(!cur.indicate("df"));
}

/// [`Scent::Repetition`] of [`Scent::Atom`]s shall indicate any repetition of [`Scent::Atom`].
#[test]
fn repetition() {
    let cur = Cur::with_scent(Scent::Repetition(&Scent::Atom('a'), Cast::Maximum));

    assert!(cur.indicate(""));
    assert!(cur.indicate("a"));
    assert!(cur.indicate("aa"));
    assert!(cur.indicate("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"));
    assert!(!cur.indicate("b"));
    assert!(!cur.indicate("ba"));
    assert!(!cur.indicate("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"));
    assert!(!cur.indicate("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaa"));
}

/// [`Scent::Union`] with [`Scent::Repetition`] shall indicate when one of the branches matches.
#[test]
fn union_with_repetition() {
    let cur = Cur::with_scent(Scent::Union(&[
        Scent::Repetition(&Scent::Atom('a'), Cast::Maximum),
        Scent::Atom('b'),
    ]));

    assert!(cur.indicate(""));
    assert!(cur.indicate("a"));
    assert!(cur.indicate("aa"));
    assert!(cur.indicate("b"));
    assert!(!cur.indicate("ab"));
    assert!(!cur.indicate("c"));
}

/// [`Scent::Sequence`] with [`Scent::Repetition`] followed by the repeated [`Scent`].
#[test]
fn sequence_any_repetition_and_repeat() {
    let cur = Cur::with_scent(Scent::Sequence(&[
        Scent::Repetition(&Scent::Atom('a'), Cast::Maximum),
        Scent::Atom('a'),
    ]));

    assert!(cur.indicate("a"));
    assert!(cur.indicate("aa"));
    assert!(cur.indicate("aaa"));
    assert!(!cur.indicate(""));
    assert!(!cur.indicate("b"));
    assert!(!cur.indicate("ab"));
    assert!(!cur.indicate("ba"));
}
