use cur::{Cast, Cur, Scent};

/// [`Scent::Clear`] shall alert an empty string.
#[test]
fn empty() {
    let cur = Cur::with_scent(Scent::Clear);

    assert!(cur.alert(""));
    assert!(!cur.alert("a"));
}

/// [`Scent::Atom`] shall alert a string with a single matching [`char`].
#[test]
fn char() {
    let cur = Cur::with_scent(Scent::Atom('a'));

    assert!(cur.alert("a"));
    assert!(!cur.alert(""));
    assert!(!cur.alert("b"));
    assert!(!cur.alert("ab"));
}

/// [`Scent::Range`] shall alert a string with a single [`char`] that is within the given range.
#[test]
fn range() {
    let cur = Cur::with_scent(Scent::Range('b', 'd'));

    assert!(cur.alert("b"));
    assert!(cur.alert("c"));
    assert!(cur.alert("d"));
    assert!(!cur.alert("a"));
    assert!(!cur.alert("e"));
}

/// [`Scent::Union`] of [`Scent::Atom`]s shall alert a string with a single [`char`] that matches one of the given [`char`]s.
#[test]
fn union_of_chars() {
    let cur = Cur::with_scent(Scent::Union(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]));

    assert!(cur.alert("a"));
    assert!(cur.alert("b"));
    assert!(cur.alert("c"));
    assert!(!cur.alert(""));
    assert!(!cur.alert("d"));
    assert!(!cur.alert("ab"));
    assert!(!cur.alert("ad"));
    assert!(!cur.alert("da"));
}

/// [`Scent::Sequence`] of [`Scent::Atom`]s shall alert a string with matching [`char`]s in matching order.
#[test]
fn sequence_of_chars() {
    let cur = Cur::with_scent(Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]));

    assert!(cur.alert("abc"));
    assert!(!cur.alert(""));
    assert!(!cur.alert("a"));
    assert!(!cur.alert("ab"));
    assert!(!cur.alert("aaa"));
    assert!(!cur.alert("abcd"));
    assert!(!cur.alert("dabc"));
}

/// [`Scent::Union`] of [`Scent`]s  where at least 1 [`Scent`] is not [`Scent::Atom`] or [`Scent::Clear`] shall alert a string matching at least 1 of the [`Scent`]s.
#[test]
fn union_sequences() {
    let cur = Cur::with_scent(Scent::Union(&[Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]), Scent::Sequence(&[Scent::Atom('d'), Scent::Atom('e')]), Scent::Atom('f')]));

    assert!(cur.alert("abc"));
    assert!(cur.alert("de"));
    assert!(cur.alert("f"));
    assert!(!cur.alert(""));
    assert!(!cur.alert("ab"));
    assert!(!cur.alert("d"));
    assert!(!cur.alert("fd"));
    assert!(!cur.alert("df"));
}

/// [`Scent::Repetition`] of [`Scent::Atom`]s shall alert any repetition of [`Scent::Atom`].
#[test]
fn any_repetition() {
    let cur = Cur::with_scent(Scent::Repetition(&Scent::Atom('a'), Cast::Maximum));

    assert!(cur.alert(""));
    assert!(cur.alert("a"));
    assert!(cur.alert("aa"));
    assert!(cur.alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"));
    assert!(!cur.alert("b"));
    assert!(!cur.alert("ba"));
    assert!(!cur.alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab"));
    assert!(!cur.alert("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaa"));
}

/// [`Scent::Union`] with [`Scent::Repetition`] shall alert when one of the branches matches.
#[test]
fn union_with_any_repetition() {
    let cur = Cur::with_scent(Scent::Union(&[Scent::Repetition(&Scent::Atom('a'), Cast::Maximum), Scent::Atom('b')]));

    assert!(cur.alert(""));
    assert!(cur.alert("a"));
    assert!(cur.alert("aa"));
    assert!(cur.alert("b"));
    assert!(!cur.alert("ab"));
    assert!(!cur.alert("c"));
}

/// [`Scent::Sequence`] with [`Scent::Repetition`] followed by the repeated [`Scent`].
#[test]
fn sequence_any_repetition_and_repeat() {
    let cur = Cur::with_scent(Scent::Sequence(&[Scent::Repetition(&Scent::Atom('a'), Cast::Maximum), Scent::Atom('a')]));

    assert!(cur.alert("a"));
    assert!(cur.alert("aa"));
    assert!(cur.alert("aaa"));
    assert!(!cur.alert(""));
    assert!(!cur.alert("b"));
    assert!(!cur.alert("ab"));
    assert!(!cur.alert("ba"));
}
