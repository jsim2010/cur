use cur::{Cur, Scent};

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

/// [`Scent::Union`] of [`Scent::Atom`]s shall alert a string with a single [`char`] that matches one of the given [`char`]s.
#[test]
fn union_chars() {
    let cur = Cur::with_scent(Scent::Union(vec![Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]));

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
fn sequence_chars() {
    let cur = Cur::with_scent(Scent::Sequence(vec![Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]));

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
    let cur = Cur::with_scent(Scent::Union(vec![Scent::Sequence(vec![Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]), Scent::Sequence(vec![Scent::Atom('d'), Scent::Atom('e')]), Scent::Atom('f')]));

    assert!(cur.alert("abc"));
    assert!(cur.alert("de"));
    assert!(cur.alert("f"));
    assert!(!cur.alert(""));
    assert!(!cur.alert("ab"));
    assert!(!cur.alert("d"));
    assert!(!cur.alert("fd"));
    assert!(!cur.alert("df"));
}
