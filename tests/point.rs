use cur::{Cur, Scent};

/// [`Scent::Clear`] shall point 0 for an empty string; otherwise it shall point [`None`].
#[test]
fn empty() {
    let cur = Cur::with_scent(Scent::Clear);

    assert_eq!(cur.point(""), Some(0));
    assert_eq!(cur.point("a"), None);
}

/// [`Scent::Atom`] shall point the index of the first matching [`char`].
#[test]
fn char() {
    let cur = Cur::with_scent(Scent::Atom('a'));

    assert_eq!(cur.point("a"), Some(0));
    assert_eq!(cur.point("ab"), Some(0));
    assert_eq!(cur.point("ba"), Some(1));
    assert_eq!(cur.point(""), None);
    assert_eq!(cur.point("b"), None);
}
