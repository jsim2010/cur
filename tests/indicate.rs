use cur::{Cur, Scent};

/// [`Scent::Clear`] shall indicate 0 for an empty string; otherwise it shall indicate [`None`].
#[test]
fn empty() {
    let cur = Cur::with_scent(Scent::Clear);

    assert_eq!(cur.indicate(""), Some(0));
    assert_eq!(cur.indicate("a"), None);
}

/// [`Scent::Atom`] shall indicate the index of the first matching [`char`].
#[test]
fn char() {
    let cur = Cur::with_scent(Scent::Atom('a'));

    assert_eq!(cur.indicate("a"), Some(0));
    assert_eq!(cur.indicate("ab"), Some(0));
    assert_eq!(cur.indicate("ba"), Some(1));
    assert_eq!(cur.indicate(""), None);
    assert_eq!(cur.indicate("b"), None);
}
