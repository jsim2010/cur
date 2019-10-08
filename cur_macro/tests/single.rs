use cur::Scent;
use cur_macro::scent;

/// [`None`] is replaced by [`Scent::Absent`].
#[test]
fn none() {
    #[scent]
    const EMPTY: Scent = None;

    assert_eq!(EMPTY, Scent::Absent);
}

/// [`char`] is replaced by [`Scent::Atom`].
#[test]
fn char() {
    #[scent]
    const CHAR: Scent = 'a';

    assert_eq!(CHAR, Scent::Atom('a'));
}

/// [`&str`] is replaced by [`Scent::Sequence`] of [`Scent::Atom`]s.
#[test]
fn string() {
    #[scent]
    const STR: Scent = "abc";

    assert_eq!(
        STR,
        Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')])
    );
}

/// Parenthesis is replaced by [`Scent`] of the expression inside.
#[test]
fn parenthesis() {
    #[scent]
    const PARENTHESIS: Scent = ('a');

    assert_eq!(PARENTHESIS, Scent::Atom('a'));
}

/// Range of [`chars`].
#[test]
fn range() {
    #[scent]
    const RANGE: Scent = 'a'..'z';

    assert_eq!(RANGE, Scent::Range('a', 'z'));
}
