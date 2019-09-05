use cur::{scent, Scent};

/// [`None`] is replaced by [`Scent::Clear`].
#[test]
fn none() {
    #[scent]
    const EMPTY: Scent = None;

    assert_eq!(EMPTY, Scent::Clear);
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
    const STRING: Scent = "abc";

    assert_eq!(STRING, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')]));
}

/// "|" is replaced by [`Scent::Union`].
#[test]
fn union() {
    #[scent]
    const UNION: Scent = "one" | "two";

    assert_eq!(UNION, Scent::Union(&[Scent::Sequence(&[Scent::Atom('o'), Scent::Atom('n'), Scent::Atom('e')]), Scent::Sequence(&[Scent::Atom('t'), Scent::Atom('w'), Scent::Atom('o')])]));
}

/// Multiple "|" are replaced by 1 [`Scent::Union`].
#[test]
fn multiple_union() {
    #[scent]
    const MULTIPLE_UNION: Scent = "one" | "two" | "three";

    assert_eq!(MULTIPLE_UNION, Scent::Union(&[Scent::Sequence(&[Scent::Atom('o'), Scent::Atom('n'), Scent::Atom('e')]), Scent::Sequence(&[Scent::Atom('t'), Scent::Atom('w'), Scent::Atom('o')]), Scent::Sequence(&[Scent::Atom('t'), Scent::Atom('h'), Scent::Atom('r'), Scent::Atom('e'), Scent::Atom('e')])]));
}

/// "+" is replaced by [`Scent::Sequence`].
#[test]
fn sequence() {
    #[scent]
    const SEQUENCE: Scent = 'a' + '1';

    assert_eq!(SEQUENCE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('1')]));
}

/// Multiple "+" are replaced by 1 [`Scent::Sequence`].
#[test]
fn multiple_sequence() {
    #[scent]
    const MULTIPLE_SEQUENCE: Scent = 'a' + '1' + ':';

    assert_eq!(MULTIPLE_SEQUENCE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('1'), Scent::Atom(':')]));
}

/// Expression in parentheses is replaced by [`Scent`].
#[test]
fn parentheses() {
    #[scent]
    const PARENTHESES: Scent = ('a');

    assert_eq!(PARENTHESES, Scent::Atom('a'));
}

/// Repeat with ".." len is replaced by [`Scent::AnyRepetition`].
#[test]
fn any_repetition() {
    #[scent]
    const ANY_REPETITION: Scent = ['a'; ..];

    assert_eq!(ANY_REPETITION, Scent::AnyRepetition(&Scent::Atom('a')));
}

/// Repeat with "1.." is replaced by [`Scent`] and [`Scent::AnyRepetition`].
#[test]
fn one_or_more() {
    #[scent]
    const ONE_OR_MORE: Scent = ['a'; 1..];

    assert_eq!(ONE_OR_MORE, Scent::Sequence(&[Scent::Atom('a'), Scent::AnyRepetition(&Scent::Atom('a'))]));
}

/// Repeat with "3.." is replaced by 3 [`Scent`]s and a [`Scent::AnyRepetition`].
#[test]
fn three_or_more() {
    #[scent]
    const THREE_OR_MORE: Scent = ['a'; 3..];

    assert_eq!(THREE_OR_MORE, Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Atom('a'), Scent::AnyRepetition(&Scent::Atom('a'))]));
}
