use cur::Scent;
use cur_macro::scent;

/// BitOr is replaced by [`Scent::Union`].
#[test]
fn union() {
    #[scent]
    const UNION: Scent = 'a' | 'b';

    assert_eq!(UNION, Scent::Union(&[Scent::Atom('a'), Scent::Atom('b')]));
}

/// Multiple BitOrs are replaced by a single [`Scent::Union`].
#[test]
fn multiple_union() {
    #[scent]
    const MULTIPLE_UNION: Scent = 'a' | 'b' | 'c';

    assert_eq!(
        MULTIPLE_UNION,
        Scent::Union(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')])
    );
}

/// Add is replaced by [`Scent::Sequence`].
#[test]
fn sequence() {
    #[scent]
    const SEQUENCE: Scent = 'a' + 'b';

    assert_eq!(
        SEQUENCE,
        Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')])
    );
}

/// Multiple Adds is replaced by a single [`Scent::Sequence`].
#[test]
fn multiple_sequence() {
    #[scent]
    const MULTIPLE_SEQUENCE: Scent = 'a' + 'b' + 'c';

    assert_eq!(
        MULTIPLE_SEQUENCE,
        Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b'), Scent::Atom('c')])
    );
}
