use cur::Scent;
use cur_macro::scent;

/// Try.
#[test]
fn try_expr() {
    #[scent]
    const TRY: Scent = 'a'?;

    assert_eq!(TRY, Scent::Union(&[Scent::Absent, Scent::Atom('a')]));
}

/// Index with usize.
#[test]
fn exact() {
    #[scent]
    const THREE_REPEATS: Scent = 'a'[3];

    assert_eq!(
        THREE_REPEATS,
        Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('a'), Scent::Atom('a')])
    );
}

/// Index with RangeFull.
#[test]
fn zero_or_more() {
    #[scent]
    const ZERO_OR_MORE: Scent = 'a'[..];

    assert_eq!(ZERO_OR_MORE, Scent::Repetition(&Scent::Atom('a')));
}

/// Index with RangeFrom.
#[test]
fn start_or_more() {
    #[scent]
    const THREE_OR_MORE: Scent = 'a'[3..];

    assert_eq!(
        THREE_OR_MORE,
        Scent::Sequence(&[
            Scent::Atom('a'),
            Scent::Atom('a'),
            Scent::Atom('a'),
            Scent::Repetition(&Scent::Atom('a'))
        ])
    );
}

/// Index with RangeTo.
#[test]
fn less_than_end() {
    #[scent]
    const LESS_THAN_FOUR: Scent = 'a'[..4];

    assert_eq!(
        LESS_THAN_FOUR,
        Scent::Sequence(&[
            Scent::Union(&[Scent::Absent, Scent::Atom('a')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')])
        ])
    );
}

/// Index with RangeToInclusive.
#[test]
fn end_or_less() {
    #[scent]
    const THREE_OR_LESS: Scent = 'a'[..=3];

    assert_eq!(
        THREE_OR_LESS,
        Scent::Sequence(&[
            Scent::Union(&[Scent::Absent, Scent::Atom('a')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')])
        ])
    );
}

/// Index with Range.
#[test]
fn start_to_end() {
    #[scent]
    const TWO_TO_FIVE: Scent = 'a'[2..5];

    assert_eq!(
        TWO_TO_FIVE,
        Scent::Sequence(&[
            Scent::Atom('a'),
            Scent::Atom('a'),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')])
        ])
    );
}

/// Index with RangeToInclusive.
#[test]
fn start_to_end_inclusive() {
    #[scent]
    const TWO_TO_FOUR_INCLUSIVE: Scent = 'a'[2..=4];

    assert_eq!(
        TWO_TO_FOUR_INCLUSIVE,
        Scent::Sequence(&[
            Scent::Atom('a'),
            Scent::Atom('a'),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a')])
        ])
    );
}

/// BitOr with Try.
#[test]
fn try_union() {
    #[scent]
    const TRY_UNION: Scent = ('a' | 'b')?;

    assert_eq!(
        TRY_UNION,
        Scent::Union(&[Scent::Absent, Scent::Atom('a'), Scent::Atom('b')])
    );
}

/// BitOr with Index.
#[test]
fn repeat_union() {
    #[scent]
    const REPEAT_UNION: Scent = ('a' | 'b')[1..3];

    assert_eq!(
        REPEAT_UNION,
        Scent::Sequence(&[
            Scent::Union(&[Scent::Atom('a'), Scent::Atom('b')]),
            Scent::Union(&[Scent::Absent, Scent::Atom('a'), Scent::Atom('b')])
        ])
    );
}

/// Add with Try.
#[test]
fn try_sequence() {
    #[scent]
    const TRY_SEQUENCE: Scent = ('a' + 'b')?;

    assert_eq!(
        TRY_SEQUENCE,
        Scent::Union(&[
            Scent::Absent,
            Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')]),
        ])
    );
}

/// Add with Index.
#[test]
fn repeat_sequence() {
    #[scent]
    const REPEAT_SEQUENCE: Scent = ('a' + 'b')[1..];

    assert_eq!(
        REPEAT_SEQUENCE,
        Scent::Sequence(&[
            Scent::Atom('a'),
            Scent::Atom('b'),
            Scent::Repetition(&Scent::Sequence(&[Scent::Atom('a'), Scent::Atom('b')]),)
        ])
    );
}
