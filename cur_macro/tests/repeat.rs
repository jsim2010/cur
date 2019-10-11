use cur::Game;
use cur_macro::game;

/// Try.
#[test]
fn try_expr() {
    #[game]
    const TRY: Game = 'a'?;

    assert_eq!(TRY, Game::Union(&[Game::Sequence(&[]), Game::Char('a')]));
}

/// Index with usize.
#[test]
fn exact() {
    #[game]
    const THREE_REPEATS: Game = 'a'[3];

    assert_eq!(
        THREE_REPEATS,
        Game::Sequence(&[Game::Char('a'), Game::Char('a'), Game::Char('a')])
    );
}

#[test]
fn exactly_one() {
    #[game]
    const ONE_REPEAT: Game = 'a'[1];

    assert_eq!(ONE_REPEAT, Game::Char('a'));
}

#[test]
fn exactly_zero() {
    #[game]
    const ZERO_REPEAT: Game = 'a'[0];

    assert_eq!(ZERO_REPEAT, Game::Sequence(&[]));
}

/// Index with RangeFull.
#[test]
fn zero_or_more() {
    #[game]
    const ZERO_OR_MORE: Game = 'a'[..];

    assert_eq!(ZERO_OR_MORE, Game::Repetition(&Game::Char('a')));
}

/// Index with RangeFrom.
#[test]
fn start_or_more() {
    #[game]
    const THREE_OR_MORE: Game = 'a'[3..];

    assert_eq!(
        THREE_OR_MORE,
        Game::Sequence(&[
            Game::Char('a'),
            Game::Char('a'),
            Game::Char('a'),
            Game::Repetition(&Game::Char('a'))
        ])
    );
}

/// Index with RangeTo.
#[test]
fn less_than_end() {
    #[game]
    const LESS_THAN_FOUR: Game = 'a'[..4];

    assert_eq!(
        LESS_THAN_FOUR,
        Game::Sequence(&[
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')])
        ])
    );
}

/// Index with RangeToInclusive.
#[test]
fn end_or_less() {
    #[game]
    const THREE_OR_LESS: Game = 'a'[..=3];

    assert_eq!(
        THREE_OR_LESS,
        Game::Sequence(&[
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')])
        ])
    );
}

/// Index with Range.
#[test]
fn start_to_end() {
    #[game]
    const TWO_TO_FIVE: Game = 'a'[2..5];

    assert_eq!(
        TWO_TO_FIVE,
        Game::Sequence(&[
            Game::Char('a'),
            Game::Char('a'),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')])
        ])
    );
}

/// Index with RangeToInclusive.
#[test]
fn start_to_end_inclusive() {
    #[game]
    const TWO_TO_FOUR_INCLUSIVE: Game = 'a'[2..=4];

    assert_eq!(
        TWO_TO_FOUR_INCLUSIVE,
        Game::Sequence(&[
            Game::Char('a'),
            Game::Char('a'),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a')])
        ])
    );
}

/// BitOr with Try.
#[test]
fn try_union() {
    #[game]
    const TRY_UNION: Game = ('a' | 'b')?;

    assert_eq!(
        TRY_UNION,
        Game::Union(&[Game::Sequence(&[]), Game::Char('a'), Game::Char('b')])
    );
}

/// BitOr with Index.
#[test]
fn repeat_union() {
    #[game]
    const REPEAT_UNION: Game = ('a' | 'b')[1..3];

    assert_eq!(
        REPEAT_UNION,
        Game::Sequence(&[
            Game::Union(&[Game::Char('a'), Game::Char('b')]),
            Game::Union(&[Game::Sequence(&[]), Game::Char('a'), Game::Char('b')])
        ])
    );
}

/// Add with Try.
#[test]
fn try_sequence() {
    #[game]
    const TRY_SEQUENCE: Game = ('a' + 'b')?;

    assert_eq!(
        TRY_SEQUENCE,
        Game::Union(&[
            Game::Sequence(&[]),
            Game::Sequence(&[Game::Char('a'), Game::Char('b')]),
        ])
    );
}

/// Add with Index.
#[test]
fn repeat_sequence() {
    #[game]
    const REPEAT_SEQUENCE: Game = ('a' + 'b')[1..];

    assert_eq!(
        REPEAT_SEQUENCE,
        Game::Sequence(&[
            Game::Char('a'),
            Game::Char('b'),
            Game::Repetition(&Game::Sequence(&[Game::Char('a'), Game::Char('b')]),)
        ])
    );
}
