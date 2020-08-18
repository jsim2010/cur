use cur::*;

game!(DIGIT = '0'..='9');

mod scents {
    use super::*;

    game!(pub LOWERCASE = 'a'..='z');
}

/// A [`Game`].
#[test]
fn copy() {
    game!(COPY = DIGIT);

    assert_eq!(*COPY, Game::Single(Scent::Range('0', '9')));
}

/// A Union.
#[test]
fn union() {
    game!(DIGIT_OR_A = 'a' | DIGIT);

    assert_eq!(
        *DIGIT_OR_A,
        Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Range('0', '9'))
        ])
    );
}

/// A sequence.
#[test]
fn sequence() {
    game!(DIGIT_AND_A = DIGIT + 'a');

    assert_eq!(
        *DIGIT_AND_A,
        Game::Sequence(vec![
            Step::Single(Scent::Range('0', '9')),
            Step::Single(Scent::Char('a'))
        ])
    );
}

/// A option.
#[test]
fn option() {
    game!(OPTIONAL_DIGIT = DIGIT?);

    assert_eq!(
        *OPTIONAL_DIGIT,
        Game::Union(vec![Branch::Sequence(vec![]), Branch::Single(Scent::Range('0', '9'))])
    );
}

/// A repetition.
#[test]
fn repetition() {
    game!(ZERO_OR_MORE_DIGITS = [DIGIT; ..]);

    assert_eq!(
        *ZERO_OR_MORE_DIGITS,
        Game::Repetition(Pattern::Single(Scent::Range('0', '9')))
    );
}

/// A path.
#[test]
fn path() {
    game!(ALPHANUM = DIGIT | scents::LOWERCASE);

    assert_eq!(
        *ALPHANUM,
        Game::Union(vec![
            Branch::Single(Scent::Range('0', '9')),
            Branch::Single(Scent::Range('a', 'z'))
        ])
    );
}
