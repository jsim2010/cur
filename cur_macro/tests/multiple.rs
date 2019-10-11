use cur::Game;
use cur_macro::game;

#[game]
const DIGIT: Game = '0'..'9';

mod scents {
    use super::*;

    #[game]
    pub const LOWERCASE: Game = 'a'..'z';
}

/// A [`Game`].
#[test]
fn copy() {
    #[game]
    const COPY: Game = DIGIT;

    assert_eq!(COPY, Game::Range('0', '9'));
}

/// A Union.
#[test]
fn union() {
    #[game]
    const DIGIT_OR_A: Game = 'a' | DIGIT;

    assert_eq!(
        DIGIT_OR_A,
        Game::Union(&[Game::Char('a'), Game::Range('0', '9')])
    );
}

/// A sequence.
#[test]
fn sequence() {
    #[game]
    const DIGIT_AND_A: Game = DIGIT + 'a';

    assert_eq!(
        DIGIT_AND_A,
        Game::Sequence(&[Game::Range('0', '9'), Game::Char('a')])
    );
}

/// A option.
#[test]
fn option() {
    #[game]
    const OPTIONAL_DIGIT: Game = DIGIT?;

    assert_eq!(
        OPTIONAL_DIGIT,
        Game::Union(&[Game::Sequence(&[]), Game::Range('0', '9')])
    );
}

/// A repetition.
#[test]
fn repetition() {
    #[game]
    const ZERO_OR_MORE_DIGITS: Game = DIGIT[..];

    assert_eq!(
        ZERO_OR_MORE_DIGITS,
        Game::Repetition(&Game::Range('0', '9'))
    );
}

/// A path.
#[test]
fn path() {
    #[game]
    const ALPHANUM: Game = DIGIT | scents::LOWERCASE;

    assert_eq!(
        ALPHANUM,
        Game::Union(&[Game::Range('0', '9'), Game::Range('a', 'z')])
    );
}
