use cur::*;

game!(DIGIT = '0'..='9');
const NEWLINE: &str = "\r\n";

mod scents {
    use super::*;

    game!(pub LOWERCASE = 'a'..='z');
}

#[test]
fn copy() {
    game!(COPY = DIGIT);

    assert_eq!(*COPY, Game::Single(Scent::Range('0', '9')));
}

#[test]
fn path() {
    game!(LOWERCASE = scents::LOWERCASE);

    assert_eq!(*LOWERCASE, Game::Single(Scent::Range('a', 'z')),);
}

// Processing::game_pattern_syntax[union]
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

#[test]
fn sequence() {
    game!(DIGIT_AND_A = (DIGIT, 'a'));

    assert_eq!(
        *DIGIT_AND_A,
        Game::Sequence(vec![
            Step::Single(Scent::Range('0', '9')),
            Step::Single(Scent::Char('a'))
        ])
    );
}

#[test]
fn option() {
    game!(OPTIONAL_DIGIT = DIGIT?);

    assert_eq!(
        *OPTIONAL_DIGIT,
        Game::Union(vec![
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Range('0', '9'))
        ])
    );
}

#[test]
fn repetition() {
    game!(ZERO_OR_MORE_LOWERCASE = [scents::LOWERCASE; ..]);

    assert_eq!(
        *ZERO_OR_MORE_LOWERCASE,
        Game::Repetition(Pattern::Single(Scent::Range('a', 'z')))
    );
}

#[test]
fn name() {
    game!(MYDIGIT = mine @ DIGIT);

    assert_eq!(
        *MYDIGIT,
        Game::Item("mine", Box::new(Game::Single(Scent::Range('0', '9')))),
    );
}

#[test]
fn literal() {
    game!(LINE = ([_; ..], NEWLINE));

    assert_eq!(
        *LINE,
        Game::Sequence(vec![
            Step::Repetition(Pattern::Single(Scent::Range('\u{0}', '\u{10ffff}'))),
            Step::Single(Scent::Char('\r')),
            Step::Single(Scent::Char('\n')),
        ])
    );
}
