use cur::*;

macro_rules! test_game {
    ($test:ident: $input:pat => $game:expr) => {
        #[test]
        fn $test() {
            game!(GAME = $input);

            assert_eq!(*GAME, $game);
        }
    };
}

test_game! {empty: () => Game::Sequence(vec![])}
test_game! {char_literal: 'a' => Game::Single(Scent::Char('a'))}
test_game! {empty_string: "" => Game::Sequence(vec![])}
test_game! {single_char_string: "a" => Game::Single(Scent::Char('a'))}
test_game! {string: "abc" => Game::Sequence(vec![
                                           Step::Single(Scent::Char('a')),
                                           Step::Single(Scent::Char('b')),
                                           Step::Single(Scent::Char('c')),
])}
test_game! {byte: b'0' => Game::Single(Scent::Char('0'))}
test_game! {byte_string: b"test" => Game::Sequence(vec![
                                                  Step::Single(Scent::Char('t')),
                                                  Step::Single(Scent::Char('e')),
                                                  Step::Single(Scent::Char('s')),
                                                  Step::Single(Scent::Char('t')),
])}
test_game! {wildcard: _ => Game::Single(Scent::Range('\u{0}', '\u{10ffff}'))}
test_game! {range: 'a'..='z' => Game::Single(Scent::Range('a', 'z'))}
