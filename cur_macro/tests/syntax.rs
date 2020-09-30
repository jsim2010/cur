use cur::*;

macro_rules! test_game {
    ($test:ident: $input:tt => $game:expr) => {
        #[test]
        fn $test() {
            game!(GAME = $input);

            assert_eq!(*GAME, $game);
        }
    };
    ($test:ident: $input:pat => $game:expr) => {
        #[test]
        fn $test() {
            game!(GAME = $input);

            assert_eq!(*GAME, $game);
        }
    };
    ($test:ident: $input:expr => $game:expr) => {
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
test_game! {optional: 'a'? => Game::Union(vec![
                                          Branch::Sequence(vec![]),
                                          Branch::Single(Scent::Char('a')),
])}
test_game! {single_sequence: ('a') => Game::Single(Scent::Char('a'))}
test_game! {sequence: ('a', 'b') => Game::Sequence(vec![
                                                   Step::Single(Scent::Char('a')),
                                                   Step::Single(Scent::Char('b')),
])}
test_game! {multiple_sequence: ('a', 'b', 'c') => Game::Sequence(vec![
                                        Step::Single(Scent::Char('a')),
                                        Step::Single(Scent::Char('b')),
                                        Step::Single(Scent::Char('c')),
])}
test_game! {optional_sequence: ('a', 'b')? => Game::Union(vec![
                                        Branch::Sequence(vec![]),
                                        Branch::Sequence(vec![
                                            Step::Single(Scent::Char('a')),
                                            Step::Single(Scent::Char('b')),
                                        ]),
])}
test_game! {sequence_with_option: ('a', 'b'?) => Game::Sequence(vec![
                                    Step::Single(Scent::Char('a')),
                                    Step::Union(vec![
                                            Branch::Sequence(vec![]),
                                            Branch::Single(Scent::Char('b')),
                                    ]),
])}
test_game! {union: 'a' | 'b' => Game::Union(vec![
                                        Branch::Single(Scent::Char('a')),
                                        Branch::Single(Scent::Char('b')),
])}
test_game! {multiple_union: 'a' | 'b' | 'c' => Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Char('b')),
            Branch::Single(Scent::Char('c'))
])}
test_game! {optional_union: ('a' | 'b')? => Game::Union(vec![
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('a')),
            Branch::Single(Scent::Char('b')),
])}
test_game! {union_with_option: 'a' | 'b'? => Game::Union(vec![
            Branch::Single(Scent::Char('a')),
            Branch::Sequence(vec![]),
            Branch::Single(Scent::Char('b')),
])}
test_game! {sequence_before_union: ('a', 'b') | 'c' => Game::Union(vec![
            Branch::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b')),
            ]),
            Branch::Single(Scent::Char('c')),
])}
test_game! {union_before_sequence: ('a', 'b' | 'c') => Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Single(Scent::Char('b')),
                Branch::Single(Scent::Char('c')),
            ]),
])}
test_game! {name: name @ 'a' => Game::Item("name", Box::new(Game::Single(Scent::Char('a'))))}
// Parentheses are included to make macro function.
test_game! {named_option: (name @ 'a'?) => Game::Item(
            "name",
            Box::new(Game::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]))
)}
test_game! {named_sequence: name @ ('a', 'b') => Game::Item(
            "name",
            Box::new(Game::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b')),
            ]))
)}
// Parentheses are included to make macro function.
test_game! {named_union: (name @ 'a' | 'b') => Game::Item(
            "name",
            Box::new(Game::Union(vec![
                Branch::Single(Scent::Char('a')),
                Branch::Single(Scent::Char('b')),
            ]))
)}
test_game! {exactly_zero: ['a'; 0] => Game::Sequence(vec![])}
test_game! {exactly_one: ['a'; 1] => Game::Single(Scent::Char('a'))}
test_game! {exact: ['a'; 3] => Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a'))
])}
test_game! {any: ['a'; ..] => Game::Repetition(Pattern::Single(Scent::Char('a')))}
test_game! {at_least: ['a'; 3..] => Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Repetition(Pattern::Single(Scent::Char('a')))
])}
test_game! {less_than: ['a'; ..4] => Game::Sequence(vec![
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
])}
test_game! {at_most: ['a'; ..=3] => Game::Sequence(vec![
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
])}
test_game! {within_exclusive: ['a'; 2..5] => Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
])}
test_game! {within_inclusive: ['a'; 2..=4] => Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('a')),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a'))
            ])
])}
test_game! {repeat_union: ['a' | 'b'; 1..3] => Game::Sequence(vec![
            Step::Union(vec![
                Branch::Single(Scent::Char('a')),
                Branch::Single(Scent::Char('b'))
            ]),
            Step::Union(vec![
                Branch::Sequence(vec![]),
                Branch::Single(Scent::Char('a')),
                Branch::Single(Scent::Char('b'))
            ])
])}
test_game! {repeat_sequence: [('a', 'b'); 1..] => Game::Sequence(vec![
            Step::Single(Scent::Char('a')),
            Step::Single(Scent::Char('b')),
            Step::Repetition(Pattern::Sequence(vec![
                Step::Single(Scent::Char('a')),
                Step::Single(Scent::Char('b'))
            ]),)
])}
