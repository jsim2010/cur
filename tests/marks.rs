use {core::iter::FromIterator, cur::*, test_log::test};

macro_rules! assert_catch {
    ($cur:ident, $id:expr => ($begin:expr, $end:expr, $s:expr)) => {
        if let Some(catch) = $cur.next() {
            if let Some(find) = catch.get($id) {
                assert_eq!(
                    find.begin(),
                    $begin,
                    "expected find begin {}, got {}",
                    $begin,
                    find.begin()
                );
                assert_eq!(
                    find.end(),
                    $end,
                    "expected find end {}, got {}",
                    $end,
                    find.end()
                );
                assert_eq!(
                    find.as_str(),
                    $s,
                    "expected find '{}', got '{:?}'",
                    $s,
                    find.as_str()
                );
            } else {
                panic!("Expected mark of `{}` was not found", $id);
            }
        } else {
            panic!("Expected catch was not found");
        }
    };
}

fn surround_odor(odor: Odor) -> Odor {
    Odor::from_iter(vec![
        Scent::Repetition(Odor::from_iter(vec![Scent::Range('\u{0}', '\u{10ffff}')])),
        Scent::Marked(odor),
        Scent::Repetition(Odor::from_iter(vec![Scent::Range('\u{0}', '\u{10ffff}')])),
    ])
}

#[test]
fn empty() {
    let odor = surround_odor(Odor::default().mark("id").unwrap());
    let mut cur = Cur::new(&odor);

    cur.set_search("");
    assert_catch!(cur, "id" => (0, 0, ""));
    assert!(cur.next().is_none());

    cur.set_search("a");
    assert_catch!(cur, "id" => (0, 0, ""));
    assert_catch!(cur, "id" => (1, 1, ""));
    assert!(cur.next().is_none());
}

#[test]
fn char() {
    let odor = surround_odor(Odor::from_iter(vec![Scent::Char('a')]).mark("id").unwrap());
    let mut cur = Cur::new(&odor);

    cur.set_search("a");
    assert_catch!(cur, "id" => (0, 1, "a"));
    assert!(cur.next().is_none());

    cur.set_search("ab");
    assert_catch!(cur, "id" => (0, 1, "a"));
    assert!(cur.next().is_none());

    cur.set_search("ba");
    assert_catch!(cur, "id" => (1, 2, "a"));
    assert!(cur.next().is_none());

    cur.set_search("bbbab");
    assert_catch!(cur, "id" => (3, 4, "a"));
    assert!(cur.next().is_none());

    cur.set_search("");
    assert!(cur.next().is_none());

    cur.set_search("b");
    assert!(cur.next().is_none());
}

#[test]
fn range() {
    let odor = surround_odor(
        Odor::from_iter(vec![Scent::Range('b', 'd')])
            .mark("id")
            .unwrap(),
    );
    let mut cur = Cur::new(&odor);

    cur.set_search("b");
    assert_catch!(cur, "id" => (0, 1, "b"));
    assert!(cur.next().is_none());

    cur.set_search("cc");
    assert_catch!(cur, "id" => (0, 1, "c"));
    assert_catch!(cur, "id" => (1, 2, "c"));
    assert!(cur.next().is_none());

    cur.set_search("de");
    assert_catch!(cur, "id" => (0, 1, "d"));
    assert!(cur.next().is_none());

    cur.set_search("ad");
    assert_catch!(cur, "id" => (1, 2, "d"));
    assert!(cur.next().is_none());

    cur.set_search("");
    assert!(cur.next().is_none());

    cur.set_search("a");
    assert!(cur.next().is_none());

    cur.set_search("e");
    assert!(cur.next().is_none());
}

#[test]
fn union_of_chars() {
    let odor = surround_odor(
        Odor::from_iter(vec![Scent::Union(MultipleOdors::new(
            Odor::from_iter(vec![Scent::Char('a')]),
            Odor::from_iter(vec![Scent::Char('b')]),
            vec![Odor::from_iter(vec![Scent::Char('c')])],
        ))])
        .mark("id")
        .unwrap(),
    );
    let mut cur = Cur::new(&odor);

    cur.set_search("a");
    assert_catch!(cur, "id" => (0, 1, "a"));
    assert!(cur.next().is_none());

    cur.set_search("bb");
    assert_catch!(cur, "id" => (0, 1, "b"));
    assert_catch!(cur, "id" => (1, 2, "b"));
    assert!(cur.next().is_none());

    cur.set_search("cd");
    assert_catch!(cur, "id" => (0, 1, "c"));
    assert!(cur.next().is_none());

    cur.set_search("ea");
    assert_catch!(cur, "id" => (1, 2, "a"));
    assert!(cur.next().is_none());

    cur.set_search("");
    assert!(cur.next().is_none());

    cur.set_search("d");
    assert!(cur.next().is_none());
}

#[test]
fn sequence_of_chars() {
    let odor = surround_odor(
        Odor::from_iter(vec![Scent::Char('a'), Scent::Char('b'), Scent::Char('c')])
            .mark("id")
            .unwrap(),
    );
    let mut cur = Cur::new(&odor);

    cur.set_search("abc");
    assert_catch!(cur, "id" => (0, 3, "abc"));
    assert!(cur.next().is_none());

    cur.set_search("abcd");
    assert_catch!(cur, "id" => (0, 3, "abc"));
    assert!(cur.next().is_none());

    cur.set_search("dabc");
    assert_catch!(cur, "id" => (1, 4, "abc"));
    assert!(cur.next().is_none());

    cur.set_search("");
    assert!(cur.next().is_none());

    cur.set_search("a");
    assert!(cur.next().is_none());

    cur.set_search("ab");
    assert!(cur.next().is_none());

    cur.set_search("aaa");
    assert!(cur.next().is_none());
}

#[test]
fn union_sequences() {
    let odor = surround_odor(
        Odor::from_iter(vec![Scent::Union(MultipleOdors::new(
            Odor::from_iter(vec![Scent::Char('a'), Scent::Char('b'), Scent::Char('c')]),
            Odor::from_iter(vec![Scent::Char('d'), Scent::Char('e')]),
            vec![Odor::from_iter(vec![Scent::Char('f')])],
        ))])
        .mark("id")
        .unwrap(),
    );
    let mut cur = Cur::new(&odor);

    cur.set_search("abc");
    assert_catch!(cur, "id" => (0, 3, "abc"));
    assert!(cur.next().is_none());

    cur.set_search("de");
    assert_catch!(cur, "id" => (0, 2, "de"));
    assert!(cur.next().is_none());

    cur.set_search("f");
    assert_catch!(cur, "id" => (0, 1, "f"));
    assert!(cur.next().is_none());

    cur.set_search("fd");
    assert_catch!(cur, "id" => (0, 1, "f"));
    assert!(cur.next().is_none());

    cur.set_search("df");
    assert_catch!(cur, "id" => (1, 2, "f"));
    assert!(cur.next().is_none());

    cur.set_search("");
    assert!(cur.next().is_none());

    cur.set_search("ab");
    assert!(cur.next().is_none());

    cur.set_search("d");
    assert!(cur.next().is_none());
}

#[test]
fn repetition() {
    let odor = surround_odor(
        Odor::from_iter(vec![Scent::Repetition(Odor::from_iter(vec![Scent::Char(
            'a',
        )]))])
        .mark("id")
        .unwrap(),
    );
    let mut cur = Cur::new(&odor);

    cur.set_search("");
    assert_catch!(cur, "id" => (0, 0, ""));
    assert!(cur.next().is_none());

    cur.set_search("a");
    assert_catch!(cur, "id" => (0, 0, ""));
    assert_catch!(cur, "id" => (0, 1, "a"));
    assert_catch!(cur, "id" => (1, 1, ""));
    assert!(cur.next().is_none());

    cur.set_search("aa");
    assert_catch!(cur, "id" => (0, 0, ""));
    assert_catch!(cur, "id" => (0, 1, "a"));
    assert_catch!(cur, "id" => (0, 2, "aa"));
    assert_catch!(cur, "id" => (1, 1, ""));
    assert_catch!(cur, "id" => (1, 2, "a"));
    assert_catch!(cur, "id" => (2, 2, ""));
    assert!(cur.next().is_none());

    cur.set_search("b");
    assert_catch!(cur, "id" => (0, 0, ""));
    assert_catch!(cur, "id" => (1, 1, ""));
    assert!(cur.next().is_none());
}

#[test]
fn sequence_repetition_and_repeat() {
    let odor = surround_odor(
        Odor::from_iter(vec![
            Scent::Char('b'),
            Scent::Repetition(Odor::from_iter(vec![Scent::Char('a')])),
            Scent::Char('b'),
        ])
        .mark("id")
        .unwrap(),
    );
    let mut cur = Cur::new(&odor);

    cur.set_search("bb");
    assert_catch!(cur, "id" => (0, 2, "bb"));
    assert!(cur.next().is_none());

    cur.set_search("bab");
    assert_catch!(cur, "id" => (0, 3, "bab"));
    assert!(cur.next().is_none());

    cur.set_search("baab");
    assert_catch!(cur, "id" => (0, 4, "baab"));
    assert!(cur.next().is_none());

    cur.set_search("bba");
    assert_catch!(cur, "id" => (0, 2, "bb"));
    assert!(cur.next().is_none());

    cur.set_search("abab");
    assert_catch!(cur, "id" => (1, 4, "bab"));
    assert!(cur.next().is_none());

    cur.set_search("");
    assert!(cur.next().is_none());

    cur.set_search("a");
    assert!(cur.next().is_none());

    cur.set_search("b");
    assert!(cur.next().is_none());

    cur.set_search("ba");
    assert!(cur.next().is_none());

    cur.set_search("baa");
    assert!(cur.next().is_none());
}
