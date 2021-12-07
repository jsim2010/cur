use {core::iter::FromIterator, cur::*, test_log::test};

macro_rules! assert_clear {
    ($cur:ident) => {
        assert!($cur.is_clear())
    };
}

macro_rules! assert_not_clear {
    ($cur:ident) => {
        assert!(!$cur.is_clear())
    };
}

#[test]
fn char() {
    let odor = Odor::from_iter(vec![Scent::Char('a')]);
    let mut cur = Cur::new(&odor);

    cur.set_search("a");
    assert_not_clear!(cur);

    cur.set_search("");
    assert_clear!(cur);

    cur.set_search("b");
    assert_clear!(cur);

    cur.set_search("ab");
    assert_clear!(cur);

    cur.set_search("bbbbba");
    assert_clear!(cur);
}

#[test]
fn range() {
    let odor = Odor::from_iter(vec![Scent::Range('b', 'd')]);
    let mut cur = Cur::new(&odor);

    cur.set_search("b");
    assert_not_clear!(cur);

    cur.set_search("c");
    assert_not_clear!(cur);

    cur.set_search("d");
    assert_not_clear!(cur);

    cur.set_search("");
    assert_clear!(cur);

    cur.set_search("a");
    assert_clear!(cur);

    cur.set_search("e");
    assert_clear!(cur);
}

#[test]
fn union_of_chars() {
    let odor = Odor::from_iter(vec![Scent::Union(MultipleOdors::new(
        Odor::from_iter(vec![Scent::Char('a')]),
        Odor::from_iter(vec![Scent::Char('b')]),
        vec![Odor::from_iter(vec![Scent::Char('c')])],
    ))]);
    let mut cur = Cur::new(&odor);

    cur.set_search("a");
    assert_not_clear!(cur);

    cur.set_search("b");
    assert_not_clear!(cur);

    cur.set_search("c");
    assert_not_clear!(cur);

    cur.set_search("");
    assert_clear!(cur);

    cur.set_search("d");
    assert_clear!(cur);

    cur.set_search("ab");
    assert_clear!(cur);

    cur.set_search("ad");
    assert_clear!(cur);

    cur.set_search("da");
    assert_clear!(cur);
}

#[test]
fn empty_sequence() {
    let odor = Odor::default();
    let mut cur = Cur::new(&odor);

    cur.set_search("");
    assert_not_clear!(cur);

    cur.set_search("a");
    assert_clear!(cur);
}

#[test]
fn sequence_of_chars() {
    let odor = Odor::from_iter(vec![Scent::Char('a'), Scent::Char('b'), Scent::Char('c')]);
    let mut cur = Cur::new(&odor);

    cur.set_search("abc");
    assert_not_clear!(cur);

    cur.set_search("");
    assert_clear!(cur);

    cur.set_search("a");
    assert_clear!(cur);

    cur.set_search("ab");
    assert_clear!(cur);

    cur.set_search("aaa");
    assert_clear!(cur);

    cur.set_search("abcd");
    assert_clear!(cur);

    cur.set_search("dabc");
    assert_clear!(cur);
}

#[test]
fn union_sequences() {
    let odor = Odor::from_iter(vec![Scent::Union(MultipleOdors::new(
        Odor::from_iter(vec![Scent::Char('a'), Scent::Char('b'), Scent::Char('c')]),
        Odor::from_iter(vec![Scent::Char('d'), Scent::Char('e')]),
        vec![Odor::from_iter(vec![Scent::Char('f')])],
    ))]);
    let mut cur = Cur::new(&odor);

    cur.set_search("abc");
    assert_not_clear!(cur);

    cur.set_search("de");
    assert_not_clear!(cur);

    cur.set_search("f");
    assert_not_clear!(cur);

    cur.set_search("");
    assert_clear!(cur);

    cur.set_search("ab");
    assert_clear!(cur);

    cur.set_search("d");
    assert_clear!(cur);

    cur.set_search("fd");
    assert_clear!(cur);

    cur.set_search("df");
    assert_clear!(cur);
}

#[test]
fn repetition() {
    let odor = Odor::from_iter(vec![Scent::Repetition(Odor::from_iter(vec![Scent::Char(
        'a',
    )]))]);
    let mut cur = Cur::new(&odor);

    cur.set_search("");
    assert_not_clear!(cur);

    cur.set_search("a");
    assert_not_clear!(cur);

    cur.set_search("aa");
    assert_not_clear!(cur);

    cur.set_search("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa");
    assert_not_clear!(cur);

    cur.set_search("b");
    assert_clear!(cur);

    cur.set_search("ba");
    assert_clear!(cur);

    cur.set_search("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab");
    assert_clear!(cur);

    cur.set_search("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaabaaaaaaaaaaaaaaaaaaaaaaaa");
    assert_clear!(cur);
}

#[test]
fn union_with_repetition() {
    let odor = Odor::from_iter(vec![Scent::Union(MultipleOdors::new(
        Odor::from_iter(vec![Scent::Repetition(Odor::from_iter(vec![Scent::Char(
            'a',
        )]))]),
        Odor::from_iter(vec![Scent::Char('b')]),
        vec![],
    ))]);
    let mut cur = Cur::new(&odor);

    cur.set_search("");
    assert_not_clear!(cur);

    cur.set_search("a");
    assert_not_clear!(cur);

    cur.set_search("aa");
    assert_not_clear!(cur);

    cur.set_search("b");
    assert_not_clear!(cur);

    cur.set_search("ab");
    assert_clear!(cur);

    cur.set_search("c");
    assert_clear!(cur);
}

#[test]
fn sequence_any_repetition_and_repeat() {
    let odor = Odor::from_iter(vec![
        Scent::Repetition(Odor::from_iter(vec![Scent::Char('a')])),
        Scent::Char('a'),
    ]);
    let mut cur = Cur::new(&odor);

    cur.set_search("a");
    assert_not_clear!(cur);

    cur.set_search("aa");
    assert_not_clear!(cur);

    cur.set_search("aaa");
    assert_not_clear!(cur);

    cur.set_search("");
    assert_clear!(cur);

    cur.set_search("b");
    assert_clear!(cur);

    cur.set_search("ab");
    assert_clear!(cur);

    cur.set_search("ba");
    assert_clear!(cur);
}
