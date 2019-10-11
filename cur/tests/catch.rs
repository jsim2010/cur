use cur::{Cur, Game};

macro_rules! assert_catch {
    ($cur:ident; $land:expr; $start:expr, $finish:expr, $s:expr $(; $id:expr=> $i_start:expr, $i_finish:expr, $i_s:expr)*) => {
        if let Some(catch) = $cur.catch($land) {
            assert_eq!(catch.start(), $start);
            assert_eq!(catch.finish(), $finish);
            assert_eq!(catch.as_str(), $s);

            $(
                if let Some(find) = catch.get($id).and_then(|finds| finds.into_iter().next()) {
                    assert_eq!(find.start(), $i_start);
                    assert_eq!(find.finish(), $i_finish);
                    assert_eq!(find.as_str(), $i_s);
                }
            )*
        }
    }
}

#[test]
fn non_item() {
    let cur = Cur::new(Game::Char('a'));

    assert_catch!(cur; "a"; 0, 1, "a");
}

#[test]
fn single_item() {
    let cur = Cur::new(Game::Item("id", &Game::Char('a')));

    assert_catch!(cur; "a"; 0, 1, "a"; "id"=> 0, 1, "a");
}

#[test]
fn inner_item() {
    let cur = Cur::new(Game::Sequence(&[
        Game::Item("id", &Game::Char('a')),
        Game::Char('b'),
    ]));

    assert_catch!(cur; "ab"; 0, 2, "ab"; "id"=> 0, 1, "a");
}
