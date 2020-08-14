//! cur - Your unicode regular expression hunting companion.
#![no_std]

extern crate alloc;

pub mod prelude {
    pub use {
        super::{Branch, Pattern, Game, Step, Scent},
        once_cell::sync::Lazy,
        cur_macro::game,
    };
}

pub use cur_macro::game;

use {
    alloc::{collections::BTreeMap, vec, vec::Vec},
    core::{ops::{Add, BitOr, Range}, str::Chars},
};

/// Signifies a desired `char`.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Scent {
    /// Matches a single `char` equal to the one given.
    Char(char),
    /// Matches a single `char` equal to or in between the two given.
    Range(char, char),
}

impl Scent {
    /// Returns if `self` matches `ch`.
    fn is_match(&self, ch: char) -> bool {
        match *self {
            Self::Char(c) => c == ch,
            Self::Range(begin, end) => (begin..=end).contains(&ch),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Branch {
    Single(Scent),
    Sequence(Vec<Step>),
    Repetition(Pattern),
    Item(&'static str, &'static Game),
}

impl From<Branch> for Vec<Branch> {
    fn from(branch: Branch) -> Vec<Branch> {
        vec![branch]
    }
}

impl From<Game> for Vec<Branch> {
    fn from(game: Game) -> Vec<Branch> {
        match game {
            Game::Single(scent) => vec![Branch::Single(scent)],
            Game::Union(branches) => branches,
            Game::Sequence(steps) => vec![Branch::Sequence(steps)],
            Game::Repetition(pattern) => vec![Branch::Repetition(pattern)],
            Game::Item(name, game) => vec![Branch::Item(name, game)],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Step {
    Single(Scent),
    Union(Vec<Branch>),
    Repetition(Pattern),
    Item(&'static str, &'static Game),
}

impl From<Game> for Step {
    fn from(game: Game) -> Self {
        match game {
            Game::Single(scent) => Self::Single(scent),
            Game::Union(branches) => Self::Union(branches),
            Game::Sequence(steps) => steps[0].clone(),
            Game::Repetition(pattern) => Self::Repetition(pattern),
            Game::Item(name, g) => Self::Item(name, g),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Pattern {
    Single(Scent),
    Union(Vec<Branch>),
    Sequence(Vec<Step>),
    Item(&'static str, &'static Game),
}

impl From<Game> for Pattern {
    fn from(game: Game) -> Self {
        match game {
            Game::Single(scent) => Self::Single(scent),
            Game::Union(branches) => Self::Union(branches),
            Game::Sequence(steps) => Self::Sequence(steps),
            Game::Repetition(pattern) => pattern,
            Game::Item(name, g) => Self::Item(name, g),
        }
    }
}

/// Signifies a desired pattern of `char`s.
#[derive(Clone, Debug, PartialEq)]
pub enum Game {
    /// Matches a single `char` as described by the given `Scent`.
    Single(Scent),
    /// Matches any of the given `Branch`es.
    ///
    /// Matches are attempted in the order of the given `Branch`es.
    Union(Vec<Branch>),
    /// Matches each of the given `Step`s in order.
    ///
    /// An empty slice matches an empty sequence of `char`s.
    Sequence(Vec<Step>),
    /// Matches any number of repetitions of the given `Pattern`.
    ///
    /// Matches are attempted starting with 0 repetitions (an empty slice) and incrementing as high as possible.
    Repetition(Pattern),
    /// Matches the given `Game` that is associated with the given `&str`
    Item(&'static str, &'static Game),
}

impl Game {
    fn branches(self) -> Vec<Branch> {
        match self {
            Self::Single(scent) => vec![Branch::Single(scent)],
            Self::Union(branches) => branches,
            Self::Sequence(steps) => vec![Branch::Sequence(steps)],
            Self::Repetition(pattern) => vec![Branch::Repetition(pattern)],
            Self::Item(name, game) => vec![Branch::Item(name, game)],
        }
    }

    fn steps(self) -> Vec<Step> {
        match self {
            Self::Single(scent) => vec![Step::Single(scent)],
            Self::Union(branches) => vec![Step::Union(branches)],
            Self::Sequence(steps) => steps, 
            Self::Repetition(pattern) => vec![Step::Repetition(pattern)],
            Self::Item(name, game) => vec![Step::Item(name, game)],
        }
    }
}

impl Add for Game
{
    type Output = Game;

    fn add(self, rhs: Self) -> Self::Output {
        let mut steps = self.steps();
        steps.append(&mut rhs.steps());
        Self::Sequence(steps)
    }
}

impl BitOr for Game
{
    type Output = Game;

    fn bitor(self, rhs: Self) -> Self::Output {
        let mut branches = self.branches();
        branches.append(&mut rhs.branches());
        Self::Union(branches)
    }
}

impl From<Branch> for Game {
    fn from(branch: Branch) -> Self {
        match branch {
            Branch::Single(scent) => Self::Single(scent),
            Branch::Sequence(steps) => Self::Sequence(steps),
            Branch::Repetition(pattern) => Self::Repetition(pattern),
            Branch::Item(name, game) => Self::Item(name, game),
        }
    }
}

impl From<Pattern> for Game {
    fn from(pattern: Pattern) -> Self {
        match pattern {
            Pattern::Single(scent) => Self::Single(scent),
            Pattern::Union(branches) => Self::Union(branches),
            Pattern::Sequence(pattern) => Self::Sequence(pattern),
            Pattern::Item(name, game) => Self::Item(name, game),
        }
    }
}

impl From<Step> for Game {
    fn from(step: Step) -> Self {
        match step {
            Step::Single(scent) => Self::Single(scent),
            Step::Union(branches) => Self::Union(branches),
            Step::Repetition(pattern) => Self::Repetition(pattern),
            Step::Item(name, game) => Self::Item(name, game),
        }
    }
}

/// Iterates over a sequence of `char`s while tracking how far it has traveled.
#[derive(Clone, Debug)]
struct Spot<'l> {
    /// An `Iterator` over the remaining `char`s.
    chars: Chars<'l>,
    /// The number of bytes that the `Spot` has traveled.
    len: usize,
}

impl<'l> Spot<'l> {
    /// Returns if `self` has no more `char`s.
    fn is_end(mut self) -> bool {
        self.chars.next().is_none()
    }
}

impl<'l> From<&'l str> for Spot<'l> {
    fn from(value: &'l str) -> Self {
        Spot {
            chars: value.chars(),
            len: 0,
        }
    }
}

impl<'l> Iterator for Spot<'l> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.chars.next();

        // Update self.len.
        if let Some(c) = item {
            self.len += c.len_utf8();
        }

        item
    }
}

/// Signifies a match of a `Game` with a `&str`.
#[derive(Clone, Debug)]
pub struct Find<'l> {
    /// The text of the match.
    text: &'l str,
    /// The index, in bytes, that marks the inclusive start of the match.
    start: usize,
    /// The index, in bytes, that marks the non-inclusive end of the match.
    finish: usize,
}

impl<'l> Find<'l> {
    /// Creates a new `Find`.
    // TODO: This function should throw an error instead of unwrapping.
    fn new(range: Range<Spot<'l>>) -> Self {
        let start = range.start.len;
        let finish = range.end.len;
        Self { start, finish, text: range.start.chars.as_str().get(..finish.checked_sub(start).unwrap()).unwrap() }
    }

    /// Returns the first index of the match.
    pub const fn start(&self) -> usize {
        self.start
    }

    /// Returns the first index after the match.
    pub const fn finish(&self) -> usize {
        self.finish
    }
}

impl<'l> AsRef<str> for Find<'l> {
    fn as_ref(&self) -> &str {
        self.text
    }
}

/// Maps `Find`s to an identifier.
// TODO: Possibly make the value be an Iterator of Find.
pub type Captures<'l, 'c> = BTreeMap<&'c str, Vec<Find<'l>>>;

/// Signifies a match of a [`Game`], where all inner [`Game::Item`]s are stored within
// TODO: Review the efficiency of Catch.
#[derive(Debug)]
pub struct Catch<'l, 'c> {
    /// The [`Find`] over the original [`Game`].
    find: Find<'l>,
    /// An map that associates names with [`Find`]s
    captures: Captures<'l, 'c>,
}

impl<'l, 'c> Catch<'l, 'c> {
    /// Creates a new `Catch`.
    const fn new(find: Find<'l>, captures: Captures<'l, 'c>) -> Self {
        Self { find, captures }
    }

    /// Returns the first index of the match with [`Game`].
    pub const fn start(&self) -> usize {
        self.find.start()
    }

    /// Returns the first index after the match with [`Game`].
    pub const fn finish(&self) -> usize {
        self.find.finish()
    }

    /// Returns the [`&str`] that matched the [`Game`].
    pub fn as_str(&self) -> &str {
        self.find.as_ref()
    }

    /// Returns the list of [`Find`]s identified by `id`.
    pub fn get(&self, id: &str) -> Option<Vec<Find<'l>>> {
        self.captures.get(id).cloned()
    }
}

/// Returns [`Iterator`] of all finish [`Spot`]s that match `game` beginning from `start`.
// TODO: Currently returned Iterator is greedy, i.e. all processing is performed prior to
// returning Iterator. Ideally, this should be lazy such that processing only occurs at the time
// each Item is requested.
fn hunt<'l, 'c>(
    game: Game,
    mut start: Spot<'l>,
    captures: &mut Captures<'l, 'c>,
) -> impl Iterator<Item = Spot<'l>> + Clone {
    let mut finishes = Vec::new();

    match game {
        Game::Single(scent) => {
            if let Some(c) = start.next() {
                if scent.is_match(c) {
                    finishes.push(start);
                }
            }
        }
        Game::Union(branches) => {
            finishes.extend(
                branches
                    .iter()
                    .flat_map(|branch| hunt(Game::from(branch.clone()), start.clone(), captures)),
            );
        }
        Game::Sequence(elements) => {
            finishes.push(start);

            for element in elements.iter() {
                // Reset finishes with each iteration because all elements of a sequence must match.
                let new_starts: Vec<Spot<'l>> = finishes.drain(..).collect();

                for new_start in new_starts {
                    finishes.extend(hunt(Game::from(element.clone()), new_start, captures));
                }
            }
        }
        Game::Repetition(game) => {
            let mut new_starts = vec![start.clone()];
            // A repetition of zero is always a match.
            finishes.push(start);

            loop {
                let new_finishes: Vec<Spot<'l>> = new_starts
                    .drain(..)
                    .flat_map(|new_start| hunt(Game::from(game.clone()), new_start, captures))
                    .collect();

                if new_finishes.is_empty() {
                    // No match found; reached end of Repetition.
                    break;
                } else {
                    finishes.extend(new_finishes.clone());
                    new_starts = new_finishes;
                }
            }
        }
        Game::Item(id, game) => {
            let item_finishes = hunt(game.clone(), start.clone(), captures);

            // TODO: Determine how to handle the possibility of multiple matches with id.
            if captures
                .insert(
                    id,
                    item_finishes
                        .clone()
                        .map(|finish| Find::new(start.clone()..finish))
                        .collect(),
                )
                .is_some()
            {
                panic!("attempted to assign `Find` to `id` that was already assigned");
            }

            finishes.extend(item_finishes);
        }
    }

    finishes.into_iter()
}

/// Searches for sequences of `char`s that match its respective [`Game`].
#[derive(Clone, Debug)]
pub struct Cur {
    /// The [`Game`] to be hunted.
    game: Game,
}

impl Cur {
    /// Creates a [`Cur`] that will hunt for `game`.
    pub const fn new(game: Game) -> Self {
        Self { game }
    }

    /// Returns if `land` matches the [`Game`] `self` is hunting.
    pub fn is_game(&self, land: &str) -> bool {
        hunt(self.game.clone(), Spot::from(land), &mut Captures::new()).any(|finish| finish.is_end())
    }

    /// Returns the first [`Find`] that matches the [`Game`] `self` is hunting starting from each consecutive [`Spot`] of `land`.
    ///
    /// [`None`] indicates the [`Game`] cannot be detected starting from any index in `land`.
    pub fn find<'l>(&self, land: &'l str) -> Option<Find<'l>> {
        self.catch(land).map(|catch| catch.find)
    }

    /// Returns the first [`Catch`] that matches the [`Game`] of `self` starting from each consecutive [`Spot`] of `land`.
    ///
    /// [`None`] indicates the [`Game`] does not match starting from any index in `land`.
    pub fn catch<'l, 'c>(&self, land: &'l str) -> Option<Catch<'l, 'c>> {
        let mut start = Spot::from(land);
        let mut captures = Captures::new();

        loop {
            if let Some(finish) = hunt(self.game.clone(), start.clone(), &mut captures).next() {
                return Some(Catch::new(Find::new(start..finish), captures));
            }

            if start.next().is_none() {
                break;
            }
        }

        None
    }

    /// Returns the first [`Find`] that matches the [`Game`] associated with the `name`.
    ///
    /// [`None`] indicates the [`Game`] does not match `land`.
    pub fn grab<'l>(&self, land: &'l str, name: &str) -> Option<Find<'l>> {
        self.catch(land)
            .and_then(|catch| catch.get(name))
            .and_then(|finds| finds.into_iter().next())
    }
}
