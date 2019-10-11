//! cur - Your unicode regular expression hunting companion.
#![warn(
    absolute_paths_not_starting_with_crate,
    anonymous_parameters,
    bare_trait_objects,
    deprecated_in_future,
    elided_lifetimes_in_paths,
    ellipsis_inclusive_range_patterns,
    explicit_outlives_requirements,
    keyword_idents,
    macro_use_extern_crate,
    missing_copy_implementations,
    missing_debug_implementations,
    missing_docs,
    missing_doc_code_examples,
    private_doc_tests,
    trivial_casts,
    trivial_numeric_casts,
    unreachable_pub,
    unsafe_code,
    unstable_features,
    unused_extern_crates,
    unused_import_braces,
    unused_labels,
    unused_lifetimes,
    unused_qualifications,
    unused_results,
    variant_size_differences,
    clippy::cargo,
    clippy::nursery,
    clippy::pedantic,
    clippy::restriction
)]
// Rustc lints that are not warned:
// box_pointers: Boxes are generally okay.
// single_use_lifetimes: There are issues with derived traits.
#![allow(
    clippy::fallible_impl_from, // Assumes a specific use; issues should be detected by tests or other lints.
    clippy::implicit_return, // Omitting the return keyword is idiomatic Rust code.
    clippy::missing_inline_in_public_items, // There are issues with derived traits.
    clippy::multiple_crate_versions, // Not always possible to resolve.
    clippy::suspicious_arithmetic_impl, // Assumes a specific use; issues should be detected by tests or other lints.
    clippy::suspicious_op_assign_impl, // Assumes a specific use; issues should be detected by tests or other lints.
)]
#![no_std]

extern crate alloc;

use alloc::{collections::BTreeMap, vec, vec::Vec};
use core::str::Chars;
pub use cur_macro::game;

/// Returns [`Iterator`] of all finish [`Spot`]s that match `game` beginning from `start`.
fn hunt<'l, 'c>(
    game: Game,
    mut start: Spot<'l>,
    captures: &mut Captures<'l, 'c>,
) -> impl Iterator<Item = Spot<'l>> + Clone {
    let mut finishes = Vec::new();

    match game {
        Game::Char(c) => {
            if start.next() == Some(c) {
                finishes.push(start);
            }
        }
        Game::Range(begin, end) => {
            if let Some(c) = start.next() {
                if (begin..=end).contains(&c) {
                    finishes.push(start);
                }
            }
        }
        Game::Union(branches) => {
            finishes.extend(
                branches
                    .iter()
                    .flat_map(|branch| hunt(*branch, start.clone(), captures)),
            );
        }
        Game::Sequence(elements) => {
            finishes.push(start);

            for element in elements.iter() {
                // Reset finishes with each iteration because all elements of a sequence must match.
                let new_starts: Vec<Spot<'l>> = finishes.drain(..).collect();

                for new_start in new_starts {
                    finishes.extend(hunt(*element, new_start, captures));
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
                    .flat_map(|new_start| hunt(*game, new_start, captures))
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
            let item_finishes = hunt(*game, start.clone(), captures);

            // TODO: Determine how to handle the possibility of multiple matches with id.
            if captures
                .insert(
                    id,
                    item_finishes
                        .clone()
                        .map(|finish| Find::new(start.clone(), finish.index))
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

/// Hunts a [`Game`] on a sequence of [`char`]s.
#[derive(Clone, Copy, Debug)]
pub struct Cur {
    /// The [`Game`] to be hunted.
    game: Game,
}

impl Cur {
    /// Creates a [`Cur`] that will hunt for `game`.
    pub const fn new(game: Game) -> Self {
        Self { game }
    }

    /// Returns if `animal` matches the [`Game`] of `self`.
    pub fn is_match(&self, animal: &str) -> bool {
        hunt(self.game, Spot::from(animal), &mut Captures::new()).any(|finish| finish.is_end())
    }

    /// Returns the first [`Find`] that matches the [`Game`] of `self` starting from each consecutive [`Spot`] of `land`.
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
            if let Some(finish) = hunt(self.game, start.clone(), &mut captures).next() {
                return Some(Catch::new(Find::new(start, finish.index), captures));
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

/// Maps a [`Vec`] of [`Find`]s to an identifier.
pub type Captures<'l, 'c> = BTreeMap<&'c str, Vec<Find<'l>>>;

/// Signifies a desired pattern of [`char`]s.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Game {
    /// Matches a single [`char`] equal to the one given.
    Char(char),
    /// Matches a single [`char`] equal to or in between the given [`char`]s.
    Range(char, char),
    /// Matches any of the given [`Game`]s.
    ///
    /// Match attempts are executed in the order of the given [`Game`]s.
    Union(&'static [Game]),
    /// Matches each of the given [`Game`]s in order.
    ///
    /// An empty slice matches an empty sequence of [`char`]s.
    Sequence(&'static [Game]),
    /// Matches any number of repetitions of the given [`Game`].
    ///
    /// Match attempts are executed starting with 0 repetitions (an empty slice) and incrementing as high as possible.
    Repetition(&'static Game),
    /// Matches the given [`Game`].
    ///
    /// If a [`Game`] containing the [`Item`] is captured, a [`Find`] representing the captured [`Game`] is associated with the given [`&str`].
    Item(&'static str, &'static Game),
}

/// Iterates over [`char`]s while tracking how far it has traveled.
#[derive(Clone, Debug)]
pub struct Spot<'l> {
    /// An [`Iterator`] over the remaining chars.
    chars: Chars<'l>,
    /// The index, in bytes (not chars), that has already been traveled.
    index: usize,
}

impl<'l> Spot<'l> {
    /// Returns if `self` has no more chars.
    fn is_end(&self) -> bool {
        self.chars.clone().next().is_none()
    }
}

impl<'l> From<&'l str> for Spot<'l> {
    fn from(value: &'l str) -> Self {
        Spot {
            chars: value.chars(),
            index: 0,
        }
    }
}

impl<'l> Iterator for Spot<'l> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let item = self.chars.next();

        // Update self.index.
        if let Some(c) = item {
            self.index += c.len_utf8();
        }

        item
    }
}

/// Signifies a match of a [`Game`].
#[derive(Clone, Debug)]
pub struct Find<'l> {
    /// The [`Spot`] where the `Find` starts.
    spot: Spot<'l>,
    /// The index, in bytes, of the finish of the match.
    finish: usize,
}

impl<'l> Find<'l> {
    /// Creates a new `Find`.
    const fn new(spot: Spot<'l>, finish: usize) -> Self {
        Self { spot, finish }
    }

    /// Returns the first index of the match with [`Game`].
    pub const fn start(&self) -> usize {
        self.spot.index
    }

    /// Returns the first index after the match with [`Game`].
    pub const fn finish(&self) -> usize {
        self.finish
    }

    /// Returns the [`&str`] that matched the [`Game`].
    pub fn as_str(&self) -> &str {
        self.spot
            .chars
            .as_str()
            .get(..self.finish.saturating_sub(self.spot.index))
            .unwrap()
    }
}

/// Signifies a match of a [`Game`], where all inner [`Game::Item`]s are stored within
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
        self.find.as_str()
    }

    /// Returns the list of [`Find`]s identified by `id`.
    pub fn get(&self, id: &str) -> Option<Vec<Find<'l>>> {
        self.captures.get(id).cloned()
    }
}
