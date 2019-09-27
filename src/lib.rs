//! cur - Your hunting companion for regular expressions.
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
    clippy::fallible_impl_from, // Above lints assume a given use; issues should be detected by tests or other lints.
    clippy::implicit_return, // Omitting the return keyword is idiomatic Rust code.
    clippy::missing_inline_in_public_items, // There are issues with derived traits.
    clippy::multiple_crate_versions, // Not always possible to resolve.
    clippy::suspicious_arithmetic_impl, // Assumes a specific use; issues should be detected by tests.
    clippy::suspicious_op_assign_impl, // Assumes a specific use; issues should be detected by tests.
)]
#![no_std]

extern crate alloc;

use alloc::{vec, vec::Vec};
use core::str::Chars;
pub use cur_macro::scent;

/// Detects a [`Scent`] on a sequence of [`char`]s.
#[derive(Clone, Copy, Debug)]
pub struct Cur {
    /// The [`Scent`] to be detected.
    scent: Scent,
}

impl Cur {
    /// Creates a [`Cur`] with `scent`.
    pub const fn with_scent(scent: Scent) -> Self {
        Self { scent }
    }

    /// Returns if `self` is able to detect its [`Scent`] over the entirety of `area`.
    pub fn indicate(&self, area: &str) -> bool {
        for tracks in self.scent.follow_trail(Tracks::new(area.chars())) {
            if tracks.is_end() {
                return true;
            }
        }

        false
    }

    /// Returns the first [`Find`] from `self` attempting to detect its [`Scent`] starting from each consecutive index of `env`.
    ///
    /// [`None`] indicates the [`Scent`] cannot be detected starting from any index in `env`.
    pub fn point(&self, env: &str) -> Option<Find> {
        let mut tracks = Tracks::new(env.chars());

        loop {
            if let Some(end_tracks) = self.scent.follow_trail(tracks.clone()).first() {
                return Some(Find::new(tracks.index, end_tracks.index));
            }

            if let Some(c) = tracks.next() {
                tracks.step(c);
            } else {
                break;
            }
        }

        None
    }
}

/// Signifies a pattern that is detectable on a sequence of [`char`]s.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Scent {
    /// Detectable on an empty sequence.
    Absent,
    /// Detectable on a single [`char`] equal to the given [`char`].
    Atom(char),
    /// Detectable on a single [`char`] equal to or in between the given [`char`]s.
    Range(char, char),
    /// Detectable when any of the given [`Scent`]s is detectable.
    ///
    /// Detections are attempted according to the order of the given [`Scent`]s.
    Union(&'static [Scent]),
    /// Detectable when each of the given [`Scent`]s is detectable in order.
    Sequence(&'static [Scent]),
    /// Detectable when any number of repetititons of the given [`Scent`] is detected.
    ///
    /// Detections are attempted starting with 0 repetitions and incrementing as high as
    /// possible.
    Repetition(&'static Scent),
}

impl Scent {
    /// Returns the [`Tracks`]s of successful attempts to detect `self` along `tracks`.
    fn follow_trail<'a>(&self, mut tracks: Tracks<'a>) -> Vec<Tracks<'a>> {
        let mut new_tracks = Vec::new();

        match self {
            Scent::Absent => new_tracks.push(tracks),
            Scent::Atom(c) => {
                if tracks.next() == Some(*c) {
                    tracks.step(*c);
                    new_tracks.push(tracks);
                }
            }
            Scent::Range(start, end) => {
                if let Some(c) = tracks.next() {
                    if (start..=end).contains(&&c) {
                        tracks.step(c);
                        new_tracks.push(tracks);
                    }
                }
            }
            Scent::Union(branches) => {
                new_tracks.extend(
                    branches
                        .iter()
                        .flat_map(|branch| branch.follow_trail(tracks.clone())),
                );
            }
            Scent::Sequence(elements) => {
                new_tracks.push(tracks);

                for element in elements.iter() {
                    if new_tracks.is_empty() {
                        break;
                    } else {
                        let hot_trails = new_tracks.clone();
                        new_tracks.clear();

                        for trail in hot_trails {
                            new_tracks.extend(element.follow_trail(trail));
                        }
                    }
                }
            }
            Scent::Repetition(scent) => {
                let mut hot_trails = vec![tracks.clone()];
                new_tracks.push(tracks);

                loop {
                    let mut next_trails = Vec::new();

                    for trail in hot_trails {
                        next_trails.extend(scent.follow_trail(trail));
                    }

                    if next_trails.is_empty() {
                        break;
                    } else {
                        hot_trails = next_trails.clone();
                        new_tracks.extend(next_trails);
                    }
                }
            }
        }

        new_tracks
    }
}

/// Iterates over [`char`]s while tracking how far it has traveled.
#[derive(Clone)]
struct Tracks<'a> {
    /// An [`Iterator`] over the remaining chars to be detected.
    chars: Chars<'a>,
    /// The index, in bytes (not chars), that has already been followed.
    index: usize,
}

impl<'a> Tracks<'a> {
    /// Creates a new `Tracks` starting at the beginning of `chars`;
    const fn new(chars: Chars<'a>) -> Self {
        Self { chars, index: 0 }
    }

    /// Returns if `self` has no more chars.
    fn is_end(&self) -> bool {
        self.chars.clone().next().is_none()
    }

    /// Increments index of `self` by the number of bytes occupied by 'c'.
    fn step(&mut self, c: char) {
        self.index += c.len_utf8();
    }
}

impl<'a> Iterator for Tracks<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        self.chars.next()
    }
}

/// Signifies the location of a detection.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Find {
    /// The index of the start of the detection.
    start: usize,
    /// The index of the end of the detection.
    end: usize,
}

impl Find {
    /// Creates a [`Find`].
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
