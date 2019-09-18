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
        for trail in self.scent.follow_trail(Trail::new(area.chars())) {
            if trail.is_end() {
                return true;
            }
        }

        false
    }

    /// Returns the first [`Find`] from `self` attempting to detect its [`Scent`] starting from each consecutive index of `env`.
    ///
    /// [`None`] indicates the [`Scent`] cannot be detected starting from any index in `env`.
    pub fn point(&self, env: &str) -> Option<Find> {
        let mut trail = Trail::new(env.chars());

        loop {
            if let Some(end_trail) = self.scent.follow_trail(trail.clone()).first() {
                return Some(Find::new(trail.index, end_trail.index));
            }

            if let Some(c) = trail.next() {
                trail.follow(c);
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
    /// It is valid to detect 0 repetitions, which is equivalent to [`Scent::Absent`]. If 1 or more repetitions are detectable, their order shall be determined by the given [`Cast`].
    Repetition(&'static Scent, Cast),
}

impl Scent {
    /// Returns the [`Trail`]s of successful attempts to detect `self` along `trail`.
    fn follow_trail<'a>(&self, mut trail: Trail<'a>) -> Vec<Trail<'a>> {
        let mut trails = Vec::new();

        match self {
            Scent::Absent => trails.push(trail),
            Scent::Atom(c) => {
                if trail.next() == Some(*c) {
                    trail.follow(*c);
                    trails.push(trail);
                }
            }
            Scent::Range(start, end) => {
                if let Some(c) = trail.next() {
                    if (start..=end).contains(&&c) {
                        trail.follow(c);
                        trails.push(trail);
                    }
                }
            }
            Scent::Union(branches) => {
                trails.extend(
                    branches
                        .iter()
                        .flat_map(|branch| branch.follow_trail(trail.clone())),
                );
            }
            Scent::Sequence(elements) => {
                trails.push(trail);

                for element in elements.iter() {
                    if trails.is_empty() {
                        break;
                    } else {
                        let hot_trails = trails.clone();
                        trails.clear();

                        for trail in hot_trails {
                            trails.extend(element.follow_trail(trail));
                        }
                    }
                }
            }
            Scent::Repetition(scent, cast) => {
                let mut hot_trails = vec![trail.clone()];
                trails.push(trail);

                loop {
                    let mut next_trails = Vec::new();

                    for trail in hot_trails {
                        next_trails.extend(scent.follow_trail(trail));
                    }

                    if next_trails.is_empty() {
                        break;
                    } else {
                        hot_trails = next_trails.clone();
                        trails.extend(next_trails);
                    }
                }

                if *cast == Cast::Maximum {
                    trails.reverse();
                }
            }
        }

        trails
    }
}

/// Iterates over [`char`]s while tracking how far it has traveled.
#[derive(Clone)]
struct Trail<'a> {
    /// An [`Iterator`] over the remaining chars to be detected.
    chars: Chars<'a>,
    /// The index, in bytes (not chars), that has already been followed.
    index: usize,
}

impl<'a> Trail<'a> {
    /// Creates a new `Trail` starting at the beginning of `chars`;
    const fn new(chars: Chars<'a>) -> Self {
        Self { chars, index: 0 }
    }

    /// Returns if `self` has no more chars.
    fn is_end(&self) -> bool {
        self.chars.clone().next().is_none()
    }

    /// Increments index of `self` by the number of bytes occupied by 'c'.
    fn follow(&mut self, c: char) {
        self.index += c.len_utf8();
    }
}

impl<'a> Iterator for Trail<'a> {
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

/// Specifies the how [`Scent::Repetition`] repeats.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Cast {
    /// [`Scent::Repetition`] will prefer the minimum number of repeats.
    Minimum,
    /// [`Scent::Repetition`] will prefer the maximum number of repeats.
    Maximum,
}
