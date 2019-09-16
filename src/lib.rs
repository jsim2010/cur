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
        let chars: Vec<char> = area.chars().collect();

        for length in self.scent.detect_lengths(&chars) {
            if chars.len() == length {
                return true;
            }
        }

        false
    }

    /// Returns the first [`Find`] from `self` attempting to detect its [`Scent`] starting from each consecutive index of `env`.
    ///
    /// [`None`] indicates the [`Scent`] cannot be detected starting from any index in `env`.
    pub fn point(&self, env: &str) -> Option<Find> {
        let chars_vec: Vec<char> = env.chars().collect();
        let chars = &mut chars_vec.iter();
        let mut index = 0;

        loop {
            if let Some(length) = self.scent.detect_lengths(chars.as_slice()).first() {
                return Some(Find::new(index, *length));
            }

            if chars.next().is_none() {
                break;
            } else {
                index += 1;
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
    /// Returns the lengths of each successful detection of `self` on `chars`.
    fn detect_lengths(&self, chars: &[char]) -> Vec<usize> {
        let mut lengths = Vec::new();

        match self {
            Scent::Absent => lengths.push(0),
            Scent::Atom(c) => {
                if chars.first() == Some(c) {
                    lengths.push(1);
                }
            }
            Scent::Range(start, end) => {
                if let Some(c) = chars.first() {
                    if (start..=end).contains(&c) {
                        lengths.push(1);
                    }
                }
            }
            Scent::Union(branches) => {
                lengths.extend(
                    branches
                        .iter()
                        .flat_map(|branch| branch.detect_lengths(chars)),
                );
            }
            Scent::Sequence(elements) => {
                let mut indexes = vec![0];

                for element in elements.iter() {
                    lengths = Vec::new();

                    for index in indexes {
                        if let Some(next_chars) = chars.get(index..) {
                            lengths.extend(
                                element
                                    .detect_lengths(next_chars)
                                    .iter()
                                    .map(|length| index + length),
                            );
                        }
                    }

                    if lengths.is_empty() {
                        break;
                    } else {
                        indexes = lengths.clone();
                    }
                }
            }
            Scent::Repetition(scent, cast) => {
                let mut indexes = vec![0];
                lengths.push(0);

                loop {
                    let mut next_lengths = Vec::new();

                    for index in indexes {
                        if let Some(next_chars) = chars.get(index..) {
                            next_lengths.extend(
                                scent
                                    .detect_lengths(next_chars)
                                    .iter()
                                    .map(|length| index + length),
                            );
                        }
                    }

                    if next_lengths.is_empty() {
                        break;
                    } else {
                        indexes = next_lengths.clone();
                        lengths.extend(next_lengths);
                    }
                }

                if *cast == Cast::Maximum {
                    lengths.reverse();
                }
            }
        }

        lengths
    }
}

/// Signifies the location of a detection.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Find {
    /// The index of the start of the detection.
    index: usize,
    /// The length of the detection.
    length: usize,
}

impl Find {
    /// Creates a [`Find`].
    pub const fn new(index: usize, length: usize) -> Self {
        Self { index, length }
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
