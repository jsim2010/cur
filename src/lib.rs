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

/// Searches for [`Scent`]s.
#[derive(Clone, Copy, Debug)]
pub struct Cur {
    /// The [`Scent`] the `cur` is searching for.
    scent: Scent,
}

impl Cur {
    /// Creates a [`Cur`] with `scent`.
    pub const fn with_scent(scent: Scent) -> Self {
        Self { scent }
    }

    /// Returns if scent matches `cover`.
    ///
    /// Note that match only occurs if scent matches with all of `area`.
    pub fn alert(&self, area: &str) -> bool {
        let chars: Vec<char> = area.chars().collect();
        let lengths = self.scent.get_find_lengths(chars.as_slice(), 0);

        for length in lengths {
            if chars.len() == length {
                return true;
            }
        }

        false
    }

    /// Searches for scent starting at each index of `env`, returning the first successful [`Find`].
    ///
    /// [`None`] indicates there was no successful scent detection.
    pub fn point(&self, env: &str) -> Option<Find> {
        let chars: Vec<char> = env.chars().collect();

        // Always check index 0, even if chars is empty; this catches cases where self.scent matches an empty string.
        if let Some(length) = self.scent.get_find_lengths(&chars, 0).first() {
            return Some(Find::new(0, *length));
        }

        for index in 1..chars.len() {
            if let Some(length) = self.scent.get_find_lengths(&chars, index).first() {
                return Some(Find::new(index, *length));
            }
        }

        None
    }
}

/// Represents a pattern to be matched against.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Scent {
    /// Matches an empty string.
    Clear,
    /// Matches a single [`char`].
    Atom(char),
    /// Matches any [`char`] inclusively between the two given.
    Range(char, char),
    /// Matches any given [`Scent`].
    ///
    /// Matches are attempted in the order of the [`Vec`].
    Union(&'static [Scent]),
    /// Matches each given [`Scent`] in the order of the [`Vec`].
    Sequence(&'static [Scent]),
    /// Matches any number of repetitions of the given [`Scent`], including 0.
    ///
    /// If the given [`Cast`] is [`Cast::Minimal`], will first match with the fewest number of repetitions. Otherwise, will first match with the greatest number of repetitions.
    Repetition(&'static Scent, Cast),
}

impl Scent {
    /// Return the lengths of all detections of `self` starting at `index` of `chars`.
    pub fn get_find_lengths(&self, chars: &[char], index: usize) -> Vec<usize> {
        let mut lengths = Vec::new();

        match self {
            Self::Clear => lengths.push(0),
            Self::Atom(c) => {
                if chars.get(index) == Some(c) {
                    lengths.push(1);
                }
            }
            Self::Range(start, end) => {
                if let Some(c) = chars.get(index) {
                    if (start..=end).contains(&c) {
                        lengths.push(1);
                    }
                }
            }
            Self::Union(branches) => {
                for branch in branches.iter() {
                    lengths.extend(branch.get_find_lengths(chars, index));
                }
            }
            Self::Sequence(elements) => {
                let mut indexes = vec![index];

                for scent in elements.iter() {
                    lengths = Vec::new();

                    for element_index in indexes {
                        let previous_length = element_index - index;
                        lengths
                            .extend(scent.get_find_lengths(chars, element_index).iter().map(|length| previous_length + length));
                    }

                    if lengths.is_empty() {
                        break;
                    } else {
                        indexes = lengths.iter().map(|length| index + length).collect();
                    }
                }
            }
            Self::Repetition(scent, cast) => {
                let mut indexes = vec![index];
                lengths.push(0);

                loop {
                    let mut next_lengths = Vec::new();

                    for element_index in indexes {
                        let previous_length = element_index - index;
                        next_lengths.extend(scent.get_find_lengths(chars, element_index).iter().map(|length| previous_length + length));
                    }

                    if next_lengths.is_empty() {
                        break;
                    } else {
                        indexes = next_lengths.iter().map(|length| index + length).collect();
                        lengths.extend(next_lengths);
                    }
                }

                if *cast == Cast::Maximum {
                    lengths.reverse()
                }
            }
        }

        lengths
    }
}

#[derive(Debug, PartialEq)]
pub struct Find {
    index: usize,
    length: usize,
}

impl Find {
    pub fn new(index: usize, length: usize) -> Self {
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
