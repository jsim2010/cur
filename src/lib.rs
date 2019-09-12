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
        let possible_detections = self.scent.get_possible_detections(chars.as_slice(), 0);

        for detection in possible_detections {
            if chars.len() == detection {
                return true;
            }
        }

        false
    }

    /// Searches for scent starting at each index of `env`, returning the first successful index.
    ///
    /// [`None`] indicates there was no successful scent detection.
    pub fn point(&self, env: &str) -> Option<usize> {
        let chars: Vec<char> = env.chars().collect();

        if let Scent::Clear = self.scent {
            if chars.is_empty() {
                Some(0)
            } else {
                None
            }
        } else {
            let mut index = 0;

            for target in 0..chars.len() {
                if self
                    .scent
                    .get_possible_detections(&chars, target)
                    .is_empty()
                {
                    index += 1;
                } else {
                    return Some(index);
                }
            }

            None
        }
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
    /// Return all possible detections of `self` in starting from `index` of `chars`.
    ///
    /// A detection is the first index after the match.
    pub fn get_possible_detections(&self, chars: &[char], mut index: usize) -> Vec<usize> {
        match self {
            Self::Clear => vec![index],
            Self::Atom(c) => {
                if chars.get(index) == Some(c) {
                    index += 1;
                    vec![index]
                } else {
                    vec![]
                }
            }
            Self::Range(start, end) => {
                if let Some(c) = chars.get(index) {
                    if (start..=end).contains(&c) {
                        index += 1;
                        return vec![index];
                    }
                }

                vec![]
            }
            Self::Union(branches) => {
                let mut possible_detections = Vec::new();

                for branch in branches.iter() {
                    possible_detections.extend(branch.get_possible_detections(chars, index));
                }

                possible_detections
            }
            Self::Sequence(elements) => {
                let mut indexes = vec![index];
                let mut possible_detections = Vec::new();

                for scent in elements.iter() {
                    possible_detections = Vec::new();

                    for element_index in indexes {
                        possible_detections
                            .extend(scent.get_possible_detections(chars, element_index));
                    }

                    if possible_detections.is_empty() {
                        break;
                    } else {
                        indexes = possible_detections.clone();
                    }
                }

                possible_detections
            }
            Self::Repetition(scent, _cast) => {
                let mut possible_detections = vec![index];

                loop {
                    let next_detections = scent.get_possible_detections(chars, index);

                    if let Some(detection) = next_detections.last() {
                        index = *detection;
                        possible_detections.extend(next_detections);
                    } else {
                        break;
                    }
                }

                possible_detections
            }
        }
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
