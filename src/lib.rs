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

use alloc::{boxed::Box, vec, vec::Vec};

#[derive(Debug)]
pub enum Scent {
    Clear,
    Atom(char),
    Union(Vec<Scent>),
    Sequence(Vec<Scent>),
    AnyRepetition(Box<Scent>),
}

impl Scent {
    fn get_possible_detections(&self, chars: &[char], mut index: usize) -> Vec<usize> {
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
            Self::Union(scents) => {
                let mut possible_detections = Vec::new();

                for branch in scents {
                    possible_detections.extend(branch.get_possible_detections(chars, index));
                }

                possible_detections
            }
            Self::Sequence(scents) => {
                let mut possible_detections = Vec::new();

                for scent in scents {
                    possible_detections = scent.get_possible_detections(chars, index);

                    if possible_detections.is_empty() {
                        break;
                    } else {
                        index = possible_detections[0];
                    }
                }

                possible_detections
            }
            Self::AnyRepetition(scent) => {
                // Return index after detecting as many of scent as possible.
                let mut possible_detections = scent.get_possible_detections(chars, index);

                while !possible_detections.is_empty() {
                    index = possible_detections[0];
                    possible_detections = scent.get_possible_detections(chars, index);
                }

                vec![index]
            }
        }
    }
}

/// Searches for [`Scent`]s.
#[derive(Debug)]
pub struct Cur {
    /// The [`Scent`] the `cur` is searching for.
    scent: Scent,
}

impl Cur {
    /// Creates a [`Cur`] with `scent`.
    pub const fn with_scent(scent: Scent) -> Self {
        Self {scent}
    }

    /// Returns if scent matches `env`.
    ///
    /// Note that match only occurs if all of `env` is covered by scent.
    pub fn alert(&self, env: &str) -> bool {
        let chars: Vec<char> = env.chars().collect();
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
    pub fn indicate(&self, env: &str) -> Option<usize> {
        let chars: Vec<char> = env.chars().collect();
        let mut index = 0;

        if let Scent::Clear = self.scent {
            if chars.is_empty() {
                Some(0)
            } else {
                None
            }
        } else {
            for target in 0..chars.len() {
                if self.scent.get_possible_detections(&chars, target).is_empty() {
                    index += 1;
                } else {
                    return Some(index);
                }
            }

            None
        }
    }
}
