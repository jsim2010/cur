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

use alloc::{vec::Vec, string::String};
use core::{iter::Peekable, str::Chars};

pub enum Scent {
    Clear,
    Atom(char),
    Union(Vec<Scent>),
    Sequence(Vec<Scent>),
}

impl Scent {
    fn alert(&self, chars: &Vec<char>, index: &mut usize) -> bool {
        match self {
            Self::Clear => true,
            Self::Atom(c) => {
                if chars.get(*index) == Some(c) {
                    *index += 1;
                    true
                } else {
                    false
                }
            }
            Self::Union(scents) => {
                for branch in scents {
                    let mut fork = *index;

                    if branch.alert(chars, &mut fork) {
                        // Only modify index if branch alerts.
                        *index = fork;
                        return true;
                    }
                }

                false
            }
            Self::Sequence(scents) => {
                for scent in scents {
                    if !scent.alert(chars, index) {
                        return false;
                    }
                }

                true
            }
        }
    }
}

pub struct Cur {
    scent: Scent,
}

impl Cur {
    pub fn with_scent(scent: Scent) -> Self {
        Cur {scent}
    }

    pub fn alert(&self, env: &str) -> bool {
        let chars = env.chars().collect();
        let mut index = 0;

        if self.scent.alert(&chars, &mut index) {
            index == chars.len()
        } else {
            false
        }
    }
}
