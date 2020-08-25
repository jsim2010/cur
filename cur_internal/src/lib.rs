//! cur_internal - Items shared by both `cur` and `cur_macro`.
#![no_std]

use {
    proc_macro2::TokenStream as TokenStream2,
    quote::{quote, ToTokens},
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
    #[inline]
    #[must_use]
    pub fn is_match(&self, ch: char) -> bool {
        match *self {
            Self::Char(c) => c == ch,
            Self::Range(begin, end) => (begin..=end).contains(&ch),
        }
    }
}

impl From<char> for Scent {
    #[inline]
    fn from(c: char) -> Self {
        Self::Char(c)
    }
}

impl ToTokens for Scent {
    #[inline]
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Char(c) => {
                quote! {
                    Scent::Char(#c)
                }
            }
            Self::Range(start, end) => {
                quote! {
                    Scent::Range(#start, #end)
                }
            }
        });
    }
}
