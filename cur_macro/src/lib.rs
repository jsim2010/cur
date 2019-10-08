//! `cur_macro` - Procedural macros for [`cur`]
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
extern crate proc_macro;

use alloc::{boxed::Box, vec, vec::Vec, string::ToString};
use core::convert::{TryFrom, TryInto};
use proc_macro::TokenStream;
use proc_macro2::{
    Delimiter, Group, Literal, Punct, Spacing, Span, TokenStream as TokenStream2, TokenTree,
};
use quote::{quote, ToTokens, TokenStreamExt};
use syn::{
    parse::{Parse, ParseStream, Result as ParseResult},
    parse_macro_input, Ident, ItemConst,
};
use syn::{
    BinOp, Error, Expr, ExprBinary, ExprIndex, ExprPath, ExprRange, ExprTry, Lit,
    RangeLimits, Path, Visibility,
};

/// Converts `item` into a `cur::Scent`.
///
/// Creating `cur::Scent`s can quickly become complex and error-prone. It is intended that a user
/// can use this procedural macro to build a `cur::Scent` that is clearly understandable using
/// valid rust syntax.
///
/// # Example(s)
/// ```
/// use cur::{scent, Scent};
///
/// #[scent]
/// const HELLO_WORLD: Scent = "Hello world!";
/// ```
#[proc_macro_attribute]
pub fn scent(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ScentInput { vis, ident, scent } = parse_macro_input!(item as ScentInput);

    TokenStream::from(quote! {
        #vis const #ident: Scent = #scent;
    })
}

/// Information required to create a const [`Scent`] definition.
struct ScentInput {
    /// Visibility of the [`Scent`].
    vis: Visibility,
    /// Identifier of the [`Scent`].
    ident: Ident,
    /// Information required to create the [`Scent`] tokens.
    scent: ScentBuilder,
}

impl Parse for ScentInput {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        input.parse().and_then(|item: ItemConst| {
            Ok(Self {
                vis: item.vis,
                ident: item.ident,
                scent: (*item.expr).try_into()?,
            })
        })
    }
}

/// Maps to [`Scent`] definitions.
///
/// Using `ScentBuilder` instead of [`Scent`] removes need for adding [`cur`] as a dependency,
/// which would be a circular dependency.
#[derive(Clone, Debug)]
enum ScentBuilder {
    /// Maps to [`Scent::Absent`].
    Absent,
    /// Maps to [`Scent::Atom`].
    Atom(char),
    /// Maps to [`Scent::Range`].
    Range(char, char),
    /// Maps to [`Scent::Union`].
    Union(Vec<ScentBuilder>),
    /// Maps to [`Scent::Sequence`].
    Sequence(Vec<ScentBuilder>),
    /// Maps to [`Scent::Repetition`].
    Repetition(Box<ScentBuilder>),
    /// Maps to the [`Path`] of a [`Scent`]
    Path(Path),
}

impl ScentBuilder {
    /// Creates a `ScentBuilder` that repeats `self` as specified by `repeater`.
    fn repeat(self, repeater: &ScentRepeater) -> Self {
        if repeater.minimum == 0 && repeater.maximum == usize::max_value() {
            Self::Repetition(Box::new(self))
        } else {
            let mut sequence = Vec::new();

            for _ in 0..repeater.minimum {
                sequence.extend(self.clone().into_elements());
            }

            if repeater.maximum == usize::max_value() {
                sequence.push(Self::Repetition(Box::new(self)));
            } else {
                for _ in repeater.minimum..repeater.maximum {
                    sequence.push(Self::Absent.branch(self.clone()));
                }
            }

            Self::Sequence(sequence)
        }
    }

    /// Creates a `ScentBuilder` with alternate `branch` if `self` fails.
    fn branch(self, branch: Self) -> Self {
        Self::Union(if let Self::Union(mut branches) = self {
            branches.push(branch);
            branches
        } else if let Self::Union(mut branches) = branch {
            branches.insert(0, self);
            branches
        } else {
            vec![self, branch]
        })
    }

    /// Converts `self` into a sequence of `ScentBuilder`s.
    fn into_elements(self) -> Vec<Self> {
        if let Self::Sequence(elements) = self {
            elements
        } else {
            vec![self]
        }
    }

    /// Converts `expr` to a [`char`].
    ///
    /// [`Err`] indicates `expr` is unable to be converted.
    fn char_try_from_expr(expr: Expr) -> ParseResult<char> {
        if let Expr::Lit(literal) = expr {
            if let Lit::Char(c) = literal.lit {
                Ok(c.value())
            } else {
                Err(Error::new_spanned(literal, "Expected char literal"))
            }
        } else {
            Err(Error::new_spanned(expr, "Expected literal"))
        }
    }

    /// Appends "Scent::" to `tokens`.
    fn append_scent_tokens(tokens: &mut TokenStream2) {
        tokens.append(Ident::new("Scent", Span::call_site()));
        tokens.append(Punct::new(':', Spacing::Joint));
        tokens.append(Punct::new(':', Spacing::Alone));
    }
}

impl ToTokens for ScentBuilder {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Absent => {
                Self::append_scent_tokens(tokens);
                tokens.append(Ident::new("Absent", Span::call_site()));
            }
            Self::Atom(c) => {
                Self::append_scent_tokens(tokens);
                tokens.append(Ident::new("Atom", Span::call_site()));
                tokens.append(Group::new(
                    Delimiter::Parenthesis,
                    TokenTree::from(Literal::character(*c)).into(),
                ));
            }
            Self::Range(start, end) => {
                Self::append_scent_tokens(tokens);
                let mut range_values = TokenStream2::new();

                tokens.append(Ident::new("Range", Span::call_site()));

                range_values.append(Literal::character(*start));
                range_values.append(Punct::new(',', Spacing::Alone));
                range_values.append(Literal::character(*end));

                tokens.append(Group::new(Delimiter::Parenthesis, range_values));
            }
            Self::Sequence(elements) => {
                Self::append_scent_tokens(tokens);
                let mut element_list = TokenStream2::new();
                let mut element_array = TokenStream2::new();

                tokens.append(Ident::new("Sequence", Span::call_site()));

                element_array.append(Punct::new('&', Spacing::Alone));

                if let Some(first_element) = elements.first() {
                    first_element.to_tokens(&mut element_list);
                }

                for element in elements.iter().skip(1) {
                    element_list.append(Punct::new(',', Spacing::Alone));
                    element.to_tokens(&mut element_list);
                }

                element_array.append(Group::new(Delimiter::Bracket, element_list));
                tokens.append(Group::new(Delimiter::Parenthesis, element_array));
            }
            Self::Union(branches) => {
                Self::append_scent_tokens(tokens);
                let mut branch_array = TokenStream2::new();
                let mut branch_list = TokenStream2::new();

                tokens.append(Ident::new("Union", Span::call_site()));

                branch_array.append(Punct::new('&', Spacing::Alone));

                if let Some(first_branch) = branches.first() {
                    first_branch.to_tokens(&mut branch_list);
                }

                for branch in branches.iter().skip(1) {
                    branch_list.append(Punct::new(',', Spacing::Alone));
                    branch.to_tokens(&mut branch_list);
                }

                branch_array.append(Group::new(Delimiter::Bracket, branch_list));
                tokens.append(Group::new(Delimiter::Parenthesis, branch_array));
            }
            Self::Repetition(scent) => {
                Self::append_scent_tokens(tokens);
                let mut repetition_args = TokenStream2::new();

                tokens.append(Ident::new("Repetition", Span::call_site()));

                repetition_args.append(Punct::new('&', Spacing::Alone));
                scent.to_tokens(&mut repetition_args);

                tokens.append(Group::new(Delimiter::Parenthesis, repetition_args));
            }
            Self::Path(path) => {
                path.to_tokens(tokens);
            }
        }
    }
}

impl TryFrom<Expr> for ScentBuilder {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Path(path) => path.try_into(),
            Expr::Lit(literal) => literal.lit.try_into(),
            Expr::Binary(binary) => binary.try_into(),
            Expr::Paren(paren) => (*paren.expr).try_into(),
            Expr::Index(index) => index.try_into(),
            Expr::Try(try_expr) => try_expr.try_into(),
            Expr::Range(range) => range.try_into(),
            Expr::Repeat(..)
            | Expr::Unary(..)
            | Expr::Box(..)
            | Expr::Await(..)
            | Expr::Array(..)
            | Expr::Call(..)
            | Expr::MethodCall(..)
            | Expr::Tuple(..)
            | Expr::Cast(..)
            | Expr::Let(..)
            | Expr::If(..)
            | Expr::While(..)
            | Expr::ForLoop(..)
            | Expr::Loop(..)
            | Expr::Match(..)
            | Expr::Closure(..)
            | Expr::Unsafe(..)
            | Expr::Block(..)
            | Expr::Assign(..)
            | Expr::AssignOp(..)
            | Expr::Field(..)
            | Expr::Reference(..)
            | Expr::Break(..)
            | Expr::Continue(..)
            | Expr::Return(..)
            | Expr::Macro(..)
            | Expr::Struct(..)
            | Expr::Group(..)
            | Expr::Async(..)
            | Expr::TryBlock(..)
            | Expr::Type(..)
            | Expr::Yield(..)
            | Expr::Verbatim(..)
            | Expr::__Nonexhaustive => Err(Error::new_spanned(value, "Invalid scent expression")),
        }
    }
}

impl TryFrom<ExprBinary> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprBinary) -> Result<Self, Self::Error> {
        let lhs: Self = (*value.left).try_into()?;
        let rhs: Self = (*value.right).try_into()?;

        match value.op {
            BinOp::BitOr(..) => Ok(lhs.branch(rhs)),
            BinOp::Add(..) => {
                let mut elements = lhs.into_elements();
                elements.append(&mut rhs.into_elements());
                Ok(Self::Sequence(elements))
            }
            BinOp::BitAnd(..)
            | BinOp::Sub(..)
            | BinOp::Mul(..)
            | BinOp::Div(..)
            | BinOp::Rem(..)
            | BinOp::And(..)
            | BinOp::Or(..)
            | BinOp::BitXor(..)
            | BinOp::Shl(..)
            | BinOp::Shr(..)
            | BinOp::Eq(..)
            | BinOp::Lt(..)
            | BinOp::Le(..)
            | BinOp::Ne(..)
            | BinOp::Ge(..)
            | BinOp::Gt(..)
            | BinOp::AddEq(..)
            | BinOp::SubEq(..)
            | BinOp::MulEq(..)
            | BinOp::DivEq(..)
            | BinOp::RemEq(..)
            | BinOp::BitXorEq(..)
            | BinOp::BitAndEq(..)
            | BinOp::BitOrEq(..)
            | BinOp::ShlEq(..)
            | BinOp::ShrEq(..) => Err(Error::new_spanned(value.op, "Invalid binary operation")),
        }
    }
}

impl TryFrom<ExprIndex> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprIndex) -> Result<Self, Self::Error> {
        let repeater = ScentRepeater::try_from(*value.index)?;
        (*value.expr)
            .try_into()
            .map(|scent: Self| scent.repeat(&repeater))
    }
}

impl TryFrom<ExprPath> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprPath) -> Result<Self, Self::Error> {
        if let Some(ident) = value.path.get_ident() {
            if ident.to_string().as_str() == "None" {
                return Ok(Self::Absent);
            }
        }
        
        Ok(Self::Path(value.path))
    }
}

impl TryFrom<ExprRange> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprRange) -> Result<Self, Self::Error> {
        Ok(Self::Range(
            value
                .from
                .map_or(Ok('\u{0}'), |from| Self::char_try_from_expr(*from))?,
            value
                .to
                .map_or(Ok('\u{10ffff}'), |to| Self::char_try_from_expr(*to))?,
        ))
    }
}

impl TryFrom<ExprTry> for ScentBuilder {
    type Error = Error;

    fn try_from(value: ExprTry) -> Result<Self, Self::Error> {
        Self::try_from(*value.expr).map(|scent| Self::Absent.branch(scent))
    }
}

impl TryFrom<Lit> for ScentBuilder {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Char(c) => Ok(Self::Atom(c.value())),
            Lit::Str(s) => Ok(Self::Sequence(
                s.value().chars().map(Self::Atom).collect(),
            )),
            Lit::ByteStr(..)
            | Lit::Byte(..)
            | Lit::Int(..)
            | Lit::Float(..)
            | Lit::Bool(..)
            | Lit::Verbatim(..) => Err(Error::new_spanned(
                value,
                "Expected character or string literal.",
            )),
        }
    }
}

/// Information regarding the number of times a [`ScentBuilder`] can be repeated.
#[derive(Debug)]
struct ScentRepeater {
    /// The smallest number of repeats.
    minimum: usize,
    /// The largest number of repeats.
    ///
    /// A number <= `minimum` indicates the [`ScentBuilder`] must be repeated exactly
    /// `minimum` times.
    maximum: usize,
}

impl ScentRepeater {
    /// Converts `lit` to a [`usize`].
    ///
    /// [`Err`] indicates `lit` is unable to be converted.
    fn usize_try_from_lit(lit: &Lit) -> ParseResult<usize> {
        if let Lit::Int(int) = lit {
            int.base10_parse::<usize>()
        } else {
            Err(Error::new_spanned(lit, "Expected usize literal"))
        }
    }

    /// Converts `expr` to a [`usize`].
    ///
    /// [`Err`] indicates `expr` is unable to be converted.
    fn usize_try_from_expr(expr: Expr) -> ParseResult<usize> {
        if let Expr::Lit(literal) = expr {
            Self::usize_try_from_lit(&literal.lit)
        } else {
            Err(Error::new_spanned(expr, "Expected literal"))
        }
    }
}

impl TryFrom<Expr> for ScentRepeater {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Lit(literal) => literal.lit.try_into(),
            Expr::Range(range) => range.try_into(),
            Expr::Try(..)
            | Expr::Repeat(..)
            | Expr::Path(..)
            | Expr::Binary(..)
            | Expr::Paren(..)
            | Expr::Unary(..)
            | Expr::Box(..)
            | Expr::Await(..)
            | Expr::Array(..)
            | Expr::Call(..)
            | Expr::MethodCall(..)
            | Expr::Tuple(..)
            | Expr::Cast(..)
            | Expr::Let(..)
            | Expr::If(..)
            | Expr::While(..)
            | Expr::ForLoop(..)
            | Expr::Loop(..)
            | Expr::Match(..)
            | Expr::Closure(..)
            | Expr::Unsafe(..)
            | Expr::Block(..)
            | Expr::Assign(..)
            | Expr::AssignOp(..)
            | Expr::Field(..)
            | Expr::Index(..)
            | Expr::Reference(..)
            | Expr::Break(..)
            | Expr::Continue(..)
            | Expr::Return(..)
            | Expr::Macro(..)
            | Expr::Struct(..)
            | Expr::Group(..)
            | Expr::Async(..)
            | Expr::TryBlock(..)
            | Expr::Type(..)
            | Expr::Yield(..)
            | Expr::Verbatim(..)
            | Expr::__Nonexhaustive => Err(Error::new_spanned(value, "Expected literal or range")),
        }
    }
}

impl TryFrom<ExprRange> for ScentRepeater {
    type Error = Error;

    fn try_from(value: ExprRange) -> Result<Self, Self::Error> {
        Ok(Self {
            minimum: value
                .clone()
                .from
                .map_or(Ok(0), |from| Self::usize_try_from_expr(*from))?,
            maximum: value.clone().to.map_or(Ok(usize::max_value()), |to| {
                Self::usize_try_from_expr(*to).map(|max| {
                    max.saturating_sub(if let RangeLimits::HalfOpen(..) = value.limits {
                        1
                    } else {
                        0
                    })
                })
            })?,
        })
    }
}

impl TryFrom<Lit> for ScentRepeater {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        Self::usize_try_from_lit(&value).map(|minimum| Self {
            minimum,
            maximum: 0,
        })
    }
}
