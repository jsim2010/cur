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

use alloc::{boxed::Box, string::ToString, vec, vec::Vec};
use core::convert::{TryFrom, TryInto};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Result as ParseResult},
    parse_macro_input, Ident, ItemConst, Type,
};
use syn::{
    BinOp, Error, Expr, ExprBinary, ExprIndex, ExprPath, ExprRange, ExprTry, ExprType, Lit, Path,
    RangeLimits, Visibility,
};

/// Converts `item` into a [`Game`].
///
/// Creating [`Game`]s can quickly become complex and error-prone. It is intended that a user
/// can use this procedural macro to build a [`Game`] using valid rust syntax that is easily understandable.
///
/// Additionally, `game` removes any unnecessary complexity in a created [`Game`], such as converting a [`Sequence`] with one element to just that element.
///
/// # Example(s)
/// ```
/// use cur::{game, Game, Scent};
///
/// #[game]
/// const HELLO_WORLD: Game = "Hello world!";
/// ```
#[proc_macro_attribute]
pub fn game(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let Input { vis, ident, expr } = parse_macro_input!(item as Input);

    TokenStream::from(quote! {
        #vis const #ident: Game = #expr;
    })
}

/// Specifies input used to create a [`Game`] definition.
struct Input {
    /// Visibility of the [`Game`].
    vis: Visibility,
    /// Identifier of the [`Game`].
    ident: Ident,
    /// Expression of the [`Game`].
    expr: GameExpr,
}

impl Parse for Input {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        // Parses input as an ItemConst and then parses the Expr to a GameExpr.
        input.parse().and_then(|item: ItemConst| {
            Ok(Self {
                vis: item.vis,
                ident: item.ident,
                expr: (*item.expr).try_into()?,
            })
        })
    }
}

/// Maps to [`Scent`] expressions.
#[derive(Clone, Debug)]
enum ScentExpr {
    /// Maps to [`Scent::Char`].
    Char(char),
    /// Maps to [`Scent::Range`].
    Range(char, char),
}

impl ToTokens for ScentExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Char(c) => {
                tokens.extend(quote! {
                    Scent::Char(#c)
                });
            }
            Self::Range(start, end) => {
                tokens.extend(quote! {
                    Scent::Range(#start, #end)
                });
            }
        }
    }
}

/// Maps to [`Game`] expressions.
///
/// Using `GameExpr` instead of [`Game`] because adding [`cur`] as a dependency would be circular.
#[derive(Clone, Debug)]
enum GameExpr {
    /// Maps to [`Game::Single`].
    Single(ScentExpr),
    /// Maps to [`Game::Union`].
    ///
    /// Each `GameExpr` in the given [`Vec`] should not be a `Union`.
    Union(Vec<GameExpr>),
    /// Maps to [`Game::Sequence`].
    ///
    /// Each `GameExpr` in the given [`Vec`] should not be a `Sequence`.
    Sequence(Vec<GameExpr>),
    /// Maps to [`Game::Repetition`].
    Repetition(Box<GameExpr>),
    /// Maps to [`Game::Item`].
    Item(Box<str>, Box<GameExpr>),
    /// Maps to the [`Path`] of a [`Game`]
    Path(Path),
}

impl GameExpr {
    /// Creates the simplest `GameExpr` that describes a sequence of `elements`.
    ///
    /// Assumes that each `GameExpr` of `elements` is not a [`Sequence`].
    fn sequence(mut elements: Vec<Self>) -> Self {
        if elements.len() == 1 {
            elements
                .pop()
                .expect("popping element from a `Vec` with a length of 1")
        } else {
            Self::Sequence(elements)
        }
    }

    /// Creates an empty [`Sequence`].
    ///
    /// Use this instead of `sequence` when it is known that the sequence will be empty.
    #[allow(clippy::missing_const_for_fn)] // Issue with lint; Vec::new() is not yet stable as a const fn.
    fn empty() -> Self {
        Self::Sequence(Vec::new())
    }

    /// Creates a `GameExpr` that repeats `self` as specified by `repeater`.
    fn repeat(self, repeater: &GameRepeater) -> Self {
        let mut elements = Vec::new();

        for _ in 0..repeater.minimum {
            elements.extend(self.clone().into_elements());
        }

        if repeater.maximum == usize::max_value() {
            elements.push(Self::Repetition(Box::new(self)));
        } else {
            for _ in repeater.minimum..repeater.maximum {
                elements.push(self.clone().optional());
            }
        }

        Self::sequence(elements)
    }

    /// Creates a `GameExpr` that can be either empty or `self`.
    fn optional(self) -> Self {
        let mut branches = vec![Self::empty()];

        branches.extend(self.into_branches());
        Self::Union(branches)
    }

    /// Creates a `GameExpr::Item` from `id` and `self`.
    fn name(self, id: Box<str>) -> Self {
        Self::Item(id, Box::new(self))
    }

    /// Creates a [`Vec`] of all branches from `self`.
    fn into_branches(self) -> Vec<Self> {
        if let Self::Union(branches) = self {
            branches
        } else {
            vec![self]
        }
    }

    /// Converts `self` into a sequence of `GameExpr`s.
    fn into_elements(self) -> Vec<Self> {
        if let Self::Sequence(elements) = self {
            elements
        } else {
            vec![self]
        }
    }
}

impl ToTokens for GameExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Single(scent) => {
                tokens.extend(quote! {
                    Game::Single(#scent)
                });
            }
            Self::Sequence(elements) => {
                tokens.extend(quote! {
                    Game::Sequence(&[#(#elements),*])
                });
            }
            Self::Union(branches) => {
                tokens.extend(quote! {
                    Game::Union(&[#(#branches),*])
                });
            }
            Self::Repetition(game) => {
                tokens.extend(quote! {
                    Game::Repetition(&#game)
                });
            }
            Self::Item(id, game) => {
                tokens.extend(quote! {
                    Game::Item(#id, &#game)
                });
            }
            Self::Path(path) => {
                path.to_tokens(tokens);
            }
        }
    }
}

impl TryFrom<Expr> for GameExpr {
    type Error = Error;

    fn try_from(value: Expr) -> Result<Self, Self::Error> {
        match value {
            Expr::Binary(binary) => binary.try_into(),
            Expr::Index(index) => index.try_into(),
            Expr::Path(path) => path.try_into(),
            Expr::Lit(literal) => literal.lit.try_into(),
            Expr::Paren(paren) => (*paren.expr).try_into(),
            Expr::Try(try_expr) => try_expr.try_into(),
            Expr::Range(range) => range.try_into(),
            Expr::Type(type_expr) => type_expr.try_into(),
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
            | Expr::Yield(..)
            | Expr::Verbatim(..)
            | Expr::__Nonexhaustive => Err(Error::new_spanned(
                value,
                "expression cannot be converted into `Game`",
            )),
        }
    }
}

impl TryFrom<ExprBinary> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprBinary) -> Result<Self, Self::Error> {
        let lhs: Self = (*value.left).try_into()?;
        let rhs: Self = (*value.right).try_into()?;

        match value.op {
            BinOp::BitOr(..) => {
                let mut branches = lhs.into_branches();
                branches.append(&mut rhs.into_branches());
                Ok(Self::Union(branches))
            }
            BinOp::Add(..) => {
                let mut elements = lhs.into_elements();
                elements.append(&mut rhs.into_elements());
                Ok(Self::sequence(elements))
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
            | BinOp::ShrEq(..) => Err(Error::new_spanned(
                value.op,
                "invalid binary operation; expected `|` or `+`",
            )),
        }
    }
}

impl TryFrom<ExprIndex> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprIndex) -> Result<Self, Self::Error> {
        let repeater = GameRepeater::try_from(*value.index)?;
        (*value.expr)
            .try_into()
            .map(|game: Self| game.repeat(&repeater))
    }
}

impl TryFrom<ExprPath> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprPath) -> Result<Self, Self::Error> {
        if let Some(ident) = value.path.get_ident() {
            if ident.to_string().as_str() == "None" {
                return Ok(Self::empty());
            }
        }

        // Assume path is valid.
        Ok(Self::Path(value.path))
    }
}

impl TryFrom<ExprRange> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprRange) -> Result<Self, Self::Error> {
        Ok(Self::Single(ScentExpr::Range(
            value
                .from
                .map_or(Ok('\u{0}'), |from| char_try_from_expr(*from))?,
            value
                .to
                .map_or(Ok('\u{10ffff}'), |to| char_try_from_expr(*to))?,
        )))
    }
}

impl TryFrom<ExprTry> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprTry) -> Result<Self, Self::Error> {
        Self::try_from(*value.expr).map(Self::optional)
    }
}

impl TryFrom<ExprType> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprType) -> Result<Self, Self::Error> {
        let id = match *value.ty.clone() {
            Type::Path(t) => t.path.get_ident().map_or(
                Err(Error::new_spanned(value.ty, "expected single ident")),
                |ident| Ok(ident.to_string().into_boxed_str()),
            ),
            Type::Array(..)
            | Type::BareFn(..)
            | Type::Group(..)
            | Type::ImplTrait(..)
            | Type::Infer(..)
            | Type::Macro(..)
            | Type::Never(..)
            | Type::Paren(..)
            | Type::Ptr(..)
            | Type::Reference(..)
            | Type::Slice(..)
            | Type::TraitObject(..)
            | Type::Tuple(..)
            | Type::Verbatim(..)
            | Type::__Nonexhaustive => Err(Error::new_spanned(value.ty, "expected path")),
        }?;
        Self::try_from(*value.expr).map(|game| game.name(id))
    }
}

impl TryFrom<Lit> for GameExpr {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Char(c) => Ok(Self::Single(ScentExpr::Char(c.value()))),
            Lit::Str(s) => Ok(Self::sequence(s.value().chars().map(|c| Self::Single(ScentExpr::Char(c))).collect())),
            Lit::ByteStr(..)
            | Lit::Byte(..)
            | Lit::Int(..)
            | Lit::Float(..)
            | Lit::Bool(..)
            | Lit::Verbatim(..) => Err(Error::new_spanned(
                value,
                "expected character or string literal",
            )),
        }
    }
}

/// Signifies the number of times a [`GameExpr`] can be repeated.
#[derive(Debug)]
struct GameRepeater {
    /// The smallest number of repeats.
    minimum: usize,
    /// The largest number of repeats.
    ///
    /// A number <= `minimum` indicates the [`GameExpr`] must be repeated exactly
    /// `minimum` times.
    maximum: usize,
}

impl TryFrom<Expr> for GameRepeater {
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
            | Expr::__Nonexhaustive => Err(Error::new_spanned(value, "expected literal or range")),
        }
    }
}

impl TryFrom<ExprRange> for GameRepeater {
    type Error = Error;

    fn try_from(value: ExprRange) -> Result<Self, Self::Error> {
        Ok(Self {
            minimum: value
                .clone()
                .from
                .map_or(Ok(0), |from| usize_try_from_expr(*from))?,
            maximum: value.clone().to.map_or(Ok(usize::max_value()), |to| {
                usize_try_from_expr(*to).map(|max| {
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

impl TryFrom<Lit> for GameRepeater {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        usize_try_from_lit(&value).map(|minimum| Self {
            minimum,
            maximum: 0,
        })
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
        usize_try_from_lit(&literal.lit)
    } else {
        Err(Error::new_spanned(expr, "Expected literal"))
    }
}
