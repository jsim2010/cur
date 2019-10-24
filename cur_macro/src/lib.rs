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
/// Creating [`Game`]s can quickly become complex and error-prone. It is intended that a user can
/// use this procedural macro to build a [`Game`] using valid rust syntax that is easily
/// understandable.
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
        // Parse input as an ItemConst and then parse the Expr of the ItemConst to a GameExpr.
        input.parse().and_then(|item: ItemConst| {
            Ok(Self {
                vis: item.vis,
                ident: item.ident,
                expr: (*item.expr).try_into()?,
            })
        })
    }
}

/// Maps to [`Game`] expressions.
#[derive(Clone, Debug)]
enum GameExpr {
    /// Maps to [`Game::Sequence`].
    ///
    /// Guarantees that each element in the sequence is not a `Sequence`.
    Sequence(Concatenation),
    /// Maps to [`Game::Union`].
    ///
    /// Guarantees that each element in the union is not a `Union`.
    Union(Alternation),
    /// Maps to any singular [`Game`].
    Singular(SingularGameExpr),
}

impl GameExpr {
    /// Creates a `GameExpr` that repeats `self` as specified by `repeater`.
    fn repeat(self, repeater: &GameRepeater) -> Self {
        let mut concatenation = Vec::new();

        for _ in 0..repeater.minimum {
            concatenation.extend(Concatenation::from(self.clone()));
        }

        if repeater.maximum == usize::max_value() {
            concatenation.push(Step::repetition(self));
        } else {
            for _ in repeater.minimum..repeater.maximum {
                concatenation.push(Step::optional(self.clone()));
            }
        }

        concatenation.into()
    }

    /// Creates a `SingularGameExpr::Item` from `id` and `self`.
    fn name(self, id: Box<str>) -> Self {
        Self::Singular(SingularGameExpr::Item(id, Box::new(self)))
    }
}

impl From<Branch> for GameExpr {
    fn from(value: Branch) -> Self {
        match value {
            Branch::Sequence(concatenation) => Self::Sequence(concatenation),
            Branch::Singular(game) => Self::Singular(game),
        }
    }
}

impl From<char> for GameExpr {
    fn from(value: char) -> Self {
        ScentExpr::from(value).into()
    }
}

impl From<Concatenation> for GameExpr {
    /// Ensures that created GameExpr is as simple as possible.
    fn from(mut value: Concatenation) -> Self {
        if value.len() == 1 {
            value
                .pop()
                .expect("popping element from a `Vec` with a length of 1")
                // Convert Step into GameExpr.
                .into()
        } else {
            Self::Sequence(value)
        }
    }
}

impl From<ScentExpr> for GameExpr {
    fn from(value: ScentExpr) -> Self {
        Self::Singular(value.into())
    }
}

impl From<Step> for GameExpr {
    fn from(value: Step) -> Self {
        match value {
            Step::Union(alternation) => Self::Union(alternation),
            Step::Singular(game) => Self::Singular(game),
        }
    }
}

impl ToTokens for GameExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Sequence(concatenation) => {
                quote! {
                    Game::Sequence(&[#(#concatenation),*])
                }
            }
            Self::Union(alternation) => {
                quote! {
                    Game::Union(&[#(#alternation),*])
                }
            }
            Self::Singular(game) => {
                quote! {
                    #game
                }
            }
        });
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
                let mut alternation: Alternation = lhs.into();
                alternation.append(&mut rhs.into());
                Ok(Self::Union(alternation))
            }
            BinOp::Add(..) => {
                let mut concatenation: Concatenation = lhs.into();
                concatenation.append(&mut rhs.into());
                Ok(concatenation.into())
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
                return Ok(Branch::empty().into());
            }
        }

        // Assume path is valid.
        Ok(Self::Singular(SingularGameExpr::Path(value.path)))
    }
}

impl TryFrom<ExprRange> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprRange) -> Result<Self, Self::Error> {
        Ok(ScentExpr::Range(
            value
                .from
                .map_or(Ok('\u{0}'), |from| char_try_from_expr(*from))?,
            value
                .to
                .map_or(Ok('\u{10ffff}'), |to| char_try_from_expr(*to))?,
        )
        .into())
    }
}

impl TryFrom<ExprTry> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprTry) -> Result<Self, Self::Error> {
        Self::try_from(*value.expr).map(|game| Step::optional(game).into())
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
            Lit::Char(c) => Ok(c.value().into()),
            Lit::Str(s) => Ok(s
                .value()
                .chars()
                .map(|c| c.into())
                .collect::<Concatenation>()
                .into()),
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

/// The component of a [`Game::Union`]; i.e. a [`Vec`] of [`Branch`]es;
type Alternation = Vec<Branch>;

impl From<GameExpr> for Alternation {
    fn from(value: GameExpr) -> Self {
        match value {
            GameExpr::Union(alternation) => alternation,
            GameExpr::Singular(game) => vec![game.into()],
            GameExpr::Sequence(elements) => vec![Branch::Sequence(elements)],
        }
    }
}

/// The component of a [`Game::Sequence`]; i.e. a [`Vec`] of [`Step`]s;
type Concatenation = Vec<Step>;

impl From<GameExpr> for Concatenation {
    fn from(value: GameExpr) -> Self {
        match value {
            GameExpr::Sequence(elements) => elements,
            GameExpr::Union(alternation) => vec![Step::Union(alternation)],
            GameExpr::Singular(game) => vec![Step::Singular(game)],
        }
    }
}

/// Maps to [`Game`] expressions other than [`Game::Sequence`].
#[derive(Clone, Debug)]
enum Step {
    /// Maps to [`Game`] expressions that are singular.
    Singular(SingularGameExpr),
    /// Maps to [`Game::Union`].
    ///
    /// Each `GameExpr` in the given [`Vec`] shall not be a `Union`.
    Union(Vec<Branch>),
}

impl Step {
    /// Creates a `Step` that is a repetition of `game`.
    fn repetition(game: GameExpr) -> Self {
        SingularGameExpr::Repetition(Box::new(game)).into()
    }

    /// Creates a `Step` that can be either empty or `game`.
    fn optional(game: GameExpr) -> Self {
        let mut alternation = vec![Branch::empty()];

        alternation.extend(Alternation::from(game));
        // Because size of alternation is at least 2, it will always be a Union.
        Self::Union(alternation)
    }
}

impl From<char> for Step {
    fn from(value: char) -> Self {
        ScentExpr::from(value).into()
    }
}

impl From<ScentExpr> for Step {
    fn from(value: ScentExpr) -> Self {
        SingularGameExpr::Scent(value).into()
    }
}

impl From<SingularGameExpr> for Step {
    fn from(value: SingularGameExpr) -> Self {
        Self::Singular(value)
    }
}

impl ToTokens for Step {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Singular(game) => {
                quote! {
                    #game
                }
            }
            Self::Union(branches) => {
                quote! {
                    Game::Union(&[#(#branches),*])
                }
            }
        });
    }
}

/// Maps with [`Game`] expressions that can be part of a [`Game::Union`].
#[derive(Clone, Debug)]
enum Branch {
    /// Maps with [`Game`]s that are singular.
    Singular(SingularGameExpr),
    /// Maps with [`Game::Sequence`].
    Sequence(Concatenation),
}

impl Branch {
    /// Creates a new `Branch` that matches the null set.
    #[allow(clippy::missing_const_for_fn)] // Lint incorrectly thinks Vec::new() is stable as a const fn.
    fn empty() -> Self {
        Self::Sequence(Vec::new())
    }
}

impl ToTokens for Branch {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Singular(game) => {
                quote! {
                    #game
                }
            }
            Self::Sequence(elements) => {
                quote! {
                    Game::Sequence(&[#(#elements),*])
                }
            }
        });
    }
}

impl From<SingularGameExpr> for Branch {
    fn from(value: SingularGameExpr) -> Self {
        Self::Singular(value)
    }
}

/// Maps to [`Game`] expressions that refer to a singular [`Game`].
#[derive(Clone, Debug)]
enum SingularGameExpr {
    /// Maps to [`Game::Single`].
    Scent(ScentExpr),
    /// Maps to [`Game::Repetition`].
    Repetition(Box<GameExpr>),
    /// Maps to [`Game::Item`].
    Item(Box<str>, Box<GameExpr>),
    /// Maps to the [`Path`] of a [`Game`]
    Path(Path),
}

impl From<ScentExpr> for SingularGameExpr {
    fn from(value: ScentExpr) -> Self {
        Self::Scent(value)
    }
}

impl ToTokens for SingularGameExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Scent(scent) => {
                quote! {
                    Game::Single(#scent)
                }
            }
            Self::Repetition(game) => {
                quote! {
                    Game::Repetition(&#game)
                }
            }
            Self::Item(id, game) => {
                quote! {
                    Game::Item(#id, &#game)
                }
            }
            Self::Path(path) => {
                quote! {
                    #path
                }
            }
        });
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

// Cannot use cur::Scent because adding cur as a dependency would be circular.
/// Maps to [`Scent`] expressions.
#[derive(Clone, Debug)]
enum ScentExpr {
    /// Maps to [`Scent::Char`].
    Char(char),
    /// Maps to [`Scent::Range`].
    Range(char, char),
}

impl From<char> for ScentExpr {
    fn from(value: char) -> Self {
        Self::Char(value)
    }
}

impl ToTokens for ScentExpr {
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
