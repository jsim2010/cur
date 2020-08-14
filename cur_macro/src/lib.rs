//! `cur_macro` - Procedural macros for [`cur`]
#![no_std]

extern crate alloc;

use alloc::{boxed::Box, string::ToString, vec, vec::Vec};
use core::convert::{TryFrom, TryInto};
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Result as ParseResult},
    parse_macro_input, Type,
};
use syn::{
    BinOp, Error, Expr, ExprBinary, ExprAssign, ExprIndex, ExprPath, ExprRange, ExprTry, ExprType, Lit, Path,
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
/// use cur::prelude::*;
///
/// game!(HELLO_WORLD = "Hello world!");
/// ```
#[proc_macro]
pub fn game(input: TokenStream) -> TokenStream {
    let Input { vis, name, expr } = parse_macro_input!(input as Input);

    TokenStream::from(quote! {
        #vis const #name: Lazy<Game> = Lazy::new(|| #expr);
    })
}

/// Specifies input used to create a [`Game`] definition.
struct Input {
    vis: Visibility,
    /// Identifier of the [`Game`].
    name: Expr,
    /// Expression of the [`Game`].
    expr: GameExpr,
}

impl Parse for Input {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let vis = input.parse()?;
        input.parse().and_then(|assign_expr: ExprAssign| {
            Ok(Self {
                vis,
                name: *assign_expr.left,
                expr: (*assign_expr.right).try_into()?,
            })
        })
    }
}

/// Maps to [`Game`] expressions.
#[derive(Clone, Debug)]
enum GameExpr {
    Single(ScentExpr),
    Sequence(Vec<GameExpr>),
    Union(Vec<GameExpr>),
    Repetition(PatternExpr),
    Path(Path),
    Item(Box<str>, Box<GameExpr>),
}

impl GameExpr {
    fn branches(&self) -> Vec<Self> {
        match self.clone() {
            GameExpr::Sequence(steps) => vec![GameExpr::Sequence(steps)],
            GameExpr::Union(branches) => branches,
            GameExpr::Single(scent) => vec![GameExpr::Single(scent)],
            GameExpr::Repetition(pattern) => vec![GameExpr::Repetition(pattern)],
            GameExpr::Path(path) => vec![GameExpr::Path(path)],
            GameExpr::Item(name, g) => vec![GameExpr::Item(name, g)],
        }
    }
}

impl From<char> for GameExpr {
    fn from(value: char) -> Self {
        ScentExpr::from(value).into()
    }
}

impl From<ScentExpr> for GameExpr {
    fn from(value: ScentExpr) -> Self {
        Self::Single(value)
    }
}

impl ToTokens for GameExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Single(scent) => {
                quote! {
                    Game::Single(#scent)
                }
            }
            Self::Sequence(steps) => {
                if steps.len() == 0 {
                    quote! {
                        Game::Sequence(vec![])
                    }
                } else {
                    quote! {
                        (#(#steps)+*)
                    }
                }
            }
            Self::Union(alternation) => {
                quote! {
                    (#(#alternation)|*)
                }
            }
            Self::Item(name, game) => {
                quote! {
                    Game::Item(#name, &#game)
                }
            }
            Self::Repetition(pattern) => {
                quote! {
                    Game::Repetition(#pattern)
                }
            }
            Self::Path(path) => {
                quote! {
                    #path.clone()
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
        let games = vec![(*value.left).try_into()?, (*value.right).try_into()?];

        match value.op {
            BinOp::BitOr(..) => Ok(Self::Union(games)),
            BinOp::Add(..) => Ok(Self::Sequence(games)),
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
        Ok(PatternExpr::from(GameExpr::try_from(*value.expr)?).repeat(&repeater))
    }
}

impl TryFrom<ExprPath> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprPath) -> Result<Self, Self::Error> {
        if let Some(ident) = value.path.get_ident() {
            if ident.to_string().as_str() == "None" {
                return Ok(Self::Sequence(vec![]));
            }
        }

        // Assume path is valid.
        Ok(Self::Path(value.path))
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
        let optional_game = Self::try_from(*value.expr)?;
        let mut branches = vec![GameExpr::Sequence(vec![])];
        branches.append(&mut optional_game.branches());
        Ok(Self::Union(branches))
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
        Self::try_from(*value.expr).map(|game| GameExpr::Item(id, Box::new(game)))
    }
}

impl TryFrom<Lit> for GameExpr {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Char(c) => Ok(c.value().into()),
            Lit::Str(s) => Ok(GameExpr::Sequence(s
                .value()
                .chars()
                .map(|c| c.into())
                .collect::<Vec<GameExpr>>())),
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

#[derive(Clone, Debug)]
enum PatternExpr {
    Single(ScentExpr),
    Sequence(Vec<GameExpr>),
    Union(Vec<GameExpr>),
    Path(Path),
    Item(Box<str>, Box<GameExpr>),
}

impl PatternExpr {
    fn branches(&self) -> Vec<GameExpr> {
        match self.clone() {
            PatternExpr::Sequence(steps) => vec![GameExpr::Sequence(steps)],
            PatternExpr::Union(branches) => branches,
            PatternExpr::Single(scent) => vec![GameExpr::Single(scent)],
            PatternExpr::Path(path) => vec![GameExpr::Path(path)],
            PatternExpr::Item(name, g) => vec![GameExpr::Item(name, g)],
        }
    }

    fn steps(&self) -> Vec<GameExpr> {
        match self.clone() {
            PatternExpr::Single(scent) => vec![GameExpr::Single(scent)],
            PatternExpr::Sequence(steps) => steps,
            PatternExpr::Union(branches) => vec![GameExpr::Union(branches)],
            PatternExpr::Path(path) => vec![GameExpr::Path(path)],
            PatternExpr::Item(name, game) => vec![GameExpr::Item(name, game)],
        }
    }

    fn repeat(self, repeater: &GameRepeater) -> GameExpr {
        let mut concatenation = Vec::new();

        for _ in 0..repeater.minimum {
            concatenation.extend(self.steps());
        }

        if repeater.maximum == usize::max_value() {
            concatenation.push(GameExpr::Repetition(self.clone().into()));
        } else {
            for _ in repeater.minimum..repeater.maximum {
                let mut branches = vec![GameExpr::Sequence(vec![])];
                branches.append(&mut self.branches());
                concatenation.push(GameExpr::Union(branches))
            }
        }

        GameExpr::Sequence(concatenation)
    }
}

impl From<GameExpr> for PatternExpr {
    fn from(game: GameExpr) -> Self {
        match game {
            GameExpr::Single(scent) => Self::Single(scent),
            GameExpr::Repetition(pattern) => pattern,
            GameExpr::Sequence(steps) => Self::Sequence(steps),
            GameExpr::Union(branches) => Self::Union(branches),
            GameExpr::Path(path) => Self::Path(path),
            GameExpr::Item(name, g) => Self::Item(name, g),
        }
    }
}

impl ToTokens for PatternExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Single(scent) => {
                quote! {
                    Pattern::Single(#scent)
                }
            }
            Self::Sequence(steps) => {
                if steps.len() == 0 {
                    quote! {
                        Pattern::Sequence(vec![])
                    }
                } else {
                    quote! {
                        Pattern::Sequence(vec![#(Step::from(#steps)),*])
                    }
                }
            }
            Self::Union(branches) => {
                quote! {
                    Pattern::Union(vec![#(#branches),*])
                }
            }
            Self::Path(path) => {
                quote! {
                    Pattern::from(#path.clone())
                }
            }
            Self::Item(name, game) => {
                quote! {
                    Pattern::Item(#name, #game)
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
