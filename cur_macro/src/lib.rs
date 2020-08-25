//! `cur_macro` - Procedural macros for [`cur`]
#![no_std]

extern crate alloc;

use {
    alloc::{boxed::Box, string::ToString, vec::Vec},
    core::convert::{TryFrom, TryInto},
    cur_internal::Scent,
    proc_macro::TokenStream,
    proc_macro2::TokenStream as TokenStream2,
    quote::{quote, ToTokens},
    syn::{
        parse::{Parse, ParseStream, Result as ParseResult},
        parse_macro_input, Type,
        BinOp, Error, Expr, ExprAssign, ExprBinary, ExprPath, ExprRange, ExprRepeat, ExprTry, ExprType,
        Lit, Path, RangeLimits, Visibility,
    },
};

const FIRST_CHAR_VALUE_AFTER_SURROGATES: u32 = 0xe000;
const LAST_CHAR_BEFORE_SURROGATES: char = '\u{d7ff}';

/// Converts `item` into a [`Game`].
///
/// Creating [`Game`]s can quickly become complex and error-prone. It is intended that a user can
/// use this procedural macro to build a [`Game`] using valid rust syntax that is easily
/// understandable.
///
/// # Example(s)
/// ```
/// use cur::*;
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
    /// The visibility of the `Game`.
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

#[derive(Clone, Debug)]
enum GameOp {
    Or,
    And,
}

impl ToTokens for GameOp {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Or => {
                quote! {|}
            }
            Self::And => {
                quote! {+}
            }
        });
    }
}

/// Maps to [`Game`] expressions.
#[derive(Clone, Debug)]
enum GameExpr {
    Empty,
    Single(Scent),
    Binary {
        op: GameOp,
        left: Box<GameExpr>,
        right: Box<GameExpr>,
    },
    Sequence(Vec<GameExpr>),
    Repetition(Box<GameExpr>),
    Path(Path),
    Item(Box<str>, Box<GameExpr>),
}

impl GameExpr {
    fn option(self) -> Self {
        Self::Binary {
            left: Box::new(Self::Empty),
            right: Box::new(self),
            op: GameOp::Or,
        }
    }

    /// Repeats `self` as specified by `quantifier`.
    fn repeat(self, quantifier: &Quantifier) -> GameExpr {
        let mut concatenation = Vec::new();

        for _ in 0..quantifier.minimum {
            concatenation.push(self.clone());
        }

        if quantifier.maximum == usize::max_value() {
            concatenation.push(GameExpr::Repetition(Box::new(self)));
        } else {
            for _ in quantifier.minimum..quantifier.maximum {
                concatenation.push(self.clone().option());
            }
        }

        GameExpr::Sequence(concatenation)
    }
}

impl From<char> for GameExpr {
    fn from(value: char) -> Self {
        Scent::from(value).into()
    }
}

impl From<ExprPath> for GameExpr {
    fn from(expr: ExprPath) -> Self {
        if let Some(ident) = expr.path.get_ident() {
            if ident.to_string().as_str() == "None" {
                return Self::Empty;
            }
        }

        // Assume path is valid.
        Self::Path(expr.path)
    }
}

impl From<Scent> for GameExpr {
    fn from(value: Scent) -> Self {
        Self::Single(value)
    }
}

impl ToTokens for GameExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Empty => {
                quote! {
                    Game::Sequence(vec![])
                }
            }
            Self::Single(scent) => {
                quote! {
                    Game::Single(#scent)
                }
            }
            #[allow(clippy::integer_arithmetic)] // Add impl is safe for Game.
            Self::Sequence(steps) => {
                if steps.is_empty() {
                    quote! {
                        Game::Sequence(vec![])
                    }
                } else {
                    quote! {
                        (#(#steps)+*)
                    }
                }
            }
            Self::Binary {left, right, op} => {
                quote! {
                    (#left #op #right)
                }
            }
            Self::Item(name, game) => {
                quote! {
                    Game::Item(#name, Box::new(#game))
                }
            }
            Self::Repetition(pattern) => {
                quote! {
                    #pattern.repeat()
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
            Expr::Path(path) => Ok(path.into()),
            Expr::Lit(literal) => literal.lit.try_into(),
            Expr::Paren(paren) => (*paren.expr).try_into(),
            Expr::Try(try_expr) => try_expr.try_into(),
            Expr::Range(range) => range.try_into(),
            Expr::Type(type_expr) => type_expr.try_into(),
            Expr::Repeat(repeat_expr) => repeat_expr.try_into(),
            Expr::Unary(..)
            | Expr::Index(..)
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
        match value.op {
            BinOp::BitOr(..) => Ok(GameOp::Or),
            BinOp::Add(..) => Ok(GameOp::And),
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
        }.and_then(|op| Ok(Self::Binary{left: Box::new((*value.left).try_into()?), right: Box::new((*value.right).try_into()?), op}))
    }
}

impl TryFrom<ExprRange> for GameExpr {
    type Error = Error;

    fn try_from(range: ExprRange) -> Result<Self, Self::Error> {
        let end = if let Some(to) = range.to {
            let to_char = char_try_from_expr(*to.clone())?;

            if let RangeLimits::HalfOpen(..) = range.limits {
                let end_code = u32::from(to_char);

                // Because end_code came from a char, FIRST_CHAR_VALUE_AFTER_SURROGATES is the only possible value that requires special handling.
                if end_code == FIRST_CHAR_VALUE_AFTER_SURROGATES {
                    LAST_CHAR_BEFORE_SURROGATES
                } else {
                    char::try_from(end_code.checked_sub(1).ok_or_else(|| Error::new_spanned(to.clone(), "End bound cannot be exclusive 0"))?)
                        .map_err(|_| Error::new_spanned(to, "Invalid value for exclusive range"))?
                }
            } else {
                to_char
            }
        } else {
            '\u{10ffff}'
        };

        Ok(Scent::Range(
            range
                .from
                .map_or(Ok('\u{0}'), |from| char_try_from_expr(*from))?,
            end,
        )
        .into())
    }
}

impl TryFrom<ExprRepeat> for GameExpr {
    type Error = Error;

    fn try_from(repeat: ExprRepeat) -> Result<Self, Self::Error> {
        let repeater = Quantifier::try_from(*repeat.len)?;
        Ok(Self::try_from(*repeat.expr)?.repeat(&repeater))
    }
}

impl TryFrom<ExprTry> for GameExpr {
    type Error = Error;

    fn try_from(value: ExprTry) -> Result<Self, Self::Error> {
        Self::try_from(*value.expr).map(Self::option)
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
        Self::try_from(*value.expr).map(|game| Self::Item(id, Box::new(game)))
    }
}

impl TryFrom<Lit> for GameExpr {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        match value {
            Lit::Char(c) => Ok(c.value().into()),
            Lit::Str(s) => Ok(Self::Sequence(
                s.value()
                    .chars()
                    .map(|c| c.into())
                    .collect::<Vec<Self>>(),
            )),
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
struct Quantifier {
    /// The smallest number of repeats.
    minimum: usize,
    /// The largest number of repeats.
    ///
    /// A number <= `minimum` indicates the [`GameExpr`] must be repeated exactly
    /// `minimum` times.
    maximum: usize,
}

impl TryFrom<Expr> for Quantifier {
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

impl TryFrom<ExprRange> for Quantifier {
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

impl TryFrom<Lit> for Quantifier {
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
