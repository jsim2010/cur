//! `cur_macro` - Implements procedural macros for [`cur`]
#![no_std]

extern crate alloc;

use {
    alloc::{string::ToString, vec::Vec},
    core::convert::{TryFrom, TryInto},
    proc_macro::TokenStream,
    proc_macro2::TokenStream as TokenStream2,
    quote::{quote, ToTokens},
    syn::{
        parse::{Parse, ParseStream, Result as ParseResult},
        parse_macro_input, Error, Expr, ExprAssign, ExprRange,
        Lit, RangeLimits, Type, Visibility, ExprType,
    },
};

/// The first scalar value after Unicode surrogates.
const FIRST_CHAR_AFTER_SURROGATES_VALUE: u32 = 0xe000;
/// The last `char` before Unicode surrogates.
const LAST_CHAR_BEFORE_SURROGATES: char = '\u{d7ff}';

/// Converts `input` into a [`Game`].
///
/// Creating [`Game`]s can quickly become complex and error-prone. It is intended that a user can
/// use this procedural macro to build a [`Game`] using valid rust syntax that is easily
/// understandable.
///
/// Calling this procedural macro will create a constant with the given visibility and name. The type of the constant shall be a `Lazy<Game>`.
///
/// # Example(s)
/// ```
/// use cur::*;
///
/// game!(HELLO_WORLD = "Hello world!");
/// ```
#[proc_macro]
#[inline]
pub fn game(input: TokenStream) -> TokenStream {
    let game_def = parse_macro_input!(input as GameDef);

    TokenStream::from(quote! {
        #game_def
    })
}

/// Specifies the data that makes up the definition of a [`Game`].
struct GameDef {
    /// The visibility of the [`Game`].
    vis: Visibility,
    /// Identifier of the [`Game`].
    name: Expr,
    /// Expression of the [`Game`].
    expr: TokenStream2,
}

impl Parse for GameDef {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        // Format of `input` is '<Visiblity> <ExprAssign>'.
        let vis = input.parse()?;
        input.parse().and_then(|assign_expr: ExprAssign| {
            Ok(Self {
                vis,
                name: *assign_expr.left,
                expr: gamify(*assign_expr.right)?,
            })
        })
    }
}

impl ToTokens for GameDef {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let vis = &self.vis;
        let name = &self.name;
        let expr = &self.expr;

        tokens.extend(quote! {
            // Use Lazy to allow lazy evaluation of the const expression since + and | are not const.
            #vis const #name: Lazy<Game> = Lazy::new(|| #expr);
        });
    }
}

/// Quotes `expr` as a `Game`.
fn gamify(expr: Expr) -> Result<TokenStream2, Error> {
    let mut tokens = TokenStream2::new();

    match expr {
        Expr::Lit(lit) => tokens.extend(gamify_lit(lit.lit)?),
        Expr::Path(path) => {
            if let Some(ident) = path.path.get_ident() {
                if ident.to_string().as_str() == "None" {
                    tokens.extend(quote!{
                        Game::Sequence(vec![])
                    });
                } else {
                    tokens.extend(quote!{
                        #path.clone()
                    });
                }
            } else {
                tokens.extend(quote!{
                    #path.clone()
                });
            }
        }
        Expr::Paren(paren) => {
            let inner_expr = gamify(*paren.expr)?;
            tokens.extend(quote!{
                (#inner_expr)
            });
        }
        Expr::Try(try_expr) => {
            let optional_expr = gamify(*try_expr.expr)?;
            tokens.extend(quote!{
                (Game::Sequence(vec![])|#optional_expr)
            });
        }
        Expr::Binary(binary) => {
            tokens.extend(gamify(*binary.left)?);
            binary.op.to_tokens(&mut tokens);
            tokens.extend(gamify(*binary.right)?);
        }
        Expr::Range(range) => {
            let end = if let Some(to) = range.to {
                let to_char = char_try_from_expr(*to.clone())?;

                if let RangeLimits::HalfOpen(..) = range.limits {
                    let end_code = u32::from(to_char);

                    // Because end_code came from a char, FIRST_CHAR_AFTER_SURROGATES_VALUE is the only possible value that requires special handling.
                    if end_code == FIRST_CHAR_AFTER_SURROGATES_VALUE {
                        LAST_CHAR_BEFORE_SURROGATES
                    } else {
                        char::try_from(end_code.checked_sub(1).ok_or_else(|| {
                            Error::new_spanned(to.clone(), "End bound cannot be exclusive 0")
                        })?)
                        .map_err(|_| Error::new_spanned(to, "Invalid value for exclusive range"))?
                    }
                } else {
                    to_char
                }
            } else {
                '\u{10ffff}'
            };

            let start = range.from.map_or(Ok('\u{0}'), |from| char_try_from_expr(*from))?;

            tokens.extend(quote!{Game::Single(Scent::Range(#start, #end))});
        }
        Expr::Type(ty) => tokens.extend(gamify_type(ty)?),
        Expr::Repeat(repeat) => {
            let quantifier = Quantifier::try_from(*repeat.len)?;
            let game = gamify(*repeat.expr)?;

            let mut games = Vec::new();

            for _ in 0..quantifier.minimum {
                games.push(game.clone());
            }

            if quantifier.maximum == usize::max_value() {
                games.push(quote!{Game::Repetition(Pattern::from(#game))});
            } else {
                for _ in quantifier.minimum..quantifier.maximum {
                    games.push(quote!{Game::Sequence(vec![])|#game});
                }
            }

            tokens.extend(quote_sequence(&games));
        }
        Expr::Array(array) => {
            let mut elements = Vec::new();

            for element in array.elems.iter() {
                elements.push(gamify(element.clone())?);
            }

            tokens.extend(quote_sequence(&elements));
        }
        Expr::Unary(..)
        | Expr::Index(..)
        | Expr::Box(..)
        | Expr::Await(..)
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
        | Expr::__Nonexhaustive => {
            return Err(Error::new_spanned(
                expr,
                "expression cannot be converted into `Game`",
            ));
        }
    }

    Ok(tokens)
}

/// Quotes `lit` as a `Game`.
fn gamify_lit(lit: Lit) -> Result<TokenStream2, Error> {
    let mut games = Vec::new();

    match lit {
        Lit::Char(c) => {
            games.push(quote!{
                Game::from(#c)
            });
        }
        Lit::Byte(byte) => {
            games.push(quote!{
                Game::from(#byte)
            });
        }
        Lit::Str(s) => {
            for c in s.value().chars() {
                games.push(quote!{
                    Game::from(#c)
                });
            }
        }
        Lit::ByteStr(byte_string) => {
            for byte in byte_string.value() {
                games.push(quote!{
                    Game::from(#byte)
                })
            }
        }
        Lit::Int(..)
        | Lit::Float(..)
        | Lit::Bool(..)
        | Lit::Verbatim(..) => {
            return Err(Error::new_spanned(
                lit,
                "expected a byte, byte string, character or string literal",
            ));
        }
    }

    Ok(quote_sequence(&games))
}

/// Quotes `ty` as a `Game`.
fn gamify_type(ty: ExprType) -> Result<TokenStream2, Error> {
    let id = match *ty.ty.clone() {
        Type::Path(t) => t.path.get_ident().map_or(
            Err(Error::new_spanned(ty.ty, "expected single ident")),
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
        | Type::__Nonexhaustive => Err(Error::new_spanned(ty.ty, "expected path")),
    }?;
    let game = gamify(*ty.expr)?;

    Ok(quote!{Game::Item(#id, Box::new(#game))})
}

/// Quotes `games` as a `Game::Sequence`.
fn quote_sequence(games: &[TokenStream2]) -> TokenStream2 {
    #[allow(clippy::integer_arithmetic)] // False positive.
    if games.is_empty() {
        // Output an empty sequence. Required because concat() cannot infer type from an empty Vec.
        // TODO: There might be a way to specify the type in concat such that this branch is not needed.
        quote!{
            Game::Sequence(vec![])
        }
    } else if games.len() == 1 {
        // Only output the single game.
        quote!{
            #((#games))*
        }
    } else {
        quote!{
            Game::Sequence(vec![#(Vec::<Step>::from(#games)),*].concat())
        }
    }
}

/// Signifies the number of times a game can be repeated.
#[derive(Debug)]
struct Quantifier {
    /// The smallest number of repeats.
    minimum: usize,
    /// The largest number of repeats.
    ///
    /// A number <= `minimum` indicates the game must be repeated exactly
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
