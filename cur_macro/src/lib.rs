//! `cur_macro` - Implements procedural macros for `cur`.
#![no_std]

extern crate alloc;

use {
    alloc::{string::ToString, vec, vec::Vec},
    core::{ops::Range, convert::{TryFrom, TryInto}},
    fehler::{throw, throws},
    proc_macro::TokenStream,
    proc_macro2::TokenStream as TokenStream2,
    quote::{quote, ToTokens},
    syn::{
        parse::{Parse, ParseStream, Result as ParseResult},
        parse_macro_input, Error, Expr, ExprAssign, ExprRange, ExprRepeat, ExprType, Lit,
        RangeLimits, Type, Visibility,
    },
};

/// The first `char` in Unicode.
const FIRST_CHAR: char = '\u{0}';
/// The last `char` in Unicode.
const LAST_CHAR: char = '\u{10ffff}';
/// The first scalar value after Unicode surrogates.
const FIRST_CHAR_AFTER_SURROGATES_VALUE: u32 = 0xe000;
/// The last `char` before Unicode surrogates.
const LAST_CHAR_BEFORE_SURROGATES: char = '\u{d7ff}';

// TODO: Write out the definition of a GameExpression.
/// Converts `input` into a `Game`.
///
/// Creating `Game`s can quickly become complex and error-prone. It is intended that a user can
/// use this procedural macro to build a `Game` using valid rust syntax that is easily
/// understandable.
///
/// Calling this procedural macro will create a constant with the given visibility and name. The type of the constant shall be a `Lazy<Game>`, which allows for operations that are not const to be evaluated after compilation.
///
/// The format of the macro input shall be `Visibility AssignmentExpression`. The value expression of the assignment expression shall be a game expression.
///
/// # Example(s)
/// ```
/// use cur::*;
///
/// game!(HELLO_WORLD = ["Hello", ' ', "world", '!']);
///
/// assert!(Cur::new(&HELLO_WORLD).is_game("Hello world!"));
/// ```
#[proc_macro]
#[inline]
pub fn game(input: TokenStream) -> TokenStream {
    let game_def = parse_macro_input!(input as GameDef);

    TokenStream::from(quote! {
        #game_def
    })
}

/// Specifies the data that makes up the definition of a `Game`.
struct GameDef {
    /// The visibility of the `Game`.
    vis: Visibility,
    /// The identifier of the `Game`.
    name: Expr,
    /// The expression of the `Game`.
    expr: TokenStream2,
}

impl Parse for GameDef {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
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
            #vis const #name: Lazy<Game> = Lazy::new(|| #expr);
        });
    }
}

/// Quotes `expr` as a `Game`.
#[throws(Error)]
fn gamify(expr: Expr) -> TokenStream2 {
    match expr {
        Expr::Lit(lit) => gamify_lit(lit.lit)?,
        Expr::Path(path) => quote! {
            #path.clone()
        },
        Expr::Paren(paren) => {
            let inner_expr = gamify(*paren.expr)?;
            quote! {
                (#inner_expr)
            }
        }
        Expr::Try(try_expr) => {
            let optional_expr = gamify(*try_expr.expr)?;
            quote! {
                Game::Sequence(vec![]) | #optional_expr
            }
        }
        Expr::Binary(binary) => {
            let left = gamify(*binary.left)?;
            let op = binary.op;
            let right = gamify(*binary.right)?;

            quote! {
                #left #op #right
            }
        }
        Expr::Range(range) => gamify_range(range)?,
        Expr::Type(ty) => gamify_type(ty)?,
        Expr::Repeat(repeat) => gamify_repeat(repeat)?,
        Expr::Array(array) => {
            let mut elements = Vec::new();

            for element in array.elems.iter() {
                elements.push(gamify(element.clone())?);
            }

            quote_sequence(&elements)
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
        | Expr::__Nonexhaustive => throw!(Error::new_spanned(
            expr,
            "expression cannot be converted into `Game`",
        )),
    }
}

/// Quotes `lit` as a `Game`.
#[throws(Error)]
fn gamify_lit(lit: Lit) -> TokenStream2 {
    let mut games = Vec::new();

    match lit {
        Lit::Char(c) => {
            games.push(quote! {
                Game::from(#c)
            });
        }
        Lit::Byte(byte) => {
            games.push(quote! {
                Game::from(#byte)
            });
        }
        Lit::Str(s) => {
            for c in s.value().chars() {
                games.push(quote! {
                    Game::from(#c)
                });
            }
        }
        Lit::ByteStr(byte_string) => {
            for byte in byte_string.value() {
                games.push(quote! {
                    Game::from(#byte)
                })
            }
        }
        Lit::Int(..) | Lit::Float(..) | Lit::Bool(..) | Lit::Verbatim(..) => {
            throw!(Error::new_spanned(
                lit,
                "expected a byte, byte string, character or string literal",
            ))
        }
    }

    quote_sequence(&games)
}

/// Quotes `range` as a `Game`.
#[throws(Error)]
fn gamify_range(range: ExprRange) -> TokenStream2 {
    let last_char = parse_last_char(&range)?;
    let first_char = match range.from {
        None => FIRST_CHAR,
        Some(from) => parse_char(*from)?,
    };
    // TODO: Submit an issue and add link here.
    // fehler does not properly return quote! so must store as local variable then return.
    let game = quote! {
        Game::Single(Scent::Range(#first_char, #last_char))
    };

    game
}

/// Quotes `repeat` as a `Game`.
#[throws(Error)]
fn gamify_repeat(repeat: ExprRepeat) -> TokenStream2 {
    let quantifier = Quantifier::try_from(*repeat.len)?;
    let game = gamify(*repeat.expr)?;
    let mut games = vec![game.clone(); quantifier.required()];

    if quantifier.is_unlimited() {
        games.push(quote! {
            Game::Repetition(Pattern::from(#game))
        });
    } else {
        games.extend(vec![quote!{
            Game::Sequence(vec![])|#game
        }; quantifier.optional()]);
    }

    quote_sequence(&games)
}

/// Quotes `ty` as a `Game`.
#[throws(Error)]
fn gamify_type(ty: ExprType) -> TokenStream2 {
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
        | Type::__Nonexhaustive => throw!(Error::new_spanned(ty.ty, "expected path")),
    }?;
    let game = gamify(*ty.expr)?;
    // fehler does not properly return quote! so must store as local variable then return.
    let item = quote! {
        Game::Item(#id, Box::new(#game))
    };

    item
}

/// Quotes `games` as a `Game::Sequence`.
fn quote_sequence(games: &[TokenStream2]) -> TokenStream2 {
    // TODO: Follow up on this false positive.
    #[allow(clippy::integer_arithmetic)] // False positive.
    if games.is_empty() {
        // Output an empty sequence. Required because concat() cannot infer type from an empty Vec.
        quote! {
            Game::Sequence(vec![])
        }
    } else if games.len() == 1 {
        // Only output the single game.
        quote! {
            #((#games))*
        }
    } else {
        quote! {
            Game::Sequence(vec![#((#games).into_steps()),*].concat())
        }
    }
}

/// Signifies the number of times a game can be repeated.
#[derive(Debug)]
struct Quantifier(Range<usize>);

impl Quantifier {
    /// Returns the number of required repetitions specified by `self`.
    const fn required(&self) -> usize {
        self.0.start
    }

    /// Returns the number of optional repetitions specified by `self`.
    fn optional(&self) -> usize {
        self.0.len()
    }

    /// Returns if `self` has no limit.
    const fn is_unlimited(&self) -> bool {
        self.0.end == usize::max_value()
    }
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
        Ok(Self (
            value
                .clone()
                .from
                .map_or(Ok(0), |from| usize_try_from_expr(*from))?..value.clone().to.map_or(Ok(usize::max_value()), |to| {
                usize_try_from_expr(*to).map(|max| {
                    max.saturating_sub(if let RangeLimits::HalfOpen(..) = value.limits {
                        1
                    } else {
                        0
                    })
                })
            })?
        ))
    }
}

impl TryFrom<Lit> for Quantifier {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        usize_try_from_lit(&value).map(|minimum| Self (
            minimum..minimum
        ))
    }
}

/// Parses the last `char` that is included in `range`.
#[throws(Error)]
fn parse_last_char(range: &ExprRange) -> char {
    match &range.to {
        None => LAST_CHAR,
        Some(to) => {
            let to_char = parse_char(*to.clone())?;

            match range.limits {
                RangeLimits::Closed(..) => to_char,
                RangeLimits::HalfOpen(..) => {
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
                }
            }
        }
    }
}

/// Converts `expr` into a [`char`].
///
/// Returns [`Err`] if `expr` is unable to be converted.
fn parse_char(expr: Expr) -> ParseResult<char> {
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
