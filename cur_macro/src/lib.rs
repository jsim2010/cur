//! `cur_macro` - Implements procedural macros for `cur`.
#![no_std]

extern crate alloc;

use {
    alloc::{boxed::Box, vec, vec::Vec},
    core::{
        convert::{TryFrom, TryInto},
        ops::Range,
    },
    proc_macro::TokenStream,
    proc_macro2::TokenStream as TokenStream2,
    quote::{quote, ToTokens},
    syn::{
        bracketed, parenthesized,
        parse::{Lookahead1, Parse, ParseStream, Result as ParseResult},
        parse_macro_input,
        punctuated::Punctuated,
        token, Error, Expr, ExprRange, Ident, Lit, LitByte, LitByteStr, LitChar, LitInt, LitStr,
        Path, PathArguments, PathSegment, RangeLimits, Token, Visibility,
    },
};

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
/// game!(HELLO_WORLD = ("Hello", ' ', "world", '!'));
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
    name: Ident,
    eq: Token![=],
    game: GameInput,
}

impl Parse for GameDef {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        Ok(Self {
            vis: input.parse()?,
            name: input.parse()?,
            eq: input.parse()?,
            game: input.parse()?,
        })
    }
}

impl ToTokens for GameDef {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let vis = &self.vis;
        let name = &self.name;
        let eq = &self.eq;
        let game = &self.game;

        tokens.extend(quote! {
            #vis const #name: Lazy<Game> #eq Lazy::new(|| #game);
        });
    }
}

#[derive(Clone)]
enum GameInput {
    Single(char),
    Range(char, char),
    Sequence(Vec<GameInput>),
    Union(Vec<GameInput>),
    Item {
        name: Ident,
        game: Box<GameInput>,
    },
    Star(Box<GameInput>),
    Repeat {
        game: Box<GameInput>,
        quantifier: Quantifier,
    },
    Path(Path),
}

impl Parse for GameInput {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut lookahead = input.lookahead1();

        let game = if lookahead.peek(token::Paren) {
            let content;
            let _ = parenthesized!(content in input);
            if content.is_empty() {
                Ok(Self::Sequence(vec![]))
            } else {
                let steps: Punctuated<GameInput, Token![,]> =
                    Punctuated::parse_terminated(&content)?;

                Ok(Self::Sequence(steps.into_iter().collect()))
            }
        } else if lookahead.peek(LitChar) {
            let c: LitChar = input.parse()?;
            lookahead = input.lookahead1();

            if lookahead.peek(Token![..=]) {
                let _: Token![..=] = input.parse()?;
                let end: LitChar = input.parse()?;
                Ok(Self::Range(c.value(), end.value()))
            } else {
                Ok(Self::Single(c.value()))
            }
        } else if lookahead.peek(LitStr) {
            let s: LitStr = input.parse()?;
            Ok(Self::Sequence(
                s.value().chars().map(|c| Self::Single(c)).collect(),
            ))
        } else if lookahead.peek(LitByte) {
            let byte: LitByte = input.parse()?;
            Ok(Self::Single(char::from(byte.value())))
        } else if lookahead.peek(LitByteStr) {
            let s: LitByteStr = input.parse()?;
            let mut games = Vec::new();

            for byte in s.value() {
                games.push(Self::Single(char::from(byte)));
            }

            Ok(Self::Sequence(games))
        } else if lookahead.peek(Token![_]) {
            let _: Token![_] = input.parse()?;
            Ok(Self::Range('\u{0}', '\u{10ffff}'))
        } else if lookahead.peek(Ident) {
            let name = input.parse()?;

            if input.peek(Token![@]) {
                let _: Token![@] = input.parse()?;

                input.parse().map(|game| Self::Item { name, game })
            } else {
                let mut segments = Punctuated::new();
                segments.push(PathSegment {
                    ident: name,
                    arguments: PathArguments::None,
                });

                while input.peek(Token![::]) {
                    let _: Token![::] = input.parse()?;
                    segments.push(input.parse()?);
                }

                Ok(Self::Path(Path {
                    leading_colon: None,
                    segments,
                }))
            }
        } else if lookahead.peek(token::Bracket) {
            let content;
            let _ = bracketed!(content in input);
            let game = content.parse()?;
            let _: Token![;] = content.parse()?;
            let quantifier = content.parse()?;

            Ok(Self::Repeat { game, quantifier })
        } else {
            Err(lookahead.error())
        };

        lookahead = input.lookahead1();

        if lookahead.peek(Token![?]) {
            let _: Token![?] = input.parse()?;
            game.map(|g| Self::Union(vec![Self::Sequence(vec![]), g]))
        } else if lookahead.peek(Token![|]) {
            let _: Token![|] = input.parse()?;
            game.and_then(|g| Ok(Self::Union(vec![g, input.parse()?])))
        } else {
            game
        }
    }
}

impl ToTokens for GameInput {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match self {
            Self::Single(c) => {
                quote! {
                    Game::Single(Scent::Char(#c))
                }
            }
            Self::Range(start, end) => {
                quote! {
                    Game::Single(Scent::Range(#start, #end))
                }
            }
            Self::Sequence(games) => match games.len() {
                0 => {
                    quote! {
                        Game::Sequence(vec![])
                    }
                }
                1 => {
                    let game = &games[0];
                    quote! {
                        #game
                    }
                }
                _ => {
                    quote! {
                        Game::Sequence(vec![#(#games.into_steps()),*].concat())
                    }
                }
            },
            Self::Union(games) => {
                quote! {
                    Game::Union(vec![#(#games.into_branches()),*].concat())
                }
            }
            Self::Item { name, game } => {
                quote! {
                    Game::Item(stringify!(#name), Box::new(#game))
                }
            }
            Self::Repeat { game, quantifier } => {
                let mut games = vec![*game.clone(); quantifier.required()];

                if quantifier.is_unlimited() {
                    games.push(Self::Star(game.clone()));
                } else {
                    games.extend(vec![
                        Self::Union(vec![Self::Sequence(vec![]), *game.clone()]);
                        quantifier.optional()
                    ]);
                }

                let sequence = Self::Sequence(games);

                quote! {
                    #sequence
                }
            }
            Self::Star(game) => {
                quote! {
                    Game::Repetition(#game.into_pattern())
                }
            }
            Self::Path(path) => {
                quote! {
                    #path.clone()
                }
            }
        })
    }
}

/// Signifies the number of times a game can be repeated.
#[derive(Clone, Debug)]
struct Quantifier(Range<usize>);

impl Quantifier {
    /// Returns the number of required repetitions specified by `self`.
    fn required(&self) -> usize {
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

impl Parse for Quantifier {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut lookahead = input.lookahead1();

        if lookahead.peek(LitInt) {
            let min = parse_usize(input)?;
            lookahead = input.lookahead1();

            Ok(Self(min..parse_max(input, lookahead).unwrap_or(min)))
        } else {
            parse_max(input, lookahead).map(|max| Self(usize::min_value()..max))
        }
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
        Ok(Self(
            value
                .clone()
                .from
                .map_or(Ok(0), |from| usize_try_from_expr(*from))?
                ..value.clone().to.map_or(Ok(usize::max_value()), |to| {
                    usize_try_from_expr(*to).map(|max| {
                        max.saturating_sub(if let RangeLimits::HalfOpen(..) = value.limits {
                            1
                        } else {
                            0
                        })
                    })
                })?,
        ))
    }
}

impl TryFrom<Lit> for Quantifier {
    type Error = Error;

    fn try_from(value: Lit) -> Result<Self, Self::Error> {
        usize_try_from_lit(&value).map(|minimum| Self(minimum..minimum))
    }
}

fn parse_max(input: ParseStream<'_>, lookahead: Lookahead1<'_>) -> ParseResult<usize> {
    let mut max = usize::max_value();

    if lookahead.peek(Token![..=]) {
        let _: Token![..=] = input.parse()?;

        if input.peek(LitInt) {
            max = parse_usize(input)?;
        }

        Ok(max)
    } else if lookahead.peek(Token![..]) {
        let _: Token![..] = input.parse()?;

        if input.peek(LitInt) {
            max = parse_usize(input).map(|max| max.saturating_sub(1))?
        }

        Ok(max)
    } else {
        Err(lookahead.error())
    }
}

fn parse_usize(input: ParseStream<'_>) -> ParseResult<usize> {
    let int: LitInt = input.parse()?;
    int.base10_parse()
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
