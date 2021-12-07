//! `cur_macro` - Implements procedural macros for cur.
#![no_std]

extern crate alloc;

use {
    alloc::{boxed::Box, vec, vec::Vec},
    core::ops::Range,
    proc_macro::TokenStream,
    proc_macro2::TokenStream as TokenStream2,
    quote::{quote, ToTokens},
    syn::{
        bracketed,
        parse::{Parse, ParseStream, Result as ParseResult},
        parse_macro_input,
        punctuated::Punctuated,
        token, Ident, LitByte, LitByteStr, LitChar, LitInt, LitStr, Path, PathArguments,
        PathSegment, Token, Visibility,
    },
};

/// Converts `input` into a definition of an `Odor` variable.
///
/// Creating `Odor`s by hand can quickly become complex and prone to errors. Thus, this procedural
/// macro provides the functionality for a user to build an `Odor` using rust syntax that is easily
/// understandable.
///
/// # General Format
/// The format of the macro input shall be `[VISIBILITY] IDENTIFIER = ODOR_EXPR`, which shall
/// output `[VISIBILITY] static IDENTIFIER: Lazy<Odor> = Lazy::new(|| ODOR)`.
///
/// # Example(s)
///
/// ## Atoms
/// ### Char
/// A char, a byte and a string containing a single char shall each be converted to the appropriate `Char`.
/// ```
/// # use cur::*;
/// odor!(SINGLE_CHAR = 'a');
/// assert_eq!(*SINGLE_CHAR, Odor::from_iter([Scent::Char('a')]));
///
/// odor!(BYTE = b'0');
/// assert_eq!(*BYTE, Odor::from_iter([Scent::Char('0')]));
///
/// odor!(SINGLE_STR = "b");
/// assert_eq!(*SINGLE_STR, Odor::from_iter([Scent::Char('b')]));
/// ```
///
/// ### Range
/// A range expression of chars is converted to a `Range` of the given chars.
/// ```
/// # use cur::*;
/// odor!(RANGE = 'a'..='z');
/// assert_eq!(*RANGE, Odor::from_iter([Scent::Range('a', 'z')]));
/// ```
///
/// ### Placeholder
/// A placeholder pattern is converted to a `Range` that matches any unicode char.
/// ```
/// # use cur::*;
/// odor!(WILDCARD = _);
/// assert_eq!(*WILDCARD, Odor::from_iter([Scent::Range('\u{0}', '\u{10ffff}')]));
/// ```
///
/// ### Str
/// A `str` is converted to a sequence of `Char`s.
/// ```
/// # use cur::*;
/// odor!(STR = "abc");
/// assert_eq!(*STR, Odor::from_iter([Scent::Char('a'), Scent::Char('b'), Scent::Char('c')]));
///
/// odor!(BYTE_STR = b"test");
/// assert_eq!(*BYTE_STR, Odor::from_iter([Scent::Char('t'), Scent::Char('e'), Scent::Char('s'), Scent::Char('t')]));
/// ```
///
/// An empty string shall be converted to an empty sequence.
/// ```
/// # use cur::*;
/// odor!(EMPTY_STR = "");
/// assert_eq!(*EMPTY_STR, Odor::from_iter([]));
/// ```
///
/// ## Operations
/// ### Array
/// An array of odor expressions is converted to a sequence of the given `Odor`s.
/// ```
/// # use cur::*;
/// odor!(SEQ = ['a', 'b', 'c']);
/// assert_eq!(*SEQ, Odor::from_iter([Scent::Char('a'), Scent::Char('b'), Scent::Char('c')]));
///
/// odor!(SINGLE_SEQ = ['a']);
/// assert_eq!(*SINGLE_SEQ, Odor::from_iter([Scent::Char('a')]));
///
/// odor!(EMPTY = []);
/// assert_eq!(*EMPTY, Odor::from_iter([]));
/// ```
///
/// ## Logical Or
/// A logical or of odor expressions is converted to a `Union` of the given `Odor`s.
/// ```
/// # use cur::*;
/// odor!(UNION = 'a' | 'b');
/// assert_eq!(*UNION, Odor::from_iter([Scent::Union(MultipleOdors::new(Odor::from_iter([Scent::Char('a')]), Odor::from_iter([Scent::Char('b')]), vec![]))]));
/// ```
///
/// Multiple logical ors are combined into a single `Union`.
/// ```
/// # use cur::*;
/// odor!(MULT_UNION = 'a' | 'b' | 'c');
/// assert_eq!(*MULT_UNION, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([Scent::Char('a')]),
///     Odor::from_iter([Scent::Char('b')]),
///     vec![Odor::from_iter([Scent::Char('c')])],
/// ))]));
/// ```
///
/// ## Try
/// A try expression of an odor expression is converted to a `Union` of an empty sequence and the given `Odor`.
/// ```
/// # use cur::*;
/// odor!(TRY = 'a'?);
/// assert_eq!(*TRY, Odor::from_iter([Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![]))]));
/// ```
///
/// ## Repetitions
/// An array type is converted to a repetition of the element `Odor` based on the size expression.
///
/// ### Exact Size
/// A literal number size is converted to a sequence of `Odor`s equal to the size.
/// ```
/// # use cur::*;
/// odor!(REP_ZERO = ['a'; 0]);
/// assert_eq!(*REP_ZERO, Odor::from_iter([]));
///
/// odor!(REP_ONE = ['a'; 1]);
/// assert_eq!(*REP_ONE, Odor::from_iter([Scent::Char('a')]));
///
/// odor!(REP_EXACT = ['a'; 3]);
/// assert_eq!(*REP_EXACT, Odor::from_iter([Scent::Char('a'), Scent::Char('a'), Scent::Char('a')]));
/// ```
///
/// ## Range Size
/// A range size is converted to the appropriate sequence.
///
/// If no maximum is given, a `Repetition` is appended to the end of the sequence.
/// ```
/// # use cur::*;
/// odor!(REP_ANY = ['a'; ..]);
/// assert_eq!(*REP_ANY, Odor::from_iter([Scent::Repetition(Odor::from_iter([Scent::Char('a')]))]));
/// ```
///
/// If a minimum is given, the sequence starts with that number of `Odor`s.
/// ```
/// # use cur::*;
/// odor!(REP_MIN = ['a'; 2..]);
/// assert_eq!(*REP_MIN, Odor::from_iter([Scent::Char('a'), Scent::Char('a'), Scent::Repetition(Odor::from_iter([Scent::Char('a')]))]));
/// ```
///
/// If a maximum is given, a sequence of `Union`s of an empty sequence and the `Odor` equal to the difference between maximum and minimum (assume 0 if none is given) are appended to the end of the sequence.
/// Both inclusive and exclusive maximums are handled.
/// ```
/// # use cur::*;
/// odor!(REP_LESS_THAN = ['a'; ..4]);
/// assert_eq!(*REP_LESS_THAN, Odor::from_iter([
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
/// ]));
///
/// odor!(REP_MAX = ['a'; ..=3]);
/// assert_eq!(*REP_MAX, Odor::from_iter([
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
/// ]));
///
/// odor!(REP_RANGE = ['a'; 2..5]);
/// assert_eq!(*REP_RANGE, Odor::from_iter([
///    Scent::Char('a'),
///    Scent::Char('a'),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
/// ]));
///
/// odor!(REP_WITHIN = ['a'; 2..=4]);
/// assert_eq!(*REP_WITHIN, Odor::from_iter([
///    Scent::Char('a'),
///    Scent::Char('a'),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
///    Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('a')]), vec![])),
/// ]));
/// ```
///
/// ## Wildcard
/// A wildcard pattern is converted to a infinite repetition of any unicode character. This is the
/// same as `[_; ..]`.
/// ```
/// # use cur::*;
/// odor!(WILDCARD = ..);
/// assert_eq!(*WILDCARD, Odor::from_iter([Scent::Repetition(Odor::from_iter([Scent::Range('\u{0}', '\u{10ffff}')]))]));
/// ```
///
/// ## Combinations
/// Each of these expressions can be combined with one another to form more complex expressions.
/// ```
/// # use cur::*;
/// odor!(OPT_SEQ = ['a', 'b']?);
/// assert_eq!(*OPT_SEQ, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([]),
///     Odor::from_iter([Scent::Char('a'), Scent::Char('b')]),
///     vec![],
/// ))]));
///
/// odor!(SEQ_WITH_OPT = ['a', 'b'?]);
/// assert_eq!(*SEQ_WITH_OPT, Odor::from_iter([
///     Scent::Char('a'),
///     Scent::Union(MultipleOdors::new(Odor::from_iter([]), Odor::from_iter([Scent::Char('b')]),
///     vec![])),
/// ]));
///
/// odor!(OPT_UNION = ['a' | 'b']?);
/// assert_eq!(*OPT_UNION, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([]),
///     Odor::from_iter([Scent::Char('a')]),
///     vec![Odor::from_iter([Scent::Char('b')])],
/// ))]));
///
/// odor!(UNION_WITH_OPT = 'a' | 'b'?);
/// assert_eq!(*UNION_WITH_OPT, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([Scent::Char('a')]),
///     Odor::from_iter([]),
///     vec![Odor::from_iter([Scent::Char('b')])],
/// ))]));
///
/// odor!(SEQ_UNION = ['a', 'b'] | 'c');
/// assert_eq!(*SEQ_UNION, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([Scent::Char('a'), Scent::Char('b')]),
///     Odor::from_iter([Scent::Char('c')]),
///     vec![],
/// ))]));
///
/// odor!(UNION_SEQ = ['a', 'b' | 'c']);
/// assert_eq!(*UNION_SEQ, Odor::from_iter([
///     Scent::Char('a'),
///     Scent::Union(MultipleOdors::new(Odor::from_iter([Scent::Char('b')]), Odor::from_iter([Scent::Char('c')]), vec![])),
/// ]));
///
/// odor!(REP_UNION = ['a' | 'b'; 1..3]);
/// assert_eq!(*REP_UNION, Odor::from_iter([
///     Scent::Union(MultipleOdors::new(
///         Odor::from_iter([Scent::Char('a')]),
///         Odor::from_iter([Scent::Char('b')]),
///         vec![]
///     )),
///     Scent::Union(MultipleOdors::new(
///         Odor::from_iter([]),
///         Odor::from_iter([Scent::Char('a')]),
///         vec![Odor::from_iter([Scent::Char('b')])],
///     )),
/// ]));
///
/// odor!(REP_SEQ = [['a', 'b']; 1..]);
/// assert_eq!(*REP_SEQ, Odor::from_iter([
///     Scent::Char('a'),
///     Scent::Char('b'),
///     Scent::Repetition(Odor::from_iter([
///         Scent::Char('a'),
///         Scent::Char('b'),
///     ])),
/// ]));
/// ```
///
/// ## Casting
/// Casting an odor expression assigns a name to the odor.
/// ```
/// # use cur::*;
/// odor!(NAMED = 'a' as id);
/// assert_eq!(*NAMED, Odor::from_iter([Scent::Char('a')]).mark("id").unwrap());
///
/// odor!(OPT = 'a'? as id);
/// assert_eq!(*OPT, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([]),
///     Odor::from_iter([Scent::Char('a')]),
///     vec![],
/// ))]).mark("id").unwrap());
///
/// odor!(SEQ = ['a', 'b'] as id);
/// assert_eq!(*SEQ, Odor::from_iter([
///     Scent::Char('a'),
///     Scent::Char('b'),
/// ]).mark("id").unwrap());
///
/// odor!(UNION = ['a' | 'b'] as id);
/// assert_eq!(*UNION, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([Scent::Char('a')]),
///     Odor::from_iter([Scent::Char('b')]),
///     vec![],
/// ))]).mark("id").unwrap());
/// ```
///
/// ## Variables.
/// Literal variables can be used in new odor definitions.
/// ```
/// # use cur::*;
/// const NEWLINE: &str = "\r\n";
/// const START: char = '1';
///
/// odor!(ONE = [START]);
/// assert_eq!(*ONE, Odor::from_iter([Scent::Char('1')]));
///
/// odor!(LINE = [START, [_;..], NEWLINE]);
/// assert_eq!(*LINE, Odor::from_iter([
///     Scent::Char('1'),
///     Scent::Repetition(Odor::from_iter([Scent::Range('\u{0}', '\u{10ffff}')])),
///     Scent::Char('\r'),
///     Scent::Char('\n'),
/// ]));
/// ```
///
/// Odor variables can also be used in new odor definitions.
/// ```
/// # use cur::*;
/// odor!(DIGIT = '0'..='9');
/// odor!(COPY = DIGIT);
/// assert_eq!(*COPY, Odor::from_iter([Scent::Range('0', '9')]));
///
/// odor!(DIGIT_OR_A = 'a' | DIGIT);
/// assert_eq!(*DIGIT_OR_A, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([Scent::Char('a')]),
///     Odor::from_iter([Scent::Range('0', '9')]),
///     vec![],
/// ))]));
///
/// odor!(DIGIT_AND_A = [DIGIT, 'a']);
/// assert_eq!(*DIGIT_AND_A, Odor::from_iter([
///     Scent::Range('0', '9'),
///     Scent::Char('a'),
/// ]));
///
/// odor!(OPT_DIGIT = DIGIT?);
/// assert_eq!(*OPT_DIGIT, Odor::from_iter([Scent::Union(MultipleOdors::new(
///     Odor::from_iter([]),
///     Odor::from_iter([Scent::Range('0', '9')]),
///     vec![],
/// ))]));
///
/// odor!(MY_DIGIT = DIGIT as mine);
/// assert_eq!(*MY_DIGIT, Odor::from_iter([Scent::Range('0', '9')]).mark("mine").unwrap());
/// ```
///
/// Odor variables can follow paths.
/// ```
/// # use cur::*;
/// mod odors {
///     use cur::*;
///     odor!(pub LOWERCASE = 'a'..='z');
/// }
///
/// odor!(LOWER = odors::LOWERCASE);
/// assert_eq!(*LOWER, Odor::from_iter([Scent::Range('a', 'z')]));
///
/// odor!(ANY_LOWER = [odors::LOWERCASE; ..]);
/// assert_eq!(*ANY_LOWER,
/// Odor::from_iter([Scent::Repetition(Odor::from_iter([Scent::Range('a', 'z')]))]));
/// ```
#[proc_macro]
pub fn odor(input: TokenStream) -> TokenStream {
    let game_def = parse_macro_input!(input as OdorDef);

    TokenStream::from(quote! {
        #game_def
    })
}

/// Specifies the input that makes up the definition of an `Odor`.
struct OdorDef {
    /// The visibility of the `Odor`.
    vis: Visibility,
    /// The identifier of the `Odor`.
    ident: Ident,
    /// The assignment operator.
    eq: Token![=],
    /// The `Odor`.
    game: OdorExpr,
}

impl Parse for OdorDef {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        Ok(Self {
            vis: input.parse()?,
            ident: input.parse()?,
            eq: input.parse()?,
            game: input.parse()?,
        })
    }
}

impl ToTokens for OdorDef {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let OdorDef {
            ref vis,
            ref ident,
            ref eq,
            ref game,
        } = *self;

        tokens.extend(quote! {
            use core::{iter::FromIterator as _, convert::TryFrom as _};
            #vis static #ident: Lazy<Odor> #eq Lazy::new(|| #game);
        });
    }
}

/// Identifies an expression that can be converted into a `Odor`.
#[derive(Clone)]
enum OdorExpr {
    /// A single [`char`].
    Char(char),
    /// A range of [`char`]s.
    Range(char, char),
    /// A sequence of [`OdorExpr`]s.
    Sequence(Vec<OdorExpr>),
    /// A union of [`OdorExpr`]s.
    Union(Vec<OdorExpr>),
    /// An [`OdorExpr`] associated with a name.
    Association {
        /// The name.
        name: Ident,
        /// The [`OdorExpr`] being marked.
        odor: Box<OdorExpr>,
    },
    /// A repetition of an [`OdorExpr`] by any number (including 0).
    Star(Box<OdorExpr>),
    /// The path to an already defined `Odor`.
    Defined(Path),
}

impl OdorExpr {
    /// Returns a [`Union`] of an empty sequence and `self`.
    fn optional(self) -> Self {
        Self::Union(vec![Self::Sequence(vec![]), self])
    }

    /// Returns a [`Range`] over all unicode [`char`]s.
    const fn placeholder() -> Self {
        Self::Range('\u{0}', '\u{10ffff}')
    }

    /// Returns the base [`OdorExpr`] from `input`.
    fn parse_base_expr(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut lookahead = input.lookahead1();

        if lookahead.peek(LitChar) {
            let c: LitChar = input.parse()?;
            lookahead = input.lookahead1();

            if lookahead.peek(Token![..=]) {
                let _: Token![..=] = input.parse()?;
                let end: LitChar = input.parse()?;
                Ok(Self::Range(c.value(), end.value()))
            } else {
                Ok(Self::Char(c.value()))
            }
        } else if lookahead.peek(LitStr) {
            let s: LitStr = input.parse()?;
            Ok(Self::Sequence(s.value().chars().map(Self::Char).collect()))
        } else if lookahead.peek(LitByte) {
            let byte: LitByte = input.parse()?;
            Ok(Self::Char(char::from(byte.value())))
        } else if lookahead.peek(LitByteStr) {
            let s: LitByteStr = input.parse()?;
            let mut odors = Vec::new();

            for byte in s.value() {
                odors.push(Self::Char(char::from(byte)));
            }

            Ok(Self::Sequence(odors))
        } else if lookahead.peek(Token![..]) {
            let _: Token![..] = input.parse()?;
            Ok(Self::Star(Box::new(Self::placeholder())))
        } else if lookahead.peek(Token![_]) {
            let _: Token![_] = input.parse()?;
            Ok(Self::placeholder())
        } else if lookahead.peek(Ident) {
            let name = input.parse()?;
            let mut segments = Punctuated::new();
            segments.push(PathSegment {
                ident: name,
                arguments: PathArguments::None,
            });

            while input.peek(Token![::]) {
                let _: Token![::] = input.parse()?;
                segments.push(input.parse()?);
            }

            Ok(Self::Defined(Path {
                leading_colon: None,
                segments,
            }))
        } else if lookahead.peek(token::Bracket) {
            let content;
            let _ = bracketed!(content in input);

            if content.is_empty() {
                Ok(Self::Sequence(vec![]))
            } else {
                let odor: Self = content.parse()?;
                let lookahead = content.lookahead1();

                if lookahead.peek(Token![;]) {
                    let _: Token![;] = content.parse()?;
                    let quantifier: Quantifier = content.parse()?;
                    let mut odors = vec![odor.clone(); quantifier.num_required()];

                    if quantifier.is_unlimited() {
                        odors.push(Self::Star(Box::new(odor)));
                    } else {
                        odors.extend(vec![odor.optional(); quantifier.num_optional()]);
                    }

                    Ok(Self::Sequence(odors))
                } else if lookahead.peek(Token![,]) {
                    let _: Token![,] = content.parse()?;
                    let steps: Punctuated<Self, Token![,]> =
                        Punctuated::parse_terminated(&content)?;
                    let mut odors = vec![odor];

                    odors.extend(steps.into_iter().collect::<Vec<Self>>());
                    Ok(Self::Sequence(odors))
                } else if content.is_empty() {
                    Ok(Self::Sequence(vec![odor]))
                } else {
                    Err(lookahead.error())
                }
            }
        } else {
            Err(lookahead.error())
        }
    }
}

impl Parse for OdorExpr {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let odor = Self::parse_base_expr(input);
        let mut lookahead = input.lookahead1();

        let odor_expr = if lookahead.peek(Token![?]) {
            let _: Token![?] = input.parse()?;
            odor.map(Self::optional)
        } else if lookahead.peek(Token![|]) {
            let _: Token![|] = input.parse()?;
            odor.and_then(|odr| Ok(Self::Union(vec![odr, input.parse()?])))
        } else {
            odor
        };

        lookahead = input.lookahead1();

        if lookahead.peek(Token![as]) {
            let _: Token![as] = input.parse()?;
            odor_expr.and_then(|odr| {
                input.parse().map(|name| Self::Association {
                    name,
                    odor: Box::new(odr),
                })
            })
        } else {
            odor_expr
        }
    }
}

impl ToTokens for OdorExpr {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        tokens.extend(match *self {
            Self::Char(ref ch) => {
                quote! {
                    Odor::from_iter(vec![Scent::Char(#ch)])
                }
            }
            Self::Range(ref first, ref last) => {
                quote! {
                    Odor::from_iter(vec![Scent::Range(#first, #last)])
                }
            }
            Self::Sequence(ref odors) => {
                if odors.is_empty() {
                    quote! {
                        Odor::from_iter(vec![])
                    }
                }
                else if let (Some(odor), None) = (odors.get(0), odors.get(1)) {
                    // Sequence only has 1 odor.
                    quote! {
                        #odor
                    }
                } else {
                    #[allow(clippy::integer_arithmetic)] { // False positive.
                        quote! {
                            Odor::from(vec![#(Odor::from(#odors)),*])
                        }
                    }
                }
            }
            #[allow(clippy::integer_arithmetic)] // False positive.
            Self::Union(ref odors) => {
                quote! {
                    Odor::from_iter(vec![Scent::Union(MultipleOdors::try_from(vec![#(#odors.into_branches()),*].concat()).unwrap())])
                }
            }
            Self::Association { ref name, ref odor } => {
                quote! {
                    Odor::from(#odor).mark(stringify!(#name)).unwrap()
                }
            }
            Self::Star(ref odor) => {
                quote! {
                    Odor::from_iter(vec![Scent::Repetition(#odor)])
                }
            }
            Self::Defined(ref path) => {
                quote! {
                    Odor::from(#path.clone())
                }
            }
        });
    }
}

/// Specifies the desired number(s) of repetitions.
#[derive(Clone, Debug)]
struct Quantifier(Range<usize>);

impl Quantifier {
    /// Returns the number of required repetitions.
    const fn num_required(&self) -> usize {
        self.0.start
    }

    /// Returns the number of optional repetitions.
    fn num_optional(&self) -> usize {
        self.0.len()
    }

    /// Returns if `self` has no maximum limit.
    const fn is_unlimited(&self) -> bool {
        self.0.end == usize::MAX
    }
}

impl Parse for Quantifier {
    fn parse(input: ParseStream<'_>) -> ParseResult<Self> {
        let mut lookahead = input.lookahead1();
        let mut min = usize::MIN;
        let mut single_value_quantifier = None;

        if lookahead.peek(LitInt) {
            min = parse_usize(input)?;

            if input.is_empty() {
                single_value_quantifier = Some(Self(min..min));
            } else {
                lookahead = input.lookahead1();
            }
        }

        if let Some(quantifier) = single_value_quantifier {
            Ok(quantifier)
        } else if lookahead.peek(Token![..=]) {
            let _: Token![..=] = input.parse()?;

            parse_usize(input).map(|max| Self(min..max))
        } else if lookahead.peek(Token![..]) {
            let _: Token![..] = input.parse()?;

            Ok(Self(
                min..parse_usize(input)
                    .map(|max| max.saturating_sub(1))
                    .unwrap_or(usize::MAX),
            ))
        } else {
            Err(lookahead.error())
        }
    }
}

/// Parses a `usize` from `input`.
fn parse_usize(input: ParseStream<'_>) -> ParseResult<usize> {
    input.parse::<LitInt>().and_then(|int| int.base10_parse())
}
