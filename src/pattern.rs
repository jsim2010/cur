//! Defines the unicode string pattern to be hunted.
use {
    alloc::{vec, vec::Vec},
    core::{
        convert::TryFrom,
        fmt::{self, Display, Formatter},
        iter::{self, Chain, FromIterator, Once, Repeat},
        slice::Iter,
    },
    once_cell::sync::Lazy,
};

/// The identifier of a mark.
pub type Name = &'static str;

/// The atomic element of a unicode string pattern.
#[derive(Clone, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum Scent {
    /// Matches the given [`char`].
    Char(char),
    /// Matches a single [`char`] whose code point is within the inclusive range of the 2 given.
    Range(char, char),
    /// Matches any of the given [`Odor`]s.
    ///
    /// Matches are attempted in the given order.
    Union(MultipleOdors),
    /// Matches any number of repetitions of the given [`Odor`].
    ///
    /// Matches are attempted starting with 0 repetitions (an empty string) and incrementing the
    /// number of repetitions by 1 until a match cannot be made.
    Repetition(Odor),
    /// Matches the given [`Odor`].
    ///
    /// Matches will be marked with the [`Name`] of the given [`Odor`].
    Marked(Odor),
}

impl Scent {
    /// Returns if `self` is marked with `name`.
    fn is_marked(&self, name: Name) -> bool {
        match *self {
            Self::Char(..) | Self::Range(..) => false,
            Self::Union(ref odors) => {
                for odor in odors {
                    if odor.is_marked(name) {
                        return true;
                    }
                }

                false
            }
            Self::Repetition(ref odor) | Self::Marked(ref odor) => odor.is_marked(name),
        }
    }
}

impl Display for Scent {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Char(c) => write!(f, "'{}'", c),
            Self::Range(begin, end) => write!(f, "'{}'..='{}'", begin, end),
            Self::Union(ref odors) => write!(f, "{}", odors.display('(', " | ", ')')),
            Self::Repetition(ref odor) => write!(f, "{}*", odor),
            Self::Marked(ref odor) => write!(f, "({})", odor),
        }
    }
}

/// An iteration of a [`slice`] of [`Scent`]s.
///
/// `'o` is the lifetime of the [`Scent`]s.
pub(crate) type Scents<'o> = Iter<'o, Scent>;

/// An error while marking an [`Odor`].
#[derive(Copy, Clone, Debug)]
pub enum MarkOdorError {
    /// The name already exists in the [`Odor`].
    NameAlreadyExists,
}

/// A unicode string pattern defined as a sequence of [`Scent`]s.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Odor {
    /// The sequence of [`Scent`]s.
    scents: Vec<Scent>,
    /// The assigned [`Name`].
    name: Option<Name>,
}

impl Odor {
    /// Converts `self` into [`Odor`]s that each represent a possible branch.
    #[must_use]
    pub fn into_branches(self) -> Vec<Self> {
        if let (Some(&Scent::Union(ref branches)), None) = (self.scents.get(0), self.scents.get(1))
        {
            branches.clone().into_vec()
        } else {
            vec![self]
        }
    }

    /// Returns `self` after marking it with `name`.
    ///
    /// # Errors
    ///
    /// If `self` or one of its [`Scent`]s has already been marked with `name`,
    /// [`MarkOdorError::NameAlreadyExists`] will be returned.
    pub fn mark(mut self, name: Name) -> Result<Self, MarkOdorError> {
        if self.scent_is_marked(name) {
            return Err(MarkOdorError::NameAlreadyExists);
        }

        self.name = Some(name);
        Ok(self)
    }

    /// Returns the [`Name`] of `self`.
    #[must_use]
    pub(crate) const fn name(&self) -> Option<Name> {
        self.name
    }

    /// Returns the [`Scents`] of `self`.
    #[must_use]
    pub(crate) fn scents(&self) -> Scents<'_> {
        self.scents.iter()
    }

    /// Returns if a [`Scent`] of `self` is marked with `name`.
    fn scent_is_marked(&self, name: Name) -> bool {
        for scent in &self.scents {
            if scent.is_marked(name) {
                return true;
            }
        }

        false
    }

    /// Returns if `self` or any of its [`Scent`]s are marked with `name`.
    fn is_marked(&self, name: Name) -> bool {
        (self.name == Some(name)) || self.scent_is_marked(name)
    }
}

impl Default for Odor {
    fn default() -> Self {
        Self::from_iter([])
    }
}

impl Display for Odor {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        for scent in &self.scents {
            write!(f, "{}", scent)?;
        }

        if let Some(name) = self.name {
            write!(f, " as \"{}\"", name)?;
        }

        Ok(())
    }
}

impl Extend<Scent> for Odor {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Scent>,
    {
        self.scents.extend(iter);
    }
}

// The following From implementations are used by odor! to convert defined items into Odors.
impl From<char> for Odor {
    fn from(c: char) -> Self {
        iter::once(Scent::Char(c)).collect()
    }
}

impl From<&str> for Odor {
    fn from(s: &str) -> Self {
        s.chars().map(Scent::Char).collect()
    }
}

impl From<Vec<Self>> for Odor {
    fn from(odors: Vec<Self>) -> Self {
        let mut scents = Vec::new();

        for mut odor in odors {
            if odor.name().is_some() {
                scents.push(Scent::Marked(odor));
            } else {
                scents.append(&mut odor.scents);
            }
        }

        Self::from_iter(scents)
    }
}

impl FromIterator<Scent> for Odor {
    fn from_iter<I>(iter: I) -> Self
    where
        I: IntoIterator<Item = Scent>,
    {
        Self {
            scents: Vec::from_iter(iter),
            name: None,
        }
    }
}

/// An error thrown while creating a [`MultipleOdors`].
#[derive(Clone, Copy, Debug)]
pub enum CreateMultipleOdorsError {
    /// Not enough [`Odor`]s were provided.
    TooFewOdors,
}

/// A sequence of at least 2 [`Odor`]s.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MultipleOdors {
    /// The first two [`Odor`]s.
    first_two: [Odor; 2],
    /// The rest of the [`Odor`]s.
    rest: Vec<Odor>,
}

impl MultipleOdors {
    /// Creates a new [`MultipleOdors`].
    #[must_use]
    pub fn new(first: Odor, second: Odor, rest: Vec<Odor>) -> Self {
        Self {
            first_two: [first, second],
            rest,
        }
    }

    /// Converts `self` into a [`Vec`] of [`Odors`].
    fn into_vec(mut self) -> Vec<Odor> {
        let mut odors = self.first_two.to_vec();
        odors.append(&mut self.rest);
        odors
    }

    /// Returns an item implementing [`Display`] of `self`.
    ///
    /// The format of the output starts with `start`, ends with `end`, and lists each [`Odor`] in
    /// `self` with `delimiter` in between.
    fn display<'a>(&'a self, start: char, delimiter: &'static str, end: char) -> impl Display + 'a {
        /// Implements display for [`MultipleOdors`].
        struct MultipleOdorsDisplay<'a> {
            /// A reference to the [`MultipleOdors`].
            multiple_odors: &'a MultipleOdors,
            /// The starting [`char`].
            start: char,
            /// The delimiter that separates each [`Odor`].
            delimiter: &'static str,
            /// The ending [`char`].
            end: char,
        }

        impl<'a> Display for MultipleOdorsDisplay<'a> {
            fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
                write!(
                    f,
                    "{}{}{}{}",
                    self.start,
                    self.multiple_odors.first_two[0],
                    self.delimiter,
                    self.multiple_odors.first_two[1]
                )?;

                for odor in &self.multiple_odors.rest {
                    write!(f, "{}{}", self.delimiter, odor)?;
                }

                write!(f, "{}", self.end)
            }
        }

        MultipleOdorsDisplay {
            multiple_odors: self,
            start,
            delimiter,
            end,
        }
    }
}

impl Display for MultipleOdors {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.display('[', ", ", ']'))
    }
}

impl Extend<Odor> for MultipleOdors {
    fn extend<I>(&mut self, iter: I)
    where
        I: IntoIterator<Item = Odor>,
    {
        self.rest.extend(iter);
    }
}

impl<'a> IntoIterator for &'a MultipleOdors {
    type Item = &'a Odor;
    type IntoIter = Chain<Iter<'a, Odor>, Iter<'a, Odor>>;

    fn into_iter(self) -> Self::IntoIter {
        self.first_two.iter().chain(self.rest.iter())
    }
}

impl TryFrom<Vec<Odor>> for MultipleOdors {
    type Error = CreateMultipleOdorsError;

    fn try_from(odors: Vec<Odor>) -> Result<Self, Self::Error> {
        odors
            .get(0)
            .and_then(|first| {
                odors.get(1).and_then(|second| {
                    odors
                        .get(2..)
                        .map(|rest| Self::new(first.clone(), second.clone(), rest.to_vec()))
                })
            })
            .ok_or(CreateMultipleOdorsError::TooFewOdors)
    }
}

/// An [`Odor`] that matches an empty string.
static EMPTY_ODOR: Lazy<Odor> = Lazy::new(Odor::default);

/// An iteration of [`Odor`]s.
///
/// `'o` is the lifetime of the [`Odor`]s.
#[derive(Debug)]
pub(crate) enum Odors<'o> {
    /// A single [`Odor`].
    Single(Option<&'o Odor>),
    /// Repeats a given [`Odor`] after a single instance of [`EMPTY_ODOR`].
    RepeatAfterEmpty(Chain<Once<&'o Odor>, Repeat<&'o Odor>>),
    /// Iterates on the given [`Vec`] of [`Odor`]s.
    List(Chain<Iter<'o, Odor>, Iter<'o, Odor>>),
}

impl<'o> Odors<'o> {
    /// Creates a new [`Odors`] that iterates `odor` once.
    pub(crate) const fn single(odor: &'o Odor) -> Self {
        Self::Single(Some(odor))
    }

    /// Creates a new [`Odors`] that starts with [`EMPTY_ODOR`] then repeats `odor`.
    pub(crate) fn repeat_after_empty(odor: &'o Odor) -> Self {
        let start: Once<&'o Odor> = iter::once(&EMPTY_ODOR);
        Self::RepeatAfterEmpty(start.chain(iter::repeat(odor)))
    }

    /// Creates a new [`Odors`] that iterates over `odors`.
    pub(crate) const fn list(odors: Chain<Iter<'o, Odor>, Iter<'o, Odor>>) -> Self {
        Self::List(odors)
    }
}

impl<'o> Iterator for Odors<'o> {
    type Item = &'o Odor;

    fn next(&mut self) -> Option<Self::Item> {
        match *self {
            Self::Single(ref mut odor) => odor.take(),
            Self::RepeatAfterEmpty(ref mut odors) => odors.next(),
            Self::List(ref mut odors) => odors.next(),
        }
    }
}
