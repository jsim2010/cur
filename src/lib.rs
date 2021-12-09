#![doc = include_str!("../README.md")]
#![no_std]

extern crate alloc;

mod hunt;
mod pattern;

pub use {
    cur_macro::odor,
    hunt::{Catch, Find},
    once_cell::sync::Lazy,
    pattern::{MarkOdorError, MultipleOdors, Name, Odor, Scent},
};

use {
    alloc::vec::Vec,
    core::{
        convert::{TryFrom, TryInto},
        mem,
    },
    hunt::{Fork, ForkKind, Haul, Progress},
    log::trace,
    pattern::{Odors, Scents},
};

/// An error thrown when attmepting to get the a [`Catch`] from [`NextCatch::Unknown`].
struct UnknownNextCatch;

/// Holds the result from attempting to find a catch.
///
/// `'s` is the lifetime of the search.
#[derive(Debug)]
enum NextCatch<'s> {
    /// The attempt found the given [`Catch`].
    Success(Catch<'s>),
    /// The attempt found no [`Catch`]es.
    NonSuccess,
    /// No atttempt was made.
    Unknown,
}

impl<'s> NextCatch<'s> {
    /// Returns if `self` is unknown.
    const fn is_unknown(&self) -> bool {
        matches!(*self, Self::Unknown)
    }

    /// Returns if `self` is a success.
    const fn is_success(&self) -> bool {
        matches!(*self, Self::Success(..))
    }

    /// Updates `self` to hold `catch`.
    fn update(&mut self, catch: Option<Catch<'s>>) {
        *self = match catch {
            Some(success) => Self::Success(success),
            None => Self::NonSuccess,
        };
    }

    /// Takes `self`, replacing it with `Self::default()`.
    fn take(&mut self) -> Self {
        mem::take(self)
    }
}

impl<'s> Default for NextCatch<'s> {
    fn default() -> Self {
        Self::Unknown
    }
}

impl<'s> TryFrom<NextCatch<'s>> for Option<Catch<'s>> {
    type Error = UnknownNextCatch;

    fn try_from(next_catch: NextCatch<'s>) -> Result<Self, Self::Error> {
        match next_catch {
            NextCatch::Success(catch) => Ok(Some(catch)),
            NextCatch::NonSuccess => Ok(None),
            NextCatch::Unknown => Err(UnknownNextCatch),
        }
    }
}

/// The state of a [`Trail`] when a [`Fork`] starts.
#[derive(Debug)]
struct Junction<'s, 'o> {
    /// The [`Haul`] of the [`Trail`] before the [`Junction`].
    ///
    /// # Safety
    ///
    /// Must not be changed once initialized.
    prev_haul: Haul<'s>,
    /// The [`Odors`].
    odors: Odors<'o>,
    /// The [`Fork`].
    fork: Fork<'s>,
    /// The [`Trail`] of the [`Fork`] currently being hunted.
    ///
    /// # Safety
    ///
    /// The search of `fork_trail` must equal `prev_haul.remaining_str()`.
    fork_trail: Option<Trail<'s, 'o>>,
}

impl<'s, 'o> Junction<'s, 'o> {
    /// Creates a new [`Junction`].
    fn new(prev_haul: Haul<'s>, mut odors: Odors<'o>, fork_kind: ForkKind) -> Self {
        let odor = odors.next();
        let mut fork = Fork::new(Progress::from(prev_haul.remaining_str()), fork_kind);
        Self {
            // SAFETY: Safety conditions of `Fork::next()` guarantee the search of `self.fork_trail` starts at `self.prev_haul.remaining_str()`.
            fork_trail: odor
                .and_then(|odor| fork.next().map(|progress| Trail::new(progress, odor))),
            odors,
            prev_haul,
            fork,
        }
    }
}

impl<'s, 'o> Iterator for Junction<'s, 'o> {
    type Item = Haul<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut haul = None;

        while let Some(ref mut trail) = self.fork_trail {
            haul = trail.next().map(|fork_haul| {
                // Safety conditions of `self.fork_trail` and `self.prev_haul` guarantee safety.
                self.fork.process_haul(&fork_haul);
                let mut my_haul = self.prev_haul.clone();

                #[allow(unsafe_code)]
                // Safety conditions of `self.fork_trail` and `self.prev_haul` guarantee safety.
                unsafe {
                    my_haul.append(fork_haul);
                }

                my_haul
            });

            if haul.is_some() {
                break;
            }

            // SAFETY: Safety conditions of `Fork::next()` guarantee the search of `self.fork_trail` starts at `self.prev_haul.remaining_str()`.
            self.fork_trail = self
                .odors
                .next()
                .and_then(|odor| self.fork.next().map(|progress| Trail::new(progress, odor)));
        }

        haul
    }
}

/// Iterates hunting [`Haul`]s that match an [`Odor`] to a given search.
///
/// `'s` is the lifetime of the search. `'o` is the lifetime of the [`Odor`].
#[derive(Debug)]
struct Trail<'s, 'o> {
    /// The current [`Haul`].
    haul: Haul<'s>,
    /// The [`Odor`] being hunted.
    odor: &'o Odor,
    /// The [`Junction`]s that were passed through on the latest hunt along with the [`Scents`]
    /// that follow them.
    junctions: Vec<(Junction<'s, 'o>, Scents<'o>)>,
    /// If the search has been hunted at least once.
    has_hunted: bool,
}

impl<'s, 'o> Trail<'s, 'o> {
    /// Creates a new [`Trail`].
    fn new(progress: Progress<'s>, odor: &'o Odor) -> Self {
        Self {
            haul: Haul::from(progress),
            odor,
            junctions: Vec::new(),
            has_hunted: false,
        }
    }

    /// Returns the success of hunting for `junction` in the remaining search.
    fn hunt_junction(&mut self, mut junction: Junction<'s, 'o>, scents: &Scents<'o>) -> bool {
        junction.next().map_or(false, |haul| {
            self.haul = haul;
            self.junctions.push((junction, scents.clone()));
            true
        })
    }

    /// Returns if the next [`char`] in the search is in between `begin` and `end`.
    fn is_next_char_in_range(&mut self, begin: char, end: char) -> bool {
        self.haul
            .next_char()
            .map_or(false, |c| (begin..=end).contains(&c))
    }

    /// Returns the success of hunting for `scent` in the remaining search.
    fn hunt_scent(&mut self, scent: &'o Scent, remaining_scents: &Scents<'o>) -> bool {
        match *scent {
            Scent::Char(ch) => self.haul.next_char() == Some(ch),
            Scent::Range(begin, end) => self.is_next_char_in_range(begin, end),
            Scent::Union(ref branches) => self.hunt_junction(
                Junction::new(
                    self.haul.clone(),
                    Odors::list(branches.into_iter()),
                    ForkKind::Stationary,
                ),
                remaining_scents,
            ),
            Scent::Repetition(ref pattern) => self.hunt_junction(
                Junction::new(
                    self.haul.clone(),
                    Odors::repeat_after_empty(pattern),
                    ForkKind::Growing,
                ),
                remaining_scents,
            ),
            Scent::Marked(ref odor) => self.hunt_junction(
                Junction::new(self.haul.clone(), Odors::single(odor), ForkKind::Stationary),
                remaining_scents,
            ),
        }
    }

    /// Returns the success of hunting for `scents` in the remaining search.
    ///
    /// The [`Haul`] of the entire trail is stored in `self.haul`. Any [`Junction`]s that are found
    /// are pushed to `self.junctions`.
    fn hunt_through_scents(&mut self, mut scents: Scents<'o>) -> bool {
        trace!("Searching '{:#?}' for {:#?}", self.haul, scents);

        while let Some(scent) = scents.next() {
            if !(self.hunt_scent(scent, &scents)) {
                return false;
            }
        }

        true
    }

    /// Returns the success of hunting a new [`Haul`] from the stored [`Junction`]s.
    ///
    /// The [`Haul`] of the entire trail is stored in `self.haul`. If a [`Haul`] exists,
    /// `self.junctions` is updated to match it. Otherwise, `self.junctions` will be empty.
    fn rehunt_from_junctions(&mut self) -> bool {
        while let Some((junction, scents)) = self.junctions.pop() {
            if self.hunt_junction(junction, &scents) && self.hunt_through_scents(scents) {
                return true;
            }
        }

        false
    }

    /// Returns the success of hunting for a [`Haul`].
    fn hunt(&mut self) -> bool {
        let success = if self.has_hunted {
            self.rehunt_from_junctions()
        } else {
            self.has_hunted = true;
            self.hunt_through_scents(self.odor.scents()) || self.rehunt_from_junctions()
        };

        if success {
            if let Some(name) = self.odor.name() {
                self.haul.identify_as(name);
            }
        }

        success
    }
}

impl<'s, 'o> Iterator for Trail<'s, 'o> {
    type Item = Haul<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        self.hunt().then(|| self.haul.clone())
    }
}

/// Hunts for a match of an [`Odor`] on a given search.
///
/// '`s' is the lifetime of the search. '`o' is the lifetime of the [`Odor`].
#[derive(Debug)]
pub struct Cur<'s, 'o> {
    /// The [`Odor`] being hunted.
    odor: &'o Odor,
    /// The [`Trail`] of the most recent search.
    trail: Trail<'s, 'o>,
    /// The [`NextCatch`]
    next_catch: NextCatch<'s>,
}

impl<'s, 'o> Cur<'s, 'o> {
    /// Creates a new [`Cur`] that will hunt `odor`.
    #[must_use]
    pub fn new(odor: &'o Odor) -> Self {
        Self {
            trail: Trail::new(Progress::from(""), odor),
            odor,
            next_catch: NextCatch::Unknown,
        }
    }

    /// Sets the search `self` will hunt to `search`.
    pub fn set_search(&mut self, search: &'s str) {
        self.trail = Trail::new(Progress::from(search), self.odor);
        self.next_catch = NextCatch::Unknown;
    }

    /// Returns if `self` cannot find any more matches of its [`Odor`].
    ///
    /// If a match is found, it will be returned by the next call of `self.next()`.
    pub fn is_clear(&mut self) -> bool {
        if self.next_catch.is_unknown() {
            let next_catch = self.next();
            self.next_catch.update(next_catch);
        }

        !self.next_catch.is_success()
    }
}

impl<'s, 'o> Iterator for Cur<'s, 'o> {
    type Item = Catch<'s>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Ok(catch) = self.next_catch.take().try_into() {
            catch
        } else {
            let mut catch = None;

            for mut haul in &mut self.trail {
                if haul.matches_finish() {
                    catch = Some(haul.take_catch());
                    break;
                }
            }

            catch
        }
    }
}
