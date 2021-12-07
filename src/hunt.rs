//! Defines items used for hunting matches in a search.
use {
    crate::pattern::Name,
    alloc::collections::{BTreeMap, VecDeque},
    core::mem,
    log::warn,
};

/// A length in bytes.
pub(crate) type ByteLen = usize;

/// Iterates the [`char`]s of a search while tracking how far the iteration has traveled.
///
/// `'s` is the lifetime of the search.
#[derive(Clone, Debug, Default)]
pub(crate) struct Progress<'s> {
    /// The search.
    search: &'s str,
    /// The number of bytes that have been traveled.
    ///
    /// # Safety
    ///
    /// This value must equal a UTF-8 boundary of `search`.
    traveled: ByteLen,
}

impl<'s> Progress<'s> {
    /// Returns if `self` is at the finish of its search.
    const fn is_at_finish(&self) -> bool {
        self.traveled == self.search.len()
    }

    /// Returns the part of the search `self` has already traveled.
    fn traveled_str(&self) -> &'s str {
        #[allow(unsafe_code)] // Each modification of self.traveled must be validated.
        unsafe {
            self.search.get_unchecked(..self.traveled)
        }
    }

    /// Returns the part of the search `self` has not traveled.
    fn remaining_str(&self) -> &'s str {
        #[allow(unsafe_code)] // Each modification of self.traveled must be validated.
        unsafe {
            self.search.get_unchecked(self.traveled..)
        }
    }

    /// Increments the number of bytes `self` has traveled by `value`.
    ///
    /// # Safety
    ///
    /// The new traveled value must equal a UTF-8 boundary of the search.
    #[allow(unsafe_code)] // Caller must meet safety conditions.
    unsafe fn increment(&mut self, value: ByteLen) {
        #[allow(clippy::integer_arithmetic)]
        {
            // If safety conditions are met, overflow is not possible.
            self.traveled += value;
        }
    }
}

impl<'s> From<&'s str> for Progress<'s> {
    fn from(search: &'s str) -> Self {
        Self {
            search,
            // SAFETY: 0 is a safe value for any search.
            traveled: 0,
        }
    }
}

impl<'s> Iterator for Progress<'s> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let next_item = self.remaining_str().chars().next();

        if let Some(next_char) = next_item {
            let next_char_byte_len = next_char.len_utf8();

            #[allow(unsafe_code)]
            // next_char is the char that starts at self.traveled. Thus, incrementing self.traveled by the byte length of next_char is safe.
            unsafe {
                self.increment(next_char_byte_len);
            }
        }

        next_item
    }
}

/// The part of a search that matches a marked [`Odor`].
///
/// `'s` is the lifetime of the search.
#[derive(Clone, Debug)]
pub struct Find<'s> {
    /// The matching part.
    found: &'s str,
    /// The byte index of the search at which `found` begins.
    ///
    /// # Safety
    ///
    /// This value must equal a UTF-8 boundary of the search.
    begin: ByteLen,
}

impl<'s> Find<'s> {
    /// Returns the byte index of the beginning of `self` in the search.
    #[must_use]
    pub const fn begin(&self) -> ByteLen {
        self.begin
    }

    /// Returns if `self` is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.found.is_empty()
    }

    /// Returns the length of `self`.
    #[must_use]
    pub const fn len(&self) -> ByteLen {
        self.found.len()
    }

    /// Returns the byte index of the ending of `self` in the search.
    #[must_use]
    pub const fn end(&self) -> ByteLen {
        #[allow(clippy::integer_arithmetic)]
        {
            // If safety conditions are met, overflow is not possible.
            self.begin() + self.len()
        }
    }

    /// Returns the [`&str`] of `self`.
    #[must_use]
    pub const fn as_str(&self) -> &str {
        self.found
    }

    /// Increments the beginning of `self` by `value`.
    ///
    /// # Safety
    ///
    /// The new beginning must be a UTF-8 boundary of the search.
    #[allow(unsafe_code)] // Caller must meet safety conditions.
    pub(crate) unsafe fn increment(&mut self, value: ByteLen) {
        #[allow(clippy::integer_arithmetic)]
        {
            // If safety conditions are met, then overflow is not possible.
            self.begin += value;
        }
    }
}

impl<'s> From<&Progress<'s>> for Find<'s> {
    fn from(progress: &Progress<'s>) -> Self {
        Self {
            found: progress.traveled_str(),
            // SAFETY: 0 is a safe value for any search.
            begin: 0,
        }
    }
}

/// The kind of [`Fork`].
#[derive(Debug)]
pub(crate) enum ForkKind {
    /// The [`Fork`] always returns the same [`Progress`].
    Stationary,
    /// The [`Fork`] can add more [`Progress`]es that travel further along the search.
    Growing,
}

/// Iterates over [`Progress`]es.
#[derive(Debug)]
pub(crate) struct Fork<'s> {
    /// The queue of [`Progress`]es that can be hunted.
    progress_queue: VecDeque<Progress<'s>>,
    /// The [`ForkKind`].
    kind: ForkKind,
}

impl<'s> Fork<'s> {
    /// Creates a new [`Fork`].
    pub(crate) fn new(progress: Progress<'s>, kind: ForkKind) -> Self {
        Self {
            progress_queue: VecDeque::from([progress]),
            kind,
        }
    }

    /// Updates `self` with `haul`.
    ///
    /// # Safety
    ///
    /// `haul` must have the same search as the `progress` given to [`Fork::new()`].
    pub(crate) fn process_haul(&mut self, haul: &Haul<'s>) {
        if let ForkKind::Growing = self.kind {
            self.progress_queue.push_back(haul.progress_cloned());
        }
    }
}

impl<'s> Iterator for Fork<'s> {
    type Item = Progress<'s>;

    // SAFETY: If returning [`Some`], the inner [`Progress`] has the same search as the `progress`
    // given to `Fork::new()`.
    fn next(&mut self) -> Option<Self::Item> {
        match self.kind {
            ForkKind::Stationary => self.progress_queue.front().cloned(),
            ForkKind::Growing => self.progress_queue.pop_front(),
        }
    }
}

/// Associates [`Find`]s with their respective [`Name`]s.
///
/// `'s` is the lifetime of the search.
pub(crate) type Catch<'s> = BTreeMap<Name, Find<'s>>;

/// Information about the match of an [`Odor`] with part of a search.
///
/// `'s` is the lifetime of the search.
#[derive(Clone, Debug, Default)]
pub(crate) struct Haul<'s> {
    /// The [`Progress`] of the hunt.
    progress: Progress<'s>,
    /// All [`Find`]s within the matching [`Odor`].
    catch: Catch<'s>,
}

impl<'s> Haul<'s> {
    /// Returns the byte length of the match.
    pub(crate) const fn len(&self) -> ByteLen {
        self.progress.traveled
    }

    /// Returns if `self` matches the finish of the search.
    pub(crate) const fn matches_finish(&self) -> bool {
        self.progress.is_at_finish()
    }

    /// Returns the part of the search `self` has not traveled.
    pub(crate) fn remaining_str(&self) -> &'s str {
        self.progress.remaining_str()
    }

    /// Returns a clone of the [`Progress`] of `self`.
    pub(crate) fn progress_cloned(&self) -> Progress<'s> {
        self.progress.clone()
    }

    /// Associates a [`Find`] of the match of `self` with `name` in the [`Catch`] of `self`.
    pub(crate) fn identify_as(&mut self, name: Name) {
        if self
            .catch
            .insert(name, Find::from(&self.progress))
            .is_some()
        {
            warn!("Found 2 Finds for Odors with the same Name.");
        }
    }

    /// Appends `other` to `self`.
    ///
    /// The [`Catch`] of `other` is updated to the search of `self` and added to `self`. The
    /// [`Progress`] of `self` is updated to include the match of `other`.
    ///
    /// # Safety
    ///
    /// - The search of `other` must equal `self.remaining_str()`.
    #[allow(unsafe_code)] // Caller must meet safety conditions.
    pub(crate) unsafe fn append(&mut self, mut other: Haul<'s>) {
        for find in other.catch.values_mut() {
            find.increment(self.len());
        }

        self.catch.append(&mut other.catch);
        self.progress.increment(other.len());
    }

    /// Returns the next [`char`] of `self`.
    pub(crate) fn next_char(&mut self) -> Option<char> {
        self.progress.next()
    }

    /// Replaces the [`Catch`] of `self` with an empty one and returns the [`Catch`].
    pub(crate) fn take_catch(&mut self) -> Catch<'s> {
        mem::take(&mut self.catch)
    }
}

impl<'s> From<Progress<'s>> for Haul<'s> {
    fn from(progress: Progress<'s>) -> Self {
        Self {
            progress,
            catch: Catch::new(),
        }
    }
}
