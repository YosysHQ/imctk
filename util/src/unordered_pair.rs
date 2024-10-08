//! Unordered pairs of [`Ord`] elements.
use std::ops;

/// An unordered pair represented as a sorted `[T; 2]`.
///
/// Note that this does allow a pair containing the same value twice.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct UnorderedPair<T> {
    values: [T; 2],
}

impl<T> ops::Deref for UnorderedPair<T> {
    type Target = [T; 2];

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}

impl<T: Ord> From<[T; 2]> for UnorderedPair<T> {
    fn from(values: [T; 2]) -> Self {
        Self::new(values)
    }
}

impl<T> From<UnorderedPair<T>> for [T; 2] {
    fn from(pair: UnorderedPair<T>) -> Self {
        pair.values
    }
}

impl<T: std::fmt::Debug> std::fmt::Debug for UnorderedPair<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(&self.values).finish()
    }
}

impl<T: Ord> UnorderedPair<T> {
    /// Creates a new unordered pair by sorting two values.
    pub fn new(values: [T; 2]) -> Self {
        let [a, b] = values;

        if a <= b {
            Self { values: [a, b] }
        } else {
            Self { values: [b, a] }
        }
    }

    /// Returns a reference to the smaller of the two elements.
    pub fn min_element(&self) -> &T {
        &self.values[0]
    }

    /// Returns a reference to the larger of the two elements.
    pub fn max_element(&self) -> &T {
        &self.values[1]
    }

    /// Applies a function to the two elements, returning the results as a new [`UnorderedPair`].
    pub fn map<U: Ord>(self, f: impl FnMut(T) -> U) -> UnorderedPair<U> {
        UnorderedPair::new(self.values.map(f))
    }

    /// Returns the two elements as a sorted array.
    pub fn into_values(self) -> [T; 2] {
        self.values
    }
}
