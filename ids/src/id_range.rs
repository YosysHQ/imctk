use std::{iter::FusedIterator, marker::PhantomData, ops::Range};

use crate::Id;

/// A range of [`Id`] values having contiguous indices.
#[derive(Clone, Copy)]
pub struct IdRange<I> {
    // SAFETY: all indices in start..end must be valid for I
    start: usize,
    end: usize,
    _phantom: PhantomData<I>,
}

impl<I: Id> From<Range<I>> for IdRange<I> {
    #[inline(always)]
    fn from(value: Range<I>) -> Self {
        // SAFETY: Indices are in bounds as they come from an `I` value
        unsafe { Self::from_index_range_unchecked(value.start.index()..value.end.index()) }
    }
}

impl<I: Id> IdRange<I> {
    /// Creates an id range given a corresponding index range.
    ///
    /// # Panics
    ///
    /// Panics when the range contains indices that are not valid for `I`.
    #[inline]
    pub fn from_index_range(range: Range<usize>) -> Self {
        assert!(range.end <= I::MAX_INDEX.saturating_add(1));
        // SAFETY: explicit asserts above
        unsafe { Self::from_index_range_unchecked(range) }
    }

    /// Creates an id range given a corresponding index range without bounds checking.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the range contains only valid indices for `I`.
    #[inline(always)]
    pub unsafe fn from_index_range_unchecked(range: Range<usize>) -> Self {
        let Range { start, end } = range;
        debug_assert!(start <= end);
        debug_assert!(end <= I::MAX_INDEX.saturating_add(1));
        Self {
            start,
            end,
            _phantom: PhantomData,
        }
    }

    /// Returns an iterator over the ids in the range.
    #[inline(always)]
    pub fn iter(&self) -> <Self as IntoIterator>::IntoIter {
        self.into_iter()
    }

    /// Returns the indices present in the id range.
    #[inline(always)]
    pub fn indices(&self) -> Range<usize> {
        self.start..self.end
    }
}

impl<I: Id> IntoIterator for IdRange<I> {
    type Item = I;

    type IntoIter =
        impl FusedIterator<Item = I> + ExactSizeIterator + DoubleEndedIterator + Clone + 'static;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        // SAFETY: safe by struct level invariant
        (self.start..self.end).map(|index| unsafe { I::from_index_unchecked(index) })
    }
}

impl<I: Id> IntoIterator for &IdRange<I> {
    type Item = I;

    type IntoIter = <IdRange<I> as IntoIterator>::IntoIter;

    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        (*self).into_iter()
    }
}
