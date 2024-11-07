//! An allocator for IDs that allows concurrent access from multiple threads.
use std::sync::atomic::Ordering::Relaxed;
use std::{marker::PhantomData, sync::atomic::AtomicUsize};

use crate::{Id, IdRange};

/// An allocator for IDs that allows concurrent access from multiple threads.
pub struct IdAlloc<T> {
    counter: AtomicUsize,
    _phantom: PhantomData<T>,
}

impl<T: Id> Default for IdAlloc<T> {
    fn default() -> Self {
        Self::new()
    }
}

/// `IdAllocError` indicates that there are not enough IDs remaining.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IdAllocError;

impl<T: Id> IdAlloc<T> {
    /// Constructs a new ID allocator.
    pub const fn new() -> Self {
        Self {
            counter: AtomicUsize::new(0),
            _phantom: PhantomData,
        }
    }
    fn alloc_indices(&self, n: usize) -> Result<usize, IdAllocError> {
        self.counter
            .fetch_update(Relaxed, Relaxed, |current_id| {
                current_id
                    .checked_add(n)
                    .filter(|&index| index <= T::MAX_ID_INDEX.saturating_add(1))
            })
            .map_err(|_| IdAllocError)
    }
    /// Allocates a single ID.
    pub fn alloc(&self) -> Result<T, IdAllocError> {
        self.alloc_indices(1).map(|index| {
            // SAFETY: the precondition was checked by `alloc_indices`
            unsafe { T::from_id_index_unchecked(index) }
        })
    }
    /// Allocates a contiguous range of the specified size.
    pub fn alloc_range(&self, n: usize) -> Result<IdRange<T>, IdAllocError> {
        self.alloc_indices(n).map(|start| {
            // SAFETY: the precondition was checked by `alloc_indices`
            unsafe { IdRange::from_index_range_unchecked(start..start + n) }
        })
    }
    /// Returns the ID that would be allocated by the next call to `alloc`.
    pub fn peek(&self) -> Result<T, IdAllocError> {
        T::try_from_id_index(self.counter.load(Relaxed)).ok_or(IdAllocError)
    }
}
