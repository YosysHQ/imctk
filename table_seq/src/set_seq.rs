//! Indexed sequence of hash sets and associated helper types.
use core::fmt;
use std::{
    borrow::Borrow,
    hash::{BuildHasher, Hash},
    mem::replace,
};

use crate::table_seq::{SubtableIter, TableSeq};

/// Iterator yielding references to a set's elements.
pub struct SetIter<'a, T> {
    inner: SubtableIter<'a, T>,
}

impl<T> Default for SetIter<'_, T> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<'a, T> Iterator for SetIter<'a, T> {
    type Item = &'a T;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next()
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<T> ExactSizeIterator for SetIter<'_, T> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

/// Indexed sequence of hash sets.
///
/// This type serves as a memory and runtime efficient replacement for `Vec<HashSet<T>>`. In
/// particular, it is optimized for the use-case where the vast majority of contained sets are tiny,
/// each having 16 or fewer entries, while still allowing for a small but significant fraction of
/// tables to be large.
pub struct SetSeq<T, S> {
    tables: TableSeq<T>,
    build_hasher: S,
}

impl<T, S: Default> Default for SetSeq<T, S> {
    fn default() -> Self {
        Self {
            tables: Default::default(),
            build_hasher: Default::default(),
        }
    }
}

impl<T: fmt::Debug, S> fmt::Debug for SetSeq<T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(&self.tables, f)
    }
}

impl<T, S> SetSeq<T, S> {
    /// Returns the number of sets in the sequence.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.tables.len()
    }

    /// Returns `true` if the sequence of sets is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }

    /// Discards all sets in the sequence.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.tables.clear()
    }

    /// Resizes the sequence by appending empty sets or discarding trailing sets.
    #[inline(always)]
    pub fn resize(&mut self, sets: usize) {
        self.tables.resize(sets)
    }

    /// Ensures that the sequence contains a set at the given index by appending emtpy sets if the
    /// sequence was too short.
    ///
    /// Provides mutable access to the set at the given index.
    #[inline(always)]
    pub fn grow_for(&mut self, set: usize) -> SetSeqSetMut<T, S> {
        self.tables.grow_for_subtable(set);
        SetSeqSetMut { seq: self, set }
    }

    /// Provides shared access to the set at a given index, panics if out-of-bounds.
    ///
    /// This is used instead of [`std::ops::Index`], as it returns a value of the custom
    /// reference-like [`SetSeqSet`] type.
    ///
    /// Panics if `set >= self.len()`.
    #[inline(always)]
    pub fn at(&self, set: usize) -> SetSeqSet<T, S> {
        assert!(self.tables.len() > set);
        SetSeqSet { seq: self, set }
    }

    /// Provides mutable access to the set at a given index, panics if out-of-bounds.
    ///
    /// This is used instead of [`std::ops::IndexMut`], as it returns a value of the custom
    /// reference-like [`SetSeqSetMut`] type.
    ///
    /// Panics if `set >= self.len()`.
    #[inline(always)]
    pub fn at_mut(&mut self, set: usize) -> SetSeqSetMut<T, S> {
        assert!(self.tables.len() > set);
        SetSeqSetMut { seq: self, set }
    }

    /// Provides shared access to the set at a given index.
    ///
    /// This returns `None` if `set >= self.len()`.
    #[inline(always)]
    pub fn get(&self, set: usize) -> Option<SetSeqSet<T, S>> {
        (self.tables.len() > set).then_some(SetSeqSet { seq: self, set })
    }

    /// Provides mutable access to the set at a given index.
    ///
    /// This returns `None` if `set >= self.len()`.
    #[inline(always)]
    pub fn get_mut(&mut self, set: usize) -> Option<SetSeqSetMut<T, S>> {
        (self.tables.len() > set).then_some(SetSeqSetMut { seq: self, set })
    }
}

impl<T, S> SetSeq<T, S> {
    #[inline(always)]
    fn set_len(&self, set: usize) -> usize {
        self.tables.subtable_len(set)
    }

    #[inline(always)]
    fn set_is_empty(&self, set: usize) -> bool {
        self.set_len(set) != 0
    }

    #[inline(always)]
    fn clear_set(&mut self, set: usize) {
        self.tables.clear_subtable(set)
    }
}

impl<T: Eq + Hash, S: BuildHasher> SetSeq<T, S> {
    fn set_contains<Q>(&self, set: usize, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);
        self.tables
            .find(set, hash, |found| found.borrow() == value)
            .is_some()
    }

    fn set_get<Q>(&self, set: usize, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);
        self.tables.find(set, hash, |found| found.borrow() == value)
    }

    fn set_insert(&mut self, set: usize, value: T) -> bool {
        let hash = self.build_hasher.hash_one(&value);
        self.tables
            .insert(
                set,
                hash,
                value,
                |found, inserting| found == inserting,
                |found| self.build_hasher.hash_one(found),
            )
            .1
            .is_none()
    }

    fn set_replace(&mut self, set: usize, value: T) -> Option<T> {
        let hash = self.build_hasher.hash_one(&value);
        let (entry, rejected) = self.tables.insert(
            set,
            hash,
            value,
            |found, inserting| found == inserting,
            |found| self.build_hasher.hash_one(found),
        );
        rejected.map(|value| replace(entry, value))
    }

    fn set_insert_unique_unchecked(&mut self, set: usize, value: T) {
        let hash = self.build_hasher.hash_one(&value);
        self.tables
            .insert_unique(set, hash, value, |found| self.build_hasher.hash_one(found));
    }

    fn set_remove<Q>(&mut self, set: usize, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);

        self.tables
            .remove(
                set,
                hash,
                |found| found.borrow() == value,
                |found| self.build_hasher.hash_one(found),
            )
            .is_some()
    }

    fn set_take<Q>(&mut self, set: usize, value: &Q) -> Option<T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);

        self.tables.remove(
            set,
            hash,
            |found| found.borrow() == value,
            |found| self.build_hasher.hash_one(found),
        )
    }
}

/// Exclusive mutable access to a set of a [`SetSeq`].
#[repr(C)] // SAFETY: layout must be compatible with SetSeqSet
pub struct SetSeqSetMut<'a, T, S> {
    seq: &'a mut SetSeq<T, S>,
    set: usize,
}

/// Shared read-only access to a set of a [`SetSeq`].
#[derive(Clone, Copy)]
#[repr(C)] // SAFETY: layout must be compatible with SetSeqSetMut
pub struct SetSeqSet<'a, T, S> {
    seq: &'a SetSeq<T, S>,
    set: usize,
}

impl<'a, T, S> std::ops::Deref for SetSeqSetMut<'a, T, S> {
    type Target = SetSeqSet<'a, T, S>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: we have compatible repr(C) layouts between SetSeqSet and SetSeqSetMut
        unsafe { &*(self as *const Self).cast() }
    }
}

impl<T: fmt::Debug, S> fmt::Debug for SetSeqSet<'_, T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<T: fmt::Debug, S> fmt::Debug for SetSeqSetMut<'_, T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<'a, T, S> SetSeqSet<'a, T, S> {
    /// Returns the number of elements the set contains.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.seq.set_len(self.set)
    }

    /// Returns `true` when the set is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.seq.set_is_empty(self.set)
    }

    /// Returns an iterator over the elements of the set.
    #[inline(always)]
    pub fn iter(&self) -> SetIter<'a, T> {
        SetIter {
            inner: self.seq.tables.subtable_iter(self.set),
        }
    }
}

impl<T, S> SetSeqSetMut<'_, T, S> {
    /// Discards all elements of the set.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.seq.clear_set(self.set)
    }
}

impl<T: Eq + Hash, S: BuildHasher> SetSeqSet<'_, T, S> {
    /// Checks whether a given value is an element of the set.
    #[inline(always)]
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.set_contains(self.set, value)
    }

    /// Returns a reference to a given value of the set.
    #[inline(always)]
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.set_get(self.set, value)
    }
}

impl<T: Eq + Hash, S: BuildHasher> SetSeqSetMut<'_, T, S> {
    /// Inserts a value into the set.
    ///
    /// If the value is already present, the given value is discarded and the set is not modified.
    ///
    /// Returns `true` when a new element was inserted and `false` when the value was already
    /// present.
    #[inline(always)]
    pub fn insert(&mut self, value: T) -> bool {
        self.seq.set_insert(self.set, value)
    }

    /// Inserts a value into the set, replacing an existing equal value.
    ///
    /// If the value is already present, the already present value is removed from the set before
    /// inserting the new value.
    ///
    /// Returns the already present value that was removed or `None` if the value was not present
    /// before.
    #[inline(always)]
    pub fn replace(&mut self, value: T) -> Option<T> {
        self.seq.set_replace(self.set, value)
    }

    /// Inserts a value into the set, assuming it is not yet present.
    ///
    /// It is an error to call this with a value already present. Doing so results in unspecified
    /// (but still safe) behavior.
    #[inline(always)]
    pub fn insert_unique_unchecked(&mut self, value: T) {
        self.seq.set_insert_unique_unchecked(self.set, value)
    }

    /// Removes a given value from the set.
    ///
    /// Returns `true` when the value was removed and `false` if the value was not present.
    #[inline(always)]
    pub fn remove<Q>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.set_remove(self.set, value)
    }

    /// Removes and returns a given value from the set.
    ///
    /// Returns `None` when the given value was not present.
    #[inline(always)]
    pub fn take<Q>(&mut self, value: &Q) -> Option<T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.set_take(self.set, value)
    }
}
