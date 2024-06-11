use core::fmt;
use std::mem::MaybeUninit;

use hashbrown::HashTable;

use super::{iter::SubtableIterMut, table::SMALL_SUBTABLE_CAPACITY, SubtableIter};

#[cfg(doc)]
use crate::TableSeq;

pub(crate) struct OwnedSubtableSmall<T> {
    values: [MaybeUninit<T>; SMALL_SUBTABLE_CAPACITY],
    hashes: [u8; SMALL_SUBTABLE_CAPACITY],
    len: u8,
    have_hashes: bool,
}

impl<T> Default for OwnedSubtableSmall<T> {
    fn default() -> Self {
        Self {
            values: unsafe { MaybeUninit::uninit().assume_init() },
            hashes: [0; SMALL_SUBTABLE_CAPACITY],
            len: 0,
            have_hashes: true,
        }
    }
}
impl<T> Drop for OwnedSubtableSmall<T> {
    fn drop(&mut self) {
        unsafe {
            std::ptr::slice_from_raw_parts_mut(self.values.as_mut_ptr().cast::<T>(), self.len())
                .drop_in_place()
        }
    }
}

impl<T> OwnedSubtableSmall<T> {
    pub unsafe fn push_with_hash_unchecked(&mut self, value: T, hash: u8) {
        debug_assert!((self.len as usize) < SMALL_SUBTABLE_CAPACITY);
        self.values
            .get_unchecked_mut(self.len as usize)
            .write(value);
        *self.hashes.get_unchecked_mut(self.len as usize) = hash;
        self.len += 1;
    }

    pub unsafe fn push_unchecked(&mut self, value: T) {
        debug_assert!((self.len as usize) < SMALL_SUBTABLE_CAPACITY);
        self.values
            .get_unchecked_mut(self.len as usize)
            .write(value);
        self.have_hashes = false;
        self.len += 1;
    }

    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            None
        } else {
            self.len -= 1;
            Some(unsafe {
                self.values
                    .get_unchecked_mut(self.len as usize)
                    .assume_init_read()
            })
        }
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn entries(&self) -> &[T] {
        unsafe { std::slice::from_raw_parts(self.values.as_ptr().cast::<T>(), self.len()) }
    }

    pub fn entries_mut(&mut self) -> &mut [T] {
        unsafe { std::slice::from_raw_parts_mut(self.values.as_mut_ptr().cast::<T>(), self.len()) }
    }
}

/// Structure holding an individually owned [`TableSeq`] subtable.
///
/// While an [`TableSeq`] is optimized for reduced memory usage, this type always has the
/// capacity to store up to 16 entries in-line and thus should only be used as temporary storage for
/// passing or returning full subtables.
pub struct OwnedSubtable<T> {
    inner: OwnedSubtableInner<T>,
}

impl<T: fmt::Debug> fmt::Debug for OwnedSubtable<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<T> Default for OwnedSubtable<T> {
    fn default() -> Self {
        Self {
            inner: OwnedSubtableInner::Small(Default::default()),
        }
    }
}

impl<T> OwnedSubtable<T> {
    pub(crate) fn single(value: T) -> Self {
        let mut values: OwnedSubtableSmall<T> = Default::default();
        unsafe { values.push_unchecked(value) };
        Self {
            inner: OwnedSubtableInner::Small(values),
        }
    }

    pub(crate) fn pair(pair: [T; 2]) -> Self {
        let [first, second] = pair;
        let mut values: OwnedSubtableSmall<T> = Default::default();
        unsafe { values.push_unchecked(first) };
        unsafe { values.push_unchecked(second) };
        Self {
            inner: OwnedSubtableInner::Small(values),
        }
    }

    pub(crate) fn small(values: OwnedSubtableSmall<T>) -> Self {
        Self {
            inner: OwnedSubtableInner::Small(values),
        }
    }

    pub(crate) fn table(table: HashTable<T>) -> Self {
        Self {
            inner: OwnedSubtableInner::Table(table),
        }
    }

    /// Returns the number of entries in this subtable.
    pub fn len(&self) -> usize {
        match &self.inner {
            OwnedSubtableInner::Table(table) => table.len(),
            OwnedSubtableInner::Small(small) => small.len(),
        }
    }

    /// Returns whether this subtable is empty.
    pub fn is_empty(&self) -> bool {
        match &self.inner {
            OwnedSubtableInner::Table(table) => table.is_empty(),
            OwnedSubtableInner::Small(small) => small.is_empty(),
        }
    }

    /// Returns an iterator yielding references to all subtable entries.
    ///
    /// The subtable maintains a fixed iteration order that only changes with mutations. Beyond
    /// that, the iteration order is unspecified.
    pub fn iter(&self) -> SubtableIter<T> {
        self.into_iter()
    }

    /// Returns an iterator yielding mutable references to all subtable entries.
    ///
    /// The subtable maintains a fixed iteration order that only changes with mutations. Beyond
    /// that, the iteration order is unspecified.
    pub fn iter_mut(&mut self) -> SubtableIterMut<T> {
        self.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a OwnedSubtable<T> {
    type Item = &'a T;

    type IntoIter = SubtableIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        match &self.inner {
            OwnedSubtableInner::Table(table) => SubtableIter::table(table),
            OwnedSubtableInner::Small(small) => SubtableIter::small(small.entries()),
        }
    }
}

impl<'a, T> IntoIterator for &'a mut OwnedSubtable<T> {
    type Item = &'a mut T;

    type IntoIter = SubtableIterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        match &mut self.inner {
            OwnedSubtableInner::Table(table) => SubtableIterMut::table(table),
            OwnedSubtableInner::Small(small) => SubtableIterMut::small(small.entries_mut()),
        }
    }
}

impl<T> IntoIterator for OwnedSubtable<T> {
    type Item = T;

    type IntoIter = SubtableIntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self.inner {
            OwnedSubtableInner::Table(table) => SubtableIntoIter::table(table),
            OwnedSubtableInner::Small(small) => SubtableIntoIter::small(small),
        }
    }
}

enum OwnedSubtableInner<T> {
    Small(OwnedSubtableSmall<T>),
    Table(HashTable<T>),
}

pub struct SubtableIntoIter<T> {
    inner: SubtableIntoIterInner<T>,
}

impl<T> SubtableIntoIter<T> {
    pub(crate) fn small(table: OwnedSubtableSmall<T>) -> Self {
        Self {
            inner: SubtableIntoIterInner::Small(table),
        }
    }

    pub(crate) fn table(table: HashTable<T>) -> Self {
        Self {
            inner: SubtableIntoIterInner::Table(table.into_iter()),
        }
    }
}

enum SubtableIntoIterInner<T> {
    Small(OwnedSubtableSmall<T>),
    Table(hashbrown::hash_table::IntoIter<T>),
}

impl<T> Iterator for SubtableIntoIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            SubtableIntoIterInner::Small(small) => small.pop(),
            SubtableIntoIterInner::Table(table) => table.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }
}

impl<T> ExactSizeIterator for SubtableIntoIter<T> {
    fn len(&self) -> usize {
        match &self.inner {
            SubtableIntoIterInner::Small(small) => small.len(),
            SubtableIntoIterInner::Table(table) => table.len(),
        }
    }
}
