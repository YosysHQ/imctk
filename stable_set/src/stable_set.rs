//! [StableSet] is a a hash set that maintains the order of its elements.
use crate::{
    index_table,
    util::{impl_iterator, simplify_range},
};
use core::hash::Hash;
use index_table::{IndexTable, SmallIndex};
use std::{borrow::Borrow, hash::BuildHasher, ops::RangeBounds};

/// A hash set that maintains the order of its elements.
///
/// In `StableSet<T, S, W>`,
/// `T: Hash + Eq` is the type of elements of the set,
/// `S: BuildHasher` is used for hashing elements
/// and `W: SmallIndex` is the type used for small indices internally (`W` should usually be omitted, it then defaults to `u32`).
#[derive(Clone)]
pub struct StableSet<T, S, W = u32> {
    index_table: IndexTable<W>,
    items: Vec<T>,
    build_hasher: S,
}

impl<T: std::fmt::Debug, S, W> std::fmt::Debug for StableSet<T, S, W> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self).finish()
    }
}

impl<T, S: Default, W> Default for StableSet<T, S, W> {
    fn default() -> Self {
        StableSet {
            index_table: IndexTable::default(),
            items: Vec::new(),
            build_hasher: S::default(),
        }
    }
}

impl<T, S: Default, W: SmallIndex> StableSet<T, S, W> {
    /// Returns an empty set.
    pub fn new() -> Self {
        Self::default()
    }
    /// Returns an empty set with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        StableSet {
            index_table: IndexTable::with_capacity(capacity),
            items: Vec::with_capacity(capacity),
            build_hasher: S::default(),
        }
    }
}

impl<T, S, W: SmallIndex> StableSet<T, S, W> {
    /// Returns an empty set with the provided BuildHasher.
    pub fn with_hasher(build_hasher: S) -> Self {
        StableSet {
            index_table: IndexTable::default(),
            items: Vec::new(),
            build_hasher,
        }
    }
    /// Returns an empty set with the specified capacity and provided BuildHasher.
    pub fn with_capacity_and_hasher(capacity: usize, build_hasher: S) -> Self {
        StableSet {
            index_table: IndexTable::with_capacity(capacity),
            items: Vec::with_capacity(capacity),
            build_hasher,
        }
    }
    /// Removes all items, but keeps the allocated memory.
    pub fn clear(&mut self) {
        self.index_table.clear();
        self.items.clear();
    }
    /// Removes all items except for the first `len` items, but keeps the allocated memory.
    pub fn truncate(&mut self, len: usize) {
        self.index_table
            .retain(|index| (index < len).then_some(index));
        self.items.truncate(len);
    }
    /// Returns the number of items in the set.
    pub fn len(&self) -> usize {
        self.items.len()
    }
    /// Returns `true` if the set is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    /// Returns the first item in the set, if it exists.
    pub fn first(&self) -> Option<&T> {
        self.items.first()
    }
    /// Returns the last item in the set, if it exists.
    pub fn last(&self) -> Option<&T> {
        self.items.last()
    }
    /// Returns a reference to a slice containing all items in the set.
    pub fn as_slice(&self) -> &[T] {
        &self.items[..]
    }
    /// Converts the set into a `Vec`. The hashtables containing the indices is dropped.
    pub fn into_vec(self) -> Vec<T> {
        self.items
    }
}

impl<T: Hash, S: BuildHasher, W: SmallIndex> StableSet<T, S, W> {
    /// Reserve memory for an extra `additional` items.
    pub fn reserve(&mut self, additional: usize) {
        self.items.reserve(additional);
        self.index_table.reserve(additional, |index| {
            self.build_hasher.hash_one(&self.items[index])
        });
    }
}

impl<T: Hash + Eq, S: BuildHasher, W: SmallIndex> StableSet<T, S, W> {
    /// Removes the last item from the set and returns it, if it exists.
    pub fn pop(&mut self) -> Option<T> {
        let opt_item = self.items.pop();
        if let Some(item) = &opt_item {
            let hash = self.build_hasher.hash_one(item);
            self.index_table
                .find_entry(hash, |index| index == self.items.len())
                .unwrap()
                .remove();
        }
        opt_item
    }
    /// Inserts an item to the end of the set, unless the set contains an equivalent item already.
    /// Returns `true` if the item was inserted.
    pub fn insert(&mut self, value: T) -> bool {
        self.insert_full(value).1
    }
    /// Inserts an item to the end of the set, unless the set contains an equivalent item already.
    /// Returns the index of the existing or new item, and `true` if the given item was inserted.
    pub fn insert_full(&mut self, value: T) -> (usize, bool) {
        self.index_table.grow_for(self.items.len(), |index| {
            self.build_hasher.hash_one(&self.items[index])
        });
        let hash = self.build_hasher.hash_one(&value);
        match self.index_table.entry(
            hash,
            |index| self.items[index] == value,
            |index| self.build_hasher.hash_one(&self.items[index]),
        ) {
            index_table::Entry::Occupied(entry) => (entry.get(), false),
            index_table::Entry::Vacant(entry) => {
                let new_index = self.items.len();
                self.items.push(value);
                entry.insert(new_index);
                (new_index, true)
            }
        }
    }
    /// Returns a reference to the item in the set equivalent to `value`, if it exists.
    pub fn get<Q>(&self, value: &Q) -> Option<&T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_full(value).map(|x| x.1)
    }
    /// Returns the index of and a reference to the item in the set equivalent to `value`, if it exists.
    pub fn get_full<Q>(&self, value: &Q) -> Option<(usize, &T)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);
        self.index_table
            .find(hash, |index| self.items[index].borrow() == value)
            .map(|index| (index, &self.items[index]))
    }
    /// Returns `true` if the set contains a value equivalent to `value`.
    pub fn contains<Q>(&self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_full(value).is_some()
    }
    /// Returns the item with the given index, if it exists.
    pub fn get_index(&self, index: usize) -> Option<&T> {
        self.items.get(index)
    }
    /// Returns the index of the item equivalent to `value`, if it exists.
    pub fn get_index_of<Q>(&self, value: &Q) -> Option<usize>
    where
        Q: Hash + Eq + ?Sized,
        T: Borrow<Q>,
    {
        let hash = self.build_hasher.hash_one(value);
        self.index_table
            .find(hash, |index| self.items[index].borrow() == value)
    }
    fn swap_remove_finish(&mut self, index: usize) -> T {
        let item = self.items.swap_remove(index);
        if index < self.items.len() {
            let swapped_hash = self.build_hasher.hash_one(&self.items[index]);
            self.index_table
                .replace(swapped_hash, |i| i == self.items.len(), index)
                .unwrap();
        }
        item
    }
    /// Removes the specified value from the set, if it exists. Returns `true` if an item was removed.
    ///
    /// The last item is put in its place.
    pub fn swap_remove<Q>(&mut self, value: &Q) -> bool
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.swap_remove_full(value).is_some()
    }
    /// Removes the specified value from the set and returns it, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_take<Q>(&mut self, value: &Q) -> Option<T>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.swap_remove_full(value).map(|x| x.1)
    }
    /// Removes the specified value from the set and returns its index and it, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_remove_full<Q>(&mut self, value: &Q) -> Option<(usize, T)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);
        match self
            .index_table
            .find_entry(hash, |index| self.items[index].borrow() == value)
        {
            Ok(entry) => {
                let index = entry.remove().0;
                let value = self.swap_remove_finish(index);
                Some((index, value))
            }
            Err(_) => None,
        }
    }
    /// Removes the item at the given index and returns it, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_remove_index(&mut self, index: usize) -> Option<T> {
        let hash = self.build_hasher.hash_one(self.items.get(index)?);
        self.index_table
            .find_entry(hash, |i| i == index)
            .unwrap()
            .remove();
        Some(self.swap_remove_finish(index))
    }
    /// Returns `true` if the set is a subset of `other`.
    ///
    /// The order of elements is ignored.
    pub fn is_subset<S2: BuildHasher>(&self, other: &StableSet<T, S2, W>) -> bool {
        self.len() <= other.len() && self.iter().all(|t| other.contains(t))
    }
    /// Removes all items from the set for which `f` evaluates to `false`.
    ///
    /// `f` is guaranteed to be called exactly once for each item and in order.
    ///
    /// The order of elements is preserved.
    pub fn retain(&mut self, mut f: impl FnMut(&T) -> bool) {
        let mut in_index = 0;
        let mut out_index = 0;
        self.items.retain(|item| {
            let hash = self.build_hasher.hash_one(item);
            let keep = f(item);
            if keep {
                if in_index != out_index {
                    self.index_table
                        .replace(hash, |index| index == in_index, out_index)
                        .unwrap();
                }
                out_index += 1;
            } else {
                self.index_table
                    .find_entry(hash, |index| index == in_index)
                    .unwrap()
                    .remove();
            }
            in_index += 1;
            keep
        })
    }
}

impl<T: Hash + Eq, S1: BuildHasher, S2: BuildHasher, W: SmallIndex> PartialEq<StableSet<T, S2, W>>
    for StableSet<T, S1, W>
{
    fn eq(&self, other: &StableSet<T, S2, W>) -> bool {
        self.len() == other.len() && self.is_subset(other)
    }
}

impl<T: Hash + Eq, S: BuildHasher, W: SmallIndex> Eq for StableSet<T, S, W> {}

/// An iterator that moves out of a set.
///
/// This struct is created by the `into_iter` method on [`StableSet`].
pub struct IntoIter<T> {
    inner: std::vec::IntoIter<T>,
}
impl<T> Iterator for IntoIter<T> {
    type Item = T;
    impl_iterator!();
}
impl<T, S, W> IntoIterator for StableSet<T, S, W> {
    type Item = T;
    type IntoIter = IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.items.into_iter(),
        }
    }
}

impl<'a, T, S, W> IntoIterator for &'a StableSet<T, S, W> {
    type Item = &'a T;
    type IntoIter = Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

/// An iterator that returns references into a set.
///
/// This struct is created by the [`iter`](StableSet::iter) method on [`StableSet`].
pub struct Iter<'a, T> {
    inner: std::slice::Iter<'a, T>,
}
impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;
    impl_iterator!();
}

impl<T, S, W> StableSet<T, S, W> {
    /// Returns an iterator over the set.
    ///
    /// The iterator yields all items in order.
    pub fn iter(&self) -> Iter<'_, T> {
        Iter {
            inner: self.items.iter(),
        }
    }
}

impl<T: Hash + Eq, S: BuildHasher, W: SmallIndex> Extend<T> for StableSet<T, S, W> {
    fn extend<IntoIter: IntoIterator<Item = T>>(&mut self, iter: IntoIter) {
        let iter = iter.into_iter();
        let (lower_bound, _) = iter.size_hint();
        self.reserve(lower_bound);
        for item in iter {
            self.insert(item);
        }
    }
}

impl<T: Hash + Eq, S: BuildHasher + Default, W: SmallIndex> FromIterator<T> for StableSet<T, S, W> {
    fn from_iter<IntoIter: IntoIterator<Item = T>>(iter: IntoIter) -> Self {
        let iter = iter.into_iter();
        let (lower_bound, _) = iter.size_hint();
        let mut set = StableSet::with_capacity(lower_bound);
        for item in iter {
            set.insert(item);
        }
        set
    }
}

impl<T: Hash, S: BuildHasher, W: SmallIndex> StableSet<T, S, W> {
    #[cfg(test)]
    pub(crate) fn check(&self) {
        assert!(self.index_table.len() == self.items.len());
        for (index, item) in self.items.iter().enumerate() {
            let hash = self.build_hasher.hash_one(item);
            assert_eq!(self.index_table.find(hash, |idx| idx == index), Some(index));
        }
    }
}

/// An iterator that removes items from a set, see [`drain`](StableSet::drain).
pub struct Drain<'a, T> {
    inner: std::vec::Drain<'a, T>,
}
impl<T> Iterator for Drain<'_, T> {
    type Item = T;
    impl_iterator!();
}

impl<T, S, W: SmallIndex> StableSet<T, S, W> {
    /// Returns an iterator yielding all elements with indices in the specified range and removes those elements from the set.
    ///
    /// Panics if the range is invalid or out of bounds.
    ///
    /// If the returned iterator is leaked, the set is left in an invalid state and can only be dropped.
    /// Any other operations may panic or return incorrect results.
    pub fn drain(&mut self, range: impl RangeBounds<usize>) -> Drain<'_, T> {
        let range = simplify_range(range, self.items.len());
        self.index_table
            .clear_range(range.start, range.end, self.items.len());
        Drain {
            inner: self.items.drain(range),
        }
    }
}
