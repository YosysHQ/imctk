//! [StableMap] is a a hash map that maintains the order of its elements.
use crate::{
    index_table,
    util::{impl_iterator, simplify_range},
};
use core::hash::Hash;
use index_table::IndexTable;
use std::{borrow::Borrow, hash::BuildHasher, ops::RangeBounds};

/// A hash map that maintains the order of its elements.
#[derive(Clone)]
pub struct StableMap<K, V, S> {
    index_table: IndexTable,
    items: Vec<(K, V)>,
    build_hasher: S,
}

impl<K, V, S: Default> Default for StableMap<K, V, S> {
    fn default() -> Self {
        StableMap {
            index_table: IndexTable::default(),
            items: Vec::new(),
            build_hasher: S::default(),
        }
    }
}
impl<K, V, S: Default> StableMap<K, V, S> {
    /// Returns an empty map.
    pub fn new() -> Self {
        Self::default()
    }
    /// Returns an empty map with the specified capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        StableMap {
            index_table: IndexTable::with_capacity(capacity),
            items: Vec::with_capacity(capacity),
            build_hasher: S::default(),
        }
    }
}

impl<K, V, S> StableMap<K, V, S> {
    /// Returns an empty map with the provided BuildHasher.
    pub fn with_hasher(build_hasher: S) -> Self {
        StableMap {
            index_table: IndexTable::default(),
            items: Vec::new(),
            build_hasher,
        }
    }
    /// Returns an empty map with the specified capacity and provided BuildHasher.
    pub fn with_capacity_and_hasher(capacity: usize, build_hasher: S) -> Self {
        StableMap {
            index_table: IndexTable::with_capacity(capacity),
            items: Vec::with_capacity(capacity),
            build_hasher,
        }
    }
}

impl<K: std::fmt::Debug, V: std::fmt::Debug, S> std::fmt::Debug for StableMap<K, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

impl<K, V, S> StableMap<K, V, S> {
    /// Returns the number of items in the map.
    pub fn len(&self) -> usize {
        self.items.len()
    }
    /// Returns `true` if the map is empty.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
    /// Returns a reference to a slice containing all key-value pairs in the set.
    pub fn as_slice(&self) -> &[(K, V)] {
        &self.items[..]
    }
    /// Removes all entries from the map, but keeps the allocated memory.
    pub fn clear(&mut self) {
        self.index_table.clear();
        self.items.clear();
    }
    /// Removes all entries except for the first `len` entries from the map, but keeps the allocated memory.
    pub fn truncate(&mut self, len: usize) {
        self.index_table
            .retain(|index| (index < len).then_some(index));
        self.items.truncate(len);
    }
    /// Returns an iterator over all key-values pairs.
    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter {
            inner: self.items.iter(),
        }
    }
    /// Returns an iterator over all keys.
    pub fn keys(&self) -> Keys<'_, K, V> {
        Keys {
            inner: self.items.iter(),
        }
    }
    /// Returns an iterator over all values.
    pub fn values(&self) -> Values<'_, K, V> {
        Values {
            inner: self.items.iter(),
        }
    }
    /// Returns an iterator over all values, allowing mutation.
    pub fn values_mut(&mut self) -> ValuesMut<'_, K, V> {
        ValuesMut {
            inner: self.items.iter_mut(),
        }
    }
    /// Returns an iterator over all key-value pairs, allowing mutation of values.
    pub fn iter_mut(&mut self) -> IterMut<'_, K, V> {
        IterMut {
            inner: self.items.iter_mut(),
        }
    }
}

impl<K: Hash + Eq, V, S: BuildHasher> StableMap<K, V, S> {
    /// Inserts `value` at `key`, replacing any previous value.
    /// Returns the index of the item and any previous value.
    ///
    /// If there was no previous value, the key-value pair is inserted at the end.
    pub fn insert_full(&mut self, key: K, value: V) -> (usize, Option<V>) {
        let hash = self.build_hasher.hash_one(&key);
        match self.index_table.entry(
            hash,
            |index| self.items[index].0 == key,
            |index| self.build_hasher.hash_one(&self.items[index].0),
        ) {
            index_table::Entry::Occupied(entry) => {
                let index = entry.get();
                let old_value = std::mem::replace(&mut self.items[index].1, value);
                (index, Some(old_value))
            }
            index_table::Entry::Vacant(entry) => {
                let index = self.items.len();
                self.items.push((key, value));
                entry.insert(index);
                (index, None)
            }
        }
    }
    /// Inserts `value` at `key`, replacing and returning any previous value.
    ///
    /// If there was no previous value, the key-value pair is inserted at the end.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.insert_full(key, value).1
    }
}

impl<K: Hash + Eq, V, S: BuildHasher> StableMap<K, V, S> {
    /// Returns the index of the entry with the specified key, if it exists.
    pub fn get_index_of<Q>(&self, key: &Q) -> Option<usize>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        let hash = self.build_hasher.hash_one(key);
        self.index_table
            .find(hash, |index| self.items[index].0.borrow() == key)
    }
    /// Returns the index and references to the key and value of the entry with the specified key, if it exists.
    pub fn get_full<Q>(&self, key: &Q) -> Option<(usize, &K, &V)>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        self.get_index_of(key).map(|index| {
            let entry = &self.items[index];
            (index, &entry.0, &entry.1)
        })
    }
    /// Returns the index and a shared references to the key and a mutable reference to the value of the entry with the specified key, if it exists.
    pub fn get_full_mut<Q>(&mut self, key: &Q) -> Option<(usize, &K, &mut V)>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        self.get_index_of(key).map(|index| {
            let entry = &mut self.items[index];
            (index, &entry.0, &mut entry.1)
        })
    }
    /// Returns a reference to the value corresponding to the specified key, if it exists.
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        self.get_full(key).map(|x| x.2)
    }
    /// Returns a mutable reference to the value corresponding to the specified key, if it exists.
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        self.get_full_mut(key).map(|x| x.2)
    }
    /// Returns a reference to the key and value with the specified index, if it exists.
    pub fn get_index(&self, index: usize) -> Option<(&K, &V)> {
        self.items.get(index).map(|entry| (&entry.0, &entry.1))
    }
    /// Returns a shared reference to the key and a mutable reference to the value with the specified index, if it exists.
    pub fn get_index_mut(&mut self, index: usize) -> Option<&mut V> {
        self.items.get_mut(index).map(|entry| &mut entry.1)
    }
    fn swap_remove_finish(&mut self, index: usize) -> (K, V) {
        let entry = self.items.swap_remove(index);
        if index < self.items.len() {
            let swapped_hash = self.build_hasher.hash_one(&self.items[index].0);
            self.index_table
                .replace(swapped_hash, |i| i == self.items.len(), index)
                .unwrap();
        }
        entry
    }
    /// Removes the entry with the specified key from the set and returns its index and its key and value, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_remove_full<Q>(&mut self, key: &Q) -> Option<(usize, K, V)>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        let hash = self.build_hasher.hash_one(key);
        match self
            .index_table
            .find_entry(hash, |index| self.items[index].0.borrow() == key)
        {
            Ok(entry) => {
                let (index, _) = entry.remove();
                let (key, value) = self.swap_remove_finish(index);
                Some((index, key, value))
            }
            Err(_) => None,
        }
    }
    /// Removes the entry with the specified key from the set and returns its value, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        Q: Hash + Eq + ?Sized,
        K: Borrow<Q>,
    {
        self.swap_remove_full(key).map(|x| x.2)
    }
    /// Removes the entry with the specified index from the set and returns its key and value, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_remove_index_full(&mut self, index: usize) -> Option<(K, V)> {
        let hash = self.build_hasher.hash_one(&self.items.get(index)?.0);
        self.index_table
            .find_entry(hash, |i| i == index)
            .unwrap()
            .remove();
        Some(self.swap_remove_finish(index))
    }
    /// Removes the entry with the specified index from the set and returns its value, if it exists.
    ///
    /// The last item is put in its place.
    pub fn swap_remove_index(&mut self, index: usize) -> Option<V> {
        self.swap_remove_index_full(index).map(|x| x.1)
    }
    /// Removes all items for which `f` evaluates to `false`.
    ///
    /// `f` is guaranteed to be called exactly once for each item and in order, and may mutate the values.
    ///
    /// The order of elements is preserved.
    pub fn retain(&mut self, mut f: impl FnMut(&K, &mut V) -> bool) {
        let mut in_index = 0;
        let mut out_index = 0;
        self.items.retain_mut(|entry| {
            let hash = self.build_hasher.hash_one(&entry.0);
            let keep = f(&entry.0, &mut entry.1);
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
    /// Returns an iterator yielding all key-value pairs with indices in the specified range and removes those entries.
    ///
    /// Panics if the range is invalid or out of bounds.
    ///
    /// If the returned iterator is leaked, the map is left in an invalid state and can only be dropped.
    /// Any other operations may panic or return incorrect results.
    pub fn drain(&mut self, range: impl RangeBounds<usize>) -> Drain<'_, K, V> {
        let range = simplify_range(range, self.items.len());
        self.index_table
            .clear_range(range.start, range.end, self.items.len());
        Drain {
            inner: self.items.drain(range),
        }
    }
}

/// An iterator over the entries of a [`StableMap`].
///
/// This struct is created by the [`iter`](`StableMap::iter`) method on [`StableMap`].
pub struct Iter<'a, K, V> {
    inner: std::slice::Iter<'a, (K, V)>,
}
impl<'a, K, V> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);
    impl_iterator!(|entry| (&entry.0, &entry.1));
}

/// An iterator over the keys of a [`StableMap`].
///
/// This struct is created by the [`keys`](`StableMap::keys`) method on [`StableMap`].
pub struct Keys<'a, K, V> {
    inner: std::slice::Iter<'a, (K, V)>,
}
impl<'a, K, V> Iterator for Keys<'a, K, V> {
    type Item = &'a K;
    impl_iterator!(|entry| &entry.0);
}

/// An iterator over the values of a [`StableMap`].
///
/// This struct is created by the [`values`](`StableMap::values`) method on [`StableMap`].
pub struct Values<'a, K, V> {
    inner: std::slice::Iter<'a, (K, V)>,
}
impl<'a, K, V> Iterator for Values<'a, K, V> {
    type Item = &'a V;
    impl_iterator!(|entry| &entry.1);
}

/// An iterator over the values of a [`StableMap`], allowing mutation.
///
/// This struct is created by the [`values_mut`](`StableMap::values_mut`) method on [`StableMap`].
pub struct ValuesMut<'a, K, V> {
    inner: std::slice::IterMut<'a, (K, V)>,
}
impl<'a, K, V> Iterator for ValuesMut<'a, K, V> {
    type Item = &'a mut V;
    impl_iterator!(|entry| &mut entry.1);
}

/// An iterator over the entries of a [`StableMap`], allowing mutation of values.
///
/// This struct is created by the [`iter_mut`](`StableMap::iter_mut`) method on [`StableMap`].
pub struct IterMut<'a, K, V> {
    inner: std::slice::IterMut<'a, (K, V)>,
}
impl<'a, K, V> Iterator for IterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);
    impl_iterator!(|entry| (&entry.0, &mut entry.1));
}

/// An iterator moving entries out of a [`StableMap`].
///
/// This struct is created by the `into_iter` method on [`StableMap`].
pub struct IntoIter<K, V> {
    inner: std::vec::IntoIter<(K, V)>,
}

impl<K, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);
    impl_iterator!();
}
impl<K, V, S> IntoIterator for StableMap<K, V, S> {
    type Item = (K, V);
    type IntoIter = IntoIter<K, V>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.items.into_iter(),
        }
    }
}

/// An iterator that removes entries from a map, see [`drain`](StableMap::drain).
pub struct Drain<'a, K, V> {
    inner: std::vec::Drain<'a, (K, V)>,
}

impl<K, V> Iterator for Drain<'_, K, V> {
    type Item = (K, V);
    impl_iterator!();
}

impl<'a, K, V, S> IntoIterator for &'a StableMap<K, V, S> {
    type Item = (&'a K, &'a V);
    type IntoIter = Iter<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}
impl<'a, K, V, S> IntoIterator for &'a mut StableMap<K, V, S> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = IterMut<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

/// A vacant entry in a [`StableMap`].
pub struct VacantEntry<'a, K, V, S> {
    entry: index_table::VacantEntry<'a>,
    key: K,
    items: &'a mut Vec<(K, V)>,
    #[allow(dead_code)] // for consistency with OccupiedEntry
    build_hasher: &'a S,
}

/// An occupied entry in a [`StableMap`].
pub struct OccupiedEntry<'a, K, V, S> {
    entry: index_table::OccupiedEntry<'a>,
    items: &'a mut Vec<(K, V)>,
    build_hasher: &'a S,
}

/// An entry in a [`StableMap`].
pub enum Entry<'a, K, V, S> {
    /// A vacant entry.
    Vacant(VacantEntry<'a, K, V, S>),
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, K, V, S>),
}

impl<K: Hash + Eq, V, S: BuildHasher> StableMap<K, V, S> {
    /// Returns the entry corresponding to the given key, allowing for insertion and/or in-place mutation.
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V, S> {
        let hash = self.build_hasher.hash_one(&key);
        match self.index_table.entry(
            hash,
            |index| self.items[index].0 == key,
            |index| self.build_hasher.hash_one(&self.items[index].0),
        ) {
            index_table::Entry::Occupied(entry) => Entry::Occupied(OccupiedEntry {
                entry,
                items: &mut self.items,
                build_hasher: &self.build_hasher,
            }),
            index_table::Entry::Vacant(entry) => Entry::Vacant(VacantEntry {
                entry,
                key,
                items: &mut self.items,
                build_hasher: &self.build_hasher,
            }),
        }
    }
}

impl<'a, K, V, S> VacantEntry<'a, K, V, S> {
    /// Returns a reference to the key that would be used for insertion.
    pub fn key(&self) -> &K {
        &self.key
    }
    /// Returns the key that would be used for insertion.
    pub fn into_key(self) -> K {
        self.key
    }
    /// Returns the index at which an entry would be inserted.
    pub fn index(&self) -> usize {
        self.items.len()
    }
    /// Inserts a value into the entry, returning a mutable reference to the value.
    pub fn insert(self, value: V) -> &'a mut V {
        let index = self.items.len();
        self.items.push((self.key, value));
        self.entry.insert(index);
        &mut self.items[index].1
    }
}

impl<'a, K, V, S> OccupiedEntry<'a, K, V, S> {
    /// Returns a reference to the key of the entry.
    pub fn key(&self) -> &K {
        &self.items[self.index()].0
    }
    /// Returns the index of the entry.
    pub fn index(&self) -> usize {
        self.entry.get()
    }
    /// Replaces the value in the entry with the provided value, returning the previous value.
    pub fn insert(self, value: V) -> V {
        std::mem::replace(self.into_mut(), value)
    }
    /// Returns a mutable reference to the value in the entry.
    pub fn get_mut(&mut self) -> &mut V {
        let index = self.index();
        &mut self.items[index].1
    }
    /// Returns a mutable reference to the value in the entry, bound to the lifetime of the map.
    pub fn into_mut(self) -> &'a mut V {
        let index = self.index();
        &mut self.items[index].1
    }
}

impl<K: Hash, V, S: BuildHasher> OccupiedEntry<'_, K, V, S> {
    /// Removes the entry from the map and returns the key-value pair.
    ///
    /// The last entry in the map is put in its place.
    pub fn swap_remove_entry(self) -> (K, V) {
        let (index, entry) = self.entry.remove();
        let keyvalue = self.items.swap_remove(index);
        if index < self.items.len() {
            let hash = self.build_hasher.hash_one(&self.items[index].0);
            entry
                .replace_other(hash, |i| i == self.items.len(), index)
                .unwrap();
        }
        keyvalue
    }
    /// Removes the entry from the map and returns the value.
    ///
    /// The last entry in the map is put in its place.
    pub fn swap_remove(self) -> V {
        self.swap_remove_entry().1
    }
}

impl<'a, K, V, S> Entry<'a, K, V, S> {
    /// Returns a refernece to the key of the entry.
    pub fn key(&self) -> &K {
        match self {
            Entry::Vacant(entry) => entry.key(),
            Entry::Occupied(entry) => entry.key(),
        }
    }
    /// Returns the index of the entry.
    pub fn index(&self) -> usize {
        match self {
            Entry::Vacant(entry) => entry.index(),
            Entry::Occupied(entry) => entry.index(),
        }
    }
    /// Inserts the default value if the entry is vacant. Returns a mutable reference to the entry.
    pub fn or_default(self) -> &'a mut V
    where
        V: Default,
    {
        self.or_insert(V::default())
    }
    /// Inserts the provided value if the entry is vacant. Returns a mutable reference to the entry.
    pub fn or_insert(self, value: V) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => entry.insert(value),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
    /// Inserts the value returned by `f()` if the entry is vacant. Returns a mutable reference to the entry.
    pub fn or_insert_with(self, f: impl FnOnce() -> V) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => entry.insert(f()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
    /// Inserts the value returned by `f(&key)` if the entry is vacant. Returns a mutable reference to the entry.
    pub fn or_insert_with_key(self, f: impl FnOnce(&K) -> V) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => {
                let value = f(entry.key());
                entry.insert(value)
            }
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
    /// Modifies the value by calling `f(&value)` if the entry is occupied. Returns the entry itself.
    pub fn and_modify(self, f: impl FnOnce(&mut V)) -> Self {
        match self {
            Entry::Vacant(entry) => Entry::Vacant(entry),
            Entry::Occupied(mut entry) => {
                f(entry.get_mut());
                Entry::Occupied(entry)
            }
        }
    }
}

impl<K: Hash, V, S: BuildHasher> StableMap<K, V, S> {
    #[cfg(test)]
    fn check(&self) {
        assert!(self.index_table.len() == self.items.len());
        for (index, (key, _)) in self.items.iter().enumerate() {
            let hash = self.build_hasher.hash_one(key);
            assert_eq!(self.index_table.find(hash, |idx| idx == index), Some(index));
        }
    }
}

#[test]
fn test() {
    use std::hash::BuildHasherDefault;
    use zwohash::ZwoHasher;
    let mut map: StableMap<String, usize, BuildHasherDefault<ZwoHasher>> = StableMap::default();
    map.insert("adam".into(), 10);
    map.insert("eve".into(), 25);
    map.insert("mallory".into(), 8);
    map.insert("jim".into(), 14);
    match map.entry("eve".to_string()) {
        Entry::Vacant(_) => unreachable!(),
        Entry::Occupied(entry) => entry.swap_remove(),
    };
    dbg!(&map);
    map.check();
}
