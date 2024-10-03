//! Indexed sequence of hash maps and associated helper types.
use core::fmt;
use std::{borrow::Borrow, hash::{BuildHasher, Hash}, mem};
use crate::table_seq::{self, SubtableIter, SubtableIterMut, TableSeq};

struct MapEntry<K, V> {
    key: K,
    value: V,
}

/// Indexed sequence of hash maps.
///
/// This type serves as a memory and runtime efficient replacement for `Vec<HashMap<K, V>>`. In
/// particular, it is optimized for the use-case where the vast majority of contained maps are tiny,
/// each having 16 or fewer entries, while still allowing for a small but significant fraction of
/// maps to be large.
pub struct MapSeq<K, V, S> {
    tables: TableSeq<MapEntry<K, V>>,
    build_hasher: S,
}

impl<K, V, S: Default> Default for MapSeq<K, V, S> {
    fn default() -> Self {
        Self {
            tables: Default::default(),
            build_hasher: Default::default(),
        }
    }
}

impl<K: fmt::Debug, V: fmt::Debug> fmt::Debug for MapEntry<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}: {:?}", self.key, self.value)
    }
}

impl<K: fmt::Debug, V: fmt::Debug, S> fmt::Debug for MapSeq<K, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(&self.tables, f)
    }
}

impl<K, V, S> MapSeq<K, V, S> {
    /// Returns the number of maps in the sequence.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.tables.len()
    }

    /// Returns `true` if the sequence of maps is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.tables.is_empty()
    }

    /// Discards all maps in the sequence.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.tables.clear()
    }

    /// Resizes the sequence by appending empty maps or discarding trailing maps.
    #[inline(always)]
    pub fn resize(&mut self, maps: usize) {
        self.tables.resize(maps)
    }

    /// Ensures that the sequence contains a map at the given index by appending emtpy maps if the
    /// sequence was too short.
    ///
    /// Provides mutable access to the map at the given index.
    #[inline(always)]
    pub fn grow_for(&mut self, map: usize) -> MapSeqMapMut<K, V, S> {
        self.tables.grow_for_subtable(map);
        MapSeqMapMut { seq: self, map }
    }

    /// Provides shared access to the map at a given index, panics if out-of-bounds.
    ///
    /// This is used instead of [`std::ops::Index`], as it returns a value of the custom
    /// reference-like [`MapSeqMap`] type.
    ///
    /// Panics if `map >= self.len()`.
    #[inline(always)]
    pub fn at(&self, map: usize) -> MapSeqMap<K, V, S> {
        assert!(self.tables.len() > map);
        MapSeqMap { seq: self, map }
    }

    /// Provides mutable access to the map at a given index, panics if out-of-bounds.
    ///
    /// This is used instead of [`std::ops::IndexMut`], as it returns a value of the custom
    /// reference-like [`MapSeqMapMut`] type.
    ///
    /// Panics if `map >= self.len()`.
    #[inline(always)]
    pub fn at_mut(&mut self, map: usize) -> MapSeqMapMut<K, V, S> {
        assert!(self.tables.len() > map);
        MapSeqMapMut { seq: self, map }
    }

    /// Provides shared access to the map at a given index.
    ///
    /// This returns `None` if `map >= self.len()`.
    #[inline(always)]
    pub fn get(&self, map: usize) -> Option<MapSeqMap<K, V, S>> {
        (self.tables.len() > map).then_some(MapSeqMap { seq: self, map })
    }

    /// Provides mutable access to the map at a given index.
    ///
    /// This returns `None` if `map >= self.len()`.
    #[inline(always)]
    pub fn get_mut(&mut self, map: usize) -> Option<MapSeqMapMut<K, V, S>> {
        (self.tables.len() > map).then_some(MapSeqMapMut { seq: self, map })
    }
}

impl<K, V, S> MapSeq<K, V, S> {
    #[inline(always)]
    fn map_len(&self, map: usize) -> usize {
        self.tables.subtable_len(map)
    }

    #[inline(always)]
    fn map_is_empty(&self, map: usize) -> bool {
        self.map_len(map) != 0
    }

    #[inline(always)]
    fn clear_map(&mut self, map: usize) {
        self.tables.clear_subtable(map)
    }
}

impl<K: Eq + Hash, V, S: BuildHasher> MapSeq<K, V, S> {
    fn map_contains_key<Q>(&self, map: usize, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(key);
        self.tables
            .find(map, hash, |found| found.key.borrow() == key)
            .is_some()
    }

    fn map_get_key_value<Q>(&self, map: usize, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(key);
        self.tables
            .find(map, hash, |found| found.key.borrow() == key)
            .map(|entry| (&entry.key, &entry.value))
    }

    fn map_get_mut<Q>(&mut self, map: usize, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(key);
        self.tables
            .find_mut(map, hash, |found| found.key.borrow() == key)
            .map(|entry| &mut entry.value)
    }

    fn map_insert(&mut self, map: usize, key: K, value: V) -> Option<V> {
        let hash = self.build_hasher.hash_one(&key);
        let (entry, returned_value) = self.tables
            .insert(
                map,
                hash,
                MapEntry { key, value },
                |found, inserting| found.key == inserting.key,
                |found| self.build_hasher.hash_one(&found.key),
            );
        returned_value.map(|MapEntry { value, .. }| mem::replace(&mut entry.value, value))
    }

    fn map_remove_entry<Q>(&mut self, map: usize, key: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(key);

        self.tables
            .remove(
                map,
                hash,
                |found| found.key.borrow() == key,
                |found| self.build_hasher.hash_one(&found.key),
            )
            .map(|entry| (entry.key, entry.value))
    }

    fn map_entry(&mut self, map: usize, key: K) -> Entry<'_, K, V> {
        let hash = self.build_hasher.hash_one(&key);
        match self.tables
            .entry(
                    map,
                    hash, 
                    |found| found.key == key,
                    |found| self.build_hasher.hash_one(&found.key)) {
            table_seq::Entry::Occupied(entry) => Entry::Occupied(OccupiedEntry(entry)),
            table_seq::Entry::Vacant(entry) => Entry::Vacant(VacantEntry(entry, key)),
        }
    }
}

/// Exclusive mutable access to a map of a [`MapSeq`].
#[repr(C)] // SAFETY: layout must be compatible with MapSeqMap
pub struct MapSeqMapMut<'a, K, V, S> {
    seq: &'a mut MapSeq<K, V, S>,
    map: usize,
}

/// Shared read-only access to a map of a [`MapSeq`].
#[repr(C)] // SAFETY: layout must be compatible with MapSeqMapMut
pub struct MapSeqMap<'a, K, V, S> {
    seq: &'a MapSeq<K, V, S>,
    map: usize,
}

impl<'a, K, V, S> Clone for MapSeqMap<'a, K, V, S> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, K, V, S> Copy for MapSeqMap<'a, K, V, S> { }

impl<'a, K, V, S> std::ops::Deref for MapSeqMapMut<'a, K, V, S> {
    type Target = MapSeqMap<'a, K, V, S>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        // SAFETY: we have compatible repr(C) layouts between MapSeqMap and MapSeqMapMut
        unsafe { &*(self as *const Self).cast() }
    }
}

impl<'a, K: fmt::Debug, V: fmt::Debug, S> fmt::Debug for MapSeqMap<'a, K, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<'a, K: fmt::Debug, V: fmt::Debug, S> fmt::Debug for MapSeqMapMut<'a, K, V, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_set().entries(self.iter()).finish()
    }
}

impl<'a, K, V, S> MapSeqMap<'a, K, V, S> {
    /// Returns the number of elements the map contains.
    #[inline(always)]
    pub fn len(&self) -> usize {
        self.seq.map_len(self.map)
    }

    /// Returns `true` when the map is empty.
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.seq.map_is_empty(self.map)
    }

    /// Returns an iterator over the key-value pairs of the map.
    #[inline(always)]
    pub fn iter(&self) -> MapIter<'a, K, V> {
        MapIter {
            inner: self.seq.tables.subtable_iter(self.map),
        }
    }

    /// Returns an iterator over the keys of the map.
    #[inline(always)]
    pub fn keys(&self) -> MapKeys<'a, K, V> {
        MapKeys {
            inner: self.seq.tables.subtable_iter(self.map),
        }
    }

    /// Returns an iterator over the values of the map.
    #[inline(always)]
    pub fn values(&self) -> MapValues<'a, K, V> {
        MapValues {
            inner: self.seq.tables.subtable_iter(self.map),
        }
    }
}

impl<'a, K, V, S> MapSeqMapMut<'a, K, V, S> {
    /// Discards all elements of the map.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.seq.clear_map(self.map)
    }

    /// Returns an iterator over the elements of the map, with mutable references to values.
    #[inline(always)]
    pub fn iter_mut(&mut self) -> MapIterMut<K, V> {
        self.reborrow().into_iter()
    }

    /// Reborrow the mutable reference to the map, creating a new mutable reference with a potentially shorter lifetime.
    #[inline(always)]
    pub fn reborrow(&mut self) -> MapSeqMapMut<'_, K, V, S> {
        MapSeqMapMut {
            seq: self.seq,
            map: self.map
        }
    }
}

impl<'a, K: Eq + Hash, V, S: BuildHasher> MapSeqMap<'a, K, V, S> {
    /// Returns `true` if the map contains an element for the given key.
    #[inline(always)]
    pub fn contains_key<Q>(&self, key: &Q) -> bool
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.map_contains_key(self.map, key)
    }

    /// Returns a reference to the value corresponding to the given key.
    #[inline(always)]
    pub fn get<Q>(&self, key: &Q) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.map_get_key_value(self.map, key).map(|(_, value)| value)
    }

    /// Returns a reference to the key-value pair corresponding to the given key.
    #[inline(always)]
    pub fn get_key_value<Q>(&self, key: &Q) -> Option<(&K, &V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.map_get_key_value(self.map, key)
    }
}

impl<'a, K: Eq + Hash, V, S: BuildHasher> MapSeqMapMut<'a, K, V, S> {
    /// Returns a mutable reference to the value corresponding to the given key.
    #[inline(always)]
    pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.map_get_mut(self.map, key)
    }

    /// Inserts a key-value pair into the map.
    ///
    /// If the map did not have this key present, [`None`] is returned.
    ///
    /// If the map did have this key present, the value is updated, and the old
    /// value is returned. The key is not updated, though; this matters for
    /// types that can be `==` without being identical.
    pub fn insert(&mut self, key: K, value: V) -> Option<V> {
        self.seq.map_insert(self.map, key, value)
    }

    /// Removes a key from the map, returning the value at the key if the key was previously in the map.
    pub fn remove<Q>(&mut self, key: &Q) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.map_remove_entry(self.map, key).map(|(_, value)| value)
    }

    /// Removes a key from the map, returning the stored key and value if the key was previously in the map.
    pub fn remove_entry<Q>(&mut self, key: &Q) -> Option<(K, V)>
    where
        K: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.seq.map_remove_entry(self.map, key)
    }

    /// Gets the given key's corresponding entry in the map for in-place manipulation.
    pub fn entry(&mut self, key: K) -> Entry<'_, K, V> {
        self.seq.map_entry(self.map, key)
    }
}

/// A view into a vacant entry in a [`MapSeq`].
/// It is part of the [`Entry`] enum.
pub struct VacantEntry<'a, K, V>(table_seq::VacantEntry<'a, MapEntry<K, V>>, K);

/// A view into an occupied entry in a [`MapSeq`].
/// It is part of the [`Entry`] enum.
pub struct OccupiedEntry<'a, K, V>(table_seq::OccupiedEntry<'a, MapEntry<K, V>>);

/// A view into a single entry in a map, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`MapSeqMapMut`].
pub enum Entry<'a, K, V> {
    /// A vacant entry.
    Vacant(VacantEntry<'a, K, V>),
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, K, V>),
}

impl<'a, K, V> VacantEntry<'a, K, V> {
    /// Gets a reference to the key that would be used when inserting a value
    /// through the `VacantEntry`.
    pub fn key(&self) -> &K {
        &self.1
    }
    /// Take ownership of the key.
    pub fn into_key(self) -> K {
        self.1
    }
    /// Sets the value of the entry with the `VacantEntry`'s key,
    /// and returns an `OccupiedEntry`.
    pub fn insert_entry(self, value: V) -> OccupiedEntry<'a, K, V> {
        let VacantEntry(entry, key) = self;
        let new_entry = entry.insert(MapEntry { key, value });
        OccupiedEntry(new_entry)
    }
    /// Sets the value of the entry with the `VacantEntry`'s key,
    /// and returns a mutable reference to it.
    pub fn insert(self, value: V) -> &'a mut V {
        self.insert_entry(value).into_mut()
    }
}

impl<'a, K, V> OccupiedEntry<'a, K, V> {
    /// Gets a reference to the value in the entry.
    pub fn get(&self) -> &V {
        &self.0.get().value
    }
    /// Gets a mutable reference to the value in the entry.
    pub fn get_mut(&mut self) -> &mut V {
        &mut self.0.get_mut().value
    }
    /// Sets the value of the entry, and returns the entry’s old value.
    pub fn insert(&mut self, value: V) -> V {
        mem::replace(self.get_mut(), value)
    }
    /// Converts the `OccupiedEntry` into a mutable reference to the value in the entry
    /// with a lifetime bound to the map itself.
    pub fn into_mut(self) -> &'a mut V {
        &mut self.0.into_mut().value
    }
    /// Gets a reference to the key in the entry.
    pub fn key(&self) -> &K {
        &self.0.get().key
    }
    /// Take the ownership of the key and value from the map.
    pub fn remove_entry(self) -> (K, V) {
        let (MapEntry {key, value}, _) = self.0.remove();
        (key, value)
    }
    /// Takes the value out of the entry, and returns it.
    pub fn remove(self) -> V {
        self.remove_entry().1
    }
}

impl<'a, K, V> Entry<'a, K, V> {
    /// Provides in-place mutable access to an occupied entry before any potential inserts into the map.
    pub fn and_modify(self, f: impl FnOnce(&mut V)) -> Self {
        match self {
            Entry::Vacant(entry) => Entry::Vacant(entry),
            Entry::Occupied(mut entry) => {
                f(entry.get_mut());
                Entry::Occupied(entry)
            }
        }
    }
    /// Ensures a value is in the entry by inserting the default if empty, and returns a mutable reference to the value in the entry.
    pub fn or_insert(self, default: V) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => entry.insert(default),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
    /// Sets the value of the entry, and returns an `OccupiedEntry`.
    pub fn insert_entry(self, value: V) -> OccupiedEntry<'a, K, V> {
        match self {
            Entry::Vacant(entry) => entry.insert_entry(value),
            Entry::Occupied(mut entry) => {
                entry.insert(value);
                entry
            }
        }
    }
    /// Ensures a value is in the entry by inserting the result of the default function if empty,
    /// and returns a mutable reference to the value in the entry.
    pub fn or_insert_with(self, default: impl FnOnce() -> V) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => entry.insert(default()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
    /// Ensures a value is in the entry by inserting, if empty, the result of the default function.
    /// This method allows for generating key-derived values for insertion by providing the default
    /// function a reference to the key that was moved during the `.entry(key)` method call.
    ///
    /// The reference to the moved key is provided so that cloning or copying the key is
    /// unnecessary, unlike with `.or_insert_with(|| ... )`.
    pub fn or_insert_with_key(self, f: impl FnOnce(&K) -> V) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => {
                let value = f(entry.key());
                entry.insert(value)
            }
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
}

impl<'a, K, V: Default> Entry<'a, K, V> {
    /// Ensures a value is in the entry by inserting the default value if empty,
    /// and returns a mutable reference to the value in the entry.
    pub fn or_default(self) -> &'a mut V {
        match self {
            Entry::Vacant(entry) => entry.insert(Default::default()),
            Entry::Occupied(entry) => entry.into_mut(),
        }
    }
}

/// Iterator yielding references to a map's keys and values.
pub struct MapIter<'a, K, V> {
    inner: SubtableIter<'a, MapEntry<K, V>>,
}

impl<'a, K, V> Default for MapIter<'a, K, V> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<'a, K, V> Iterator for MapIter<'a, K, V> {
    type Item = (&'a K, &'a V);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|entry| (&entry.key, &entry.value))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, K, V> ExactSizeIterator for MapIter<'a, K, V> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.inner.len()
    }
}


/// Iterator yielding references to a map's keys.
pub struct MapKeys<'a, K, V> {
    inner: SubtableIter<'a, MapEntry<K, V>>,
}

impl<'a, K, V> Default for MapKeys<'a, K, V> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<'a, K, V> Iterator for MapKeys<'a, K, V> {
    type Item = &'a K;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|entry| &entry.key)
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, K, V> ExactSizeIterator for MapKeys<'a, K, V> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

/// Iterator yielding references to a map's values.
pub struct MapValues<'a, K, V> {
    inner: SubtableIter<'a, MapEntry<K, V>>,
}

impl<'a, K, V> Default for MapValues<'a, K, V> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<'a, K, V> Iterator for MapValues<'a, K, V> {
    type Item = &'a V;

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|entry| &entry.value)
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, K, V> ExactSizeIterator for MapValues<'a, K, V> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

/// Iterator yielding references to a map's keys and values, with mutable value references.
pub struct MapIterMut<'a, K, V> {
    inner: SubtableIterMut<'a, MapEntry<K, V>>,
}

impl<'a, K, V> Default for MapIterMut<'a, K, V> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            inner: Default::default(),
        }
    }
}

impl<'a, K, V> Iterator for MapIterMut<'a, K, V> {
    type Item = (&'a K, &'a mut V);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|entry| (&entry.key, &mut entry.value))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

impl<'a, K, V> ExactSizeIterator for MapIterMut<'a, K, V> {
    #[inline(always)]
    fn len(&self) -> usize {
        self.inner.len()
    }
}

impl<'a, K, V, S> IntoIterator for MapSeqMap<'a, K, V, S> {
    type Item = (&'a K, &'a V);
    type IntoIter = MapIter<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, K, V, S> IntoIterator for MapSeqMapMut<'a, K, V, S> {
    type Item = (&'a K, &'a mut V);
    type IntoIter = MapIterMut<'a, K, V>;
    fn into_iter(self) -> Self::IntoIter {
        MapIterMut {
            inner: self.seq.tables.subtable_iter_mut(self.map)
        }
    }
}

impl<'a, K, V, S> Extend<(K, V)> for MapSeqMapMut<'a, K, V, S>
where
    K: Eq + Hash,
    S: BuildHasher
{
    fn extend<T: IntoIterator<Item = (K, V)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(k, v);
        }
    }
}

impl<'a, 'b, K, V, S> Extend<(&'b K, &'b V)> for MapSeqMapMut<'a, K, V, S>
where
    K: Eq + Hash + Copy,
    V: Copy,
    S: BuildHasher
{
    fn extend<T: IntoIterator<Item = (&'b K, &'b V)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(*k, *v);
        }
    }
}

impl<'a, K, Q, V, S> std::ops::Index<&Q> for MapSeqMap<'a, K, V, S>
where
    K: Eq + Hash + Borrow<Q>,
    S: BuildHasher,
    Q: Eq + Hash
{
    type Output = V;

    fn index(&self, index: &Q) -> &Self::Output {
        self.get(index).expect("no entry found for key")
    }
}

impl<'a, K, Q, V, S> std::ops::Index<&Q> for MapSeqMapMut<'a, K, V, S>
    where K: Eq + Hash + Borrow<Q>, S: BuildHasher, Q: Eq + Hash
{
    type Output = V;

    fn index(&self, index: &Q) -> &Self::Output {
        self.get(index).expect("no entry found for key")
    }
}