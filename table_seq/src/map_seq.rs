use core::fmt;
use std::{borrow::Borrow, hash::{BuildHasher, BuildHasherDefault, Hash}};
use crate::table_seq::{TableSeq, SubtableIter, SubtableIterMut};

struct MapEntry<K, V> {
    key: K,
    value: V,
}

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
        self.tables
            .insert(
                map,
                hash,
                MapEntry { key, value },
                |found, inserting| found.key == inserting.key,
                |found| self.build_hasher.hash_one(&found.key),
            )
            .1
            .map(|entry| entry.value)
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
}


/// Exclusive mutable access to a map of a [`MapSeq`].
#[repr(C)] // SAFETY: layout must be compatible with MapSeqMap
pub struct MapSeqMapMut<'a, K, V, S> {
    seq: &'a mut MapSeq<K, V, S>,
    map: usize,
}

/// Shared read-only access to a map of a [`MapSeq`].
#[derive(Clone, Copy)]
#[repr(C)] // SAFETY: layout must be compatible with MapSeqMapMut
pub struct MapSeqMap<'a, K, V, S> {
    seq: &'a MapSeq<K, V, S>,
    map: usize,
}

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

    /// Returns an iterator over the elements of the map, with mutable references to values.
    #[inline(always)]
    pub fn iter_mut(&'a mut self) -> MapIterMut<'a, K, V> {
        MapIterMut {
            inner: self.seq.tables.subtable_iter_mut(self.map)
        }
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

#[test]
fn test_foo() {
    use zwohash::ZwoHasher;
    let mut foo: MapSeq<String, u32, BuildHasherDefault<ZwoHasher>> = Default::default();
    let mut bar = foo.grow_for(3);
    bar.insert("foo".to_string(), 100);
    for (_, value) in bar.iter_mut() {
        *value *= 2;
    }
    bar.insert("bar".to_string(), 42);
    println!("{:?}", foo);
}