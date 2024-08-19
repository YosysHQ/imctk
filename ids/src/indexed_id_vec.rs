//! An [`IdVec`] with a hash set index of the contained values.
use std::{
    borrow::Borrow,
    hash::{BuildHasher, BuildHasherDefault, Hash},
    mem::replace,
    ops::Deref,
};

use hashbrown::HashTable;
use zwohash::ZwoHasher;

use crate::{id_vec::IdVec, Id};

/// A collection type that extends an [`IdVec`] with a hash set index of the contained values.
///
/// This imlements [`Deref<Target=IdVec<K, V>>`][`Deref`] so that all read-only [`IdVec`] methods
/// work as-is.
pub struct IndexedIdVec<K, V, S = BuildHasherDefault<ZwoHasher>> {
    items: IdVec<K, V>,
    table: HashTable<K>,
    build_hasher: S,
}

impl<K: Id, V: Hash + Eq, S: Default + BuildHasher> FromIterator<V> for IndexedIdVec<K, V, S> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        // TODO pre-reserve storage?
        let mut new = Self::default();
        for item in iter {
            new.insert(item);
        }
        new
    }
}

impl<K: Id, V, S> Deref for IndexedIdVec<K, V, S> {
    type Target = IdVec<K, V>;

    fn deref(&self) -> &Self::Target {
        &self.items
    }
}

impl<K: std::fmt::Debug + Id, V: std::fmt::Debug> std::fmt::Debug for IndexedIdVec<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.items, f)
    }
}

impl<K: Id, V, S: Default> Default for IndexedIdVec<K, V, S> {
    fn default() -> Self {
        Self {
            items: Default::default(),
            table: Default::default(),
            build_hasher: Default::default(),
        }
    }
}

impl<K: Id, V: Hash + Eq, S: BuildHasher> IndexedIdVec<K, V, S> {
    /// Returns the underlying [`IdVec`], discarding the set index.
    pub fn into_id_vec(self) -> IdVec<K, V> {
        self.items
    }

    /// Tries to insert a new value using the next available key.
    ///
    /// If the set didn't already contain the new value, it returns the newly allocated key, a
    /// reference to the inserted value and `true`. If the value was already in the set, it returns
    /// the key and value reference for the existing entry and `false`.
    pub fn insert(&mut self, value: V) -> (K, &V, bool) {
        let hash = self.build_hasher.hash_one(&value);

        match self.table.entry(
            hash,
            |&other_key| self.items[other_key] == value,
            |&other_key| self.build_hasher.hash_one(&self.items[other_key]),
        ) {
            hashbrown::hash_table::Entry::Occupied(entry) => {
                let key = *entry.get();
                (key, &mut self.items[key], false)
            }
            hashbrown::hash_table::Entry::Vacant(entry) => {
                let (new_key, value_mut) = self.items.push(value);
                entry.insert(new_key);
                (new_key, value_mut, true)
            }
        }
    }

    /// Replaces the value at a given key with a new value.
    ///
    /// If the set didn't already contain the new value, it returns the previously value at the
    /// given key wrapped in `Ok`. Otherwise it returns the key that already contains the given
    /// value wrapped in `Err`.
    pub fn replace(&mut self, key: K, value: V) -> Result<V, K> {
        let new_hash = self.build_hasher.hash_one(&value);
        let old_hash = self.build_hasher.hash_one(&self.items[key]);

        match self.table.entry(
            new_hash,
            |&other_key| *self.items[other_key].borrow() == value,
            |&other_key| self.build_hasher.hash_one(&self.items[other_key]),
        ) {
            hashbrown::hash_table::Entry::Occupied(entry) => {
                return Err(*entry.get());
            }
            hashbrown::hash_table::Entry::Vacant(entry) => {
                entry.insert(key);
            }
        }

        let found = self
            .table
            .find_entry(old_hash, |&other_key| other_key == key)
            .unwrap();
        found.remove();

        let old_value = replace(&mut self.items[key], value);

        Ok(old_value)
    }

    /// Retrieves the key for a value in the set.
    ///
    /// Returns `None` if the set doesn't contain the given value.
    pub fn get_id<Q>(&self, value: &Q) -> Option<K>
    where
        V: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let hash = self.build_hasher.hash_one(value);
        let &id = self
            .table
            .find(hash, |&key| *self.items[key].borrow() == *value)?;
        Some(id)
    }

    /// Removes all entries from the collection.
    pub fn clear(&mut self) {
        self.table.clear();
        self.items.clear();
    }

    /// Returns an iterator over all entries and removes them from the collection.
    pub fn drain(&mut self) -> std::vec::Drain<'_, V> {
        self.table.clear();
        self.items.drain_all_values()
    }
}
