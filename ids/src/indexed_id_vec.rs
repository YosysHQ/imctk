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

impl<K: Id, V: Clone, S: Clone> Clone for IndexedIdVec<K, V, S> {
    fn clone(&self) -> Self {
        Self {
            items: self.items.clone(),
            table: self.table.clone(),
            build_hasher: self.build_hasher.clone(),
        }
    }
}

impl<K: Id, V: std::fmt::Debug, S> std::fmt::Debug for IndexedIdVec<K, V, S> {
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

impl<K: Id, V: Hash + Eq, S> From<IndexedIdVec<K, V, S>> for IdVec<K, V> {
    fn from(value: IndexedIdVec<K, V, S>) -> Self {
        value.items
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
    pub fn get_key<Q>(&self, value: &Q) -> Option<K>
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

    /// Returns whether there already is an entry for the given value.
    pub fn contains_value<Q>(&self, value: &Q) -> bool
    where
        V: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        self.get_key(value).is_some()
    }

    /// Removes a value and closes any resulting gap among the keys by changing the key of the last
    /// entry to be the newly unused key.
    pub fn swap_remove_value<Q>(&mut self, value: &Q) -> Option<(K, V)>
    where
        V: Borrow<Q>,
        Q: Hash + Eq + ?Sized,
    {
        let key = self.get_key(value)?;

        let (last_key, last_value) = self.pop().unwrap();

        let removed_value = if key != last_key {
            self.replace(key, last_value).unwrap()
        } else {
            last_value
        };

        Some((key, removed_value))
    }

    /// Removes and returns the last entry.
    ///
    /// Returns `None` if the collection is empty.
    pub fn pop(&mut self) -> Option<(K, V)> {
        let (key, value) = self.items.pop()?;
        self.table
            .find_entry(self.build_hasher.hash_one(&value), |&found| found == key)
            .unwrap()
            .remove();
        Some((key, value))
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

    /// Inserts values from an iterator using the smallset available ids as keys.
    #[inline]
    pub fn extend_values(&mut self, iter: impl IntoIterator<Item = V>) {
        for value in iter {
            self.insert(value);
        }
    }
}

#[cfg(test)]
mod tests {
    use rand::{rngs::SmallRng, Rng, SeedableRng};
    use zwohash::HashSet;

    use super::*;

    #[test]
    fn test_vs_std() {
        let mut set = <HashSet<usize>>::default();
        let mut vec = vec![];
        let mut idvec = <IndexedIdVec<u32, usize>>::default();

        for _ in 0..10 {
            let mut rng = SmallRng::seed_from_u64(0);

            for _ in 0..rng.gen_range(10..100) {
                let val = rng.gen_range(0..50);
                let inserted = set.insert(val);
                if inserted {
                    vec.push(val);
                }
                assert_eq!(idvec.insert(val).2, inserted);
            }

            idvec.clear();
            vec.clear();
            set.clear();

            for &value in set.iter() {
                idvec.insert(value);
            }
            assert_eq!(format!("{:?}", set), format!("{:?}", idvec));

            set.clear();
            idvec.clear();
        }
    }

    #[test]
    fn test_replace() {
        let mut idvec = <IndexedIdVec<u32, u32>>::default();

        for v in 0..4 {
            idvec.insert(v);
        }

        assert_eq!(idvec.replace(0, 0), Err(0));
        assert_eq!(idvec.replace(0, 1), Err(1));
        assert_eq!(idvec.replace(0, 4), Ok(0));
        assert_eq!(idvec.replace(1, 0), Ok(1));
        assert_eq!(idvec.replace(0, 1), Ok(4));
        assert_eq!(idvec.replace(1, 0), Err(1));
    }
}
