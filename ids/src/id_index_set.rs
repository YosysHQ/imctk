#![allow(missing_docs)] // TODO document module
use std::{
    borrow::Borrow,
    hash::{BuildHasher, BuildHasherDefault, Hash},
};

use hashbrown::HashTable;
use zwohash::ZwoHasher;

use crate::{
    id_vec::{IdSlice, IdVec},
    Id,
};

pub struct IdIndexSet<K, V, S = BuildHasherDefault<ZwoHasher>> {
    items: IdVec<K, V>,
    table: HashTable<K>,
    build_hasher: S,
}

impl<K: std::fmt::Debug + Id, V: std::fmt::Debug> std::fmt::Debug for IdIndexSet<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.items, f)
    }
}

impl<K: Id, V, S: Default> Default for IdIndexSet<K, V, S> {
    fn default() -> Self {
        Self {
            items: Default::default(),
            table: Default::default(),
            build_hasher: Default::default(),
        }
    }
}

impl<K: Id, V: Hash + Eq, S: BuildHasher> IdIndexSet<K, V, S> {
    pub fn into_id_vec(self) -> IdVec<K, V> {
        self.items
    }

    pub fn insert(&mut self, value: V) -> (K, &mut V, Option<V>) {
        let hash = self.build_hasher.hash_one(&value);

        match self.table.entry(
            hash,
            |&other_key| self.items[other_key] == value,
            |&other_key| self.build_hasher.hash_one(&self.items[other_key]),
        ) {
            hashbrown::hash_table::Entry::Occupied(entry) => {
                let key = *entry.get();
                (key, &mut self.items[key], Some(value))
            }
            hashbrown::hash_table::Entry::Vacant(entry) => {
                let (new_key, value_mut) = self.items.push(value);
                entry.insert(new_key);
                (new_key, value_mut, None)
            }
        }
    }

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

    pub fn clear(&mut self) {
        self.table.clear();
        self.items.clear();
    }
}

impl<K: Id, V, S> std::ops::Deref for IdIndexSet<K, V, S> {
    type Target = IdSlice<K, V>;

    fn deref(&self) -> &Self::Target {
        self.items.deref()
    }
}
