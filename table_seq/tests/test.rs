#![allow(missing_docs)] // test only
use std::{
    fmt::Debug,
    hash::{BuildHasher, BuildHasherDefault, Hash},
};

use hashbrown::HashTable;
use table_seq::TableSeq;
use zwohash::ZwoHasher;

/// Computes the hash of a value using imctk's default hasher.
#[inline]
pub fn hash_value<T: std::hash::Hash>(value: T) -> u64 {
    <BuildHasherDefault<ZwoHasher>>::default().hash_one(value)
}

/// Computes the hash of a reference using imctk's default hasher.
///
/// This forwards to [`hash_value`]. Restricting the argument to be a reference is occasionally
/// useful for type inference or for avoiding warnings.
#[inline]
pub fn hash_ref<T: std::hash::Hash>(value: &T) -> u64 {
    hash_value(value)
}

pub struct TestTableSeq<T> {
    under_test: TableSeq<T>,
    spec: Vec<HashTable<T>>,
}

impl<T> Default for TestTableSeq<T> {
    fn default() -> Self {
        Self {
            under_test: Default::default(),
            spec: Default::default(),
        }
    }
}

impl<T> TestTableSeq<T> {
    pub fn resize(&mut self, len: usize) {
        self.spec.resize_with(len, HashTable::default);
        self.under_test.resize(len);
    }
    pub fn grow_for_subtable(&mut self, subtable: usize) {
        if self.spec.len() <= subtable {
            self.spec.resize_with(subtable + 1, HashTable::default);
        }
        self.under_test.grow_for_subtable(subtable);
    }

    pub fn test_iter(&self, subtable: usize)
    where
        T: Hash + Eq + Debug,
    {
        assert_eq!(
            self.spec[subtable].len(),
            self.under_test.subtable_len(subtable)
        );

        assert_eq!(
            self.spec[subtable].iter().len(),
            self.under_test.subtable_iter(subtable).len()
        );

        for item in self.spec[subtable].iter() {
            assert_eq!(
                self.under_test
                    .find(subtable, hash_ref(item), |found| found == item),
                Some(item),
            );
        }
        for item in self.under_test.subtable_iter(subtable) {
            assert_eq!(
                self.spec[subtable].find(hash_ref(item), |found| found == item),
                Some(item),
            );
        }
    }

    pub fn test_drain(&mut self, subtable: usize)
    where
        T: Hash + Eq + Debug,
    {
        assert_eq!(
            self.spec[subtable].len(),
            self.under_test.subtable_len(subtable)
        );

        let mut owned = self.under_test.take_subtable(subtable);

        assert_eq!(owned.len(), self.spec[subtable].len());
        assert_eq!(owned.iter().len(), self.spec[subtable].len());
        assert_eq!(owned.iter_mut().len(), self.spec[subtable].len());

        for item in &owned {
            assert_eq!(
                self.spec[subtable].find(hash_ref(&item), |found| found == item),
                Some(item),
            );
        }

        for item in &mut owned {
            assert_eq!(
                self.spec[subtable].find(hash_ref(&item), |found| found == item),
                Some(&*item),
            );
        }

        for item in owned {
            assert_eq!(
                self.spec[subtable].find(hash_ref(&item), |found| *found == item),
                Some(&item),
            );
        }

        self.spec[subtable].clear();
    }

    pub fn test_iter_all(&self)
    where
        T: Hash + Eq + Debug,
    {
        assert_eq!(self.spec.is_empty(), self.under_test.is_empty());
        assert_eq!(self.spec.len(), self.under_test.len());

        for i in 0..self.spec.len() {
            self.test_iter(i);
        }
    }

    pub fn insert(&mut self, subtable: usize, item: T)
    where
        T: Hash + Eq + Debug + Clone,
    {
        let mut inserted = false;
        self.spec[subtable]
            .entry(hash_ref(&item), |found| *found == item, hash_ref)
            .or_insert_with(|| {
                inserted = true;
                item.clone()
            });
        let (_, rejected) = self.under_test.insert(
            subtable,
            hash_ref(&item),
            item,
            |found, inserting| found == inserting,
            hash_ref,
        );
        assert_eq!(rejected.is_none(), inserted);
    }

    pub fn insert_unique(&mut self, subtable: usize, item: T)
    where
        T: Hash + Eq + Debug + Clone,
    {
        self.spec[subtable].insert_unique(hash_ref(&item), item.clone(), hash_ref);
        self.under_test
            .insert_unique(subtable, hash_ref(&item), item, hash_ref);
    }

    pub fn remove(&mut self, subtable: usize, item: &T)
    where
        T: Hash + Eq + Debug + Clone,
    {
        let removed =
            self.under_test
                .remove(subtable, hash_ref(item), |found| found == item, hash_ref);
        match self.spec[subtable].entry(hash_ref(item), |found| found == item, hash_ref) {
            hashbrown::hash_table::Entry::Occupied(found) => {
                assert_eq!(Some(found.remove().0), removed);
            }
            hashbrown::hash_table::Entry::Vacant(_) => {
                assert!(removed.is_none());
            }
        }
    }

    pub fn clear_subtable(&mut self, subtable: usize) {
        self.spec[subtable].clear();
        self.under_test.clear_subtable(subtable);
    }

    pub fn find_mut(&mut self, subtable: usize, value: &T) -> Option<(&mut T, &mut T)>
    where
        T: Hash + Eq + Debug + Clone,
    {
        match (
            self.spec[subtable].find_mut(hash_ref(value), |found| found == value),
            self.under_test
                .find_mut(subtable, hash_ref(value), |found| found == value),
        ) {
            (None, None) => None,
            (Some(spec), Some(under_test)) => Some((spec, under_test)),
            _ => panic!(),
        }
    }

    pub fn insert_with_entry(&mut self, subtable: usize, item: T)
    where
        T: Hash + Eq + Debug + Clone,
    {
        let mut inserted_spec = false;
        let mut inserted_dut = false;
        self.spec[subtable]
            .entry(hash_ref(&item), |found| *found == item, hash_ref)
            .or_insert_with(|| {
                inserted_spec = true;
                item.clone()
            });
        self.under_test
            .entry(subtable, hash_ref(&item), |found| found == &item, hash_ref)
            .or_insert_with(|| {
                inserted_dut = true;
                item
            });
        assert_eq!(inserted_spec, inserted_dut);
    }

    pub fn remove_with_entry(&mut self, subtable: usize, item: &T)
    where
        T: Hash + Eq + Debug,
    {
        let removed =
            match self
                .under_test
                .entry(subtable, hash_ref(item), |found| found == item, hash_ref)
            {
                table_seq::table_seq::Entry::Occupied(entry) => Some(entry.remove().0),
                table_seq::table_seq::Entry::Vacant(_) => None,
            };
        match self.spec[subtable].entry(hash_ref(item), |found| found == item, hash_ref) {
            hashbrown::hash_table::Entry::Occupied(found) => {
                assert_eq!(Some(found.remove().0), removed);
            }
            hashbrown::hash_table::Entry::Vacant(_) => {
                assert!(removed.is_none());
            }
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub struct MapEntry<K, V> {
    pub key: K,
    pub value: V,
}

impl<K: PartialEq, V> PartialEq for MapEntry<K, V> {
    fn eq(&self, other: &Self) -> bool {
        self.key == other.key
    }
}

impl<K: Eq, V> Eq for MapEntry<K, V> {}

impl<K: Hash, V> Hash for MapEntry<K, V> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key.hash(state);
    }
}

#[test]
fn test_insertion() {
    let mut table = <TestTableSeq<usize>>::default();
    let size = 10000;
    table.resize(size / 2);
    for i in 1..size {
        for j in 1..size {
            if i % j == 0 {
                table.grow_for_subtable(j);
                table.insert_unique(j, i);
            }
        }
    }
    table.test_iter_all();

    let size = 1000;

    for i in 1..size {
        for j in 1..size {
            if (i ^ j) % 7 == 0 {
                table.grow_for_subtable(i);
                table.insert(i, j);
            }
        }
    }
}

#[test]
fn test_removal() {
    let mut flip_flop = false;
    for size in [2, 3, 4, 5, 10, 100, 1000, 10000] {
        let mut table = <TestTableSeq<usize>>::default();

        for i in 1..size.min(1000000 / size) {
            for j in 1..size {
                if (i ^ j) % 7 == 0 {
                    table.grow_for_subtable(i);
                    table.insert(i, j);
                }
            }
        }
        table.resize(table.spec.len() - table.spec.len() / 10);
        for p in [11, 5, 3, 2, 7] {
            for i in 1..size.min(1000000 / size) {
                table.grow_for_subtable(i);
                for j in 1..size {
                    if (i ^ j) % p == 0 {
                        table.remove(i, &j);
                    }
                    if (i ^ !j) % p == 0 && table.spec[i].len() <= p {
                        flip_flop = !flip_flop;
                        if flip_flop {
                            table.clear_subtable(i);
                        } else {
                            table.test_drain(i);
                        }
                        continue;
                    }
                }
            }
            table.test_iter_all();
        }
        assert_eq!(table.under_test.flat_len(), 0);
    }
}

#[test]
fn test_insertion_with_entry() {
    let mut table = <TestTableSeq<usize>>::default();
    let size = 10000;
    table.resize(size / 2);
    for i in 1..size {
        for j in 1..size {
            if i % j == 0 {
                table.grow_for_subtable(j);
                table.insert_with_entry(j, i);
            }
        }
    }
    table.test_iter_all();

    let size = 1000;

    for i in 1..size {
        for j in 1..size {
            if (i ^ j) % 7 == 0 {
                table.grow_for_subtable(i);
                table.insert_with_entry(i, j);
            }
        }
    }

    table.test_iter_all();
}

#[test]
fn test_removal_with_entry() {
    let mut flip_flop = false;
    for size in [2, 3, 4, 5, 10, 100, 1000, 10000] {
        let mut table = <TestTableSeq<usize>>::default();

        for i in 1..size.min(1000000 / size) {
            for j in 1..size {
                if (i ^ j) % 7 == 0 {
                    table.grow_for_subtable(i);
                    table.insert(i, j);
                }
            }
        }
        table.resize(table.spec.len() - table.spec.len() / 10);
        for p in [11, 5, 3, 2, 7] {
            for i in 1..size.min(1000000 / size) {
                table.grow_for_subtable(i);
                for j in 1..size {
                    if (i ^ j) % p == 0 {
                        table.remove_with_entry(i, &j);
                    }
                    if (i ^ !j) % p == 0 && table.spec[i].len() <= p {
                        flip_flop = !flip_flop;
                        if flip_flop {
                            table.clear_subtable(i);
                        } else {
                            table.test_drain(i);
                        }
                        continue;
                    }
                }
            }
            table.test_iter_all();
        }
        assert_eq!(table.under_test.flat_len(), 0);
    }
}

#[test]
fn test_mutation() {
    let mut table = <TestTableSeq<MapEntry<usize, usize>>>::default();
    let size = 100;
    table.resize(size / 2);
    for i in 1..size {
        for j in 1..size {
            if i % j == 0 {
                table.grow_for_subtable(j);
                table.insert_unique(j, MapEntry { key: i, value: 0 });
            }
        }
    }

    for p in [11, 5, 3, 2, 7] {
        for i in 1..size {
            assert_eq!(
                table.under_test.subtable_iter_mut(i).len(),
                table.spec[i].len()
            );
            for entry in table.under_test.subtable_iter_mut(i) {
                entry.value += 1;
            }
            for entry in table.spec[i].iter_mut() {
                entry.value += 1;
            }

            for j in 1..size {
                if (i ^ j) % p == 0 {
                    if let Some((a, b)) = table.find_mut(j, &MapEntry { key: i, value: 0 }) {
                        assert_eq!(a, b);
                        a.value += 1;
                        b.value += 1;
                    }
                }
            }
        }
    }
}

#[test]
fn test_debug() {
    let mut table = TestTableSeq::default();
    for i in 0..10 {
        table.grow_for_subtable(i);
        table.insert(i, i);
    }
    assert_eq!(
        format!("{:?}", table.spec),
        format!("{:?}", table.under_test)
    );
}
