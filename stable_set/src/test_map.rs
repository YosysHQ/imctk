#![allow(missing_docs)]
use crate::StableMap;
use indexmap::IndexMap;
use rand::prelude::*;
use std::{
    borrow::Borrow,
    fmt::Debug,
    hash::{BuildHasherDefault, Hash},
};
use zwohash::ZwoHasher;

type ZwoMap<K, V> = StableMap<K, V, BuildHasherDefault<ZwoHasher>, u8>;

struct CheckedMap<K, V> {
    dut: ZwoMap<K, V>,
    ref_set: IndexMap<K, V>,
}

impl<K: Hash + Eq + Clone + Debug, V: Eq + Clone + Debug> CheckedMap<K, V> {
    fn new() -> Self {
        CheckedMap {
            dut: ZwoMap::new(),
            ref_set: IndexMap::new(),
        }
    }
    fn len(&self) -> usize {
        self.ref_set.len()
    }
    fn get_full<Q>(&mut self, key: &Q) -> Option<(usize, &K, &V)>
    where
        Q: Hash + Eq,
        K: Borrow<Q>,
    {
        let ref_result = self.ref_set.get_full(key);
        let dut_result = self.dut.get_full(key);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn get_index(&mut self, index: usize) -> Option<(&K, &V)> {
        let ref_result = self.ref_set.get_index(index);
        let dut_result = self.dut.get_index(index);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn insert_full(&mut self, key: K, value: V) -> (usize, Option<V>) {
        let ref_result = self.ref_set.insert_full(key.clone(), value.clone());
        let dut_result = self.dut.insert_full(key, value);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn swap_remove_full<Q>(&mut self, key: &Q) -> Option<(usize, K, V)>
    where
        Q: Hash + Eq,
        K: Borrow<Q>,
    {
        let ref_result = self.ref_set.swap_remove_full(key);
        let dut_result = self.dut.swap_remove_full(key);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn swap_remove_index(&mut self, index: usize) -> Option<(K, V)> {
        let ref_result = self.ref_set.swap_remove_index(index);
        let dut_result = self.dut.swap_remove_index(index);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn retain(&mut self, f: impl Fn(&K, &mut V) -> bool) {
        let mut ref_iter = self.ref_set.iter();
        self.dut.retain(|k, v| {
            // make sure that retain visits in the correct order
            assert_eq!(ref_iter.next(), Some((k, &*v)));
            f(k, v)
        });
        assert_eq!(ref_iter.next(), None);
        self.ref_set.retain(f);
        self.check();
    }
    fn drain(&mut self, r: impl std::ops::RangeBounds<usize> + Clone) {
        let ref_iter = self.ref_set.drain(r.clone());
        let dut_iter = self.dut.drain(r);
        assert!(Iterator::eq(ref_iter, dut_iter));
        self.dut.check();
    }
    fn check(&mut self) {
        self.dut.check();
        assert!(Iterator::eq(self.ref_set.iter(), self.dut.iter()));
    }
    fn iterator_check(&mut self) {
        assert!(Iterator::eq(self.ref_set.iter_mut(), self.dut.iter_mut()));
        assert!(Iterator::eq(self.ref_set.keys(), self.dut.keys()));
        assert!(Iterator::eq(self.ref_set.values(), self.dut.values()));
        assert!(Iterator::eq(
            self.ref_set.values_mut(),
            self.dut.values_mut()
        ));
    }
    fn entry_insert_or_remove(&mut self, key: K, value: V) -> impl Debug + '_ {
        #[derive(Debug, PartialEq, Eq)]
        enum OpResult<'a, K, V> {
            Inserted(&'a mut V),
            Removed((K, V)),
        }
        let ref_result = match self.ref_set.entry(key.clone()) {
            indexmap::map::Entry::Occupied(entry) => OpResult::Removed(entry.swap_remove_entry()),
            indexmap::map::Entry::Vacant(entry) => OpResult::Inserted(entry.insert(value.clone())),
        };
        let dut_result = match self.dut.entry(key) {
            crate::stable_map::Entry::Occupied(entry) => {
                OpResult::Removed(entry.swap_remove_entry())
            }
            crate::stable_map::Entry::Vacant(entry) => OpResult::Inserted(entry.insert(value)),
        };
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn entry_or_insert(&mut self, key: K, value: V) -> &mut V {
        let ref_result = self.ref_set.entry(key.clone()).or_insert(value.clone());
        let dut_result = self.dut.entry(key).or_insert(value);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    /// NB: `random_likelihood` is **not** a probability. `random_likelihood == 2.0` would be 2:1 odds random:present, i.e. 2/3 probability.
    fn present_or_random_key<R: Rng + SeedableRng>(
        &self,
        random_likelihood: f64,
        rng: &mut R,
        mut rand_k: impl FnMut(&mut R) -> K,
    ) -> K {
        debug_assert!(random_likelihood >= 0.0);
        if self.len() == 0 || rng.gen_range(0.0..1.0 + random_likelihood) >= 1.0 {
            rand_k(rng)
        } else {
            self.ref_set.iter().choose(rng).unwrap().0.clone()
        }
    }
    fn random_index<R: Rng + SeedableRng>(&self, error_likelihood: f64, rng: &mut R) -> usize {
        let max = (self.len() as f64 * (1.0 + error_likelihood)).ceil() as usize;
        rng.gen_range(0..=max)
    }
}

macro_rules! weighted_choose {
    ($rng:expr, $($name:ident: $weight:expr => $body:expr),+) => {
        {
            enum Branches { $( $name,  )* }
            let weights = [$((Branches::$name, $weight)),+];
            match weights.choose_weighted($rng, |x| x.1).unwrap().0 {
                $(Branches::$name => $body),*
            }
        }
    }
}

fn test_suite<K, V, R>(
    mut rand_k: impl FnMut(&mut R) -> K,
    mut rand_v: impl FnMut(&mut R) -> V,
    retain_fn: impl Fn(&K, &mut V) -> bool,
) where
    K: Hash + Eq + Clone + Debug,
    V: Eq + Clone + Debug,
    R: Rng + SeedableRng,
{
    let mut map: CheckedMap<K, V> = CheckedMap::new();
    let mut rng = R::seed_from_u64(39);
    let mut max_size = 0;
    let verbosity = 1;
    for _ in 0..5000 {
        weighted_choose! {&mut rng,
            Insert: 2.0 => {
                let k = map.present_or_random_key(6.0, &mut rng, &mut rand_k);
                let v = rand_v(&mut rng);
                let result = map.insert_full(k.clone(), v.clone());
                if verbosity > 0 {
                    println!("inserting {k:?}: {v:?} -> {result:?}");
                }
            },
            Get: 0.5 => {
                let k = map.present_or_random_key(1.0, &mut rng, &mut rand_k);
                let result = map.get_full(&k);
                if verbosity > 0 {
                    println!("getting {k:?} -> {result:?}");
                }
            },
            GetIndex: 0.3 => {
                let index = map.random_index(0.1, &mut rng);
                let result = map.get_index(index);
                if verbosity > 0 {
                    println!("getting index {index:?} -> {result:?}");
                }
            },
            Remove: 0.5 => {
                let k = map.present_or_random_key(1.0, &mut rng, &mut rand_k);
                let result = map.swap_remove_full(&k);
                if verbosity > 0 {
                    println!("removing {k:?} -> {result:?}");
                }
            },
            RemoveIndex: 0.2 => {
                let index = map.random_index(0.1, &mut rng);
                let result = map.swap_remove_index(index);
                if verbosity > 0 {
                    println!("removing index {index:?} -> {result:?}");
                }
            },
            Retain: 0.05 => {
                let old_len = map.len();
                map.retain(&retain_fn);
                let new_len = map.len();
                if verbosity > 0 {
                    println!("retaining, {old_len} -> {new_len}");
                }
            },
            Drain: 0.05 => {
                if map.len() > 0 {
                    let start_index = rng.gen_range(0..map.len());
                    let max_len = rng.gen_range(1..=map.len().div_ceil(10));
                    let end_index = std::cmp::min(map.len(), start_index + max_len);
                    map.drain(start_index..end_index);
                    if verbosity > 0 {
                        println!("draining {start_index}..{end_index}");
                    }
                }
            },
            EntryInsertOrRemove: 0.3 => {
                let key = map.present_or_random_key(1.0, &mut rng, &mut rand_k);
                let value = rand_v(&mut rng);
                let result = map.entry_insert_or_remove(key.clone(), value.clone());
                if verbosity > 0 {
                    println!("entry insert/remove {key:?}: {value:?} -> {result:?}");
                }
            },
            EntryOrInsert: 0.3 => {
                let key = map.present_or_random_key(1.0, &mut rng, &mut rand_k);
                let value = rand_v(&mut rng);
                let result = map.entry_or_insert(key.clone(), value.clone());
                if verbosity > 0 {
                    println!("entry or_insert {key:?}: {value:?} -> {result:?}");
                }
            },
            Check: 0.15 => {
                map.check();
            }
        };
        max_size = std::cmp::max(max_size, map.len());
    }
    map.check();
    map.iterator_check();
    println!("max size {max_size}");
}

#[test]
fn test_suite_usize_usize() {
    test_suite::<usize, usize, rand_pcg::Pcg64>(
        |rng| rng.gen::<usize>() >> rng.gen_range(0..usize::BITS),
        |rng| rng.gen(),
        |k, v| {
            *v = v.wrapping_add(3);
            k % 7 < 6
        },
    );
}

#[test]
fn test_suite_boxed_usize_boxed_usize() {
    test_suite::<Box<usize>, Box<usize>, rand_pcg::Pcg64>(
        |rng| Box::new(rng.gen::<usize>() >> rng.gen_range(0..usize::BITS)),
        |rng| Box::new(rng.gen()),
        |k, v| {
            **v = v.wrapping_add(3);
            **k % 7 < 6
        },
    );
}

#[test]
fn test_suite_string_u64() {
    test_suite::<String, u64, rand_pcg::Pcg64>(
        |rng| {
            let len = rng.gen_range(4..16);
            String::from_iter((0..len).map(|_| rng.gen_range('!'..'~')))
        },
        |rng| rng.gen(),
        |k: &String, v| {
            *v = (*v).wrapping_add(3);
            !k.contains('!')
        },
    );
}

#[test]
fn test_suite_string_string() {
    test_suite::<String, String, rand_pcg::Pcg64>(
        |rng| {
            let len = rng.gen_range(4..16);
            String::from_iter((0..len).map(|_| rng.gen_range('!'..'~')))
        },
        |rng| {
            let len = rng.gen_range(8..32);
            String::from_iter((0..len).map(|_| rng.gen_range('!'..'~')))
        },
        |k: &String, v| {
            let ch = v.remove(0);
            v.push(ch);
            !k.contains('!')
        },
    );
}

fn dedup_vec<K: Hash + Eq + Clone, V>(vec: &mut Vec<(K, V)>) {
    let mut seen = std::collections::HashSet::new();
    vec.retain(|item| seen.insert(item.0.clone()));
}

#[test]
fn test_basic() {
    let mut map: ZwoMap<String, usize> = Default::default();
    map.insert("adam".into(), 10);
    map.insert("eve".into(), 23);
    map.insert("mallory".into(), 40);
    map.insert("jim".into(), 5);
    assert_eq!(map.get("adam").copied(), Some(10));
    assert_eq!(map.get_index_of("eve"), Some(1));
    assert_eq!(map.get_full("jim"), Some((3, &"jim".into(), &5)));
    assert_eq!(map.get_index(2), Some((&"mallory".into(), &40)));
    assert_eq!(map.insert_full("jim".into(), 15), (3, Some(5)));
    assert_eq!(map.swap_remove("eve"), Some(23));
    assert_eq!(map.get_index(1), Some((&"jim".into(), &15)));
    assert_eq!(map.keys().collect::<Vec<_>>(), ["adam", "jim", "mallory"]);
    assert_eq!(map.values().collect::<Vec<_>>(), [&10, &15, &40]);
    assert_eq!(map.len(), 3);
    assert!(!map.is_empty());
    map.check();
}

#[test]
fn test_retain() {
    let mut rng = rand_pcg::Pcg64::seed_from_u64(10);
    let mut in_data: Vec<(u32, u64)> = (0..1000).map(|_| (rng.gen(), rng.gen())).collect();
    dedup_vec(&mut in_data);
    let mut map: ZwoMap<u32, u64> = in_data.iter().copied().collect();
    map.retain(|key, value| (*key as u64).wrapping_add(*value) % 7 == 4);
    map.check();
    let mut out_data = in_data.clone();
    out_data.retain(|&(key, value)| (key as u64).wrapping_add(value) % 7 == 4);
    assert_eq!(
        map.iter().map(|(&k, &v)| (k, v)).collect::<Vec<_>>(),
        out_data
    );
}

#[test]
fn test_drain() {
    let mut rng = rand_pcg::Pcg64::seed_from_u64(11);
    let mut in_data: Vec<(u32, u64)> = (0..1000).map(|_| (rng.gen(), rng.gen())).collect();
    dedup_vec(&mut in_data);
    let mut map: ZwoMap<u32, u64> = in_data.iter().copied().collect();
    let map_drain = map.drain(100..150);
    let mut out_data = in_data.clone();
    let out_data_drain = out_data.drain(100..150);
    assert!(map_drain.eq(out_data_drain));
    map.check();
    assert_eq!(
        map.iter().map(|(&k, &v)| (k, v)).collect::<Vec<_>>(),
        out_data
    );
}
