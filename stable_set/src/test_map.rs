#![cfg(test)]
#![allow(missing_docs)]
use crate::StableMap;
use rand::prelude::*;
use std::hash::BuildHasherDefault;
use zwohash::ZwoHasher;
use std::hash::Hash;

type ZwoMap<K, V> = StableMap<K, V, BuildHasherDefault<ZwoHasher>>;

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
    assert_eq!(map.iter().map(|(&k, &v)| (k, v)).collect::<Vec<_>>(), out_data);
}