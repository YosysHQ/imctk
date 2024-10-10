#![cfg(test)]
#![allow(missing_docs)]
use crate::StableSet;
use std::hash::BuildHasherDefault;
use zwohash::ZwoHasher;

type ZwoSet<T> = StableSet<T, BuildHasherDefault<ZwoHasher>>;

#[test]
fn test_primes() {
    let mut set: ZwoSet<usize> = (2..200).collect();
    let mut ref_primes = [
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89,
        97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181,
        191, 193, 197, 199
    ].into_iter();
    while let Some(&k) = set.first() {
        assert_eq!(ref_primes.next(), Some(k));
        set.retain(|&n| n % k != 0);
        set.check();
    }
    assert_eq!(ref_primes.next(), None);
    assert!(set.is_empty());
}