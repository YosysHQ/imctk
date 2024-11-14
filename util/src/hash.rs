//! Utilities for computing hashes.
use std::hash::{BuildHasher, BuildHasherDefault as StdBuildHasherDefault};

use zwohash::ZwoHasher;

pub type BuildHasherDefault<Hasher = ZwoHasher> = StdBuildHasherDefault<Hasher>;

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
pub fn hash_ref<T: std::hash::Hash + ?Sized>(value: &T) -> u64 {
    hash_value(value)
}
