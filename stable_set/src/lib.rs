//! [StableMap] and [StableSet] are a hash map and hash set that preserve the order of their entries,
//! i.e. the order in which entries are inserted is remembered and iterators yield elements in that order.
//! 
//! Strictly speaking, this only holds if elements are not removed, as removal via the `swap_remove_*` series of methods perturbs the order
//! (use the more expensive `retain` operation to remove elements while preserving their order).
//! 
//! Both structs are implemented as a `Vec` storing the actual entries, supplemented by a hashbrown `HashTable` to allow for fast lookups.
//! This table only stores indices into the `Vec`.
//! 
//! This crate is very similar to the `index_map` crate, however there are two key differences:
//! 
//! 1. Hashes are not stored with the entries, but instead recalculated when needed.
//!    This saves memory and potentially improves performance when hashes are cheap to calculate.
//! 2. For small tables, the index table stores `u32` values, again saving memory.
//!    Tables are automatically upgraded to `usize` values as needed.
//!
//! Memory usage is approximately `(k + key size + value size) * num entries` bytes where `k` is 5 for small tables (less than 4 billion entries) and 9 otherwise.

mod index_table;
mod util;

pub use stable_set::StableSet;
pub use stable_map::StableMap;

pub mod stable_set;
pub mod stable_map;