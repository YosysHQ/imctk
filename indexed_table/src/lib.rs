#![warn(missing_docs)]
//! Memory optimized indexed sequences of hash tables.
//!
//! This crate provides the [`IndexedTable`] type, which is a memory and runtime efficient
//! replacement for `Vec<HashTable<T>>`. In particular, it is optimized for the use-case where the
//! vast majority of contained hash tables are tiny, each having 16 or fewer entries, while still
//! allowing for a small but significant fraction of tables to be large.
//!
//! ## Motivation and Comparision to Alternatives
//!
//! For sufficiently sparse but large sequences of tables, using [`IndexedTable`] only requires a
//! small fraction of the memory used by a `Vec<HashTable<T>>`. This often comes with reduced cache
//! pressure, increasing performance at the same itme.
//!
//! Compared to alternatives that combine all subtables into a single hash table by making the
//! subtable index part of the key, [`IndexedTable`] has two major advantages:
//!
//! * It maintains locality of subtables, which can result in a large performance advantage for use
//! cases where consecutive accesses are typically to the same subtable or to the same small set of
//! subtables. Using a single hash table would spread those accesses randomly across all the full
//! table, hitting a performance cliff each time the full table size exceeds the size of a cache
//! level. With an [`IndexedTable`] this only happens with the combined size of currently "hot"
//! subtables exceeds the cache size.
//!
//! * An [`IndexedTable`] provides methods for iterating over individual subtables. Such methods
//! usually can't be provided when using a single hash table without storing and maintaining
//! additional metadata.
//!
//! Compared to comparison based alternatives like B-trees, which provide even more flexibility with
//! regards to iteration over subranges, [`IndexedTable`] provides better performance for large
//! class of access patterns on sufficiently large collections, as it gets away with an essentially
//! constant, but more importantly small number of cache misses per random accesses. At the same
//! time it can exploit locality on the level of subtables, somewhat reducing the advantage B-trees
//! have over a single big hash table.
//!
//! Compared to comparison based alternatives like B-forests (a collection of B-trees), where each
//! subtable is a separate tree, [`IndexedTable`] tends to have much lower memory usage when many
//! subtables have only one or two entries, as this will waste a lot of memory in mostly empty
//! B-tree root nodes. Note that the optimizations that [`IndexedTable`] uses here are not specific
//! to hash tables and could also be applied to a collection of B-trees, only creating root nodes
//! when a subtable becomes sufficiently large to justify the memory requirements.
//!
//! ## Implementation Details
//!
//! This section describes implementation details which are not publicly exposed, but can be helpful
//! to developers who want to gain a better understanding of the memory usage and runtime behavior
//! when using this type.
//!
//! The details in this section are purely informative and are not part of the any stable API.
//!
//! Internally, an [`IndexedTable`] groups subtables into chunks of 16 consecutive subtables
//! (including empty subtables).
//!
//! Each chunk has a 32-bit bitmap with 2 bits per subtable to distinguish between the following
//! four cases:
//!
//! * an empty subtable,
//! * a singleton subtable,
//! * a subtable containing a pair of entries, or
//! * a larger subtable.
//!
//! In addition to the bitmap, a chunk has a 32-bit reference to a node containing the chunk's data.
//! This means the minimal metadata overhead when storing only empty subtables is 64 bit per chunk
//! or 4 bit per subtable. Subtables with not more than two entries are stored inline in the chunk's
//! data node, this requiring no metadata apart from the chunk's bitmap. For larger subtables, only
//! the subtable metadata is stored inline.
//!
//! To further reduce the metadata overhead and to allow the use of 32-bit references, data nodes
//! are not directly allocated from the global allocator. To limit the required size for node
//! references, an indexed table uses a separate node allocator for every `1 << 20` subtables[^1],
//! which adds only a small fraction of a bit to the minimal metadata overhead given a sufficiently
//! large number of subtables.
//!
//! [^1]: Due to a technicality we add allocators in pairs and thus require two allocators for
//!     every span of `1 << 21` subtables.
//!
//! Each node allocator maintains a dynamically growing slab for 16 different size classes. Each
//! slab packs individual nodes without any extra metadata. The size class of a node is encoded as
//! part of the 32-bit reference, avoiding the need to store it separately.
//!
//! Managing our own allocations from slabs of consecutive nodes also reduces fragmentation in the
//! global allocator. At the same time it can result in internal fragmentation as the chunks
//! belonging to an allocator move between different size classes. This can already be an
//! improvement, as this fragmentation does not persist beyond the [`IndexedTable`]'s lifetime, but
//! as an additional countermeasure we perform automatic periodic defragmentation of allocators
//! having poor utilization. This is done on a pair of allocators at a time, as opposed to the whole
//! indexed table, to avoid latency spikes that scale linearly with the number of subtables.
//!
//! For storing larger subtables, there are two different representations. With both of them, the
//! chunk's node contains only the subtable metadata. Their entries are stored externally, either in
//! other slab allocated nodes or in storage obtained from the global allocator.
//!
//! For subtables with up to 16 entries we use a compact hashtable where all entries occupy
//! consecutive slots and where 8-bit hashes of all entries are stored as part of the metadata. The
//! entries themselves are stored in another node, again using a compact 32-bit reference. [^2]
//!
//! For subtables with more than 16 entries, we use hashbrown's [`HashTable`]. While the
//! [`HashTable`] struct itself is stored inline, avoiding an extra indirection, the [`HashTable`]
//! itself will allocate storage for the entries from the global allocator.
//!
//! [^2]: By using pairs of node allocators we can ensure that the chunk's data node and the entries
//!     of the contained small tables occupy different allocators. Since our slabs are dynamically
//!     growing, we cannot allocate a new node without potentially invalidating pointers to any
//!     existing node of the same size class. Thus, sharing the same allocator for both would make
//!     pointer invalidation during small hash table operations quite complicated.

#[cfg(doc)]
use hashbrown::HashTable;

mod node_allocator;

pub mod indexed_table;

#[doc(inline)]
pub use indexed_table::IndexedTable;

// TODO Implement at least IndexedSet and IndexedMap on top of IndexedTable
