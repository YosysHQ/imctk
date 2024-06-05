//! Indexed sequence of low-level hash tables with explicit hashing and associated helper types.
use std::{marker::PhantomData, mem::ManuallyDrop};

use hashbrown::HashTable;

use crate::node_allocator::AllocatorClass;

mod chunk;
mod iter;
mod node;
mod owned;
mod table;

use chunk::{Chunk, EntryType, CHUNK_MASK, CHUNK_SHIFT, CHUNK_SIZE};
use node::{NodeAllocator, NodeRef, SizeClass};
use owned::OwnedSubtableSmall;
use table::{SmallSubtable, Subtable};

pub use iter::{SubtableIter, SubtableIterMut};
pub use owned::OwnedSubtable;

/// Indexed sequence of low-level hash tables with explicit hashing.
///
/// This type serves as a memory and runtime efficient replacement for `Vec<HashTable<T>>`. In
/// particular, it is optimized for the use-case where the vast majority of contained hash tables
/// are tiny, each having 16 or fewer entries, while still allowing for a small but significant
/// fraction of tables to be large.
///
/// The provided API is loosely based on hashbrown's [`HashTable`] and on std's [`Vec`]. For methods
/// that operate on individual entries, we include a `subtable` suffix or prefix to indicate
/// `Vec`-level methods that operate on whole subtables instead of individual entries. For methods
/// that operate on collections of entries, we include a `subtable` suffix or prefix to indicate
/// `HashTable`-level methods that operate on an individual subtable instead of the collection of
/// all subtables. For methods that operate on all entries of all subtables we include `flat` suffix
/// or prefix.

pub struct IndexedTable<T> {
    _phantom: PhantomData<T>,
    allocators: Vec<NodeAllocator<T>>,
    subtables: usize,
    entries: usize,
    defrag_counter: u16,
    allocator_sweep: usize,
    chunks: Vec<Chunk<T>>,
}

impl<T> Default for IndexedTable<T> {
    fn default() -> Self {
        Self {
            _phantom: PhantomData,
            chunks: Default::default(),
            allocators: Default::default(),
            allocator_sweep: 0,
            defrag_counter: 0,
            subtables: 0,
            entries: 0,
        }
    }
}

const ALLOCATOR_SHIFT: u32 = 20;

/// Drop guard to preven exposing invalid entries on panics.
struct InvalidateChunkOnDrop<'a, T>(&'a mut Chunk<T>);

impl<'a, T> Drop for InvalidateChunkOnDrop<'a, T> {
    fn drop(&mut self) {
        // This is only called when the drop implementation of an entry panics. In that situation,
        // we do the minimum necessary to remain memory safe but spend no effort cleaning up. Since
        // we may not expose the entry that panicked on drop, we mark the complete chunk as empty.
        // This leaks everything else in the same chunk, but it is memory safe.

        debug_assert!(
            false,
            "IndexedTable does not support storing values that panic when dropped"
        );
        self.0.meta = Default::default();
    }
}

impl<'a, T> InvalidateChunkOnDrop<'a, T> {
    fn defuse(self) {
        let _ = ManuallyDrop::new(self);
    }
}

impl<T> IndexedTable<T> {
    /// Resizes the indexed table to a given number of subtables.
    ///
    /// When this is used to increase the number of subtables, empty subtables are appended at the
    /// end. When used to decrease the number of subtables, excess subtables at the end are
    /// discarded, including any entries they may have contained before.
    ///
    /// This is the primary way to create new empty subtables within an indexed table.
    ///
    /// # Examples
    ///
    /// ```should_panic
    /// use indexed_table::IndexedTable;
    /// let mut table: IndexedTable<u64> = Default::default();
    /// let hasher = |&value: &u64| value.wrapping_mul(0x2545f4914f6cdd1d);
    ///
    /// // Subtables are not implicitly created, so this will panic!
    /// table.insert_unique(5, 42, hasher(&42), hasher);
    /// ```
    ///
    /// ```
    /// # use indexed_table::IndexedTable;
    /// # let mut table: IndexedTable<u64> = Default::default();
    /// # let hasher = |&value: &u64| value.wrapping_mul(0x2545f4914f6cdd1d);
    /// #
    /// // Initializing subtables with `resize` avoids this panic
    /// table.resize(10);
    /// table.insert_unique(5, 42, hasher(&42), hasher);
    /// table.insert_unique(5, 43, hasher(&43), hasher);
    /// table.insert_unique(6, 44, hasher(&44), hasher);
    ///
    /// assert_eq!(table.len(), 10);
    /// assert_eq!(table.flat_len(), 3);
    ///
    /// // Reducing the number of subtables also removes their entries
    /// table.resize(8);
    /// assert_eq!(table.len(), 8);
    /// assert_eq!(table.flat_len(), 3);
    ///
    /// table.resize(6);
    /// assert_eq!(table.len(), 6);
    /// assert_eq!(table.flat_len(), 2);
    ///
    /// table.resize(2);
    /// assert_eq!(table.len(), 2);
    /// assert_eq!(table.flat_len(), 0);
    /// ```
    ///
    pub fn resize(&mut self, subtables: usize) {
        let chunks = subtables.div_ceil(CHUNK_SIZE);
        let allocators = subtables.div_ceil(1 << ALLOCATOR_SHIFT).next_multiple_of(2);

        if subtables < self.subtables {
            for chunk_index in chunks..self.chunks.len() {
                unsafe { self.drop_chunk(chunk_index) }
            }

            let chunk_subtables = (chunks * CHUNK_SIZE).min(self.subtables);
            for subtable in subtables..chunk_subtables {
                self.clear_subtable(subtable);
            }
        }

        self.chunks.resize_with(chunks, Default::default);
        self.allocators.resize_with(allocators, Default::default);
        self.subtables = subtables;
    }

    /// Returns the number of contained subtables.
    ///
    /// This does count empty subtables.
    ///
    /// See also [`subtable_len`][Self::subtable_len] and [`flat_len`][Self::flat_len].
    pub fn len(&self) -> usize {
        self.subtables
    }

    /// Returns `true` when this indexed table contains no subtables.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Hot-path for allocator defragmentation.
    ///
    /// We only check every `1 << 16` mutations, which should be sufficient with an allocator
    /// handling `1 << 20` subtables.
    #[inline]
    fn defragment_allocator(&mut self, allocator_index: usize) {
        self.defrag_counter = self.defrag_counter.wrapping_add(1);
        if self.defrag_counter == 0 {
            self.defragment_allocator_cold(allocator_index & !1);
        }
    }

    /// Cold-path for allocator defragmentation
    ///
    /// We alternate between checking the currently accessed allocator pair and a linear sweep
    /// through all allocators. The former means we react more quickly to usage patterns of local
    /// mutations that produce a lot of fragmentation in a single allocator, while the latter means
    /// we will eventually detect fragmentation even for allocators for which the accesses happen to
    /// consistently miss the defragmentation counter overflowing.
    #[cold]
    #[inline(never)]
    fn defragment_allocator_cold(&mut self, mut allocator_index: usize) {
        self.allocator_sweep += 1;
        if self.allocator_sweep == self.allocators.len() {
            self.allocator_sweep = 0;
        }
        if self.allocator_sweep & 1 == 0 {
            allocator_index = self.allocator_sweep;
        }
        let stats =
            self.allocators[allocator_index].stats + self.allocators[allocator_index + 1].stats;

        // Only bother if there's sufficient absolute and relative fragmentation.
        if stats.reserved < 256 || stats.used * 3 >= stats.reserved * 2 {
            return;
        }

        // We defragment by copying all nodes over to a new allocator pair, fixing up node
        // references in all associated chunks and chunk nodes.

        let mut new_allocator_0 = <NodeAllocator<T>>::default();
        let mut new_allocator_1 = <NodeAllocator<T>>::default();
        let mut new_allocators = [&mut new_allocator_0, &mut new_allocator_1];

        for chunk_index in (allocator_index) << (ALLOCATOR_SHIFT - CHUNK_SHIFT)
            ..(allocator_index + 2) << (ALLOCATOR_SHIFT - CHUNK_SHIFT)
        {
            let allocator_index = chunk_index >> (ALLOCATOR_SHIFT - CHUNK_SHIFT);

            let Some(chunk) = self.chunks.get_mut(chunk_index) else {
                break;
            };

            if chunk.meta.is_empty() {
                continue;
            }

            unsafe {
                let old_chunk_alloc = &mut self.allocators[allocator_index];
                let new_chunk_alloc = &mut new_allocators[allocator_index & 1];
                let size_class = chunk.node.size_class();
                let old_node_ptr = old_chunk_alloc.ptr(chunk.node);

                let new_node_ref = new_chunk_alloc.alloc(size_class);
                let new_node_ptr = new_chunk_alloc.ptr(new_node_ref);
                old_node_ptr
                    .cast::<u8>()
                    .copy_to(new_node_ptr.cast::<u8>(), size_class.size());

                chunk.node = new_node_ref;

                let new_node = chunk.node(new_chunk_alloc);

                for table in (*new_node.tables_raw()).iter_mut() {
                    if let Subtable::Small(table) = table {
                        let old_table_alloc = &mut self.allocators[allocator_index ^ 1];
                        let new_table_alloc = &mut new_allocators[(allocator_index & 1) ^ 1];

                        table.move_node(old_table_alloc, new_table_alloc);
                    }
                }
            }
        }

        self.allocators[allocator_index] = new_allocator_0;
        self.allocators[allocator_index ^ 1] = new_allocator_1;
    }

    /// Inserts an entry into a subtable using the given hash value.
    ///
    /// When an equivalent entry is already present, the subtable is not modified.
    ///
    /// Returns a pair containing a) mutable reference to an equivalent entry present in the table,
    /// either an existing or the newly inserted value and b) the passed value if an equivalent
    /// entry was already present.
    ///
    /// This method calls `eq` to determine if a candidate entry is equivalent. Any entry of the
    /// selected subtable is a potential argument for `eq`, but it will never be called with entries
    /// from other subtables.
    ///
    /// When switching representations or resizing large subtables, `hasher` will be called to
    /// re-compute hash values of the selected subtable. It will not be called for entries from
    /// other subtables.
    // TODO examples/doctests
    pub fn insert(
        &mut self,
        subtable: usize,
        value: T,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        hasher: impl Fn(&T) -> u64,
    ) -> (&mut T, Option<T>) {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            self.defragment_allocator(allocator_index);
            let chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);
            let chunk = self.chunks.get_unchecked_mut(chunk_index);

            if chunk.meta.is_empty() {
                self.entries += 1;
                let size_class = SizeClass::class_for_index(0);

                chunk.node = chunk_alloc.alloc(size_class);
                let entry_ptr = chunk_alloc.ptr(chunk.node);
                entry_ptr.write(value);
                chunk.meta.make_single(chunk_slot);
                return (&mut *entry_ptr, None);
            }

            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => {
                    self.entries += 1;
                    let mut node = chunk.node(chunk_alloc);

                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    node.make_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    let entry_ptr = node.entry_ptr(entry_offset);
                    entry_ptr.write(value);

                    chunk.meta.make_single(chunk_slot);
                    (&mut *entry_ptr, None)
                }
                EntryType::Single => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    if eq(&*found_entry_ptr) {
                        return (&mut *found_entry_ptr, Some(value));
                    }
                    self.entries += 1;

                    node.make_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    let entry_ptr = node.entry_ptr(entry_offset);
                    entry_ptr.write(value);

                    chunk.meta.make_pair(chunk_slot);
                    (&mut *entry_ptr, None)
                }
                EntryType::Pair => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);

                    for i in 0..2 {
                        let found_entry_ptr = node.entry_ptr(entry_offset + i);
                        if eq(&*found_entry_ptr) {
                            return (&mut *found_entry_ptr, Some(value));
                        }
                    }
                    self.entries += 1;

                    let found_pair = node.entry_ptr(entry_offset).cast::<[T; 2]>().read();
                    let table_offset = chunk.meta.table_offset(chunk_slot);

                    node.close_entry_pair_gap_and_make_table_gap_resize(
                        entry_offset,
                        table_offset,
                        chunk,
                        chunk_alloc,
                    );

                    let table_ptr = node.table_ptr(table_offset);

                    let table_alloc = &mut self.allocators[allocator_index ^ 1];
                    let (entry_ptr, table) =
                        SmallSubtable::new(found_pair, value, hash, hasher, table_alloc);

                    table_ptr.write(Subtable::Small(table));
                    chunk.meta.make_table(chunk_slot);

                    (&mut *entry_ptr, None)
                }
                EntryType::Table => {
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let node = chunk.node(chunk_alloc);

                    let table_ptr = node.table_ptr(table_offset);
                    let table = &mut *table_ptr;

                    match table {
                        Subtable::Large(table) => match table.entry(hash, eq, hasher) {
                            hashbrown::hash_table::Entry::Occupied(entry) => {
                                (entry.into_mut(), Some(value))
                            }
                            hashbrown::hash_table::Entry::Vacant(entry) => {
                                self.entries += 1;
                                (entry.insert(value).into_mut(), None)
                            }
                        },
                        Subtable::Small(int_table) => {
                            let table_alloc = &mut self.allocators[allocator_index ^ 1];

                            match int_table.insert(value, hash, eq, table_alloc) {
                                Ok((entry_ptr, value)) => {
                                    self.entries += value.is_none() as usize;
                                    (&mut *entry_ptr, value)
                                }
                                Err(value) => {
                                    let mut hash_table = HashTable::with_capacity(CHUNK_SIZE * 2);
                                    int_table.drain_and_dealloc_with(
                                        |value, _byte_hash| {
                                            hash_table.insert_unique(
                                                hasher(&value),
                                                value,
                                                &hasher,
                                            );
                                        },
                                        table_alloc,
                                    );

                                    *table = Subtable::Large(hash_table);
                                    let Subtable::Large(hash_table) = table else {
                                        unreachable!();
                                    };

                                    (
                                        hash_table.insert_unique(hash, value, &hasher).into_mut(),
                                        None,
                                    )
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    /// Inserts an entry into a subtable using the given hash value, without checking for whether an
    /// equivalent entry is already present.
    ///
    /// Returns a mutable reference to the entry containing the newly inserted value.
    ///
    /// When switching representations or resizing large subtables, `hasher` will be called to
    /// re-compute hash values of the selected subtable. It will not be called for entries from
    /// other subtables.
    // TODO examples/doctests
    pub fn insert_unique(
        &mut self,
        subtable: usize,
        value: T,
        hash: u64,
        hasher: impl Fn(&T) -> u64,
    ) -> &mut T {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            self.defragment_allocator(allocator_index);
            let chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);
            let chunk = self.chunks.get_unchecked_mut(chunk_index);

            self.entries += 1;

            if chunk.meta.is_empty() {
                let size_class = SizeClass::class_for_index(0);

                chunk.node = chunk_alloc.alloc(size_class);
                let entry_ptr = chunk_alloc.ptr(chunk.node);
                entry_ptr.write(value);
                chunk.meta.make_single(chunk_slot);
                return &mut *entry_ptr;
            }

            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => {
                    let mut node = chunk.node(chunk_alloc);

                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    node.make_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    let entry_ptr = node.entry_ptr(entry_offset);
                    entry_ptr.write(value);

                    chunk.meta.make_single(chunk_slot);
                    &mut *entry_ptr
                }
                EntryType::Single => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);

                    node.make_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    let entry_ptr = node.entry_ptr(entry_offset);
                    entry_ptr.write(value);

                    chunk.meta.make_pair(chunk_slot);
                    &mut *entry_ptr
                }
                EntryType::Pair => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);

                    let found_pair = node.entry_ptr(entry_offset).cast::<[T; 2]>().read();
                    let table_offset = chunk.meta.table_offset(chunk_slot);

                    node.close_entry_pair_gap_and_make_table_gap_resize(
                        entry_offset,
                        table_offset,
                        chunk,
                        chunk_alloc,
                    );

                    let table_ptr = node.table_ptr(table_offset);

                    let table_alloc = &mut self.allocators[allocator_index ^ 1];
                    let (entry_ptr, table) =
                        SmallSubtable::new(found_pair, value, hash, hasher, table_alloc);

                    table_ptr.write(Subtable::Small(table));
                    chunk.meta.make_table(chunk_slot);

                    &mut *entry_ptr
                }
                EntryType::Table => {
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let node = chunk.node(chunk_alloc);

                    let table_ptr = node.table_ptr(table_offset);
                    let table = &mut *table_ptr;

                    match table {
                        Subtable::Large(table) => {
                            table.insert_unique(hash, value, hasher).into_mut()
                        }
                        Subtable::Small(int_table) => {
                            let table_alloc = &mut self.allocators[allocator_index ^ 1];

                            match int_table.insert_unique(value, hash, table_alloc) {
                                Ok(entry_ptr) => &mut *entry_ptr,
                                Err(value) => {
                                    let mut hash_table = HashTable::with_capacity(CHUNK_SIZE * 2);
                                    int_table.drain_and_dealloc_with(
                                        |value, _byte_hash| {
                                            hash_table.insert_unique(
                                                hasher(&value),
                                                value,
                                                &hasher,
                                            );
                                        },
                                        table_alloc,
                                    );

                                    *table = Subtable::Large(hash_table);
                                    let Subtable::Large(hash_table) = table else {
                                        unreachable!();
                                    };

                                    hash_table.insert_unique(hash, value, &hasher).into_mut()
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // TODO find, find_mut

    // TODO entry API

    /// Finds and removes an entry from a subtable using the given hash value.
    ///
    /// When an equivalent entry is not found, the subtable is not modified, otherwise the value of
    /// the found entry is returned.
    ///
    /// This method calls `eq` to determine if a candidate entry is equivalent. Any entry of the
    /// selected subtable is a potential argument for `eq`, but it will never be called with entries
    /// from other subtables.
    ///
    /// When switching representations or resizing large subtables, `hasher` will be called to
    /// re-compute hash values of the selected subtable. It will not be called for entries
    /// from other subtables.
    // TODO examples/doctests
    pub fn remove(
        &mut self,
        subtable: usize,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        hasher: impl Fn(&T) -> u64,
    ) -> Option<T> {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            self.defragment_allocator(allocator_index);

            let chunk = self.chunks.get_unchecked_mut(chunk_index);
            let chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);

            if chunk.meta.is_empty() {
                return None;
            }
            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => None,
                EntryType::Single => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    if !eq(&*found_entry_ptr) {
                        return None;
                    }
                    self.entries -= 1;
                    let value = found_entry_ptr.read();

                    node.close_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }
                    Some(value)
                }
                EntryType::Pair => {
                    let mut node = chunk.node(chunk_alloc);
                    let mut entry_offset = chunk.meta.entry_offset(chunk_slot);

                    let mut to_check = 2;
                    let mut found_entry_ptr;
                    loop {
                        found_entry_ptr = node.entry_ptr(entry_offset);
                        if eq(&*found_entry_ptr) {
                            break;
                        }
                        to_check -= 1;
                        entry_offset += 1;
                        if to_check == 0 {
                            return None;
                        }
                    }
                    self.entries -= 1;
                    let value = found_entry_ptr.read();

                    node.close_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_single(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }
                    Some(value)
                }
                EntryType::Table => {
                    let mut node = chunk.node(chunk_alloc);
                    let table_offset = chunk.meta.table_offset(chunk_slot);

                    let table_ptr = node.table_ptr(table_offset);
                    let table = &mut *table_ptr;

                    match table {
                        Subtable::Large(table) => {
                            match table.find_entry(hash, eq) {
                                Ok(found) => {
                                    self.entries -= 1;
                                    let removed = found.remove().0;

                                    // TODO external -> internal shrinking
                                    if table.is_empty() {
                                        table_ptr.drop_in_place();
                                        node.close_table_gap_resize(
                                            table_offset,
                                            chunk,
                                            chunk_alloc,
                                        );
                                        chunk.meta.make_empty(chunk_slot);
                                        if chunk.meta.is_empty() {
                                            chunk_alloc.dealloc(chunk.node)
                                        }
                                    } else if table.capacity() > table.len() * 3 + 1024 {
                                        table.shrink_to_fit(hasher);
                                    }

                                    Some(removed)
                                }
                                Err(_) => None,
                            }
                        }
                        Subtable::Small(table) => {
                            let table_alloc = &mut self.allocators[allocator_index ^ 1];

                            let removed = table.remove(hash, eq, table_alloc);

                            self.entries -= removed.is_some() as usize;

                            // TODO earlier shrinking
                            if table.is_empty() {
                                table.drop_and_dealloc(table_alloc);
                                let chunk_alloc =
                                    self.allocators.get_unchecked_mut(allocator_index);
                                node.close_table_gap_resize(table_offset, chunk, chunk_alloc);
                                chunk.meta.make_empty(chunk_slot);
                                if chunk.meta.is_empty() {
                                    chunk_alloc.dealloc(chunk.node)
                                }
                            }

                            removed
                        }
                    }
                }
            }
        }
    }

    /// Discards all entries of a given subtable, leaving the subtable entry.
    ///
    /// The contained entries are dropped in place, see [`take_subtable`][Self::take_subtable] for a
    /// method that clears a subtable but returns the contained entries.
    // TODO examples/doctests
    pub fn clear_subtable(&mut self, subtable: usize) {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            self.defragment_allocator(allocator_index);

            let chunk = self.chunks.get_unchecked_mut(chunk_index);
            let mut chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);

            if chunk.meta.is_empty() {
                return;
            }
            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => (),
                EntryType::Single => {
                    self.entries -= 1;
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    let drop_guard = InvalidateChunkOnDrop(chunk);
                    found_entry_ptr.drop_in_place();
                    drop_guard.defuse();

                    node.close_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }
                }
                EntryType::Pair => {
                    self.entries -= 2;
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    let drop_guard = InvalidateChunkOnDrop(chunk);
                    found_entry_ptr.cast::<[T; 2]>().drop_in_place();
                    drop_guard.defuse();

                    node.close_entry_pair_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }
                }
                EntryType::Table => {
                    let mut node = chunk.node(chunk_alloc);
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let table_ptr = node.table_ptr(table_offset);

                    match &mut *table_ptr {
                        Subtable::Small(table) => {
                            self.entries -= table.len();
                            let table_alloc = &mut self.allocators[allocator_index ^ 1];
                            table.drop_and_dealloc(table_alloc);
                            chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);
                        }
                        Subtable::Large(table) => {
                            self.entries -= table.len();
                        }
                    }

                    let drop_guard = InvalidateChunkOnDrop(chunk);
                    table_ptr.drop_in_place();
                    drop_guard.defuse();

                    node.close_table_gap_resize(table_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }
                }
            }
        }
    }

    /// Removes and returns all entries from a subtable, leaving an empty subtable.
    ///
    /// See [`clear_subtable`][Self::take_subtable] for a method that clears a subtable while
    /// dropping all entries in place.
    // TODO should we specify that the returned subtable maintains the iteration order?
    // TODO examples/doctests
    pub fn take_subtable(&mut self, subtable: usize) -> OwnedSubtable<T> {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            self.defragment_allocator(allocator_index);

            let chunk = self.chunks.get_unchecked_mut(chunk_index);
            let chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);

            if chunk.meta.is_empty() {
                return OwnedSubtable::default();
            }
            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => OwnedSubtable::default(),
                EntryType::Single => {
                    self.entries -= 1;
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    let value = found_entry_ptr.read();

                    node.close_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }
                    OwnedSubtable::single(value)
                }
                EntryType::Pair => {
                    self.entries -= 2;
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    let pair = found_entry_ptr.cast::<[T; 2]>().read();

                    node.close_entry_pair_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }

                    OwnedSubtable::pair(pair)
                }
                EntryType::Table => {
                    let mut node = chunk.node(chunk_alloc);
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let table_ptr = node.table_ptr(table_offset);

                    let table = table_ptr.read();

                    node.close_table_gap_resize(table_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node)
                    }

                    match table {
                        Subtable::Large(table) => {
                            self.entries -= table.len();
                            OwnedSubtable::table(table)
                        }
                        Subtable::Small(mut table) => {
                            self.entries -= table.len();
                            let table_alloc =
                                self.allocators.get_unchecked_mut(allocator_index ^ 1);
                            let mut owned = OwnedSubtableSmall::default();
                            table.drain_and_dealloc_with(
                                |value, byte_hash| owned.push_with_hash_unchecked(value, byte_hash),
                                table_alloc,
                            );
                            OwnedSubtable::small(owned)
                        }
                    }
                }
            }
        }
    }

    /// Returns the number of entries of a given subtable.
    ///
    /// See also [`len`][Self::len] and [`flat_len`][Self::flat_len].
    // TODO examples/doctests
    pub fn subtable_len(&self, subtable: usize) -> usize {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            let Some(chunk) = self.chunks.get(chunk_index) else {
                return 0;
            };
            let chunk_alloc = self.allocators.get_unchecked(allocator_index);

            if chunk.meta.is_empty() {
                return 0;
            }
            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => 0,
                EntryType::Single => 1,
                EntryType::Pair => 2,
                EntryType::Table => {
                    let node = chunk.node(chunk_alloc);
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let table_ptr = node.table_ptr(table_offset);
                    let table = &*table_ptr;

                    match table {
                        Subtable::Large(table) => table.len(),
                        Subtable::Small(table) => table.len(),
                    }
                }
            }
        }
    }

    /// Returns the total number of entries across all subtables.
    ///
    /// See also [`len`][Self::len] and [`subtable_len`][Self::subtable_len].
    // TODO examples/doctests
    pub fn flat_len(&self) -> usize {
        self.entries
    }

    /// Returns an iterator yielding references for all entries of a subtable.
    ///
    /// Each subtable maintains a fixed iteration order that only changes with mutations. Beyond
    /// that, the iteration order is unspecified.
    // TODO serialization & deserialization maintaining iteration order!
    // TODO examples/doctests
    pub fn subtable_iter(&self, subtable: usize) -> SubtableIter<T> {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            let Some(chunk) = self.chunks.get(chunk_index) else {
                return Default::default();
            };
            let chunk_alloc = self.allocators.get_unchecked(allocator_index);

            if chunk.meta.is_empty() {
                return Default::default();
            }
            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => Default::default(),
                EntryType::Single => {
                    let node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset).cast_const();

                    SubtableIter::single(&*found_entry_ptr)
                }
                EntryType::Pair => {
                    let node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    let pair_ptr = found_entry_ptr.cast::<[T; 2]>().cast_const();

                    SubtableIter::pair(&*pair_ptr)
                }
                EntryType::Table => {
                    let node = chunk.node(chunk_alloc);
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let table_ptr = node.table_ptr(table_offset).cast_const();
                    let table = &*table_ptr;
                    match table {
                        Subtable::Large(table) => SubtableIter::table(table),
                        Subtable::Small(table) => {
                            let table_alloc = self.allocators.get_unchecked(allocator_index ^ 1);

                            SubtableIter::small(table.entries(table_alloc))
                        }
                    }
                }
            }
        }
    }

    /// Returns an iterator yielding mutable references for all entries of a subtable.
    ///
    /// Each subtable maintains a fixed iteration order that only changes with mutations. Beyond
    /// that, the iteration order is unspecified.
    // TODO serialization & deserialization maintaining iteration order!
    // TODO examples/doctests
    pub fn subtable_iter_mut(&mut self, subtable: usize) -> SubtableIterMut<T> {
        assert!(subtable < self.subtables);
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            let Some(chunk) = self.chunks.get(chunk_index) else {
                return Default::default();
            };
            let chunk_alloc = self.allocators.get_unchecked(allocator_index);

            if chunk.meta.is_empty() {
                return Default::default();
            }
            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => Default::default(),
                EntryType::Single => {
                    let node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    SubtableIterMut::single(&mut *found_entry_ptr)
                }
                EntryType::Pair => {
                    let node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    let pair_ptr = found_entry_ptr.cast::<[T; 2]>();

                    SubtableIterMut::pair(&mut *pair_ptr)
                }
                EntryType::Table => {
                    let node = chunk.node(chunk_alloc);
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let table_ptr = node.table_ptr(table_offset);
                    let table = &mut *table_ptr;
                    match table {
                        Subtable::Large(table) => SubtableIterMut::table(table),
                        Subtable::Small(table) => {
                            let table_alloc =
                                self.allocators.get_unchecked_mut(allocator_index ^ 1);

                            SubtableIterMut::small(table.entries_mut(table_alloc))
                        }
                    }
                }
            }
        }
    }
}

impl<T> IndexedTable<T> {
    unsafe fn drop_chunk(&mut self, chunk_index: usize) {
        let chunk = self.chunks.get_unchecked_mut(chunk_index);
        if !chunk.meta.is_empty() {
            let allocator_index = chunk_index >> (ALLOCATOR_SHIFT - CHUNK_SHIFT);
            let chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);
            let node = chunk.node(chunk_alloc);
            self.entries -= node.entry_count();
            if node.table_count() > 0 {
                let table_alloc = self.allocators.get_unchecked_mut(allocator_index ^ 1);
                let table_slice = node.tables_raw();
                for table in (*table_slice).iter_mut() {
                    match table {
                        Subtable::Small(table) => {
                            self.entries -= table.len();
                            table.drop_and_dealloc(table_alloc);
                        }
                        Subtable::Large(table) => {
                            self.entries -= table.len();
                        }
                    }
                }
                table_slice.drop_in_place();
            }
            node.entries_raw().drop_in_place();
        }
    }
}

impl<T> Drop for IndexedTable<T> {
    fn drop(&mut self) {
        for chunk_index in 0..self.chunks.len() {
            unsafe { self.drop_chunk(chunk_index) };
        }
    }
}
