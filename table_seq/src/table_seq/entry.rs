use crate::table_seq::*;

enum VacantEntryKind<'a, T> {
    EmptyChunk,
    EmptyTable,
    SingletonTable,
    // Since we can't store the hasher and we don't want to eagerly grow every pair into a small table,
    // we remember the hashes of the future entry and the two existing entries here.
    PairTable(u64, [u64; 2]),
    SmallTable(SmallSubtableVacantEntry<'a, T>),
    LargeTable(hashbrown::hash_table::VacantEntry<'a, T>),
}

/// A view into a vacant entry in a [`TableSeq`]'s subtable.
/// It is part of the [`Entry`] enum.
// SAFETY: The kind accurately describes the state of the subtable.
pub struct VacantEntry<'a, T> {
    tables: *mut TableSeq<T>,
    subtable: usize,
    kind: VacantEntryKind<'a, T>,
}

#[allow(clippy::enum_variant_names)] // more descriptive this way
enum OccupiedEntryKind<'a, T> {
    SingletonTable,
    // The `usize` argument is the index into the subtable (guaranteed to be either 0 or 1).
    PairTable(usize),
    // On both SmallTable and LargeTable the `bool` is true iff the OccupiedEntry is the only entry in the table.
    SmallTable(SmallSubtableOccupiedEntry<'a, T>, bool),
    // We need MaybeUninit here because we may deallocate the HashTable, at which point the OccupiedEntry becomes invalid.
    LargeTable(
        MaybeUninit<hashbrown::hash_table::OccupiedEntry<'a, T>>,
        bool,
    ),
}

/// A view into an occupied entry in a [`TableSeq`]'s subtable.
/// It is part of the [`Entry`] enum.
// SAFETY: The kind accurately describes the state of the subtable.
pub struct OccupiedEntry<'a, T> {
    tables: *mut TableSeq<T>,
    subtable: usize,
    entry_ptr: *mut T,
    kind: OccupiedEntryKind<'a, T>,
}

/// A view into a single entry in a [`TableSeq`]'s subtable, which may either be vacant or occupied.
///
/// This `enum` is constructed from the [`entry`] method on [`TableSeq`].
pub enum Entry<'a, T> {
    /// A vacant entry.
    Vacant(VacantEntry<'a, T>),
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, T>),
}

impl<T> TableSeq<T> {
    /// Gets the entry with the given hash value in a subtable for in-place manipulation.
    ///
    /// If there are existing entries with the hash value and `eq` returns true for one of them, an OccupiedEntry referring to that entry is returned.
    /// Otherwise, a VacantEntry is returned.
    ///
    /// `eq` may be called on any entries in the given subtable, but is never called on entries from other subtables.
    ///
    /// If VacantEntry is returned, `entry` may resize the subtable to prepare for insertion.
    /// In that case, `hasher` is called on all entries in the subtable to recompute their hash values.
    /// It will never be called on entries from another subtable.
    pub fn entry(
        &mut self,
        subtable: usize,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        hasher: impl Fn(&T) -> u64,
    ) -> Entry<'_, T> {
        assert!(subtable < self.subtables);
        // SAFETY: with the subtable checked to be in bounds, every unsafe call contained below
        // either requires just the global data structure invariants to hold or has documented
        // requirements that pair up with the immediately preceding or following operations.
        // TODO reduce unsafe scope and go into more detail
        unsafe {
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            self.defragment_allocator(allocator_index);
            let chunk_alloc = self.allocators.get_unchecked_mut(allocator_index);
            let chunk = self.chunks.get_unchecked_mut(chunk_index);

            if chunk.meta.is_empty() {
                return Entry::Vacant(VacantEntry {
                    tables: self,
                    subtable,
                    kind: VacantEntryKind::EmptyChunk,
                });
            }

            match chunk.meta.entry_type(chunk_slot) {
                EntryType::Empty => Entry::Vacant(VacantEntry {
                    tables: self,
                    subtable,
                    kind: VacantEntryKind::EmptyTable,
                }),
                EntryType::Single => {
                    let node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    let found_entry_ptr = node.entry_ptr(entry_offset);

                    if eq(&*found_entry_ptr) {
                        Entry::Occupied(OccupiedEntry {
                            tables: self,
                            subtable,
                            entry_ptr: found_entry_ptr,
                            kind: OccupiedEntryKind::SingletonTable,
                        })
                    } else {
                        Entry::Vacant(VacantEntry {
                            tables: self,
                            subtable,
                            kind: VacantEntryKind::SingletonTable,
                        })
                    }
                }
                EntryType::Pair => {
                    let node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);

                    for i in 0..2 {
                        let found_entry_ptr = node.entry_ptr(entry_offset + i);
                        if eq(&*found_entry_ptr) {
                            return Entry::Occupied(OccupiedEntry {
                                tables: self,
                                subtable,
                                entry_ptr: found_entry_ptr,
                                kind: OccupiedEntryKind::PairTable(i),
                            });
                        }
                    }
                    let hash0 = hasher(&*node.entry_ptr(entry_offset));
                    let hash1 = hasher(&*node.entry_ptr(entry_offset + 1));
                    Entry::Vacant(VacantEntry {
                        tables: self,
                        subtable,
                        kind: VacantEntryKind::PairTable(hash, [hash0, hash1]),
                    })
                }
                EntryType::Table => {
                    let table_offset = chunk.meta.table_offset(chunk_slot);
                    let node = chunk.node(chunk_alloc);
                    let table_ptr = node.table_ptr(table_offset);

                    match &mut *table_ptr {
                        Subtable::Large(table) => {
                            let is_single = table.len() == 1;
                            match table.entry(hash, eq, hasher) {
                                hashbrown::hash_table::Entry::Occupied(mut entry) => {
                                    Entry::Occupied(OccupiedEntry {
                                        tables: self,
                                        subtable,
                                        entry_ptr: &mut *entry.get_mut(),
                                        kind: OccupiedEntryKind::LargeTable(
                                            MaybeUninit::new(entry),
                                            is_single,
                                        ),
                                    })
                                }
                                hashbrown::hash_table::Entry::Vacant(entry) => {
                                    Entry::Vacant(VacantEntry {
                                        tables: self,
                                        subtable,
                                        kind: VacantEntryKind::LargeTable(entry),
                                    })
                                }
                            }
                        }
                        Subtable::Small(int_table) => {
                            let table_alloc = &mut self.allocators[allocator_index ^ 1];
                            let is_single = int_table.len() == 1;

                            match int_table.entry(hash, eq, table_alloc) {
                                SmallSubtableEntry::Occupied(mut entry) => {
                                    Entry::Occupied(OccupiedEntry {
                                        tables: self,
                                        subtable,
                                        entry_ptr: &mut *entry.get_mut(),
                                        kind: OccupiedEntryKind::SmallTable(entry, is_single),
                                    })
                                }
                                SmallSubtableEntry::Vacant(entry) => Entry::Vacant(VacantEntry {
                                    tables: self,
                                    subtable,
                                    kind: VacantEntryKind::SmallTable(entry),
                                }),
                                SmallSubtableEntry::FullTable(int_table) => {
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
                                    let _ = int_table; // int_table has been invalidated, don't use it anymore

                                    // SAFETY: int_table has been drained and invalidated, we can just overwrite it
                                    table_ptr.write(Subtable::Large(hash_table));
                                    let Some(Subtable::Large(hash_table)) = table_ptr.as_mut()
                                    else {
                                        unreachable!();
                                    };

                                    let hashbrown::hash_table::Entry::Vacant(new_entry) =
                                        hash_table.entry(hash, |_| false, &hasher)
                                    else {
                                        unreachable!()
                                    };
                                    Entry::Vacant(VacantEntry {
                                        tables: self,
                                        subtable,
                                        kind: VacantEntryKind::LargeTable(new_entry),
                                    })
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

impl<'a, T> VacantEntry<'a, T> {
    /// Inserts an entry into the subtable at the hash value corresponding to the `VacantEntry`.
    ///
    /// If `value` does not hash to that hash value, the table is left in an indeterminate, but memory-safe state.
    pub fn insert(self, value: T) -> OccupiedEntry<'a, T> {
        // SAFETY: all relevant preconditions hold by construction
        // TODO reduce unsafe scope and go into more detail
        unsafe {
            let VacantEntry {
                tables,
                subtable,
                kind,
            } = self;
            let chunk_slot = (self.subtable & CHUNK_MASK) as u32;
            let chunk_index = self.subtable >> CHUNK_SHIFT;
            let allocator_index = self.subtable >> ALLOCATOR_SHIFT;

            let chunk_alloc = (*tables).allocators.get_unchecked_mut(allocator_index);
            let chunk = (*tables).chunks.get_unchecked_mut(chunk_index);
            match kind {
                VacantEntryKind::EmptyChunk => {
                    (*tables).entries += 1;
                    let size_class = SizeClass::class_for_index(0);

                    chunk.node = chunk_alloc.alloc(size_class);
                    let entry_ptr = chunk_alloc.ptr(chunk.node);
                    entry_ptr.write(value);
                    chunk.meta.make_single(chunk_slot);
                    OccupiedEntry {
                        tables,
                        subtable,
                        entry_ptr,
                        kind: OccupiedEntryKind::SingletonTable,
                    }
                }
                VacantEntryKind::EmptyTable => {
                    (*tables).entries += 1;
                    let mut node = chunk.node(chunk_alloc);

                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    node.make_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    let entry_ptr = node.entry_ptr(entry_offset);
                    entry_ptr.write(value);

                    chunk.meta.make_single(chunk_slot);
                    OccupiedEntry {
                        tables,
                        subtable,
                        entry_ptr,
                        kind: OccupiedEntryKind::SingletonTable,
                    }
                }
                VacantEntryKind::SingletonTable => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    (*tables).entries += 1;

                    node.make_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    let entry_ptr = node.entry_ptr(entry_offset);
                    entry_ptr.write(value);

                    chunk.meta.make_pair(chunk_slot);
                    OccupiedEntry {
                        tables,
                        subtable,
                        entry_ptr,
                        kind: OccupiedEntryKind::PairTable(0),
                    }
                }
                VacantEntryKind::PairTable(hash, pair_hashes) => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);
                    (*tables).entries += 1;

                    let found_pair = node.entry_ptr(entry_offset).cast::<[T; 2]>().read();
                    let table_offset = chunk.meta.table_offset(chunk_slot);

                    node.close_entry_pair_gap_and_make_table_gap_resize(
                        entry_offset,
                        table_offset,
                        chunk,
                        chunk_alloc,
                    );

                    let table_ptr = node.table_ptr(table_offset);

                    let table_alloc = &mut (*tables).allocators[allocator_index ^ 1];
                    let (entry_ptr, table) =
                        SmallSubtable::new(found_pair, pair_hashes, value, hash, table_alloc);

                    table_ptr.write(Subtable::Small(table));
                    chunk.meta.make_table(chunk_slot);

                    let Subtable::Small(table) = table_ptr.as_mut().unwrap() else {
                        unreachable!()
                    };

                    OccupiedEntry {
                        tables,
                        subtable,
                        entry_ptr,
                        kind: OccupiedEntryKind::SmallTable(
                            SmallSubtableOccupiedEntry::from_entry_ptr(table, entry_ptr),
                            false,
                        ),
                    }
                }
                VacantEntryKind::SmallTable(vacant_entry) => {
                    let table_alloc = &mut (*tables).allocators[allocator_index ^ 1];
                    let mut new_entry = vacant_entry.insert(value, table_alloc);
                    (*tables).entries += 1;
                    OccupiedEntry {
                        tables,
                        subtable,
                        entry_ptr: &mut *new_entry.get_mut(),
                        kind: OccupiedEntryKind::SmallTable(new_entry, false),
                    }
                }
                VacantEntryKind::LargeTable(vacant_entry) => {
                    let mut new_entry = vacant_entry.insert(value);
                    (*tables).entries += 1;
                    OccupiedEntry {
                        tables,
                        subtable,
                        entry_ptr: &mut *new_entry.get_mut(),
                        kind: OccupiedEntryKind::LargeTable(MaybeUninit::new(new_entry), false),
                    }
                }
            }
        }
    }

    /// Converts the `VacantEntry` into a mutable reference to the underlying `TableSeq`.
    pub fn into_tables(self) -> &'a mut TableSeq<T> {
        // SAFETY: self.tables is valid by construction
        unsafe { &mut *self.tables }
    }
    /// Returns the subtable index of the `VacantEntry`.
    pub fn subtable(&self) -> usize {
        self.subtable
    }
}

impl<'a, T> OccupiedEntry<'a, T> {
    /// Gets a reference to the value of the entry.
    pub fn get(&self) -> &T {
        // SAFETY: entry_ptr is valid by construction
        unsafe { &*self.entry_ptr }
    }
    /// Gets a mutable reference to the value of the entry.
    pub fn get_mut(&mut self) -> &mut T {
        // SAFETY: entry_ptr is valid by construction
        unsafe { &mut *self.entry_ptr }
    }
    /// Converts the `OccupiedEntry` into a mutable reference to the value of the entry.
    pub fn into_mut(self) -> &'a mut T {
        // SAFETY: entry_ptr is valid by construction
        unsafe { &mut *self.entry_ptr }
    }
    /// Converts the `OccupiedEntry` into a mutable reference to the underlying `TableSeq`.
    pub fn into_tables(self) -> &'a mut TableSeq<T> {
        // SAFETY: self.tables is valid by construction
        unsafe { &mut *self.tables }
    }
    /// Returns the subtable index of the `OccupiedEntry`.
    pub fn subtable(&self) -> usize {
        self.subtable
    }
    /// Removes the entry from the subtable, returning the value of the entry and a `VacantEntry` referring to the same slot.
    pub fn remove(self) -> (T, VacantEntry<'a, T>) {
        // SAFETY: all relevant preconditions hold by construction
        // TODO reduce unsafe scope and go into more detail
        unsafe {
            let OccupiedEntry {
                tables,
                subtable,
                entry_ptr,
                kind,
            } = self;
            let chunk_slot = (subtable & CHUNK_MASK) as u32;
            let chunk_index = subtable >> CHUNK_SHIFT;
            let allocator_index = subtable >> ALLOCATOR_SHIFT;

            let chunk = (*tables).chunks.get_unchecked_mut(chunk_index);
            let chunk_alloc = (*tables).allocators.get_unchecked_mut(allocator_index);

            match kind {
                OccupiedEntryKind::SingletonTable => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot);

                    (*tables).entries -= 1;
                    let value = entry_ptr.read();

                    node.close_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_empty(chunk_slot);
                    let kind = if chunk.meta.is_empty() {
                        chunk_alloc.dealloc(chunk.node);
                        VacantEntryKind::EmptyChunk
                    } else {
                        VacantEntryKind::EmptyTable
                    };
                    (
                        value,
                        VacantEntry {
                            tables,
                            subtable,
                            kind,
                        },
                    )
                }
                OccupiedEntryKind::PairTable(index) => {
                    let mut node = chunk.node(chunk_alloc);
                    let entry_offset = chunk.meta.entry_offset(chunk_slot) + index;

                    (*tables).entries -= 1;
                    let value = entry_ptr.read();

                    node.close_entry_gap_resize(entry_offset, chunk, chunk_alloc);

                    chunk.meta.make_single(chunk_slot);
                    (
                        value,
                        VacantEntry {
                            tables,
                            subtable,
                            kind: VacantEntryKind::SingletonTable,
                        },
                    )
                }
                OccupiedEntryKind::SmallTable(entry, will_delete) => {
                    let mut node = chunk.node(chunk_alloc);
                    let table_alloc = &mut (*tables).allocators[allocator_index ^ 1];
                    (*tables).entries -= 1;
                    let (removed, entry) = entry.remove(table_alloc);

                    // TODO earlier shrinking
                    let kind = if will_delete {
                        let table = entry.into_table();
                        let table_offset = chunk.meta.table_offset(chunk_slot);
                        table.drop_and_dealloc(table_alloc);
                        let chunk_alloc = (*tables).allocators.get_unchecked_mut(allocator_index);
                        node.close_table_gap_resize(table_offset, chunk, chunk_alloc);
                        chunk.meta.make_empty(chunk_slot);
                        if chunk.meta.is_empty() {
                            chunk_alloc.dealloc(chunk.node);
                            VacantEntryKind::EmptyChunk
                        } else {
                            VacantEntryKind::EmptyTable
                        }
                    } else {
                        VacantEntryKind::SmallTable(entry)
                    };
                    (
                        removed,
                        VacantEntry {
                            tables,
                            subtable,
                            kind,
                        },
                    )
                }
                OccupiedEntryKind::LargeTable(entry, will_delete) => {
                    (*tables).entries -= 1;
                    let (removed, entry) = entry.assume_init().remove();

                    // TODO external -> internal shrinking
                    let kind = if will_delete {
                        let _ = entry; // we're deleting the table, thus entry becomes unusable
                        let mut node = chunk.node(chunk_alloc);
                        let table_offset = chunk.meta.table_offset(chunk_slot);
                        let table_ptr = node.table_ptr(table_offset);
                        table_ptr.drop_in_place();
                        node.close_table_gap_resize(table_offset, chunk, chunk_alloc);
                        chunk.meta.make_empty(chunk_slot);
                        if chunk.meta.is_empty() {
                            chunk_alloc.dealloc(chunk.node);
                            VacantEntryKind::EmptyChunk
                        } else {
                            VacantEntryKind::EmptyTable
                        }
                    } else {
                        VacantEntryKind::LargeTable(entry)
                    };
                    (
                        removed,
                        VacantEntry {
                            tables,
                            subtable,
                            kind,
                        },
                    )
                }
            }
        }
    }
}

impl<'a, T> Entry<'a, T> {
    /// Converts the `Entry` into a mutable reference to the underlying `TableSeq`.
    pub fn into_tables(self) -> &'a mut TableSeq<T> {
        match self {
            Entry::Vacant(entry) => entry.into_tables(),
            Entry::Occupied(entry) => entry.into_tables(),
        }
    }
    /// Returns the subtable index of the `Entry`.
    pub fn subtable(self) -> usize {
        match self {
            Entry::Vacant(entry) => entry.subtable(),
            Entry::Occupied(entry) => entry.subtable(),
        }
    }
    /// Inserts an entry into the subtable at the hash value corresponding to the `Entry`, overwriting any existing value.
    ///
    /// If `value` does not hash to that hash value, the table is left in an indeterminate, but memory-safe state.
    pub fn insert(self, value: T) -> OccupiedEntry<'a, T> {
        match self {
            Entry::Vacant(entry) => entry.insert(value),
            Entry::Occupied(mut entry) => {
                *entry.get_mut() = value;
                entry
            }
        }
    }
    /// Ensures a value is in the entry by inserting the default if empty, and returns an `OccupiedEntry`.
    pub fn or_insert(self, default: T) -> OccupiedEntry<'a, T> {
        match self {
            Entry::Vacant(entry) => entry.insert(default),
            Entry::Occupied(entry) => entry,
        }
    }
    /// Ensures a value is in the entry by inserting the result of the default function if empty, and returns an `OccupiedEntry`.
    pub fn or_insert_with(self, default: impl FnOnce() -> T) -> OccupiedEntry<'a, T> {
        match self {
            Entry::Vacant(entry) => entry.insert(default()),
            Entry::Occupied(entry) => entry,
        }
    }
    /// Provides in-place mutable access to an occupied entry before any potential inserts into the `TableSeq`.
    pub fn and_modify(self, f: impl FnOnce(&mut T)) -> Self {
        match self {
            Entry::Vacant(entry) => Entry::Vacant(entry),
            Entry::Occupied(mut entry) => {
                f(entry.get_mut());
                Entry::Occupied(entry)
            }
        }
    }
}
