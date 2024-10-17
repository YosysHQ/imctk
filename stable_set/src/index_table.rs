use hashbrown::hash_table::HashTable;

/// Trait for a small index type. You can assume `T: SmallIndex` means `T = u32` most of the time.
///
/// The only exception is for testing this crate, where we use `T = u8`.
///
/// This is basically `TryInto<usize> + TryFrom<usize>` with some convenience methods
pub trait SmallIndex: Copy + std::fmt::Debug + TryInto<usize> + TryFrom<usize> {
    fn into_usize(self) -> usize {
        // the unreachable! should be straightforwardly impossible bc the conversion is infallible
        // it would be nice if we could just rely on Into<usize> existing
        // but std doesn't define it for u32 because you might be on a 16-bit platform...
        self.try_into().unwrap_or_else(|_| unreachable!())
    }
    fn from_usize_unwrap(index: usize) -> Self {
        // unfortunately .try_into().unwrap() doesn't work in the generic case
        // because TryFrom::Error might not be Debug
        index
            .try_into()
            .unwrap_or_else(|_| panic!("shouldn't happen: small index overflow in IndexTable"))
    }
}

impl SmallIndex for u8 {}
impl SmallIndex for u16 {}
impl SmallIndex for u32 {}
impl SmallIndex for usize {}

// We could just use u32 directly for the small index,
// but this is slightly less error-prone and allows us to use a smaller type for testing.
#[derive(Debug, Clone)]
pub enum IndexTable<S> {
    Small(HashTable<S>),
    Large(HashTable<usize>),
}

#[derive(Debug)]
pub enum OccupiedEntry<'a, S> {
    Small(hashbrown::hash_table::OccupiedEntry<'a, S>),
    Large(hashbrown::hash_table::OccupiedEntry<'a, usize>),
}

#[derive(Debug)]
pub enum VacantEntry<'a, S> {
    Small(hashbrown::hash_table::VacantEntry<'a, S>),
    Large(hashbrown::hash_table::VacantEntry<'a, usize>),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum AbsentEntry<'a, S> {
    Small(hashbrown::hash_table::AbsentEntry<'a, S>),
    Large(hashbrown::hash_table::AbsentEntry<'a, usize>),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Entry<'a, S> {
    Occupied(OccupiedEntry<'a, S>),
    Vacant(VacantEntry<'a, S>),
}

impl<S> Default for IndexTable<S> {
    fn default() -> Self {
        IndexTable::Small(HashTable::new())
    }
}

impl<S: SmallIndex> IndexTable<S> {
    pub fn with_capacity(capacity: usize) -> Self {
        if S::try_from(capacity).is_ok() {
            IndexTable::Small(HashTable::with_capacity(capacity))
        } else {
            IndexTable::Large(HashTable::with_capacity(capacity))
        }
    }
    #[inline(always)]
    pub fn entry(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(usize) -> bool,
        hasher: impl Fn(usize) -> u64,
    ) -> Entry<'_, S> {
        match self {
            IndexTable::Small(table) => match table.entry(
                hash,
                |&index| eq(index.into_usize()),
                |&index| hasher(index.into_usize()),
            ) {
                hashbrown::hash_table::Entry::Occupied(entry) => {
                    Entry::Occupied(OccupiedEntry::Small(entry))
                }
                hashbrown::hash_table::Entry::Vacant(entry) => {
                    Entry::Vacant(VacantEntry::Small(entry))
                }
            },
            IndexTable::Large(table) => {
                match table.entry(hash, |&index| eq(index), |&index| hasher(index)) {
                    hashbrown::hash_table::Entry::Occupied(entry) => {
                        Entry::Occupied(OccupiedEntry::Large(entry))
                    }
                    hashbrown::hash_table::Entry::Vacant(entry) => {
                        Entry::Vacant(VacantEntry::Large(entry))
                    }
                }
            }
        }
    }
    #[inline(always)]
    pub fn find_entry(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(usize) -> bool,
    ) -> Result<OccupiedEntry<'_, S>, AbsentEntry<'_, S>> {
        match self {
            IndexTable::Small(table) => {
                match table.find_entry(hash, |&index| eq(index.into_usize())) {
                    Ok(entry) => Ok(OccupiedEntry::Small(entry)),
                    Err(entry) => Err(AbsentEntry::Small(entry)),
                }
            }
            IndexTable::Large(table) => match table.find_entry(hash, |&index| eq(index)) {
                Ok(entry) => Ok(OccupiedEntry::Large(entry)),
                Err(entry) => Err(AbsentEntry::Large(entry)),
            },
        }
    }
    #[inline(always)]
    pub fn find(&self, hash: u64, mut eq: impl FnMut(usize) -> bool) -> Option<usize> {
        match self {
            IndexTable::Small(table) => table
                .find(hash, |&index| eq(index.into_usize()))
                .map(|&index| index.into_usize()),
            IndexTable::Large(table) => table.find(hash, |&index| eq(index)).copied(),
        }
    }
    #[inline(always)]
    #[must_use]
    pub fn replace(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(usize) -> bool,
        value: usize,
    ) -> Option<usize> {
        match self {
            IndexTable::Small(table) => {
                table
                    .find_mut(hash, |&index| eq(index.into_usize()))
                    .map(|index_ref| {
                        std::mem::replace(index_ref, S::from_usize_unwrap(value)).into_usize()
                    })
            }
            IndexTable::Large(table) => table
                .find_mut(hash, |&index| eq(index))
                .map(|index_ref| std::mem::replace(index_ref, value)),
        }
    }
    #[inline(always)]
    pub fn is_small(&self) -> bool {
        match self {
            IndexTable::Small(_) => true,
            IndexTable::Large(_) => false,
        }
    }
    #[inline(always)]
    pub fn grow_for(&mut self, index: usize, hasher: impl Fn(usize) -> u64) {
        if S::try_from(index).is_err() && self.is_small() {
            self.grow_cold(hasher)
        }
    }
    #[inline(never)]
    #[cold]
    fn grow_cold(&mut self, hasher: impl Fn(usize) -> u64) {
        let IndexTable::Small(old_table) =
            std::mem::replace(self, IndexTable::Large(HashTable::new()))
        else {
            unreachable!()
        };
        let IndexTable::Large(new_table) = self else {
            unreachable!()
        };
        new_table.reserve(old_table.len(), |&j| hasher(j));
        for i in old_table {
            new_table.insert_unique(hasher(i.into_usize()), i.into_usize(), |&j| hasher(j));
        }
    }
    #[inline(always)]
    pub fn len(&self) -> usize {
        match self {
            IndexTable::Small(table) => table.len(),
            IndexTable::Large(table) => table.len(),
        }
    }
    #[inline(always)]
    pub fn clear(&mut self) {
        match self {
            IndexTable::Small(table) => table.clear(),
            IndexTable::Large(table) => table.clear(),
        }
    }
    #[inline(always)]
    pub fn retain(&mut self, mut f: impl FnMut(usize) -> Option<usize>) {
        match self {
            IndexTable::Small(table) => table.retain(|index| match f((*index).into_usize()) {
                Some(new_index) => {
                    *index = S::from_usize_unwrap(new_index);
                    true
                }
                None => false,
            }),
            IndexTable::Large(table) => table.retain(|index| match f(*index) {
                Some(new_index) => {
                    *index = new_index;
                    true
                }
                None => false,
            }),
        }
    }
    #[inline(always)]
    pub fn reserve(&mut self, additional: usize, hasher: impl Fn(usize) -> u64) {
        self.grow_for((self.len() + additional).saturating_sub(1), &hasher);
        match self {
            IndexTable::Small(table) => {
                table.reserve(additional, |&index| hasher(index.into_usize()))
            }
            IndexTable::Large(table) => table.reserve(additional, |&index| hasher(index)),
        }
    }
    pub fn clear_range(&mut self, start: usize, end: usize, len: usize) {
        if end <= start {
        } else if start == 0 && end >= len {
            self.clear();
        } else {
            self.retain(|index| {
                if index < start {
                    Some(index)
                } else if index < end {
                    None
                } else {
                    Some(index - (end - start))
                }
            });
        }
    }
}

impl<'a, S: SmallIndex> VacantEntry<'a, S> {
    #[inline(always)]
    pub fn insert(self, index: usize) -> OccupiedEntry<'a, S> {
        match self {
            VacantEntry::Small(entry) => {
                OccupiedEntry::Small(entry.insert(S::from_usize_unwrap(index)))
            }
            VacantEntry::Large(entry) => OccupiedEntry::Large(entry.insert(index)),
        }
    }
    // We can't implement `into_table`, so as a workaround we have to reimplement methods we need here.
    #[inline(always)]
    #[must_use]
    pub fn replace_other(
        self,
        hash: u64,
        mut eq: impl FnMut(usize) -> bool,
        value: usize,
    ) -> Option<usize> {
        match self {
            VacantEntry::Small(entry) => entry
                .into_table()
                .find_mut(hash, |&index| eq(index.into_usize()))
                .map(|index_ref| {
                    std::mem::replace(index_ref, S::from_usize_unwrap(value)).into_usize()
                }),
            VacantEntry::Large(entry) => entry
                .into_table()
                .find_mut(hash, |&index| eq(index))
                .map(|index_ref| std::mem::replace(index_ref, value)),
        }
    }
}

impl<'a, S: SmallIndex> OccupiedEntry<'a, S> {
    #[inline(always)]
    pub fn get(&self) -> usize {
        match self {
            OccupiedEntry::Small(entry) => (*entry.get()).into_usize(),
            OccupiedEntry::Large(entry) => *entry.get(),
        }
    }
    #[inline(always)]
    pub fn remove(self) -> (usize, VacantEntry<'a, S>) {
        match self {
            OccupiedEntry::Small(entry) => {
                let (index, entry) = entry.remove();
                (index.into_usize(), VacantEntry::Small(entry))
            }
            OccupiedEntry::Large(entry) => {
                let (index, entry) = entry.remove();
                (index, VacantEntry::Large(entry))
            }
        }
    }
}
