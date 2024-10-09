use hashbrown::hash_table::HashTable;

#[derive(Debug, Clone)]
pub enum IndexTable {
    Small(HashTable<u32>),
    Large(HashTable<usize>),
}

#[derive(Debug)]
pub enum OccupiedEntry<'a> {
    Small(hashbrown::hash_table::OccupiedEntry<'a, u32>),
    Large(hashbrown::hash_table::OccupiedEntry<'a, usize>),
}

#[derive(Debug)]
pub enum VacantEntry<'a> {
    Small(hashbrown::hash_table::VacantEntry<'a, u32>),
    Large(hashbrown::hash_table::VacantEntry<'a, usize>),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum AbsentEntry<'a> {
    Small(hashbrown::hash_table::AbsentEntry<'a, u32>),
    Large(hashbrown::hash_table::AbsentEntry<'a, usize>),
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum Entry<'a> {
    Occupied(OccupiedEntry<'a>),
    Vacant(VacantEntry<'a>),
}

impl Default for IndexTable {
    fn default() -> Self {
        IndexTable::Small(HashTable::new())
    }
}

impl IndexTable {
    pub fn with_capacity(capacity: usize) -> Self {
        if Self::try_as_small(capacity).is_ok() {
            IndexTable::Small(HashTable::with_capacity(capacity))
        } else {
            IndexTable::Large(HashTable::with_capacity(capacity))
        }
    }
    #[inline(always)]
    fn as_small(index: usize) -> u32 {
        Self::try_as_small(index).unwrap()
    }
    #[inline(always)]
    fn try_as_small(index: usize) -> Result<u32, std::num::TryFromIntError> {
        u32::try_from(index)
    }
    #[inline(always)]
    pub fn entry(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(usize) -> bool,
        hasher: impl Fn(usize) -> u64,
    ) -> Entry<'_> {
        match self {
            IndexTable::Small(table) => match table.entry(
                hash,
                |&index| eq(index as usize),
                |&index| hasher(index as usize),
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
    ) -> Result<OccupiedEntry<'_>, AbsentEntry<'_>> {
        match self {
            IndexTable::Small(table) => match table.find_entry(hash, |&index| eq(index as usize)) {
                Ok(entry) => Ok(OccupiedEntry::Small(entry)),
                Err(entry) => Err(AbsentEntry::Small(entry)),
            },
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
                .find(hash, |&index| eq(index as usize))
                .map(|&index| index as usize),
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
            IndexTable::Small(table) => table
                .find_mut(hash, |&index| eq(index as usize))
                .map(|index_ref| std::mem::replace(index_ref, Self::as_small(value)) as usize),
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
        if Self::try_as_small(index).is_err() && self.is_small() {
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
            new_table.insert_unique(hasher(i as usize), i as usize, |&j| hasher(j));
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
            IndexTable::Small(table) => table.retain(|index| match f(*index as usize) {
                Some(new_index) => {
                    *index = Self::as_small(new_index);
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
            IndexTable::Small(table) => table.reserve(additional, |&index| hasher(index as usize)),
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

impl<'a> VacantEntry<'a> {
    #[inline(always)]
    pub fn insert(self, index: usize) -> OccupiedEntry<'a> {
        match self {
            VacantEntry::Small(entry) => {
                OccupiedEntry::Small(entry.insert(IndexTable::as_small(index)))
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
            VacantEntry::Small(entry) => entry.into_table()
                .find_mut(hash, |&index| eq(index as usize))
                .map(|index_ref| std::mem::replace(index_ref, IndexTable::as_small(value)) as usize),
            VacantEntry::Large(entry) => entry.into_table()
                .find_mut(hash, |&index| eq(index))
                .map(|index_ref| std::mem::replace(index_ref, value)),
        }
    }
}

impl<'a> OccupiedEntry<'a> {
    #[inline(always)]
    pub fn get(&self) -> usize {
        match self {
            OccupiedEntry::Small(entry) => *entry.get() as usize,
            OccupiedEntry::Large(entry) => *entry.get(),
        }
    }
    #[inline(always)]
    pub fn remove(self) -> (usize, VacantEntry<'a>) {
        match self {
            OccupiedEntry::Small(entry) => {
                let (index, entry) = entry.remove();
                (index as usize, VacantEntry::Small(entry))
            }
            OccupiedEntry::Large(entry) => {
                let (index, entry) = entry.remove();
                (index, VacantEntry::Large(entry))
            }
        }
    }
}
