use std::{
    hash::{Hash, Hasher},
    num::NonZeroU32,
    ops,
    ptr::NonNull,
};

use bstr::{BStr, BString, ByteSlice};
use zwohash::ZwoHasher;

use std::cell::Cell;

thread_local! {
    pub static DISPLAY_TABLE: Cell<Option<NonNull<IdTable>>> = const { Cell::new(None) };
}

/// assert_eq!(FOO.get(), 1);
// thread_local! {}

pub struct IdEntry {
    offset: usize,
    len: usize,
    hash: u64,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(NonZeroU32);

impl std::fmt::Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let table: Option<NonNull<IdTable>> = DISPLAY_TABLE.get();
        if let Some(table) = table {
            unsafe { std::fmt::Display::fmt(&table.as_ref()[*self], f) }
        } else {
            f.debug_tuple("Id").field(&self.0).finish()
        }
    }
}

impl Id {
    pub fn new(index: usize) -> Self {
        assert!(index < u32::MAX as usize);
        Self(NonZeroU32::new((index as u32) + 1).unwrap())
    }
    pub fn index(self) -> usize {
        (self.0.get() - 1) as usize
    }
}

#[derive(Default)]
pub struct IdTable {
    entries: Vec<IdEntry>,
    lookup: hashbrown::hash_table::HashTable<Id>,
    buf: BString,
}

impl std::fmt::Debug for IdTable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(
                self.entries
                    .iter()
                    .enumerate()
                    .map(|(i, entry)| (i, self.buf[entry.offset..][..entry.len].as_bstr())),
            )
            .finish()
    }
}

impl IdTable {
    pub fn insert(&mut self, name: &BStr) -> Id {
        let mut hasher = ZwoHasher::default();
        name.hash(&mut hasher);
        let hash = hasher.finish();

        if let Some(&id) = self.lookup.find(hash, |id| {
            let entry = &self.entries[id.index()];
            entry.hash == hash
                && entry.len == name.len()
                && &self.buf[entry.offset..][..entry.len] == name
        }) {
            return id;
        }

        let offset = self.buf.len();
        self.buf.extend_from_slice(name);
        let id = Id::new(self.entries.len());
        self.entries.push(IdEntry {
            offset,
            len: name.len(),
            hash,
        });

        self.lookup
            .insert_unique(hash, id, |id| self.entries[id.index()].hash);

        id
    }

    pub fn get(&self, index: Id) -> Option<&BStr> {
        let entry = self.entries.get(index.index())?;
        Some(self.buf[entry.offset..][..entry.len].as_bstr())
    }

    pub fn install<R>(&self, inner: impl FnOnce() -> R) -> R {
        let old = DISPLAY_TABLE.replace(Some(NonNull::from(self)));

        let result = inner();

        // TODO drop guard for DISPLAY_TABLE

        DISPLAY_TABLE.set(old);

        result
    }
}

impl ops::Index<Id> for IdTable {
    type Output = BStr;

    fn index(&self, index: Id) -> &Self::Output {
        self.get(index).unwrap()
    }
}

#[cfg(test)]
mod tests {
    use bstr::ByteSlice;

    use super::*;

    #[test]
    fn test_table() {
        let mut id_tab = IdTable::default();

        let x = id_tab.insert(b"foobar".as_bstr());
        let z = id_tab.insert(b"baz".as_bstr());
        let y = id_tab.insert(b"foobar".as_bstr());
        assert_eq!(x, y);
        assert_eq!(&id_tab[x], b"foobar".as_bstr());
        assert_eq!(&id_tab[z], b"baz".as_bstr());
    }
}
