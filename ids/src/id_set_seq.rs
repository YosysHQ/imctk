//! Id indexed sequence of hash sets.
use std::{hash::BuildHasherDefault, marker::PhantomData};

use table_seq::set_seq::{SetSeq, SetSeqSet, SetSeqSetMut};
use zwohash::ZwoHasher;

use crate::Id;

/// Indexed sequence of hash sets.
///
/// This type serves as a memory and runtime efficient replacement for `IdVec<I, HashSet<T>>`.
pub struct IdSetSeq<I: Id, T, S = BuildHasherDefault<ZwoHasher>> {
    set_seq: SetSeq<T, S>,
    _phantom: PhantomData<I>,
}

impl<I: Id, T, S> std::ops::Deref for IdSetSeq<I, T, S> {
    type Target = SetSeq<T, S>;

    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.set_seq
    }
}

impl<I: Id, T, S> IdSetSeq<I, T, S> {
    /// Discards all sets in the sequence.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.set_seq.clear();
    }

    /// Resizes the sequence by appending empty sets or discarding trailing sets.
    #[inline(always)]
    pub fn resize(&mut self, len: usize) {
        self.set_seq.resize(len);
    }

    /// Ensures that the sequence contains a set for the given id by appending emtpy sets if the
    /// sequence was too short.
    ///
    /// Provides mutable access to the set for the given id.
    #[inline(always)]
    pub fn grow_for(&mut self, id: I) -> SetSeqSetMut<T, S> {
        self.set_seq.grow_for(id.id_index())
    }

    /// Provides shared access to the set for a given id, panics if the id is out-of-bounds.
    ///
    /// This is used instead of [`std::ops::Index`], as it returns a value of the custom
    /// reference-like [`SetSeqSet`] type.
    ///
    /// Panics if `id.id_index() >= self.len()`.
    #[inline(always)]
    pub fn at(&self, id: I) -> SetSeqSet<T, S> {
        self.set_seq.at(id.id_index())
    }

    /// Provides mutable access to the set for a given id, panics if the id is out-of-bounds.
    ///
    /// This is used instead of [`std::ops::IndexMut`], as it returns a value of the custom
    /// reference-like [`SetSeqSetMut`] type.
    ///
    /// Panics if `id.id_index() >= self.len()`.
    #[inline(always)]
    pub fn at_mut(&mut self, id: I) -> SetSeqSetMut<T, S> {
        self.set_seq.at_mut(id.id_index())
    }

    /// Provides shared access to the set for a given id.
    ///
    /// This returns `None` if `id.id_index() >= self.len()`.
    #[inline(always)]
    pub fn get(&self, id: I) -> Option<SetSeqSet<T, S>> {
        self.set_seq.get(id.id_index())
    }

    /// Provides mutable access to the set for a given id.
    ///
    /// This returns `None` if `id.id_index() >= self.len()`.
    #[inline(always)]
    pub fn get_mut(&mut self, id: I) -> Option<SetSeqSetMut<T, S>> {
        self.set_seq.get_mut(id.id_index())
    }
}

impl<I: Id, T, S: Default> Default for IdSetSeq<I, T, S> {
    fn default() -> Self {
        Self {
            set_seq: Default::default(),
            _phantom: PhantomData,
        }
    }
}
