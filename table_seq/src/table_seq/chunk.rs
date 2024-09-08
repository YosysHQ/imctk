use crate::table_seq::node::NodeRef;

use super::node::{Node, NodeAllocator};

type EntryMask = u16;

pub const CHUNK_SIZE: usize = EntryMask::BITS as usize;
pub const CHUNK_SHIFT: u32 = {
    assert!(CHUNK_SIZE.is_power_of_two());
    CHUNK_SIZE.trailing_zeros()
};
pub const CHUNK_MASK: usize = CHUNK_SIZE - 1;

pub struct Chunk<T> {
    pub meta: ChunkMetadata,

    /// Only valid when the metadata indicates this chunk is non-empty
    pub node: NodeRef<T>,
}

impl<T> Default for Chunk<T> {
    #[inline(always)]
    fn default() -> Self {
        Self {
            meta: Default::default(),
            node: Default::default(),
        }
    }
}

impl<T> Chunk<T> {
    /// # Safety
    /// Callers need to ensure that the metadata for this chunk is correct and up to date and that
    /// the passed allocator is the correct one for this chunk's node (if the chunk is non-empty,
    /// otherwise it isn't considered to have a node).
    #[inline(always)]
    pub unsafe fn node(&self, allocator: &NodeAllocator<T>) -> Node<T> {
        debug_assert!(!self.meta.is_empty());
        // SAFETY: safe since we forward the node validity requirements and read the correct entry
        // and table counts from the metadata (which we require to be up to date)
        unsafe { allocator.node(self.node, self.meta.entry_count(), self.meta.table_count()) }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum EntryType {
    Empty,
    Single,
    Pair,
    Table,
}

#[derive(Default)]
pub struct ChunkMetadata {
    mask_entry: EntryMask,
    mask_large: EntryMask,
}

impl ChunkMetadata {
    #[inline(always)]
    pub fn entry_type(&self, index: u32) -> EntryType {
        match (
            self.mask_entry & (1 << index) != 0,
            self.mask_large & (1 << index) != 0,
        ) {
            (false, false) => EntryType::Empty,
            (true, false) => EntryType::Single,
            (true, true) => EntryType::Pair,
            (false, true) => EntryType::Table,
        }
    }

    #[inline(always)]
    pub fn make_single(&mut self, index: u32) {
        self.mask_entry |= 1 << index;
        self.mask_large &= !(1 << index);
    }

    #[inline(always)]
    pub fn make_pair(&mut self, index: u32) {
        self.mask_entry |= 1 << index;
        self.mask_large |= 1 << index;
    }

    #[inline(always)]
    pub fn make_table(&mut self, index: u32) {
        self.mask_entry &= !(1 << index);
        self.mask_large |= 1 << index;
    }

    #[inline(always)]
    pub fn make_empty(&mut self, index: u32) {
        self.mask_entry &= !(1 << index);
        self.mask_large &= !(1 << index);
    }

    #[inline(always)]
    pub fn table_offset(&self, index: u32) -> usize {
        let tables = self.mask_large & !self.mask_entry;

        let mask_before = !(!0 << index);

        (tables & mask_before).count_ones() as usize
    }

    #[inline(always)]
    pub fn entry_offset(&self, index: u32) -> usize {
        let singles_or_pairs = self.mask_entry;
        let pairs = self.mask_entry & self.mask_large;

        let mask_before = !(!0 << index);
        (singles_or_pairs & mask_before).count_ones() as usize
            + (pairs & mask_before).count_ones() as usize
    }

    #[inline(always)]
    pub fn table_count(&self) -> usize {
        let tables = self.mask_large & !self.mask_entry;

        tables.count_ones() as usize
    }

    #[inline(always)]
    pub fn entry_count(&self) -> usize {
        let singles_or_pairs = self.mask_entry;
        let pairs = self.mask_entry & self.mask_large;

        (singles_or_pairs).count_ones() as usize + (pairs).count_ones() as usize
    }

    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.mask_entry == 0 && self.mask_large == 0
    }
}
