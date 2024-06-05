use std::{mem::replace, num::NonZeroU16};

use hashbrown::HashTable;

use crate::node_allocator::AllocatorClass;

use super::{NodeAllocator, NodeRef, SizeClass};

pub const SMALL_SUBTABLE_CAPACITY: usize = 16; // Can't exceed 16 with the current implementation

pub enum Subtable<T> {
    Large(HashTable<T>),
    Small(SmallSubtable<T>),
}

pub struct SmallSubtable<T> {
    node: NodeRef<T>,
    hashes: [u8; SMALL_SUBTABLE_CAPACITY],
    len: u8,
}

fn byte_hash_from_hash(hash: u64) -> u8 {
    (hash >> (usize::BITS - 8)) as u8
}

#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
fn find_byte_among_16(needle: u8, haystack: &[u8; 16]) -> u16 {
    #[cfg(target_arch = "x86")]
    use core::arch::x86;
    #[cfg(target_arch = "x86_64")]
    use core::arch::x86_64 as x86;

    unsafe {
        let bytes = x86::_mm_loadu_si128((haystack as *const [u8; 16]).cast::<x86::__m128i>());

        let needle = x86::_mm_set1_epi8(needle as i8);

        x86::_mm_movemask_epi8(x86::_mm_cmpeq_epi8(bytes, needle)) as u16
    }
}

#[cfg(not(any(target_arch = "x86", target_arch = "x86_64")))]
fn find_byte_among_16(needle: u8, haystack: &[u8; 16]) -> u16 {
    let mut found = 0;
    for (i, &candidate) in haystack.iter().enumerate() {
        found |= ((candidate == needle) as u16) << i;
    }
    found
}

impl<T> SmallSubtable<T> {
    pub unsafe fn new(
        pair: [T; 2],
        third: T,
        third_hash: u64,
        hasher: impl Fn(&T) -> u64,
        allocator: &mut NodeAllocator<T>,
    ) -> (*mut T, Self) {
        let node = allocator.alloc(SizeClass::at_least_3());

        let node_ptr = allocator.ptr(node);

        let mut hashes = [0; SMALL_SUBTABLE_CAPACITY];
        for i in 0..2 {
            hashes[i] = byte_hash_from_hash(hasher(&pair[i]));
        }
        hashes[2] = byte_hash_from_hash(third_hash);

        node_ptr.cast::<[T; 2]>().write(pair);

        let entry_ptr = node_ptr.add(2);
        entry_ptr.write(third);

        (
            entry_ptr,
            Self {
                node,
                hashes,
                len: 3,
            },
        )
    }

    pub unsafe fn insert(
        &mut self,
        value: T,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        allocator: &mut NodeAllocator<T>,
    ) -> Result<(*mut T, Option<T>), T> {
        let byte_hash = byte_hash_from_hash(hash);
        let mut matches = find_byte_among_16(byte_hash, &self.hashes);

        let node_ptr = allocator.ptr(self.node);

        while let Some(found_match) = NonZeroU16::new(matches) {
            matches &= matches - 1;
            let match_index = found_match.trailing_zeros() as usize;
            if match_index >= self.len as usize {
                break;
            }

            let entry_ptr = node_ptr.add(match_index);
            if eq(&*entry_ptr) {
                return Ok((entry_ptr, Some(value)));
            }
        }

        let target_offset = self.len as usize;
        if target_offset == SMALL_SUBTABLE_CAPACITY {
            return Err(value);
        }

        self.len += 1;

        self.hashes[target_offset] = byte_hash;

        let size_class = self.node.size_class();

        if target_offset < size_class.len() {
            let entry_ptr = node_ptr.add(target_offset);
            entry_ptr.write(value);
            Ok((entry_ptr, None))
        } else {
            // we only grow by one at a time so no need to loop
            let required_size_class = size_class.next();

            let new_node = allocator.alloc(required_size_class);

            let new_node_ptr = allocator.ptr(new_node);
            let node_ptr = allocator.ptr(self.node);

            new_node_ptr.copy_from_nonoverlapping(node_ptr, target_offset);

            allocator.dealloc(self.node);
            self.node = new_node;

            let entry_ptr = new_node_ptr.add(target_offset);
            entry_ptr.write(value);
            Ok((entry_ptr, None))
        }
    }

    pub unsafe fn insert_unique(
        &mut self,
        value: T,
        hash: u64,
        allocator: &mut NodeAllocator<T>,
    ) -> Result<*mut T, T> {
        let byte_hash = byte_hash_from_hash(hash);

        let node_ptr = allocator.ptr(self.node);

        let target_offset = self.len as usize;
        if target_offset == SMALL_SUBTABLE_CAPACITY {
            return Err(value);
        }

        self.len += 1;

        self.hashes[target_offset] = byte_hash;

        let size_class = self.node.size_class();

        if target_offset < size_class.len() {
            let entry_ptr = node_ptr.add(target_offset);
            entry_ptr.write(value);
            Ok(entry_ptr)
        } else {
            // we only grow by one at a time so no need to loop
            let required_size_class = size_class.next();

            let new_node = allocator.alloc(required_size_class);

            let new_node_ptr = allocator.ptr(new_node);
            let node_ptr = allocator.ptr(self.node);

            new_node_ptr.copy_from_nonoverlapping(node_ptr, target_offset);

            allocator.dealloc(self.node);
            self.node = new_node;

            let entry_ptr = new_node_ptr.add(target_offset);
            entry_ptr.write(value);
            Ok(entry_ptr)
        }
    }

    pub unsafe fn remove(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        allocator: &mut NodeAllocator<T>,
    ) -> Option<T> {
        let byte_hash = byte_hash_from_hash(hash);
        let mut matches = find_byte_among_16(byte_hash, &self.hashes);

        let node_ptr = allocator.ptr(self.node);

        let len = self.len as usize;

        while let Some(found_match) = NonZeroU16::new(matches) {
            matches &= matches - 1;
            let match_index = found_match.trailing_zeros() as usize;
            if match_index >= self.len as usize {
                break;
            }

            let entry_ptr = node_ptr.add(match_index);
            if eq(&*entry_ptr) {
                let value = entry_ptr.read();

                let last_ptr = node_ptr.add(len - 1);
                last_ptr.copy_to(entry_ptr, 1);

                self.hashes[match_index] = self.hashes[len - 1];

                self.len -= 1;

                return Some(value);
            }
        }

        None
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub unsafe fn entries<'a>(&self, alloc: &'a NodeAllocator<T>) -> &'a [T] {
        let len = self.len as usize;
        let node_ptr = alloc.ptr(self.node);

        std::slice::from_raw_parts(node_ptr, len)
    }

    pub unsafe fn entries_mut<'a>(&self, alloc: &'a mut NodeAllocator<T>) -> &'a mut [T] {
        let len = self.len as usize;
        let node_ptr = alloc.ptr(self.node);

        std::slice::from_raw_parts_mut(node_ptr, len)
    }

    pub unsafe fn drain_and_dealloc_with(
        &mut self,
        mut f: impl FnMut(T, u8),
        alloc: &mut NodeAllocator<T>,
    ) {
        let len = replace(&mut self.len, 0) as usize;
        let node_ptr = alloc.ptr(self.node);
        for i in 0..len {
            f(node_ptr.add(i).read(), self.hashes[i]);
        }

        alloc.dealloc(self.node);
    }

    pub unsafe fn drop_and_dealloc(&mut self, alloc: &mut NodeAllocator<T>) {
        let len = replace(&mut self.len, 0) as usize;
        let node_ptr = alloc.ptr(self.node);
        std::ptr::slice_from_raw_parts_mut(node_ptr, len).drop_in_place();
        alloc.dealloc(self.node);
    }

    pub unsafe fn move_node(
        &mut self,
        old_alloc: &mut NodeAllocator<T>,
        new_alloc: &mut NodeAllocator<T>,
    ) {
        let size_class = self.node.size_class();
        let old_node_ptr = old_alloc.ptr(self.node);

        let new_node_ref = new_alloc.alloc(size_class);
        let new_node_ptr = new_alloc.ptr(new_node_ref);

        old_node_ptr
            .cast::<u8>()
            .copy_to(new_node_ptr.cast::<u8>(), size_class.size());

        self.node = new_node_ref;
    }
}
