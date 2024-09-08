use std::{
    mem::{replace, MaybeUninit},
    num::NonZeroU16,
};

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

    // SAFETY: the pointer to haystack is valid and has sufficient size and alignment for
    // loadu_si128
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
    pub fn new(
        pair: [T; 2],
        third: T,
        third_hash: u64,
        hasher: impl Fn(&T) -> u64,
        allocator: &mut NodeAllocator<T>,
    ) -> (*mut T, Self) {
        let node = allocator.alloc(SizeClass::at_least_3());

        // SAFETY: just allocated, thus valid
        let node_ptr = unsafe { allocator.ptr(node) };

        let mut hashes = [0; SMALL_SUBTABLE_CAPACITY];
        for i in 0..2 {
            hashes[i] = byte_hash_from_hash(hasher(&pair[i]));
        }
        hashes[2] = byte_hash_from_hash(third_hash);

        // SAFETY: allocated node has size at least 3 so we can write the first two entries
        unsafe { node_ptr.cast::<[T; 2]>().write(pair) };

        // SAFETY: and also the third entry
        unsafe {
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
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn insert(
        &mut self,
        hash: u64,
        value: T,
        mut eq: impl FnMut(&T, &T) -> bool,
        allocator: &mut NodeAllocator<T>,
    ) -> Result<(*mut T, Option<T>), T> {
        let byte_hash = byte_hash_from_hash(hash);
        let mut matches = find_byte_among_16(byte_hash, &self.hashes);

        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { allocator.ptr(self.node) };

        while let Some(found_match) = NonZeroU16::new(matches) {
            matches &= matches - 1;
            let match_index = found_match.trailing_zeros() as usize;
            if match_index >= self.len as usize {
                break;
            }

            // SAFETY: we just checked that the match_index is still in bounds
            let entry_ptr = unsafe { node_ptr.add(match_index) };
            // SAFETY: so we can also safely dereference it
            if eq(unsafe { &*entry_ptr }, &value) {
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
            // SAFETY: we have still capacity left for an additional slot at target_offset
            let entry_ptr = unsafe { node_ptr.add(target_offset) };
            // SAFETY: and can thus safely write to it
            unsafe { entry_ptr.write(value) };
            Ok((entry_ptr, None))
        } else {
            // we only grow by one at a time so no need to loop
            let required_size_class = size_class.next();

            let new_node = allocator.alloc(required_size_class);

            // SAFETY: just allocated above, so valid
            let new_node_ptr = unsafe { allocator.ptr(new_node) };
            // SAFETY: valid by our own requirements
            let node_ptr = unsafe { allocator.ptr(self.node) };

            // SAFETY: the new node has a larger size class and is a new allocation so it's valid
            // target and the copy is in bounds
            unsafe { new_node_ptr.copy_from_nonoverlapping(node_ptr, target_offset) };

            // SAFETY: valid up to this point by our own requirements
            unsafe { allocator.dealloc(self.node) };
            self.node = new_node;

            // SAFETY: in bounds since this was one past the end for the previous node and the new
            // node is of a larger size class.
            let entry_ptr = unsafe { new_node_ptr.add(target_offset) };
            // SAFETY: so we can also safely write to it
            unsafe { entry_ptr.write(value) };
            Ok((entry_ptr, None))
        }
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn insert_unique(
        &mut self,
        value: T,
        hash: u64,
        allocator: &mut NodeAllocator<T>,
    ) -> Result<*mut T, T> {
        let byte_hash = byte_hash_from_hash(hash);

        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { allocator.ptr(self.node) };

        let target_offset = self.len as usize;
        if target_offset == SMALL_SUBTABLE_CAPACITY {
            return Err(value);
        }

        self.len += 1;

        self.hashes[target_offset] = byte_hash;

        let size_class = self.node.size_class();

        if target_offset < size_class.len() {
            // SAFETY: we have still capacity left for an additional slot at target_offset
            let entry_ptr = unsafe { node_ptr.add(target_offset) };
            // SAFETY: and can thus safely write to it
            unsafe { entry_ptr.write(value) };
            Ok(entry_ptr)
        } else {
            // we only grow by one at a time so no need to loop
            let required_size_class = size_class.next();

            let new_node = allocator.alloc(required_size_class);

            // SAFETY: just allocated above, so valid
            let new_node_ptr = unsafe { allocator.ptr(new_node) };
            // SAFETY: valid by our own requirements
            let node_ptr = unsafe { allocator.ptr(self.node) };

            // SAFETY: the new node has a larger size class and is a new allocation so it's valid
            // target and the copy is in bounds
            unsafe { new_node_ptr.copy_from_nonoverlapping(node_ptr, target_offset) };

            // SAFETY: valid up to this point by our own requirements
            unsafe { allocator.dealloc(self.node) };
            self.node = new_node;

            // SAFETY: in bounds since this was one past the end for the previous node and the new
            // node is of a larger size class.
            let entry_ptr = unsafe { new_node_ptr.add(target_offset) };
            // SAFETY: so we can also safely write to it
            unsafe { entry_ptr.write(value) };
            Ok(entry_ptr)
        }
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn remove(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        allocator: &mut NodeAllocator<T>,
    ) -> Option<T> {
        let byte_hash = byte_hash_from_hash(hash);
        let mut matches = find_byte_among_16(byte_hash, &self.hashes);

        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { allocator.ptr(self.node) };

        let len = self.len as usize;

        while let Some(found_match) = NonZeroU16::new(matches) {
            matches &= matches - 1;
            let match_index = found_match.trailing_zeros() as usize;
            if match_index >= len {
                break;
            }

            // SAFETY: we just checked that the match_index is still in bounds
            let entry_ptr = unsafe { node_ptr.add(match_index) };
            // SAFETY: so we can also safely dereference it
            if eq(unsafe { &*entry_ptr }) {
                // SAFETY: and safely read it taking ownership
                let value = unsafe { entry_ptr.read() };

                // SAFETY: since we know we're non-emtpy this will be in bounds
                let last_ptr = unsafe { node_ptr.add(len - 1) };
                // SAFETY: if the item we just read taking ownership from was the last item, we're
                // moving the now uninitialized item in place, otherwise the source is initialized
                // and the target is uninitialized, with both being in bounds
                unsafe {
                    last_ptr
                        .cast::<MaybeUninit<T>>()
                        .copy_to(entry_ptr.cast::<MaybeUninit<T>>(), 1)
                };

                self.hashes[match_index] = self.hashes[len - 1];

                self.len -= 1;

                return Some(value);
            }
        }

        None
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn find(
        &self,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        allocator: &NodeAllocator<T>,
    ) -> Option<&T> {
        let byte_hash = byte_hash_from_hash(hash);
        let mut matches = find_byte_among_16(byte_hash, &self.hashes);

        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { allocator.ptr(self.node) };

        let len = self.len as usize;

        while let Some(found_match) = NonZeroU16::new(matches) {
            matches &= matches - 1;
            let match_index = found_match.trailing_zeros() as usize;
            if match_index >= len {
                break;
            }

            // SAFETY: we just checked that the match_index is still in bounds
            let entry_ptr = unsafe { node_ptr.add(match_index) };
            // SAFETY: so we can also safely dereference it
            let entry_ref = unsafe { &*entry_ptr };
            if eq(entry_ref) {
                return Some(entry_ref);
            }
        }

        None
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn find_mut(
        &mut self,
        hash: u64,
        mut eq: impl FnMut(&T) -> bool,
        allocator: &mut NodeAllocator<T>,
    ) -> Option<&mut T> {
        let byte_hash = byte_hash_from_hash(hash);
        let mut matches = find_byte_among_16(byte_hash, &self.hashes);

        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { allocator.ptr(self.node) };

        let len = self.len as usize;

        while let Some(found_match) = NonZeroU16::new(matches) {
            matches &= matches - 1;
            let match_index = found_match.trailing_zeros() as usize;
            if match_index >= len {
                break;
            }

            // SAFETY: we just checked that the match_index is still in bounds
            let entry_ptr = unsafe { node_ptr.add(match_index) };
            // SAFETY: so we can also safely dereference it
            let entry_ref = unsafe { &mut *entry_ptr };
            if eq(entry_ref) {
                return Some(entry_ref);
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

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn entries<'a>(&self, alloc: &'a NodeAllocator<T>) -> &'a [T] {
        let len = self.len as usize;
        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { alloc.ptr(self.node) };

        // SAFETY: and also require the first `len` item slots to be initialized
        unsafe { std::slice::from_raw_parts(node_ptr, len) }
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    pub unsafe fn entries_mut<'a>(&self, alloc: &'a mut NodeAllocator<T>) -> &'a mut [T] {
        let len = self.len as usize;
        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { alloc.ptr(self.node) };

        // SAFETY: and also require the first `len` item slots to be initialized
        unsafe { std::slice::from_raw_parts_mut(node_ptr, len) }
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    ///
    /// This call invalidates the `SmallSubtable`.
    pub unsafe fn drain_and_dealloc_with(
        &mut self,
        mut f: impl FnMut(T, u8),
        alloc: &mut NodeAllocator<T>,
    ) {
        let len = replace(&mut self.len, 0) as usize;
        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { alloc.ptr(self.node) };
        for i in 0..len {
            // SAFETY: and also require the first `len` item slots to be initialized, so we may read
            // and consume them
            f(unsafe { node_ptr.add(i).read() }, self.hashes[i]);
        }

        // SAFETY: the node is still alive in the allocator up to this point
        unsafe {
            alloc.dealloc(self.node);
        }
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed and that the nodes owned by this subtable are not modified except by calling
    /// `SmallSubtable` methods.
    ///
    /// This call invalidates the `SmallSubtable`.
    pub unsafe fn drop_and_dealloc(&mut self, alloc: &mut NodeAllocator<T>) {
        let len = replace(&mut self.len, 0) as usize;
        // SAFETY: we require our node to be alive in the given allocator
        let node_ptr = unsafe { alloc.ptr(self.node) };
        // SAFETY: and also require the first `len` item slots to be initialized
        unsafe { std::ptr::slice_from_raw_parts_mut(node_ptr, len).drop_in_place() };
        // SAFETY: the node is still alive in the allocator up to this point
        unsafe { alloc.dealloc(self.node) };
    }

    /// # Safety
    /// Callers need to ensure that the `SmallSubtable` is valid, that the correct allocator is
    /// passed as `old_alloc` and that the nodes owned by this subtable are not modified except by
    /// calling `SmallSubtable` methods.
    ///
    /// After this call `new_alloc` is considered the correct allocator for this `SmallSubtable`.
    pub unsafe fn move_node(
        &mut self,
        old_alloc: &mut NodeAllocator<T>,
        new_alloc: &mut NodeAllocator<T>,
    ) {
        let size_class = self.node.size_class();
        // SAFETY: we require our node to be alive in the given allocator
        let old_node_ptr = unsafe { old_alloc.ptr(self.node) };

        let new_node_ref = new_alloc.alloc(size_class);
        // SAFETY: the new node is just allocated and thus valid
        let new_node_ptr = unsafe { new_alloc.ptr(new_node_ref) };

        // SAFETY: both nodes have the same size class so we can copy as many bytes as nodes of that
        // size class contain
        unsafe {
            old_node_ptr
                .cast::<MaybeUninit<u8>>()
                .copy_to(new_node_ptr.cast::<MaybeUninit<u8>>(), size_class.size())
        };

        self.node = new_node_ref;
    }
}
