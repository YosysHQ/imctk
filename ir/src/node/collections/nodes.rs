//! Heterogeneous collection of [`Node`] values supporting dynamic updates.
use std::{
    alloc::Layout,
    fmt::{self, Debug},
    hash::{BuildHasher, BuildHasherDefault},
    mem::ManuallyDrop,
    ptr::null_mut,
};

use hashbrown::HashTable;
use imctk_ids::Id;
use imctk_util::give_take::Take;
use indexmap::IndexSet;
use zwohash::ZwoHasher;

use crate::node::{
    generic::{DynNode, Node, NodeType},
    vtables::{DynNodeType, GenericNodeType, KnownNodeType},
    NodeId,
};

pub(crate) union ChunkSlot<T> {
    _entry: ManuallyDrop<T>,
    _next: u16,
}

#[repr(C)]
struct RawNodeChunk<V: GenericNodeType> {
    vtable: V,
    buf: *mut u8,
    cap: u16,
    len: u16,
    next: u16,
    type_index: u16,
}

impl<V: GenericNodeType> fmt::Debug for RawNodeChunk<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries((0..self.capacity()).flat_map(|i| self.get(i).map(|entry| (i, entry))))
            .finish()
    }
}

impl<V: GenericNodeType> Drop for RawNodeChunk<V> {
    fn drop(&mut self) {
        for i in 0..self.capacity() {
            self.drop(i);
        }
        self.resize_buf(0);
    }
}

impl<V: GenericNodeType> RawNodeChunk<V> {
    fn new(vtable: V, type_index: u16) -> Self {
        Self {
            vtable,
            buf: null_mut(),
            cap: 0,
            len: 0,
            next: 0,
            type_index,
        }
    }
    fn new_with_capacity(vtable: V, type_index: u16, capacity: usize) -> Self {
        let mut new = Self::new(vtable, type_index);
        new.resize_buf(capacity);
        new.initialize_new_free(0);
        new
    }

    pub fn capacity(&self) -> usize {
        self.cap as usize
    }

    pub fn len(&self) -> usize {
        self.len as usize
    }

    fn layout_for_capacity(&self, cap: usize) -> Layout {
        assert!(cap <= u16::MAX as usize);
        let align = self.vtable.slot_align();
        let size = self.vtable.slot_size();

        let data_size = size.checked_mul(cap).unwrap();
        debug_assert_eq!(data_size, self.bitmap_offset_for_capacity(cap));

        let bitmap_size = Self::bitmap_size_for_capacity(cap);

        let size = data_size + bitmap_size;

        Layout::from_size_align(size, align).unwrap()
    }

    #[inline(always)]
    fn bitmap_offset_for_capacity(&self, cap: usize) -> usize {
        self.vtable.slot_size() * cap
    }

    #[inline(always)]
    fn bitmap_size_for_capacity(cap: usize) -> usize {
        cap.div_ceil(8)
    }

    fn bitmap_ptr(&self) -> *mut [u8] {
        // SAFETY: in bounds, compare against `layout_for_capacity`
        unsafe {
            std::ptr::slice_from_raw_parts_mut(
                self.buf
                    .add(self.bitmap_offset_for_capacity(self.capacity())),
                Self::bitmap_size_for_capacity(self.capacity()),
            )
        }
    }

    fn bitmap(&self) -> &[u8] {
        // SAFETY: we own the bitmap, so we can reference it with our lifetime
        unsafe { &*self.bitmap_ptr().cast_const() }
    }

    fn bitmap_mut(&mut self) -> &mut [u8] {
        // SAFETY: we own the bitmap, so we can mutably reference it with our lifetime
        unsafe { &mut *self.bitmap_ptr() }
    }

    /// # Safety
    /// Callers have to ensure `index < self.capacity()`
    unsafe fn slot_ptr(&self, index: usize) -> *mut u8 {
        debug_assert!(index <= self.capacity());
        // SAFETY: Stays in bound given our documented safety requirements, see also
        // `layout_for_capacity`
        unsafe { self.buf.add(self.vtable.slot_size() * index) }
    }

    fn resize_buf(&mut self, new_cap: usize) {
        let old_cap = self.capacity();

        if old_cap == new_cap {
            // nothing to do
        } else if old_cap == 0 {
            let new_layout = self.layout_for_capacity(new_cap);

            // SAFETY: We know `new_cap != 0` and that's sufficient to prevent a zero sized layout
            // due to the `ChunkSlot` wrapper and the bitmap.
            self.buf = unsafe { std::alloc::alloc(new_layout) };
            if self.buf.is_null() {
                std::alloc::handle_alloc_error(new_layout);
            }

            self.cap = new_cap as u16;
        } else if new_cap == 0 {
            let old_layout = self.layout_for_capacity(old_cap);

            // SAFETY: We know `old_cap != 0`, so we know that we have an allocation.
            unsafe { std::alloc::dealloc(self.buf, old_layout) };

            self.buf = null_mut();
            self.cap = 0;
        } else {
            let new_layout = self.layout_for_capacity(new_cap);
            let old_layout = self.layout_for_capacity(old_cap);

            // SAFETY: We know `new_cap != 0` and `old_cap != 0` so neither layout is zero-sized.
            self.buf = unsafe { std::alloc::realloc(self.buf, old_layout, new_layout.size()) };

            if self.buf.is_null() {
                std::alloc::handle_alloc_error(new_layout);
            }

            self.cap = new_cap as u16;
        }
    }

    /// Initializes the bitmap and the free list after growing the buffer.
    ///
    /// This assumes that the first `len` slots are filled and all following slots are free.
    fn initialize_new_free(&mut self, len: usize) {
        let bitmap = self.bitmap_mut();

        let (filled, rest) = bitmap.split_at_mut(len / 8);
        filled.fill(!0);

        let free = if len % 8 != 0 {
            let (partial, free) = rest.split_first_mut().unwrap();
            *partial = !(!1 << (len - 1));
            free
        } else {
            rest
        };

        free.fill(0);

        for i in len..self.capacity() {
            // SAFETY: `i < self.capacity()` holds, so this is in bounds
            unsafe {
                self.slot_ptr(i).cast::<u16>().write((i + 1) as u16);
            }
        }
    }

    pub fn is_present(&self, index: usize) -> bool {
        if index >= self.capacity() {
            return false;
        }

        // SAFETY: With a bitmap size of `self.capacity().div_ceil(8)` the check above ensures this
        // stays in bounds.
        unsafe { *self.bitmap().get_unchecked(index / 8) & (1 << (index % 8)) != 0 }
    }

    fn take_present(&mut self, index: usize) -> bool {
        if index >= self.capacity() {
            return false;
        }

        // SAFETY: With a bitmap size of `self.capacity().div_ceil(8)` the check above ensures this
        // stays in bounds.
        let byte_ref = unsafe { self.bitmap_mut().get_unchecked_mut(index / 8) };
        let byte = *byte_ref;

        *byte_ref = byte & !(1 << (index % 8));
        byte & (1 << (index % 8)) != 0
    }

    fn grow(&mut self) {
        debug_assert_eq!(self.len, self.cap);

        let new_cap = (self.capacity() * 2).clamp(1, 1 << 15);
        assert!(new_cap > self.capacity());

        self.resize_buf(new_cap);

        self.initialize_new_free(self.len());
    }

    pub fn get(&self, index: usize) -> Option<&V::RefTarget> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe { &*self.vtable.cast_mut_ptr(self.slot_ptr(index)).cast_const() })
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut V::RefTarget> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe { &mut *self.vtable.cast_mut_ptr(self.slot_ptr(index)) })
    }

    pub fn drop(&mut self, index: usize) -> bool {
        if !self.take_present(index) {
            return false;
        }

        // SAFETY: the `take_present` check rejects out-of-bounds indices
        let ptr = unsafe { self.slot_ptr(index) };

        // SAFETY: the `take_present` check rejects empty slots and our vtable matches the node type
        // for all entries in this chunk.
        unsafe { self.vtable.drop_in_place(ptr) };

        // SAFETY: the next field of the union slot is a u16
        unsafe { (ptr as *mut u16).write(self.next) };

        self.next = index as u16;

        self.len -= 1;

        true
    }
}

impl<T: Node> KnownNodeChunk<T> {
    pub fn insert(&mut self, item: T) -> (usize, &mut T) {
        let index = self.next as usize;

        if self.next == self.cap {
            self.grow(); // Ensures that `self.next` becomes a valid index of a free slot or panics
        }

        // SAFETY: `self.next` always is either a valid index of a free slot or `self.cap` and we
        // already called `grow` to reduce the second case to the first.
        let ptr = unsafe { self.slot_ptr(index) };

        // SAFETY: this reads the `_next` field of the slot union
        self.next = unsafe { (ptr as *mut u16).read() };
        self.len += 1;

        let ptr = ptr as *mut T;
        // SAFETY: this writes the `_entry` field of the slot union
        unsafe {
            ptr.write(item);
        }

        // SAFETY: we know index is in bounds for the slots, making `index / 8` in bounds for the
        // bitmap
        unsafe {
            *self.bitmap_mut().get_unchecked_mut(index / 8) |= 1 << (index % 8);
        }

        // SAFETY: we own the buffer holding the item we just inserted so we can safely return a
        // mutable reference to it with our lifetime
        (index, unsafe { &mut *ptr })
    }

    pub fn remove(&mut self, index: usize) -> Option<T> {
        if !self.take_present(index) {
            return None;
        }

        // SAFETY: the `take_present` check rejects out-of-bounds indices
        let ptr = unsafe { self.slot_ptr(index) };

        // SAFETY: the `take_present` check rejects empty slots
        let node = unsafe { (ptr as *mut T).read() };

        // SAFETY: this writes the `_next` field of the slot union
        unsafe { (ptr as *mut u16).write(self.next) };

        self.next = index as u16;
        self.len -= 1;

        Some(node)
    }

    pub fn into_dynamic(self) -> DynNodeChunk {
        // SAFETY: Every KnownNodeChunk<T> is a valid DynNodeChunk
        unsafe { std::mem::transmute(self) }
    }

    #[allow(dead_code)] // TODO remove when this is used
    pub fn as_dynamic(&self) -> &DynNodeChunk {
        // SAFETY: Every KnownNodeChunk<T> is a valid DynNodeChunk
        unsafe { std::mem::transmute(self) }
    }

    #[allow(dead_code)] // TODO remove when this is used
    pub fn as_dynamic_mut(&mut self) -> &mut DynNodeChunk {
        // SAFETY: Every KnownNodeChunk<T> is a valid DynNodeChunk
        unsafe { std::mem::transmute(self) }
    }
}

impl DynNodeChunk {
    #[allow(dead_code)] // TODO remove when this is used
    pub fn into_known<T: Node>(self) -> Result<KnownNodeChunk<T>, Self> {
        if self.vtable.has_type::<T>() {
            // SAFETY: Every DynNodeChunk with the correct vtable is a valid KnownNodeChunk<T>
            Ok(unsafe { std::mem::transmute::<Self, KnownNodeChunk<T>>(self) })
        } else {
            Err(self)
        }
    }

    #[allow(dead_code)] // TODO remove when this is used
    pub fn as_known<T: Node>(&self) -> Option<&KnownNodeChunk<T>> {
        if self.vtable.has_type::<T>() {
            // SAFETY: Every DynNodeChunk with the correct vtable is a valid KnownNodeChunk<T>
            Some(unsafe { std::mem::transmute::<&Self, &KnownNodeChunk<T>>(self) })
        } else {
            None
        }
    }

    pub fn as_known_mut<T: Node>(&mut self) -> Option<&mut KnownNodeChunk<T>> {
        if self.vtable.has_type::<T>() {
            // SAFETY: Every DynNodeChunk with the correct vtable is a valid KnownNodeChunk<T>
            Some(unsafe { std::mem::transmute::<&mut Self, &mut KnownNodeChunk<T>>(self) })
        } else {
            None
        }
    }

    #[allow(dead_code)] // TODO remove when this is used
    /// # Safety
    /// The caller has to ensure that the node type `T` matches the dynamic node type of this chunk.
    pub unsafe fn into_known_unchecked<T: Node>(self) -> KnownNodeChunk<T> {
        debug_assert!(self.vtable.has_type::<T>());
        // SAFETY: Every DynNodeChunk with the correct vtable is a valid KnownNodeChunk<T>, which is
        // a documented safety requirement for calling this
        unsafe { std::mem::transmute(self) }
    }

    #[allow(dead_code)] // TODO remove when this is used
    /// # Safety
    /// The caller has to ensure that the node type `T` matches the dynamic node type of this chunk.
    pub unsafe fn as_known_unchecked<T: Node>(&self) -> &KnownNodeChunk<T> {
        debug_assert!(self.vtable.has_type::<T>());
        // SAFETY: Every DynNodeChunk with the correct vtable is a valid KnownNodeChunk<T>, which is
        // a documented safety requirement for calling this
        unsafe { std::mem::transmute(self) }
    }

    /// # Safety
    /// The caller has to ensure that the node type `T` matches the dynamic node type of this chunk.
    pub unsafe fn as_known_unchecked_mut<T: Node>(&mut self) -> &mut KnownNodeChunk<T> {
        debug_assert!(self.vtable.has_type::<T>());
        // SAFETY: Every DynNodeChunk with the correct vtable is a valid KnownNodeChunk<T>, which is
        // a documented safety requirement for calling this
        unsafe { std::mem::transmute(self) }
    }
}

type DynNodeChunk = RawNodeChunk<DynNodeType>;

type KnownNodeChunk<T> = RawNodeChunk<KnownNodeType<T>>;

impl<T: Node> KnownNodeChunk<T> {
    fn new_known(type_index: u16) -> Self {
        RawNodeChunk::new(KnownNodeType::new(), type_index)
    }

    fn new_known_with_capacity(type_index: u16, capacity: usize) -> Self {
        RawNodeChunk::new_with_capacity(KnownNodeType::new(), type_index, capacity)
    }
}

struct NodeTypeChunks {
    node_type: NodeType,
    active_chunk: u32,
    len: usize,
    chunks: IndexSet<u32, BuildHasherDefault<ZwoHasher>>,
}

const CHUNK_SIZE: usize = 1 << 12;

/// A heterogeneous collection of [`Node`] values.
///
/// Every inserted node is assigned a [`NodeId`] which can be used to look up, modify or delete
/// nodes. The order in which node ids are assigned and/or re-used is deterministic for the same
/// sequence of operations, but otherwise unspecified and may change across versions.
#[derive(Default)]
pub struct Nodes {
    chunks: Vec<DynNodeChunk>,
    types: Vec<NodeTypeChunks>,
    last_type: u16,
    types_table: HashTable<u16>,
    len: usize,
}

impl Debug for Nodes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map().entries(self.iter()).finish()
    }
}

fn split_id(id: NodeId) -> (usize, usize) {
    let id_index = id.id_index();
    (id_index / CHUNK_SIZE, id_index % CHUNK_SIZE)
}

/// Error cases for accesses a [`Node`] among [`Nodes`] using a given [`NodeId`] and [`NodeType`].
#[derive(Clone, Copy, Debug)]
pub enum NodeError {
    /// The [`NodeId`] is not present in the collection of [`Nodes`].
    NotPresent,
    /// The [`NodeId`] is present but does not have the given [`NodeType`].
    UnexpectedNodeType {
        /// The actual type of the found node.
        found_type: NodeType,
    },
}

impl Nodes {
    /// Returns the number of contained nodes.
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns `true` when the collection of nodes is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    fn find_type<T: Node>(&mut self) -> u16 {
        if let Some(type_data) = self.types.get(self.last_type as usize) {
            if type_data.node_type == NodeType::of::<T>() {
                return self.last_type;
            }
        }

        let hash = <BuildHasherDefault<ZwoHasher>>::default().hash_one(NodeType::of::<T>());

        let entry = self.types_table.entry(
            hash,
            |&entry| self.types[entry as usize].node_type == NodeType::of::<T>(),
            |&entry| {
                <BuildHasherDefault<ZwoHasher>>::default()
                    .hash_one(self.types[entry as usize].node_type)
            },
        );
        match entry {
            hashbrown::hash_table::Entry::Occupied(entry) => *entry.get(),
            hashbrown::hash_table::Entry::Vacant(entry) => {
                let new_chunk = self.chunks.len();
                let type_index: u16 = self.types.len().try_into().unwrap();
                self.chunks
                    .push(<KnownNodeChunk<T>>::new_known(type_index).into_dynamic());
                self.types.push(NodeTypeChunks {
                    node_type: NodeType::of::<T>(),
                    active_chunk: new_chunk as u32,
                    chunks: [new_chunk as u32].into_iter().collect(),
                    len: 0,
                });

                entry.insert(type_index);
                type_index
            }
        }
    }

    // TODO instead of insert_and_get or returning references by default etc, use an entry API.

    #[inline(always)]
    pub(crate) fn insert_and_get<T: Node>(&mut self, node: T) -> (NodeId, &T, &Self) {
        let (node_id, node_ptr) = self.insert_raw(node);

        // SAFETY: we own the node and the resulting reference takes a borrow of self
        (node_id, unsafe { &*node_ptr.cast_const() }, self)
    }

    /// Insert a [`Node`] with a statically known node type.
    pub fn insert<T: Node>(&mut self, node: T) -> (NodeId, &mut T) {
        let (node_id, node_ptr) = self.insert_raw(node);
        // SAFETY: we own the node and the resulting mutable reference takes a mutable borrow of
        // self
        (node_id, unsafe { &mut *node_ptr })
    }

    pub(crate) fn insert_raw<T: Node>(&mut self, node: T) -> (NodeId, *mut T) {
        let type_index = self.find_type::<T>();

        self.last_type = type_index;

        let type_data = &mut self.types[type_index as usize];

        let mut chunk_index = type_data.active_chunk as usize;

        // SAFETY: the `active_chunk` index of the entries in `self.types` will always be in-bounds
        let mut chunk = unsafe { self.chunks.get_unchecked_mut(chunk_index) };

        if chunk.len() == CHUNK_SIZE {
            chunk_index = self.chunks.len();

            self.chunks.push(
                <KnownNodeChunk<T>>::new_known_with_capacity(type_index, CHUNK_SIZE).into_dynamic(),
            );
            type_data.active_chunk = chunk_index as u32;
            type_data.chunks.insert(chunk_index as u32);

            // SAFETY: we just pushed an item to that index
            chunk = unsafe { self.chunks.get_unchecked_mut(chunk_index) };
        }

        // SAFETY: we obtained the chunk either via the `NodeTypeChunks` obtained from
        // `find_type::<T>()` or we just created it ourselves, in both cases we know it will store
        // nodes of type `T`.
        let (slot, node) = unsafe { chunk.as_known_unchecked_mut::<T>().insert(node) };
        self.len += 1;
        type_data.len += 1;
        (
            NodeId::from_id_index(chunk_index * CHUNK_SIZE + slot),
            node as *mut _,
        )
    }

    #[inline(always)]
    pub(crate) fn insert_and_get_dyn(&mut self, node: Take<DynNode>) -> (NodeId, &DynNode, &Self) {
        let (node_id, node_ptr) = self.insert_dyn_raw(node);

        // SAFETY: we own the node and the resulting reference takes a borrow of self
        (node_id, unsafe { &*node_ptr.cast_const() }, self)
    }

    pub(crate) fn insert_dyn(&mut self, node: Take<DynNode>) -> (NodeId, &mut DynNode) {
        let (node_id, node_ptr) = self.insert_dyn_raw(node);
        // SAFETY: we own the node and the resulting mutable reference takes a mutable borrow of
        // self
        (node_id, unsafe { &mut *node_ptr })
    }

    pub(crate) fn insert_dyn_raw(&mut self, node: Take<DynNode>) -> (NodeId, *mut DynNode) {
        let node_type = DynNodeType(node.node_type());
        // SAFETY: as required, we're transferring ownership using `into_raw_ptr`
        let (node_id, node_ptr) =
            unsafe { node_type.insert_raw_dyn(self, node.into_raw_ptr() as *mut u8) };

        // SAFETY: the returned raw pointer will point to the same node, so we can use the same
        // NodeType to construct a trait object pointer
        (node_id, unsafe { node_type.cast_mut_ptr(node_ptr) })
    }

    /// Returns a trait object reference to the node with a given [`NodeId`].
    ///
    /// Returns `None` if the collection has no node of the given id.
    pub fn get_dyn(&self, node_id: NodeId) -> Option<&DynNode> {
        let (chunk_index, chunk_slot) = split_id(node_id);

        self.chunks.get(chunk_index)?.get(chunk_slot)
    }

    /// Returns a mutable trait object reference to the node with a given [`NodeId`].
    ///
    /// Returns `None` if the collection has no node of the given id.
    pub fn get_dyn_mut(&mut self, node_id: NodeId) -> Option<&mut DynNode> {
        let (chunk_index, chunk_slot) = split_id(node_id);

        self.chunks.get_mut(chunk_index)?.get_mut(chunk_slot)
    }

    /// Removes the [`Node`] with a given [`NodeId`] and a statically known node type.
    ///
    /// On success, this will return the removed node. Otherwise, returns a
    /// [`NodeError::NotPresent`] when there is no such node or a [`NodeError::UnexpectedNodeType`]
    /// when the specified node has a different [`NodeType`].
    ///
    /// See [`discard`][`Self::discard`] for a method that removes nodes of arbitrary node types,
    /// dropping the removed node in-place.
    pub fn remove<T: Node>(&mut self, node_id: NodeId) -> Result<T, NodeError> {
        let (chunk_index, chunk_slot) = split_id(node_id);
        if let Some(chunk) = self.chunks.get_mut(chunk_index) {
            match chunk.as_known_mut::<T>() {
                Some(chunk) => match chunk.remove(chunk_slot) {
                    Some(node) => {
                        self.len -= 1;
                        // SAFETY: the type_index value of a chunk is always in bounds
                        unsafe {
                            self.types.get_unchecked_mut(chunk.type_index as usize).len -= 1;
                        }
                        Ok(node)
                    }
                    None => Err(NodeError::NotPresent),
                },
                None => {
                    if chunk.is_present(chunk_slot) {
                        Err(NodeError::UnexpectedNodeType {
                            found_type: chunk.vtable.node_type(),
                        })
                    } else {
                        Err(NodeError::NotPresent)
                    }
                }
            }
        } else {
            Err(NodeError::NotPresent)
        }
    }

    /// Removes the [`Node`] with a given [`NodeId`], passing an ownership-transferring dynamically
    /// typed reference to a callback closure.
    ///
    /// When a node with the given id was found, this will return the callback result and `None`
    /// otherwise.
    pub fn remove_dyn_with<R>(
        &mut self,
        node_id: NodeId,
        f: impl for<'a> FnOnce(Take<'a, DynNode>) -> R,
    ) -> Option<R> {
        let (chunk_index, chunk_slot) = split_id(node_id);
        let chunk = self.chunks.get_mut(chunk_index)?;
        let slot_mut = chunk.get_mut(chunk_slot)?;
        let slot_ptr = slot_mut as *mut DynNode;

        chunk.take_present(chunk_slot);

        // SAFETY: even though the slot is already marked as free by take_present, we maintain
        // ownership of the storage until we return (after that point re-use of the slot would be
        // possible). Since we pass the resulting `Take` reference to a HRTB closure, the `Take`
        // reference cannot escape and we know that no one is still using the storage when we
        // return.
        let result = f(unsafe { Take::from_raw_ptr(slot_ptr) });

        self.len -= 1;
        // SAFETY: the type_index value of a chunk is always in bounds
        unsafe {
            self.types.get_unchecked_mut(chunk.type_index as usize).len -= 1;
        }

        Some(result)
    }

    /// Removes the [`Node`] with a given [`NodeId`].
    ///
    /// Returns `false` when there was no node of the given id and `true` when such a node was
    /// removed.
    ///
    /// This drops the node in-place, see [`remove`][Self::remove] for a method that returns the
    /// removed node, but requires a statically known node type.
    pub fn discard(&mut self, node_id: NodeId) -> bool {
        log::trace!("discard {node_id:?}");
        let (chunk_index, chunk_slot) = split_id(node_id);
        let Some(chunk) = self.chunks.get_mut(chunk_index) else { return false };

        let dropped = chunk.drop(chunk_slot);
        self.len -= dropped as usize;
        // SAFETY: the type_index value of a chunk is always in bounds
        unsafe {
            self.types.get_unchecked_mut(chunk.type_index as usize).len -= dropped as usize;
        }
        dropped
    }

    /// Iterate over all nodes, yielding the assigned id paired with a trait object reference for
    /// each node.
    ///
    /// The results are ordered by id.
    pub fn iter(&self) -> impl Iterator<Item = (NodeId, &DynNode)> {
        self.chunks.iter().enumerate().flat_map(|(index, chunk)| {
            (0..chunk.capacity()).flat_map(move |slot| {
                chunk
                    .get(slot)
                    .map(|node| (NodeId::from_id_index(index * CHUNK_SIZE + slot), node))
            })
        })
    }

    /// Iterate over the used node types, yielding each corresponding [`NodeType`] paired with the
    /// number of contained nodes of that type.
    pub fn node_type_stats(&self) -> impl Iterator<Item = (NodeType, usize)> + '_ {
        self.types
            .iter()
            .flat_map(|data| (data.len != 0).then_some((data.node_type, data.len)))
    }
}
