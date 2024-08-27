use std::{
    marker::PhantomData,
    mem::{align_of, size_of},
};

use crate::node_allocator::{AllocatorClass, AllocatorStats, ClassAllocator, GenericNodeAllocator};

use super::{
    chunk::{Chunk, CHUNK_SIZE},
    table::Subtable,
};

const fn max_size(a: usize, b: usize) -> usize {
    if a < b {
        b
    } else {
        a
    }
}

pub struct SizeClass<T> {
    _phantom: PhantomData<T>,
    index: u8,
}

impl<T> Copy for SizeClass<T> {}

impl<T> Clone for SizeClass<T> {
    fn clone(&self) -> Self {
        *self
    }
}

const SIZE_CLASS_COUNT: usize = 16;
const SIZE_CLASS_BITS: u32 = (SIZE_CLASS_COUNT - 1).ilog2() + 1;
const SIZE_CLASS_MASK: NodeId = !(!0 << SIZE_CLASS_BITS);

impl<T> SizeClass<T> {
    const MAX_SIZE: usize = max_size(
        size_of::<T>() * 2 * CHUNK_SIZE,
        size_of::<Subtable<T>>() * CHUNK_SIZE,
    );
    const MIN_SIZE: usize = max_size(size_of::<u32>(), size_of::<T>());

    const CLASS_SIZE: [usize; SIZE_CLASS_COUNT] = {
        let mut sizes = [1; SIZE_CLASS_COUNT];

        sizes[SIZE_CLASS_COUNT - 1] = Self::MAX_SIZE;
        sizes[0] = Self::MIN_SIZE;

        let mut i = SIZE_CLASS_COUNT - 1;

        // Initialize with an exponential sequence
        while i > 1 {
            i -= 1;
            sizes[i] = sizes[i + 1] * 3 / 4;
        }

        // Postprocess it to
        // * increase by at least one entry in each step
        // * increase by at least as much as the previous step did
        // * fullfill all alignment requirements
        // * exactly hit 16 entries (max small table size)
        let mut step = size_of::<T>();

        while i + 1 < SIZE_CLASS_COUNT {
            // Increase by at least the current minimal step size
            if sizes[i] < sizes[i - 1] + step {
                sizes[i] = sizes[i - 1] + step;
            }

            // Always align to the size of an entry
            sizes[i] += sizes[i].wrapping_neg() & (align_of::<T>() - 1);

            // Additionally if we're large enough to store a table, align for that too
            if sizes[i] >= size_of::<Subtable<T>>() {
                sizes[i] += sizes[i].wrapping_neg() & (align_of::<Subtable<T>>() - 1);
            }

            // If we jumped over the 16 entry size, adjust some values to exactly hit 16 entries
            if sizes[i] > 16 * size_of::<T>() && sizes[i - 1] < 16 * size_of::<T>() {
                let delta = 16 * size_of::<T>() - sizes[i - 1];
                sizes[i - 1] = 16 * size_of::<T>();
                let mut j = 1;

                // To avoid a big jump we also increase some of the preceding entries and then
                // restart the postprocessing to ensure this doesn't break any invariants.

                while j < i - 1 {
                    sizes[i - 1 - j] += delta >> j;
                    j += 1;
                }
                i = 1;
                step = size_of::<T>();
                continue;
            }

            // Update the minimal step size to be at least the last step taken
            if step < sizes[i] - sizes[i - 1] {
                step = sizes[i] - sizes[i - 1];
            }

            i += 1;
        }

        sizes
    };

    const CLASS_LEN: [usize; SIZE_CLASS_COUNT] = {
        let mut size_classes = [0usize; SIZE_CLASS_COUNT];

        let mut i = 0;
        while i < SIZE_CLASS_COUNT {
            size_classes[i] = Self::CLASS_SIZE[i] / size_of::<T>();
            i += 1;
        }
        size_classes
    };

    const AT_LEAST_3_INDEX: usize = {
        let mut i = 0;
        while Self::CLASS_LEN[i] < 3 {
            i += 1;
        }
        i
    };

    pub fn at_least_3() -> Self {
        unsafe { Self::class_for_index(Self::AT_LEAST_3_INDEX) }
    }

    pub fn len(self) -> usize {
        Self::CLASS_LEN[self.index()]
    }

    pub fn next(self) -> Self {
        debug_assert_ne!(self.index as usize, Self::CLASS_LEN.len() - 1);
        Self {
            _phantom: PhantomData,
            index: self.index + 1,
        }
    }
}

unsafe impl<T> AllocatorClass for SizeClass<T> {
    type ClassHolder = [ClassAllocator; SIZE_CLASS_COUNT];

    fn index(self) -> usize {
        self.index as usize
    }

    fn size(self) -> usize {
        Self::CLASS_SIZE[self.index()]
    }

    unsafe fn class_for_index(index: usize) -> Self {
        debug_assert!(index < SIZE_CLASS_COUNT);
        Self {
            _phantom: PhantomData,
            index: index as u8,
        }
    }

    fn align(self) -> usize {
        let size = self.size();

        ((size - 1) & !size) + 1
    }

    fn offset(self, id: usize) -> usize {
        self.size() * id
    }

    fn max_id(self) -> usize {
        (NodeId::MAX as usize) >> SIZE_CLASS_BITS
    }

    fn initialize_classes() -> Self::ClassHolder {
        std::array::from_fn(|_| ClassAllocator::default())
    }
}

type NodeId = u32;

pub struct NodeRef<T> {
    _phantom: PhantomData<T>,
    code: NodeId,
}

impl<T> Default for NodeRef<T> {
    fn default() -> Self {
        Self {
            _phantom: PhantomData,
            code: 0,
        }
    }
}

impl<T> NodeRef<T> {
    pub fn new(class: SizeClass<T>, index: usize) -> Self {
        debug_assert!(index < (NodeId::MAX as usize) >> SIZE_CLASS_BITS);
        Self {
            _phantom: PhantomData,
            code: (class.index as NodeId) | ((index as NodeId) << SIZE_CLASS_BITS),
        }
    }

    pub fn size_class(&self) -> SizeClass<T> {
        SizeClass {
            _phantom: PhantomData,
            index: (self.code & SIZE_CLASS_MASK) as u8,
        }
    }

    pub fn index(&self) -> usize {
        (self.code >> SIZE_CLASS_BITS) as usize
    }
}

impl<T> Copy for NodeRef<T> {}

impl<T> Clone for NodeRef<T> {
    fn clone(&self) -> Self {
        *self
    }
}

pub struct NodeAllocator<T> {
    pub inner: GenericNodeAllocator<SizeClass<T>>,
    pub stats: AllocatorStats,
}

impl<T> Default for NodeAllocator<T> {
    fn default() -> Self {
        Self {
            inner: Default::default(),
            stats: AllocatorStats::default(),
        }
    }
}

impl<T> NodeAllocator<T> {
    pub fn alloc(&mut self, class: SizeClass<T>) -> NodeRef<T> {
        NodeRef::new(class, self.inner.alloc(class, &mut self.stats))
    }

    pub unsafe fn ptr(&self, node: NodeRef<T>) -> *mut T {
        unsafe { self.inner.ptr(node.size_class(), node.index()).cast() }
    }

    pub unsafe fn node(&self, node: NodeRef<T>, entry_count: usize, table_count: usize) -> Node<T> {
        let size_class = node.size_class();
        Node {
            size_class,
            entry_count,
            table_count,
            ptr: unsafe { self.inner.ptr(size_class, node.index()).cast() },
        }
    }

    pub unsafe fn dealloc(&mut self, node: NodeRef<T>) {
        unsafe {
            self.inner
                .dealloc(node.size_class(), node.index(), &mut self.stats)
        }
    }
}

pub struct Node<T> {
    size_class: SizeClass<T>,
    entry_count: usize,
    table_count: usize,
    ptr: *mut T,
}

impl<T> Node<T> {
    #[inline(always)]
    pub fn ptr(&self) -> *mut T {
        self.ptr
    }

    #[inline(always)]
    pub fn refresh_ptr(&mut self, ptr: *mut T) {
        self.ptr = ptr
    }

    #[inline(always)]
    pub fn size_class(&self) -> SizeClass<T> {
        self.size_class
    }

    #[inline(always)]
    pub fn entry_count(&self) -> usize {
        self.entry_count
    }

    #[inline(always)]
    pub fn table_count(&self) -> usize {
        self.table_count
    }

    #[inline]
    pub fn grow_for_size(&self, required_size: usize) -> Option<SizeClass<T>> {
        if required_size <= self.size_class.size() {
            None
        } else {
            let mut new_size_class = self.size_class.next();
            while required_size > new_size_class.size() {
                new_size_class = new_size_class.next();
            }
            Some(new_size_class)
        }
    }

    #[inline(always)]
    pub fn grow_for_new_entry(&self) -> Option<SizeClass<T>> {
        let required_size =
            self.table_count * size_of::<Subtable<T>>() + (self.entry_count + 1) * size_of::<T>();
        if required_size <= self.size_class.size() {
            None
        } else {
            // the next class has space for at least one additional entry, no need to loop
            Some(self.size_class.next())
        }
    }

    #[inline(always)]
    pub fn resize_for_new_table_replacing_entry_pair(&self) -> Option<SizeClass<T>> {
        #[allow(clippy::comparison_chain)]
        if size_of::<Subtable<T>>() > size_of::<[T; 2]>() {
            self.grow_for_size(
                (self.table_count + 1) * size_of::<Subtable<T>>()
                    + (self.entry_count - 2) * size_of::<T>(),
            )
        } else if size_of::<Subtable<T>>() < size_of::<[T; 2]>() {
            self.shrink_for_size(
                (self.table_count + 1) * size_of::<Subtable<T>>()
                    + (self.entry_count - 2) * size_of::<T>(),
            )
        } else {
            None
        }
    }

    #[inline]
    pub fn shrink_for_size(&self, target_size: usize) -> Option<SizeClass<T>> {
        if self.size_class().index() <= 2 || target_size == 0 {
            return None;
        }

        let target_class =
            unsafe { <SizeClass<T>>::class_for_index(self.size_class().index() - 2) };

        if target_size <= target_class.size() {
            Some(target_class)
        } else {
            None
        }
    }

    #[inline]
    pub fn shrink_for_removed_entry(&self) -> Option<SizeClass<T>> {
        self.shrink_for_size(
            self.table_count * size_of::<Subtable<T>>() + (self.entry_count - 1) * size_of::<T>(),
        )
    }

    #[inline]
    pub fn shrink_for_removed_entry_pair(&self) -> Option<SizeClass<T>> {
        self.shrink_for_size(
            self.table_count * size_of::<Subtable<T>>() + (self.entry_count - 2) * size_of::<T>(),
        )
    }

    #[inline]
    pub fn shrink_for_removed_table(&self) -> Option<SizeClass<T>> {
        self.shrink_for_size(
            (self.table_count - 1) * size_of::<Subtable<T>>() + self.entry_count * size_of::<T>(),
        )
    }

    #[inline(always)]
    pub unsafe fn entry_ptr(&self, offset: usize) -> *mut T {
        unsafe { self.ptr.add(offset) }
    }

    #[inline(always)]
    pub unsafe fn entries_raw(&self) -> *mut [T] {
        std::ptr::slice_from_raw_parts_mut(self.ptr(), self.entry_count())
    }

    #[inline(always)]
    pub unsafe fn tables_slice_end(&self) -> *mut Subtable<T> {
        unsafe {
            self.ptr
                .cast::<u8>()
                .add(self.size_class.size())
                .cast::<Subtable<T>>()
        }
    }

    #[inline(always)]
    pub unsafe fn tables_slice_start(&self) -> *mut Subtable<T> {
        unsafe { self.tables_slice_end().sub(self.table_count) }
    }

    #[inline(always)]
    pub unsafe fn tables_raw(&self) -> *mut [Subtable<T>] {
        std::ptr::slice_from_raw_parts_mut(unsafe { self.tables_slice_start() }, self.table_count)
    }

    #[inline(always)]
    pub unsafe fn table_ptr(&self, offset: usize) -> *mut Subtable<T> {
        unsafe { self.tables_slice_end().sub(offset + 1) }
    }

    #[inline(always)]
    pub unsafe fn close_table_gap(&mut self, gap_offset: usize) {
        let source = unsafe { self.tables_slice_start() };
        let dest = unsafe { source.add(1) };
        self.table_count -= 1;
        unsafe { source.copy_to(dest, self.table_count - gap_offset) };
    }

    #[inline(always)]
    pub unsafe fn close_table_gap_move_into(&mut self, dest_node: &mut Self, gap_offset: usize) {
        debug_assert_eq!(dest_node.entry_count, 0);
        debug_assert_eq!(dest_node.table_count, 0);
        debug_assert_ne!(self.ptr, dest_node.ptr);

        // move entries
        let source = self.ptr();
        let dest = dest_node.ptr();
        unsafe { source.copy_to_nonoverlapping(dest, self.entry_count) };

        if self.table_count > 0 {
            // move table suffix
            let source = unsafe { self.tables_slice_start() };
            let dest = unsafe { dest_node.tables_slice_end().sub(self.table_count - 1) };
            unsafe { source.copy_to_nonoverlapping(dest, self.table_count - 1 - gap_offset) };

            // move table prefix
            let source = unsafe { self.tables_slice_end().sub(gap_offset) };
            let dest = unsafe { dest_node.tables_slice_end().sub(gap_offset) };
            unsafe { source.copy_to_nonoverlapping(dest, gap_offset) };
        }

        dest_node.entry_count = self.entry_count;
        dest_node.table_count = self.table_count - 1;

        self.entry_count = 0;
        self.table_count = 0;
    }

    #[inline(always)]
    pub unsafe fn close_table_gap_resize(
        &mut self,
        table_offset: usize,
        chunk: &mut Chunk<T>,
        chunk_alloc: &mut NodeAllocator<T>,
    ) {
        if let Some(new_size_class) = self.shrink_for_removed_table() {
            let new_node_ref = chunk_alloc.alloc(new_size_class);
            let mut new_node = unsafe { chunk_alloc.node(new_node_ref, 0, 0) };
            self.refresh_ptr(unsafe { chunk_alloc.ptr(chunk.node) });

            unsafe { self.close_table_gap_move_into(&mut new_node, table_offset) };
            unsafe { chunk_alloc.dealloc(chunk.node) };
            chunk.node = new_node_ref;
            *self = new_node;
        } else {
            unsafe { self.close_table_gap(table_offset) };
        }
    }

    #[inline(always)]
    pub unsafe fn make_table_gap(&mut self, gap_offset: usize) {
        let source = unsafe { self.tables_slice_start() };
        let dest = unsafe { source.sub(1) };
        unsafe { source.copy_to(dest, self.table_count - gap_offset) };
        self.table_count += 1;
    }

    #[inline(always)]
    pub unsafe fn close_entry_pair_gap_and_make_table_gap(
        &mut self,
        entry_gap_offset: usize,
        table_gap_offset: usize,
    ) {
        unsafe { self.close_entry_pair_gap(entry_gap_offset) };
        unsafe { self.make_table_gap(table_gap_offset) };
    }

    #[inline(always)]
    pub unsafe fn close_entry_pair_gap_and_make_table_gap_move_into(
        &mut self,
        dest_node: &mut Self,
        entry_gap_offset: usize,
        table_gap_offset: usize,
    ) {
        debug_assert_eq!(dest_node.entry_count, 0);
        debug_assert_eq!(dest_node.table_count, 0);
        debug_assert_ne!(self.ptr, dest_node.ptr);

        dest_node.entry_count = self.entry_count - 2;

        // move entry prefix
        let source = self.ptr();
        let dest = dest_node.ptr();
        unsafe { source.copy_to_nonoverlapping(dest, entry_gap_offset) };

        // move entry suffix
        let source = unsafe { self.entry_ptr(entry_gap_offset + 2) };
        let dest = unsafe { dest_node.entry_ptr(entry_gap_offset) };
        unsafe { source.copy_to_nonoverlapping(dest, dest_node.entry_count - entry_gap_offset) };

        if self.table_count > 0 {
            // move table suffix
            let source = unsafe { self.tables_slice_start() };
            let dest = unsafe { dest_node.tables_slice_end().sub(self.table_count + 1) };
            unsafe { source.copy_to_nonoverlapping(dest, self.table_count - table_gap_offset) };

            // move table prefix
            let source = unsafe { self.tables_slice_end().sub(table_gap_offset) };
            let dest = unsafe { dest_node.tables_slice_end().sub(table_gap_offset) };
            unsafe { source.copy_to_nonoverlapping(dest, table_gap_offset) };
        }

        dest_node.table_count = self.table_count + 1;

        self.entry_count = 0;
        self.table_count = 0;
    }

    #[inline(always)]
    pub unsafe fn close_entry_pair_gap_and_make_table_gap_resize(
        &mut self,
        entry_offset: usize,
        table_offset: usize,
        chunk: &mut Chunk<T>,
        chunk_alloc: &mut NodeAllocator<T>,
    ) {
        if let Some(new_size_class) = self.resize_for_new_table_replacing_entry_pair() {
            let new_node_ref = chunk_alloc.alloc(new_size_class);
            let mut new_node = unsafe { chunk_alloc.node(new_node_ref, 0, 0) };
            self.refresh_ptr(unsafe { chunk_alloc.ptr(chunk.node) });

            unsafe {
                self.close_entry_pair_gap_and_make_table_gap_move_into(
                    &mut new_node,
                    entry_offset,
                    table_offset,
                )
            };
            unsafe { chunk_alloc.dealloc(chunk.node) };
            chunk.node = new_node_ref;
            *self = new_node;
        } else {
            unsafe { self.close_entry_pair_gap_and_make_table_gap(entry_offset, table_offset) };
        }
    }

    #[inline(always)]
    pub unsafe fn close_entry_gap(&mut self, gap_offset: usize) {
        let source = unsafe { self.entry_ptr(gap_offset + 1) };
        let dest = unsafe { source.sub(1) };
        self.entry_count -= 1;
        unsafe { source.copy_to(dest, self.entry_count - gap_offset) };
    }

    #[inline(always)]
    pub unsafe fn close_entry_gap_move_into(&mut self, dest_node: &mut Self, gap_offset: usize) {
        debug_assert_eq!(dest_node.entry_count, 0);
        debug_assert_eq!(dest_node.table_count, 0);
        debug_assert_ne!(self.ptr, dest_node.ptr);

        dest_node.entry_count = self.entry_count - 1;
        dest_node.table_count = self.table_count;

        // move entry prefix
        let source = self.ptr();
        let dest = dest_node.ptr();
        unsafe { source.copy_to_nonoverlapping(dest, gap_offset) };

        // move entry suffix
        let source = unsafe { self.entry_ptr(gap_offset + 1) };
        let dest = unsafe { dest_node.entry_ptr(gap_offset) };
        unsafe { source.copy_to_nonoverlapping(dest, dest_node.entry_count - gap_offset) };

        // move tables
        if self.table_count > 0 {
            let source = unsafe { self.tables_slice_start() };
            let dest = unsafe { dest_node.tables_slice_start() };
            unsafe { source.copy_to_nonoverlapping(dest, self.table_count) };
        }

        self.entry_count = 0;
        self.table_count = 0;
    }

    #[inline(always)]
    pub unsafe fn close_entry_gap_resize(
        &mut self,
        entry_offset: usize,
        chunk: &mut Chunk<T>,
        chunk_alloc: &mut NodeAllocator<T>,
    ) {
        if let Some(new_size_class) = self.shrink_for_removed_entry() {
            let new_node_ref = chunk_alloc.alloc(new_size_class);
            let mut new_node = unsafe { chunk_alloc.node(new_node_ref, 0, 0) };
            self.refresh_ptr(unsafe { chunk_alloc.ptr(chunk.node) });

            unsafe { self.close_entry_gap_move_into(&mut new_node, entry_offset) };
            unsafe { chunk_alloc.dealloc(chunk.node) };
            chunk.node = new_node_ref;
            *self = new_node;
        } else {
            unsafe { self.close_entry_gap(entry_offset) };
        }
    }

    #[inline(always)]
    pub unsafe fn close_entry_pair_gap(&mut self, gap_offset: usize) {
        let source = unsafe { self.entry_ptr(gap_offset + 2) };
        let dest = unsafe { source.sub(2) };
        self.entry_count -= 2;
        unsafe { source.copy_to(dest, self.entry_count - gap_offset) };
    }

    #[inline(always)]
    pub unsafe fn close_entry_pair_gap_move_into(
        &mut self,
        dest_node: &mut Self,
        gap_offset: usize,
    ) {
        debug_assert_eq!(dest_node.entry_count, 0);
        debug_assert_eq!(dest_node.table_count, 0);
        debug_assert_ne!(self.ptr, dest_node.ptr);

        dest_node.entry_count = self.entry_count - 2;
        dest_node.table_count = self.table_count;

        // move entry prefix
        let source = self.ptr();
        let dest = dest_node.ptr();
        unsafe { source.copy_to_nonoverlapping(dest, gap_offset) };

        // move entry suffix
        let source = unsafe { self.entry_ptr(gap_offset + 2) };
        let dest = unsafe { dest_node.entry_ptr(gap_offset) };
        unsafe { source.copy_to_nonoverlapping(dest, dest_node.entry_count - gap_offset) };

        // move tables
        if self.table_count > 0 {
            let source = unsafe { self.tables_slice_start() };
            let dest = unsafe { dest_node.tables_slice_start() };
            unsafe { source.copy_to_nonoverlapping(dest, self.table_count) };
        }

        self.entry_count = 0;
        self.table_count = 0;
    }

    #[inline(always)]
    pub unsafe fn close_entry_pair_gap_resize(
        &mut self,
        entry_offset: usize,
        chunk: &mut Chunk<T>,
        chunk_alloc: &mut NodeAllocator<T>,
    ) {
        if let Some(new_size_class) = self.shrink_for_removed_entry_pair() {
            let new_node_ref = chunk_alloc.alloc(new_size_class);
            let mut new_node = unsafe { chunk_alloc.node(new_node_ref, 0, 0) };
            self.refresh_ptr(unsafe { chunk_alloc.ptr(chunk.node) });

            unsafe { self.close_entry_pair_gap_move_into(&mut new_node, entry_offset) };
            unsafe { chunk_alloc.dealloc(chunk.node) };
            chunk.node = new_node_ref;
            *self = new_node;
        } else {
            unsafe { self.close_entry_pair_gap(entry_offset) };
        }
    }

    #[inline(always)]
    pub unsafe fn make_entry_gap_resize(
        &mut self,
        entry_offset: usize,
        chunk: &mut Chunk<T>,
        chunk_alloc: &mut NodeAllocator<T>,
    ) {
        if let Some(new_size_class) = self.grow_for_new_entry() {
            let new_node_ref = chunk_alloc.alloc(new_size_class);
            let mut new_node = unsafe { chunk_alloc.node(new_node_ref, 0, 0) };
            self.refresh_ptr(unsafe { chunk_alloc.ptr(chunk.node) });

            unsafe { self.make_entry_gap_move_into(&mut new_node, entry_offset) };
            unsafe { chunk_alloc.dealloc(chunk.node) };
            chunk.node = new_node_ref;
            *self = new_node;
        } else {
            unsafe { self.make_entry_gap(entry_offset) };
        }
    }

    #[inline(always)]
    pub unsafe fn make_entry_gap(&mut self, gap_offset: usize) {
        let source = unsafe { self.entry_ptr(gap_offset) };
        let dest = unsafe { source.add(1) };
        unsafe { source.copy_to(dest, self.entry_count - gap_offset) };
        self.entry_count += 1;
    }

    #[inline(always)]
    pub unsafe fn make_entry_gap_move_into(&mut self, dest_node: &mut Self, gap_offset: usize) {
        debug_assert_eq!(dest_node.entry_count, 0);
        debug_assert_eq!(dest_node.table_count, 0);
        debug_assert_ne!(self.ptr, dest_node.ptr);

        // move entry prefix
        let source = self.ptr();
        let dest = dest_node.ptr();
        unsafe { source.copy_to_nonoverlapping(dest, gap_offset) };

        // move entry suffix
        let source = unsafe { self.entry_ptr(gap_offset) };
        let dest = unsafe { dest_node.entry_ptr(gap_offset + 1) };
        unsafe { source.copy_to_nonoverlapping(dest, self.entry_count - gap_offset) };

        dest_node.entry_count = self.entry_count + 1;
        dest_node.table_count = self.table_count;

        // move tables
        if self.table_count > 0 {
            let source = unsafe { self.tables_slice_start() };
            let dest = unsafe { dest_node.tables_slice_start() };
            unsafe { source.copy_to_nonoverlapping(dest, self.table_count) };
        }

        self.entry_count = 0;
        self.table_count = 0;
    }
}
