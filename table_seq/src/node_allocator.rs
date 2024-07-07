use std::{
    alloc::{alloc, dealloc, handle_alloc_error, realloc, Layout},
    marker::PhantomData,
    ops,
    ptr::null_mut,
};

/// Calls [`alloc`] and panics on errors.
/// # Safety
/// Same requirements as `alloc`.
unsafe fn alloc_buf(layout: Layout) -> *mut u8 {
    // SAFETY: delegated requirements to caller
    let buf = unsafe { alloc(layout) };
    if buf.is_null() {
        handle_alloc_error(layout);
    };
    buf
}

/// Calls [`realloc`] and panics on errors.
/// # Safety
/// Same requirements as `realloc`.
unsafe fn realloc_buf(buf: *mut u8, old_layout: Layout, new_layout: Layout) -> *mut u8 {
    assert_eq!(old_layout.align(), new_layout.align());
    // SAFETY: delegated requirements to caller
    let buf = unsafe { realloc(buf, old_layout, new_layout.size()) };
    if buf.is_null() {
        handle_alloc_error(new_layout);
    };
    buf
}

/// Type used to select among different size classes managed by the same [`GenericNodeAllocator`].
///
/// # Safety
/// The return value of `Self::index` must always be in bounds for unchecked accesses into
/// [`Self::ClassHolder`].
///
/// Calling [`Self::initialize_classes`] must return valid [`ClassAllocator`] values.
pub unsafe trait AllocatorClass: Copy {
    type ClassHolder: AsRef<[ClassAllocator]> + AsMut<[ClassAllocator]>;

    fn initialize_classes() -> Self::ClassHolder;

    fn index(self) -> usize;

    fn size(self) -> usize;

    fn align(self) -> usize {
        let size = self.size();

        ((size - 1) & !size) + 1
    }

    fn offset(self, id: usize) -> usize {
        self.size() * id
    }

    unsafe fn class_for_index(index: usize) -> Self;

    fn max_id(self) -> usize {
        isize::MAX as usize
    }
}

pub struct ClassAllocator {
    base: *mut u8,
    cap: u32,
    free: u32,
    #[cfg(debug_assertions)]
    allocated_nodes: std::collections::HashSet<usize>,
}

impl Default for ClassAllocator {
    fn default() -> Self {
        Self {
            base: null_mut(),
            cap: 0,
            free: 0,
            #[cfg(debug_assertions)]
            allocated_nodes: Default::default(),
        }
    }
}

/// Bundles a pointer to the allocator for a size class with the metadata for the size class itself.
struct ClassAllocatorPtr<C: AllocatorClass> {
    data: *mut ClassAllocator,
    size: C,
}

#[repr(packed)]
struct FreelistIndex {
    index: u32,
}

impl<C: AllocatorClass> ClassAllocatorPtr<C> {
    pub fn new(data: *mut ClassAllocator, size: C) -> Self {
        Self { data, size }
    }

    /// Obtains a pointer to an allocation of this size class
    /// # Safety
    /// The underlying allocator must be alive and the id must correspond to a currently alive
    /// allocation made using the same underlying allocator.
    pub unsafe fn ptr(&self, id: usize) -> *mut u8 {
        // SAFETY: we require the allocator to be alive
        let data = unsafe { &*self.data };

        // checks whether the id belongs to an allocation that is alive
        #[cfg(debug_assertions)]
        debug_assert!(data.allocated_nodes.contains(&id));

        // SAFETY: will stay within the allocated storage when the id belongs to an alive
        // allocation, which is a documnted requirement (and checked with debug assertions)
        unsafe { data.base.add(self.size.size() * id) }
    }

    /// Grows the storage for this allactor.
    /// # Safety
    /// The underlying allocator must be alive.
    pub unsafe fn grow(&self, stats: &mut AllocatorStats) {
        // SAFETY: we require the allocator to be alive
        let data = unsafe { &mut *self.data };

        let size = self.size.size();
        let align = self.size.align();

        let new_cap = (data.cap as usize * 2 - data.cap as usize / 4 * 3)
            .clamp(2, ((isize::MAX as usize) / size).min(self.size.max_id()))
            as u32;
        assert_ne!(new_cap, data.cap);

        let old_layout = Layout::from_size_align(size * data.cap as usize, align).unwrap();
        let new_layout = Layout::from_size_align(size * new_cap as usize, align).unwrap();

        // SAFETY: the new_cap is clamped away from zero and the size of a class is always non-zero
        // as it's large enough to include the freelist index. This ensures we wont pass a zero size
        // to the global allocator.
        unsafe {
            if data.cap == 0 {
                data.base = alloc_buf(new_layout);
            } else {
                data.base = realloc_buf(data.base, old_layout, new_layout);
            }
        }

        stats.reserved += new_layout.size() - old_layout.size();

        for new_id in data.cap..new_cap {
            // SAFETY: this stays in bounds and only writes to the newly grown part of the buffer
            unsafe {
                let ptr = data.base.add(size * new_id as usize);
                ptr.cast::<FreelistIndex>()
                    .write(FreelistIndex { index: new_id + 1 });
            }
        }
        data.cap = new_cap;
    }

    /// Allocates a new item from this allocator.
    /// # Safety
    /// The underlying allocator must be alive.
    pub unsafe fn alloc(&self, stats: &mut AllocatorStats) -> usize {
        // SAFETY: we require the allocator to be alive
        let data = unsafe { &mut *self.data };
        let id = data.free;

        if id >= data.cap {
            // SAFETY: allocator is alive
            unsafe { self.grow(stats) };
        }

        // SAFETY: allocator is still alive
        let data = unsafe { &mut *self.data };

        // SAFETY: this read to update the freelist head pointer is in bounds since if it wasn't at
        // the start of this function we would have called grow which ensures it will be afterwards
        // (or panics if it can't).
        unsafe {
            data.free = (*data
                .base
                .add(self.size.offset(id as usize))
                .cast::<FreelistIndex>())
            .index;
        }

        stats.used += self.size.size();

        #[cfg(debug_assertions)]
        data.allocated_nodes.insert(id as usize);

        id as usize
    }

    /// Deallocates an item previously allocated from this allocator.
    /// # Safety
    /// The underlying allocator must be alive and the id must correspond to a currently alive
    /// allocation made using the same underlying allocator.
    pub unsafe fn dealloc(&self, id: usize, stats: &mut AllocatorStats) {
        // SAFETY: we require the allocator to be alive
        let data = unsafe { &mut *self.data };

        #[cfg(debug_assertions)]
        debug_assert!(data.allocated_nodes.remove(&id));

        stats.used -= self.size.size();

        // SAFETY: since we require the allocation with the given id to be alive (which we also
        // check with a debug assert), this will be in bounds
        unsafe {
            data.base
                .add(self.size.offset(id))
                .cast::<FreelistIndex>()
                .write(FreelistIndex { index: data.free });
        }
        data.free = id as u32;
    }

    /// Deallocates the backing storage managed by this allocator.
    /// # Safety
    /// The underlying allocator must be alive.
    pub unsafe fn drop_storage(&self) {
        // SAFETY: we require the allocator to be alive
        let data = unsafe { &mut *self.data };
        if data.cap != 0 {
            let align = self.size.align();
            let old_layout =
                Layout::from_size_align(self.size.size() * data.cap as usize, align).unwrap();
            // SAFETY: due to the minimum size enforced by the freelist pointer a non-zero cap
            // ensures a non-zero allocation size
            unsafe { dealloc(data.base, old_layout) };
            data.cap = 0;
        }
    }
}

pub struct GenericNodeAllocator<C: AllocatorClass> {
    classes: C::ClassHolder,
    _phantom: PhantomData<C>,
}

impl<C: AllocatorClass> Default for GenericNodeAllocator<C> {
    fn default() -> Self {
        Self {
            classes: C::initialize_classes(),
            _phantom: PhantomData,
        }
    }
}

impl<C: AllocatorClass> Drop for GenericNodeAllocator<C> {
    fn drop(&mut self) {
        for (i, class) in self.classes.as_mut().iter_mut().enumerate() {
            // SAFETY: all size class allocators are alive between the initialization in `default()`
            // and when this runs and drops them
            unsafe {
                ClassAllocatorPtr::new(class, C::class_for_index(i)).drop_storage();
            }
        }
    }
}

impl<C: AllocatorClass> GenericNodeAllocator<C> {
    fn class_ptr(&self, size: C) -> ClassAllocatorPtr<C> {
        ClassAllocatorPtr::new(
            // SAFETY: in bounds given [`AllocatorClass`] requirements
            unsafe { self.classes.as_ref().get_unchecked(size.index()) as *const _ as *mut _ },
            size,
        )
    }

    fn class_ptr_mut(&mut self, size: C) -> ClassAllocatorPtr<C> {
        ClassAllocatorPtr::new(
            // SAFETY: in bounds given [`AllocatorClass`] requirements
            unsafe { self.classes.as_mut().get_unchecked_mut(size.index()) as *mut _ },
            size,
        )
    }

    pub fn alloc(&mut self, size: C, stats: &mut AllocatorStats) -> usize {
        // SAFETY: all size class allocators are alive when self is alive
        unsafe { self.class_ptr_mut(size).alloc(stats) }
    }

    /// Deallocates an item previously allocated from this allocator with the given size class.
    /// # Safety
    /// The given `id` must correspond to an allocation of the given size class that is currently
    /// alive.
    pub unsafe fn dealloc(&mut self, size: C, id: usize, stats: &mut AllocatorStats) {
        // SAFETY: all size class allocators are alive when self is alive and we pass the
        // requriement for the id to the caller.
        unsafe { self.class_ptr_mut(size).dealloc(id, stats) }
    }

    /// Returns a pointer to an item previously allocated from this allocator with the given size
    /// class.
    /// # Safety
    /// The given `id` must correspond to an allocation of the given size class that is currently
    /// alive.
    pub unsafe fn ptr(&self, size: C, id: usize) -> *mut u8 {
        // SAFETY: all size class allocators are alive when self is alive and we pass the
        // requriement for the id to the caller.
        unsafe { self.class_ptr(size).ptr(id) }
    }
}

#[derive(Clone, Copy, Default, Debug)]
pub struct AllocatorStats {
    pub used: usize,
    pub reserved: usize,
}

impl ops::Add for AllocatorStats {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        Self {
            used: self.used + rhs.used,
            reserved: self.reserved + rhs.reserved,
        }
    }
}
