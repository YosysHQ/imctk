use std::{
    alloc::{alloc, dealloc, handle_alloc_error, realloc, Layout},
    marker::PhantomData,
    ops,
    ptr::null_mut,
};

unsafe fn alloc_buf(layout: Layout) -> *mut u8 {
    let buf = alloc(layout);
    if buf.is_null() {
        handle_alloc_error(layout);
    };
    buf
}

unsafe fn realloc_buf(buf: *mut u8, old_layout: Layout, new_layout: Layout) -> *mut u8 {
    assert_eq!(old_layout.align(), new_layout.align());
    let buf = realloc(buf, old_layout, new_layout.size());
    if buf.is_null() {
        handle_alloc_error(new_layout);
    };
    buf
}

pub trait AllocatorClass: Copy {
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

    fn class_for_index(index: usize) -> Self;

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

    pub unsafe fn ptr(&self, id: usize) -> *mut u8 {
        let data = &*self.data;
        #[cfg(debug_assertions)]
        debug_assert!(data.allocated_nodes.contains(&id));

        data.base.add(self.size.size() * id)
    }

    pub unsafe fn grow(&self, stats: &mut AllocatorStats) {
        let data = &mut *self.data;

        let size = self.size.size();
        let align = self.size.align();

        let new_cap = (data.cap as usize * 2 - data.cap as usize / 4 * 3)
            .clamp(2, ((isize::MAX as usize) / size).min(self.size.max_id()))
            as u32;
        assert_ne!(new_cap, data.cap);

        let old_layout = Layout::from_size_align(size * data.cap as usize, align).unwrap();
        let new_layout = Layout::from_size_align(size * new_cap as usize, align).unwrap();

        if data.cap == 0 {
            data.base = alloc_buf(new_layout);
        } else {
            data.base = realloc_buf(data.base, old_layout, new_layout);
        }

        stats.reserved += new_layout.size() - old_layout.size();

        for new_id in data.cap..new_cap {
            let ptr = data.base.add(size * new_id as usize);
            ptr.cast::<FreelistIndex>()
                .write(FreelistIndex { index: new_id + 1 });
        }
        data.cap = new_cap;
    }

    pub unsafe fn alloc(&self, stats: &mut AllocatorStats) -> usize {
        let data = &mut *self.data;
        let id = data.free;

        if id >= data.cap {
            self.grow(stats);
        }

        let data = &mut *self.data;
        data.free = (*data
            .base
            .add(self.size.offset(id as usize))
            .cast::<FreelistIndex>())
        .index;

        stats.used += self.size.size();

        #[cfg(debug_assertions)]
        data.allocated_nodes.insert(id as usize);

        id as usize
    }

    pub unsafe fn dealloc(&self, id: usize, stats: &mut AllocatorStats) {
        let data = &mut *self.data;

        #[cfg(debug_assertions)]
        debug_assert!(data.allocated_nodes.remove(&id));

        stats.used -= self.size.size();

        data.base
            .add(self.size.offset(id))
            .cast::<FreelistIndex>()
            .write(FreelistIndex { index: data.free });
        data.free = id as u32;
    }

    pub unsafe fn drop_storage(&self) {
        let data = &mut *self.data;
        if data.cap != 0 {
            let align = self.size.align();
            let old_layout =
                Layout::from_size_align(self.size.size() * data.cap as usize, align).unwrap();
            dealloc(data.base, old_layout);
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
            unsafe {
                ClassAllocatorPtr::new(class, C::class_for_index(i)).drop_storage();
            }
        }
    }
}

impl<C: AllocatorClass> GenericNodeAllocator<C> {
    unsafe fn class_ptr(&self, size: C) -> ClassAllocatorPtr<C> {
        ClassAllocatorPtr::new(
            self.classes.as_ref().get_unchecked(size.index()) as *const _ as *mut _,
            size,
        )
    }

    unsafe fn class_ptr_mut(&mut self, size: C) -> ClassAllocatorPtr<C> {
        ClassAllocatorPtr::new(
            self.classes.as_mut().get_unchecked_mut(size.index()) as *mut _,
            size,
        )
    }

    pub unsafe fn alloc(&mut self, size: C, stats: &mut AllocatorStats) -> usize {
        self.class_ptr_mut(size).alloc(stats)
    }

    pub unsafe fn dealloc(&mut self, size: C, id: usize, stats: &mut AllocatorStats) {
        self.class_ptr_mut(size).dealloc(id, stats)
    }

    pub unsafe fn ptr(&self, size: C, id: usize) -> *mut u8 {
        self.class_ptr(size).ptr(id)
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
