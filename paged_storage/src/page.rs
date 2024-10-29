// FIXME change all mentions of vtable

use core::fmt;
use std::{
    alloc::Layout,
    marker::PhantomData,
    mem::{ManuallyDrop, MaybeUninit},
    ptr::null_mut,
};

use imctk_util::{give, give_take::Take};

const PAGE_SIZE: usize = 1 << 12;

/// Drop guard to prevent exposing invalid entries on panics.
struct InvalidatePageOnDrop<'a>(&'a mut RawPage);

impl Drop for InvalidatePageOnDrop<'_> {
    fn drop(&mut self) {
        debug_assert!(false, "RawPage callback panic");
        // self.0.buf = RawPage::new(self.0.variant_index, self.0.item_layout);
        self.0.buf = null_mut();
        self.0.cap = 0;
        self.0.len = 0;
        self.0.next = 0;
    }
}

impl InvalidatePageOnDrop<'_> {
    fn defuse(self) {
        let _ = ManuallyDrop::new(self);
    }
}

pub struct RawPage {
    buf: *mut u8,
    cap: u16,
    len: u16,
    next: u16,
    item_layout: Layout,
    item_size: usize,
    on_spare_capacity_list: bool,
}

impl Drop for RawPage {
    fn drop(&mut self) {
        self.resize_buf(0);
    }
}

impl fmt::Debug for RawPage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries((0..self.capacity()).flat_map(|i| self.get(i).map(|entry| (i, entry))))
            .finish()
    }
}

impl RawPage {
    pub fn new(item_layout: Layout) -> Self {
        // We pad the item so that we can store a u16 free list index in an item slot
        let align = ((item_layout.align() - 1) | 0b1) + 1;
        let item_size = item_layout.size();
        let size = item_size.max(align);
        let item_layout = Layout::from_size_align(size, align).unwrap();

        Self {
            buf: null_mut(),
            cap: 0,
            len: 0,
            next: 0,
            item_layout,
            item_size,
            on_spare_capacity_list: false,
        }
    }

    pub fn new_with_capacity(item_layout: Layout, capacity: usize) -> Self {
        let mut new = Self::new(item_layout);
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
        let align = self.item_layout.align();
        let size = self.item_layout.size();

        let data_size = size.checked_mul(cap).unwrap();
        debug_assert_eq!(data_size, self.bitmap_offset_for_capacity(cap));

        let bitmap_size = Self::bitmap_size_for_capacity(cap);

        let size = data_size + bitmap_size;

        Layout::from_size_align(size, align).unwrap()
    }

    #[inline(always)]
    fn bitmap_offset_for_capacity(&self, cap: usize) -> usize {
        self.item_layout.size() * cap
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
        unsafe { self.buf.add(self.item_layout.size() * index) }
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

    #[must_use]
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

        let new_cap = (self.capacity() * 2).clamp(1, PAGE_SIZE);
        assert!(new_cap > self.capacity());

        debug_assert!((0..self.capacity()).all(|i| self.is_present(i)));

        self.resize_buf(new_cap);

        self.initialize_new_free(self.len());
    }

    pub fn get_raw(&self, index: usize) -> Option<*mut u8> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe { self.slot_ptr(index) })
    }

    pub fn get(&self, index: usize) -> Option<&[MaybeUninit<u8>]> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe {
            std::slice::from_raw_parts(self.slot_ptr(index).cast_const().cast(), self.item_size)
        })
    }

    pub unsafe fn cast_get<T>(&self, index: usize) -> Option<&T> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe { &*self.slot_ptr(index).cast_const().cast() })
    }

    pub fn get_mut(&self, index: usize) -> Option<&mut [MaybeUninit<u8>]> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe { std::slice::from_raw_parts_mut(self.slot_ptr(index).cast(), self.item_size) })
    }

    pub unsafe fn cast_get_mut<T>(&mut self, index: usize) -> Option<&mut T> {
        if !self.is_present(index) {
            return None;
        }

        // SAFETY: the `is_present` check rejects out-of-bounds indices as well as empty slots and
        // our vtable matches the node type for all entries in this chunk.
        Some(unsafe { &mut *self.slot_ptr(index).cast() })
    }

    pub unsafe fn take_with_raw<R>(
        &mut self,
        index: usize,
        take_fn: impl FnOnce(*mut u8) -> R,
    ) -> Option<R> {
        if !self.take_present(index) {
            return None;
        }

        // SAFETY: the `take_present` check rejects out-of-bounds indices
        let ptr = unsafe { self.slot_ptr(index) };

        let result;
        {
            let guard = InvalidatePageOnDrop(self);
            result = take_fn(ptr);
            guard.defuse();
        }

        // SAFETY: the next field of the union slot is a u16
        unsafe { (ptr as *mut u16).write(self.next) };

        self.next = index as u16;

        self.len -= 1;

        Some(result)
    }

    pub fn take_with<R>(
        &mut self,
        index: usize,
        take_fn: impl FnOnce(&mut [MaybeUninit<u8>]) -> R,
    ) -> Option<R> {
        unsafe {
            let size = self.item_size;
            self.take_with_raw(index, |ptr| {
                take_fn(std::slice::from_raw_parts_mut(ptr.cast(), size))
            })
        }
    }

    pub unsafe fn cast_take_with<T, R>(
        &mut self,
        index: usize,
        take_fn: impl FnOnce(Take<T>) -> R,
    ) -> Option<R> {
        unsafe { self.take_with_raw(index, |ptr| take_fn(Take::from_raw_ptr(ptr.cast()))) }
    }

    pub fn insert_raw(&mut self, insert_fn: impl FnOnce(*mut u8)) -> (usize, *mut u8) {
        let index = self.next as usize;

        debug_assert!(!self.is_present(index));

        if self.next == self.cap {
            self.grow(); // Ensures that `self.next` becomes a valid index of a free slot or panics
        }

        // SAFETY: `self.next` always is either a valid index of a free slot or `self.cap` and we
        // already called `grow` to reduce the second case to the first.
        let ptr = unsafe { self.slot_ptr(index) };

        // SAFETY: this reads the `_next` field of the slot union
        self.next = unsafe { (ptr as *mut u16).read() };
        self.len += 1;

        debug_assert!(self.len as usize <= PAGE_SIZE);

        {
            let guard = InvalidatePageOnDrop(self);
            insert_fn(ptr);
            guard.defuse();
        }

        // SAFETY: we know index is in bounds for the slots, making `index / 8` in bounds for the
        // bitmap
        unsafe {
            *self.bitmap_mut().get_unchecked_mut(index / 8) |= 1 << (index % 8);
        }

        // SAFETY: we own the buffer holding the item we just inserted so we can safely return a
        // mutable reference to it with our lifetime
        (index, ptr)
    }

    pub fn insert(
        &mut self,
        insert_fn: impl FnOnce(&mut [MaybeUninit<u8>]),
    ) -> (usize, &mut [MaybeUninit<u8>]) {
        unsafe {
            let size = self.item_size;
            let (index, ptr) = self.insert_raw(|target| {
                insert_fn(std::slice::from_raw_parts_mut(target.cast(), size))
            });

            (index, std::slice::from_raw_parts_mut(ptr.cast(), size))
        }
    }

    pub unsafe fn cast_insert_give<T>(&mut self, value: Take<T>) -> (usize, &mut T) {
        unsafe {
            let (index, ptr) =
                self.insert_raw(|target| target.cast::<T>().write(value.into_raw_ptr().read()));

            (index, &mut *ptr.cast())
        }
    }
}

#[repr(transparent)]
pub struct TypedPage<T> {
    raw: RawPage,
    _phantom: PhantomData<T>,
}

impl<T> std::ops::Deref for TypedPage<T> {
    type Target = RawPage;

    fn deref(&self) -> &Self::Target {
        &self.raw
    }
}

impl<T: fmt::Debug> fmt::Debug for TypedPage<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries((0..self.capacity()).flat_map(|i| self.get(i).map(|entry| (i, entry))))
            .finish()
    }
}

impl<T> Drop for TypedPage<T> {
    fn drop(&mut self) {
        for i in 0..self.capacity() {
            self.remove(i);
        }
    }
}

impl<T> TypedPage<T> {
    unsafe fn from_raw_ref(raw: &RawPage) -> &Self {
        unsafe { &*(raw as *const RawPage).cast() }
    }

    unsafe fn from_raw_mut(raw: &mut RawPage) -> &mut Self {
        unsafe { &mut *(raw as *mut RawPage).cast() }
    }

    unsafe fn from_raw(raw: RawPage) -> Self {
        Self {
            raw,
            _phantom: PhantomData,
        }
    }

    pub fn as_raw(&self) -> &RawPage {
        &self.raw
    }

    pub unsafe fn as_raw_mut(&mut self) -> &mut RawPage {
        &mut self.raw
    }

    pub fn into_raw(self) -> RawPage {
        unsafe { std::mem::transmute(self) }
    }

    fn new() -> Self {
        Self {
            raw: RawPage::new(Layout::new::<T>()),
            _phantom: PhantomData,
        }
    }

    fn new_with_capacity(capacity: usize) -> Self {
        Self {
            raw: RawPage::new_with_capacity(Layout::new::<T>(), capacity),
            _phantom: PhantomData,
        }
    }

    pub fn get(&self, index: usize) -> Option<&T> {
        unsafe { self.raw.cast_get(index) }
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut T> {
        unsafe { self.raw.cast_get_mut(index) }
    }

    pub fn take_with<R>(&mut self, index: usize, take_fn: impl FnOnce(Take<T>) -> R) -> Option<R> {
        unsafe { self.raw.cast_take_with(index, take_fn) }
    }

    pub fn insert_give(&mut self, value: Take<T>) -> (usize, &mut T) {
        unsafe { self.raw.cast_insert_give(value) }
    }

    pub fn insert(&mut self, value: T) -> (usize, &mut T) {
        give!(value = value);
        self.insert_give(value)
    }

    pub fn remove(&mut self, index: usize) -> Option<T> {
        self.take_with(index, |v| v.take())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_page() {
        let mut page = <TypedPage<Box<u8>>>::new();
        let v0 = page.insert(0.into()).0;
        let v1 = page.insert(1.into()).0;
        let v2 = page.insert(2.into()).0;

        assert_eq!(page.remove(v0), Some(0.into()));
        assert_eq!(page.remove(v0), None);

        assert_eq!(page.remove(v2), Some(2.into()));
        assert_eq!(page.remove(v2), None);

        assert_eq!(page.remove(v1), Some(1.into()));
        assert_eq!(page.remove(v1), None);

        let v0 = page.insert(0.into()).0;
        let v1 = page.insert(1.into()).0;
        let v2 = page.insert(2.into()).0;

        assert_eq!(page.remove(v0), Some(0.into()));
        assert_eq!(page.remove(v0), None);

        assert_eq!(page.remove(v2), Some(2.into()));
        assert_eq!(page.remove(v2), None);

        assert_eq!(page.remove(v1), Some(1.into()));
        assert_eq!(page.remove(v1), None);
    }
}
