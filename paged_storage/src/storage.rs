//! A paged storage stores nodes of different types efficiently by packing nodes of the same type into pages.
// TODO: more detail

use std::{alloc::Layout, collections::HashSet, marker::PhantomData};

use imctk_ids::{id_vec::IdVec, Id};

use crate::page::{RawPage, PAGE_BITS, PAGE_SIZE};

/// A catalog represents a set of node variants.
///
/// When this set is statically known, the catalog would be a zero-sized type, but dynamically allocated variants are supported, too.
///
/// # Safety
/// - Items of a variant fit within the layout returned by `item_layout(catalog, variant)`.
pub unsafe trait PagedStorageCatalog {
    /// `Id` type that represents variants.
    type Variant: Id;

    /// Drops the item of the given variant at the given pointer.
    ///
    /// # Safety
    /// - The pointer must be valid and point to an initialized item of the given variant.
    unsafe fn drop_item(&mut self, variant: Self::Variant, item: *mut u8);

    /// Returns the memory layout used to store items of the given variant.
    fn item_layout(&self, variant: Self::Variant) -> Layout;
}

/// An owned item that can be stored in paged storage.
///
/// # Safety
/// The following sequence does not cause UB for any `value`:
///
/// 1. `variant = value.storage_variant(catalog)`
/// 2. `ptr` is allocated with layout `catalog.item_layout(variant)`
/// 3. `value.write_to_storage(ptr)`
/// 4. Any sequence of the following, in any order:
///
///     - `T::ref_storage(ptr, catalog, variant)` for any `T: PagedStorageItemRef<C>`, and any safe operations on the result of that.
///     - `T::mut_storage(ptr, catalog, variant)` for any `T: PagedStorageItemMut<C>`, and any safe operations on the result of that.
///     - `T::read_from_storage(ptr, catalog, variant)` for any `T: PagedStorageItem<C>` **if it returns `None`**
///
/// 5. One of the following:
///     
///     - `T::read_from_storage(ptr, catalog, variant)` for any `T: PagedStorageItem<C>`
///     - `catalog.drop_item(variant, ptr)`
///
/// Further invariants:
/// - If `T::read_from_storage(ptr, catalog, variant)` is `Some(x)` then `x.storage_variant(catalog) == variant`.
pub unsafe trait PagedStorageItem<C: PagedStorageCatalog>: Sized {
    /// Returns the variant ID for the given value.
    ///
    /// May allocate a new variant ID dynamically.
    fn storage_variant(&self, catalog: &mut C) -> C::Variant;

    /// Moves the given value into storage at the given pointer.
    ///
    /// This initializes the memory at `ptr`.
    ///
    /// # Safety
    /// - The pointer must be valid and point to uninitialized memory of appropriate layout.
    unsafe fn write_to_storage(self, ptr: *mut u8);

    /// Moves a value out of storage at the given pointer.
    ///
    /// If the specified variant is not compatible with `Self`, then `None` is returned.
    ///
    /// If a value is returned, the memory at `ptr` is considered uninitialized thereafter.
    ///
    /// # Safety
    /// - The pointer must be valid and point to an initialized item of the given variant.
    unsafe fn read_from_storage(ptr: *const u8, catalog: &C, variant: C::Variant) -> Option<Self>;
}

/// An immutable reference to an item in paged storage.
///
/// This is explicitly allowed to be simply a copy of the data.
/// An `enum` of references is another valid implementation.
///
/// # Safety
/// The invariants in the `PagedStorageItem` documentation are upheld.
pub unsafe trait PagedStorageItemRef<'storage, C: PagedStorageCatalog>: Sized {
    /// Creates a reference to or copy of the node at `ptr` of the given variant.
    /// 
    /// Returns `None` if either the variant or the data at `ptr` is not valid for this type.
    ///
    /// # Safety
    /// - The pointer must be valid and point to an initialized item of the given variant.
    unsafe fn ref_storage(ptr: *const u8, catalog: &C, variant: C::Variant) -> Option<Self>;

    /// Returns all storage variants that may contain values compatible with this type.
    ///
    /// Strictly speaking, this returns an upper bound to the set of compatible variants that have been stored into the paged storage.
    ///
    /// Two specific cases that are permitted by this definition:
    ///
    /// 1. For simplicity, returning `a..=b`, even though not every value in that range can be used with this type.
    /// 2. Dynamically allocated variants, where the full set is impossible to determine.
    fn possible_storage_variants(catalog: &C) -> impl Iterator<Item = C::Variant> + '_;
}

/// A mutable reference to an item in paged storage.
///
/// Unlike `PagedStorageItemRef`, this must be an actual reference, and not a copy, except for immutable portions of the data.
/// For example, an `enum` of mutable references corresponding to different variant IDs is acceptable, since it's not legal to change the variant ID.
///
/// # Safety
/// The invariants in the `PagedStorageItem` documentation are upheld.
pub unsafe trait PagedStorageItemMut<'storage, C: PagedStorageCatalog>: Sized {
    /// Creates a mutable reference to the node at `ptr` of the given variant.
    ///
    /// Returns `None` if either the variant or the data at `ptr` is not valid for this type.
    ///
    /// # Safety
    /// - The pointer must be valid and point to an initialized item of the given variant.
    unsafe fn mut_storage(ptr: *mut u8, catalog: &C, variant: C::Variant) -> Option<Self>;
}

/// This trait is a promise that all values of this type have the same variant.
/// 
/// # Safety
/// - If `PagedStorageItem` is also implemented, then `PagedStorageItem::storage_variant(value, catalog)` always returns `VARIANT`.
pub unsafe trait PagedStorageVariant<C: PagedStorageCatalog> {
    /// The variant of values of this type.
    const VARIANT: C::Variant;
}

struct StoragePage<C: PagedStorageCatalog> {
    variant: C::Variant,
    page: RawPage,
}

struct VariantPages {
    active_page: u32,
    len: usize,
    pages: HashSet<u32>, // TODO change to StableSet
    pages_with_spare_capacity: Vec<u32>,
}

/// A paged storage stores nodes of different types efficiently by packing nodes of the same type into pages.
// TODO: more detail
pub struct PagedStorage<ID, C: PagedStorageCatalog> {
    catalog: C,
    pages: Vec<StoragePage<C>>,
    variant_index: IdVec<C::Variant, VariantPages>,
    len: usize,
    _phantom_data: PhantomData<ID>,
}

impl<ID, T: PagedStorageCatalog> Drop for PagedStorage<ID, T> {
    fn drop(&mut self) {
        for page in self.pages.iter_mut() {
            let variant = page.variant;
            for i in 0..page.page.capacity() {
                // SAFETY: take_with is guaranteed to give us only valid pointers
                unsafe {
                    page.page.take_with(i, |item| {
                        self.catalog.drop_item(variant, item);
                    });
                }
            }
        }
    }
}

impl VariantPages {
    fn new(first_page: u32) -> Self {
        VariantPages {
            active_page: first_page,
            len: 0,
            pages: [first_page].into(),
            pages_with_spare_capacity: vec![],
        }
    }
}

impl<ID: Id, C: PagedStorageCatalog + Default> Default for PagedStorage<ID, C> {
    fn default() -> Self {
        Self::with_catalog(C::default())
    }
}

impl<ID: Id, C: PagedStorageCatalog> PagedStorage<ID, C> {
    /// Creates a new empty paged storage with the given catalog.
    pub fn with_catalog(catalog: C) -> Self {
        Self {
            catalog,
            pages: Vec::new(),
            variant_index: IdVec::default(),
            len: 0,
            _phantom_data: PhantomData,
        }
    }
    fn make_index(page_id: usize, slot_id: usize) -> ID {
        ID::from_id_index(page_id << PAGE_BITS | slot_id)
    }
    fn split_index(index: ID) -> (usize, usize) {
        let id = index.id_index();
        let page_id = id >> PAGE_BITS;
        let slot_id = id & ((1 << PAGE_BITS) - 1);
        (page_id, slot_id)
    }
    fn alloc_page(catalog: &C, pages: &mut Vec<StoragePage<C>>, variant: C::Variant) -> u32 {
        let id = pages.len() as u32;
        let page = RawPage::new(catalog.item_layout(variant));
        pages.push(StoragePage { variant, page });
        id
    }
    fn variant_pages(&mut self, variant: C::Variant) -> &mut VariantPages {
        self.variant_index.grow_for_key_with(variant, || {
            VariantPages::new(Self::alloc_page(&self.catalog, &mut self.pages, variant))
        })
    }
    fn did_insertion(&mut self, variant: C::Variant) {
        let variant_page = &mut self.variant_index[variant];
        let page = &mut self.pages[variant_page.active_page as usize].page;
        if page.len() == PAGE_SIZE {
            variant_page.active_page = variant_page
                .pages_with_spare_capacity
                .pop()
                .unwrap_or_else(|| Self::alloc_page(&self.catalog, &mut self.pages, variant));
        }
        variant_page.len += 1;
        self.len += 1;
    }
    fn did_removal<R>(
        &mut self,
        variant: C::Variant,
        page_id: usize,
        result: Option<R>,
    ) -> Option<R> {
        if result.is_some() {
            let variant_page = &mut self.variant_index[variant];
            let page = &mut self.pages[page_id].page;
            if page.len() == PAGE_SIZE - 1 {
                variant_page.pages_with_spare_capacity.push(page_id as u32);
            }
            variant_page.len -= 1;
            self.len -= 1;
        }
        result
    }
    /// Inserts the given node into the paged storage and returns its ID.
    pub fn insert<T: PagedStorageItem<C>>(&mut self, node: T) -> ID {
        let variant = node.storage_variant(&mut self.catalog);
        let page_id = self.variant_pages(variant).active_page as usize;
        let page = &mut self.pages[page_id].page;
        // SAFETY: insert_raw is guaranteed to give us a valid pointer.
        let (slot_id, _) = unsafe { page.insert(|ptr| node.write_to_storage(ptr)) };
        self.did_insertion(variant);
        Self::make_index(page_id, slot_id)
    }
    /// Tries to access the node with the given ID as type `T`.
    ///
    /// Depending on the type `T`, the result is either a reference into the node or a copy of the node.
    ///
    /// Returns `None` if the ID does not refer to a node or type `T` is not compatible with that node.
    pub fn try_get<'a, T: PagedStorageItemRef<'a, C>>(&'a self, index: ID) -> Option<T> {
        let (page_id, slot_id) = Self::split_index(index);
        let StoragePage { variant, page } = self.pages.get(page_id)?;
        let ptr = page.get(slot_id)?;
        // SAFETY: If it succeeds, get is guaranteed to give us a valid pointer.
        unsafe { T::ref_storage(ptr, &self.catalog, *variant) }
    }
    /// Accesses the node with the given ID as type `T`.
    ///
    /// Depending on the type `T`, the result is either a reference into the node or a copy of the node.
    ///
    /// Panics if the ID does not refer to a node or type `T` is not compatible with that node.
    pub fn get<'a, T: PagedStorageItemRef<'a, C>>(&'a self, index: ID) -> T {
        self.try_get(index).unwrap()
    }
    /// Tries to create a mutable reference of type `T` to the node with the given ID.
    ///
    /// Returns `None` if the ID does not refer to a node or type `T` is not compatible with that node.
    pub fn try_get_mut<'a, T: PagedStorageItemMut<'a, C>>(&'a mut self, index: ID) -> Option<T> {
        let (page_id, slot_id) = Self::split_index(index);
        let StoragePage { variant, page } = self.pages.get(page_id)?;
        let ptr = page.get(slot_id)?;
        // SAFETY: If it succeeds, get is guaranteed to give us a valid pointer.
        unsafe { T::mut_storage(ptr, &self.catalog, *variant) }
    }
    /// Creates a mutable reference of type `T` to the node with the given ID.
    ///
    /// Panics if the ID does not refer to a node or type `T` is not compatible with that node.
    pub fn get_mut<'a, T: PagedStorageItemMut<'a, C>>(&'a mut self, index: ID) -> T {
        self.try_get_mut(index).unwrap()
    }
    /// Tries to discard the node with the given ID. Returns `true` if it succeeds.
    ///
    /// Any previous content of the node is dropped.
    ///
    /// Returns `false` if the ID does not refer to a node.
    pub fn try_discard(&mut self, index: ID) -> bool {
        let (page_id, slot_id) = Self::split_index(index);
        if let Some(StoragePage { variant, page }) = self.pages.get_mut(page_id) {
            let variant = *variant;
            // SAFETY: take_with is guaranteed to give us a valid pointer and will then mark the slot as free.
            let result =
                unsafe { page.take_with(slot_id, |ptr| self.catalog.drop_item(variant, ptr)) };
            self.did_removal(variant, page_id, result).is_some()
        } else {
            false
        }
    }
    /// Discards the node with the given ID.
    ///
    /// The previous content of the node is dropped.
    ///
    /// Panics if the ID does not refer to a node.
    pub fn discard(&mut self, index: ID) {
        assert!(self.try_discard(index));
    }
    /// Tries to remove and return the node with the given ID as type `T`.
    ///
    /// Returns `None` if the ID does not refer to a node or type `T` is not compatible with that node.
    /// No nodes are removed in this case.
    pub fn try_remove<T: PagedStorageItem<C>>(&mut self, index: ID) -> Option<T> {
        let (page_id, slot_id) = Self::split_index(index);
        let StoragePage { variant, page } = self.pages.get_mut(page_id)?;
        let variant = *variant;
        // SAFETY: maybe_take_with is guaranteed to give us a valid pointer and, if we succeed, will then mark the slot as free.
        let result = unsafe {
            page.maybe_take_with(slot_id, |ptr| {
                T::read_from_storage(ptr, &self.catalog, variant)
            })
        };
        self.did_removal(variant, page_id, result)
    }
    /// Removes and returns the node with the given ID as type `T`.
    ///
    /// Panics if the ID does not refer to a node or type `T` is not compatible with that node.
    pub fn remove<T: PagedStorageItem<C>>(&mut self, index: ID) -> T {
        self.try_remove(index).unwrap()
    }
    /// Returns the number of nodes in the paged storage.
    pub fn len(&self) -> usize {
        self.len
    }
    /// Returns `true` if there are no nodes in the paged storage.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }
    /// Returns the number of nodes with the given variant in the paged storage.
    pub fn len_by_variant(&self, variant: C::Variant) -> usize {
        self.variant_index
            .get(variant)
            .map(|page| page.len)
            .unwrap_or(0)
    }
    fn pages_by_variant(
        &self,
        variant: C::Variant,
    ) -> impl Iterator<Item = (usize, &RawPage)> + '_ {
        self.variant_index
            .get(variant)
            .into_iter()
            .flat_map(|variant_pages| {
                variant_pages
                    .pages
                    .iter()
                    .map(|page_id| (*page_id as usize, &self.pages[*page_id as usize].page))
            })
    }
    /// Returns an iterator yielding all IDs that refer to nodes of the given variant.
    pub fn indices_by_variant(&self, variant: C::Variant) -> impl Iterator<Item = ID> + '_ {
        self.pages_by_variant(variant).flat_map(|(page_id, page)| {
            page.iter()
                .map(move |(slot_id, _)| Self::make_index(page_id, slot_id))
        })
    }
    /// Returns an iterator yielding the ID and a reference for every node that is compatible with type `T`.
    pub fn iter<'a, T: PagedStorageItemRef<'a, C> + 'a>(
        &'a self,
    ) -> impl Iterator<Item = (ID, T)> + 'a {
        T::possible_storage_variants(&self.catalog).flat_map(move |variant| {
            self.pages_by_variant(variant)
                .flat_map(move |(page_id, page)| {
                    page.iter().flat_map(move |(slot_id, ptr)| {
                        // SAFETY: iter() is guaranteed to give us a valid pointer.
                        unsafe { T::ref_storage(ptr, &self.catalog, variant) }
                            .map(|r| (Self::make_index(page_id, slot_id), r))
                    })
                })
        })
    }
}

#[cfg(test)]
#[path = "test_storage.rs"]
mod test_storage;