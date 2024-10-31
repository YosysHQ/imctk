//! A paged storage stores nodes of different types efficiently by packing nodes of the same type into pages.
// TODO: more detail

mod page;

pub mod storage;

pub use storage::{
    PagedStorage, PagedStorageCatalog, PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
    PagedStorageVariant,
};
