#![allow(missing_docs, dead_code)] // FIXME prototyping

mod page;

mod storage {
    use imctk_ids::{id_vec::IdVec, indexed_id_vec::IndexedIdVec};

    use crate::page::RawPage;

    pub unsafe trait PagedStorageCatalog {
        unsafe fn drop_item(&mut self, variant: u32, item: *mut u8);
    }

    pub trait PagedStorageItem<C: PagedStorageCatalog> {
        fn variant(&self, catalog: &mut C) -> u32;
    }

    struct StoragePage {
        variant: u32,
        page: RawPage,
    }

    struct VariantPages {
        active_page: u32,
        len: usize,
        pages: IndexedIdVec<u32, u32>,
        pages_with_spare_capacity: Vec<u32>,
    }

    pub struct PagedStorage<T: PagedStorageCatalog> {
        catalog: T,
        pages: Vec<StoragePage>,
        variant_index: IdVec<u32, VariantPages>,
        len: usize,
    }

    impl<T: PagedStorageCatalog> Drop for PagedStorage<T> {
        fn drop(&mut self) {
            for page in self.pages.iter_mut() {
                let variant = page.variant;
                for i in 0..page.page.capacity() {
                    unsafe {
                        page.page.take_with_raw(i, |item| {
                            self.catalog.drop_item(variant, item);
                        });
                    }
                }
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        enum ExampleItem {
            Single(u32),
            Double([u32; 2]),
        }

        #[test]
        fn test_storage() {}
    }
}
