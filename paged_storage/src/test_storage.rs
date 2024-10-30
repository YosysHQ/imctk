#![allow(missing_docs, dead_code, clippy::undocumented_unsafe_blocks)]
use super::*;

#[derive(Debug)]
enum ExampleItem {
    Single(u32),
    Double([u32; 2]),
}

#[derive(Debug)]
enum ExampleItemRef<'a> {
    Single(&'a u32),
    Double(&'a [u32; 2]),
}

#[derive(Default)]
struct ExampleCatalog;

unsafe impl PagedStorageCatalog for ExampleCatalog {
    type Variant = u32;

    unsafe fn drop_item(&mut self, variant: u32, item: *mut u8) {
        unsafe {
            let _ = ExampleItem::read_from_storage(item, self, variant);
        }
    }

    fn item_layout(&self, variant: u32) -> Layout {
        match variant {
            1 => Layout::new::<u32>(),
            2 => Layout::new::<[u32; 2]>(),
            _ => unreachable!(),
        }
    }
}

unsafe impl PagedStorageItem<ExampleCatalog> for ExampleItem {
    fn storage_variant(&self, _catalog: &mut ExampleCatalog) -> u32 {
        match self {
            ExampleItem::Single(_) => 1,
            ExampleItem::Double(_) => 2,
        }
    }

    unsafe fn write_to_storage(self, ptr: *mut u8) {
        unsafe {
            match self {
                ExampleItem::Single(value) => std::ptr::write(ptr as *mut u32, value),
                ExampleItem::Double(values) => std::ptr::write(ptr as *mut [u32; 2], values),
            }
        }
    }

    unsafe fn read_from_storage(
        ptr: *const u8,
        _catalog: &ExampleCatalog,
        variant: u32,
    ) -> Option<Self> {
        unsafe {
            match variant {
                1 => Some(ExampleItem::Single(std::ptr::read(ptr as *mut u32))),
                2 => Some(ExampleItem::Double(std::ptr::read(ptr as *mut [u32; 2]))),
                _ => None,
            }
        }
    }
}

unsafe impl<'a> PagedStorageItemRef<'a, ExampleCatalog> for ExampleItem {
    unsafe fn ref_storage(ptr: *const u8, _catalog: &ExampleCatalog, variant: u32) -> Option<Self> {
        unsafe {
            match variant {
                1 => Some(ExampleItem::Single(*(ptr as *const u32))),
                2 => Some(ExampleItem::Double(*(ptr as *const [u32; 2]))),
                _ => None,
            }
        }
    }

    fn possible_storage_variants(
        _catalog: &ExampleCatalog,
    ) -> impl Iterator<Item = <ExampleCatalog as PagedStorageCatalog>::Variant> + '_ {
        1..=2
    }
}

unsafe impl<'a> PagedStorageItemRef<'a, ExampleCatalog> for ExampleItemRef<'a> {
    unsafe fn ref_storage(ptr: *const u8, _catalog: &ExampleCatalog, variant: u32) -> Option<Self> {
        unsafe {
            match variant {
                1 => Some(ExampleItemRef::Single(&*(ptr as *const u32))),
                2 => Some(ExampleItemRef::Double(&*(ptr as *const [u32; 2]))),
                _ => None,
            }
        }
    }

    fn possible_storage_variants(
        _catalog: &ExampleCatalog,
    ) -> impl Iterator<Item = <ExampleCatalog as PagedStorageCatalog>::Variant> + '_ {
        1..=2
    }
}

unsafe impl PagedStorageItem<ExampleCatalog> for u32 {
    fn storage_variant(&self, _catalog: &mut ExampleCatalog) -> u32 {
        1
    }

    unsafe fn write_to_storage(self, ptr: *mut u8) {
        unsafe { std::ptr::write(ptr as *mut u32, self) };
    }

    unsafe fn read_from_storage(
        ptr: *const u8,
        _catalog: &ExampleCatalog,
        variant: u32,
    ) -> Option<Self> {
        (variant == 1).then(|| unsafe { std::ptr::read(ptr as *mut u32) })
    }
}

#[test]
fn test_storage() {
    let mut storage: PagedStorage<u32, ExampleCatalog> = PagedStorage::default();
    let _a = storage.insert(ExampleItem::Single(13));
    let b = storage.insert(ExampleItem::Single(39));
    let _c = storage.insert(ExampleItem::Double([25, 39]));
    for i in storage.indices_by_variant(1) {
        println!("{i} {:?}", storage.get::<ExampleItemRef>(i));
    }
    for i in storage.indices_by_variant(2) {
        println!("{i} {:?}", storage.get::<ExampleItemRef>(i));
    }
    for (i, r) in storage.iter::<ExampleItemRef>() {
        println!("{i} {r:?}");
    }
    println!("{:?}", storage.try_remove::<ExampleItem>(b));
}
