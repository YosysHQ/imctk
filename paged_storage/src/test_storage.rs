#![allow(missing_docs, dead_code, clippy::undocumented_unsafe_blocks)]
use std::{collections::HashMap, fmt::Debug};

use rand::distributions::Standard;
use rand::prelude::*;
use rand::SeedableRng;

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
enum ExampleItem {
    Single(u32),
    Double([u32; 2]),
}

#[derive(Debug, PartialEq, Eq)]
enum ExampleItemRef<'a> {
    Single(&'a u32),
    Double(&'a [u32; 2]),
}

impl<'a> From<&'a ExampleItem> for ExampleItemRef<'a> {
    fn from(value: &'a ExampleItem) -> Self {
        match value {
            ExampleItem::Single(value) => ExampleItemRef::Single(value),
            ExampleItem::Double(value) => ExampleItemRef::Double(value),
        }
    }
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

unsafe impl<'a> PagedStorageItemRef<'a, ExampleCatalog> for &'a u32 {
    unsafe fn ref_storage(ptr: *const u8, _catalog: &ExampleCatalog, variant: <ExampleCatalog as PagedStorageCatalog>::Variant) -> Option<Self> {
        (variant == 1).then(|| unsafe { &*(ptr as *const u32) })
    }

    fn possible_storage_variants(_catalog: &ExampleCatalog) -> impl Iterator<Item = <ExampleCatalog as PagedStorageCatalog>::Variant> + '_ {
        1..=1
    }
}

impl<'a> TryFrom<&'a ExampleItem> for &'a u32 {
    type Error = ();

    fn try_from(value: &'a ExampleItem) -> Result<Self, Self::Error> {
        match value {
            ExampleItem::Single(value) => Ok(value),
            _ => Err(()),
        }
    }
}

impl Distribution<ExampleItem> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> ExampleItem {
        match rng.gen() {
            false => ExampleItem::Single(rng.gen()),
            true => ExampleItem::Double(rng.gen()),
        }
    }
}

struct CheckedPagedStorage<C: PagedStorageCatalog, I> {
    dut: PagedStorage<u32, C>,
    ref_map: HashMap<u32, I>,
}

impl<C, I> CheckedPagedStorage<C, I>
where
    C: PagedStorageCatalog + Default,
    I: PagedStorageItem<C> + Clone + Debug + Eq,
{
    fn new() -> Self {
        CheckedPagedStorage {
            dut: PagedStorage::default(),
            ref_map: HashMap::new(),
        }
    }
    fn insert(&mut self, node: I) -> u32 {
        let id = self.dut.insert(node.clone());
        assert!(self.ref_map.insert(id, node).is_none());
        id
    }
    fn try_get<'a, R>(&'a self, index: u32) -> Option<R>
    where
        R: PagedStorageItemRef<'a, C> + Eq + Debug + TryFrom<&'a I>,
    {
        let item: Option<R> = self.dut.try_get(index);
        assert_eq!(
            item,
            self.ref_map.get(&index).and_then(|x| x.try_into().ok())
        );
        item
    }
    fn try_remove(&mut self, index: u32) -> Option<I> {
        let item = self.dut.try_remove(index);
        assert_eq!(item, self.ref_map.remove(&index));
        item
    }
    fn try_discard(&mut self, index: u32) -> bool {
        let item = self.dut.try_discard(index);
        assert_eq!(item, self.ref_map.remove(&index).is_some());
        item
    }
    fn check(&mut self) {
        assert_eq!(self.dut.len(), self.ref_map.len());
        assert_eq!(self.dut.is_empty(), self.ref_map.is_empty());
        let mut variants: HashMap<_, HashSet<u32>> = HashMap::new();
        for (id, item) in self.ref_map.iter() {
            variants
                .entry(item.storage_variant(&mut self.dut.catalog))
                .or_default()
                .insert(*id);
        }
        for (variant, set) in variants {
            assert_eq!(self.dut.len_by_variant(variant), set.len());
            assert_eq!(set, self.dut.indices_by_variant(variant).collect());
        }
    }
    fn check_iter<'a, R>(&'a self)
    where
        R: PagedStorageItemRef<'a, C> + 'a + Eq + Debug + TryFrom<&'a I>,
    {
        let dut_set: HashMap<u32, R> = self.dut.iter().collect();
        let ref_set: HashMap<u32, R> = self
            .ref_map
            .iter()
            .filter_map(|(id, value)| value.try_into().ok().map(|r| (*id, r)))
            .collect();
        assert_eq!(dut_set, ref_set);
    }
    /// NB: `random_likelihood` is **not** a probability. `random_likelihood == 2.0` would be 2:1 odds random:present, i.e. 2/3 probability.
    fn present_or_random<R: Rng + SeedableRng>(
        &self,
        random_likelihood: f64,
        rng: &mut R,
    ) -> u32 {
        debug_assert!(random_likelihood >= 0.0);
        if self.ref_map.is_empty() || rng.gen_range(0.0..1.0 + random_likelihood) >= 1.0 {
            rng.gen()
        } else {
            *self.ref_map.keys().choose(rng).unwrap()
        }
    }
}

macro_rules! weighted_choose {
    ($rng:expr, $($name:ident: $weight:expr => $body:expr),+) => {
        {
            enum Branches { $( $name,  )* }
            let weights = [$((Branches::$name, $weight)),+];
            match weights.choose_weighted($rng, |x| x.1).unwrap().0 {
                $(Branches::$name => $body),*
            }
        }
    }
}

#[test]
fn test_storage() {
    let mut storage: CheckedPagedStorage<ExampleCatalog, ExampleItem> = CheckedPagedStorage::new();
    let mut rng = rand_pcg::Pcg64::seed_from_u64(59);
    let verbosity = 1;
    for _ in 0..5000 {
        weighted_choose! { &mut rng,
            Insert: 1.0 => {
                let item: ExampleItem = rng.gen();
                let id = storage.insert(item.clone());
                if verbosity > 0 {
                    println!("inserting {item:?} -> {id}");
                }
            },
            Remove: 1.0 => {
                let id = storage.present_or_random(0.5, &mut rng);
                let item = storage.try_remove(id);
                if verbosity > 0 {
                    println!("removing {id} -> {item:?}");
                }
            },
            Discard: 0.3 => {
                let id = storage.present_or_random(0.5, &mut rng);
                let ok = storage.try_discard(id);
                if verbosity > 0 {
                    println!("discarding {id} -> {ok:?}");
                }
            },
            Get: 1.0 => {
                let id = storage.present_or_random(0.5, &mut rng);
                let item: Option<ExampleItemRef> = storage.try_get(id);
                if verbosity > 0 {
                    println!("getting {id} -> {item:?}");
                }
            },
            CheckIter: 0.3 => {
                storage.check_iter::<ExampleItemRef>();
                if verbosity > 0 {
                    println!("check iter");
                }
            },
            CheckIterSingle: 0.3 => {
                storage.check_iter::<&u32>();
                if verbosity > 0 {
                    println!("check iter u32");
                }
            }
        };
        storage.check();
    }
}
