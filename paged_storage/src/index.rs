#![allow(missing_docs, dead_code)]
use std::hash::Hash;

use imctk_ids::{id_set_seq::IdSetSeq, Id};
use imctk_util::hash::hash_value;
use table_seq::TableSeq;

use crate::{
    PagedStorage, PagedStorageCatalog, PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
};

pub trait NewLifetime {
    type NewLifetime<'a> where Self: 'a;
}

impl<T> NewLifetime for &T {
    type NewLifetime<'b> = &'b T where Self: 'b;
}

impl<T> NewLifetime for &mut T {
    type NewLifetime<'b> = &'b mut T where Self: 'b;
}

pub trait IndexedTerm<Var: Id>: Eq + Hash {
    fn use_vars(&self) -> impl Iterator<Item = Var> + '_;
    fn max_var(&self) -> Var {
        self.use_vars().fold(Var::MIN_ID, Var::max)
    }
}

pub trait IndexedNode<Var: Id> {
    type Term: IndexedTerm<Var>;
    fn def_var(&self) -> Option<Var>;
    fn term(&self) -> &Self::Term;
}

pub trait IndexedCatalog<Var: Id>: PagedStorageCatalog + Sized {
    type Ref<'a>: PagedStorageItemRef<'a, Self> + IndexedNode<Var>;
}

pub struct NodeIndex<NodeId, Var: Id> {
    defs: IdSetSeq<Var, NodeId>,
    uses: IdSetSeq<Var, NodeId>,
    unique: TableSeq<NodeId>,
}

impl<NodeId, Var: Id> Default for NodeIndex<NodeId, Var> {
    fn default() -> Self {
        Self {
            defs: Default::default(),
            uses: Default::default(),
            unique: Default::default(),
        }
    }
}

impl<NodeId: Id, Var: Id> NodeIndex<NodeId, Var> {
    fn insert<'a, C, R>(&mut self, id: NodeId, item: R, storage: &'a PagedStorage<NodeId, C>)
    where
        C: PagedStorageCatalog,
        R: PagedStorageItemRef<'a, C> + IndexedNode<Var>,
    {
        if let Some(def) = item.def_var() {
            self.defs.grow_for(def).insert(id);
        }
        let term = item.term();
        for var in term.use_vars() {
            self.uses.grow_for(var).insert(id);
        }
        let subtable = term.max_var().id_index();
        self.unique.grow_for_subtable(subtable);
        let result = self.unique.insert(
            subtable,
            hash_value(term),
            id,
            |id, _| storage.get::<R>(*id).term() == term,
            |id| hash_value(storage.get::<R>(*id).term()),
        );
        assert!(result.1.is_none());
    }
    fn remove<'a, C, R>(&mut self, id: NodeId, item: R, storage: &'a PagedStorage<NodeId, C>)
    where
        C: PagedStorageCatalog,
        R: PagedStorageItemRef<'a, C> + IndexedNode<Var>,
    {
        if let Some(def) = item.def_var() {
            self.defs.at_mut(def).remove(&id);
        }
        let term = item.term();
        for var in term.use_vars() {
            self.uses.at_mut(var).remove(&id);
        }
        self.unique.remove(
            term.max_var().id_index(),
            hash_value(term),
            |id1| id == *id1,
            |id| hash_value(storage.get::<R>(*id).term()),
        );
    }
}

pub struct IndexedPagedStorage<NodeId, Var: Id, C: PagedStorageCatalog> {
    storage: PagedStorage<NodeId, C>,
    index: NodeIndex<NodeId, Var>,
}

impl<NodeId: Id, Var: Id, C: IndexedCatalog<Var>> IndexedPagedStorage<NodeId, Var, C> {
    pub fn with_catalog(catalog: C) -> Self {
        Self {
            storage: PagedStorage::with_catalog(catalog),
            index: NodeIndex::default(),
        }
    }
    pub fn insert<T: PagedStorageItem<C>>(&mut self, item: T) -> NodeId {
        let id = self.storage.insert(item);
        let item = self.storage.get::<C::Ref<'_>>(id);
        self.index.insert(id, item, &self.storage);
        id
    }
    pub fn try_get<'a, R: PagedStorageItemRef<'a, C>>(&'a mut self, id: NodeId) -> Option<R> {
        self.storage.try_get(id)
    }
    pub fn try_remove<T: PagedStorageItem<C>>(&mut self, id: NodeId) -> Option<T> {
        let item: C::Ref<'_> = self.storage.try_get(id)?;
        self.index.remove(id, item, &self.storage);
        Some(self.storage.remove(id))
    }
    pub fn try_discard(&mut self, id: NodeId) -> bool {
        let Some(item) = self.storage.try_get::<C::Ref<'_>>(id) else {
            return false;
        };
        self.index.remove(id, item, &self.storage);
        self.storage.discard(id);
        true
    }
    pub fn try_mutate<R, RV>(
        &mut self,
        id: NodeId,
        fun: impl for<'a> FnOnce(<R as NewLifetime>::NewLifetime<'a>) -> RV,
    ) -> Option<RV>
    where
        R: NewLifetime,
        for<'a> R::NewLifetime<'a>: PagedStorageItemMut<'a, C>
    {
        let item: C::Ref<'_> = self.storage.try_get(id)?;
        self.index.remove(id, item, &self.storage);
        let result = self.storage.try_get_mut(id).map(fun);
        let item: C::Ref<'_> = self.storage.try_get(id)?;
        self.index.insert(id, item, &self.storage);
        result
    }
    pub fn indices_by_variant(&self, variant: C::Variant) -> impl Iterator<Item = NodeId> + '_ {
        self.storage.indices_by_variant(variant)
    }
    pub fn iter<'a, R: PagedStorageItemRef<'a, C> + 'a>(
        &'a self,
    ) -> impl Iterator<Item = (NodeId, R)> + 'a {
        self.storage.iter()
    }
    pub fn find_defs(&self, var: Var) -> impl Iterator<Item = NodeId> + '_ {
        self.index.defs.at(var).iter().copied()
    }
    pub fn find_uses(&self, var: Var) -> impl Iterator<Item = NodeId> + '_ {
        self.index.uses.at(var).iter().copied()
    }
    pub fn find_term<'a>(&'a self, term: <C::Ref<'a> as IndexedNode<Var>>::Term) -> Option<NodeId> {
        self.index
            .unique
            .find(term.max_var().id_index(), hash_value(&term), |id| {
                self.storage.get::<C::Ref<'_>>(*id).term() == &term
            })
            .copied()
    }
}

#[cfg(test)]
#[allow(clippy::undocumented_unsafe_blocks, unsafe_op_in_unsafe_fn)]
mod tests {
    use super::*;
    use std::{alloc::Layout, ptr};

    use crate::{PagedStorageCatalog, PagedStorageItem, PagedStorageItemRef};

    #[derive(PartialEq, Eq, Hash, Debug)]
    struct AndTerm(Var, Var);
    #[derive(PartialEq, Eq, Hash, Debug)]
    struct OrTerm(Var, Var);
    #[derive(PartialEq, Eq, Hash, Debug)]
    struct NotTerm(Var);

    type Var = u32;
    #[derive(PartialEq, Eq, Hash)]
    enum ExampleTerm {
        And(AndTerm),
        Or(OrTerm),
        Not(NotTerm),
    }
    struct ExampleItem(Var, ExampleTerm);
    #[derive(PartialEq, Eq, Hash, Debug)]
    enum ExampleTermRef<'a> {
        And(&'a AndTerm),
        Or(&'a OrTerm),
        Not(&'a NotTerm),
    }
    #[derive(Debug)]
    struct ExampleItemRef<'a>(Var, ExampleTermRef<'a>);
    enum ExampleTermMut<'a> {
        And(&'a mut AndTerm),
        Or(&'a mut OrTerm),
        Not(&'a mut NotTerm),
    }
    struct ExampleItemMut<'a>(&'a mut Var, ExampleTermMut<'a>);

    struct ExampleCatalog;

    unsafe impl PagedStorageCatalog for ExampleCatalog {
        type Variant = u32;

        unsafe fn drop_item(&mut self, _variant: Self::Variant, _itemm: *mut u8) {}

        fn item_layout(&self, variant: Self::Variant) -> std::alloc::Layout {
            match variant {
                0 => Layout::new::<(Var, AndTerm)>(),
                1 => Layout::new::<(Var, OrTerm)>(),
                2 => Layout::new::<(Var, NotTerm)>(),
                _ => unreachable!(),
            }
        }
    }

    unsafe impl PagedStorageItem<ExampleCatalog> for ExampleItem {
        fn storage_variant(&self, _catalog: &mut ExampleCatalog) -> u32 {
            match self.1 {
                ExampleTerm::And(_) => 0,
                ExampleTerm::Or(_) => 1,
                ExampleTerm::Not(_) => 2,
            }
        }

        unsafe fn write_to_storage(self, ptr: *mut u8) {
            match self {
                ExampleItem(a, ExampleTerm::And(term)) => {
                    ptr::write(ptr as *mut (Var, AndTerm), (a, term))
                }
                ExampleItem(a, ExampleTerm::Or(term)) => {
                    ptr::write(ptr as *mut (Var, OrTerm), (a, term))
                }
                ExampleItem(a, ExampleTerm::Not(term)) => {
                    ptr::write(ptr as *mut (Var, NotTerm), (a, term))
                }
            }
        }

        unsafe fn read_from_storage(
            ptr: *const u8,
            _catalog: &ExampleCatalog,
            variant: u32,
        ) -> Option<Self> {
            match variant {
                0 => {
                    let (var, term) = ptr::read(ptr as *const (Var, AndTerm));
                    Some(ExampleItem(var, ExampleTerm::And(term)))
                }
                1 => {
                    let (var, term) = ptr::read(ptr as *const (Var, OrTerm));
                    Some(ExampleItem(var, ExampleTerm::Or(term)))
                }
                2 => {
                    let (var, term) = ptr::read(ptr as *const (Var, NotTerm));
                    Some(ExampleItem(var, ExampleTerm::Not(term)))
                }
                _ => None,
            }
        }
    }

    unsafe impl<'a> PagedStorageItemRef<'a, ExampleCatalog> for ExampleItemRef<'a> {
        unsafe fn ref_storage(
            ptr: *const u8,
            _catalog: &ExampleCatalog,
            variant: <ExampleCatalog as PagedStorageCatalog>::Variant,
        ) -> Option<Self> {
            match variant {
                0 => {
                    let item = &*(ptr as *const (Var, AndTerm));
                    Some(ExampleItemRef(item.0, ExampleTermRef::And(&item.1)))
                }
                1 => {
                    let item = &*(ptr as *const (Var, OrTerm));
                    Some(ExampleItemRef(item.0, ExampleTermRef::Or(&item.1)))
                }
                2 => {
                    let item = &*(ptr as *const (Var, NotTerm));
                    Some(ExampleItemRef(item.0, ExampleTermRef::Not(&item.1)))
                }
                _ => None,
            }
        }

        fn possible_storage_variants(
            _catalog: &ExampleCatalog,
        ) -> impl Iterator<Item = <ExampleCatalog as PagedStorageCatalog>::Variant> + '_ {
            0..=2
        }
    }

    unsafe impl<'a> PagedStorageItemMut<'a, ExampleCatalog> for ExampleItemMut<'a> {
        unsafe fn mut_storage(
            ptr: *mut u8,
            _catalog: &ExampleCatalog,
            variant: <ExampleCatalog as PagedStorageCatalog>::Variant,
        ) -> Option<Self> {
            match variant {
                0 => {
                    let (var, term) = &mut *(ptr as *mut (Var, AndTerm));
                    Some(ExampleItemMut(var, ExampleTermMut::And(term)))
                }
                1 => {
                    let (var, term) = &mut *(ptr as *mut (Var, OrTerm));
                    Some(ExampleItemMut(var, ExampleTermMut::Or(term)))
                }
                2 => {
                    let (var, term) = &mut *(ptr as *mut (Var, NotTerm));
                    Some(ExampleItemMut(var, ExampleTermMut::Not(term)))
                }
                _ => None,
            }
        }
    }

    impl<'a> NewLifetime for ExampleItemMut<'a> {
        type NewLifetime<'b> = ExampleItemMut<'b> where 'a: 'b;
    }

    impl<'a> IndexedTerm<Var> for ExampleTermRef<'a> {
        fn use_vars(&self) -> impl Iterator<Item = Var> + '_ {
            match self {
                ExampleTermRef::And(&AndTerm(a, b)) => vec![a, b],
                ExampleTermRef::Or(&OrTerm(a, b)) => vec![a, b],
                ExampleTermRef::Not(&NotTerm(a)) => vec![a],
            }
            .into_iter()
        }
    }

    impl<'a> IndexedNode<Var> for ExampleItemRef<'a> {
        type Term = ExampleTermRef<'a>;

        fn def_var(&self) -> Option<Var> {
            Some(self.0)
        }

        fn term(&self) -> &Self::Term {
            &self.1
        }
    }

    impl IndexedCatalog<Var> for ExampleCatalog {
        type Ref<'a> = ExampleItemRef<'a>;
    }

    unsafe impl<'a> PagedStorageItemMut<'a, ExampleCatalog> for &'a mut AndTerm {
        unsafe fn mut_storage(
            ptr: *mut u8,
            _catalog: &ExampleCatalog,
            variant: <ExampleCatalog as PagedStorageCatalog>::Variant,
        ) -> Option<Self> {
            (variant == 0).then(|| &mut (*(ptr as *mut (Var, AndTerm))).1)
        }
    }

    #[test]
    fn test() {
        let mut storage: IndexedPagedStorage<u32, Var, ExampleCatalog> =
            IndexedPagedStorage::with_catalog(ExampleCatalog);
        let a = storage.insert(ExampleItem(14, ExampleTerm::And(AndTerm(3, 8))));
        storage.insert(ExampleItem(16, ExampleTerm::Or(OrTerm(3, 10))));
        storage.insert(ExampleItem(8, ExampleTerm::Not(NotTerm(4))));
        println!(
            "{:?}",
            storage.try_mutate::<&mut AndTerm, _>(a, |r| r.0 = 7)
        );
        storage.try_mutate::<ExampleItemMut, _>(a, |r| *r.0 = 3);
        println!("{:?}", storage.try_get::<ExampleItemRef>(0));
        println!(
            "{:?}",
            storage.find_term(ExampleTermRef::And(&AndTerm(7, 8)))
        );
        println!("{:?}", storage.find_uses(7).collect::<Vec<_>>());
    }
}
