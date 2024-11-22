#![allow(missing_docs, dead_code)]
use std::hash::Hash;

use imctk_ids::{id_set_seq::IdSetSeq, Id};
use imctk_union_find::Element;
use imctk_util::hash::hash_value;
use table_seq::TableSeq;

use crate::{
    PagedStorage, PagedStorageCatalog, PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
};

pub trait IndexedTermRef<C: IndexedCatalog>: Eq + Hash {
    fn use_vars(&self) -> impl Iterator<Item = C::Var> + '_;
    fn nonguarding_vars(&self) -> impl Iterator<Item = C::Var> + '_ {
        self.use_vars()
    }
    fn max_var(&self) -> C::Var {
        self.use_vars().fold(C::Var::MIN_ID, C::Var::max)
    }
    fn map(&self, fun: impl FnMut(C::Lit) -> C::Lit) -> C::Term;
}

pub trait IndexedNodeRef<C: IndexedCatalog> {
    fn output(&self) -> C::Lit;
    fn term(&self) -> C::TermRef<'_>;
    fn map(&self, mut fun: impl FnMut(C::Lit) -> C::Lit) -> C::Node {
        let new_output = fun(self.output());
        let new_term = self.term().map(fun);
        C::Node::new(new_output, new_term)
    }
}

pub trait IndexedNode<C: IndexedCatalog>: IndexedNodeRef<C> {
    fn new(output: C::Lit, term: C::Term) -> Self;
}

pub trait IndexedNodeMut<C: IndexedCatalog> {
    fn rewrite(&mut self, fun: impl FnMut(C::Lit) -> C::Lit);
}

pub trait IndexedNodeMutFamily<C: IndexedCatalog> {
    type Instantiate<'a>: PagedStorageItemMut<'a, C> + IndexedNodeMut<C>
    where
        Self: 'a;
}

impl<C: IndexedCatalog, T> IndexedNodeMutFamily<C> for &mut T
where
    for<'a> &'a mut T: PagedStorageItemMut<'a, C> + IndexedNodeMut<C>,
{
    type Instantiate<'a>
        = &'a mut T
    where
        Self: 'a;
}

pub trait IndexedCatalog: PagedStorageCatalog + Sized {
    type Var: Id;
    type Lit: Id + Element<Atom = Self::Var>;
    type NodeId: Id;
    type Node: 'static + PagedStorageItem<Self> + IndexedNode<Self>;
    type NodeRef<'a>: PagedStorageItemRef<'a, Self> + IndexedNodeRef<Self> + std::fmt::Debug;
    type NodeMut: IndexedNodeMutFamily<Self>;
    type Term: 'static;
    type TermRef<'a>: IndexedTermRef<Self> + From<&'a Self::Term>;
    fn term_eq(a: &Self::TermRef<'_>, b: &Self::TermRef<'_>) -> bool;
}

pub struct NodeIndex<C: IndexedCatalog> {
    defs: IdSetSeq<C::Var, C::NodeId>,
    uses: IdSetSeq<C::Var, C::NodeId>,
    unique: TableSeq<C::NodeId>,
}

impl<C: IndexedCatalog> Default for NodeIndex<C> {
    fn default() -> Self {
        Self {
            defs: Default::default(),
            uses: Default::default(),
            unique: Default::default(),
        }
    }
}

impl<C: IndexedCatalog> NodeIndex<C> {
    fn insert<'a, R>(
        &mut self,
        id: C::NodeId,
        item: R,
        storage: &'a PagedStorage<C::NodeId, C>,
    ) -> Option<C::NodeId>
    where
        R: PagedStorageItemRef<'a, C> + IndexedNodeRef<C>,
    {
        let term = item.term();
        let subtable = term.max_var().id_index();
        self.unique.grow_for_subtable(subtable);
        let result = self.unique.insert(
            subtable,
            hash_value(&term),
            id,
            |id, _| C::term_eq(&storage.get::<R>(*id).term(), &term),
            |id| hash_value(storage.get::<R>(*id).term()),
        );
        self.defs.grow_for(item.output().atom()).insert(id);
        for var in term.use_vars() {
            self.uses.grow_for(var).insert(id);
        }
        if result.1.is_some() {
            Some(*result.0)
        } else {
            None
        }
    }
    fn remove<'a, R>(&mut self, id: C::NodeId, item: R, storage: &'a PagedStorage<C::NodeId, C>)
    where
        R: PagedStorageItemRef<'a, C> + IndexedNodeRef<C>,
    {
        self.defs.at_mut(item.output().atom()).remove(&id);
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

pub struct IndexedPagedStorage<C: IndexedCatalog> {
    storage: PagedStorage<C::NodeId, C>,
    index: NodeIndex<C>,
}

#[derive(Debug, Clone)]
pub enum MutateResult<NodeId> {
    Ok,
    NotFound,
    TypeError,
    Equivalent(NodeId),
}

impl<C: IndexedCatalog> IndexedPagedStorage<C> {
    pub fn with_catalog(catalog: C) -> Self {
        Self {
            storage: PagedStorage::with_catalog(catalog),
            index: NodeIndex::default(),
        }
    }
    pub fn insert<T: PagedStorageItem<C>>(&mut self, item: T) -> C::NodeId {
        let id = self.storage.insert(item);
        let item = self.storage.get::<C::NodeRef<'_>>(id);
        self.index.insert(id, item, &self.storage);
        id
    }
    pub fn try_get<'a, R: PagedStorageItemRef<'a, C>>(&'a self, id: C::NodeId) -> Option<R> {
        self.storage.try_get(id)
    }
    pub fn get<'a, R: PagedStorageItemRef<'a, C>>(&'a self, id: C::NodeId) -> R {
        self.storage.get(id)
    }
    pub fn try_remove<T: PagedStorageItem<C>>(&mut self, id: C::NodeId) -> Option<T> {
        let item: C::NodeRef<'_> = self.storage.try_get(id)?;
        self.index.remove(id, item, &self.storage);
        Some(self.storage.remove(id))
    }
    pub fn try_discard(&mut self, id: C::NodeId) -> bool {
        let Some(item) = self.storage.try_get::<C::NodeRef<'_>>(id) else {
            return false;
        };
        self.index.remove(id, item, &self.storage);
        self.storage.discard(id);
        true
    }
    pub fn discard(&mut self, id: C::NodeId) {
        assert!(self.try_discard(id));
    }
    pub fn try_mutate<R: IndexedNodeMutFamily<C>>(
        &mut self,
        id: C::NodeId,
        fun: impl for<'a> FnOnce(R::Instantiate<'a>),
    ) -> MutateResult<C::NodeId> {
        let Some(item) = self.storage.try_get::<C::NodeRef<'_>>(id) else {
            return MutateResult::NotFound;
        };
        self.index.remove(id, item, &self.storage);
        let mutation_result = self.storage.try_get_mut(id).map(fun);
        let item: C::NodeRef<'_> = self.storage.get(id);
        let insertion_result = self.index.insert(id, item, &self.storage);
        match (mutation_result, insertion_result) {
            (Some(()), None) => MutateResult::Ok,
            (Some(()), Some(node_id)) => MutateResult::Equivalent(node_id),
            (None, Some(_)) => unreachable!(),
            (None, None) => MutateResult::TypeError,
        }
    }
    pub fn indices_by_variant(&self, variant: C::Variant) -> impl Iterator<Item = C::NodeId> + '_ {
        self.storage.indices_by_variant(variant)
    }
    pub fn iter<'a, R: PagedStorageItemRef<'a, C> + 'a>(
        &'a self,
    ) -> impl Iterator<Item = (C::NodeId, R)> + 'a {
        self.storage.iter()
    }
    pub fn find_defs(&self, var: C::Var) -> impl Iterator<Item = C::NodeId> + '_ {
        self.index
            .defs
            .get(var)
            .into_iter()
            .flat_map(|set| set.iter().copied())
    }
    pub fn find_uses(&self, var: C::Var) -> impl Iterator<Item = C::NodeId> + '_ {
        self.index
            .uses
            .get(var)
            .into_iter()
            .flat_map(|set| set.iter().copied())
    }
    pub fn find_term(&self, term: &C::TermRef<'_>) -> Option<C::NodeId> {
        let subtable = term.max_var().id_index();
        if subtable >= self.index.unique.len() {
            return None;
        }
        self.index
            .unique
            .find(subtable, hash_value(term), |id| {
                C::term_eq(&self.storage.get::<C::NodeRef<'_>>(*id).term(), term)
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

    #[derive(PartialEq, Eq, Hash, Debug, Clone)]
    struct AndTerm(Var, Var);
    #[derive(PartialEq, Eq, Hash, Debug, Clone)]
    struct OrTerm(Var, Var);
    #[derive(PartialEq, Eq, Hash, Debug, Clone)]
    struct NotTerm(Var);

    type Var = u32;
    #[derive(PartialEq, Eq, Hash, Debug)]
    enum ExampleTerm {
        And(AndTerm),
        Or(OrTerm),
        Not(NotTerm),
    }
    #[derive(Debug)]
    struct ExampleItem(Var, ExampleTerm);
    #[derive(PartialEq, Eq, Hash, Debug, Clone)]
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

    impl<'a> IndexedTermRef<ExampleCatalog> for ExampleTermRef<'a> {
        fn use_vars(&self) -> impl Iterator<Item = Var> + '_ {
            match self {
                ExampleTermRef::And(&AndTerm(a, b)) => vec![a, b],
                ExampleTermRef::Or(&OrTerm(a, b)) => vec![a, b],
                ExampleTermRef::Not(&NotTerm(a)) => vec![a],
            }
            .into_iter()
        }

        fn map(
            &self,
            fun: impl FnMut(
                <ExampleCatalog as IndexedCatalog>::Lit,
            ) -> <ExampleCatalog as IndexedCatalog>::Lit,
        ) -> <ExampleCatalog as IndexedCatalog>::Term {
            todo!()
        }
    }

    impl<'a> From<&'a ExampleTerm> for ExampleTermRef<'a> {
        fn from(value: &'a ExampleTerm) -> Self {
            match value {
                ExampleTerm::And(and_term) => ExampleTermRef::And(and_term),
                ExampleTerm::Or(or_term) => ExampleTermRef::Or(or_term),
                ExampleTerm::Not(not_term) => ExampleTermRef::Not(not_term),
            }
        }
    }

    impl IndexedNodeRef<ExampleCatalog> for ExampleItem {
        fn output(&self) -> <ExampleCatalog as IndexedCatalog>::Lit {
            self.0
        }

        fn term(&self) -> <ExampleCatalog as IndexedCatalog>::TermRef<'_> {
            (&self.1).into()
        }
    }

    impl IndexedNode<ExampleCatalog> for ExampleItem {
        fn new(
            output: <ExampleCatalog as IndexedCatalog>::Lit,
            term: <ExampleCatalog as IndexedCatalog>::Term,
        ) -> Self {
            ExampleItem(output, term)
        }
    }

    impl IndexedNodeRef<ExampleCatalog> for ExampleItemRef<'_> {
        fn output(&self) -> <ExampleCatalog as IndexedCatalog>::Lit {
            self.0
        }

        fn term(&self) -> <ExampleCatalog as IndexedCatalog>::TermRef<'_> {
            self.1.clone()
        }
    }

    impl IndexedNodeMut<ExampleCatalog> for ExampleItemMut<'_> {
        fn rewrite(&mut self, _fun: impl FnMut(Var) -> Var) {
            todo!()
        }
    }

    impl<'a> IndexedNodeMutFamily<ExampleCatalog> for ExampleItemMut<'a> {
        type Instantiate<'b>
            = ExampleItemMut<'b>
        where
            'a: 'b;
    }

    impl IndexedCatalog for ExampleCatalog {
        type Var = Var;
        type Lit = Var;
        type NodeId = u32;
        type Node = ExampleItem;
        type NodeRef<'a> = ExampleItemRef<'a>;
        type NodeMut = ExampleItemMut<'static>;
        type Term = ExampleTerm;
        type TermRef<'a> = ExampleTermRef<'a>;

        fn term_eq(a: &Self::TermRef<'_>, b: &Self::TermRef<'_>) -> bool {
            a == b
        }
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

    impl IndexedNodeMut<ExampleCatalog> for &mut AndTerm {
        fn rewrite(&mut self, _fun: impl FnMut(Var) -> Var) {
            todo!()
        }
    }

    #[test]
    fn test() {
        let mut storage = IndexedPagedStorage::with_catalog(ExampleCatalog);
        let a = storage.insert(ExampleItem(14, ExampleTerm::And(AndTerm(3, 8))));
        storage.insert(ExampleItem(16, ExampleTerm::Or(OrTerm(3, 10))));
        storage.insert(ExampleItem(8, ExampleTerm::Not(NotTerm(4))));
        println!("{:?}", storage.try_mutate::<&mut AndTerm>(a, |r| r.0 = 7));
        storage.try_mutate::<ExampleItemMut>(a, |r| *r.0 = 3);
        println!("{:?}", storage.try_get::<ExampleItemRef>(0));
        println!(
            "{:?}",
            storage.find_term(&ExampleTermRef::And(&AndTerm(7, 8)))
        );
        println!("{:?}", storage.find_uses(7).collect::<Vec<_>>());
    }
}
