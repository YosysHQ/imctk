use std::ops::Deref;

use imctk_ids::{Id, IdAlloc};
use imctk_lit::{Lit, Var};
use imctk_paged_storage::{
    index::{IndexedCatalog, IndexedNode, IndexedPagedStorage},
    PagedStorageItem, PagedStorageItemRef,
};
use imctk_union_find::TrackedUnionFind;

use crate::bitlevel::Node;

#[repr(C)]
struct EgraphRef<'a, NodeId: Id, Catalog: IndexedCatalog<Var>> {
    storage: &'a IndexedPagedStorage<NodeId, Var, Catalog>,
    union_find: &'a TrackedUnionFind<Var, Lit>,
    var_alloc: &'a IdAlloc<Var>,
}

#[repr(C)]
struct EgraphMut<'a, NodeId: Id, Catalog: IndexedCatalog<Var>> {
    storage: &'a mut IndexedPagedStorage<NodeId, Var, Catalog>,
    union_find: &'a mut TrackedUnionFind<Var, Lit>,
    var_alloc: &'a mut IdAlloc<Var>,
}

impl<'a, NodeId: Id, Catalog: IndexedCatalog<Var>> Deref for EgraphMut<'a, NodeId, Catalog> {
    type Target = EgraphRef<'a, NodeId, Catalog>;

    fn deref(&self) -> &Self::Target {
        unsafe {
            &*(self as *const EgraphMut<'a, NodeId, Catalog>
                as *const EgraphRef<'a, NodeId, Catalog>)
        }
    }
}

impl<'a, NodeId: Id, Catalog: IndexedCatalog<Var>> EgraphRef<'a, NodeId, Catalog> {
    pub fn try_get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: NodeId) -> Option<T> {
        self.storage.try_get(id)
    }
}

type CatalogTerm<'a, Catalog: IndexedCatalog<Var>> = <Catalog::Ref<'a> as IndexedNode<Var>>::Term;

impl<'a, NodeId: Id, Catalog: IndexedCatalog<Var>> EgraphMut<'a, NodeId, Catalog> {
    pub fn insert_term_full(
        &mut self,
        term: CatalogTerm<'static, Catalog>,
    ) -> (NodeId, Lit)
    where
        for<'b> Node<CatalogTerm<'b, Catalog>>: PagedStorageItemRef<'b, Catalog>,
        Node<CatalogTerm<'static, Catalog>>: PagedStorageItem<Catalog>,

    {
        if let Some(node_id) = self.storage.find_term(&term) {
            let node = self
                .storage
                .get::<Node<CatalogTerm<Catalog>>>(node_id);
            (node_id, node.output)
        } else {
            let output = self.var_alloc.alloc().unwrap().as_lit();
            let node_id = self.storage.insert(Node { output, term });
            (node_id, output)
        }
    }
}
