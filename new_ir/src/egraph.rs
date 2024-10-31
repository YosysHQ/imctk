use std::{
    collections::HashSet,
    ops::{Deref, DerefMut},
};

use imctk_ids::{Id, IdAlloc};
use imctk_paged_storage::{
    index::{
        IndexedCatalog, IndexedNode, IndexedNodeMut, IndexedNodeRef, IndexedPagedStorage,
        MutateResult,
    },
    PagedStorageItemRef,
};
use imctk_union_find::{
    tracked_union_find::{Change, ObserverToken},
    Element, TrackedUnionFind,
};

struct EgraphStorage<C: IndexedCatalog> {
    storage: IndexedPagedStorage<C>,
    observer_token: ObserverToken,
    var_alloc: IdAlloc<C::Var>,
}

impl<Catalog: IndexedCatalog> Deref for EgraphStorage<Catalog> {
    type Target = IndexedPagedStorage<Catalog>;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<Catalog: IndexedCatalog> DerefMut for EgraphStorage<Catalog> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.storage
    }
}

impl<Catalog: IndexedCatalog + Default> EgraphStorage<Catalog> {
    fn new(tuf: &mut TrackedUnionFind<Catalog::Var, Catalog::Lit>) -> Self {
        let var_alloc = IdAlloc::new();
        // since 0 is false, skip it
        var_alloc.alloc().unwrap();
        EgraphStorage {
            storage: IndexedPagedStorage::with_catalog(Default::default()),
            observer_token: tuf.start_observing(),
            var_alloc,
        }
    }
}

#[repr(C)]
struct EgraphRef<'a, Catalog: IndexedCatalog> {
    storage: &'a EgraphStorage<Catalog>,
    union_find: &'a TrackedUnionFind<Catalog::Var, Catalog::Lit>,
}

#[repr(C)]
struct EgraphMut<'a, Catalog: IndexedCatalog> {
    storage: &'a mut EgraphStorage<Catalog>,
    union_find: &'a mut TrackedUnionFind<Catalog::Var, Catalog::Lit>,
}

impl<'a, Catalog: IndexedCatalog> Deref for EgraphMut<'a, Catalog> {
    type Target = EgraphRef<'a, Catalog>;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const EgraphMut<'a, Catalog> as *const EgraphRef<'a, Catalog>) }
    }
}

impl<'a, Catalog: IndexedCatalog> EgraphRef<'a, Catalog> {
    pub fn try_get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: Catalog::NodeId) -> Option<T> {
        self.storage.try_get(id)
    }
    pub fn get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: Catalog::NodeId) -> T {
        self.storage.get(id)
    }
}

impl<Catalog: IndexedCatalog> EgraphMut<'_, Catalog> {
    pub fn insert_term_full(&mut self, term: Catalog::Term) -> (Catalog::NodeId, Catalog::Lit) {
        let node_id = self.storage.find_term(&(&term).into());
        if let Some(node_id) = node_id {
            let node = self.storage.get::<Catalog::NodeRef<'_>>(node_id);
            (node_id, node.output())
        } else {
            let output = Element::from_atom(self.storage.var_alloc.alloc().unwrap());
            let node_id = self.storage.insert(Catalog::Node::new(output, term));
            (node_id, output)
        }
    }
    pub fn insert_node(&mut self, node: Catalog::Node) -> Catalog::NodeId {
        let node_id = self.storage.find_term(&node.term());
        if let Some(node_id) = node_id {
            let existing_node = self.storage.get::<Catalog::NodeRef<'_>>(node_id);
            let (ok, r) = self
                .union_find
                .union_full([node.output(), existing_node.output()]);
            assert!(ok || r[0] == r[1]);
            node_id
        } else {
            self.storage.insert(node)
        }
    }
    pub fn union_full(&mut self, lits: [Catalog::Lit; 2]) -> (bool, [Catalog::Lit; 2]) {
        self.union_find.union_full(lits)
    }
    fn collect_rebuild_variables(&mut self) -> HashSet<Catalog::Var> {
        let mut result: HashSet<Catalog::Var> = HashSet::new();
        let mut iter = self
            .union_find
            .drain_changes(&mut self.storage.observer_token);
        while let Some(change) = iter.next() {
            match *change {
                Change::Union {
                    new_repr,
                    merged_repr,
                } => result.extend([new_repr, merged_repr.atom()]),
                Change::MakeRepr { new_repr, old_repr } => {
                    result.extend([new_repr, old_repr.atom()])
                }
                Change::Renumber(_) => todo!(),
            }
        }
        result
    }
    fn choose_representatives(&mut self, _variables: &HashSet<Catalog::Var>) {
        self.union_find.make_repr(Catalog::Var::MIN_ID);
    }
    fn collect_rebuild_nodes(
        &mut self,
        variables: &HashSet<Catalog::Var>,
    ) -> HashSet<Catalog::NodeId> {
        let mut result: HashSet<Catalog::NodeId> = HashSet::new();
        for &variable in variables {
            result.extend(self.storage.find_defs(variable));
            result.extend(self.storage.find_uses(variable));
        }
        result
    }
    fn rewrite_nodes(&mut self, nodes: &HashSet<Catalog::NodeId>) {
        for &node_id in nodes {
            let result = self
                .storage
                .try_mutate::<Catalog::NodeMut>(node_id, |mut r| {
                    r.rewrite(|lit| self.union_find.find(lit))
                });
            match result {
                MutateResult::Ok => {}
                MutateResult::NotFound | MutateResult::TypeError => unreachable!(),
                MutateResult::Equivalent(other_node_id) => {
                    let node = self.storage.get::<Catalog::NodeRef<'_>>(node_id);
                    let other_node = self.storage.get::<Catalog::NodeRef<'_>>(other_node_id);
                    let (ok, r) = self
                        .union_find
                        .union_full([node.output(), other_node.output()]);
                    assert!(ok || r[0] == r[1]);
                }
            }
        }
    }
    pub fn rebuild(&mut self) {
        let mut variables;
        while {
            variables = self.collect_rebuild_variables();
            !variables.is_empty()
        } {
            self.choose_representatives(&variables);
            variables.retain(|v| !self.union_find.is_repr(*v));
            let nodes = self.collect_rebuild_nodes(&variables);
            self.rewrite_nodes(&nodes);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitlevel::*;
    use imctk_lit::{Lit, Var};

    #[test]
    fn test() {
        let mut tuf = TrackedUnionFind::<Var, Lit>::new();
        let mut storage: EgraphStorage<BitlevelCatalog> = EgraphStorage::new(&mut tuf);
        let mut egraph = EgraphMut {
            storage: &mut storage,
            union_find: &mut tuf,
        };
        let (_, input0) = egraph.insert_term_full(BitlevelTerm::Input(InputId(0)));
        let (_, input1) = egraph.insert_term_full(BitlevelTerm::Input(InputId(1)));
        let (_, input2) = egraph.insert_term_full(BitlevelTerm::Input(InputId(2)));
        let (_, input3) = egraph.insert_term_full(BitlevelTerm::Input(InputId(3)));
        let (_, and0) =
            egraph.insert_term_full(BitlevelTerm::And(AndTerm([input0, input2].into())));
        let (_, and1) =
            egraph.insert_term_full(BitlevelTerm::And(AndTerm([input1, input2].into())));
        let (_, _xor0) = egraph.insert_term_full(BitlevelTerm::Xor(XorTerm([and0, input3].into())));
        let (_, _xor1) = egraph.insert_term_full(BitlevelTerm::Xor(XorTerm([and1, input3].into())));
        for node in egraph.storage.iter::<Node<BitlevelTerm>>() {
            println!("{node:?}");
        }
        println!("-----");
        egraph.union_full([input0, input1]);
        egraph.rebuild();
        for node in egraph.storage.iter::<Node<BitlevelTerm>>() {
            println!("{node:?}");
        }
    }
}
