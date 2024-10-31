use std::{
    collections::HashSet,
    ops::{Deref, DerefMut},
};

use imctk_ids::{Id, IdAlloc};
use imctk_lit::{Lit, Var};
use imctk_paged_storage::{
    index::{IndexedCatalog, IndexedNodeMut, IndexedPagedStorage, MutateResult, NewLifetime},
    PagedStorageItem, PagedStorageItemMut, PagedStorageItemRef,
};
use imctk_union_find::{
    tracked_union_find::{Change, ObserverToken},
    TrackedUnionFind,
};

use crate::bitlevel::Node;

struct EgraphStorage<NodeId: Id, C: IndexedCatalog> {
    storage: IndexedPagedStorage<NodeId, C>,
    observer_token: ObserverToken,
    var_alloc: IdAlloc<Var>,
}

impl<NodeId: Id, Catalog: IndexedCatalog> Deref for EgraphStorage<NodeId, Catalog> {
    type Target = IndexedPagedStorage<NodeId, Catalog>;

    fn deref(&self) -> &Self::Target {
        &self.storage
    }
}

impl<NodeId: Id, Catalog: IndexedCatalog> DerefMut for EgraphStorage<NodeId, Catalog> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.storage
    }
}

impl<NodeId: Id, Catalog: IndexedCatalog + Default> EgraphStorage<NodeId, Catalog> {
    fn new(tuf: &mut TrackedUnionFind<Var, Lit>) -> Self {
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
struct EgraphRef<'a, NodeId: Id, Catalog: IndexedCatalog> {
    storage: &'a EgraphStorage<NodeId, Catalog>,
    union_find: &'a TrackedUnionFind<Var, Lit>,
}

#[repr(C)]
struct EgraphMut<'a, NodeId: Id, Catalog: IndexedCatalog> {
    storage: &'a mut EgraphStorage<NodeId, Catalog>,
    union_find: &'a mut TrackedUnionFind<Var, Lit>,
}

impl<'a, NodeId: Id, Catalog: IndexedCatalog> Deref for EgraphMut<'a, NodeId, Catalog> {
    type Target = EgraphRef<'a, NodeId, Catalog>;

    fn deref(&self) -> &Self::Target {
        unsafe {
            &*(self as *const EgraphMut<'a, NodeId, Catalog>
                as *const EgraphRef<'a, NodeId, Catalog>)
        }
    }
}

impl<'a, NodeId: Id, Catalog: IndexedCatalog> EgraphRef<'a, NodeId, Catalog> {
    pub fn try_get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: NodeId) -> Option<T> {
        self.storage.try_get(id)
    }
    pub fn get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: NodeId) -> T {
        self.storage.get(id)
    }
}

impl<NodeId: Id, Catalog: IndexedCatalog<Var = Var, Lit = Lit>> EgraphMut<'_, NodeId, Catalog>
where
    for<'b> Node<Catalog::TermRef<'b>>: PagedStorageItemRef<'b, Catalog>,
    for<'b> <Catalog::Mut<'static> as NewLifetime>::NewLifetime<'b>:
        PagedStorageItemMut<'b, Catalog> + IndexedNodeMut<Catalog>,
    Node<Catalog::Term>: PagedStorageItem<Catalog>,
{
    pub fn insert_term_full(&mut self, term: Catalog::Term) -> (NodeId, Lit) {
        let node_id = self.storage.find_term(&(&term).into());
        if let Some(node_id) = node_id {
            let node = self.storage.get::<Node<Catalog::TermRef<'_>>>(node_id);
            (node_id, node.output)
        } else {
            let output = self.storage.var_alloc.alloc().unwrap().as_lit();
            let node_id = self.storage.insert(Node { output, term });
            (node_id, output)
        }
    }
    pub fn insert_node(&mut self, node: Node<Catalog::Term>) -> NodeId {
        let node_id = self.storage.find_term(&(&node.term).into());
        if let Some(node_id) = node_id {
            let existing_node = self.storage.get::<Node<Catalog::TermRef<'_>>>(node_id);
            let (ok, r) = self
                .union_find
                .union_full([node.output, existing_node.output]);
            assert!(ok || r[0] == r[1]);
            node_id
        } else {
            self.storage.insert(node)
        }
    }
    pub fn union_full(&mut self, lits: [Lit; 2]) -> (bool, [Lit; 2]) {
        self.union_find.union_full(lits)
    }
    fn collect_rebuild_variables(&mut self) -> HashSet<Var> {
        let mut result: HashSet<Var> = HashSet::new();
        let mut iter = self
            .union_find
            .drain_changes(&mut self.storage.observer_token);
        while let Some(change) = iter.next() {
            match change {
                Change::Union {
                    new_repr,
                    merged_repr,
                } => result.extend([*new_repr, merged_repr.var()]),
                Change::MakeRepr { new_repr, old_repr } => {
                    result.extend([*new_repr, old_repr.var()])
                }
                Change::Renumber(_) => todo!(),
            }
        }
        result
    }
    fn choose_representatives(&mut self, _variables: &HashSet<Var>) {
        self.union_find.make_repr(Var::FALSE);
    }
    fn collect_rebuild_nodes(&mut self, variables: &HashSet<Var>) -> HashSet<NodeId> {
        let mut result: HashSet<NodeId> = HashSet::new();
        for &variable in variables {
            result.extend(self.storage.find_defs(variable));
            result.extend(self.storage.find_uses(variable));
        }
        result
    }
    fn rewrite_nodes(&mut self, nodes: &HashSet<NodeId>) {
        for &node_id in nodes {
            let result = self
                .storage
                .try_mutate::<Catalog::Mut<'_>>(node_id, |mut r| {
                    r.rewrite(|lit| self.union_find.find(lit))
                });
            match result {
                MutateResult::Ok => {}
                MutateResult::NotFound | MutateResult::TypeError => unreachable!(),
                MutateResult::Equivalent(other_node_id) => {
                    let node = self.storage.get::<Node<Catalog::TermRef<'_>>>(node_id);
                    let other_node = self
                        .storage
                        .get::<Node<Catalog::TermRef<'_>>>(other_node_id);
                    let (ok, r) = self.union_find.union_full([node.output, other_node.output]);
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
            variables.retain(|v| self.union_find.find(v.as_lit()) != v.as_lit());
            let nodes = self.collect_rebuild_nodes(&variables);
            self.rewrite_nodes(&nodes);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitlevel::*;

    #[test]
    fn test() {
        let mut tuf = TrackedUnionFind::<Var, Lit>::new();
        let mut storage: EgraphStorage<u32, BitlevelCatalog> = EgraphStorage::new(&mut tuf);
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
