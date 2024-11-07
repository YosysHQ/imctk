use std::{
    collections::HashSet,
    ops::{Deref, DerefMut},
    sync::Arc,
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
    change_tracking::{self, ChangeTracking},
    tracked_union_find::{Change, Renumbering},
    Element, TrackedUnionFind,
};

pub use imctk_union_find::change_tracking::{Generation, ObserverToken};

pub enum EgraphChange<C: IndexedCatalog> {
    Insert(C::NodeId),
    Renumber(Generation, Generation),
}

impl<C: IndexedCatalog> change_tracking::Change for EgraphChange<C> {
    fn as_renumbering(&self) -> Option<(Generation, Generation)> {
        match self {
            EgraphChange::Renumber(old_generation, new_generation) => {
                Some((*old_generation, *new_generation))
            }
            _ => None,
        }
    }
}

pub struct EgraphStorage<C: IndexedCatalog> {
    storage: IndexedPagedStorage<C>,
    observer_token: ObserverToken,
    var_alloc: IdAlloc<C::Var>,
    change_tracking: ChangeTracking<EgraphChange<C>>,
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
    fn new(observer_token: ObserverToken) -> Self {
        let var_alloc = IdAlloc::new();
        // since 0 is false, skip it
        var_alloc.alloc().unwrap();
        EgraphStorage {
            storage: IndexedPagedStorage::with_catalog(Default::default()),
            observer_token,
            var_alloc,
            change_tracking: Default::default(),
        }
    }
}

#[repr(C)]
pub struct EgraphRef<'a, Catalog: IndexedCatalog> {
    storage: &'a EgraphStorage<Catalog>,
    union_find: &'a TrackedUnionFind<Catalog::Var, Catalog::Lit>,
}

#[repr(C)]
pub struct EgraphMut<'a, Catalog: IndexedCatalog> {
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
    pub fn new(
        storage: &'a EgraphStorage<Catalog>,
        union_find: &'a TrackedUnionFind<Catalog::Var, Catalog::Lit>,
    ) -> Self {
        EgraphRef {
            storage,
            union_find,
        }
    }
    pub fn try_get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: Catalog::NodeId) -> Option<T> {
        self.storage.try_get(id)
    }
    pub fn get<T: PagedStorageItemRef<'a, Catalog>>(&self, id: Catalog::NodeId) -> T {
        self.storage.get(id)
    }
    pub fn iter<T: PagedStorageItemRef<'a, Catalog> + 'a>(
        &self,
    ) -> impl Iterator<Item = (Catalog::NodeId, T)> + 'a {
        self.storage.iter()
    }
}

impl<'a, Catalog: IndexedCatalog + Default> EgraphMut<'a, Catalog> {
    pub fn new(
        storage: &'a mut EgraphStorage<Catalog>,
        union_find: &'a mut TrackedUnionFind<Catalog::Var, Catalog::Lit>,
    ) -> Self {
        EgraphMut {
            storage,
            union_find,
        }
    }
    pub fn insert_term_full(&mut self, term: Catalog::Term) -> (Catalog::NodeId, Catalog::Lit) {
        let node_id = self.storage.find_term(&(&term).into());
        if let Some(node_id) = node_id {
            let node = self.storage.get::<Catalog::NodeRef<'_>>(node_id);
            (node_id, node.output())
        } else {
            let output = Element::from_atom(self.storage.var_alloc.alloc().unwrap());
            let node_id = self.storage.insert(Catalog::Node::new(output, term));
            self.storage
                .change_tracking
                .log(EgraphChange::Insert(node_id));
            (node_id, output)
        }
    }
    pub fn insert_term(&mut self, term: Catalog::Term) -> Catalog::Lit {
        self.insert_term_full(term).1
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
            let node_id = self.storage.insert(node);
            self.storage
                .change_tracking
                .log(EgraphChange::Insert(node_id));
            node_id
        }
    }
    pub fn fresh_var(&mut self) -> Catalog::Var {
        self.storage.var_alloc.alloc().unwrap()
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
                Change::Renumber(_) => unreachable!(),
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
    fn rewrite_nodes(&mut self, nodes: &HashSet<Catalog::NodeId>, delete_redundant: bool) {
        let mut discarded_nodes = HashSet::new();
        for &node_id in nodes {
            let result = self
                .storage
                .try_mutate::<Catalog::NodeMut>(node_id, |mut r| {
                    r.rewrite(|lit| self.union_find.find(lit))
                });
            match result {
                MutateResult::Ok => {}
                MutateResult::NotFound => {
                    debug_assert!(discarded_nodes.contains(&node_id));
                }
                MutateResult::TypeError => unreachable!(),
                MutateResult::Equivalent(other_node_id) => {
                    {
                        let node = self.storage.get::<Catalog::NodeRef<'_>>(node_id);
                        let other_node = self.storage.get::<Catalog::NodeRef<'_>>(other_node_id);
                        let (ok, r) = self
                            .union_find
                            .union_full([node.output(), other_node.output()]);
                        assert!(ok || r[0] == r[1]);
                    }
                    if delete_redundant {
                        self.storage.discard(other_node_id);
                        discarded_nodes.insert(other_node_id);
                    }
                }
            }
        }
    }
    pub fn rebuild(&mut self) {
        let mut delete_redundant = false;
        if let Some(renumbering) = self.collect_renumberings() {
            self.apply_renumbering(&renumbering);
            delete_redundant = true;
        }
        let mut variables;
        while {
            variables = self.collect_rebuild_variables();
            !variables.is_empty()
        } {
            self.choose_representatives(&variables);
            variables.retain(|v| !self.union_find.is_repr(*v));
            let nodes = self.collect_rebuild_nodes(&variables);
            self.rewrite_nodes(&nodes, delete_redundant);
        }
    }
    fn collect_renumberings(&mut self) -> Option<Arc<Renumbering<Catalog::Var, Catalog::Lit>>> {
        let mut iter = self
            .union_find
            .drain_changes(&mut self.storage.observer_token);
        let mut renumbering: Option<Arc<Renumbering<_, _>>> = None;
        while iter.any_renumberings() {
            if let Change::Renumber(next_renumbering) = iter.next().unwrap() {
                if let Some(previous_renumbering) = renumbering {
                    renumbering = Some(Arc::new(previous_renumbering.compose(next_renumbering)));
                } else {
                    renumbering = Some(next_renumbering.clone());
                }
            }
        }
        iter.stop();
        renumbering
    }
    fn apply_renumbering(&mut self, renumbering: &Renumbering<Catalog::Var, Catalog::Lit>) {
        let mut new_storage: EgraphStorage<Catalog> =
            EgraphStorage::new(self.union_find.clone_token(&self.storage.observer_token));
        let mut new_egraph = EgraphMut {
            storage: &mut new_storage,
            union_find: self.union_find,
        };
        for (_, node) in self.storage.iter::<Catalog::NodeRef<'_>>() {
            let node = node.map(|lit| renumbering.old_to_new(lit).unwrap());
            new_egraph.insert_node(node);
        }
        new_storage
            .var_alloc
            .alloc_range(renumbering.reverse().len() - 1)
            .unwrap();
        let old_storage = std::mem::replace(self.storage, new_storage);
        self.union_find.stop_observing(old_storage.observer_token);
        self.storage.change_tracking.log(EgraphChange::Renumber(
            renumbering.old_generation(),
            renumbering.new_generation(),
        ));
    }
    pub fn start_observing(&mut self) -> ObserverToken {
        self.storage.change_tracking.start_observing()
    }
    pub fn clone_token(&mut self, token: &ObserverToken) -> ObserverToken {
        self.storage.change_tracking.clone_token(token)
    }
    pub fn stop_observing(&mut self, token: ObserverToken) {
        self.storage.change_tracking.stop_observing(token);
    }
    pub fn generation(&self) -> Generation {
        self.storage.change_tracking.generation()
    }
    pub fn drain_changes_with_fn(
        &mut self,
        token: &mut ObserverToken,
        mut f: impl FnMut(&[EgraphChange<Catalog>], &IndexedPagedStorage<Catalog>),
    ) -> bool {
        self.storage.change_tracking.drain_changes_with_fn(token, |ch| f(ch, &self.storage.storage))
    }
   pub fn drain_changes<'b>(
        &mut self,
        token: &'b mut ObserverToken,
    ) -> (change_tracking::DrainChanges<'_, 'b, EgraphChange<Catalog>>, &IndexedPagedStorage<Catalog>) {
        (self.storage.change_tracking.drain_changes(token), &self.storage.storage)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::bitlevel::*;
    use imctk_ids::{id_vec::IdVec, IdRange};
    use imctk_lit::{Lit, Var};
    use imctk_union_find::UnionFind;

    fn repr_reduction(
        var_count: Var,
        union_find: &UnionFind<Var, Lit>,
    ) -> (IdVec<Var, Option<Lit>>, IdVec<Var, Lit>) {
        let mut forward: IdVec<Var, Option<Lit>> = IdVec::default();
        let mut reverse: IdVec<Var, Lit> = IdVec::default();
        for atom in IdRange::from(Var::MIN_ID..var_count) {
            let repr = union_find.find(atom.as_lit());
            let new_repr = *forward.grow_for_key(repr.atom()).get_or_insert_with(|| {
                let new_repr = reverse.push(Lit::from_atom(repr.var())).0;
                Lit::from_atom(new_repr)
            });
            forward
                .grow_for_key(atom)
                .replace(Element::<Var>::apply_pol_of(new_repr, repr));
        }
        (forward, reverse)
    }

    #[test]
    fn test() {
        let mut tuf = TrackedUnionFind::<Var, Lit>::new();
        let mut storage: EgraphStorage<BitlevelCatalog> = EgraphStorage::new(tuf.start_observing());
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
        let var_count = egraph.storage.var_alloc.alloc().unwrap();
        println!("-----");
        egraph.union_full([input0, input1]);
        let (forward, reverse) = repr_reduction(var_count, egraph.union_find.get_union_find());
        println!("{forward:?} {reverse:?}");
        egraph.union_find.renumber(forward, reverse);
        egraph.rebuild();
        for node in egraph.storage.iter::<Node<BitlevelTerm>>() {
            println!("{node:?}");
        }
    }
}