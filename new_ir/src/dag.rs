use std::{
    marker::PhantomData,
    ops::{Deref, DerefMut},
    sync::Arc,
};

use imctk_ids::id_vec::IdVec;
use imctk_paged_storage::index::{IndexedCatalog, IndexedNodeRef, IndexedTerm};
use imctk_union_find::{
    change_tracking::ObserverToken,
    tracked_union_find::{Change, Renumbering},
    Element,
};

use crate::egraph::{EgraphChange, EgraphMut, EgraphRef, EgraphRenumbering};

pub trait InsertionPolicy<C: IndexedCatalog> {
    fn on_insertion(dag: &mut IrDagCore<C>, egraph: &EgraphRef<C>, node_id: C::NodeId);
}

pub struct AlwaysInsert;

impl<C: IndexedCatalog> InsertionPolicy<C> for AlwaysInsert {
    fn on_insertion(dag: &mut IrDagCore<C>, egraph: &EgraphRef<C>, node_id: C::NodeId) {
        let _ = dag.try_add_node(egraph, node_id);
    }
}

pub struct NeverInsert;

impl<C: IndexedCatalog> InsertionPolicy<C> for NeverInsert {
    fn on_insertion(_dag: &mut IrDagCore<C>, _egraph: &EgraphRef<C>, _node_id: C::NodeId) {}
}

#[derive(Clone, Debug)]
pub enum IrDagError {
    UsesUndefinedVar,
    AlreadyDefined,
}

pub struct IrDagCore<C: IndexedCatalog> {
    def: IdVec<C::Var, Option<(C::NodeId, u32)>>,
}

impl<C: IndexedCatalog> IrDagCore<C> {
    pub fn with_len(len: usize) -> Self {
        IrDagCore {
            def: IdVec::from_vec(vec![None; len]),
        }
    }
    pub fn try_def(&self, var: C::Var) -> Option<C::NodeId> {
        self.def[var].map(|x| x.0)
    }
    pub fn def(&self, var: C::Var) -> C::NodeId {
        self.try_def(var).unwrap()
    }
    pub fn try_add_node(
        &mut self,
        egraph: &EgraphRef<C>,
        node_id: C::NodeId,
    ) -> Result<(), IrDagError> {
        let node: C::NodeRef<'_> = egraph.get(node_id);
        let def_var = node.output().atom();
        let mut level = 0;
        for var in node.term().nonguarding_vars() {
            if let Some((_, var_level)) = self.def[var] {
                level = Ord::max(level, var_level + 1);
            } else {
                return Err(IrDagError::UsesUndefinedVar);
            }
        }
        match &mut self.def[def_var] {
            Some(_) => Err(IrDagError::AlreadyDefined),
            def @ None => {
                *def = Some((node_id, level));
                Ok(())
            }
        }
    }
    pub fn add_node(&mut self, egraph: &EgraphRef<C>, node_id: C::NodeId) {
        self.try_add_node(egraph, node_id).unwrap()
    }
    fn handle_union(&mut self, new_repr: C::Var, merged_repr: C::Var) {
        if let Some((b_id, b_level)) = self.def[merged_repr].take() {
            if let Some((_, a_level)) = self.def[new_repr] {
                if a_level > b_level {
                    self.def[new_repr] = Some((b_id, b_level));
                }
            } else {
                self.def[new_repr] = Some((b_id, b_level));
            }
        }
    }
    fn handle_makerepr(&mut self, new_repr: C::Var, old_repr: C::Var) {
        let value = self.def[old_repr].take();
        let replaced_value = std::mem::replace(&mut self.def[new_repr], value);
        debug_assert!(replaced_value.is_none());
    }
    fn handle_renumbering(&mut self, renumbering: &Arc<Renumbering<C::Var, C::Lit>>) {
        let new_def = IdVec::from_vec(
            renumbering
                .reverse()
                .iter()
                .map(|(_, &old_var)| self.def[old_var.atom()])
                .collect(),
        );
        self.def = new_def;
    }
    fn handle_egraph_renumbering(&mut self, renumbering: &Arc<EgraphRenumbering<C>>) {
        let new_def = IdVec::from_vec(
            self.def
                .iter()
                .map(|(_, value)| {
                    value.map(|(id, level)| (renumbering.forward()[id].unwrap(), level))
                })
                .collect(),
        );
        self.def = new_def;
    }
}

pub struct IrDag<C: IndexedCatalog, Policy: InsertionPolicy<C>> {
    core: IrDagCore<C>,
    tuf_token: ObserverToken,
    egraph_token: ObserverToken,
    _phantom: PhantomData<Policy>,
}

impl<C: IndexedCatalog, Policy: InsertionPolicy<C>> Deref for IrDag<C, Policy> {
    type Target = IrDagCore<C>;

    fn deref(&self) -> &Self::Target {
        &self.core
    }
}

impl<C: IndexedCatalog, Policy: InsertionPolicy<C>> DerefMut for IrDag<C, Policy> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.core
    }
}

impl<C: IndexedCatalog, Policy: InsertionPolicy<C>> IrDag<C, Policy> {
    pub fn new(egraph: &mut EgraphMut<C>) -> Self {
        let tuf_token = egraph.union_find_mut().start_observing();
        let egraph_token = egraph.start_observing();
        IrDag {
            core: IrDagCore::with_len(egraph.union_find().len()),
            tuf_token,
            egraph_token,
            _phantom: PhantomData,
        }
    }
    pub fn refresh(&mut self, egraph: &EgraphRef<C>) {
        {
            let mut iter = egraph.union_find().drain_changes(&mut self.tuf_token);
            while let Some(change) = iter.next() {
                match change {
                    &Change::Union {
                        new_repr,
                        merged_repr,
                    } => self.core.handle_union(new_repr, merged_repr.atom()),
                    &Change::MakeRepr { new_repr, old_repr } => {
                        self.core.handle_makerepr(new_repr, old_repr.atom())
                    }
                    Change::Renumber(renumbering) => self.core.handle_renumbering(renumbering),
                    &Change::AllocAtoms { new_max } => {
                        self.core.def.grow_for_key(new_max);
                    }
                }
            }
        }
        {
            let mut iter = egraph.drain_changes(&mut self.egraph_token);
            while let Some(change) = iter.next() {
                match change {
                    &EgraphChange::Insert(node_id) => {
                        Policy::on_insertion(&mut self.core, egraph, node_id)
                    }
                    EgraphChange::Renumber(renumbering) => {
                        self.core.handle_egraph_renumbering(renumbering)
                    }
                }
            }
        }
    }
}
