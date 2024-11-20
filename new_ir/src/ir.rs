use imctk_lit::{Lit, Var};
use imctk_paged_storage::PagedStorageItemRef;
use imctk_union_find::TrackedUnionFind;

use crate::{
    bitlevel::{BitlevelCatalog, BitlevelTerm},
    dag::{self, IrDag},
    egraph::{EgraphMut, EgraphRef, EgraphStorage},
};

#[derive(Debug)]
pub struct BitIr {
    pub union_find: TrackedUnionFind<Var, Lit>,
    pub egraph: EgraphStorage<BitlevelCatalog>,
    pub primary_def: IrDag<BitlevelCatalog, dag::AlwaysInsert>,
}

impl Default for BitIr {
    fn default() -> Self {
        let mut union_find = TrackedUnionFind::default();
        let observer = union_find.start_observing();
        let mut egraph = EgraphStorage::new(observer);
        let primary_def = IrDag::new(&mut EgraphMut::new(&mut egraph, &mut union_find));
        Self {
            union_find,
            egraph,
            primary_def,
        }
    }
}

impl BitIr {
    pub fn egraph_ref(&self) -> EgraphRef<'_, BitlevelCatalog> {
        EgraphRef::new(&self.egraph, &self.union_find)
    }
    pub fn egraph_mut(&mut self) -> EgraphMut<'_, BitlevelCatalog> {
        EgraphMut::new(&mut self.egraph, &mut self.union_find)
    }
    pub fn try_primary_def<'a, R>(&'a self, var: Var) -> Option<R>
    where
        R: PagedStorageItemRef<'a, BitlevelCatalog>,
    {
        self.primary_def
            .try_def(var)
            .and_then(|node_id| self.egraph.try_get(node_id))
    }
    pub fn primary_def<'a, R>(&'a self, var: Var) -> R
    where
        R: PagedStorageItemRef<'a, BitlevelCatalog>,
    {
        self.try_primary_def(var).unwrap()
    }
    pub fn refresh(&mut self) {
        self.egraph_mut().rebuild();
        self.primary_def
            .refresh(&EgraphRef::new(&self.egraph, &self.union_find))
    }
    pub fn term(&mut self, term: impl Into<BitlevelTerm>) -> Lit {
        self.egraph_mut().insert_term(term.into())
    }
}
