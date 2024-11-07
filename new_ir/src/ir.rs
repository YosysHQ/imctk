use imctk_lit::{Lit, Var};
use imctk_union_find::TrackedUnionFind;

use crate::{
    bitlevel::BitlevelCatalog,
    egraph::{EgraphMut, EgraphRef, EgraphStorage},
};

pub struct BitIr {
    pub union_find: TrackedUnionFind<Var, Lit>,
    pub egraph: EgraphStorage<BitlevelCatalog>,
}

impl BitIr {
    pub fn egraph_ref(&self) -> EgraphRef<'_, BitlevelCatalog> {
        EgraphRef::new(&self.egraph, &self.union_find)
    }
    pub fn egraph_mut(&mut self) -> EgraphMut<'_, BitlevelCatalog> {
        EgraphMut::new(&mut self.egraph, &mut self.union_find)
    }
}
