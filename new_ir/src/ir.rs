use imctk_lit::{Lit, Var};
use imctk_union_find::TrackedUnionFind;

use crate::{
    bitlevel::BitlevelCatalog,
    egraph::{EgraphMut, EgraphRef, EgraphStorage},
};

#[derive(Debug)]
pub struct BitIr {
    pub union_find: TrackedUnionFind<Var, Lit>,
    pub egraph: EgraphStorage<BitlevelCatalog>,
}

impl Default for BitIr {
    fn default() -> Self {
        let mut union_find = TrackedUnionFind::default();
        let observer = union_find.start_observing();
        Self { union_find, egraph: EgraphStorage::new(observer) }
    }
}

impl BitIr {
    pub fn egraph_ref(&self) -> EgraphRef<'_, BitlevelCatalog> {
        EgraphRef::new(&self.egraph, &self.union_find)
    }
    pub fn egraph_mut(&mut self) -> EgraphMut<'_, BitlevelCatalog> {
        EgraphMut::new(&mut self.egraph, &mut self.union_find)
    }
}
