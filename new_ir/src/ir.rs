use imctk_lit::{Lit, Var};
use imctk_paged_storage::PagedStorageItemRef;
use imctk_union_find::TrackedUnionFind;
use num_bigint::BigUint;

use crate::{
    bitlevel::{self, BitlevelCatalog, BitlevelTerm, Node},
    dag::{self, IrDag},
    egraph::{EgraphMut, EgraphRef, EgraphStorage},
    wordlevel::{Concat, Const, HasSort, Sort, WordlevelCatalog, WordlevelNode, WordlevelTerm},
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
        union_find.ensure_allocated(Var::FALSE);
        EgraphMut::new(&mut egraph, &mut union_find).insert_node(Node {
            output: Lit::FALSE,
            term: BitlevelTerm::ConstFalse(crate::bitlevel::ConstFalse),
        });
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
    pub fn node(&mut self, output: Lit, term: impl Into<BitlevelTerm>) {
        self.egraph_mut().insert_node(Node {
            output,
            term: term.into(),
        });
    }
    pub fn find_term(&self, term: impl Into<BitlevelTerm>) -> Option<Lit> {
        let node_id = self.egraph.find_term(&term.into())?;
        let node: Node<BitlevelTerm> = self.egraph.get(node_id);
        Some(node.output)
    }
    pub fn and(&mut self, a: Lit, b: Lit) -> Lit {
        self.term(BitlevelTerm::And(bitlevel::And([a, b].into())))
    }
    pub fn and_node(&mut self, a: Lit, b: Lit, q: Lit) {
        self.node(q, BitlevelTerm::And(bitlevel::And([a, b].into())))
    }
    pub fn or(&mut self, a: Lit, b: Lit) -> Lit {
        !self.term(BitlevelTerm::And(bitlevel::And([!a, !b].into())))
    }
    pub fn or_node(&mut self, a: Lit, b: Lit, q: Lit) {
        self.node(!q, BitlevelTerm::And(bitlevel::And([!a, !b].into())))
    }
    pub fn xor(&mut self, a: Lit, b: Lit) -> Lit {
        self.term(BitlevelTerm::Xor(bitlevel::Xor([a, b].into())))
    }
    pub fn xor_node(&mut self, a: Lit, b: Lit, q: Lit) {
        self.node(q, BitlevelTerm::Xor(bitlevel::Xor([a, b].into())))
    }
    pub fn reduce_and(&mut self, args: impl IntoIterator<Item = Lit>) -> Lit {
        args.into_iter()
            .reduce(|a, b| self.and(a, b))
            .unwrap_or(Lit::FALSE)
    }
    pub fn reduce_or(&mut self, args: impl IntoIterator<Item = Lit>) -> Lit {
        args.into_iter()
            .reduce(|a, b| self.or(a, b))
            .unwrap_or(Lit::FALSE)
    }
    pub fn reduce_xor(&mut self, args: impl IntoIterator<Item = Lit>) -> Lit {
        args.into_iter()
            .reduce(|a, b| self.xor(a, b))
            .unwrap_or(Lit::FALSE)
    }
    pub fn fresh_lits(&mut self, n: usize) -> Vec<Lit> {
        self.union_find
            .fresh_atoms(n)
            .iter()
            .map(|v| v.as_lit())
            .collect()
    }
}

#[derive(Debug)]
pub struct WordIr {
    pub union_find: TrackedUnionFind<Var, Var>,
    pub egraph: EgraphStorage<WordlevelCatalog>,
    pub primary_def: IrDag<WordlevelCatalog, dag::AlwaysInsert>,
}

impl Default for WordIr {
    fn default() -> Self {
        let mut union_find = TrackedUnionFind::default();
        let observer = union_find.start_observing();
        let mut egraph = EgraphStorage::new(observer);
        let primary_def = IrDag::new(&mut EgraphMut::new(&mut egraph, &mut union_find));
        union_find.ensure_allocated(Var::FALSE);
        Self {
            union_find,
            egraph,
            primary_def,
        }
    }
}

impl WordIr {
    pub fn egraph_ref(&self) -> EgraphRef<'_, WordlevelCatalog> {
        EgraphRef::new(&self.egraph, &self.union_find)
    }
    pub fn egraph_mut(&mut self) -> EgraphMut<'_, WordlevelCatalog> {
        EgraphMut::new(&mut self.egraph, &mut self.union_find)
    }
    pub fn try_primary_def<'a, R>(&'a self, var: Var) -> Option<R>
    where
        R: PagedStorageItemRef<'a, WordlevelCatalog>,
    {
        self.primary_def
            .try_def(var)
            .and_then(|node_id| self.egraph.try_get(node_id))
    }
    pub fn primary_def<'a, R>(&'a self, var: Var) -> R
    where
        R: PagedStorageItemRef<'a, WordlevelCatalog>,
    {
        self.try_primary_def(var).unwrap()
    }
    pub fn refresh(&mut self) {
        self.egraph_mut().rebuild();
        self.primary_def
            .refresh(&EgraphRef::new(&self.egraph, &self.union_find))
    }
    pub fn term(&mut self, term: impl Into<WordlevelTerm>) -> Var {
        self.egraph_mut().insert_term(term.into())
    }
    pub fn node(&mut self, output: Var, term: impl Into<WordlevelTerm>) {
        self.egraph_mut().insert_node(WordlevelNode {
            output,
            term: term.into(),
        });
    }
    pub fn find_term(&self, term: impl Into<WordlevelTerm>) -> Option<Var> {
        let node_id = self.egraph.find_term(&term.into())?;
        let node: WordlevelNode<WordlevelTerm> = self.egraph.get(node_id);
        Some(node.output)
    }
    pub fn constant(&mut self, mut value: u64, sort: Sort) -> Var {
        if sort.0 < 64 {
            value &= (1u64 << sort.0) - 1;
        }
        self.term(WordlevelTerm::Const(Const { value, sort }))
    }
    pub fn big_constant(&mut self, value: &BigUint, sort: Sort) -> Var {
        let mut bits = 0;
        let mut result = None;
        for word in value.iter_u64_digits() {
            let word_var;
            if bits + 64 <= sort.0 {
                word_var = self.constant(word, Sort(64));
            } else if bits < sort.0 {
                let mask = (1u64 << (sort.0 - bits)) - 1;
                word_var = self.constant(word & mask, Sort(sort.0 - bits));
            } else {
                break;
            }
            bits += 64;
            if let Some(previous) = result {
                let current_sort = Sort(bits.min(sort.0));
                result = Some(self.term(WordlevelTerm::Concat(Concat(
                    [previous, word_var],
                    current_sort,
                ))));
            } else {
                result = Some(word_var);
            }
        }
        result.unwrap_or_else(|| self.constant(0, sort))
    }
    pub fn sort_of(&self, var: Var) -> Sort {
        let mut iter = self
            .egraph
            .find_defs(var)
            .map(|id| -> WordlevelNode<WordlevelTerm> { self.egraph.get(id) });
        let node = iter.next().unwrap();
        let sort = node.term.sort();
        debug_assert!(iter.all(|node| node.term.sort() == sort));
        sort
    }
}
