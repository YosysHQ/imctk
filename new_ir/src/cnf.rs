use imctk_ids::Id;
use imctk_lit::{Lit, Var};
use imctk_union_find::{change_tracking::ObserverToken, tracked_union_find::Change};

use crate::{
    bitlevel::{BitlevelTerm, InputId, Node, SteadyInputId},
    egraph::EgraphChange,
    ir::BitIr,
};

pub trait CnfSink {
    type Var: Id + Into<Self::Lit>;
    type Lit: Id + imctk_lit::Negate<Negated = Self::Lit>;
    type Error;
    fn input(&mut self, input: InputId, lit: Self::Lit) {}
    fn steady_input(&mut self, steady_input: SteadyInputId, lit: Self::Lit) {}
    fn var(&mut self, var: Var) -> Self::Var;
    fn clause(&mut self, clause: &[Self::Lit]) -> Result<(), Self::Error>;
    fn equiv(&mut self, lits: [Self::Lit; 2]) -> Result<(), Self::Error> {
        self.clause(&[lits[0], !lits[1]])?;
        self.clause(&[lits[1], !lits[0]])
    }
}

fn handle_node<Sink: CnfSink>(
    sink: &mut Sink,
    node: Node<BitlevelTerm>,
) -> Result<(), Sink::Error> {
    let output = node.output.lookup(|var| sink.var(var).into());
    match node.term {
        BitlevelTerm::ConstFalse(_) => {
            sink.clause(&[!output])?;
        }
        BitlevelTerm::Input(input_id) => {
            sink.input(input_id, output);
        }
        BitlevelTerm::SteadyInput(steady_input_id) => {
            sink.steady_input(steady_input_id, output);
        }
        BitlevelTerm::And(and_term) => {
            let [a, b] = (*and_term.0).map(|lit| lit.lookup(|var| sink.var(var).into()));
            sink.clause(&[output, !a, !b])?;
            sink.clause(&[!output, a])?;
            sink.clause(&[!output, b])?;
        }
        BitlevelTerm::Xor(xor_term) => {
            let [a, b] = (*xor_term.0).map(|lit| lit.lookup(|var| sink.var(var).into()));
            sink.clause(&[output, a, !b])?;
            sink.clause(&[output, !a, b])?;
            sink.clause(&[!output, a, b])?;
            sink.clause(&[!output, !a, !b])?;
        }
        BitlevelTerm::Reg(_) => todo!(),
    }
    Ok(())
}

pub fn translate_to_cnf<Sink: CnfSink>(ir: &BitIr, sink: &mut Sink) -> Result<(), Sink::Error> {
    for (_, node) in ir.egraph_ref().iter::<Node<BitlevelTerm>>() {
        handle_node(sink, node)?;
    }
    Ok(())
}

impl CnfSink for Vec<Vec<Lit>> {
    type Var = Var;
    type Lit = Lit;
    type Error = ();

    fn var(&mut self, var: Var) -> Self::Var {
        var
    }

    fn clause(&mut self, clause: &[Self::Lit]) -> Result<(), Self::Error> {
        self.push(clause.into());
        Ok(())
    }
}

pub struct IncrementalCnf {
    egraph_token: ObserverToken,
    tuf_token: ObserverToken,
}

impl IncrementalCnf {
    pub fn new_only_updates(ir: &mut BitIr) -> Self {
        let egraph_token = ir.egraph_mut().start_observing();
        let tuf_token = ir.union_find.start_observing();
        IncrementalCnf {
            egraph_token,
            tuf_token,
        }
    }

    pub fn new_full<Sink: CnfSink>(ir: &mut BitIr, sink: &mut Sink) -> Result<Self, Sink::Error> {
        translate_to_cnf(ir, sink)?;
        Ok(Self::new_only_updates(ir))
    }

    pub fn finish(self, ir: &mut BitIr) {
        ir.egraph_mut().stop_observing(self.egraph_token);
        ir.union_find.stop_observing(self.tuf_token);
    }

    pub fn update<Sink: CnfSink>(
        &mut self,
        ir: &mut BitIr,
        sink: &mut Sink,
    ) -> Result<(), Sink::Error> {
        {
            let egraph = ir.egraph_ref();
            let mut iter = egraph.drain_changes(&mut self.egraph_token);
            while let Some(change) = iter.next() {
                match change {
                    &EgraphChange::Insert(node_id) => {
                        let node = egraph.get(node_id);
                        handle_node(sink, node)?;
                    }
                    EgraphChange::Renumber(_) => unimplemented!(),
                }
            }
        }
        {
            let mut iter = ir.union_find.drain_changes(&mut self.tuf_token);
            while let Some(change) = iter.next() {
                match change {
                    &Change::Union {
                        new_repr,
                        merged_repr,
                    } => {
                        let new_repr_cnf = sink.var(new_repr);
                        let merged_repr_cnf = merged_repr.lookup(|var| sink.var(var).into());
                        sink.equiv([new_repr_cnf.into(), merged_repr_cnf])?;
                    }
                    Change::MakeRepr { .. } | Change::AllocAtoms { .. } => {}
                    Change::Renumber(_) => unimplemented!(),
                }
            }
        }
        Ok(())
    }
}
