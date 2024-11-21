use imctk_abc::sat::glucose2::{CnfOnly, Solver};
use imctk_ids::{id_vec::IdVec, Id, IdAlloc};
use imctk_lit::{Lit, Var};
use imctk_paged_storage::index::IndexedTermRef;

use crate::{
    bitlevel::{BitlevelTerm, InputId, Node, Reg},
    cnf::{CnfSink, IncrementalCnf},
    ir::BitIr,
};

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct Frame(pub u32);

#[derive(Default, Debug)]
struct Unroller {
    var_map: IdVec<Frame, IdVec<Var, Option<Lit>>>,
    input_map: IdVec<Frame, IdVec<InputId, Option<InputId>>>,
    unrolled_input_alloc: IdAlloc<InputId>,
}

impl Unroller {
    fn get(&mut self, seq_ir: &BitIr, unrolled_ir: &mut BitIr, frame: Frame, lit: Lit) -> Lit {
        if let Some(unrolled_lit) = *self.var_map.grow_for_key(frame).grow_for_key(lit.var()) {
            return unrolled_lit ^ lit.pol();
        }
        let node: Node<BitlevelTerm> = seq_ir.primary_def(lit.var());
        let v1 = seq_ir.union_find.find(node.output);
        let v2 = seq_ir.union_find.find(lit.as_pos());
        debug_assert_eq!(v1.var(), v2.var());
        let pol = v1.pol() ^ v2.pol();
        let unrolled_lit = match node.term {
            BitlevelTerm::Reg(Reg { init, next }) => {
                if frame.0 == 0 {
                    self.get(seq_ir, unrolled_ir, frame, init)
                } else {
                    self.get(seq_ir, unrolled_ir, Frame(frame.0 - 1), next)
                }
            }
            BitlevelTerm::Input(input_id) => {
                let unrolled_id = *self
                    .input_map
                    .grow_for_key(frame)
                    .grow_for_key(input_id)
                    .get_or_insert_with(|| self.unrolled_input_alloc.alloc());
                unrolled_ir.term(BitlevelTerm::Input(unrolled_id))
            }
            BitlevelTerm::ConstFalse(_)
            | BitlevelTerm::SteadyInput(_)
            | BitlevelTerm::And(_)
            | BitlevelTerm::Xor(_) => {
                let unrolled_term = node
                    .term
                    .map(|lit| self.get(seq_ir, unrolled_ir, frame, lit));
                unrolled_ir.term(unrolled_term)
            }
        } ^ pol;
        self.var_map[frame][lit.var()] = Some(unrolled_lit);
        unrolled_lit ^ lit.pol()
    }
}

pub struct Bmc {
    unroller: Unroller,
    unrolled_ir: BitIr,
    cnf: IncrementalCnf,
    solver: Solver<'static, CnfOnly>,
    assumptions: Vec<Lit>,
}

impl CnfSink for Solver<'_, CnfOnly> {
    type Var = Var;
    type Lit = Lit;
    type Error = ();
    fn var(&mut self, var: Var) -> Self::Var {
        var
    }
    fn clause(&mut self, clause: &[Self::Lit]) -> Result<(), Self::Error> {
        self.add_clause(clause);
        Ok(())
    }
}

pub struct BmcWitness<'a> {
    property_index: usize,
    max_frame: Frame,
    bmc: &'a mut Bmc,
}

impl Bmc {
    pub fn new(mut solver: Solver<'static, CnfOnly>) -> Self {
        let unroller = Unroller::default();
        let mut unrolled_ir = BitIr::default();
        let cnf = IncrementalCnf::new_full(&mut unrolled_ir, &mut solver).unwrap();
        Bmc {
            unroller,
            unrolled_ir,
            cnf,
            solver,
            assumptions: Vec::new(),
        }
    }
    pub fn assume(&mut self, assumption: Lit) {
        self.assumptions.push(assumption);
    }
    pub fn run(
        &mut self,
        seq_ir: &BitIr,
        depth: u32,
        bad_state_outputs: &[Lit],
    ) -> Option<BmcWitness<'_>> {
        let mut unrolled_assumptions = Vec::new();
        for frame in (0..=depth).map(Frame) {
            unrolled_assumptions.extend(self.assumptions.iter().map(|lit| {
                self.unroller
                    .get(seq_ir, &mut self.unrolled_ir, frame, *lit)
            }));
            for (property_index, &bad_state) in bad_state_outputs.iter().enumerate() {
                let unrolled_bad_state =
                    self.unroller
                        .get(seq_ir, &mut self.unrolled_ir, frame, bad_state);
                self.cnf
                    .update(&mut self.unrolled_ir, &mut self.solver)
                    .unwrap();
                unrolled_assumptions.push(unrolled_bad_state);
                let result = self.solver.solve_assuming(&unrolled_assumptions);
                unrolled_assumptions.pop();
                match result {
                    None => unimplemented!(),
                    Some(true) => {
                        return Some(BmcWitness {
                            property_index,
                            max_frame: frame,
                            bmc: self,
                        });
                    }
                    Some(false) => {}
                }
            }
        }
        None
    }
}

impl BmcWitness<'_> {
    pub fn get_lit(&self, frame: Frame, lit: Lit) -> Option<bool> {
        if frame > self.max_frame {
            None
        } else {
            let unrolled_lit = (*self.bmc.unroller.var_map.get(frame)?.get(lit.var())?)?;
            self.bmc.solver.value(unrolled_lit)
        }
    }
    pub fn get_input(&self, frame: Frame, input_id: InputId) -> Option<bool> {
        if frame > self.max_frame {
            None
        } else {
            let unrolled_input_id = (*self.bmc.unroller.input_map.get(frame)?.get(input_id)?)?;
            let unrolled_lit = self
                .bmc
                .unrolled_ir
                .find_term(BitlevelTerm::Input(unrolled_input_id))
                .unwrap();
            self.bmc.solver.value(unrolled_lit)
        }
    }
}

#[cfg(test)]
mod tests {
    use std::io::{BufReader, BufWriter, Write};

    use flussab_aiger::aig::OrderedAig;

    use crate::aiger::{AigerImporter, AigerLit, AigerVar};

    use super::*;

    fn opt_to_char(b: Option<bool>) -> char {
        match b {
            None => 'x',
            Some(true) => '1',
            Some(false) => '0',
        }
    }

    fn print_witness(
        mut f: impl std::io::Write,
        witness: &BmcWitness,
        aiger_map: &IdVec<AigerVar, Lit>,
        ordered_aig: &OrderedAig<AigerLit>,
        prop_index: usize,
    ) -> Result<(), std::io::Error> {
        writeln!(f, "1")?;
        writeln!(f, "b{prop_index}")?;
        for latch_index in 0..ordered_aig.latches.len() {
            let aiger_var = AigerVar(Var::from_index(1 + ordered_aig.input_count + latch_index));
            let lit = aiger_map[aiger_var];
            write!(f, "{}", opt_to_char(witness.get_lit(Frame(0), lit)))?;
        }
        writeln!(f)?;
        for frame in (0..=witness.max_frame.0).map(Frame) {
            for input_id in (0..ordered_aig.input_count as u32).map(InputId) {
                write!(f, "{}", opt_to_char(witness.get_input(frame, input_id)))?;
            }
            writeln!(f)?;
        }
        writeln!(f, ".")?;
        Ok(())
    }

    #[test]
    fn test() {
        let depth = 10;
        let file = std::fs::File::open("aiger_test.aig").unwrap();
        let file = BufReader::new(file);
        let mut seq_ir = BitIr::default();
        let (aiger_map, ordered_aig) = AigerImporter::default()
            .import_binary(&mut seq_ir, file)
            .unwrap();
        seq_ir.refresh();
        let mut bmc = Bmc::new(Solver::default());
        for constraint in &ordered_aig.invariant_constraints {
            bmc.assume(constraint.lookup(|var| aiger_map[var]));
        }
        let bad_state_properties: Vec<_> = ordered_aig.bad_state_properties.iter().map(|lit| lit.lookup(|var| aiger_map[var])).collect();
        if let Some(witness) = bmc.run(&seq_ir, depth, &bad_state_properties) {
            let mut witness_file = BufWriter::new(std::fs::File::create("witness").unwrap());
            print_witness(&mut witness_file, &witness, &aiger_map, &ordered_aig, witness.property_index)
                .unwrap();
            witness_file.flush().unwrap();
        } else {
            panic!("unsat");
        }
    }
}
