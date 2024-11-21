use imctk_abc::sat::glucose2::{CnfOnly, Solver};
use imctk_ids::{id_vec::IdVec, Id, IdAlloc, IdRange};
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

impl Default for Bmc {
    fn default() -> Self {
        Self::new()
    }
}

impl Bmc {
    pub fn new() -> Self {
        Self::with_solver(Default::default())
    }
    pub fn with_solver(mut solver: Solver<'static, CnfOnly>) -> Self {
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
    pub fn assume(&mut self, assumptions: impl IntoIterator<Item = Lit>) {
        self.assumptions.extend(assumptions);
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
    pub fn max_frame(&self) -> Frame {
        self.max_frame
    }
    pub fn frames(&self) -> IdRange<Frame> {
        IdRange::from_index_range(0..1 + self.max_frame.0 as usize)
    }
    pub fn property_index(&self) -> usize {
        self.property_index
    }
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
