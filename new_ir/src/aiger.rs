use std::io;

use flussab_aiger::{aig::OrderedAig, ParseError};
use imctk_ids::{id_vec::IdVec, Id, IdAlloc, IdRange};
use imctk_lit::{Lit, Var};

use crate::{
    bitlevel::{AndTerm, BitlevelTerm, InputId, Node, Reg, SteadyInputId},
    ir::BitIr,
};

#[derive(Debug, Id)]
#[repr(transparent)]
pub struct AigerVar(pub Var);
#[derive(Debug, Id)]
#[repr(transparent)]
pub struct AigerLit(pub Lit);

impl AigerVar {
    pub const FALSE: AigerVar = AigerVar(Var::FALSE);
    pub fn from_index(index: usize) -> Self {
        AigerVar(Var::from_index(index))
    }
}

impl AigerLit {
    pub fn var(self) -> AigerVar {
        AigerVar(self.0.var())
    }
    pub fn lookup<T: imctk_lit::Negate>(self, f: impl FnOnce(AigerVar) -> T) -> T::Negated {
        self.0.lookup(|var| f(AigerVar(var)))
    }
}

impl flussab_aiger::Lit for AigerLit {
    const MAX_CODE: usize = Lit::MAX_CODE;
    fn from_code(code: usize) -> Self {
        AigerLit(Lit::from_code(code))
    }
    fn code(self) -> usize {
        self.0.code()
    }
}

#[derive(Default, Debug)]
pub struct AigerImporter {
    var_map: IdVec<AigerVar, Option<Lit>>,
    latch_init: IdVec<SteadyInputId, Option<Lit>>,
}

#[derive(Debug)]
pub struct AigerMapping {
    var_map: IdVec<AigerVar, Lit>,
    aig: OrderedAig<AigerLit>,
}

impl AigerImporter {
    pub fn import_binary(
        self,
        ir: &mut BitIr,
        file: impl io::Read,
    ) -> Result<AigerMapping, ParseError> {
        let parser = flussab_aiger::binary::Parser::from_read(file, Default::default())?;
        let aig = parser.parse()?;
        Ok(self.import_ordered(ir, aig))
    }
    pub fn import_ordered(mut self, ir: &mut BitIr, aig: OrderedAig<AigerLit>) -> AigerMapping {
        let mut egraph = ir.egraph_mut();

        self.var_map
            .resize_with(aig.max_var_index + 1, Default::default);
        self.var_map[AigerVar::FALSE].get_or_insert(Lit::FALSE);
        let aiger_vars = IdAlloc::<AigerVar>::new();
        assert_eq!(aiger_vars.alloc(), AigerVar::FALSE);

        for (input_index, input_var) in aiger_vars.alloc_range(aig.input_count).iter().enumerate() {
            let input_id = InputId(input_index as u32);
            self.var_map[input_var]
                .get_or_insert_with(|| egraph.insert_term(BitlevelTerm::Input(input_id)));
        }

        self.latch_init
            .resize_with(aig.latches.len(), Default::default);

        let latch_output_vars = aiger_vars.alloc_range(aig.latches.len());
        let latch_init_inputs =
            IdRange::from(SteadyInputId(0)..SteadyInputId(aig.latches.len() as u32));

        let latches = aig
            .latches
            .iter()
            .zip(latch_init_inputs)
            .zip(latch_output_vars);

        for ((aiger_latch, init), output_var) in latches {
            let init_lit =
                *self.latch_init[init].get_or_insert_with(|| match aiger_latch.initialization {
                    Some(constant) => Lit::FALSE ^ constant,
                    None => egraph.insert_term(BitlevelTerm::SteadyInput(init)),
                });
            let next = aiger_latch.next_state.lookup(|var| {
                *self.var_map[var].get_or_insert_with(|| egraph.fresh_var().as_lit())
            });
            let term = BitlevelTerm::Reg(Reg {
                next,
                init: init_lit,
            });
            if let Some(lit) = self.var_map[output_var] {
                egraph.insert_node(Node { output: lit, term });
            } else {
                self.var_map[output_var] = Some(egraph.insert_term(term));
            }
        }

        for aiger_and in aig.and_gates.iter() {
            let output_aiger_var = aiger_vars.alloc();
            let inputs = aiger_and
                .inputs
                .map(|lit| lit.lookup(|var| self.var_map[var].unwrap()));
            let term = BitlevelTerm::And(AndTerm(inputs.into()));
            if let Some(lit) = self.var_map[output_aiger_var] {
                egraph.insert_node(Node { output: lit, term });
            } else {
                self.var_map[output_aiger_var] = Some(egraph.insert_term(term));
            }
        }

        let var_map = IdVec::from_vec(
            self.var_map
                .into_values()
                .into_iter()
                .map(|lit| lit.unwrap())
                .collect(),
        );

        AigerMapping { var_map, aig }
    }
}

impl AigerMapping {
    pub fn ordered_aig(&self) -> &OrderedAig<AigerLit> {
        &self.aig
    }
    pub fn into_ordered_aig(self) -> OrderedAig<AigerLit> {
        self.aig
    }
    pub fn aiger_lit_to_ir(&self, lit: AigerLit) -> Lit {
        lit.lookup(|var| self.var_map[var])
    }
    pub fn aiger_input_to_ir(&self, input: usize) -> InputId {
        InputId(input.try_into().unwrap())
    }
    pub fn bad_state_properties(&self) -> Vec<Lit> {
        self.aig
            .bad_state_properties
            .iter()
            .map(|&lit| self.aiger_lit_to_ir(lit))
            .collect()
    }
    pub fn invariant_constraints(&self) -> Vec<Lit> {
        self.aig
            .invariant_constraints
            .iter()
            .map(|&lit| self.aiger_lit_to_ir(lit))
            .collect()
    }
    pub fn inputs(&self) -> IdRange<InputId> {
        IdRange::from_index_range(0..self.aig.input_count)
    }
    pub fn latch_outputs(&self) -> &[Lit] {
        let start = 1 + self.aig.input_count;
        let end = start + self.aig.latches.len();
        &self.var_map.values()[start..end]
    }
}
