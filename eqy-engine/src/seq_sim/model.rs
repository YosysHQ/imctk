//! Linear sequential circuit simulation model.
//!
//! This is a cricuit representation dedicated to sequential simulation. It only supports And and
//! Xor gates and stores the combinational part of the circuit in topologically sorted order. This
//! makes it a static representation, unlike an [environment][Env], but offers significant
//! performance benefits when running simulation for multiple time steps.
use imctk_extract::extract_topo_sorted_primary_defs;
use imctk_ids::{id_vec::IdVec, indexed_id_vec::IndexedIdVec};
use imctk_ir::{
    env::Env,
    node::fine::circuit::{AndNode, InitNode, RegNode, XorNode},
    var::{Lit, Var},
};
use imctk_util::unordered_pair::UnorderedPair;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct XaigStep {
    pub inputs: [Lit; 2],
}

impl std::fmt::Debug for XaigStep {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_and() {
            f.debug_tuple("And")
                .field(&self.inputs[0])
                .field(&self.inputs[1])
                .finish()
        } else {
            f.debug_tuple("Xor")
                .field(&self.inputs[1])
                .field(&self.inputs[0])
                .finish()
        }
    }
}

impl XaigStep {
    pub fn and(lits: UnorderedPair<Lit>) -> Self {
        Self {
            inputs: [lits[0], lits[1]],
        }
    }

    pub fn xor(lits: UnorderedPair<Lit>) -> Self {
        if lits[0] == lits[1] {
            Self {
                inputs: [Lit::FALSE; 2],
            }
        } else {
            Self {
                inputs: [lits[1], lits[0]],
            }
        }
    }

    pub fn is_and(&self) -> bool {
        self.inputs[0] <= self.inputs[1]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Step {
    Xaig(XaigStep),
    Other(Option<Lit>),
}

/// Linear sequential circuit simulation model.
///
/// See the [module level documentation][self] for a general overview.
#[derive(Default)]
pub struct SimModel {
    pub init_steps: IdVec<Var, Step>,
    pub next_steps: IdVec<Var, Step>,
    pub zero_values: IdVec<Var, bool>,
    pub sim_from_env: IdVec<Var, Option<Lit>>,
    pub env_from_sim: IdVec<Var, Lit>,
    pub read_state: IdVec<Var, Var>,
}

pub fn extract_sim_model(env: &Env, targets: impl IntoIterator<Item = Var>) -> SimModel {
    let targets: Vec<_> = targets.into_iter().collect();
    let order = extract_topo_sorted_primary_defs(env, targets.iter().copied());

    let mut init_steps: IdVec<Var, Step> = IdVec::default();
    let mut next_steps: IdVec<Var, Step> = IdVec::default();
    let mut zero_values: IdVec<Var, bool> = IdVec::default();

    let mut sim_from_env: IdVec<Var, Option<Lit>> = Default::default();
    let mut env_from_sim: IdVec<Var, Lit> = Default::default();

    let mut regs: Vec<Var> = vec![];
    let mut inputs: Vec<Var> = vec![];
    let mut steady_inputs: Vec<Var> = vec![];
    let mut read_state: IndexedIdVec<Var, Var> = Default::default();

    env_from_sim.push(Lit::FALSE);
    sim_from_env.push(Some(Lit::FALSE));
    zero_values.push(false);

    let step = Step::Xaig(XaigStep::and([Lit::FALSE; 2].into()));

    init_steps.push(step);
    next_steps.push(step);

    for var in order {
        let steady = env.var_defs().is_steady(var);

        let node = env.def_node(var).unwrap();
        if let Some(and) = node.dyn_cast::<AndNode>() {
            let step_var = env_from_sim.push(var ^ and.output.pol()).0;
            *sim_from_env.grow_for_key(var) = Some(step_var ^ and.output.pol());

            let inputs = and
                .term
                .inputs
                .map(|input| input.lookup(|var| sim_from_env[var].unwrap()));
            let step = Step::Xaig(XaigStep::and(inputs));
            init_steps.push(step);

            if steady {
                next_steps.push(Step::Other(Some(step_var.as_lit())));
            } else {
                next_steps.push(step);
            }

            let [zero_a, zero_b] = inputs
                .into_values()
                .map(|input| input.lookup(|var| zero_values[var]));
            zero_values.push(zero_a & zero_b);
        } else if let Some(xor) = node.dyn_cast::<XorNode>() {
            let step_var = env_from_sim.push(var ^ xor.output.pol()).0;
            *sim_from_env.grow_for_key(var) = Some(step_var ^ xor.output.pol());

            let inputs = xor.term.inputs.map(|input| sim_from_env[input].unwrap());
            let step = Step::Xaig(XaigStep::xor(inputs));
            init_steps.push(step);

            if steady {
                next_steps.push(Step::Other(Some(step_var.as_lit())));
            } else {
                next_steps.push(step);
            }

            let [zero_a, zero_b] = inputs
                .into_values()
                .map(|input| input.lookup(|var| zero_values[var]));
            zero_values.push(zero_a ^ zero_b);
        } else if let Some(init) = node.dyn_cast::<InitNode>() {
            let step_var = env_from_sim.push(var ^ init.output.pol()).0;
            *sim_from_env.grow_for_key(var) = Some(step_var ^ init.output.pol());

            let input = sim_from_env[init.term.input].unwrap();
            init_steps.push(Step::Xaig(XaigStep::and([Lit::TRUE, input].into())));

            next_steps.push(Step::Other(Some(step_var.as_lit())));

            let zero = input.lookup(|input| zero_values[input]);
            zero_values.push(zero);
        } else if let Some(reg) = node.dyn_cast::<RegNode>() {
            let step_var = env_from_sim.push(var ^ reg.output.pol()).0;
            *sim_from_env.grow_for_key(var) = Some(step_var ^ reg.output.pol());

            let init_input = reg.term.init.lookup(|var| sim_from_env[var].unwrap());
            init_steps.push(Step::Xaig(XaigStep::and([Lit::TRUE, init_input].into())));
            next_steps.push(Step::Other(None));
            regs.push(step_var);

            let zero = init_input.lookup(|input| zero_values[input]);
            zero_values.push(zero);
        } else {
            let output_lit = node.output_lit().unwrap();
            let step_var = env_from_sim.push(var ^ output_lit.pol()).0;
            *sim_from_env.grow_for_key(var) = Some(step_var ^ output_lit.pol());

            init_steps.push(Step::Other(None));
            if steady {
                next_steps.push(Step::Other(Some(step_var.as_lit())));
                steady_inputs.push(step_var);
            } else {
                next_steps.push(Step::Other(None));
                inputs.push(step_var);
            }

            zero_values.push(false);
        }
    }

    for &var in regs.iter() {
        let env_lit = env_from_sim[var];
        let node = env.def_node(env_lit.var()).unwrap();
        let reg = node.dyn_cast::<RegNode>().unwrap();
        assert_eq!(reg.output, env_lit);

        let from = sim_from_env[reg.term.next].unwrap();

        next_steps[var] = Step::Other(Some(from));
    }

    for (_step_var, step) in next_steps.iter_mut() {
        if let Step::Other(Some(prev)) = step {
            *prev = (read_state.insert(prev.var()).0) ^ prev.pol();
        }
    }

    SimModel {
        init_steps,
        next_steps,
        zero_values,
        sim_from_env,
        env_from_sim,
        read_state: read_state.into_id_vec(),
    }
}
