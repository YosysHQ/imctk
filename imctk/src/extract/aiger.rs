//! Extraction of and-inverter-graphs (AIGs) to produce AIGER files.

use flussab_aiger::aig::{OrderedAig, OrderedAndGate, OrderedLatch};
use imctk_ids::{id_vec::IdVec, Id};
use imctk_ir::{
    env::Env,
    node::fine::circuit::{AndNode, InitNode, InputNode, RegNode, SteadyInputNode, XorNode},
    var::{Lit, Var},
};
use imctk_util::vec_sink::VecSink;
use zwohash::HashMap;

/// An extracted AIG together with extraction metadata.
pub struct ExtractedAig {
    /// The extracted AIG
    pub aig: flussab_aiger::aig::OrderedAig<Lit>,
    /// Maps environment variables to equivalent AIG literals
    pub aig_from_env: IdVec<Var, Option<Lit>>,
    /// Maps enviornment variables to environment literals that are equivalent in the initial time
    /// step and are used to represent the initial value in the extracted AIG
    pub init_repr_for_steady: HashMap<Var, Lit>,
}

/// Extracts an AIGER compatible sequential AIG for the given sequence of output literals.
///
/// This will tranparently expand imctk Xor terms into an equivalent network of 3 AIG And gates.
pub fn extract_aiger(
    env: &Env,
    outputs: impl IntoIterator<Item = Lit>,
    mut expand: impl FnMut(Var) -> bool,
) -> ExtractedAig {
    // TODO keep expand and/or have alternative for compacting inputs?

    let mut outputs = Vec::from_iter(outputs.into_iter().map(|lit| env.var_defs().lit_repr(lit)));

    let mut order_inputs: Vec<Var> = vec![];
    let mut order_unexpanded_inputs: Vec<Var> = vec![];
    let mut order_steady_inputs: Vec<Var> = vec![];
    let mut order_unexpanded_steady_inputs: Vec<Var> = vec![];

    let mut order_const_init_regs: Vec<Var> = vec![];
    let mut order_steady_init_regs: Vec<Var> = vec![];
    let mut order_unexpanded_init_regs: Vec<Var> = vec![];

    let mut order_early_emulated_init_regs: Vec<Var> = vec![];

    let mut order: Vec<Var> = vec![];

    let mut visited: IdVec<Var, Option<bool>> = Default::default();

    visited.resize(env.var_defs().len(), Some(false));

    visited[Var::FALSE] = Some(true);

    struct TraversalFrame {
        incoming_stack_level: usize,
        current: Var,
    }

    let mut traversal_stack: Vec<TraversalFrame> = vec![];
    let mut incoming_stack: Vec<Var> = vec![];

    let mut roots = Vec::from_iter(outputs.iter().map(|lit| lit.var()));

    let mut reg_count = 0;

    let mut max_steady_input = None;
    let mut max_input = None;

    let mut uses_emulated_reg_init = false;

    while let Some(mut current) = roots.pop() {
        'traversal: loop {
            current = env.var_defs().var_repr(current);
            let mut incoming_stack_level;

            'enter: {
                macro_rules! traversal_return {
                    () => {
                        if let Some(frame) = traversal_stack.pop() {
                            current = frame.current;
                            incoming_stack_level = frame.incoming_stack_level;
                            break 'enter;
                        } else {
                            break 'traversal;
                        }
                    };
                }

                if visited[current].expect("cycle during AIGER extraction") {
                    traversal_return!();
                }
                visited[current] = None;
                incoming_stack_level = incoming_stack.len();

                if !expand(current) {
                    if env.var_defs().is_steady(current) {
                        order_unexpanded_steady_inputs.push(current);
                        reg_count += 1;
                    } else {
                        order_unexpanded_inputs.push(current);
                    }
                    visited[current] = Some(true);
                    traversal_return!();
                }

                let node = env
                    .def_node(current)
                    .expect("undefined var during AIGER extraction");

                if let Some(reg) = node.dyn_cast::<RegNode>() {
                    roots.push(reg.term.next);

                    let init_var = reg.term.init.var();
                    if init_var == Var::FALSE {
                        order_const_init_regs.push(current);
                        reg_count += 1;
                        visited[current] = Some(true);
                        traversal_return!();
                    } else if !expand(init_var) && env.var_defs().is_steady(init_var) {
                        order_unexpanded_init_regs.push(current);
                        reg_count += 1;
                        visited[current] = Some(true);
                        traversal_return!();
                    } else if let Some(steady_input) = env
                        .def_node(init_var)
                        .expect("undefined var during AIGER extraction")
                        .dyn_cast::<SteadyInputNode>()
                    {
                        max_steady_input = max_steady_input.max(Some(steady_input.term));

                        order_steady_init_regs.push(current);
                        reg_count += 1;
                        visited[current] = Some(true);
                        traversal_return!();
                    }

                    incoming_stack.push(reg.term.init.var());
                    reg_count += 1;
                    uses_emulated_reg_init = true;
                } else if let Some(init) = node.dyn_cast::<InitNode>() {
                    incoming_stack.push(init.term.input);
                    reg_count += 1;
                } else if let Some(input) = node.dyn_cast::<InputNode>() {
                    max_input = max_input.max(Some(input.term));
                    order_inputs.push(current);
                    visited[current] = Some(true);
                    traversal_return!();
                } else if let Some(steady_input) = node.dyn_cast::<SteadyInputNode>() {
                    max_steady_input = max_steady_input.max(Some(steady_input.term));
                    order_steady_inputs.push(current);
                    reg_count += 1;
                    uses_emulated_reg_init = true;
                    visited[current] = Some(true);
                    traversal_return!();
                } else {
                    node.dyn_append_unguarded_input_vars(VecSink::new(&mut incoming_stack));
                }
            }

            loop {
                if incoming_stack.len() > incoming_stack_level {
                    let next = incoming_stack.pop().unwrap();

                    traversal_stack.push(TraversalFrame {
                        incoming_stack_level,
                        current,
                    });
                    current = next;
                    continue 'traversal;
                }

                visited[current] = Some(true);
                order.push(current);

                if let Some(frame) = traversal_stack.pop() {
                    current = frame.current;
                    incoming_stack_level = frame.incoming_stack_level;
                } else {
                    break 'traversal;
                }
            }
        }
    }

    let mut init_repr_for_steady: HashMap<Var, Lit> = Default::default();

    for &var in order_steady_inputs.iter() {
        init_repr_for_steady.insert(var, var.as_lit());
    }

    for &var in order_unexpanded_steady_inputs.iter() {
        init_repr_for_steady.insert(var, var.as_lit());
    }

    for regs in [&mut order_steady_init_regs, &mut order_unexpanded_init_regs] {
        regs.retain(|&var| {
            let reg = env.def_node(var).unwrap().dyn_cast::<RegNode>().unwrap();

            match init_repr_for_steady.entry(reg.term.init.var()) {
                std::collections::hash_map::Entry::Occupied(_) => {
                    order_early_emulated_init_regs.push(var);
                    false
                }
                std::collections::hash_map::Entry::Vacant(entry) => {
                    entry.insert(var ^ reg.term.init.pol() ^ reg.output.pol());
                    true
                }
            }
        });
    }

    uses_emulated_reg_init |= !order_early_emulated_init_regs.is_empty();
    reg_count += uses_emulated_reg_init as usize;

    let input_count = (order_inputs.len() + order_unexpanded_inputs.len()).max(
        max_input
            .map(|input| input.id_index() + 1)
            .unwrap_or_default(),
    );

    reg_count = reg_count.max(
        max_steady_input
            .map(|input| input.id_index() + 1)
            .unwrap_or_default(),
    );

    let mut aig_from_env: IdVec<Var, Option<Lit>> = Default::default();
    aig_from_env.resize(env.var_defs().len(), None);
    aig_from_env[Var::FALSE] = Some(Lit::FALSE);

    let mut inputs: Vec<bool> = vec![false; input_count];

    for &var in order_inputs.iter() {
        let input = env.def_node(var).unwrap().dyn_cast::<InputNode>().unwrap();

        let input_index = input.term.id_index();

        let aiger_input = Var::from_index(1 + input_index);
        inputs[input_index] = true;
        aig_from_env[var] = Some(aiger_input ^ input.output.pol());
    }

    let mut unused_inputs = Vec::from_iter(
        inputs
            .iter()
            .enumerate()
            .rev()
            .flat_map(|(input_index, &used)| (!used).then_some(input_index)),
    );

    for &var in order_unexpanded_inputs.iter() {
        let input_index = unused_inputs.pop().unwrap();

        let aiger_input = Var::from_index(1 + input_index).as_lit();
        inputs[input_index] = true;
        aig_from_env[var] = Some(aiger_input)
    }

    let mut latches: Vec<OrderedLatch<Lit>> = vec![];

    latches.resize(
        reg_count,
        OrderedLatch {
            next_state: Lit::FALSE,
            initialization: Some(false),
        },
    );

    let latch_offset = 1 + input_count;

    for &var in order_steady_inputs.iter() {
        let steady_input = env
            .def_node(var)
            .unwrap()
            .dyn_cast::<SteadyInputNode>()
            .unwrap();

        let latch_index = steady_input.term.id_index();
        let aiger_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

        latches[latch_index].initialization = None;
        latches[latch_index].next_state = steady_input.output;
        aig_from_env[var] = Some(aiger_latch_output ^ steady_input.output.pol());
    }

    for &var in order_steady_init_regs.iter() {
        let reg = env.def_node(var).unwrap().dyn_cast::<RegNode>().unwrap();
        let steady_input = env
            .def_node(reg.term.init.var())
            .unwrap()
            .dyn_cast::<SteadyInputNode>()
            .unwrap();

        let init_pol = steady_input.output.pol() ^ reg.term.init.pol();

        let latch_index = steady_input.term.id_index();

        let aiger_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

        latches[latch_index].initialization = None;
        latches[latch_index].next_state = reg.term.next.as_lit() ^ init_pol;
        aig_from_env[var] = Some(aiger_latch_output ^ reg.output.pol() ^ init_pol);
    }

    let mut unused_latches = Vec::from_iter(
        latches
            .iter()
            .enumerate()
            .rev()
            .flat_map(|(latch_index, latch)| latch.initialization.is_some().then_some(latch_index)),
    );

    for &var in order_unexpanded_steady_inputs.iter() {
        let latch_index = unused_latches.pop().unwrap();

        let aiger_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

        latches[latch_index].initialization = None;
        latches[latch_index].next_state = var.as_lit();
        aig_from_env[var] = Some(aiger_latch_output);
    }

    for &var in order_unexpanded_init_regs.iter() {
        let reg = env.def_node(var).unwrap().dyn_cast::<RegNode>().unwrap();

        let latch_index = unused_latches.pop().unwrap();

        let aiger_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

        latches[latch_index].initialization = None;
        latches[latch_index].next_state = reg.term.next.as_lit();
        aig_from_env[var] = Some(aiger_latch_output ^ reg.output.pol());
    }

    for &var in order_const_init_regs.iter() {
        let reg = env.def_node(var).unwrap().dyn_cast::<RegNode>().unwrap();

        let latch_index = unused_latches.pop().unwrap();

        let aiger_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

        latches[latch_index].initialization = Some(reg.term.init != Lit::FALSE);
        latches[latch_index].next_state = reg.term.next.as_lit();
        aig_from_env[var] = Some(aiger_latch_output ^ reg.output.pol());
    }

    let mut emulated_init_pulse = Lit::FALSE;

    if uses_emulated_reg_init {
        let latch_index = unused_latches.pop().unwrap();

        emulated_init_pulse = Var::from_index(latch_offset + latch_index).as_lit();

        latches[latch_index].initialization = Some(true);
        latches[latch_index].next_state = Lit::FALSE;
    }

    let mut and_gates = vec![];
    let and_offset = latch_offset + latches.len();

    for &var in [order_early_emulated_init_regs.iter(), order.iter()]
        .into_iter()
        .flatten()
    {
        let node = env.def_node(var).unwrap();

        if let Some(reg) = node.dyn_cast::<RegNode>() {
            let aig_init = reg
                .term
                .init
                .lookup(|var| init_repr_for_steady.get(&var).unwrap())
                .lookup(|var| aig_from_env[var].unwrap());

            let latch_index = unused_latches.pop().unwrap();

            let aiger_raw_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

            latches[latch_index].initialization = Some(false);
            latches[latch_index].next_state = reg.term.next.as_lit();

            let aiger_masked_init = Var::from_index(and_offset + and_gates.len()).as_lit();

            and_gates.push(OrderedAndGate {
                inputs: [aig_init, emulated_init_pulse],
            });

            let aiger_emulated_latch_output =
                !Var::from_index(and_offset + and_gates.len()).as_lit();
            and_gates.push(OrderedAndGate {
                inputs: [!aiger_masked_init, !aiger_raw_latch_output],
            });

            aig_from_env[var] = Some(aiger_emulated_latch_output ^ reg.output.pol());
        } else if let Some(and) = node.dyn_cast::<AndNode>() {
            let inputs = and
                .term
                .inputs
                .into_values()
                .map(|lit| lit.lookup(|var| aig_from_env[var].unwrap()));

            let aiger_and_output = Var::from_index(and_offset + and_gates.len()).as_lit();

            and_gates.push(OrderedAndGate { inputs });

            aig_from_env[var] = Some(aiger_and_output ^ and.output.pol());
        } else if let Some(xor) = node.dyn_cast::<XorNode>() {
            let [a, b] = xor
                .term
                .inputs
                .into_values()
                .map(|var| aig_from_env[var].unwrap());

            let aiger_xor_both = Var::from_index(and_offset + and_gates.len()).as_lit();
            and_gates.push(OrderedAndGate { inputs: [a, b] });

            let aiger_xor_neither = Var::from_index(and_offset + and_gates.len()).as_lit();
            and_gates.push(OrderedAndGate { inputs: [!a, !b] });

            let aiger_xor_output = Var::from_index(and_offset + and_gates.len()).as_lit();
            and_gates.push(OrderedAndGate {
                inputs: [!aiger_xor_both, !aiger_xor_neither],
            });

            aig_from_env[var] = Some(aiger_xor_output ^ xor.output.pol());
        } else if let Some(init) = node.dyn_cast::<InitNode>() {
            let aig_init = init
                .term
                .input
                .as_lit()
                .lookup(|var| init_repr_for_steady.get(&var).unwrap())
                .lookup(|var| aig_from_env[var].unwrap());
            let latch_index = unused_latches.pop().unwrap();

            let aiger_raw_latch_output = Var::from_index(latch_offset + latch_index).as_lit();

            latches[latch_index].initialization = Some(false);
            latches[latch_index].next_state = init.output;

            let aiger_masked_init = Var::from_index(and_offset + and_gates.len()).as_lit();

            and_gates.push(OrderedAndGate {
                inputs: [aig_init, emulated_init_pulse],
            });

            let aiger_emulated_latch_output =
                !Var::from_index(and_offset + and_gates.len()).as_lit();
            and_gates.push(OrderedAndGate {
                inputs: [!aiger_masked_init, !aiger_raw_latch_output],
            });

            aig_from_env[var] = Some(aiger_emulated_latch_output ^ init.output.pol());
        } else {
            panic!("unsupported node {node:?}");
        }
    }

    for latch in latches.iter_mut() {
        latch.next_state = latch.next_state.lookup(|var| aig_from_env[var].unwrap());
    }

    for output in outputs.iter_mut() {
        *output = output.lookup(|var| aig_from_env[var].unwrap());
    }

    let aig = OrderedAig {
        max_var_index: and_offset + and_gates.len() - 1,
        input_count,
        latches,
        outputs,
        and_gates,
        ..Default::default()
    };

    ExtractedAig {
        aig,
        aig_from_env,
        init_repr_for_steady,
    }
}

/// An extracted combinational-only AIG together with extraction metadata.
pub struct ExtractedCombAig {
    /// The extracted AIG
    pub aig: OrderedAig<Lit>,
    /// Maps environment variables to equivalent AIG literals
    pub aig_from_env: IdVec<Var, Option<Lit>>,
    /// Maps AIG variables to equivalent environment literals
    pub env_from_aig: IdVec<Var, Lit>,
}

/// Extracts a combinational AIG containing only the given topologically ordered variables.
// TODO document exact requirements and behavior
pub fn extract_comb_aiger(
    env: &Env,
    vars: impl IntoIterator<Item = Var>,
    mut force_input: impl FnMut(Var) -> bool,
) -> ExtractedCombAig {
    let mut aig_from_env: IdVec<Var, Option<Lit>> = Default::default();
    let mut env_from_aig: IdVec<Var, Lit> = Default::default();
    aig_from_env.resize(env.var_defs().len(), None);
    aig_from_env[Var::FALSE] = Some(Lit::FALSE);

    env_from_aig.push(Lit::FALSE);

    let mut vars = Vec::from_iter(vars);
    let mut comb_logic = vec![];

    vars.retain(|&var| {
        let mut as_input = true;
        aig_from_env[var] = Some(Lit::FALSE); // placeholder
        if !force_input(var) {
            if let Some(def_node) = env.def_node(var) {
                if let Some(and) = def_node.dyn_cast::<AndNode>() {
                    if and
                        .term
                        .inputs
                        .iter()
                        .all(|&lit| aig_from_env[lit.var()].is_some())
                    {
                        as_input = false;
                        comb_logic.push(var);
                    }
                }
            }
        }
        as_input
    });

    let input_count = vars.len();

    let and_offset = input_count + 1;

    let mut and_gates = vec![];

    for var in vars {
        let (aig_var, _) = env_from_aig.push(var.as_lit());
        aig_from_env[var] = Some(aig_var.as_lit());
    }

    for var in comb_logic {
        let def_node = env.def_node(var).unwrap();
        if let Some(and) = def_node.dyn_cast::<AndNode>() {
            let (aig_var, _) = env_from_aig.push(and.output);

            and_gates.push(OrderedAndGate {
                inputs: and
                    .term
                    .inputs
                    .into_values()
                    .map(|lit| lit.lookup(|var| aig_from_env[var].unwrap())),
            });
            aig_from_env[var] = Some(aig_var ^ and.output.pol());
        } else {
            unreachable!()
        }
    }

    let aig = OrderedAig {
        max_var_index: and_offset + and_gates.len() - 1,
        input_count,
        and_gates,
        ..Default::default()
    };

    ExtractedCombAig {
        aig,
        aig_from_env,
        env_from_aig,
    }
}
