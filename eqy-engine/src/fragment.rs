use std::collections::BinaryHeap;

use imctk_ids::Id;
use imctk_ir::{
    env::Env,
    node::{
        fine::circuit::{
            And, AndNode, FineCircuitNodeBuilder, Init, InitNode, Input, InputNode, Reg, RegNode,
            SteadyInput, SteadyInputNode,
        },
        NodeId,
    },
    prelude::NodeBuilder,
    var::{Lit, Var},
};
use zwohash::{HashMap, HashSet};

use crate::{
    pdr_wrapper::{solve_with_pdr, PdrOptions, PdrResult},
    seq_sim::{model::extract_sim_model, on_demand::OnDemandSeqSim},
};

pub struct ExtractedFragment {
    pub frag_env: Env,
    pub frag_from_full: HashMap<Var, Lit>,
    pub full_from_frag: HashMap<Var, Lit>,
    pub used_equivs: Vec<[Lit; 2]>,
    pub conditions: HashSet<Lit>,
}

impl ExtractedFragment {
    pub fn new(
        window: impl IntoIterator<Item = Var>,
        full_env: &Env,
        mut repr: impl FnMut(Var) -> Lit,
    ) -> ExtractedFragment {
        let window = HashSet::from_iter(window);

        let mut vars = vec![];

        for &var in window.iter() {
            let var = full_env.var_defs().var_repr(var);
            if var == Var::FALSE {
                continue;
            }
            let node = full_env.def_node(var).unwrap();

            let mut define = !(node.dyn_cast::<InputNode>().is_some()
                || node.dyn_cast::<SteadyInputNode>().is_some());

            if define {
                node.dyn_foreach_input_var(&mut |input_var| {
                    define &= input_var == Var::FALSE || window.contains(&input_var);
                    define
                });
            }

            vars.push((var, define));
        }
        vars.sort_unstable_by_key(|&(var, _)| (full_env.var_defs().level_bound(var), var));

        let mut repr_map = <HashMap<Var, Lit>>::default();

        let mut def_reprs: Vec<Var> = vec![];

        let mut conditions: Vec<NodeId> = vec![];

        let mut extracted = ExtractedFragment {
            frag_env: Env::default(),
            frag_from_full: [(Var::FALSE, Lit::FALSE)].into_iter().collect(),
            full_from_frag: [(Var::FALSE, Lit::FALSE)].into_iter().collect(),
            used_equivs: vec![],
            conditions: HashSet::default(),
        };

        let mut input_map = <HashMap<Var, Lit>>::default();

        let mut frag_input_count = 0;
        let mut frag_steady_input_count = 0;

        {
            for &(full_var, define) in vars.iter() {
                let equiv_repr = repr(full_var);
                let full_repr = *repr_map
                    .entry(equiv_repr.var())
                    .or_insert(full_var ^ equiv_repr.pol())
                    ^ equiv_repr.pol();

                if full_repr == full_var.as_lit() {
                    let frag_lit;

                    if define {
                        def_reprs.push(full_var);
                        frag_lit = extracted
                            .frag_env
                            .fresh_var_with_level_bound(full_env.var_defs().level_bound(full_var))
                            .as_lit();
                        extracted
                            .full_from_frag
                            .insert(frag_lit.var(), full_var ^ frag_lit.pol());
                    } else {
                        frag_lit = *input_map.entry(full_var).or_insert_with(|| {
                            let frag_lit = if full_env.var_defs().is_steady(full_var) {
                                let term = SteadyInput::from_id_index(frag_steady_input_count);
                                frag_steady_input_count += 1;
                                extracted.frag_env.term(term)
                            } else {
                                let term = Input::from_id_index(frag_input_count);
                                frag_input_count += 1;
                                extracted.frag_env.term(term)
                            };
                            extracted
                                .full_from_frag
                                .insert(frag_lit.var(), full_var ^ frag_lit.pol());
                            frag_lit
                        })
                    };

                    extracted.frag_from_full.insert(full_var, frag_lit);
                } else {
                    extracted.used_equivs.push([full_var.as_lit(), full_repr]);
                    let frag_repr = full_repr.lookup(|var| extracted.frag_from_full[&var]);
                    extracted.frag_from_full.insert(full_var, frag_repr);
                    if define {
                        if let Some((node_id, _)) = full_env.def_node_with_id(full_var) {
                            conditions.push(node_id);
                        }
                    }
                }

                for node_id in full_env
                    .defs_index()
                    .find_non_primary_defs_unordered(full_var)
                {
                    let node = full_env.nodes().get_dyn(node_id).unwrap();

                    let mut define = !(node.dyn_cast::<InputNode>().is_some()
                        || node.dyn_cast::<SteadyInputNode>().is_some());

                    if define {
                        node.dyn_foreach_input_var(&mut |input_var| {
                            define &= input_var == Var::FALSE || window.contains(&input_var);
                            define
                        });
                    }

                    if define {
                        conditions.push(node_id);
                    }
                }
            }
        }

        for (reprs, frag_from_full) in [(&mut def_reprs, &extracted.frag_from_full)] {
            for &full_var in reprs.iter() {
                let node = full_env.def_node(full_var).unwrap();

                if let Some(reg) = node.dyn_cast::<RegNode>() {
                    let frag_output = reg.output.lookup(|var| frag_from_full[&var]);
                    let frag_init = reg.term.init.lookup(|var| extracted.frag_from_full[&var]);
                    let frag_next = extracted.frag_from_full[&reg.term.next];
                    extracted.frag_env.node(RegNode {
                        output: frag_output ^ frag_next.pol(),
                        term: Reg {
                            init: frag_init ^ frag_next.pol(),
                            next: frag_next.var(),
                        },
                    });
                } else if let Some(init) = node.dyn_cast::<InitNode>() {
                    let frag_output = init.output.lookup(|var| frag_from_full[&var]);
                    let frag_input = extracted.frag_from_full[&init.term.input];
                    extracted.frag_env.node(InitNode {
                        output: frag_output ^ frag_input.pol(),
                        term: Init {
                            input: frag_input.var(),
                        },
                    });
                } else {
                    node.dyn_add_to_env_with_var_map(&mut extracted.frag_env, &mut |var| {
                        frag_from_full[&var]
                    });
                }
            }
        }

        extracted.frag_env.rebuild_egraph();

        for lit in [extracted.frag_from_full.values_mut()]
            .into_iter()
            .flatten()
        {
            *lit = extracted.frag_env.lit_repr(*lit);
        }

        let mut folded_condition_map: HashMap<Lit, Lit> = Default::default();

        {
            for &node_id in conditions.iter() {
                let node = full_env.nodes().get_dyn(node_id).unwrap();

                let frag_output;
                let frag_alt_output;
                if let Some(reg) = node.dyn_cast::<RegNode>() {
                    frag_output = reg.output.lookup(|var| extracted.frag_from_full[&var]);
                    let frag_init = reg.term.init.lookup(|var| extracted.frag_from_full[&var]);
                    let frag_next = extracted.frag_from_full[&reg.term.next];
                    frag_alt_output = extracted.frag_env.reg(frag_init, frag_next);
                } else if let Some(init) = node.dyn_cast::<InitNode>() {
                    frag_output = init.output.lookup(|var| extracted.frag_from_full[&var]);
                    let frag_input = extracted.frag_from_full[&init.term.input];
                    frag_alt_output = extracted.frag_env.init(frag_input);
                } else {
                    let term = node.dyn_term().unwrap();
                    frag_output = node
                        .output_lit()
                        .unwrap()
                        .lookup(|var| extracted.frag_from_full[&var]);
                    frag_alt_output = term
                        .dyn_add_to_env_with_var_map(&mut extracted.frag_env, &mut |var| {
                            extracted.frag_from_full[&var]
                        });
                }

                if frag_output == frag_alt_output {
                    continue;
                }

                let mut condition_lit = !extracted.frag_env.xor([frag_output, frag_alt_output]);

                condition_lit = *folded_condition_map
                    .entry(condition_lit)
                    .or_insert_with(|| {
                        let condition_d =
                            extracted.frag_env.fresh_var_with_max_level_bound().as_lit();
                        let condition_q = extracted.frag_env.reg(Lit::TRUE, condition_d);
                        extracted.frag_env.node(AndNode {
                            output: condition_d,
                            term: And {
                                inputs: [condition_q, condition_lit].into(),
                            },
                        });
                        condition_d
                    });

                extracted.conditions.insert(condition_lit);
            }
        }

        extracted
    }

    pub fn solve_with_pdr_lazy(
        &mut self,
        mut frag_target: Lit,
        options: &PdrOptions,
    ) -> Option<PdrResult> {
        let mut sim_data = None;
        let mut merge_heap = <BinaryHeap<(u32, Lit)>>::default();
        let initial_frag_target = frag_target;

        loop {
            let (result, _pdr_stats) = solve_with_pdr(&self.frag_env, [frag_target], options);

            let Some(PdrResult::Cex(cex)) = &result else {
                return result;
            };

            let (sim_model, sim, conditions) = sim_data.get_or_insert_with(|| {
                let sim_model =
                    extract_sim_model(&self.frag_env, self.frag_env.var_defs().repr_vars());
                let sim = OnDemandSeqSim::default();
                let conditions = self.conditions.clone();
                (sim_model, sim, conditions)
            });

            let cex_frame = cex.inputs.next_unused_key().prev().unwrap();

            sim.reset();

            for (frame, inputs) in cex.inputs.iter() {
                for &frag_input_lit in inputs.iter() {
                    let sim_input_lit =
                        frag_input_lit.lookup(|var| sim_model.sim_from_env[var].unwrap());
                    let prev = sim
                        .fix_input_lit_value(sim_model, frame, sim_input_lit, true)
                        .unwrap();
                    assert!(prev.is_none());
                }
            }

            let sim_target = initial_frag_target.lookup(|var| sim_model.sim_from_env[var].unwrap());
            assert!(sim.lit_value(sim_model, cex_frame, sim_target));

            merge_heap.clear();
            merge_heap.push((
                !self.frag_env.var_defs().level_bound(frag_target.var()),
                frag_target,
            ));

            let mut violated = false;

            conditions.retain(|&frag_condition| {
                let sim_condition =
                    frag_condition.lookup(|var| sim_model.sim_from_env[var].unwrap());

                let retain = sim.lit_value(sim_model, cex_frame, sim_condition);
                if !retain {
                    violated = true;

                    merge_heap.push((
                        !self.frag_env.var_defs().level_bound(frag_condition.var()),
                        frag_condition,
                    ));
                }
                retain
            });

            if !violated {
                return result;
            }

            while merge_heap.len() > 1 {
                let a = merge_heap.pop().unwrap().1;
                let b = merge_heap.pop().unwrap().1;
                let lit = self.frag_env.and([a, b]);
                merge_heap.push((!self.frag_env.var_defs().level_bound(lit.var()), lit));
            }

            frag_target = merge_heap.pop().unwrap_or((0, Lit::TRUE)).1;
        }
    }
}
