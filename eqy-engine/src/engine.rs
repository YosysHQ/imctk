use std::{
    fmt::{Debug, Display, Write},
    ops::Range,
};

use crate::{
    circuit_sat::CircuitSat,
    fragment::ExtractedFragment,
    pdr_wrapper::{solve_with_pdr, PdrCex, PdrOptions, PdrResult},
    refinement::{
        bmc::BmcRefinement, rarity_sim::RaritySimRefinement, EnvVarRefinement, RefinementContext,
    },
    seq_sim::{self, on_demand::OnDemandSeqSim},
    unroll::{Unroll, UnrollMode},
    TimeStep,
};
use imctk_extract::select_primary_defs;
use imctk_ids::{id_vec::IdVec, indexed_id_vec::IndexedIdVec, Id};
use imctk_ir::{
    env::Env,
    node::fine::circuit::FineCircuitNodeBuilder,
    prelude::{DynNode, NodeBuilderDyn},
    var::{Lit, Var},
};
use rand::{rngs::SmallRng, Rng, SeedableRng};
use zwohash::{HashMap, HashSet};

pub struct EngineOptions {
    pub rarity_sim_rounds: usize,
    pub bmc_depth: usize,
    pub window: Range<usize>,
}

pub fn run_eqy_engine(
    env: &mut Env,
    target_properties: &[Lit],
    options: EngineOptions,
) -> Option<PdrResult> {
    let mut refine = EnvVarRefinement::default();
    for var in env.var_defs().repr_vars() {
        refine.insert_item(var);
    }

    let mut sim_model = seq_sim::model::extract_sim_model(env, env.var_defs().repr_vars());

    let mut ctx = RefinementContext {
        env,
        refine: &mut refine,
        sim_model: &mut sim_model,
    };

    if options.rarity_sim_rounds != 0 {
        log::info!("{} refinement by rarity simulation {0}", sep('=', 6));
        let mut rarity_sim_refine = RaritySimRefinement::new(&mut ctx);
        for round in 1..=options.rarity_sim_rounds {
            rarity_sim_refine.run_round(&mut ctx);
            log::info!("rarity sim round {round}: {}", ctx.refine.stats());
        }
    }

    let mut bmc_refine = BmcRefinement::default();
    if options.bmc_depth != 0 {
        log::info!("{} refinement by bmc {0}", sep('=', 6));
        for time in TimeStep::first_n(options.bmc_depth) {
            log::info!("{} {time} {0}", sep('-', 6));
            bmc_refine.refine_for_time_step(&mut ctx, time);
            log::info!("bmc {time}: {}", ctx.refine.stats());
        }
    }

    let mut timeout = <HashSet<[Lit; 2]>>::default();

    let mut full_bmc = Unroll::new(UnrollMode::Bmc);
    let mut full_bmc_env = Env::default();
    let mut full_sat = CircuitSat::default();

    let mut requested = <HashMap<[Lit; 2], u32>>::default();

    let mut rng = SmallRng::seed_from_u64(0);

    for window_depth in options.window.clone() {
        log::info!(
            "{} proving candidates using pdr, window depth {window_depth} {0}",
            sep('=', 6)
        );
        let mut repr_for_root = <HashMap<Var, Var>>::default();

        for (var, root) in ctx.refine.nonisolated_with_root_iter() {
            repr_for_root
                .entry(root)
                .and_modify(|repr| {
                    if ctx.env.var_defs().level_bound(*repr) > ctx.env.var_defs().level_bound(var) {
                        *repr = var
                    }
                })
                .or_insert(var);
        }

        let mut repr_for_var = <HashMap<Var, Lit>>::default();

        *ctx.sim_model = seq_sim::model::extract_sim_model(ctx.env, ctx.env.var_defs().repr_vars());

        let to_check: Vec<[Lit; 2]> = ctx
            .refine
            .nonisolated_with_root_iter()
            .flat_map(|(var, root)| {
                let repr = repr_for_root[&root];
                let repr_lit = repr
                    ^ ctx.sim_model.sim_from_env[repr]
                        .unwrap()
                        .lookup(|var| ctx.sim_model.zero_values[var])
                    ^ ctx.sim_model.sim_from_env[var]
                        .unwrap()
                        .lookup(|var| ctx.sim_model.zero_values[var]);

                repr_for_var.insert(var, repr_lit);

                (var.as_lit() != repr_lit).then(|| [var.as_lit(), repr_lit])
            })
            .collect();

        #[derive(Default)]
        struct Stats {
            pub counter: usize,
            pub proved: usize,
            pub spec_proved: usize,
            pub cex: usize,
            pub full_cex: usize,
            pub timeout: usize,
            pub congruence: usize,
        }

        impl Display for Stats {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let Self {
                    counter,
                    proved,
                    spec_proved,
                    congruence,
                    cex,
                    full_cex,
                    timeout,
                } = *self;
                write!(f, "[{counter}]")?;

                write!(f, " proved: {proved} [{spec_proved}]")?;
                write!(f, " cong: {congruence}")?;
                write!(f, " cex: {cex} ({full_cex})")?;
                write!(f, " timeout: {timeout}")?;
                Ok(())
            }
        }

        let mut stats = Stats::default();

        for &pair in to_check.iter() {
            requested.remove(&pair);
        }

        let mut requested_list: Vec<_> = requested
            .iter()
            .map(|(&pair, &count)| (!count, pair))
            .collect();
        requested_list.sort();
        requested_list.truncate(to_check.len());

        for &pair in to_check
            .iter()
            .chain(requested_list.iter().map(|(_, pair)| pair))
        {
            requested.remove(&pair);

            stats.counter += 1;
            if stats.counter % 10000 == 0 {
                log::info!("{stats}");
                log::info!("windowed: {}", ctx.refine.stats());
            }
            let pair = pair.map(|lit| ctx.env.lit_repr(lit));
            if pair[0] == pair[1] {
                stats.congruence += 1;
                continue;
            }
            if !(ctx.refine.is_ancestor_of(pair[0].var(), pair[1].var())
                || ctx.refine.is_ancestor_of(pair[1].var(), pair[0].var()))
            {
                stats.cex += 1;
                continue;
            }
            if timeout.contains(&pair) {
                stats.timeout += 1;
                continue;
            }

            fn expand_window(window: &mut IndexedIdVec<u32, Var>, env: &Env, full_var: Var) {
                let mut handle_node = |node: &DynNode| {
                    node.dyn_foreach_input_var(&mut |input_var| {
                        if input_var != Var::FALSE {
                            window.insert(input_var);
                        }
                        true
                    });
                };

                if let Some(node) = env.def_node(full_var) {
                    handle_node(node);
                }

                for node_id in env.defs_index().find_non_primary_defs_unordered(full_var) {
                    let node = env.nodes().get_dyn(node_id).unwrap();
                    handle_node(node);
                }
            }

            let mut window = <IndexedIdVec<u32, Var>>::default();
            let delta = ctx.env.xor(pair);
            window.insert(delta.var());

            let mut start = 0;
            for _ in 0..window_depth {
                let end = window.next_unused_key();
                for pos in start..end {
                    let entry = window[pos];
                    expand_window(&mut window, ctx.env, entry);
                }

                if start == end {
                    break;
                }
                start = end;
            }

            let mut window_reprs = <HashMap<Var, Lit>>::default();
            let mut requests: Vec<[Lit; 2]> = vec![];

            for &full_var in window.values() {
                let full_repr = repr_for_var
                    .get(&full_var)
                    .copied()
                    .unwrap_or(full_var.as_lit());

                let equiv_lit = match window_reprs.entry(full_repr.var()) {
                    std::collections::hash_map::Entry::Occupied(mut entry) => {
                        let equiv_lit = *entry.get() ^ full_repr.pol();
                        if rng.gen() {
                            *entry.get_mut() = full_var ^ full_repr.pol();
                        }
                        equiv_lit
                    }
                    std::collections::hash_map::Entry::Vacant(entry) => {
                        entry.insert(full_var ^ full_repr.pol());
                        continue;
                    }
                };

                assert_ne!(equiv_lit, full_var.as_lit());
                requests.push([full_var.as_lit(), equiv_lit]);
            }

            let mut extracted =
                ExtractedFragment::new(window.values().iter().copied(), ctx.env, |var| {
                    var.as_lit()
                });

            let pdr_options = PdrOptions {
                frame_limit: Some(64),
                limit: Some(1024),
                pre_bmc: 4,
                pre_k_induction: 2,
                run_scorr: window_depth >= 4,
                run_dc2: window_depth >= 4,
                ..PdrOptions::default()
            };

            let frag_delta = delta.lookup(|var| extracted.frag_from_full[&var]);

            let result = extracted.solve_with_pdr_lazy(frag_delta, &pdr_options);

            if !matches!(result, Some(PdrResult::Unreachable)) {
                for &request in requests.iter() {
                    *requested.entry(request).or_default() += 1;
                }
                *requested.entry(pair).or_default() += 1;
            }

            match result {
                Some(PdrResult::Unreachable) => {
                    stats.proved += 1;
                    ctx.env.equiv(pair);
                    ctx.sync_equivs();
                }
                Some(PdrResult::Cex(mut cex)) => {
                    cex.minimize(&extracted.frag_env, frag_delta);
                    stats.cex += 1;

                    let cex_frame = cex.inputs.next_unused_key().prev().unwrap();

                    let mut full_lits = vec![];

                    for (time, inputs) in cex.inputs.iter() {
                        for &frag_lit in inputs.iter() {
                            let full_lit = frag_lit.lookup(|var| extracted.full_from_frag[&var]);
                            full_lits.push(full_bmc.unroll(
                                ctx.env,
                                &mut full_bmc_env,
                                time,
                                full_lit,
                            ));
                        }
                    }

                    full_sat.cumulative_conflict_limit(Some(1000));

                    let sat_result = full_sat
                        .query_cube(&mut full_bmc_env, full_lits.iter().copied())
                        == Some(true);

                    if sat_result {
                        requested.remove(&pair);
                        stats.full_cex += 1;

                        let mut sim = OnDemandSeqSim::default();

                        *ctx.sim_model = seq_sim::model::extract_sim_model(
                            ctx.env,
                            ctx.env.var_defs().repr_vars(),
                        );

                        for &lit in full_sat.input_model() {
                            let (time, full_lit) = full_bmc
                                .find_seq_input(ctx.env, &full_bmc_env, lit)
                                .unwrap();

                            let sim_lit =
                                full_lit.lookup(|var| ctx.sim_model.sim_from_env[var].unwrap());

                            sim.fix_input_lit_value(ctx.sim_model, time, sim_lit, true);
                        }

                        for frame in TimeStep::first_n(cex_frame.id_index() + 1) {
                            for &lit in target_properties.iter() {
                                let lit = ctx.env.lit_repr(lit);
                                let sim_lit =
                                    lit.lookup(|var| ctx.sim_model.sim_from_env[var].unwrap());
                                if sim.lit_value(ctx.sim_model, frame, sim_lit) {
                                    let mut full_cex_inputs =
                                        <IdVec<TimeStep, HashSet<Lit>>>::default();
                                    full_cex_inputs
                                        .resize_with(frame.id_index() + 1, Default::default);

                                    for frame in TimeStep::first_n(frame.id_index() + 1) {
                                        for (sim_var, &env_lit) in ctx.sim_model.env_from_sim.iter()
                                        {
                                            if let Some(value) = sim.input_lit_value(
                                                ctx.sim_model,
                                                frame,
                                                sim_var.as_lit(),
                                            ) {
                                                full_cex_inputs[frame]
                                                    .insert(ctx.env.lit_repr(env_lit ^ !value));
                                            }
                                        }
                                    }

                                    return Some(PdrResult::Cex(PdrCex {
                                        inputs: full_cex_inputs,
                                    }));
                                }
                            }
                        }

                        for frame in TimeStep::first_n(cex_frame.id_index() + 1) {
                            ctx.refine.refine_all(&mut Default::default(), |var| {
                                let lit = var
                                    ^ ctx.sim_model.sim_from_env[var]
                                        .unwrap()
                                        .lookup(|var| ctx.sim_model.zero_values[var]);

                                let sim_lit =
                                    lit.lookup(|var| ctx.sim_model.sim_from_env[var].unwrap());

                                sim.lit_value(ctx.sim_model, frame, sim_lit)
                            });
                        }
                    }
                }
                _ => {
                    timeout.insert(pair);
                    stats.timeout += 1;
                }
            }
        }
        log::info!("{stats} [final]");
        log::info!("windowed: {}", ctx.refine.stats());

        select_primary_defs(ctx.env);
    }

    let pdr_option = PdrOptions {
        abc_output: true,
        run_scorr: true,
        run_dc2: true,
        pre_bmc: 1,
        ..PdrOptions::default()
    };

    log::info!("{} full pdr {0}", sep('=', 6));

    let target_properties =
        Vec::from_iter(target_properties.iter().map(|&lit| ctx.env.lit_repr(lit)));

    let (result, _pdr_stats) =
        solve_with_pdr(ctx.env, target_properties.iter().copied(), &pdr_option);

    result
}

fn sep(c: char, len: usize) -> impl Debug + Display {
    imctk_util::fmt::fmt_closure(move |f| {
        for _ in 0..len {
            f.write_char(c)?;
        }
        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use imctk_ir::{
        node::fine::circuit::{Input, SteadyInput},
        prelude::NodeBuilder,
    };
    use rand::seq::SliceRandom;
    use seq_sim::model::extract_sim_model;

    use super::*;

    #[test]
    fn test_example_pass() {
        imctk_logger::test_setup("trace");
        let mut env = Env::default();

        let data = include_bytes!("../tests/aiger/fuzzmiter.aig");

        let (seq_env_from_aiger, aig) =
            imctk_aiger::import::import_binary_aiger(&mut env, &mut data.as_slice()).unwrap();

        env.rebuild_egraph();

        let target_properties = Vec::from_iter(
            aig.outputs
                .iter()
                .chain(aig.bad_state_properties.iter())
                .map(|&lit| env.lit_repr(lit.lookup(|var| seq_env_from_aiger[var]))),
        );

        let result = run_eqy_engine(
            &mut env,
            &target_properties,
            EngineOptions {
                rarity_sim_rounds: 5,
                bmc_depth: 5,
                window: 3..5,
            },
        );

        assert!(matches!(result, Some(PdrResult::Unreachable)));
    }

    #[test]
    fn test_example_fail() {
        imctk_logger::test_setup("trace");
        let mut env = Env::default();

        let data = include_bytes!("../tests/aiger/fuzzmiter2.aig");

        let (seq_env_from_aiger, aig) =
            imctk_aiger::import::import_binary_aiger(&mut env, &mut data.as_slice()).unwrap();

        env.rebuild_egraph();

        let target_properties = Vec::from_iter(
            aig.outputs
                .iter()
                .chain(aig.bad_state_properties.iter())
                .map(|&lit| env.lit_repr(lit.lookup(|var| seq_env_from_aiger[var]))),
        );

        let result = run_eqy_engine(
            &mut env,
            &target_properties,
            EngineOptions {
                rarity_sim_rounds: 5,
                bmc_depth: 5,
                window: 3..5,
            },
        );

        let Some(PdrResult::Cex(cex)) = result else { panic!() };

        let sim_model = extract_sim_model(&env, env.var_defs().repr_vars());

        let mut sim = OnDemandSeqSim::default();
        let cex_frame = cex.inputs.next_unused_key().prev().unwrap();
        for (time, cex_inputs) in cex.inputs {
            for input in cex_inputs {
                let sim_input = input.lookup(|var| sim_model.sim_from_env[var].unwrap());

                sim.fix_input_lit_value(&sim_model, time, sim_input, true)
                    .unwrap();
            }
        }

        assert!(target_properties.iter().any(|&prop| sim.lit_value(
            &sim_model,
            cex_frame,
            env.lit_repr(prop)
                .lookup(|var| sim_model.sim_from_env[var].unwrap())
        )));
    }

    #[test]
    fn random_circuits() {
        imctk_logger::test_setup("trace");
        let mut rng = SmallRng::seed_from_u64(1);
        for i in 0..200 {
            let scale = i / 16 + 1;
            let mut env = Env::default();

            let mut lits = vec![];

            for i in 0..rng.gen_range(4..16) {
                lits.push(env.term(Input::from_id_index(i)));
            }

            let mut inputs = lits.clone();
            let mut steady_inputs = <IdVec<SteadyInput, Lit>>::default();

            lits.push(Lit::FALSE);

            let mut reg_next_lits = vec![];

            for _ in 0..rng.gen_range(4 * scale..64 * scale) {
                if rng.gen_ratio(1, 8) {
                    let reg_next = env.fresh_var_with_max_level_bound().as_lit();
                    reg_next_lits.push(reg_next);
                    let init = if rng.gen() {
                        let init = env.term(steady_inputs.next_unused_key());
                        steady_inputs.push(init);
                        init
                    } else {
                        *lits.choose(&mut rng).unwrap()
                    };
                    lits.push(env.reg(init, reg_next ^ rng.gen::<bool>()));
                } else {
                    let inputs: [Lit; 2] = std::array::from_fn(|_| {
                        *lits.choose(&mut rng).unwrap() ^ rng.gen::<bool>()
                    });

                    lits.push(if rng.gen() {
                        env.and(inputs)
                    } else {
                        env.xor(inputs)
                    })
                }
            }

            for &lit in reg_next_lits.iter() {
                env.equiv([lit, *lits.choose(&mut rng).unwrap()]);
            }

            env.rebuild_egraph();

            let prop = env.lit_repr(*lits.choose(&mut rng).unwrap());

            if prop.is_const() {
                continue;
            }

            if let Some(PdrResult::Cex(cex)) = run_eqy_engine(
                &mut env,
                &[prop],
                EngineOptions {
                    rarity_sim_rounds: rng.gen_range(0..10),
                    bmc_depth: rng.gen_range(0..5),
                    window: 3..rng.gen_range(3..5),
                },
            ) {
                let mut sim = OnDemandSeqSim::default();
                let cex_frame = cex.inputs.next_unused_key().prev().unwrap();

                env.rebuild_egraph();

                let prop = env.lit_repr(prop);
                for input in inputs.iter_mut() {
                    *input = env.lit_repr(*input);
                }

                for input in steady_inputs.values_mut() {
                    *input = env.lit_repr(*input);
                }

                let sim_model = extract_sim_model(
                    &env,
                    [prop.var()]
                        .into_iter()
                        .chain(inputs.iter().map(|&lit| lit.var()))
                        .chain(steady_inputs.values().iter().map(|&lit| lit.var())),
                );

                for (time, cex_inputs) in cex.inputs {
                    for input in cex_inputs {
                        let orig_input = input.lookup(|var| {
                            let node = env.def_node(var).unwrap();
                            let term = node.dyn_term().unwrap();
                            let pol = node.output_lit().unwrap().pol();

                            if let Some(&input) = term.dyn_cast::<Input>() {
                                inputs[input.id_index()] ^ pol
                            } else {
                                assert_eq!(time, TimeStep::FIRST);
                                steady_inputs[*term.dyn_cast::<SteadyInput>().unwrap()] ^ pol
                            }
                        });
                        let sim_input =
                            orig_input.lookup(|var| sim_model.sim_from_env[var].unwrap());

                        sim.fix_input_lit_value(&sim_model, time, sim_input, true)
                            .unwrap();
                    }
                }

                assert!(sim.lit_value(
                    &sim_model,
                    cex_frame,
                    prop.lookup(|var| sim_model.sim_from_env[var].unwrap())
                ));
            }
        }
    }
}
