use std::{
    fmt::{Debug, Display, Write},
    ops::Range,
};

use crate::{
    circuit_sat::CircuitSat,
    fragment::ExtractedFragment,
    pdr_wrapper::{solve_with_pdr, PdrOptions, PdrResult},
    refinement::{
        bmc::BmcRefinement, rarity_sim::RaritySimRefinement, EnvVarRefinement, RefinementContext,
    },
    seq_sim::{self, on_demand::OnDemandSeqSim},
    unroll::{Unroll, UnrollMode},
    TimeStep,
};
use imctk_ids::{indexed_id_vec::IndexedIdVec, Id};
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
                log::info!("pdr: {}", ctx.refine.stats());
            }
            let pair = pair.map(|lit| ctx.env.lit_repr(lit));
            if pair[0] == pair[1] {
                stats.congruence += 1;
                continue;
            }
            if !(ctx.refine.contains(pair[0].var(), pair[1].var())
                || ctx.refine.contains(pair[1].var(), pair[0].var()))
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
                    stats.cex += 1;

                    let cex_frame = cex.inputs.next_unused_key().prev().unwrap();

                    let mut frag_bmc = Unroll::new(UnrollMode::Bmc);
                    let mut frag_bmc_env = Env::default();
                    let mut frag_sat = CircuitSat::default();

                    let mut frag_inputs = vec![];

                    for (time, inputs) in cex.inputs.iter() {
                        for &input_lit in inputs.iter() {
                            frag_inputs.push(frag_bmc.unroll(
                                &extracted.frag_env,
                                &mut frag_bmc_env,
                                time,
                                input_lit,
                            ));
                        }
                    }

                    let bmc_target = frag_bmc.unroll(
                        &extracted.frag_env,
                        &mut frag_bmc_env,
                        cex_frame,
                        frag_delta,
                    );

                    let sat_result = frag_sat
                        .query_cube(
                            &mut frag_bmc_env,
                            frag_inputs.iter().copied().chain([!bmc_target]),
                        )
                        .unwrap();

                    assert!(!sat_result);

                    let mut full_lits = vec![];

                    for &lit in frag_sat.unsat_cubes().flatten() {
                        if lit == !bmc_target {
                            continue;
                        }

                        let (time, frag_lit) = frag_bmc
                            .find_seq_input(&extracted.frag_env, &frag_bmc_env, lit)
                            .unwrap_or_else(|| {
                                log::info!("{lit} not found");
                                panic!();
                            });

                        let full_lit = frag_lit.lookup(|var| extracted.full_from_frag[&var]);
                        full_lits.push(full_bmc.unroll(ctx.env, &mut full_bmc_env, time, full_lit));
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
                                .unwrap_or_else(|| {
                                    log::info!("{lit} not found");
                                    panic!();
                                });

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
                                    cex.inputs.truncate(frame.id_index() + 1);
                                    return Some(PdrResult::Cex(cex));
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
    }

    let pdr_option = PdrOptions {
        abc_output: true,
        run_scorr: true,
        run_dc2: true,
        ..PdrOptions::default()
    };

    log::info!("{} full pdr {0}", sep('=', 6));

    let (result, _pdr_stats) = solve_with_pdr(
        ctx.env,
        target_properties.iter().copied(),
        &pdr_option,
    );

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
