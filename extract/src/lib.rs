//! Utilities for extracting parts of an environment.
use std::collections::BTreeSet;

use imctk_ids::{id_vec::IdVec, Id32};
use imctk_ir::{
    env::{Env, VarDef},
    node::NodeId,
    var::{Lit, Var},
};
use imctk_util::{topo_sorted_sccs::TopoSortedSccs, vec_sink::VecSink};

// TODO I'm not so sure about the module organization for extraction related utilities

/// Predecessor callback for [`TopoSortedSccs`] that traverses unguarded inputs of an environment's
/// primary definition graph.
pub fn primary_def_unguarded_input_vars(env: &Env) -> impl FnMut(Var, VecSink<Var>) + '_ {
    |var, mut preds| {
        let var_def = env.var_defs().var_def(var);
        log::trace!("primary def {var}: {var_def:?}");
        match var_def {
            Some(VarDef::Node(node_id)) => env
                .nodes()
                .get_dyn(node_id)
                .unwrap()
                .dyn_append_unguarded_input_vars(preds),
            Some(VarDef::Equiv(lit)) => {
                preds.push(lit.var());
            }
            None => (),
        }
    }
}

/// Extract topologically sorted transitive input cones in the primary definition graph.
///
/// This starts with a set of target variables and computes their combined transitive input cone.
/// The input cone computation will follow guarded inputs that are allowed to form primary
/// definition cycles (e.g. the "next" input of registers).
///
/// For topologocailly sorting the nodes of the input cone guarded inputs are ignored. If the
/// resulting input cones contain an unguarded cyle, this method will panic.
pub fn extract_topo_sorted_primary_defs(
    env: &Env,
    targets: impl IntoIterator<Item = Var>,
) -> Vec<Var> {
    partial_extract_topo_sorted_primary_defs(env, targets, |_, _| true)
}

/// Extract topologically sorted partial transitive input cones in the primary definition graph.
// TODO document how to control partial input cone expansion
pub fn partial_extract_topo_sorted_primary_defs(
    env: &Env,
    targets: impl IntoIterator<Item = Var>,
    mut expand: impl FnMut(Var, VecSink<Var>) -> bool,
) -> Vec<Var> {
    let mut tss = <TopoSortedSccs<IdVec<Var, Option<Id32>>>>::default();

    let mut roots: Vec<Var> = vec![];
    let mut order: Vec<Var> = Default::default();

    let mut callback = |tss: &TopoSortedSccs<_>, scc: &[Var], roots: &mut Vec<Var>| {
        assert_eq!(
            scc.len(),
            1,
            "unguarded primary definition cycle detected: {scc:?}"
        );
        let var = scc[0];
        if var == Var::FALSE {
            return;
        }
        order.push(var);

        if let Some(VarDef::Node(node_id)) = env.var_defs().var_def(var) {
            let mut root_pos = roots.len();

            // TODO add an iter directly for guarded inputs?

            log::trace!("var {var:?} node {node_id:?}");

            env.nodes()
                .get_dyn(node_id)
                .unwrap()
                .dyn_append_input_vars(VecSink::new(roots));

            while let Some(&root) = roots.get(root_pos) {
                if tss.processed(root) {
                    roots.swap_remove(root_pos);
                } else {
                    root_pos += 1;
                }
            }
        }
    };

    let mut wrapped = primary_def_unguarded_input_vars(env);

    tss.process(
        targets,
        |var, mut incoming| {
            if expand(var, incoming.borrow_mut()) {
                wrapped(var, incoming.borrow_sink());
            }
        },
        |tss, scc| callback(tss, scc, &mut roots),
    );

    while let Some(root) = roots.pop() {
        tss.process_one(
            root,
            |var, mut incoming| {
                if expand(var, incoming.borrow_mut()) {
                    wrapped(var, incoming.borrow_sink());
                }
            },
            |tss, scc| callback(tss, scc, &mut roots),
        )
    }

    order
}

/// Select new primary definitions for all variables in the environment.
// TODO document how definitions are selected
pub fn select_primary_defs(env: &mut Env) {
    select_primary_defs_by(env, |_, _| true);
}

/// Select new primary definitions for all variables in the environment using a user provided
/// predicate to select preferred candidates.
// TODO document how definitions are selected
pub fn select_primary_defs_by(env: &mut Env, mut candidate: impl FnMut(&Env, NodeId) -> bool) {
    // TODO this could likely be optimized quite a bit

    type Score = (u32, u32);

    let mut queue: BTreeSet<(Score, Var)> = Default::default();
    let mut scores: IdVec<Var, Option<(Score, NodeId)>> = Default::default();

    scores.resize(env.var_defs().len(), None);

    let mut update = |env: &Env,
                      queue: &mut BTreeSet<_>,
                      updated: &mut IdVec<Var, bool>,
                      scores: &mut IdVec<_, _>,
                      mut score: (u32, u32),
                      node_id: NodeId,
                      output_var: Var| {
        if updated[output_var] {
            return;
        }
        if !candidate(env, node_id) {
            score.0 += 1;
        }

        let entry = &mut scores[output_var];

        match entry {
            Some((prev_score, prev_node_id)) => {
                match score.cmp(prev_score) {
                    std::cmp::Ordering::Less => {
                        queue.remove(&(*prev_score, output_var));
                        *prev_score = score;
                        *prev_node_id = node_id;
                        queue.insert((score, output_var));
                    }
                    std::cmp::Ordering::Equal => {
                        // This is important to ensure that the used iteration order doesn't
                        // leak through
                        *prev_node_id = (*prev_node_id).min(node_id);
                    }
                    std::cmp::Ordering::Greater => {}
                }
            }
            None => {
                *entry = Some((score, node_id));
                queue.insert((score, output_var));
            }
        }
    };
    let mut updated: IdVec<Var, bool> = Default::default();
    updated.resize(env.var_defs().len(), false);
    updated[Var::FALSE] = true;

    for (node_id, node) in env.nodes().iter() {
        let Some(output_var) = node.output_var() else { continue };
        let mut source = true;
        node.dyn_foreach_unguarded_input_var(&mut |var| {
            if var != Var::FALSE {
                source = false;
            }
            source
        });

        if source {
            let score = (0, 0);
            update(
                env,
                &mut queue,
                &mut updated,
                &mut scores,
                score,
                node_id,
                output_var,
            );
        }
    }

    let mut tmp = vec![];

    while let Some((_score, var)) = queue.pop_first() {
        assert!(!updated[var], "{var}");
        updated[var] = true;
        {
            let (_, node_id) = scores[var].unwrap();

            env.make_primary_def(node_id);
        }

        for use_id in env.uses_index().find_uses_unordered(var) {
            let Some(output_var) = env.nodes().get_dyn(use_id).unwrap().output_var() else { continue };

            let mut ready = true;

            tmp.clear();
            env.nodes()
                .get_dyn(use_id)
                .unwrap()
                .dyn_append_unguarded_input_vars(VecSink::new(&mut tmp));
            tmp.sort_unstable();
            tmp.dedup();

            for &input_var in tmp.iter() {
                if !updated[input_var] {
                    ready = false;
                    break;
                }
            }

            if ready {
                let mut score = (0, 0);

                for &input_var in tmp.iter() {
                    if input_var == Var::FALSE {
                        continue;
                    }
                    let (input_score, _) = scores[input_var].unwrap();
                    let (a, b) = input_score;
                    score = score.max((a, b + 1));
                }

                update(
                    env,
                    &mut queue,
                    &mut updated,
                    &mut scores,
                    score,
                    use_id,
                    output_var,
                );
            }
        }
    }
}

/// Returns a copy of the environment with topologically sorted variable ids.
pub fn duplicate(env: &mut Env) -> (Env, IdVec<Var, Lit>, IdVec<Var, Option<Lit>>) {
    let mut env_from_duplicate: IdVec<Var, Lit> = Default::default();
    let mut duplicate_from_env: IdVec<Var, Option<Lit>> = Default::default();

    env_from_duplicate.push(Lit::FALSE);
    duplicate_from_env.push(Some(Lit::FALSE));

    let mut duplicate = Env::default();

    let order = extract_topo_sorted_primary_defs(env, env.var_defs().repr_vars());

    for var in order {
        if let Some(node) = env.def_node(var) {
            node.dyn_foreach_input_var(&mut |input_var| {
                let reduced_var = duplicate_from_env.grow_for_key(input_var);
                if reduced_var.is_none() {
                    let new_var =
                        duplicate.fresh_var_with_level_bound(env.var_defs().level_bound(input_var));
                    assert_eq!(new_var, env_from_duplicate.next_unused_key());
                    env_from_duplicate.push(input_var.as_lit());
                    *reduced_var = Some(new_var.as_lit());
                }
                true
            });

            let reduced_var = duplicate_from_env.grow_for_key(var);
            if reduced_var.is_none() {
                let new_var = duplicate.fresh_var_with_level_bound(env.var_defs().level_bound(var));
                assert_eq!(new_var, env_from_duplicate.next_unused_key());
                env_from_duplicate.push(var.as_lit());
                *reduced_var = Some(new_var.as_lit());
            }

            node.dyn_add_to_env_with_var_map(&mut duplicate, &mut |var| {
                duplicate_from_env[var].unwrap()
            });
        }
    }

    for (_node_id, node) in env.nodes().iter() {
        node.dyn_add_to_env_with_var_map(&mut duplicate, &mut |var| {
            duplicate_from_env[var].unwrap()
        });
    }
    (duplicate, env_from_duplicate, duplicate_from_env)
}

/// Returns a copy of the environment limited to the input cones of the given targets, using
/// topologically sorted variable ids.
pub fn duplicate_partial(
    zelf: &mut Env,
    targets: impl IntoIterator<Item = Var>,
) -> (Env, IdVec<Var, Lit>, IdVec<Var, Option<Lit>>) {
    let mut env_from_duplicate: IdVec<Var, Lit> = Default::default();
    let mut duplicate_from_env: IdVec<Var, Option<Lit>> = Default::default();

    duplicate_from_env.resize(zelf.var_defs().len(), None);

    env_from_duplicate.push(Lit::FALSE);
    duplicate_from_env[Var::FALSE] = Some(Lit::FALSE);

    let mut duplicate = Env::default();

    let targets: Vec<_> = targets
        .into_iter()
        .map(|var| zelf.var_defs().var_repr(var))
        .collect();

    let order = extract_topo_sorted_primary_defs(zelf, targets.iter().copied());

    for var in order {
        if let Some(node) = zelf.def_node(var) {
            node.dyn_foreach_input_var(&mut |input_var| {
                let reduced_var = duplicate_from_env.grow_for_key(input_var);
                if reduced_var.is_none() {
                    let new_var = duplicate
                        .fresh_var_with_level_bound(zelf.var_defs().level_bound(input_var));
                    assert_eq!(new_var, env_from_duplicate.next_unused_key());
                    env_from_duplicate.push(input_var.as_lit());
                    *reduced_var = Some(new_var.as_lit());
                }
                true
            });

            let reduced_var = duplicate_from_env.grow_for_key(var);
            if reduced_var.is_none() {
                let new_var =
                    duplicate.fresh_var_with_level_bound(zelf.var_defs().level_bound(var));
                assert_eq!(new_var, env_from_duplicate.next_unused_key());
                env_from_duplicate.push(var.as_lit());
                *reduced_var = Some(new_var.as_lit());
            }

            node.dyn_add_to_env_with_var_map(&mut duplicate, &mut |var| {
                duplicate_from_env[var].unwrap()
            });
        }
    }

    let mut tmp_vars = vec![];

    'outer: for (_node_id, node) in zelf.nodes().iter() {
        if let Some(output_var) = node.output_var() {
            if duplicate_from_env[output_var].is_none() {
                continue;
            }
        }
        node.dyn_append_input_vars(VecSink::new(&mut tmp_vars));
        for input_var in tmp_vars.drain(..) {
            if duplicate_from_env[input_var].is_none() {
                continue 'outer;
            }
        }

        node.dyn_add_to_env_with_var_map(&mut duplicate, &mut |var| {
            duplicate_from_env[var].unwrap()
        });
    }

    (duplicate, env_from_duplicate, duplicate_from_env)
}
