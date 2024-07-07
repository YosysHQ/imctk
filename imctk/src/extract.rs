//! Utilities for extracting parts of an environment.
use imctk_ids::{id_vec::IdVec, Id32};

use crate::{
    ir::{
        env::{Env, VarDef},
        var::Var,
    },
    topo_sorted_sccs::TopoSortedSccs,
    vec_sink::VecSink,
};

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
    let mut tss = <TopoSortedSccs<IdVec<Var, Option<Id32>>>>::default();

    let mut roots: Vec<Var> = vec![];
    let mut order: Vec<Var> = Default::default();

    let mut callback = |tss: &TopoSortedSccs<_>, scc: &[Var], roots: &mut Vec<Var>| {
        assert_eq!(
            scc.len(),
            1,
            "unguraded primary definition cycle detected: {scc:?}"
        );
        let var = scc[0];
        if var == Var::FALSE {
            return;
        }
        order.push(var);

        if let Some(VarDef::Node(node_id)) = env.var_defs().var_def(var) {
            let mut root_pos = roots.len();

            // TODO add an iter directly for guarded inputs?

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

    tss.process(
        targets,
        primary_def_unguarded_input_vars(env),
        |tss, scc| callback(tss, scc, &mut roots),
    );

    while let Some(root) = roots.pop() {
        tss.process_one(root, primary_def_unguarded_input_vars(env), |tss, scc| {
            callback(tss, scc, &mut roots)
        })
    }

    order
}
