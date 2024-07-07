use super::*;

impl Env {
    /// Incrementally restores the egraph invariants for the full environment.
    ///
    /// Note that using [`Env::raw_nodes`] it may be possible to violate the egraph invariants in
    /// ways that this method cannot repair. This does not happen when using the envrionment's
    /// [`NodeBuilder`] methods.

    pub fn rebuild_egraph(&mut self) {
        let mut node_ids = vec![];
        let mut pending = vec![];

        let mut renamed_vars = 0;
        let mut rewritten_nodes = 0;
        let mut reduced_nodes = 0;
        let mut redundant_nodes = 0;
        let mut found_congruences = 0;
        let mut passes = 0;

        let mut node_buf = take(&mut self.node_buf);
        let mut node_buf_var_map = take(&mut self.node_buf_var_map);

        while !self.index.pending_equivs.is_empty() || !self.index.reduction_queue.is_empty() {
            loop {
                swap(&mut pending, &mut self.index.pending_equivs);
                if pending.is_empty() {
                    break;
                }

                log::trace!("egraph rebuild pass {passes}: len {}", pending.len());
                log::trace!("egraph rebuild pending {pending:?}");

                passes += 1;
                renamed_vars += pending.len();

                node_ids.clear();

                for var in pending.drain(..) {
                    log::trace!(
                        "egraph rebuild var {var} -> {:?}",
                        self.var_defs.var_def(var)
                    );
                    node_ids.extend(self.index.defs_index.find_non_primary_defs_unordered(var));
                    node_ids.extend(self.index.uses_index.find_uses_unordered(var));
                    node_ids.extend(self.var_defs.var_defs[var].def_node());
                }

                node_ids.sort_unstable();
                node_ids.dedup();

                'outer: for node_id in node_ids.drain(..) {
                    log::trace!(
                        "egraph rebuild node before: {node_id:?} {:?}",
                        self.nodes.get_dyn(node_id)
                    );

                    let Some((node, node_role_before)) =
                        Self::get_node_with_role(&self.nodes, &self.var_defs, node_id)
                    else {
                        continue;
                    };
                    log::trace!("egraph rebuild node_role before: {node_role_before:?}");

                    self.index
                        .remove_dyn_node(&self.nodes, node_id, node, node_role_before);

                    self.nodes
                        .get_dyn_mut(node_id)
                        .unwrap()
                        .dyn_apply_var_map(&mut |var| self.var_defs.update_lit_repr(var.as_pos()));

                    rewritten_nodes += 1;

                    let node = self.nodes.get_dyn(node_id).unwrap();

                    let mut new_equiv = None;

                    if let Some(found_node) = self
                        .index
                        .structural_hash_index
                        .find_dyn_node(&self.nodes, node)
                    {
                        let (other_node, other_role) = Self::get_node_with_role(
                            &self.nodes,
                            &self.var_defs,
                            found_node.node_id,
                        )
                        .unwrap();
                        log::trace!("other_node {:?} {other_node:?}", found_node.node_id);
                        log::trace!("other_role {other_role:?}");

                        match (node_role_before, other_role) {
                            (NodeRole::Constraint, NodeRole::Constraint) => {
                                self.nodes.discard(node_id);
                                redundant_nodes += 1;
                                continue 'outer;
                            }
                            (NodeRole::Constraint, _) | (_, NodeRole::Constraint) => {
                                unreachable!()
                            }

                            (NodeRole::PrimaryDef(var_before), NodeRole::PrimaryDef(other_var)) => {
                                assert_ne!(var_before, other_var);

                                let level_bound = self.var_defs.var_defs[var_before]
                                    .level_bound()
                                    .min(self.var_defs.var_defs[other_var].level_bound());
                                self.var_defs.var_defs[var_before].clear_def();
                                self.var_defs.var_defs[other_var].set_level_bound(level_bound);
                                self.var_defs.var_defs[var_before].set_level_bound(level_bound);
                                self.nodes.discard(node_id);
                                redundant_nodes += 1;

                                found_congruences +=
                                    self.insert_equiv(found_node.equiv.unwrap()) as usize;
                                continue 'outer;
                            }
                            (NodeRole::PrimaryDef(_), NodeRole::Equivalence(_)) => {
                                self.index.remove_dyn_node(
                                    &self.nodes,
                                    found_node.node_id,
                                    other_node,
                                    other_role,
                                );

                                self.nodes.discard(found_node.node_id);
                                redundant_nodes += 1;

                                new_equiv = found_node.equiv;
                            }
                            (NodeRole::Equivalence(_), NodeRole::PrimaryDef(_)) => {
                                self.nodes.discard(node_id);
                                redundant_nodes += 1;

                                found_congruences +=
                                    self.insert_equiv(found_node.equiv.unwrap()) as usize;
                                continue 'outer;
                            }
                            (NodeRole::Equivalence(_), NodeRole::Equivalence(_)) => {
                                self.nodes.discard(node_id);
                                redundant_nodes += 1;

                                found_congruences +=
                                    self.insert_equiv(found_node.equiv.unwrap()) as usize;
                                continue 'outer;
                            }
                        }
                    }

                    let (node, node_role_after) =
                        Self::get_node_with_role(&self.nodes, &self.var_defs, node_id).unwrap();

                    #[cfg(debug_assertions)]
                    {
                        match (node_role_before, node_role_after) {
                            (NodeRole::Constraint, NodeRole::Constraint)
                            | (NodeRole::Equivalence(_), NodeRole::Equivalence(_)) => {}
                            (NodeRole::Constraint, _) | (_, NodeRole::Constraint) => {
                                unreachable!();
                            }
                            (NodeRole::PrimaryDef(var_before), NodeRole::PrimaryDef(var_after)) => {
                                assert_eq!(var_before, var_after)
                            }
                            (NodeRole::PrimaryDef(_), NodeRole::Equivalence(_)) => {
                                // Cannot happen since equiv does eager updating of the primary def
                                unreachable!();
                            }
                            (NodeRole::Equivalence(_), NodeRole::PrimaryDef(_)) => {
                                // Cannot happen since a primary def cannot appear out of thin air
                                unreachable!();
                            }
                        }
                    }

                    log::trace!("egraph rebuild node after: {node_id:?} {node:?}");

                    log::trace!("egraph rebuild node_role after: {node_role_after:?}");

                    self.index
                        .add_dyn_node(&self.nodes, node_id, node, node_role_after);

                    self.index.reduction_queue.push(node_id);

                    if let Some(new_equiv) = new_equiv {
                        self.insert_equiv(new_equiv);
                    }
                }
            }

            swap(&mut node_ids, &mut self.index.reduction_queue);

            for node_id in node_ids.drain(..) {
                log::trace!(
                    "egraph reduce node before: {node_id:?} {:?}",
                    self.nodes.get_dyn(node_id)
                );

                let Some((_node, node_role_before)) =
                    Self::get_node_with_role(&self.nodes, &self.var_defs, node_id)
                else {
                    continue;
                };
                log::trace!("egraph reduce node_role before: {node_role_before:?}");

                let node = self.nodes.get_dyn_mut(node_id).unwrap();

                if node.dyn_reduce_into_buf(&mut node_buf) {
                    log::trace!("egraph reduction {node:?} {node_buf:?}");
                    reduced_nodes += 1;

                    let node = self.nodes.get_dyn(node_id).unwrap();
                    self.index
                        .remove_dyn_node(&self.nodes, node_id, node, node_role_before);
                    self.nodes.discard(node_id);

                    if matches!(node_role_before, NodeRole::PrimaryDef(_)) {
                        node_buf.drain_into_node_builder(self.build_defs(), &mut node_buf_var_map);
                    } else {
                        node_buf.drain_into_node_builder(self, &mut node_buf_var_map);
                    }
                }
            }
        }

        self.node_buf = node_buf;
        self.node_buf_var_map = node_buf_var_map;

        redundant_nodes -= found_congruences;

        log::debug!("egraph passes={passes}");
        log::debug!("egraph reduced_nodes={reduced_nodes}");
        log::debug!("egraph renamed_vars={renamed_vars}");
        log::debug!("egraph rewritten_nodes={rewritten_nodes}");
        log::debug!("egraph found_congruences={found_congruences}");
        log::debug!("egraph redundant_nodes={redundant_nodes}");
    }
}
