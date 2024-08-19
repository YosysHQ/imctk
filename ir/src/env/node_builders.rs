use super::*;

/// Environment wrapper that provides lower-level access to an [environment's][Env] [nodes][Node].
#[derive(SubtypeCast, NewtypeCast)]
#[repr(transparent)]
pub struct RawEnvNodes(Env);

impl EnvWrapper for RawEnvNodes {
    /// Returns the a mutable reference to the wrapped environment.
    fn env_mut(&mut self) -> &mut Env {
        &mut self.0
    }

    /// Returns a reference to the wrapped environment.
    fn env(&self) -> &Env {
        &self.0
    }
}

/// Selects the primary definition updating behavior for [`RawEnvNodes`].
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum DefMode {
    /// Only add a new nodes as primary definition when there is no existing primary definition
    WhenMissing,
    /// Add a new node as primary definition whenever the level bounds indicate that this will not
    /// introduce any cycles into the primmary definition graph.
    WhenAcyclic,
}

impl DefMode {
    /// Returns `true` if the def mode is [`WhenAcyclic`].
    ///
    /// [`WhenAcyclic`]: DefMode::WhenAcyclic
    #[must_use]
    pub fn is_when_acyclic(&self) -> bool {
        matches!(self, Self::WhenAcyclic)
    }
}

impl RawEnvNodes {
    /// Insert a node, assuming it is already fully reduced and not yet part of the environment.
    ///
    /// This assumes the node is already fully reduced and will not perform automatic reduction.
    ///
    /// While it is memory-safe to violate the canonicity or uniqueness assumptions, doing so may
    /// prevent the environment from maintaining the configured invariants and indices.
    pub fn insert_unique_irreducible_node<T: Node>(
        &mut self,
        def_mode: DefMode,
        node: T,
    ) -> (NodeId, &T) {
        self.0.var_defs.var_defs.grow_for_key(node.max_var());

        let output_var = node.output_var();

        let (node_id, node_ref, nodes) = self.0.nodes.insert_and_get(node);

        if let Some(updates) = &mut self.0.updates {
            updates.nodes.push(node_id);
        }

        let node_role = if let Some(output_var) = output_var {
            let mut encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];
            if output_var == Var::FALSE {
                NodeRole::Equivalence(output_var)
            } else if encoded_var_repr.def_is_none() {
                encoded_var_repr.set_def_node(node_id);

                let mut level_bound = 0;

                for var in node_ref.unguarded_input_var_iter() {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                }
                encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];

                encoded_var_repr.set_level_bound(level_bound);
                NodeRole::PrimaryDef(output_var)
            } else if def_mode.is_when_acyclic() {
                debug_assert!(!matches!(encoded_var_repr.def(), Some(VarDef::Equiv(_))));

                let old_level_bound = encoded_var_repr.level_bound();
                let old_def_node = encoded_var_repr.def_node();

                let mut level_bound = 0;
                for var in node_ref.unguarded_input_var_iter() {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                }

                if level_bound < old_level_bound {
                    log::trace!("def update node");
                    self.0
                        .index
                        .change_primary_def(nodes, output_var, old_def_node, None);

                    encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];

                    encoded_var_repr.set_level_bound(level_bound);
                    encoded_var_repr.set_def_node(node_id);
                    NodeRole::PrimaryDef(output_var)
                } else {
                    NodeRole::Equivalence(output_var)
                }
            } else {
                NodeRole::Equivalence(output_var)
            }
        } else {
            NodeRole::Constraint
        };

        self.0.index.add_node(nodes, node_id, node_ref, node_role);

        (node_id, node_ref)
    }

    /// Insert a dynamically typed node, assuming it is already fully reduced and not yet part of
    /// the environment.
    ///
    /// This assumes the node is already fully reduced and will not perform automatic reduction.
    ///
    /// While it is memory-safe to violate the canonicity or uniqueness assumptions, doing so may
    /// prevent the environment from maintaining the configured invariants and indices.
    pub fn insert_unique_irreducible_dyn_node(
        &mut self,
        def_mode: DefMode,
        node: Take<DynNode>,
    ) -> (NodeId, &DynNode) {
        self.0.var_defs.var_defs.grow_for_key(node.max_var());

        let output_var = node.output_var();

        let (node_id, node_ref, nodes) = self.0.nodes.insert_and_get_dyn(node);

        if let Some(updates) = &mut self.0.updates {
            updates.nodes.push(node_id);
        }

        let node_role = if let Some(output_var) = output_var {
            let mut encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];
            if output_var == Var::FALSE {
                NodeRole::Equivalence(output_var)
            } else if encoded_var_repr.def_is_none() {
                encoded_var_repr.set_def_node(node_id);

                let mut level_bound = 0;

                node_ref.dyn_foreach_unguarded_input_var(&mut |var| {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                    true
                });

                encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];

                encoded_var_repr.set_level_bound(level_bound);
                NodeRole::PrimaryDef(output_var)
            } else if def_mode.is_when_acyclic() {
                debug_assert!(!matches!(encoded_var_repr.def(), Some(VarDef::Equiv(_))));

                let old_level_bound = encoded_var_repr.level_bound();
                let old_def_node = encoded_var_repr.def_node();

                let mut level_bound = 0;
                node_ref.dyn_foreach_unguarded_input_var(&mut |var| {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                    true
                });

                if level_bound < old_level_bound {
                    log::trace!("def update dyn node");
                    self.0
                        .index
                        .change_primary_def(nodes, output_var, old_def_node, None);

                    encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];

                    encoded_var_repr.set_level_bound(level_bound);
                    encoded_var_repr.set_def_node(node_id);
                    NodeRole::PrimaryDef(output_var)
                } else {
                    NodeRole::Equivalence(output_var)
                }
            } else {
                NodeRole::Equivalence(output_var)
            }
        } else {
            NodeRole::Constraint
        };

        self.0
            .index
            .add_dyn_node(nodes, node_id, node_ref, node_role);

        (node_id, node_ref)
    }

    /// Remove a node from the environment.
    ///
    /// This does not remove any variables and thus calling this can leave used variables without
    /// any primary definition.
    pub fn discard_node(&mut self, node_id: NodeId) -> bool {
        let Some((node_ref, node_role)) =
            Env::get_node_with_role(&self.0.nodes, &self.0.var_defs, node_id)
        else {
            return false;
        };

        self.0
            .index
            .remove_dyn_node(&self.0.nodes, node_id, node_ref, node_role);

        if let NodeRole::PrimaryDef(output_var) = node_role {
            self.0.var_defs.var_defs[output_var] = Default::default();
        }

        self.0.nodes.discard(node_id);

        true
    }

    /// Ensures the presence of a [term node][TermNode] for the given [term][Term], retrieving its
    /// node id and its output.
    ///
    /// This assumes the term is already fully reduced and will not perform automatic reduction.
    ///
    /// If there is an existing term node for the given term, this returns a tuple containing the
    /// existing node's [`NodeId`], its output variable/literal and `false`. Otherwise this inserts
    /// a new term node and returns a tuple containing the new node's [`NodeId`], output and `true`.
    pub fn insert_irreducible_term_node<T: Term>(
        &mut self,
        def_mode: DefMode,
        term: T,
    ) -> (NodeId, T::Output, bool) {
        if let Some((node_id, output)) = self
            .0
            .index
            .structural_hash_index
            .find_term(&self.0.nodes, &term)
        {
            if def_mode.is_when_acyclic() {
                let output_var =
                    <T::Output>::process_var_or_lit(output, |var| var, |lit| lit.var());
                let encoded_var_repr = &self.0.var_defs.var_defs[output_var];

                let old_level_bound = encoded_var_repr.level_bound();

                let mut level_bound = 0;
                for var in term.unguarded_input_var_iter() {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                }

                if level_bound < old_level_bound {
                    log::trace!("def update term");
                    self.0
                        .make_primary_def_with_level_bound(node_id, level_bound);
                }
            }
            return (node_id, output, false);
        }

        let (new_var, _) = self.0.var_defs.var_defs.push(EncodedVarDef::default());
        let output = <T::Output>::build_var_or_lit(new_var, |var| var, |var| var.as_lit());

        let (node_id, _) = self.insert_unique_irreducible_node(def_mode, TermNode { output, term });

        (node_id, output, true)
    }

    /// Ensures the presence of a [term node][TermNode] for the given dynamically typed
    /// [term][Term], retrieving its node id and its output.
    ///
    /// This assumes the term is already fully reduced and will not perform automatic reduction.
    ///
    /// If there is an existing term node for the given term, this returns a tuple containing the
    /// existing node's [`NodeId`], its output variable/literal and `false`. Otherwise this inserts
    /// a new term node and returns a tuple containing the new node's [`NodeId`], output and `true`.
    pub fn insert_irreducible_dyn_term_node(
        &mut self,
        def_mode: DefMode,
        term: Take<DynTerm>,
    ) -> (NodeId, Lit, bool) {
        if let Some((node_id, output)) = self
            .0
            .index
            .structural_hash_index
            .find_dyn_term(&self.0.nodes, &*term)
        {
            if def_mode.is_when_acyclic() {
                let encoded_var_repr = &self.0.var_defs.var_defs[output.var()];

                let old_level_bound = encoded_var_repr.level_bound();

                let mut level_bound = 0;
                term.dyn_foreach_unguarded_input_var(&mut |var| {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                    true
                });

                if level_bound < old_level_bound {
                    self.0
                        .make_primary_def_with_level_bound(node_id, level_bound);
                }
            }
            return (node_id, output, false);
        }

        let (new_var, _) = self.0.var_defs.var_defs.push(EncodedVarDef::default());
        let output = new_var.as_lit();

        let (node_id, _) = dyn_term_into_dyn_term_node(output, term, |node| {
            self.insert_unique_irreducible_dyn_node(def_mode, node)
        });

        (node_id, output, true)
    }

    pub(crate) fn insert_irreducible_dyn_term(
        &mut self,
        def_mode: DefMode,
        term: Take<DynTerm>,
    ) -> Lit {
        self.insert_irreducible_dyn_term_node(def_mode, term).1
    }

    /// Ensures the presence of a [term node][TermNode] for the given [term][Term], retrieving
    /// its output.
    ///
    /// This calls [`insert_irreducible_term_node`][Self::insert_irreducible_term_node], returning
    /// only the output variable or literal.
    pub fn insert_irreducible_term<T: Term>(&mut self, def_mode: DefMode, term: T) -> T::Output {
        self.insert_irreducible_term_node(def_mode, term).1
    }

    /// Ensures the presence of a given node, retrieving its node id.
    ///
    /// This assumes the node is already fully reduced and will not perform automatic reduction.
    ///
    /// If the node is already present, it returns a pair of the [`NodeId`] of the existing node and
    /// `false`, otherwise it returns a pair of the newly inserted node's id and `true`.
    ///
    /// If the passed node defines an output variable and there is an existing node that differs
    /// only in their output variable/literal, this makes both output variables/literals equivalent
    /// and considers the node to be already present.
    pub fn insert_irreducible_node<T: Node>(
        &mut self,
        def_mode: DefMode,
        node: T,
    ) -> (NodeId, bool) {
        if let Some(found_node) = self
            .0
            .index
            .structural_hash_index
            .find_node(&self.0.nodes, &node)
        {
            if let Some(equiv) = found_node.equiv {
                if def_mode.is_when_acyclic() {
                    let [node_output, found_output] = equiv;

                    let old_level_bound = self.0.var_defs.var_defs[node_output.var()]
                        .level_bound()
                        .min(self.0.var_defs.var_defs[found_output.var()].level_bound());

                    let mut level_bound = 0;
                    for var in node.unguarded_input_var_iter() {
                        level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                    }

                    if level_bound < old_level_bound {
                        self.0
                            .make_primary_def_with_level_bound(found_node.node_id, level_bound);
                    }
                }
                self.0.insert_equiv(equiv);
            }
            return (found_node.node_id, false);
        }

        let node_id = self.insert_unique_irreducible_node(def_mode, node).0;
        (node_id, true)
    }

    /// Ensures the presence of a given dynamically typed node, retrieving its node id.
    ///
    /// This assumes the node is already fully reduced and will not perform automatic reduction.
    ///
    /// If the node is already present, it returns a pair of the [`NodeId`] of the existing node and
    /// `false`, otherwise it returns a pair of the newly inserted node's id and `true`.
    ///
    /// If the passed node defines an output variable and there is an existing node that differs
    /// only in their output variable/literal, this makes both output variables/literals equivalent
    /// and considers the node to be already present.
    pub fn insert_irreducible_dyn_node(
        &mut self,
        def_mode: DefMode,
        node: Take<DynNode>,
    ) -> (NodeId, bool) {
        if let Some(found_node) = self
            .0
            .index
            .structural_hash_index
            .find_dyn_node(&self.0.nodes, &*node)
        {
            if let Some(equiv) = found_node.equiv {
                if def_mode.is_when_acyclic() {
                    let [node_output, found_output] = equiv;

                    let old_level_bound = self.0.var_defs.var_defs[node_output.var()]
                        .level_bound()
                        .min(self.0.var_defs.var_defs[found_output.var()].level_bound());

                    let mut level_bound = 0;
                    node.dyn_foreach_unguarded_input_var(&mut |var| {
                        level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                        true
                    });

                    if level_bound < old_level_bound {
                        self.0
                            .make_primary_def_with_level_bound(found_node.node_id, level_bound);
                    }
                }
                self.0.insert_equiv(equiv);
            }
            return (found_node.node_id, false);
        }

        let node_id = self.insert_unique_irreducible_dyn_node(def_mode, node).0;
        (node_id, true)
    }
}

impl EnvWrapper for Env {
    fn env(&self) -> &Env {
        self
    }

    fn env_mut(&mut self) -> &mut Env {
        self
    }
}

impl NodeBuilderDyn for Env {
    fn dyn_term(&mut self, mut term: Take<DynTerm>) -> Lit {
        let pol = term.dyn_apply_var_map(&mut |var| self.var_defs.update_lit_repr(var.as_lit()));

        if let Some(output) = term.dyn_reduce_into_buf(&mut self.node_buf) {
            let mut node_buf = take(&mut self.node_buf);
            let var_map = node_buf.drain_into_node_builder(self);
            let output = output.lookup(|var| var_map.map_var(var)) ^ pol;
            self.node_buf = node_buf;

            return output;
        }

        self.raw_nodes()
            .insert_irreducible_dyn_term(DefMode::WhenMissing, term)
            ^ pol
    }

    fn dyn_node(&mut self, mut node: Take<DynNode>) {
        node.dyn_apply_var_map(&mut |var| self.var_defs.update_lit_repr(var.as_lit()));

        if node.dyn_reduce_into_buf(&mut self.node_buf) {
            let mut node_buf = take(&mut self.node_buf);
            node_buf.drain_into_node_builder(self);
            self.node_buf = node_buf;
            return;
        }

        self.raw_nodes()
            .insert_irreducible_dyn_node(DefMode::WhenMissing, node);
    }

    fn equiv(&mut self, equiv: [Lit; 2]) {
        self.insert_equiv(equiv);
    }

    fn valid_temporary_vars(&self, count: usize) -> bool {
        (Var::MAX_ID_INDEX.saturating_add(1) - self.var_defs.len()) >= count
    }
}

impl NodeBuilder for Env {
    fn term<T: Term>(&mut self, mut term: T) -> T::Output {
        let pol = term.apply_var_map(|var| self.var_defs.update_lit_repr(var.as_lit()));

        if let Some(output) = term.reduce(self) {
            return output ^ pol;
        }

        self.raw_nodes()
            .insert_irreducible_term(DefMode::WhenMissing, term)
            ^ pol
    }

    fn node<T: Node>(&mut self, mut node: T) {
        node.apply_var_map(|var| self.var_defs.update_lit_repr(var.as_lit()));

        if node.reduce(self) {
            return;
        }

        self.raw_nodes()
            .insert_irreducible_node(DefMode::WhenMissing, node);
    }
}

/// Environment wrapper with a  [`NodeBuilder`] implementation that replaces primary definitions
/// when adding nodes.
///
/// While the default `NodeBuilder` implementation for [`Env`] will use added nodes as primary
/// definitions for any variable that was not defined before, it will not change existing primary
/// definitions. In constrast, the `NodeBuilder` implementation of this wrapper type will always try
/// to update existing primary definition, subject to level bound restrictions.
#[derive(SubtypeCast, NewtypeCast)]
#[repr(transparent)]
pub struct DefBuilder(Env);

impl EnvWrapper for DefBuilder {
    fn env(&self) -> &Env {
        &self.0
    }

    fn env_mut(&mut self) -> &mut Env {
        &mut self.0
    }
}

impl NodeBuilderDyn for DefBuilder {
    fn dyn_term(&mut self, mut term: Take<DynTerm>) -> Lit {
        let pol = term.dyn_apply_var_map(&mut |var| self.0.var_defs.update_lit_repr(var.as_lit()));

        if let Some(output) = term.dyn_reduce_into_buf(&mut self.0.node_buf) {
            let mut node_buf = take(&mut self.0.node_buf);
            let var_map = node_buf.drain_into_node_builder(self);
            let output = output.lookup(|var| var_map.map_var(var)) ^ pol;
            self.0.node_buf = node_buf;
            return output;
        }

        self.0
            .raw_nodes()
            .insert_irreducible_dyn_term(DefMode::WhenAcyclic, term)
            ^ pol
    }

    fn dyn_node(&mut self, mut node: Take<DynNode>) {
        node.dyn_apply_var_map(&mut |var| self.0.var_defs.update_lit_repr(var.as_lit()));

        if node.dyn_reduce_into_buf(&mut self.0.node_buf) {
            let mut node_buf = take(&mut self.0.node_buf);
            node_buf.drain_into_node_builder(self);
            self.0.node_buf = node_buf;
            return;
        }

        self.0
            .raw_nodes()
            .insert_irreducible_dyn_node(DefMode::WhenAcyclic, node);
    }

    fn equiv(&mut self, equiv: [Lit; 2]) {
        self.0.insert_equiv(equiv);
    }

    fn valid_temporary_vars(&self, count: usize) -> bool {
        (Var::MAX_ID_INDEX.saturating_add(1) - self.0.var_defs.len()) >= count
    }
}

impl NodeBuilder for DefBuilder {
    fn term<T: Term>(&mut self, mut term: T) -> T::Output {
        let pol = term.apply_var_map(|var| self.0.var_defs.update_lit_repr(var.as_lit()));

        if let Some(output) = term.reduce(self) {
            return output ^ pol;
        }

        self.0
            .raw_nodes()
            .insert_irreducible_term(DefMode::WhenAcyclic, term)
            ^ pol
    }

    fn node<T: Node>(&mut self, mut node: T) {
        node.apply_var_map(|var| self.0.var_defs.update_lit_repr(var.as_lit()));

        if node.reduce(self) {
            return;
        }

        self.0
            .raw_nodes()
            .insert_irreducible_node(DefMode::WhenAcyclic, node);
    }
}

impl Env {
    /// Returns a read-only view of the [nodes][`Nodes`] stored in this environment.
    pub fn nodes(&self) -> &Nodes {
        &self.nodes
    }

    /// Returns an environment wrapper used for updating existing primary definitions.
    pub fn build_defs(&mut self) -> &mut DefBuilder {
        DefBuilder::from_repr_mut(self)
    }

    /// Provides lower-level access to an environment's [nodes][`Node`].
    pub fn raw_nodes(&mut self) -> &mut RawEnvNodes {
        RawEnvNodes::from_repr_mut(self)
    }

    /// Returns a read-only view of the primary definitions and level bounds for all variables in
    /// the environment.
    pub fn var_defs(&self) -> &VarDefs {
        &self.var_defs
    }

    pub(super) fn get_node_with_role<'a>(
        nodes: &'a Nodes,
        var_reprs: &VarDefs,
        node_id: NodeId,
    ) -> Option<(&'a DynNode, NodeRole)> {
        let node_ref = nodes.get_dyn(node_id)?;

        let output_var = node_ref.output_var();

        let node_role = if let Some(output_var) = output_var {
            match var_reprs.var_def(output_var) {
                Some(VarDef::Node(def_node_id)) if def_node_id == node_id => {
                    NodeRole::PrimaryDef(output_var)
                }
                _ => NodeRole::Equivalence(output_var),
            }
        } else {
            NodeRole::Constraint
        };

        Some((node_ref, node_role))
    }

    /// Adds a new variable to the environment and returns it.
    ///
    /// Note that the resulting variable starts out without any definition and with a level bound of
    /// zero. For some use cases this is an issue for maintaining acyclicity of the primary
    /// definition graph. In those cases, [`Self::fresh_var_with_level_bound`] or
    /// [`Self::fresh_var_with_max_level_bound`] can be used instead.
    pub fn fresh_var(&mut self) -> Var {
        self.var_defs.var_defs.push(EncodedVarDef::default()).0
    }

    /// Adds a new variable to the environment and returns it, using the maximal supported level bound.
    ///
    /// Until a definition for the variable is provided, which will lower the level bound, using
    /// this as an unguarded input to a new node will produce a panic as there is no remaining
    /// higher level bound to assign to that node's output.
    ///
    /// The main use case for this is to tie loops into guarded inputs, e.g. the register next state
    /// input, where the fresh variable is only connected to the guarded input until and then later
    /// defined by adding a node or adding an equivalence to an existing variable or literal.
    pub fn fresh_var_with_max_level_bound(&mut self) -> Var {
        let mut encoded_var_repr = EncodedVarDef::default();
        encoded_var_repr.set_max_level_bound();

        self.var_defs.var_defs.push(encoded_var_repr).0
    }

    /// Adds a new variable to the environment and returns it, using a given level bound.
    pub fn fresh_var_with_level_bound(&mut self, level_bound: u32) -> Var {
        let mut encoded_var_repr = EncodedVarDef::default();
        encoded_var_repr.set_level_bound(level_bound);

        self.var_defs.var_defs.push(encoded_var_repr).0
    }

    /// Makes two literals equivalent.
    ///
    /// This will chose a representative for the newly merged equivalence class by considering
    /// whether either literal represents a constant boolean value, the level bound for either
    /// literal's variable and finally the estimated cost of rewriting all occurences of the
    /// non-representative literal (as reported by the current environment configuration).
    ///
    /// Taking the level bound into consideration helps maintaining acyclicity of the primary
    /// definition graph.
    ///
    /// This will panic when attempting to make a literal equivalent to its negation.
    pub(crate) fn insert_equiv(&mut self, equiv: [Lit; 2]) -> bool {
        let [mut a, mut b] = equiv.map(|lit| self.var_defs.update_lit_repr(lit));

        self.var_defs.var_defs.grow_for_key(a.max(b).var());

        if a.var() == b.var() {
            // TODO we might want to optionally represent a conflict (if so, also update the docs!)
            assert_eq!(a, b);
            return false;
        }

        if b.is_const() {
            // always prefer a constant value as representative
            (a, b) = (b, a);
        } else if a.is_const() {
            // always prefer a constant value as representative
        } else {
            // compare the level bound to avoid introducing primary definition cycles

            if self.index.rewrite_cost(a.var()) < self.index.rewrite_cost(b.var()) {
                (a, b) = (b, a);
            }
        }

        let repr = a ^ b.pol();
        let equiv = b.var();

        let equiv_level = self.var_defs.level_bound(equiv);
        let repr_level = self.var_defs.level_bound(repr.var());

        let min_level = repr_level.min(equiv_level);

        let equiv_def = self.var_defs.var_defs[equiv].def();
        let repr_def = self.var_defs.var_defs[repr.var()].def();

        log::trace!(
            "equiv: {equiv}~{equiv_def:?} [{equiv_level}] repr: {repr}~{repr_def:?} [{repr_level}]"
        );

        self.index.add_equiv(&self.nodes, repr, equiv);
        if let Some(updates) = &mut self.updates {
            updates.equivs.push(equiv);
        }

        if repr.is_const()
            || (repr_level == min_level && (equiv_level != min_level || repr_def.is_some()))
        {
            self.clear_primary_def(equiv);
            self.var_defs.var_defs[equiv].set_def_equiv(repr);
            self.var_defs.var_defs[equiv].set_level_bound(min_level);
            self.var_defs.var_defs[repr.var()].set_level_bound(min_level);
        } else {
            self.clear_primary_def(repr.var());
            self.var_defs.var_defs[equiv].set_def_equiv(repr);
            self.var_defs.var_defs[equiv].set_level_bound(min_level);
            self.var_defs.var_defs[repr.var()].set_level_bound(min_level);

            match equiv_def {
                Some(VarDef::Node(node_id)) => {
                    self.make_primary_def_with_level_bound(node_id, min_level);
                }
                Some(VarDef::Equiv(_)) => unreachable!(),
                None => (),
            }
        }
        true
    }

    /// Returns the index of all non-primary variable definitions.
    pub fn defs_index(&self) -> &DefsIndex {
        &self.index.defs_index
    }

    /// Returns the index of all variable uses.
    pub fn uses_index(&self) -> &UsesIndex {
        &self.index.uses_index
    }

    /// Returns the index of all nodes and terms indexed by their definition (i.e. excluding the
    /// output of [`TermNode`] nodes).
    pub fn structural_hash_index(&self) -> &StructuralHashIndex {
        &self.index.structural_hash_index
    }

    /// Makes the specified node the primary definition for its output variable, updating its level
    /// bound according to the node's input level bounds.
    // TODO are there any issues with making this public?
    pub fn make_primary_def(&mut self, node_id: NodeId) {
        self.make_primary_def_inner(node_id, None)
    }

    /// Makes the specified node the primary definition for its output variable using a given level
    /// bound.
    // TODO are there any issues with making this public?
    pub fn make_primary_def_with_level_bound(&mut self, node_id: NodeId, level_bound: u32) {
        self.make_primary_def_inner(node_id, Some(level_bound))
    }

    fn make_primary_def_inner(&mut self, node_id: NodeId, level_bound: Option<u32>) {
        let (mut node, mut node_role) =
            Self::get_node_with_role(&self.nodes, &self.var_defs, node_id).unwrap();
        log::trace!("make_primary_def {node_id:?} {node_role:?} {node:?}");

        let level_bound = level_bound.unwrap_or_else(|| {
            let mut level_bound = 0;
            node.dyn_foreach_unguarded_input_var(&mut |var| {
                level_bound = level_bound.max(self.var_defs.var_defs[var].level_bound() + 1);
                true
            });
            level_bound
        });

        match node_role {
            NodeRole::PrimaryDef(output_var) => {
                assert_ne!(output_var, Var::FALSE);
                self.var_defs.var_defs[output_var].set_level_bound(level_bound);
            }
            NodeRole::Equivalence(mut output_var) => loop {
                assert_ne!(output_var, Var::FALSE);
                let def_node =
                    match self.var_defs.var_defs[output_var].def() {
                        Some(VarDef::Equiv(_)) => {
                            self.index
                                .remove_dyn_node(&self.nodes, node_id, node, node_role);

                            self.nodes.get_dyn_mut(node_id).unwrap().dyn_apply_var_map(
                                &mut |var| self.var_defs.update_lit_repr(var.as_lit()),
                            );

                            self.index.reduction_queue.push(node_id);
                            (node, node_role) =
                                Self::get_node_with_role(&self.nodes, &self.var_defs, node_id)
                                    .unwrap();

                            output_var = node.output_var().unwrap();

                            self.index
                                .add_dyn_node(&self.nodes, node_id, node, node_role);

                            continue;
                        }
                        Some(VarDef::Node(node_id)) => Some(node_id),
                        None => None,
                    };

                self.index
                    .change_primary_def(&self.nodes, output_var, def_node, Some(node_id));
                let var_def = &mut self.var_defs.var_defs[output_var];

                var_def.set_def_node(node_id);

                var_def.set_level_bound(level_bound);

                break;
            },
            NodeRole::Constraint => {
                panic!("constraint node cannot be used as primary definition")
            }
        }
    }

    pub(crate) fn clear_primary_def(&mut self, var: Var) {
        match self.var_defs.var_defs[var].def() {
            Some(VarDef::Node(node)) => {
                self.index
                    .change_primary_def(&self.nodes, var, Some(node), None);
            }
            Some(VarDef::Equiv(_)) => return,
            _ => (),
        }
        self.var_defs.var_defs[var].clear_def()
    }
}
