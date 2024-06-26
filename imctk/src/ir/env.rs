//! Environments for storing and maintaining the internal representation.
use std::mem::swap;

use config::{DynamicIndexContext, EnvConfig, EnvConfigDetail};
use imctk_ids::{id_vec::IdVec, Id, Id32};

use crate::ir::{
    node::NodeId,
    var::{Pol, VarOrLit},
};

use super::{
    index::DynamicIndex,
    node::{
        expr::{Expr, ExprNode},
        DynNode, Node, Nodes,
    },
    var::{Lit, Var},
};

pub mod config {
    //! Static environment configuration.

    use crate::ir::{
        index::{DefsIndex, DynamicIndex, FoundNode, StructuralHashIndex, UsesIndex},
        node::{expr::Expr, DynNode, Node, NodeId, Nodes},
        var::{Lit, Var},
    };

    use super::NodeRole;

    pub(crate) use sealed::EnvConfigDetail;

    mod sealed {
        use super::*;

        // TODO some of this probably should be public
        pub trait EnvConfigDetail:
            Default + for<'a> DynamicIndex<Context<'a> = DynamicIndexContext<'a>>
        {
            #[allow(unused_variables)]
            #[inline(always)]
            fn find_expr<T: Expr>(
                &self,
                context: DynamicIndexContext,
                expr: &T,
            ) -> Option<(NodeId, T::Output)> {
                None
            }

            #[allow(unused_variables)]
            #[inline(always)]
            fn find_node<T: Node>(
                &self,
                context: DynamicIndexContext,
                node: &T,
            ) -> Option<FoundNode> {
                None
            }

            #[allow(unused_variables)]
            #[inline(always)]
            fn find_dyn_node(
                &self,
                context: DynamicIndexContext,
                node: &DynNode,
            ) -> Option<FoundNode> {
                None
            }

            #[allow(unused_variables)]
            #[inline(always)]
            fn rewrite_cost(&self, var: Var) -> usize {
                0
            }
        }
    }

    /// Static environment configuration.
    ///
    /// See [`Env`][super::Env].
    pub trait EnvConfig: sealed::EnvConfigDetail {}

    /// Context provided to dynamic indices of an [`EnvConfig`].
    pub struct DynamicIndexContext<'a> {
        pub(super) nodes: &'a Nodes,
    }

    /// Minimal environment configuration.
    ///
    /// This configuration includes no automatically maintained dynamic indices. This results in the
    /// lowest memory overhead as well as the lowest runtime overhead for core environment
    /// operations. This comes at the cost of not supporting any of the operations depending on such
    /// an index.
    #[derive(Default)]
    pub struct Unindexed;

    impl DynamicIndex for Unindexed {
        type Context<'a> = DynamicIndexContext<'a>;

        #[inline(always)]
        fn add_dyn_node(
            &mut self,
            _context: Self::Context<'_>,
            _node_id: crate::ir::node::NodeId,
            _node: &crate::ir::node::DynNode,

            _node_role: NodeRole,
        ) {
        }

        #[inline(always)]
        fn remove_dyn_node(
            &mut self,
            _context: Self::Context<'_>,
            _node_id: crate::ir::node::NodeId,
            _node: &crate::ir::node::DynNode,
            _node_role: NodeRole,
        ) {
        }

        #[inline(always)]
        fn change_primary_def(
            &mut self,
            _context: Self::Context<'_>,
            _var: Var,
            _old_primary_def: Option<NodeId>,
            _new_primary_def: Option<NodeId>,
        ) {
        }

        #[inline(always)]
        fn add_equiv(&mut self, _context: Self::Context<'_>, _repr: Lit, _equiv: Var) {}
    }

    impl sealed::EnvConfigDetail for Unindexed {}
    impl EnvConfig for Unindexed {}

    /// Environment configuration for structural hashing.
    ///
    /// This configuration maintains only a single index required to support structural hashing.
    #[derive(Default)]
    pub struct StructuralHashing {
        structural_hash_index: StructuralHashIndex,
    }

    impl DynamicIndex for StructuralHashing {
        type Context<'a> = DynamicIndexContext<'a>;

        fn add_dyn_node(
            &mut self,
            context: Self::Context<'_>,
            node_id: crate::ir::node::NodeId,
            node: &crate::ir::node::DynNode,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .add_dyn_node(context.nodes, node_id, node, node_role)
        }

        fn add_node<T: crate::ir::node::Node>(
            &mut self,
            context: Self::Context<'_>,
            node_id: NodeId,
            node: &T,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .add_node(context.nodes, node_id, node, node_role)
        }

        fn remove_dyn_node(
            &mut self,
            context: Self::Context<'_>,
            node_id: crate::ir::node::NodeId,
            node: &crate::ir::node::DynNode,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .remove_dyn_node(context.nodes, node_id, node, node_role)
        }

        fn remove_node<T: crate::ir::node::Node>(
            &mut self,
            context: Self::Context<'_>,
            node_id: NodeId,
            node: &T,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .remove_node(context.nodes, node_id, node, node_role)
        }

        fn change_primary_def(
            &mut self,
            context: Self::Context<'_>,
            var: Var,
            old_primary_def: Option<NodeId>,
            new_primary_def: Option<NodeId>,
        ) {
            self.structural_hash_index.change_primary_def(
                context.nodes,
                var,
                old_primary_def,
                new_primary_def,
            )
        }

        #[inline(always)]
        fn add_equiv(&mut self, _context: Self::Context<'_>, _repr: Lit, _equiv: Var) {}
    }

    impl EnvConfig for StructuralHashing {}
    impl sealed::EnvConfigDetail for StructuralHashing {
        fn find_expr<T: Expr>(
            &self,
            context: DynamicIndexContext,
            expr: &T,
        ) -> Option<(NodeId, T::Output)> {
            self.structural_hash_index.find_expr(context.nodes, expr)
        }
    }

    /// Environment configuration containing a default set of indices.
    #[derive(Default)]
    pub struct Indexed {
        pub(super) structural_hash_index: StructuralHashIndex,
        pub(super) defs_index: DefsIndex,
        pub(super) uses_index: UsesIndex,

        pub(super) pending_equivs: Vec<Var>,
    }

    impl DynamicIndex for Indexed {
        type Context<'a> = DynamicIndexContext<'a>;

        fn add_dyn_node(
            &mut self,
            context: Self::Context<'_>,
            node_id: crate::ir::node::NodeId,
            node: &crate::ir::node::DynNode,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .add_dyn_node(context.nodes, node_id, node, node_role);
            self.defs_index.add_dyn_node((), node_id, node, node_role);
            self.uses_index.add_dyn_node((), node_id, node, node_role);
        }

        fn add_node<T: crate::ir::node::Node>(
            &mut self,
            context: Self::Context<'_>,
            node_id: NodeId,
            node: &T,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .add_node(context.nodes, node_id, node, node_role);
            self.defs_index.add_node((), node_id, node, node_role);
            self.uses_index.add_node((), node_id, node, node_role);
        }

        fn remove_dyn_node(
            &mut self,
            context: Self::Context<'_>,
            node_id: crate::ir::node::NodeId,
            node: &crate::ir::node::DynNode,
            node_role: NodeRole,
        ) {
            self.uses_index
                .remove_dyn_node((), node_id, node, node_role);
            self.defs_index
                .remove_dyn_node((), node_id, node, node_role);
            self.structural_hash_index
                .remove_dyn_node(context.nodes, node_id, node, node_role);
        }

        fn remove_node<T: crate::ir::node::Node>(
            &mut self,
            context: Self::Context<'_>,
            node_id: NodeId,
            node: &T,
            node_role: NodeRole,
        ) {
            self.uses_index.remove_node((), node_id, node, node_role);
            self.defs_index.remove_node((), node_id, node, node_role);
            self.structural_hash_index
                .remove_node(context.nodes, node_id, node, node_role);
        }

        fn change_primary_def(
            &mut self,
            context: Self::Context<'_>,
            var: Var,
            old_primary_def: Option<NodeId>,
            new_primary_def: Option<NodeId>,
        ) {
            self.structural_hash_index.change_primary_def(
                context.nodes,
                var,
                old_primary_def,
                new_primary_def,
            );
            self.defs_index
                .change_primary_def((), var, old_primary_def, new_primary_def);
            self.uses_index
                .change_primary_def((), var, old_primary_def, new_primary_def);
        }

        fn add_equiv(&mut self, _context: Self::Context<'_>, _repr: Lit, equiv: Var) {
            self.pending_equivs.push(equiv);
        }
    }
    impl EnvConfig for Indexed {}
    impl sealed::EnvConfigDetail for Indexed {
        #[inline(always)]
        fn find_expr<T: Expr>(
            &self,
            context: DynamicIndexContext,
            expr: &T,
        ) -> Option<(NodeId, T::Output)> {
            self.structural_hash_index.find_expr(context.nodes, expr)
        }

        #[inline(always)]
        fn find_node<T: Node>(&self, context: DynamicIndexContext, node: &T) -> Option<FoundNode> {
            self.structural_hash_index.find_node(context.nodes, node)
        }

        #[inline(always)]
        fn find_dyn_node(&self, context: DynamicIndexContext, node: &DynNode) -> Option<FoundNode> {
            self.structural_hash_index
                .find_dyn_node(context.nodes, node)
        }

        #[inline(always)]
        fn rewrite_cost(&self, var: Var) -> usize {
            self.uses_index.use_count(var) + self.defs_index.non_primary_def_count(var)
        }
    }
}

/// Indicates whether a node is the primary definition or an equivalent definition of a variable or
/// alternatively whether it is a constraint on the node inputs.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum NodeRole {
    /// Used for a node that is the primary definition of a variable.
    PrimaryDef(Var),
    /// Used for a node that gives an equivalent definition of a variable.
    Equivalence(Var),
    /// Used for a node that constrains its inputs.
    Constraint,
}

/// Definition of a variable, either the defining node or an equivalent literal.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum VarDef {
    /// The referenced node is the primary definition of the variable.
    Node(NodeId),
    /// The referenced literal is the primary definition of the variable.
    Equiv(Lit),
}

/// Compact encoding of a (u31, Option<VarDef>)
#[derive(Default)]
struct EncodedVarDef {
    level_bound_and_flag: u32,
    id: Option<Id32>,
}

impl EncodedVarDef {
    const FLAG_MASK: u32 = 1 << 31;

    pub fn var_def(&self) -> Option<VarDef> {
        self.id.map(|id| {
            if self.level_bound_and_flag & Self::FLAG_MASK != 0 {
                VarDef::Equiv(Lit::from_base_id(id))
            } else {
                VarDef::Node(NodeId::from_base_id(id))
            }
        })
    }

    pub fn var_def_node(&self) -> Option<NodeId> {
        if self.level_bound_and_flag & Self::FLAG_MASK != 0 {
            None
        } else {
            self.id.map(NodeId::from_base_id)
        }
    }

    #[allow(dead_code)] // serves as documentation for the more specialized methods
    pub fn set_var_def(&mut self, var_repr: Option<VarDef>) {
        self.id = var_repr.map(|repr| match repr {
            VarDef::Node(node_id) => node_id.into_base_id(),
            VarDef::Equiv(lit) => lit.into_base_id(),
        });
        if matches!(var_repr, Some(VarDef::Equiv(_))) {
            self.level_bound_and_flag &= !Self::FLAG_MASK;
        } else {
            self.level_bound_and_flag |= Self::FLAG_MASK;
        }
    }

    pub fn clear_var_repr(&mut self) {
        self.id = None;
        self.level_bound_and_flag &= !Self::FLAG_MASK;
    }

    pub fn set_node_var_repr(&mut self, node: NodeId) {
        self.id = Some(node.into_base_id());
        self.level_bound_and_flag &= !Self::FLAG_MASK;
    }

    pub fn set_equiv_var_repr(&mut self, lit: Lit) {
        self.id = Some(lit.into_base_id());
        self.level_bound_and_flag |= Self::FLAG_MASK;
    }

    pub fn var_repr_is_none(&self) -> bool {
        self.id.is_none()
    }

    pub fn level_bound(&self) -> u32 {
        self.level_bound_and_flag & !Self::FLAG_MASK
    }

    pub fn set_level_bound(&mut self, level_bound: u32) {
        debug_assert!(level_bound < Self::FLAG_MASK);
        self.level_bound_and_flag = (self.level_bound_and_flag & Self::FLAG_MASK) | level_bound;
    }
}

/// Maintains the primary definitions of all variables.
///
/// This combines a polarity-aware union-find data structure over all equivalent literals with a map
/// storing the defining node for each equivalence class representative.
// TODO expand and explain the level bound
#[derive(Default)]
pub struct VarDefs {
    var_defs: IdVec<Var, EncodedVarDef>,
}

impl VarDefs {
    /// Retrieves the primary definition of a variable.
    ///
    /// Returns `None` when the environment does not contain a designed primary definition.
    pub fn var_def(&self, var: Var) -> Option<VarDef> {
        self.var_defs.get(var)?.var_def()
    }

    /// Returns the canonical representative literal equivalent to the given literal.
    fn lit_repr(&self, mut lit: Lit) -> Lit {
        while let Some(VarDef::Equiv(repr)) = self.var_def(lit.var()) {
            lit = repr ^ lit.pol();
        }

        lit
    }

    /// Returns the canonical representative variable for the given variable.
    ///
    /// This ignores polarities, and thus might return a variable that's equivalent to the negation
    /// of the passed variable. Most of the time, [`lit_repr`][Self::lit_repr] should be used
    /// instead.
    fn var_repr(&self, mut var: Var) -> Var {
        while let Some(VarDef::Equiv(repr)) = self.var_def(var) {
            var = repr.var()
        }

        var
    }

    /// Returns a best-effort upper bound on the level of the node that defines a given variable.
    // TODO go into more detail on how this is maintained, intended to be used, guarded inputs, etc.
    fn level_bound(&self, var: Var) -> u32 {
        if let Some(repr) = self.var_defs.get(self.var_repr(var)) {
            repr.level_bound()
        } else {
            0
        }
    }
}

/// An environment for storing and maintaining the internal representation.
///
/// The environment is parametrized by an [`EnvConfig`] configuration, which affects the invariants
/// and indices maintained for the environment.
// TODO add a general overview, as this is the main entry point for the IR
pub struct Env<Config: EnvConfig> {
    nodes: Nodes,
    var_defs: VarDefs,
    config: Config,
}

impl<Config: EnvConfig> Default for Env<Config> {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            var_defs: Default::default(),
            config: Default::default(),
        }
    }
}

impl<Config: EnvConfig> Env<Config> {
    /// Returns a read-only view of the [nodes][`Nodes`] stored in this environment.
    pub fn nodes(&self) -> &Nodes {
        &self.nodes
    }

    /// Insert a node, assuming it is canonicalized and not yet part of the environment.
    ///
    /// While it is memory-safe to violate the canonicity or uniqueness assumptions, doing so may
    /// prevent the environment from maintaining the configured invariants and indices.
    pub fn insert_unique_canonical_node<T: Node>(&mut self, node: T) -> (NodeId, &T) {
        self.var_defs.var_defs.grow_for_key(node.max_var());

        let output_var = node.output_var();

        let (node_id, node_ref, nodes) = self.nodes.insert_and_get(node);

        let node_role = if let Some(output_var) = output_var {
            let mut encoded_var_repr = &mut self.var_defs.var_defs[output_var];
            if encoded_var_repr.var_repr_is_none() {
                encoded_var_repr.set_node_var_repr(node_id);

                let mut level_bound = 0;

                for var in node_ref.unguarded_input_var_iter() {
                    level_bound = level_bound.max(self.var_defs.level_bound(var) + 1);
                }
                encoded_var_repr = &mut self.var_defs.var_defs[output_var];

                encoded_var_repr.set_level_bound(level_bound);
                NodeRole::PrimaryDef(output_var)
            } else {
                NodeRole::Equivalence(output_var)
            }
        } else {
            NodeRole::Constraint
        };

        self.config
            .add_node(DynamicIndexContext { nodes }, node_id, node_ref, node_role);

        (node_id, node_ref)
    }

    fn get_node_with_role<'a>(
        nodes: &'a Nodes,
        var_reprs: &VarDefs,
        node_id: NodeId,
    ) -> Option<(&'a DynNode, NodeRole)> {
        let node_ref = nodes.get(node_id)?;

        let output_var = node_ref.output_var();

        let node_role = if let Some(output_var) = output_var {
            if var_reprs.var_def(output_var) == Some(VarDef::Node(node_id)) {
                NodeRole::PrimaryDef(output_var)
            } else {
                NodeRole::Equivalence(output_var)
            }
        } else {
            NodeRole::Constraint
        };

        Some((node_ref, node_role))
    }

    /// Remove a node from the environment.
    ///
    /// This does not remove any variables and thus calling this can leave used variables without
    /// any primary definition.
    pub fn discard_node(&mut self, node_id: NodeId) -> bool {
        let Some((node_ref, node_role)) =
            Self::get_node_with_role(&self.nodes, &self.var_defs, node_id)
        else {
            return false;
        };

        self.config.remove_dyn_node(
            DynamicIndexContext { nodes: &self.nodes },
            node_id,
            node_ref,
            node_role,
        );

        if let NodeRole::PrimaryDef(output_var) = node_role {
            self.var_defs.var_defs[output_var] = Default::default();
        }

        self.nodes.discard(node_id);

        true
    }

    /// Adds and returns a new variable to the environment.
    ///
    /// Note that the resulting variable starts out without any definition and with a level bound of
    /// zero. For some use cases this is an issue for maintaining acyclicity of the primary
    /// definition graph. In those cases,
    /// [`fresh_var_with_level_bound`][Self::fresh_var_with_level_bound] can be used instead.
    pub fn fresh_var(&mut self) -> Var {
        self.var_defs.var_defs.push(EncodedVarDef::default()).0
    }

    /// Adds and returns a new variable to the environment, using a given level bound.
    pub fn fresh_var_with_level_bound(&mut self, level_bound: u32) -> Var {
        let mut encoded_var_repr = EncodedVarDef::default();
        encoded_var_repr.set_level_bound(level_bound);

        self.var_defs.var_defs.push(encoded_var_repr).0
    }

    /// Ensures the presence of an [expression node][ExprNode] for the given [expression][Expr],
    /// retrieving its node id and its output.
    ///
    /// If there is an existing expression node for the given expression, this returns a tuple
    /// containing its [`NodeId`], its output variable/literal and `false`. Otherwise this inserts a
    /// new expression node and returns a tuple containing the new node's [`NodeId`], output and
    /// `true`.
    ///
    /// Before looking for an existing expression node, the given expression is canonicalized w.r.t.
    /// the known variable/literal equivalences.
    pub fn insert_expr_node<T: Expr>(&mut self, expr: T) -> (NodeId, T::Output, bool) {
        if let Some((node_id, output)) = self
            .config
            .find_expr(DynamicIndexContext { nodes: &self.nodes }, &expr)
        {
            return (node_id, output, false);
        }

        let (new_var, _) = self.var_defs.var_defs.push(EncodedVarDef::default());
        let output = <T::Output as VarOrLit>::from_var_with_pol_for_lit(new_var, Pol::Pos);

        let (node_id, _) = self.insert_unique_canonical_node(ExprNode { output, expr });

        (node_id, output, true)
    }

    /// Ensures the presence of an [expression node][ExprNode] for the given [expression][Expr],
    /// retrieving its output.
    ///
    /// This calls [`insert_expr_node`][Self::insert_expr_node], returning only the output variable
    /// or literal.
    pub fn insert_expr<T: Expr>(&mut self, expr: T) -> T::Output {
        self.insert_expr_node(expr).1
    }

    /// Ensures the presence of a given node, retrieving its node id.
    ///
    /// If the node is already present, it returns a pair of the [`NodeId`] of the existing node and
    /// `false`, otherwise it returns a pair of the newly inserted node's id and `true`.
    ///
    /// If the passed node defines an output variable and there is an existing node that differs
    /// only in their output variable/literal, this makes both output variables/literals equivalent
    /// and considers the node to be already present.
    pub fn insert_node<T: Node>(&mut self, mut node: T) -> (NodeId, bool) {
        node.apply_var_map(|var| self.var_defs.lit_repr(var.as_pos()));

        // TODO decay based on local constants or equivalences (also update docs!)

        if let Some(found_node) = self
            .config
            .find_node(DynamicIndexContext { nodes: &self.nodes }, &node)
        {
            if let Some(equiv) = found_node.equiv {
                self.insert_equiv(equiv);
            }
            return (found_node.node, false);
        }

        let node_id = self.insert_unique_canonical_node(node).0;
        (node_id, true)
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
    pub fn insert_equiv(&mut self, equiv: [Lit; 2]) -> bool {
        let [mut a, mut b] = equiv.map(|lit| self.var_defs.lit_repr(lit));

        self.var_defs.var_defs.grow_for_key(a.max(b).var());

        if a.var() == b.var() {
            // TODO we might want to optionally represent a conflict (if so, also update the docs!)
            assert_eq!(a, b);
            return false;
        }

        if b.is_const() {
            // always prefer a constant value as representative
            (a, b) = (b, a);
        } else {
            // then use the level bound to avoid introducing primary definition cycles
            match self
                .var_defs
                .level_bound(a.var())
                .cmp(&self.var_defs.level_bound(b.var()))
            {
                std::cmp::Ordering::Less => (a, b) = (b, a),
                std::cmp::Ordering::Greater => (),
                std::cmp::Ordering::Equal => {
                    // and break ties by considering the rewriting cost
                    if self.config.rewrite_cost(a.var()) < self.config.rewrite_cost(b.var()) {
                        (a, b) = (b, a);
                    }
                }
            }
        }

        let repr = a ^ b.pol();
        let equiv = b.var();

        if let Some(VarDef::Node(node_id)) = self.var_defs.var_defs[equiv].var_def() {
            // no need to handle Some(VarRepr::Equiv(_)) due to the use of lit_repr above
            self.config.change_primary_def(
                DynamicIndexContext { nodes: &self.nodes },
                equiv,
                Some(node_id),
                None,
            );
        }

        self.var_defs.var_defs[equiv].set_equiv_var_repr(repr);

        self.config
            .add_equiv(DynamicIndexContext { nodes: &self.nodes }, repr, equiv);
        true
    }
}

impl Env<config::Indexed> {
    // TODO use a trait bound instead of concrete config::Indexed

    /// Restore egraph invariants for the environment's node graph.
    // TODO go into more detail
    pub fn rebuild_egraph(&mut self) {
        let mut node_ids = vec![];
        let mut pending = vec![];

        let mut renamed_vars = 0;
        let mut rewritten_nodes = 0;
        let mut redundant_nodes = 0;
        let mut found_congruences = 0;
        let mut passes = 0;

        loop {
            swap(&mut pending, &mut self.config.pending_equivs);
            if pending.is_empty() {
                break;
            }

            log::trace!(
                "egraph rebuild pass {passes}: (len {}) {pending:?}",
                pending.len(),
            );

            passes += 1;
            renamed_vars += pending.len();

            for var in pending.drain(..) {
                log::trace!("egraph rebuild var {var}");
                node_ids.clear();
                node_ids.extend(self.config.defs_index.find_non_primary_defs_unordered(var));
                node_ids.extend(self.config.uses_index.find_uses_unordered(var));
                node_ids.extend(self.var_defs.var_defs[var].var_def_node());
                node_ids.sort_unstable();
                node_ids.dedup();

                log::trace!(
                    "egraph rebuild nodes: (len {}) {node_ids:?}",
                    node_ids.len(),
                );

                for node_id in node_ids.drain(..) {
                    log::trace!("egraph rebuild node: {node_id:?}");

                    let (node, node_role) =
                        Self::get_node_with_role(&self.nodes, &self.var_defs, node_id).unwrap();

                    self.config.remove_dyn_node(
                        DynamicIndexContext { nodes: &self.nodes },
                        node_id,
                        node,
                        node_role,
                    );

                    let mut restore_var_repr = false;
                    let mut restore_level_bound = 0;

                    if let Some(output_var) = node.output_var() {
                        if self.var_defs.var_defs[output_var].var_def()
                            == Some(VarDef::Node(node_id))
                        {
                            restore_var_repr = true;
                            restore_level_bound = self.var_defs.var_defs[output_var].level_bound();
                            self.var_defs.var_defs[output_var].clear_var_repr();
                        }
                    }

                    rewritten_nodes += 1;

                    self.nodes
                        .get_mut(node_id)
                        .unwrap()
                        .dyn_apply_var_map(&mut |var| self.var_defs.lit_repr(var.as_pos()));

                    let node = self.nodes.get(node_id).unwrap();

                    if let Some(found_node) = self
                        .config
                        .find_dyn_node(DynamicIndexContext { nodes: &self.nodes }, node)
                    {
                        let other_node = self.nodes.get(found_node.node).unwrap();

                        if let Some(equiv) = found_node.equiv {
                            log::trace!("egraph congruence {equiv:?} {node:?} {other_node:?}");
                            found_congruences += self.insert_equiv(equiv) as usize;
                        } else {
                            log::trace!("egraph redundant node {node:?} {other_node:?}");
                            redundant_nodes += 1;
                        }
                        self.nodes.discard(node_id);
                        continue;
                    }

                    if restore_var_repr {
                        if let Some(output_var) = node.output_var() {
                            if self.var_defs.var_defs[output_var].var_repr_is_none() {
                                self.var_defs.var_defs[output_var].set_node_var_repr(node_id);
                                self.var_defs.var_defs[output_var]
                                    .set_level_bound(restore_level_bound);
                            }
                        }
                    }

                    self.config.add_dyn_node(
                        DynamicIndexContext { nodes: &self.nodes },
                        node_id,
                        node,
                        node_role,
                    );
                }
            }
        }

        log::debug!("egraph passes={passes}");
        log::debug!("egraph renamed_vars={renamed_vars}");
        log::debug!("egraph rewritten_nodes={rewritten_nodes}");
        log::debug!("egraph found_congruences={found_congruences}");
        log::debug!("egraph redundant_nodes={redundant_nodes}");
    }
}
