//! Environments for storing and maintaining the internal representation.
use std::mem::take;

use config::{DynamicIndexContext, EnvConfig, EnvConfigEgraph};
use imctk_ids::{id_vec::IdVec, Id, Id32};
use imctk_transparent::{NewtypeCast, SubtypeCast};

use crate::{give_take::Take, ir::var::VarOrLit};

use super::{
    node::{
        builder::{NodeBuilder, NodeBuilderDyn},
        collections::{
            buf::{NodeBuf, NodeBufVarMap},
            nodes::Nodes,
        },
        generic::{dyn_value_into_dyn_value_node, DynNode, DynValue, Node, Value, ValueNode},
        NodeId,
    },
    var::{Lit, Var},
};

pub mod config {
    //! Static environment configuration.

    use std::mem::swap;

    use crate::ir::{
        index::{DefsIndex, DynamicIndex, FoundNode, StructuralHashIndex, UsesIndex},
        node::{
            collections::nodes::Nodes,
            generic::{DynNode, DynValue, Node, Value},
            NodeId,
        },
        var::{Lit, Var},
    };

    use super::NodeRole;

    #[allow(unused_imports)] // rustdoc only
    use super::Env;

    mod sealed {

        use crate::ir::node::generic::DynValue;

        use super::*;

        // TODO some of this probably should be public
        pub trait EnvConfigDetail:
            Default + for<'a> DynamicIndex<Context<'a> = DynamicIndexContext<'a>>
        {
            #[allow(unused_variables)]
            #[inline(always)]
            fn find_value<T: Value>(
                &self,
                context: DynamicIndexContext,
                value: &T,
            ) -> Option<(NodeId, T::Output)> {
                None
            }

            #[allow(unused_variables)]
            #[inline(always)]
            fn find_dyn_value(
                &self,
                context: DynamicIndexContext,
                value: &DynValue,
            ) -> Option<(NodeId, Lit)> {
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

        pub trait EnvConfigEgraphDetail {
            fn take_pending_equivs(&mut self, equivs: &mut Vec<Var>);

            fn defs_index(&self) -> &DefsIndex;
            fn uses_index(&self) -> &UsesIndex;
        }
    }

    /// Static environment configuration.
    ///
    /// See [`Env`].
    pub trait EnvConfig: sealed::EnvConfigDetail + 'static {}

    /// Environment configuration supporting egraph rebuilding.
    ///
    /// See [`Env::rebuild_egraph``].

    pub trait EnvConfigEgraph: EnvConfig + sealed::EnvConfigEgraphDetail + 'static {}

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
            _node: &crate::ir::node::generic::DynNode,

            _node_role: NodeRole,
        ) {
        }

        #[inline(always)]
        fn remove_dyn_node(
            &mut self,
            _context: Self::Context<'_>,
            _node_id: crate::ir::node::NodeId,
            _node: &crate::ir::node::generic::DynNode,
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
            node: &crate::ir::node::generic::DynNode,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .add_dyn_node(context.nodes, node_id, node, node_role)
        }

        fn add_node<T: crate::ir::node::generic::Node>(
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
            node: &crate::ir::node::generic::DynNode,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .remove_dyn_node(context.nodes, node_id, node, node_role)
        }

        fn remove_node<T: crate::ir::node::generic::Node>(
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
        fn find_value<T: Value>(
            &self,
            context: DynamicIndexContext,
            value: &T,
        ) -> Option<(NodeId, T::Output)> {
            self.structural_hash_index.find_value(context.nodes, value)
        }
        fn find_dyn_value(
            &self,
            context: DynamicIndexContext,
            value: &DynValue,
        ) -> Option<(NodeId, Lit)> {
            self.structural_hash_index
                .find_dyn_value(context.nodes, value)
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
            node: &crate::ir::node::generic::DynNode,
            node_role: NodeRole,
        ) {
            self.structural_hash_index
                .add_dyn_node(context.nodes, node_id, node, node_role);
            self.defs_index.add_dyn_node((), node_id, node, node_role);
            self.uses_index.add_dyn_node((), node_id, node, node_role);
        }

        fn add_node<T: crate::ir::node::generic::Node>(
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
            node: &crate::ir::node::generic::DynNode,
            node_role: NodeRole,
        ) {
            self.uses_index
                .remove_dyn_node((), node_id, node, node_role);
            self.defs_index
                .remove_dyn_node((), node_id, node, node_role);
            self.structural_hash_index
                .remove_dyn_node(context.nodes, node_id, node, node_role);
        }

        fn remove_node<T: crate::ir::node::generic::Node>(
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
        fn find_value<T: Value>(
            &self,
            context: DynamicIndexContext,
            value: &T,
        ) -> Option<(NodeId, T::Output)> {
            self.structural_hash_index.find_value(context.nodes, value)
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

    impl EnvConfigEgraph for Indexed {}
    impl sealed::EnvConfigEgraphDetail for Indexed {
        fn take_pending_equivs(&mut self, equivs: &mut Vec<Var>) {
            swap(&mut self.pending_equivs, equivs);
        }

        fn defs_index(&self) -> &DefsIndex {
            &self.defs_index
        }

        fn uses_index(&self) -> &UsesIndex {
            &self.uses_index
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
///
/// It also maintains a best-effort upper bound on the level for each variable. The level is defined
/// as the height of the DAG that defines a variable in terms of the inputs. While these bounds are
/// correctly updated when adding value nodes, equivalences and during egraph rebuilding, other
/// operations may not do so. This means correctness of these bounds is not a general environment
/// invariant.
pub struct VarDefs {
    var_defs: IdVec<Var, EncodedVarDef>,
}

impl Default for VarDefs {
    fn default() -> Self {
        Self {
            var_defs: IdVec::from_vec(vec![Default::default()]),
        }
    }
}

impl VarDefs {
    /// Retrieves the primary definition of a variable.
    ///
    /// Returns `None` when the environment does not contain a designed primary definition.
    pub fn var_def(&self, var: Var) -> Option<VarDef> {
        self.var_defs.get(var)?.var_def()
    }

    /// Returns the canonical representative literal equivalent to the given literal.
    pub fn lit_repr(&self, mut lit: Lit) -> Lit {
        while let Some(VarDef::Equiv(repr)) = self.var_def(lit.var()) {
            lit = repr ^ lit.pol();
        }

        lit
    }

    fn update_lit_repr(&mut self, mut lit: Lit) -> Lit {
        let repr = self.lit_repr(lit);

        while let Some(VarDef::Equiv(parent)) = self.var_def(lit.var()) {
            self.var_defs[lit.var()].set_equiv_var_repr(repr ^ lit.pol());
            lit = parent ^ lit.pol();
        }

        repr
    }

    /// Returns the canonical representative variable for the given variable.
    ///
    /// This ignores polarities, and thus might return a variable that's equivalent to the negation
    /// of the passed variable. Most of the time, [`lit_repr`][Self::lit_repr] should be used
    /// instead.
    pub fn var_repr(&self, mut var: Var) -> Var {
        while let Some(VarDef::Equiv(repr)) = self.var_def(var) {
            var = repr.var()
        }

        var
    }

    /// Returns a best-effort upper bound on the level of the node that defines a given variable.
    // TODO go into more detail on how this is maintained, intended to be used, guarded inputs, etc.
    pub fn level_bound(&self, var: Var) -> u32 {
        if let Some(repr) = self.var_defs.get(self.var_repr(var)) {
            repr.level_bound()
        } else {
            0
        }
    }

    /// Returns the number of assigned variables.
    pub fn len(&self) -> usize {
        self.var_defs.len()
    }

    /// Returns whether no variables have been assigned.
    pub fn is_empty(&self) -> bool {
        self.var_defs.is_empty()
    }
}

/// An environment for storing and maintaining the internal representation.
///
/// The environment is parametrized by an [`EnvConfig`] configuration, which affects the invariants
/// and indices maintained for the environment.
// TODO add a general overview, as this is the main entry point for the IR
pub struct Env<Config: EnvConfig = config::Indexed> {
    nodes: Nodes,
    var_defs: VarDefs,
    config: Config,
    node_buf: NodeBuf,
    node_buf_var_map: NodeBufVarMap,
}

impl<Config: EnvConfig> Default for Env<Config> {
    fn default() -> Self {
        Self {
            nodes: Default::default(),
            var_defs: Default::default(),
            config: Default::default(),
            node_buf: Default::default(),
            node_buf_var_map: Default::default(),
        }
    }
}

/// Environment wrapper that provides lower-level access to an [environment's][Env] [nodes][Node].
#[derive(SubtypeCast, NewtypeCast)]
#[repr(transparent)]
pub struct RawEnvNodes<Config: EnvConfig>(Env<Config>);

impl<Config: EnvConfig> RawEnvNodes<Config> {
    /// Returns the a mutable reference to the wrapped environment.
    pub fn env_mut(&mut self) -> &mut Env<Config> {
        &mut self.0
    }

    /// Returns a reference to the wrapped environment.
    pub fn env(&self) -> &Env<Config> {
        &self.0
    }

    /// Insert a node, assuming it is already fully reduced and not yet part of the environment.
    ///
    /// This assumes the value is already fully reduced and will not perform automatic reduction.
    ///
    /// While it is memory-safe to violate the canonicity or uniqueness assumptions, doing so may
    /// prevent the environment from maintaining the configured invariants and indices.
    pub fn insert_unique_irreducible_node<T: Node>(&mut self, node: T) -> (NodeId, &T) {
        self.0.var_defs.var_defs.grow_for_key(node.max_var());

        let output_var = node.output_var();

        let (node_id, node_ref, nodes) = self.0.nodes.insert_and_get(node);

        let node_role = if let Some(output_var) = output_var {
            let mut encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];
            if encoded_var_repr.var_repr_is_none() {
                encoded_var_repr.set_node_var_repr(node_id);

                let mut level_bound = 0;

                for var in node_ref.unguarded_input_var_iter() {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                }
                encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];

                encoded_var_repr.set_level_bound(level_bound);
                NodeRole::PrimaryDef(output_var)
            } else {
                NodeRole::Equivalence(output_var)
            }
        } else {
            NodeRole::Constraint
        };

        self.0
            .config
            .add_node(DynamicIndexContext { nodes }, node_id, node_ref, node_role);

        (node_id, node_ref)
    }

    /// Insert a dynamically typed node, assuming it is already fully reduced and not yet part of
    /// the environment.
    ///
    /// This assumes the value is already fully reduced and will not perform automatic reduction.
    ///
    /// While it is memory-safe to violate the canonicity or uniqueness assumptions, doing so may
    /// prevent the environment from maintaining the configured invariants and indices.
    pub fn insert_unique_irreducible_dyn_node(
        &mut self,
        node: Take<DynNode>,
    ) -> (NodeId, &DynNode) {
        self.0.var_defs.var_defs.grow_for_key(node.max_var());

        let output_var = node.output_var();

        let (node_id, node_ref, nodes) = self.0.nodes.insert_and_get_dyn(node);

        let node_role = if let Some(output_var) = output_var {
            let mut encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];
            if encoded_var_repr.var_repr_is_none() {
                encoded_var_repr.set_node_var_repr(node_id);

                let mut level_bound = 0;

                node_ref.dyn_foreach_unguarded_input_var(&mut |var| {
                    level_bound = level_bound.max(self.0.var_defs.level_bound(var) + 1);
                    true
                });

                encoded_var_repr = &mut self.0.var_defs.var_defs[output_var];

                encoded_var_repr.set_level_bound(level_bound);
                NodeRole::PrimaryDef(output_var)
            } else {
                NodeRole::Equivalence(output_var)
            }
        } else {
            NodeRole::Constraint
        };

        self.0
            .config
            .add_dyn_node(DynamicIndexContext { nodes }, node_id, node_ref, node_role);

        (node_id, node_ref)
    }

    /// Remove a node from the environment.
    ///
    /// This does not remove any variables and thus calling this can leave used variables without
    /// any primary definition.
    pub fn discard_node(&mut self, node_id: NodeId) -> bool {
        let Some((node_ref, node_role)) =
            <Env<Config>>::get_node_with_role(&self.0.nodes, &self.0.var_defs, node_id)
        else {
            return false;
        };

        self.0.config.remove_dyn_node(
            DynamicIndexContext {
                nodes: &self.0.nodes,
            },
            node_id,
            node_ref,
            node_role,
        );

        if let NodeRole::PrimaryDef(output_var) = node_role {
            self.0.var_defs.var_defs[output_var] = Default::default();
        }

        self.0.nodes.discard(node_id);

        true
    }

    /// Ensures the presence of a [value node][ValueNode] for the given [value][Value], retrieving
    /// its node id and its output.
    ///
    /// This assumes the value is already fully reduced and will not perform automatic reduction.
    ///
    /// If there is an existing value node for the given value, this returns a tuple containing the
    /// existing node's [`NodeId`], its output variable/literal and `false`. Otherwise this inserts
    /// a new value node and returns a tuple containing the new node's [`NodeId`], output and
    /// `true`.
    // TODO update docs for the canonical part
    pub fn insert_irreducible_value_node<T: Value>(
        &mut self,
        value: T,
    ) -> (NodeId, T::Output, bool) {
        if let Some((node_id, output)) = self.0.config.find_value(
            DynamicIndexContext {
                nodes: &self.0.nodes,
            },
            &value,
        ) {
            return (node_id, output, false);
        }

        let (new_var, _) = self.0.var_defs.var_defs.push(EncodedVarDef::default());
        let output = <T::Output>::build_var_or_lit(new_var, |var| var, |var| var.as_pos());

        let (node_id, _) = self.insert_unique_irreducible_node(ValueNode { output, value });

        (node_id, output, true)
    }
    /// Ensures the presence of a [value node][ValueNode] for the given dynamically typed
    /// [value][Value], retrieving its node id and its output.
    ///
    /// This assumes the value is already fully reduced and will not perform automatic reduction.
    ///
    /// If there is an existing value node for the given value, this returns a tuple containing the
    /// existing node's [`NodeId`], its output variable/literal and `false`. Otherwise this inserts
    /// a new value node and returns a tuple containing the new node's [`NodeId`], output and
    /// `true`.
    pub fn insert_irreducible_dyn_value_node(
        &mut self,
        value: Take<DynValue>,
    ) -> (NodeId, Lit, bool) {
        if let Some((node_id, output)) = self.0.config.find_dyn_value(
            DynamicIndexContext {
                nodes: &self.0.nodes,
            },
            &*value,
        ) {
            return (node_id, output, false);
        }

        let (new_var, _) = self.0.var_defs.var_defs.push(EncodedVarDef::default());
        let output = new_var.as_pos();

        let (node_id, _) = dyn_value_into_dyn_value_node(output, value, |node| {
            self.insert_unique_irreducible_dyn_node(node)
        });

        (node_id, output, true)
    }

    pub(crate) fn insert_irreducible_dyn_value(&mut self, value: Take<DynValue>) -> Lit {
        self.insert_irreducible_dyn_value_node(value).1
    }

    /// Ensures the presence of a [value node][ValueNode] for the given [value][Value], retrieving
    /// its output.
    ///
    /// This calls [`insert_irreducible_value_node`][Self::insert_irreducible_value_node], returning
    /// only the output variable or literal.
    pub fn insert_irreducible_value<T: Value>(&mut self, value: T) -> T::Output {
        self.insert_irreducible_value_node(value).1
    }

    /// Ensures the presence of a given node, retrieving its node id.
    ///
    /// This assumes the value is already fully reduced and will not perform automatic reduction.
    ///
    /// If the node is already present, it returns a pair of the [`NodeId`] of the existing node and
    /// `false`, otherwise it returns a pair of the newly inserted node's id and `true`.
    ///
    /// If the passed node defines an output variable and there is an existing node that differs
    /// only in their output variable/literal, this makes both output variables/literals equivalent
    /// and considers the node to be already present.
    pub fn insert_irreducible_node<T: Node>(&mut self, node: T) -> (NodeId, bool) {
        if let Some(found_node) = self.0.config.find_node(
            DynamicIndexContext {
                nodes: &self.0.nodes,
            },
            &node,
        ) {
            if let Some(equiv) = found_node.equiv {
                self.0.insert_equiv(equiv);
            }
            return (found_node.node, false);
        }

        let node_id = self.insert_unique_irreducible_node(node).0;
        (node_id, true)
    }

    /// Ensures the presence of a given dynamically typed node, retrieving its node id.
    ///
    /// This assumes the value is already fully reduced and will not perform automatic reduction.
    ///
    /// If the node is already present, it returns a pair of the [`NodeId`] of the existing node and
    /// `false`, otherwise it returns a pair of the newly inserted node's id and `true`.
    ///
    /// If the passed node defines an output variable and there is an existing node that differs
    /// only in their output variable/literal, this makes both output variables/literals equivalent
    /// and considers the node to be already present.
    pub fn insert_irreducible_dyn_node(&mut self, node: Take<DynNode>) -> (NodeId, bool) {
        if let Some(found_node) = self.0.config.find_dyn_node(
            DynamicIndexContext {
                nodes: &self.0.nodes,
            },
            &*node,
        ) {
            if let Some(equiv) = found_node.equiv {
                self.0.insert_equiv(equiv);
            }
            return (found_node.node, false);
        }

        let node_id = self.insert_unique_irreducible_dyn_node(node).0;
        (node_id, true)
    }
}

impl<Config: EnvConfig> NodeBuilderDyn for Env<Config> {
    fn dyn_value(&mut self, mut value: Take<DynValue>) -> Lit {
        let pol = value.dyn_apply_var_map(&mut |var| self.var_defs.update_lit_repr(var.as_pos()));

        if let Some(output) = value.dyn_reduce_into_buf(&mut self.node_buf) {
            let mut node_buf = take(&mut self.node_buf);
            let mut node_buf_var_map = take(&mut self.node_buf_var_map);
            node_buf.drain_into_node_builder(self, &mut node_buf_var_map);
            self.node_buf = node_buf;
            self.node_buf_var_map = node_buf_var_map;

            return output.map_var_to_lit(|var| self.node_buf_var_map.map_var(var)) ^ pol;
        }

        self.raw_nodes().insert_irreducible_dyn_value(value) ^ pol
    }

    fn dyn_node(&mut self, mut node: Take<DynNode>) {
        node.dyn_apply_var_map(&mut |var| self.var_defs.update_lit_repr(var.as_pos()));

        if node.dyn_reduce_into_buf(&mut self.node_buf) {
            let mut node_buf = take(&mut self.node_buf);
            let mut node_buf_var_map = take(&mut self.node_buf_var_map);
            node_buf.drain_into_node_builder(self, &mut node_buf_var_map);
            self.node_buf = node_buf;
            self.node_buf_var_map = node_buf_var_map;
            return;
        }

        self.raw_nodes().insert_irreducible_dyn_node(node);
    }

    fn equiv(&mut self, equiv: [Lit; 2]) {
        self.insert_equiv(equiv);
    }

    fn valid_temporary_vars(&self, count: usize) -> bool {
        (Var::MAX_ID_INDEX.saturating_add(1) - self.var_defs.len()) >= count
    }
}

impl<Config: EnvConfig> NodeBuilder for Env<Config> {
    fn value<T: Value>(&mut self, mut value: T) -> T::Output {
        let pol = value.apply_var_map(|var| self.var_defs.update_lit_repr(var.as_pos()));

        if let Some(output) = value.reduce(self) {
            return output ^ pol;
        }

        self.raw_nodes().insert_irreducible_value(value) ^ pol
    }

    fn node<T: Node>(&mut self, mut node: T) {
        node.apply_var_map(|var| self.var_defs.update_lit_repr(var.as_pos()));

        if node.reduce(self) {
            return;
        }

        self.raw_nodes().insert_irreducible_node(node);
    }
}

impl<Config: EnvConfig> Env<Config> {
    /// Returns a read-only view of the [nodes][`Nodes`] stored in this environment.
    pub fn nodes(&self) -> &Nodes {
        &self.nodes
    }

    /// Provides lower-level access to an environment's [nodes][`Node`].
    pub fn raw_nodes(&mut self) -> &mut RawEnvNodes<Config> {
        RawEnvNodes::from_repr_mut(self)
    }

    /// Returns a read-only view of the primary definitions and level bounds for all variables in
    /// the environment.
    pub fn var_defs(&self) -> &VarDefs {
        &self.var_defs
    }

    fn get_node_with_role<'a>(
        nodes: &'a Nodes,
        var_reprs: &VarDefs,
        node_id: NodeId,
    ) -> Option<(&'a DynNode, NodeRole)> {
        let node_ref = nodes.get_dyn(node_id)?;

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
        } else {
            let a_def = matches!(
                self.var_defs.var_defs[a.var()].var_def(),
                Some(VarDef::Node(_))
            );
            let b_def = matches!(
                self.var_defs.var_defs[b.var()].var_def(),
                Some(VarDef::Node(_))
            );
            // prefer a node with a primary definition as representative, then use the level bound
            // to avoid introducing primary definition cycles
            match a_def.cmp(&b_def).then_with(|| {
                self.var_defs
                    .level_bound(a.var())
                    .cmp(&self.var_defs.level_bound(b.var()))
            }) {
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

            debug_assert!(
                repr.is_const()
                    || matches!(
                        self.var_defs.var_defs[repr.var()].var_def(),
                        Some(VarDef::Node(_))
                    )
            );
        }

        self.var_defs.var_defs[equiv].set_equiv_var_repr(repr);

        self.config
            .add_equiv(DynamicIndexContext { nodes: &self.nodes }, repr, equiv);
        true
    }
}

impl<Config: EnvConfigEgraph> Env<Config> {
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

        loop {
            self.config.take_pending_equivs(&mut pending);
            if pending.is_empty() {
                break;
            }

            log::trace!("egraph rebuild pass {passes}: len {}", pending.len());
            log::trace!("egraph rebuild pending {pending:?}");

            passes += 1;
            renamed_vars += pending.len();

            for var in pending.drain(..) {
                log::trace!("egraph rebuild var {var}");
                node_ids.clear();
                node_ids.extend(
                    self.config
                        .defs_index()
                        .find_non_primary_defs_unordered(var),
                );
                node_ids.extend(self.config.uses_index().find_uses_unordered(var));
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
                        .get_dyn_mut(node_id)
                        .unwrap()
                        .dyn_apply_var_map(&mut |var| self.var_defs.update_lit_repr(var.as_pos()));

                    let node = self.nodes.get_dyn_mut(node_id).unwrap();

                    if node.dyn_reduce_into_buf(&mut node_buf) {
                        reduced_nodes += 1;
                        self.nodes.discard(node_id);
                        node_buf.drain_into_node_builder(self, &mut node_buf_var_map);
                        continue;
                    }

                    let node = self.nodes.get_dyn(node_id).unwrap();

                    if let Some(found_node) = self
                        .config
                        .find_dyn_node(DynamicIndexContext { nodes: &self.nodes }, node)
                    {
                        let other_node = self.nodes.get_dyn(found_node.node).unwrap();

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

        self.node_buf = node_buf;
        self.node_buf_var_map = node_buf_var_map;

        log::debug!("egraph passes={passes}");
        log::debug!("egraph reduced_nodes={reduced_nodes}");
        log::debug!("egraph renamed_vars={renamed_vars}");
        log::debug!("egraph rewritten_nodes={rewritten_nodes}");
        log::debug!("egraph found_congruences={found_congruences}");
        log::debug!("egraph redundant_nodes={redundant_nodes}");
    }
}
