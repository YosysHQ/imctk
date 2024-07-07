//! Environments for storing and maintaining the internal representation.
use std::mem::{swap, take};

use imctk_ids::{id_vec::IdVec, Id, Id32};
use imctk_transparent::{NewtypeCast, SubtypeCast};

use crate::{
    give_take::Take,
    ir::{
        index::{DefsIndex, UsesIndex},
        var::VarOrLit,
    },
};

use super::{
    index::{DynamicIndex, StructuralHashIndex},
    node::{
        builder::{NodeBuilder, NodeBuilderDyn},
        collections::{
            buf::{NodeBuf, NodeBufVarMap},
            nodes::Nodes,
        },
        generic::{dyn_term_into_dyn_term_node, DynNode, DynTerm, Node, Term, TermNode},
        NodeId,
    },
    var::{Lit, Var},
};

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

impl std::fmt::Debug for EncodedVarDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EncodedVarDef")
            .field("var_def", &self.def())
            .field("level_bound", &self.level_bound())
            .finish()
    }
}

impl EncodedVarDef {
    const FLAG_MASK: u32 = 1 << 31;

    pub fn def(&self) -> Option<VarDef> {
        match (self.level_bound_and_flag & Self::FLAG_MASK != 0, self.id) {
            (true, Some(id)) => Some(VarDef::Equiv(Lit::from_base_id(id))),
            (true, None) => unreachable!(),
            (false, Some(id)) => Some(VarDef::Node(NodeId::from_base_id(id))),
            (false, None) => None,
        }
    }

    pub fn def_node(&self) -> Option<NodeId> {
        if self.level_bound_and_flag & Self::FLAG_MASK != 0 {
            None
        } else {
            self.id.map(NodeId::from_base_id)
        }
    }

    pub fn set_def(&mut self, var_def: Option<VarDef>) {
        match var_def {
            Some(VarDef::Node(node_id)) => {
                self.id = Some(node_id.into_base_id());
                self.level_bound_and_flag &= !Self::FLAG_MASK;
            }
            Some(VarDef::Equiv(lit)) => {
                self.id = Some(lit.into_base_id());
                self.level_bound_and_flag |= Self::FLAG_MASK;
            }
            None => {
                self.id = None;
                self.level_bound_and_flag &= !Self::FLAG_MASK;
            }
        }
    }

    pub fn clear_def(&mut self) {
        self.set_def(None);
    }

    pub fn set_def_node(&mut self, node: NodeId) {
        self.set_def(Some(VarDef::Node(node)));
    }

    pub fn set_def_equiv(&mut self, lit: Lit) {
        self.set_def(Some(VarDef::Equiv(lit)));
    }

    pub fn def_is_none(&self) -> bool {
        self.id.is_none() && (self.level_bound_and_flag & Self::FLAG_MASK == 0)
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
/// correctly updated when adding term nodes, equivalences and during egraph rebuilding, other
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
        self.var_defs.get(var)?.def()
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
            self.var_defs[lit.var()].set_def_equiv(repr ^ lit.pol());
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

#[derive(Default)]
struct EnvIndex {
    structural_hash_index: StructuralHashIndex,
    defs_index: DefsIndex,
    uses_index: UsesIndex,
    pending_equivs: Vec<Var>,
    reduction_queue: Vec<NodeId>,
}

impl EnvIndex {
    pub fn add_node<T: Node>(
        &mut self,
        nodes: &Nodes,
        node_id: NodeId,
        node: &T,
        node_role: NodeRole,
    ) {
        self.structural_hash_index
            .add_node(nodes, node_id, node, node_role);
        self.defs_index.add_node((), node_id, node, node_role);
        self.uses_index.add_node((), node_id, node, node_role);
    }

    pub fn add_dyn_node(
        &mut self,
        nodes: &Nodes,
        node_id: NodeId,
        node: &DynNode,
        node_role: NodeRole,
    ) {
        self.structural_hash_index
            .add_dyn_node(nodes, node_id, node, node_role);
        self.defs_index.add_dyn_node((), node_id, node, node_role);
        self.uses_index.add_dyn_node((), node_id, node, node_role);
    }

    pub fn remove_node<T: Node>(
        &mut self,
        nodes: &Nodes,
        node_id: NodeId,
        node: &T,
        node_role: NodeRole,
    ) {
        self.structural_hash_index
            .remove_node(nodes, node_id, node, node_role);
        self.defs_index.remove_node((), node_id, node, node_role);
        self.uses_index.remove_node((), node_id, node, node_role);
    }

    pub fn remove_dyn_node(
        &mut self,
        nodes: &Nodes,
        node_id: NodeId,
        node: &DynNode,
        node_role: NodeRole,
    ) {
        self.structural_hash_index
            .remove_dyn_node(nodes, node_id, node, node_role);
        self.defs_index
            .remove_dyn_node((), node_id, node, node_role);
        self.uses_index
            .remove_dyn_node((), node_id, node, node_role);
    }

    pub fn change_primary_def(
        &mut self,
        nodes: &Nodes,
        var: Var,
        old_primary_def: Option<NodeId>,
        new_primary_def: Option<NodeId>,
    ) {
        log::trace!("change_primary_def {var} {old_primary_def:?} => {new_primary_def:?}");
        self.structural_hash_index
            .change_primary_def(nodes, var, old_primary_def, new_primary_def);
        self.defs_index
            .change_primary_def((), var, old_primary_def, new_primary_def);
        self.uses_index
            .change_primary_def((), var, old_primary_def, new_primary_def);
    }

    pub fn add_equiv(&mut self, nodes: &Nodes, repr: Lit, equiv: Var) {
        self.structural_hash_index.add_equiv(nodes, repr, equiv);
        self.defs_index.add_equiv((), repr, equiv);
        self.uses_index.add_equiv((), repr, equiv);

        self.pending_equivs.push(equiv);
    }

    #[inline(always)]
    fn rewrite_cost(&self, var: Var) -> usize {
        self.uses_index.use_count(var) + self.defs_index.non_primary_def_count(var)
    }
}

/// An environment for storing and maintaining the internal representation.
// TODO add a general overview, as this is the main entry point for the IR
#[derive(Default)]
pub struct Env {
    nodes: Nodes,
    var_defs: VarDefs,

    index: EnvIndex,

    node_buf: NodeBuf,
    node_buf_var_map: NodeBufVarMap,
}

/// Types that wrap and expose different aspects of an [environment][`Env`].
pub trait EnvWrapper {
    /// Returns a reference to the wrapped environment.
    fn env(&self) -> &Env;
    /// Returns a mutable reference to the wrapped environment.
    // TODO should this be a separate trait to allow wrappers with additional invariants?
    fn env_mut(&mut self) -> &mut Env;
}

mod node_builders;
mod rebuild_egraph;
