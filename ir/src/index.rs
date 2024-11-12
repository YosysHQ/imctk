//! Indices and index maintenance for environments.
use std::hash::{BuildHasher, BuildHasherDefault};
use table_seq::TableSeq;
use zwohash::ZwoHasher;

use crate::var::Var;

use crate::node::{
    collections::nodes::Nodes,
    generic::{DynNode, Node},
    NodeId,
};

use super::{
    env::NodeRole,
    node::generic::{DynTerm, Term, TermNode},
    var::Lit,
};

/// Types that maintain a dynamic index of an environment.
pub trait DynamicIndex {
    /// Additional context required for index maintenance.
    type Context<'a>;

    /// Process the addition of a new dynamically typed node.
    fn add_dyn_node(
        &mut self,
        context: Self::Context<'_>,
        node_id: NodeId,
        node: &DynNode,
        node_role: NodeRole,
    );

    /// Process the removal of a new dynamically typed node.
    fn remove_dyn_node(
        &mut self,
        context: Self::Context<'_>,
        node_id: NodeId,
        node: &DynNode,
        node_role: NodeRole,
    );

    /// Process the addition of a new statically typed node.
    ///
    /// By default, this forwards to the dynamically typed version and indices cannot expect
    /// additions and removals to be consistent w.r.t. static vs. dynamic typing.
    fn add_node<T: Node>(
        &mut self,
        context: Self::Context<'_>,
        node_id: NodeId,
        node: &T,
        node_role: NodeRole,
    ) {
        self.add_dyn_node(context, node_id, node, node_role)
    }

    /// Process the removal of a new statically typed node.
    ///
    /// By default, this forwards to the dynamically typed version and indices cannot expect
    /// additions and removals to be consistent w.r.t. static vs. dynamic typing.
    fn remove_node<T: Node>(
        &mut self,
        context: Self::Context<'_>,
        node_id: NodeId,
        node: &T,
        node_role: NodeRole,
    ) {
        self.remove_dyn_node(context, node_id, node, node_role)
    }

    /// Process a change in the node considered the primary definition for a variable.
    fn change_primary_def(
        &mut self,
        context: Self::Context<'_>,
        var: Var,
        old_primary_def: Option<NodeId>,
        new_primary_def: Option<NodeId>,
    );

    /// Process the addition of a literal equivalence.
    fn add_equiv(&mut self, context: Self::Context<'_>, repr: Lit, equiv: Var);
}

/// Index to look up a [`NodeId`] given a defining [`Node`] or [`Term`].
#[derive(Default)]
pub struct StructuralHashIndex {
    tables: TableSeq<NodeId>,
}

impl DynamicIndex for StructuralHashIndex {
    type Context<'a> = &'a Nodes;

    fn add_dyn_node(
        &mut self,
        nodes: &Nodes,
        node_id: NodeId,
        node: &DynNode,
        _node_rol: NodeRole,
    ) {
        let hash = node.def_hash();

        let var = node.representative_input_var();

        if self.tables.len() <= var.index() {
            self.tables.resize(var.index() + 1);
        }

        self.tables
            .insert_unique(var.index(), hash, node_id, |&node_id| {
                nodes.get_dyn(node_id).unwrap().def_hash()
            });
    }

    fn remove_dyn_node(
        &mut self,
        nodes: &Nodes,
        node_id: NodeId,
        node: &DynNode,
        _node_role: NodeRole,
    ) {
        let hash = node.def_hash();

        let var = node.representative_input_var();

        assert!(
            self.tables
                .remove(
                    var.index(),
                    hash,
                    |&candidate| candidate == node_id,
                    |&node_id| nodes.get_dyn(node_id).unwrap().def_hash(),
                )
                .is_some(),
            "removed node is missing {} {node_id:?} {:?}",
            var,
            nodes.get_dyn(node_id)
        );
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

/// A node found by the [`StructuralHashIndex`].
#[derive(Debug)]
pub struct FoundNode {
    /// The id of the found node.
    pub node_id: NodeId,
    /// The output equivalence implied by the target and found node.
    pub equiv: Option<[Lit; 2]>, // TODO dedicated type for an equivalence?
}

impl StructuralHashIndex {
    /// Find an existing [`TermNode`] for a given [`Term`].
    pub fn find_term<T: Term>(&self, nodes: &Nodes, term: &T) -> Option<(NodeId, T::Output)> {
        let hash = term.def_hash();
        let var = term.representative_input_var();

        if var.index() < self.tables.len() {
            let mut output = <T::Output>::default();
            let node_id = *self.tables.find(var.index(), hash, |&candidate| {
                let Some(candidate_ref) = nodes.get_dyn(candidate) else {
                    return false;
                };
                let Some(candidate_ref) = candidate_ref.dyn_cast::<TermNode<T>>() else {
                    return false;
                };
                let eq = candidate_ref.term.def_eq(term);
                if eq {
                    output = candidate_ref.output;
                }
                eq
            })?;

            Some((node_id, output))
        } else {
            None
        }
    }

    /// Find an existing [`TermNode`] for a given dynamically typed [`Term`].
    pub fn find_dyn_term(&self, nodes: &Nodes, term: &DynTerm) -> Option<(NodeId, Lit)> {
        let hash = term.def_hash();
        let var = term.representative_input_var();

        if var.index() < self.tables.len() {
            let mut output = Lit::FALSE;
            let node_id = *self.tables.find(var.index(), hash, |&candidate| {
                let Some(candidate_ref) = nodes.get_dyn(candidate) else {
                    return false;
                };
                let Some(candidate_term_ref) = candidate_ref.dyn_term() else {
                    return false;
                };
                let eq = candidate_term_ref.dyn_def_eq(term);
                if eq {
                    output = candidate_ref.output_lit().unwrap();
                }
                eq
            })?;

            Some((node_id, output))
        } else {
            None
        }
    }

    /// Find an existing [`Node`] that may differ from the given node in the used output
    /// variable/literal. Statically typed version.
    pub fn find_node<T: Node>(&self, nodes: &Nodes, node: &T) -> Option<FoundNode> {
        let hash = node.def_hash();
        let var = node.representative_input_var();

        if var.index() >= self.tables.len() {
            return None;
        }

        let found = *self.tables.find(var.index(), hash, |&candidate| {
            node.dyn_def_eq(nodes.get_dyn(candidate).unwrap())
        })?;

        let mut equiv = None;

        if let Some(node_output) = node.output_lit() {
            let found_node = nodes.get_dyn(found).unwrap();

            if let Some(found_output) = found_node.output_lit() {
                equiv = Some([node_output, found_output]);
            }
        }

        Some(FoundNode {
            node_id: found,
            equiv,
        })
    }

    /// Find an existing [`Node`] that may differ from the given node in the used output
    /// variable/literal. Dynamically typed version.
    pub fn find_dyn_node(&self, nodes: &Nodes, node: &DynNode) -> Option<FoundNode> {
        // TODO this is a literal duplicate of the code above, but not an instance of the generic
        // function above, what's the best way to dedup this?
        let hash = node.def_hash();
        let var = node.representative_input_var();

        if var.index() >= self.tables.len() {
            return None;
        }

        let found = *self.tables.find(var.index(), hash, |&candidate| {
            node.dyn_def_eq(nodes.get_dyn(candidate).unwrap())
        })?;

        let mut equiv = None;

        if let Some(node_output) = node.output_lit() {
            let found_node = nodes.get_dyn(found).unwrap();

            if let Some(found_output) = found_node.output_lit() {
                equiv = Some([node_output, found_output]);
            }
        }

        Some(FoundNode {
            node_id: found,
            equiv,
        })
    }
}

/// Index to look up nodes given one of their input variables.
#[derive(Default)]
pub struct UsesIndex {
    tables: TableSeq<NodeId>,
}

impl DynamicIndex for UsesIndex {
    type Context<'a> = ();

    fn add_dyn_node(
        &mut self,
        _context: (),
        node_id: NodeId,
        node: &DynNode,
        _node_role: NodeRole,
    ) {
        let hash = Self::node_id_hash(node_id);

        node.dyn_foreach_input_var(&mut |var: Var| {
            if self.tables.len() <= var.index() {
                self.tables.resize(var.index() + 1);
            }

            self.tables.insert(
                var.index(),
                hash,
                node_id,
                |&candidate, _| candidate == node_id,
                |&node_id| Self::node_id_hash(node_id),
            );

            true
        });
    }

    fn add_node<T: Node>(&mut self, _context: (), node_id: NodeId, node: &T, _node_role: NodeRole) {
        let hash = Self::node_id_hash(node_id);

        for var in node.input_var_iter() {
            if self.tables.len() <= var.index() {
                self.tables.resize(var.index() + 1);
            }

            self.tables.insert(
                var.index(),
                hash,
                node_id,
                |&candidate, _| candidate == node_id,
                |&node_id| Self::node_id_hash(node_id),
            );
        }
    }

    fn remove_dyn_node(
        &mut self,
        _context: (),
        node_id: NodeId,
        node: &DynNode,
        _node_role: NodeRole,
    ) {
        let hash = Self::node_id_hash(node_id);

        node.dyn_foreach_input_var(&mut |var: Var| {
            self.tables.remove(
                var.index(),
                hash,
                |&candidate| candidate == node_id,
                |&node_id| Self::node_id_hash(node_id),
            );

            true
        });
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

impl UsesIndex {
    fn node_id_hash(node_id: NodeId) -> u64 {
        <BuildHasherDefault<ZwoHasher>>::default().hash_one(node_id)
    }

    /// Returns the node ids of all nodes that use the given variable as input.
    ///
    /// The order of the resulting iterator is unspecified and should not be dependent upon for any
    /// oreder-sensitive operation.
    pub fn find_uses_unordered(&self, var: Var) -> impl Iterator<Item = NodeId> + '_ {
        (if var.index() < self.tables.len() {
            Some(self.tables.subtable_iter(var.index()).copied())
        } else {
            None
        })
        .into_iter()
        .flatten()
    }

    /// Returns the number of nodes that use the given variable as input.
    ///
    /// This is a constant time operation.
    pub fn use_count(&self, var: Var) -> usize {
        if var.index() < self.tables.len() {
            self.tables.subtable_len(var.index())
        } else {
            0
        }
    }
}

/// Index to look up non-primary definition nodes that have a given variable as output.
#[derive(Default)]
pub struct DefsIndex {
    tables: TableSeq<NodeId>,
}

impl DynamicIndex for DefsIndex {
    type Context<'a> = ();
    fn add_dyn_node(&mut self, _: (), node_id: NodeId, node: &DynNode, node_role: NodeRole) {
        if matches!(node_role, NodeRole::PrimaryDef(_)) {
            return;
        }
        let Some(output_var) = node.output_var() else {
            return;
        };

        if self.tables.len() <= output_var.index() {
            self.tables.resize(output_var.index() + 1);
        }

        let hash = Self::node_id_hash(node_id);

        self.tables
            .insert_unique(output_var.index(), hash, node_id, |&node_id| {
                Self::node_id_hash(node_id)
            });
    }

    fn remove_dyn_node(&mut self, _: (), node_id: NodeId, node: &DynNode, node_role: NodeRole) {
        if matches!(node_role, NodeRole::PrimaryDef(_)) {
            return;
        }

        let Some(output_var) = node.output_var() else {
            return;
        };

        if self.tables.len() > output_var.index() {
            let hash = Self::node_id_hash(node_id);
            self.tables.remove(
                output_var.index(),
                hash,
                |&candidate| candidate == node_id,
                |&node_id| Self::node_id_hash(node_id),
            );
        }
    }

    fn change_primary_def(
        &mut self,
        _: (),
        var: Var,
        old_primary_def: Option<NodeId>,
        new_primary_def: Option<NodeId>,
    ) {
        if old_primary_def == new_primary_def {
            return;
        }

        if let Some(new_primary_def) = new_primary_def {
            if var.index() < self.tables.len() {
                let new_hash = Self::node_id_hash(new_primary_def);
                self.tables.remove(
                    var.index(),
                    new_hash,
                    |&candidate| candidate == new_primary_def,
                    |&node_id| Self::node_id_hash(node_id),
                );
            }
        }
        if let Some(old_primary_def) = old_primary_def {
            if self.tables.len() <= var.index() {
                self.tables.resize(var.index() + 1);
            }
            let old_hash = Self::node_id_hash(old_primary_def);
            self.tables
                .insert_unique(var.index(), old_hash, old_primary_def, |&node_id| {
                    Self::node_id_hash(node_id)
                });
        }
    }

    #[inline(always)]
    fn add_equiv(&mut self, _context: Self::Context<'_>, _repr: Lit, _equiv: Var) {}
}

impl DefsIndex {
    fn node_id_hash(node_id: NodeId) -> u64 {
        <BuildHasherDefault<ZwoHasher>>::default().hash_one(node_id)
    }

    /// Returns the node ids of all nodes that use the given variable as output and are not the
    /// primary definition for that variable.
    ///
    /// The order of the resulting iterator is unspecified and should not be dependent upon for any
    /// oreder-sensitive operation.
    pub fn find_non_primary_defs_unordered(&self, var: Var) -> impl Iterator<Item = NodeId> + '_ {
        (if var.index() < self.tables.len() {
            Some(self.tables.subtable_iter(var.index()).copied())
        } else {
            None
        })
        .into_iter()
        .flatten()
    }

    /// Returns the number of nodes that use the given variable as output and are not the primary
    /// definition for that variable.
    ///
    /// This is a constant time operation.
    pub fn non_primary_def_count(&self, var: Var) -> usize {
        if var.index() < self.tables.len() {
            self.tables.subtable_len(var.index())
        } else {
            0
        }
    }
}
