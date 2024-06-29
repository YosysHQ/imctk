//! Generic interface for adding equivalences, nodes and values.
use crate::{
    give_take::Take,
    ir::var::{Lit, VarOrLit},
};

use super::generic::{DynNode, DynValue, Node, Value};

/// Types with support for adding equivalences and nodes, including value nodes with automatically
/// assigned output variables.
///
/// Everything that is object-safe is part of the [`NodeBuilderDyn`] supertrait.
pub trait NodeBuilder: NodeBuilderDyn {
    /// Ensure the presence of a value node for the given value, returning the assigned output
    /// variable or literal.
    fn value<T: Value>(&mut self, value: T) -> T::Output {
        give!(value: DynValue = value);
        <T::Output as VarOrLit>::build_var_or_lit(self.dyn_value(value), |lit| lit.var(), |lit| lit)
    }
    /// Ensure the presence of the given node.
    fn node<T: Node>(&mut self, node: T) {
        give!(node: DynNode = node);
        self.dyn_node(node)
    }
}

/// Object-safe supertrait for [`NodeBuilder`].
pub trait NodeBuilderDyn {
    /// Ensure the presence of a dynamically typed value node, returning the assigned output
    /// literal.
    ///
    /// If the value node has a non-Boolean output, the polarity of the returned literal will be
    /// positive.
    fn dyn_value(&mut self, value: Take<DynValue>) -> Lit;

    /// Ensure the presence of the given dynamically typed node.
    fn dyn_node(&mut self, node: Take<DynNode>);

    /// Add an equivalence of literals.
    fn equiv(&mut self, equiv: [Lit; 2]);

    /// Returns whether the largest `count` variables are unused by the builder and thus safe to use
    /// as temporary variables.
    ///
    /// Note that it's usually not a good idea to add any nodes, values or equivalences containing
    /// such variables to the builder, as this will allocate all preceding variables.
    #[allow(unused_variables)]
    fn valid_temporary_vars(&self, count: usize) -> bool {
        false
    }
}

/// A trait object for a [`NodeBuilder`].
///
/// Since [`NodeBuilder`] is not object safe, we cannot use `dyn NodeBuilder`. Instead have to use a
/// separate [`NodeBuilderDyn`] trait and provide this type alias to avoid the need for `dyn
/// NodeBuilderDyn`.
pub type DynNodeBuilder = dyn NodeBuilderDyn;

impl NodeBuilder for DynNodeBuilder {
    fn value<T: Value>(&mut self, value: T) -> T::Output {
        give!(value: DynValue = value);

        let lit = self.dyn_value(value);

        <T::Output as VarOrLit>::build_var_or_lit(lit, |lit| lit.var(), |lit| lit)
    }

    fn node<T: Node>(&mut self, node: T) {
        give!(node: DynNode = node);

        self.dyn_node(node);
    }
}
