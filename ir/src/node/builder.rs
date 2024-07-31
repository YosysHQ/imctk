//! Generic interface for adding equivalences, nodes and terms.

use crate::var::{Lit, VarOrLit};
use imctk_util::give_take::{give, Take};

use super::generic::{DynNode, DynTerm, Node, Term};

/// Types with support for adding equivalences and nodes, including term nodes with automatically
/// assigned output variables.
///
/// Everything that is object-safe is part of the [`NodeBuilderDyn`] supertrait.
pub trait NodeBuilder: NodeBuilderDyn {
    /// Ensure the presence of a term node for the given term, returning the assigned output
    /// variable or literal.
    fn term<T: Term>(&mut self, term: T) -> T::Output {
        give!(term: DynTerm = term);
        <T::Output as VarOrLit>::build_var_or_lit(self.dyn_term(term), |lit| lit.var(), |lit| lit)
    }
    /// Ensure the presence of the given node.
    fn node<T: Node>(&mut self, node: T) {
        give!(node: DynNode = node);
        self.dyn_node(node)
    }
}

/// Object-safe supertrait for [`NodeBuilder`].
pub trait NodeBuilderDyn {
    /// Ensure the presence of a dynamically typed term node, returning the assigned output
    /// literal.
    ///
    /// If the term node has a non-Boolean output, the polarity of the returned literal will be
    /// positive.
    fn dyn_term(&mut self, term: Take<DynTerm>) -> Lit;

    /// Ensure the presence of the given dynamically typed node.
    fn dyn_node(&mut self, node: Take<DynNode>);

    /// Add an equivalence of literals.
    fn equiv(&mut self, equiv: [Lit; 2]);

    /// Returns whether the largest `count` variables are unused by the builder and thus safe to use
    /// as temporary variables.
    ///
    /// Note that it's usually not a good idea to add any nodes, terms or equivalences containing
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
    fn term<T: Term>(&mut self, term: T) -> T::Output {
        give!(term: DynTerm = term);

        let lit = self.dyn_term(term);

        <T::Output as VarOrLit>::build_var_or_lit(lit, |lit| lit.var(), |lit| lit)
    }

    fn node<T: Node>(&mut self, node: T) {
        give!(node: DynNode = node);

        self.dyn_node(node);
    }
}
