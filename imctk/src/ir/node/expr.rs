//! Nodes that define an output variable.
use std::{
    fmt::Debug,
    hash::{BuildHasher, BuildHasherDefault, Hash},
};

use zwohash::ZwoHasher;

use crate::ir::var::{Lit, Pol, Var, VarOrLit};

use super::{Node, NodeDyn};

/// Internal representation types representing expressions.
///
/// An expression defines a value in an [environment][crate::ir::env]. That value can be defined
/// in terms of the values taken by [variables][crate::ir::var] in the environment.
///
/// An expression itself does not assign a variable for the defined value. This is done by
/// wrapping an expression in an [`ExprNode`].
pub trait Expr: Debug + Clone + Eq + Hash + 'static {
    /// Whether the output can be represented as a variable or can require a literal.
    type Output: VarOrLit;

    /// A short name identifying the operation.
    const NAME: &'static str;

    /// Returns an iterator over all input variables of the expression.
    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_;

    /// Returns an iterator over all input variables that should be taken into consideration when
    /// maintaining acyclicity.
    ///
    /// The default implementation forwards to [`input_var_iter`][Self::input_var_iter].
    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.input_var_iter()
    }

    /// Rewrites all variables in the expression using a given mapping.
    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit)
        -> <Self::Output as VarOrLit>::Pol;

    /// Returns the variable with the largest id among all variables referenced by this expression.
    fn max_var(&self) -> Var {
        self.input_var_iter().max().unwrap_or(Var::FALSE)
    }

    /// Returns a representative input variable.
    ///
    /// This is used like a hash value that is restricted to the contained variables or
    /// `Var::FALSE` and enables certain internal optimizations.
    fn representative_input_var(&self) -> Var {
        self.input_var_iter().next().unwrap_or(Var::FALSE)
    }

    /// Returns whether two expressions are equivalent.
    ///
    /// This defaults to forwarding to [`Eq`].
    fn def_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Returns a hash value of this expression.
    ///
    /// This defaults to forwarding to [`Hash`] using a [`ZwoHasher`].
    fn def_hash(&self) -> u64 {
        <BuildHasherDefault<ZwoHasher>>::default().hash_one(self)
    }
}

/// Node type to assign the value of an expression to an output variable.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ExprNode<T: Expr> {
    /// The variable or literal holding the value defined by the expression.
    pub output: T::Output,
    /// The expression defining the value of the output variable or literal.
    pub expr: T,
}

impl<T: Expr> Node for ExprNode<T> {
    const NAME: &'static str = T::NAME;

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.expr.input_var_iter()
    }

    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.expr.unguarded_input_var_iter()
    }

    fn def_eq(&self, other: &Self) -> bool {
        self.expr.def_eq(&other.expr)
    }

    fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) {
        let mut new_output_lit = self.output.as_lit().map_var_to_lit(&mut var_map);

        let expr_pol = self.expr.apply_var_map(var_map);

        new_output_lit ^= expr_pol.into();

        let (new_output, Pol::Pos) = <T::Output>::from_lit_with_residue_pol(new_output_lit) else {
            panic!("Expr output of non-Boolean type mapped to negative literal");
        };

        self.output = new_output
    }
}

impl<T: Expr> NodeDyn for ExprNode<T> {
    fn def_hash(&self) -> u64 {
        self.expr.def_hash()
    }

    fn representative_input_var(&self) -> Var {
        self.expr.representative_input_var()
    }

    fn output_var(&self) -> Option<Var> {
        Some(self.output.var())
    }

    fn max_var(&self) -> Var {
        self.output.var().max(self.expr.max_var())
    }
}
