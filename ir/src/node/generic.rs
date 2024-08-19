//! Traits and utilities for writing code that is generic over node and term types.
use std::{
    fmt::Debug,
    hash::{BuildHasher, BuildHasherDefault, Hash},
};

use imctk_transparent::{NewtypeCast, SubtypeCast};
use imctk_util::{give_take::Take, vec_sink::VecSink};
use zwohash::ZwoHasher;

use crate::{
    env::Env,
    var::{Lit, Pol, Var, VarOrLit},
};

use super::{
    builder::NodeBuilder,
    collections::buf::NodeBuf,
    vtables::{DynNodeType, DynTermType, GenericNodeType, GenericTermType},
};

pub(crate) mod sealed {
    /// Wrapper type to have crate private const items in public traits
    pub struct SealedWrapper<T>(pub T);
}

pub(crate) use sealed::SealedWrapper;

pub use super::vtables::{NodeType, TermType};

/// Allows using a type for internal represenation nodes.
///
/// Everything that is object-safe is part of the [`NodeDyn`] supertrait.
pub trait Node: NodeDyn + Debug + Eq + Hash + Clone + 'static {
    /// A short name identifying the node type.
    const NAME: &'static str;

    #[doc(hidden)]
    const TERM_TYPE_FOR_TERM_WRAPPER: Option<SealedWrapper<fn() -> TermType>> = None;

    #[doc(hidden)]
    const STATIC_TYPE_INFO: SealedWrapper<usize> = SealedWrapper(0);

    /// Returns an iterator over all input variables of the node.
    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        [].into_iter()
    }

    /// Returns an iterator over all input variables that should be taken into consideration when
    /// maintaining acyclicity.
    ///
    /// The default implementation forwards to [`input_var_iter`][Self::input_var_iter].
    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.input_var_iter()
    }

    /// Returns whether two nodes can be treated as equivalent.
    ///
    /// In particular for [`TermNode`] nodes, the output variable or literal is ignored as it is
    /// fully defined in terms of the input variables.
    ///
    /// This defaults to forwarding to [`Eq`], which is correct for constraint nodes.
    fn def_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Rewrites all variables in the term using a given mapping.
    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit) {
        let _ = var_map;

        // When a node contains variables, this default implementation isn't correct
        debug_assert!(self.input_var_iter().next().is_none());
        debug_assert!(self.output_var().is_none());
    }

    /// Performs node-local simplifying rewrites.
    ///
    /// This can either update the node in-place, returning `false` or produce replacement nodes in
    /// the passed [`NodeBuilder`], returning `true`.
    ///
    /// It is an error to produce nodes in the builder when returning `false`.
    #[allow(unused_variables)]
    fn reduce(&mut self, builder: &mut impl NodeBuilder) -> bool {
        false
    }
}

/// Object-safe supertrait for [`Node`].
pub trait NodeDyn: NodeDynAuto + Debug {
    /// Returns a hash value of the defining part of a node.
    ///
    /// In particular for [`TermNode`] nodes, the output variable or literal is ignored as it is
    /// fully defined in terms of the input variables.
    ///
    /// This defaults to forwarding to [`Hash`] using a [`ZwoHasher`].
    fn def_hash(&self) -> u64 {
        self.zzz_hidden_default_def_hash()
    }

    /// Returns a representative input variable.
    ///
    /// This is used like a hash value that is restricted to the contained variables or
    /// `Var::FALSE` and enables certain internal optimizations.
    fn representative_input_var(&self) -> Var {
        self.zzz_hidden_default_representative_input_var()
    }

    /// Returns the output variable defined by this node or `None` if this node does not have an
    /// output.
    ///
    /// If the output is a literal, the literal's variable is still considered an output variable in
    /// the context of this method.
    fn output_var(&self) -> Option<Var> {
        None
    }

    /// Returns the output literal defined by this node or `None` if this node does not have an
    /// output.
    ///
    /// If the output is a variable, this should return a positive polarity literal for that
    /// variable, which is what the default implementation does.
    fn output_lit(&self) -> Option<Lit> {
        self.output_var().map(|var| var.as_pos())
    }

    /// Returns the variable with the largest id among all variables referenced by this node.
    fn max_var(&self) -> Var {
        self.zzz_hidden_default_max_var()
    }

    /// Returns the term whose value this node assigns to the node's output or `None` if this node
    /// is not a [`TermNode`].
    fn dyn_term(&self) -> Option<&DynTerm> {
        None
    }
}

/// Automatically implemented object-safe supertrait for [`Node`].
///
/// This contains object-safe methods that will be automatically implemented via a blanket
/// implementation using the provided [`Node`] and [`NodeDyn`] items.
pub trait NodeDynAuto: Debug {
    /// Returns the dynamic [`NodeType`] corresponding to the concrete [`Node`] implementation
    /// for trait object.
    fn node_type(&self) -> NodeType;

    /// Object safe wrapper of [`Node::input_var_iter`].
    ///
    /// See [`Self::dyn_foreach_input_var`] for a version that uses a
    /// [`VecSink`] instead of a dynamic callback.
    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Alternative object safe wrapper of [`Node::input_var_iter`].
    ///
    /// See [`Self::dyn_foreach_input_var`] for a version that uses a dynamic callback instead of a
    /// [`VecSink`].
    fn dyn_append_input_vars(&self, sink: VecSink<Var>);

    /// Object safe wrapper of [`Node::unguarded_input_var_iter`].
    ///
    /// See [`Self::dyn_append_unguarded_input_vars`] for a version that uses a [`VecSink`] instead
    /// of a dynamic callback.
    fn dyn_foreach_unguarded_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Alternative object safe wrapper of [`Node::unguarded_input_var_iter`].
    ///
    /// See [`Self::dyn_foreach_unguarded_input_var`] for a version that uses a dynamic callback
    /// instead of a [`VecSink`].
    fn dyn_append_unguarded_input_vars(&self, sink: VecSink<Var>);

    /// Object safe wrapper of [`Node::def_eq`].
    fn dyn_def_eq(&self, other: &DynNode) -> bool;

    /// Object safe wrapper of [`Node::apply_var_map`].
    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit);

    /// Object safe wrapper of [`Node::reduce`].
    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> bool;

    /// Adds a copy of this node to the given environment using a given variable mapping.
    fn dyn_add_to_env_with_var_map(&self, env: &mut Env, var_map: &mut dyn FnMut(Var) -> Lit);

    /// Adds a copy of this node to the given node buffer using a given variable mapping.
    fn dyn_add_to_buf_with_var_map(&self, buf: &mut NodeBuf, var_map: &mut dyn FnMut(Var) -> Lit);

    #[doc(hidden)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_max_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_def_hash(&self) -> u64;
}

impl<T: Node> NodeDynAuto for T {
    fn node_type(&self) -> NodeType {
        NodeType::of::<T>()
    }

    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool) {
        for var in self.input_var_iter() {
            if !f(var) {
                break;
            }
        }
    }

    fn dyn_append_input_vars(&self, mut sink: VecSink<Var>) {
        sink.extend(self.input_var_iter())
    }

    fn dyn_foreach_unguarded_input_var(&self, f: &mut dyn FnMut(Var) -> bool) {
        for var in self.unguarded_input_var_iter() {
            if !f(var) {
                break;
            }
        }
    }

    fn dyn_append_unguarded_input_vars(&self, mut sink: VecSink<Var>) {
        sink.extend(self.unguarded_input_var_iter())
    }

    fn dyn_def_eq(&self, other: &DynNode) -> bool {
        let Some(other) = other.dyn_cast::<Self>() else {
            return false;
        };

        self.def_eq(other)
    }

    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit) {
        self.apply_var_map(var_repr)
    }

    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> bool {
        self.reduce(buf)
    }

    fn dyn_add_to_env_with_var_map(&self, env: &mut Env, var_map: &mut dyn FnMut(Var) -> Lit) {
        let mut clone = self.clone();
        clone.apply_var_map(var_map);
        env.node(clone);
    }

    fn dyn_add_to_buf_with_var_map(&self, buf: &mut NodeBuf, var_map: &mut dyn FnMut(Var) -> Lit) {
        let mut clone = self.clone();
        clone.apply_var_map(var_map);
        buf.node(clone);
    }

    #[inline(always)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var {
        self.input_var_iter().next().unwrap_or(Var::FALSE)
    }

    #[inline(always)]
    fn zzz_hidden_default_max_var(&self) -> Var {
        self.input_var_iter()
            .max()
            .unwrap_or(Var::FALSE)
            .max(self.output_var().unwrap_or(Var::FALSE))
    }

    #[inline(always)]
    fn zzz_hidden_default_def_hash(&self) -> u64 {
        <BuildHasherDefault<ZwoHasher>>::default().hash_one(self)
    }
}

/// A trait object for a [`Node`].
///
/// Since [`Node`] is not object safe, we cannot use `dyn Node`. Instead have to use a separate
/// [`NodeDyn`] trait and provide this type alias to avoid the need for `dyn NodeDyn`.
pub type DynNode = dyn NodeDyn;

impl DynNode {
    /// Performs a checked dyamic cast of a [`DynNode`] to a given [`Node`] type.
    pub fn dyn_cast<T: Node>(&self) -> Option<&T> {
        if self.node_type() != NodeType::of::<T>() {
            None
        } else {
            // SAFETY: we checked the `node_type` which works like `TypeId`.
            Some(unsafe { &*(self as *const DynNode as *const T) })
        }
    }
}

/// Types that define a value within an environment.
///
/// A [`Term`] type defines a value in an [environment][crate::env]. That value can be given as
/// a function of values assigned to [variables][crate::var] in the environment.
///
/// A term itself is not automatically assigned to a variable. This is done by combining a term and
/// output variable (or literal) in a [`TermNode`].
///
/// Everything that is object-safe is part of the [`TermDyn`] supertrait.
pub trait Term: Debug + Clone + Eq + Hash + TermDyn + 'static {
    /// Whether the output can be represented as a variable or can require a literal.
    type Output: VarOrLit + 'static;

    /// A short name identifying the operation.
    const NAME: &'static str;

    #[doc(hidden)]
    const STATIC_TYPE_INFO: SealedWrapper<usize> = SealedWrapper(0);

    /// Returns an iterator over all input variables of the term.
    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_;

    /// Returns an iterator over all input variables that should be taken into consideration when
    /// maintaining acyclicity.
    ///
    /// The default implementation forwards to [`input_var_iter`][Self::input_var_iter].
    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.input_var_iter()
    }

    /// Rewrites all variables in the term using a given mapping.
    #[must_use]
    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit)
        -> <Self::Output as VarOrLit>::Pol;

    /// Returns whether two [`Term`]s define the same value.
    ///
    /// This defaults to forwarding to [`Eq`].
    fn def_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Performs term-local simplifying rewrites.
    ///
    /// This can either update the term in-place, returning `None` or produce replacement nodes in
    /// the passed [`NodeBuilder`], returning an output variable or literal equivalent to the value
    /// of this term.
    ///
    /// It is an error to produce nodes in the builder when returning `None`.
    #[allow(unused_variables)]
    fn reduce(&mut self, buf: &mut impl NodeBuilder) -> Option<Self::Output> {
        None
    }

    /// Performs node-local simplifying rewrites of a term node holding this term.
    ///
    /// This is invoked when calling [`Node::reduce`] on a [`TermNode`].
    ///
    /// The default implementation performs reduction using [`Self::reduce`], adding an equivalence
    /// of the returned new output and the output previously present in the [`TermNode`].
    ///
    /// The default implementation is accessible as [`default_reduce_node`], which allows
    /// implementations to partially override the default behavior.
    fn reduce_node(&mut self, output: Self::Output, builder: &mut impl NodeBuilder) -> bool {
        default_reduce_node(self, output, builder)
    }

    /// Returns whether this term represents a steady value given a callback to determine whether
    /// the inputs represent steady values.
    fn is_steady(&self, input_steady: impl Fn(Var) -> bool) -> bool {
        for var in self.unguarded_input_var_iter() {
            if !input_steady(var) {
                return false;
            }
        }
        true
    }
}

/// Default implementation of [`Term::reduce_node`].
///
/// This default implementation performs reduction using [`Term::reduce`], adding an equivalence of
/// the returned new output and the output previously present in the [`TermNode`].
///
/// This should only be called when implementing [`Term::reduce_node`] to partially override the
/// default behavior.
pub fn default_reduce_node<T: Term>(
    term: &mut T,
    output: T::Output,
    builder: &mut impl NodeBuilder,
) -> bool {
    let Some(new_output) = term.reduce(builder) else {
        return false;
    };

    if new_output != output {
        builder.equiv(
            [output, new_output].map(|x| x.process_var_or_lit(|var| var.as_pos(), |lit| lit)),
        );
    }

    true
}

/// Object-safe supertrait for [`Term`].
pub trait TermDyn: Debug + TermDynAuto {
    /// Returns the variable with the largest id among all variables referenced by this term.
    #[inline(always)]
    fn max_var(&self) -> Var {
        self.zzz_hidden_default_max_var()
    }

    /// Returns a hash value for the value defined by this term.
    ///
    /// This defaults to forwarding to [`Hash`] using a [`ZwoHasher`].
    #[inline(always)]
    fn def_hash(&self) -> u64 {
        self.zzz_hidden_default_def_hash()
    }

    /// Returns a representative input variable.
    ///
    /// This is used like a hash value that is restricted to the contained variables or
    /// `Var::FALSE` and enables certain internal optimizations.
    #[inline(always)]
    fn representative_input_var(&self) -> Var {
        self.zzz_hidden_default_representative_input_var()
    }
}

/// Automatically implemented object-safe supertrait for [`Term`].
///
/// This contains object-safe methods that will be automatically implemented via a blanket
/// implementation using the provided [`Term`] and [`TermDyn`] items.
pub trait TermDynAuto {
    /// Returns the dynamic [`TermType`] corresponding to the concrete [`Term`] implementation
    /// for trait object.
    fn term_type(&self) -> TermType;

    /// Object safe wrapper of [`Term::input_var_iter`].
    ///
    /// See [`Self::dyn_foreach_input_var`] for a version that uses a [`VecSink`] instead of a
    /// dynamic callback.
    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Alternative object safe wrapper of [`Term::input_var_iter`].
    ///
    /// See [`Self::dyn_foreach_input_var`] for a version that uses a dynamic callback instead of a
    /// [`VecSink`].
    fn dyn_append_input_vars(&self, sink: VecSink<Var>);

    /// Object safe wrapper of [`Term::unguarded_input_var_iter`].
    ///
    /// See [`Self::dyn_append_unguarded_input_vars`] for a version that uses a [`VecSink`] instead
    /// of a dynamic callback.
    fn dyn_foreach_unguarded_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Alternative object safe wrapper of [`Term::unguarded_input_var_iter`].
    ///
    /// See [`Self::dyn_foreach_unguarded_input_var`] for a version that uses a dynamic callback
    /// instead of a [`VecSink`].
    fn dyn_append_unguarded_input_vars(&self, sink: VecSink<Var>);

    /// Object safe wrapper of [`Term::is_steady`] using an environment to look up input steadiness.
    fn dyn_is_steady_in_env(&self, env: &Env) -> bool;

    #[doc(hidden)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_max_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_def_hash(&self) -> u64;

    /// Object safe wrapper of [`Term::def_eq`].
    fn dyn_def_eq(&self, other: &DynTerm) -> bool;

    /// Object safe wrapper of [`Term::apply_var_map`].
    #[must_use]
    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit) -> Pol;

    /// Object safe wrapper of [`Term::reduce`].
    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> Option<Lit>;

    /// Adds a copy of this term to the given node buffer using a given variable mapping.
    fn dyn_add_to_buf_with_var_map(
        &self,
        buf: &mut NodeBuf,
        var_map: &mut dyn FnMut(Var) -> Lit,
    ) -> Lit;

    /// Adds a copy of this term to the given environment using a given variable mapping.
    fn dyn_add_to_env_with_var_map(
        &self,
        env: &mut Env,
        var_map: &mut dyn FnMut(Var) -> Lit,
    ) -> Lit;
}

impl<T: Term> TermDynAuto for T {
    fn term_type(&self) -> TermType {
        TermType::of::<T>()
    }

    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool) {
        for var in self.input_var_iter() {
            if !f(var) {
                break;
            }
        }
    }

    fn dyn_append_input_vars(&self, mut sink: VecSink<Var>) {
        sink.extend(self.input_var_iter())
    }

    fn dyn_foreach_unguarded_input_var(&self, f: &mut dyn FnMut(Var) -> bool) {
        for var in self.unguarded_input_var_iter() {
            if !f(var) {
                break;
            }
        }
    }

    fn dyn_append_unguarded_input_vars(&self, mut sink: VecSink<Var>) {
        sink.extend(self.unguarded_input_var_iter())
    }

    fn dyn_is_steady_in_env(&self, env: &Env) -> bool {
        self.is_steady(|var| env.var_defs().is_steady(var))
    }

    #[inline(always)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var {
        self.input_var_iter().next().unwrap_or(Var::FALSE)
    }

    #[inline(always)]
    fn zzz_hidden_default_max_var(&self) -> Var {
        self.input_var_iter().max().unwrap_or(Var::FALSE)
    }

    #[inline(always)]
    fn zzz_hidden_default_def_hash(&self) -> u64 {
        <BuildHasherDefault<ZwoHasher>>::default().hash_one(self)
    }

    fn dyn_def_eq(&self, other: &DynTerm) -> bool {
        let Some(other) = other.dyn_cast::<Self>() else {
            return false;
        };

        self.def_eq(other)
    }

    #[must_use]
    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit) -> Pol {
        <T::Output as VarOrLit>::process_pol(self.apply_var_map(var_repr), || Pol::Pos, |pol| pol)
    }

    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> Option<Lit> {
        self.reduce(buf).map(|output| {
            <T::Output as VarOrLit>::process_var_or_lit(output, |var| var.as_pos(), |lit| lit)
        })
    }

    fn dyn_add_to_buf_with_var_map(
        &self,
        buf: &mut NodeBuf,
        var_map: &mut dyn FnMut(Var) -> Lit,
    ) -> Lit {
        let mut clone = self.clone();
        let pol = <<Self as Term>::Output>::process_pol(
            clone.apply_var_map(var_map),
            || Pol::Pos,
            |pol| pol,
        );
        buf.term(clone)
            .process_var_or_lit(|var| var.as_lit(), |lit| lit)
            ^ pol
    }

    fn dyn_add_to_env_with_var_map(
        &self,
        env: &mut Env,
        var_map: &mut dyn FnMut(Var) -> Lit,
    ) -> Lit {
        let mut clone = self.clone();
        let pol = <<Self as Term>::Output>::process_pol(
            clone.apply_var_map(var_map),
            || Pol::Pos,
            |pol| pol,
        );

        env.term(clone)
            .process_var_or_lit(|var| var.as_lit(), |lit| lit)
            ^ pol
    }
}

/// A trait object for a [`Term`].
///
/// Since [`Term`] is not object safe, we cannot use `dyn Term`. Instead have to use a separate
/// [`TermDyn`] trait and provide this type alias to avoid the need for `dyn TermDyn`.
pub type DynTerm = dyn TermDyn;

impl DynTerm {
    /// Performs a checked dyamic cast of a [`DynTerm`] to a given [`Term`] type.
    pub fn dyn_cast<T: Term>(&self) -> Option<&T> {
        if self.term_type() != TermType::of::<T>() {
            None
        } else {
            // SAFETY: we checked the `term_type` which works like `TypeId`.
            Some(unsafe { &*(self as *const DynTerm as *const T) })
        }
    }
}

pub(crate) fn dyn_term_into_dyn_term_node<T>(
    output: Lit,
    term: Take<DynTerm>,
    f: impl for<'a> FnOnce(Take<'a, DynNode>) -> T,
) -> T {
    let dyn_type = DynTermType(term.term_type());
    // SAFETY: `Take` means we obtain and pass along ownership. `dyn_type.term_node()` is the
    // correct vtable for the node constructed by `build_term_node` (as apposed to
    // `dyn_type.term_wrapper()`)
    unsafe {
        dyn_type.build_term_node(output, term.into_raw_ptr() as *mut u8, |ptr| {
            f(Take::from_raw_ptr(dyn_type.term_node().cast_mut_ptr(ptr)))
        })
    }
}

pub(crate) fn dyn_term_into_dyn_term_wrapper(term: Take<DynTerm>) -> Take<DynNode> {
    let dyn_type = DynTermType(term.term_type());
    let raw_ptr = term.into_raw_ptr() as *mut u8;
    // SAFETY: `Take` means we obtain and pass along ownership. `TermWrapper` is a transparent
    // wrapper, so we can cast the pointer and `dyn_type.term_wrapper()` is the correct vtable for
    // a `TermWrapper`.
    unsafe { Take::from_raw_ptr(dyn_type.term_wrapper().cast_mut_ptr(raw_ptr)) }
}

pub(crate) fn dyn_term_wrapper_as_dyn_term(wrapper: &DynNode) -> Option<&DynTerm> {
    let node_type = DynNodeType(wrapper.node_type());
    let term_type = DynTermType(node_type.wrapped_term_type()?);
    // SAFETY: `TermWrapper` is a transparent wrapper, so we can cast the pointer and
    // `node_type.wrapped_term_type()` is the correct vtable for a the contained term (or None if we
    // don't have a `TermWrapper`).
    unsafe {
        Some(
            &*term_type
                .cast_mut_ptr(wrapper as *const DynNode as *const u8 as *mut u8)
                .cast_const(),
        )
    }
}

/// Internal only transparent wrapper that turns a [`Term`] into a [`Node`].
///
/// This is used to store `Term` types in internal [`Nodes`] collections. A generic or dynamic
/// `TermWrapper` reference, nor any collection type holding these should ever be exposed as part of
/// the public API.
///
/// Without an associated output variable (as in [`TermNode`]) it is, in geenral, not well-defined
/// how a [`Term`] should behave as a node. Thus some of the [`Node`] methods of a `TermWrapper`
/// will simply panic.
#[derive(SubtypeCast, NewtypeCast, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub(crate) struct TermWrapper<T: Term> {
    pub(crate) term: T,
}

impl<T: Term> Node for TermWrapper<T> {
    const NAME: &'static str = T::NAME;

    const TERM_TYPE_FOR_TERM_WRAPPER: Option<SealedWrapper<fn() -> TermType>> =
        Some(SealedWrapper(TermType::of::<T>));

    const STATIC_TYPE_INFO: SealedWrapper<usize> = T::STATIC_TYPE_INFO;

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.term.input_var_iter()
    }

    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.term.unguarded_input_var_iter()
    }

    fn def_eq(&self, other: &Self) -> bool {
        self.term.def_eq(&other.term)
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) {
        panic!("TermWrapper used as Node");
    }

    fn reduce(&mut self, _builder: &mut impl NodeBuilder) -> bool {
        panic!("TermWrapper used as Node");
    }
}

impl<T: Term> NodeDyn for TermWrapper<T> {
    fn def_hash(&self) -> u64 {
        self.term.def_hash()
    }

    fn representative_input_var(&self) -> Var {
        self.term.representative_input_var()
    }

    fn output_var(&self) -> Option<Var> {
        None
    }

    fn max_var(&self) -> Var {
        self.term.max_var()
    }
}

/// Node type to assign a variable for the value defined by a [`Term`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct TermNode<T: Term> {
    /// The variable or literal holding the value defined by the term.
    pub output: T::Output,
    /// The term defining the value of the output variable.
    pub term: T,
}

impl<T: Term> Node for TermNode<T> {
    const NAME: &'static str = T::NAME;

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.term.input_var_iter()
    }

    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.term.unguarded_input_var_iter()
    }

    fn def_eq(&self, other: &Self) -> bool {
        self.term.def_eq(&other.term)
    }

    fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) {
        let mut new_output_lit = self
            .output
            .process_var_or_lit(|var| var.as_pos(), |lit| lit)
            .lookup(&mut var_map);

        let term_pol = self.term.apply_var_map(var_map);

        new_output_lit ^= term_pol.into();

        let new_output = <T::Output>::build_var_or_lit(
            new_output_lit,
            |lit| {
                assert!(
                    lit.is_pos(),
                    "Term output of non-Boolean type mapped to negative literal"
                );
                lit.var()
            },
            |lit| lit,
        );

        self.output = new_output
    }

    fn reduce(&mut self, builder: &mut impl NodeBuilder) -> bool {
        self.term.reduce_node(self.output, builder)
    }
}

impl<T: Term> NodeDyn for TermNode<T> {
    fn def_hash(&self) -> u64 {
        self.term.def_hash()
    }

    fn representative_input_var(&self) -> Var {
        self.term.representative_input_var()
    }

    fn output_var(&self) -> Option<Var> {
        Some(self.output.process_var_or_lit(|var| var, |lit| lit.var()))
    }

    fn output_lit(&self) -> Option<Lit> {
        Some(
            self.output
                .process_var_or_lit(|var| var.as_pos(), |lit| lit),
        )
    }

    fn max_var(&self) -> Var {
        self.output
            .process_var_or_lit(|var| var, |lit| lit.var())
            .max(self.term.max_var())
    }

    fn dyn_term(&self) -> Option<&DynTerm> {
        Some(&self.term)
    }
}
