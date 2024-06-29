//! Traits and utilities for writing code that is generic over node and value types.
use std::{
    fmt::Debug,
    hash::{BuildHasher, BuildHasherDefault, Hash},
};

use imctk_transparent::{NewtypeCast, SubtypeCast};
use zwohash::ZwoHasher;

use crate::{
    give_take::Take,
    ir::var::{Lit, Pol, Var, VarOrLit},
};

use super::{
    builder::NodeBuilder,
    collections::buf::NodeBuf,
    vtables::{DynValueType, GenericNodeType, GenericValueType},
};

pub(crate) mod sealed {
    /// Wrapper type to have crate private const items in public traits
    pub struct SealedWrapper<T>(pub T);
}

pub(crate) use sealed::SealedWrapper;

pub use super::vtables::{NodeType, ValueType};

/// Allows using a type for internal represenation nodes.
///
/// Everything that is object-safe is part of the [`NodeDyn`] supertrait.
pub trait Node: NodeDyn + Debug + Eq + Hash + Clone + 'static {
    /// A short name identifying the node type.
    const NAME: &'static str;

    #[doc(hidden)]
    const VALUE_TYPE_FOR_VALUE_WRAPPER: Option<SealedWrapper<fn() -> ValueType>> = None;

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
    /// In particular for [`ValueNode`] nodes, the output variable or literal is ignored as it is
    /// fully defined in terms of the input variables.
    ///
    /// This defaults to forwarding to [`Eq`], which is correct for constraint nodes.
    fn def_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Rewrites all variables in the value using a given mapping.
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
    /// In particular for [`ValueNode`] nodes, the output variable or literal is ignored as it is
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

    /// Returns the value this node assigns to the output node or `None` if this node is not a
    /// [`ValueNode`].
    fn dyn_value(&self) -> Option<&DynValue> {
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
    fn dyn_foreach_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Object safe wrapper of [`Node::unguarded_input_var_iter`].
    fn dyn_foreach_unguarded_input_var(&self, f: &mut dyn FnMut(Var) -> bool);

    /// Object safe wrapper of [`Node::def_eq`].
    fn dyn_def_eq(&self, other: &DynNode) -> bool;

    /// Object safe wrapper of [`Node::apply_var_map`].
    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit);

    /// Object safe wrapper of [`Node::reduce`].
    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> bool;

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

    fn dyn_foreach_unguarded_input_var(&self, f: &mut dyn FnMut(Var) -> bool) {
        for var in self.unguarded_input_var_iter() {
            if !f(var) {
                break;
            }
        }
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

/// Types that define a value.
///
/// A [`Value`] type defines a value in an [environment][crate::ir::env]. That value can be defined
/// in terms of the values assigned to [variables][crate::ir::var] in the environment.
///
/// A value itself is not automatically assigned a variable. This is done by wrapping a value in a
/// [`ValueNode`].
///
/// Everything that is object-safe is part of the [`ValueDyn`] supertrait.
pub trait Value: Debug + Clone + Eq + Hash + ValueDyn + 'static {
    /// Whether the output can be represented as a variable or can require a literal.
    type Output: VarOrLit + 'static;

    /// A short name identifying the operation.
    const NAME: &'static str;

    /// Returns an iterator over all input variables of the value.
    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_;

    /// Returns an iterator over all input variables that should be taken into consideration when
    /// maintaining acyclicity.
    ///
    /// The default implementation forwards to [`input_var_iter`][Self::input_var_iter].
    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.input_var_iter()
    }

    /// Rewrites all variables in the value using a given mapping.
    fn apply_var_map(&mut self, var_map: impl FnMut(Var) -> Lit)
        -> <Self::Output as VarOrLit>::Pol;

    /// Returns whether two [`Value`]s define the same value.
    ///
    /// This defaults to forwarding to [`Eq`].
    fn def_eq(&self, other: &Self) -> bool {
        self == other
    }

    /// Performs value-local simplifying rewrites.
    ///
    /// This can either update the value in-place, returning `None` or produce replacement nodes in
    /// the passed [`NodeBuilder`], returning an output variable or literal equivalent to the given
    /// value.
    ///
    /// It is an error to produce nodes in the builder when returning `None`.
    #[allow(unused_variables)]
    fn reduce(&mut self, buf: &mut impl NodeBuilder) -> Option<Self::Output> {
        None
    }

    /// Performs node-local simplifying rewrites of a value node containing this value.
    ///
    /// This is invoked when calling [`Node::reduce`] on a [`ValueNode`].
    ///
    /// The default implementation performs reduction using [`Self::reduce`], adding an equivalence
    /// of the returned new output and the output previously present in the [`ValueNode`].
    fn reduce_node(&mut self, output: Self::Output, builder: &mut impl NodeBuilder) -> bool {
        let Some(new_output) = self.reduce(builder) else {
            return false;
        };

        if new_output != output {
            builder.equiv(
                [output, new_output].map(|x| x.process_var_or_lit(|var| var.as_pos(), |lit| lit)),
            );
        }

        true
    }
}

/// Object-safe supertrait for [`Value`].
pub trait ValueDyn: Debug + ValueDynAuto {
    /// Returns the variable with the largest id among all variables referenced by this value.
    #[inline(always)]
    fn max_var(&self) -> Var {
        self.zzz_hidden_default_max_var()
    }

    /// Returns a hash value of this value definition.
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

/// Automatically implemented object-safe supertrait for [`Value`].
///
/// This contains object-safe methods that will be automatically implemented via a blanket
/// implementation using the provided [`Value`] and [`ValueDyn`] items.
pub trait ValueDynAuto {
    /// Returns the dynamic [`ValueType`] corresponding to the concrete [`Value`] implementation
    /// for trait object.
    fn value_type(&self) -> ValueType;

    #[doc(hidden)]
    fn zzz_hidden_default_representative_input_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_max_var(&self) -> Var;

    #[doc(hidden)]
    fn zzz_hidden_default_def_hash(&self) -> u64;

    /// Object safe wrapper of [`Value::def_eq`].
    fn dyn_def_eq(&self, other: &DynValue) -> bool;

    /// Object safe wrapper of [`Value::apply_var_map`].
    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit) -> Pol;

    /// Object safe wrapper of [`Value::reduce`].
    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> Option<Lit>;
}

impl<T: Value> ValueDynAuto for T {
    fn value_type(&self) -> ValueType {
        ValueType::of::<T>()
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

    fn dyn_def_eq(&self, other: &DynValue) -> bool {
        let Some(other) = other.dyn_cast::<Self>() else {
            return false;
        };

        self.def_eq(other)
    }

    fn dyn_apply_var_map(&mut self, var_repr: &mut dyn FnMut(Var) -> Lit) -> Pol {
        <T::Output as VarOrLit>::process_pol(self.apply_var_map(var_repr), || Pol::Pos, |pol| pol)
    }

    fn dyn_reduce_into_buf(&mut self, buf: &mut NodeBuf) -> Option<Lit> {
        self.reduce(buf).map(|output| {
            <T::Output as VarOrLit>::process_var_or_lit(output, |var| var.as_pos(), |lit| lit)
        })
    }
}

/// A trait object for a [`Value`].
///
/// Since [`Value`] is not object safe, we cannot use `dyn Value`. Instead have to use a separate
/// [`ValueDyn`] trait and provide this type alias to avoid the need for `dyn ValueDyn`.
pub type DynValue = dyn ValueDyn;

impl DynValue {
    /// Performs a checked dyamic cast of a [`DynNode`] to a given [`Node`] type.
    pub fn dyn_cast<T: Value>(&self) -> Option<&T> {
        if self.value_type() != ValueType::of::<T>() {
            None
        } else {
            // SAFETY: we checked the `node_type` which works like `TypeId`.
            Some(unsafe { &*(self as *const DynValue as *const T) })
        }
    }
}

pub(crate) fn dyn_value_into_dyn_value_node<T>(
    output: Lit,
    value: Take<DynValue>,
    f: impl for<'a> FnOnce(Take<'a, DynNode>) -> T,
) -> T {
    let dyn_type = DynValueType(value.value_type());
    // SAFETY: `Take` means we obtain and pass along ownership. `dyn_type.value_node()` is the
    // correct vtable for the node constructed by `build_value_node` (as apposed to
    // `dyn_type.value_wrapper()`)
    unsafe {
        dyn_type.build_value_node(output, value.into_raw_ptr() as *mut u8, |ptr| {
            f(Take::from_raw_ptr(dyn_type.value_node().cast_mut_ptr(ptr)))
        })
    }
}

pub(crate) fn dyn_value_into_dyn_value_wrapper(value: Take<DynValue>) -> Take<DynNode> {
    let dyn_type = DynValueType(value.value_type());
    let raw_ptr = value.into_raw_ptr() as *mut u8;
    // SAFETY: `Take` means we obtain and pass along ownership. `ValueWrapper` is a transparent
    // wrapper, so we can cast the pointer and `dyn_type.value_wrapper()` is the correct vtable for
    // a `ValueWrapper`.
    unsafe { Take::from_raw_ptr(dyn_type.value_wrapper().cast_mut_ptr(raw_ptr)) }
}

/// Internal only transparent wrapper that turns a [`Value`] into a [`Node`].
///
/// This is used to store `Value` types in internal [`Nodes`] collections. A generic or dynamic
/// `ValueWrapper` reference, nor any collection type holding these should ever be exposed as part
/// of the public API.
///
/// Without an associated output value (as in [`ValueNode`]) it is, in geenral, not well-defined how
/// a [`Value`] should behave as a node. Thus some of the [`Node`] methods of a `ValueWrapper` will
/// simply panic.
#[derive(SubtypeCast, NewtypeCast, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[repr(transparent)]
pub(crate) struct ValueWrapper<T: Value> {
    pub(crate) value: T,
}

impl<T: Value> Node for ValueWrapper<T> {
    const NAME: &'static str = T::NAME;

    const VALUE_TYPE_FOR_VALUE_WRAPPER: Option<SealedWrapper<fn() -> ValueType>> =
        Some(SealedWrapper(ValueType::of::<T>));

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.value.input_var_iter()
    }

    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.value.unguarded_input_var_iter()
    }

    fn def_eq(&self, other: &Self) -> bool {
        self.value.def_eq(&other.value)
    }

    fn apply_var_map(&mut self, _var_map: impl FnMut(Var) -> Lit) {
        panic!("ValueWrapper used as Node");
    }

    fn reduce(&mut self, _builder: &mut impl NodeBuilder) -> bool {
        panic!("ValueWrapper used as Node");
    }
}

impl<T: Value> NodeDyn for ValueWrapper<T> {
    fn def_hash(&self) -> u64 {
        self.value.def_hash()
    }

    fn representative_input_var(&self) -> Var {
        self.value.representative_input_var()
    }

    fn output_var(&self) -> Option<Var> {
        None
    }

    fn max_var(&self) -> Var {
        self.value.max_var()
    }
}

/// Node type to assign a value to an output variable.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct ValueNode<T: Value> {
    /// The variable or literal holding the defined value.
    pub output: T::Output,
    /// The defined value of the output variable or literal.
    pub value: T,
}

impl<T: Value> Node for ValueNode<T> {
    const NAME: &'static str = T::NAME;

    fn input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.value.input_var_iter()
    }

    fn unguarded_input_var_iter(&self) -> impl Iterator<Item = Var> + '_ {
        self.value.unguarded_input_var_iter()
    }

    fn def_eq(&self, other: &Self) -> bool {
        self.value.def_eq(&other.value)
    }

    fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) {
        let mut new_output_lit = self
            .output
            .process_var_or_lit(|var| var.as_pos(), |lit| lit)
            .map_var_to_lit(&mut var_map);

        let value_pol = self.value.apply_var_map(var_map);

        new_output_lit ^= value_pol.into();

        let new_output = <T::Output>::build_var_or_lit(
            new_output_lit,
            |lit| {
                assert!(
                    lit.is_pos(),
                    "Value output of non-Boolean type mapped to negative literal"
                );
                lit.var()
            },
            |lit| lit,
        );

        self.output = new_output
    }

    fn reduce(&mut self, builder: &mut impl NodeBuilder) -> bool {
        self.value.reduce_node(self.output, builder)
    }
}

impl<T: Value> NodeDyn for ValueNode<T> {
    fn def_hash(&self) -> u64 {
        self.value.def_hash()
    }

    fn representative_input_var(&self) -> Var {
        self.value.representative_input_var()
    }

    fn output_var(&self) -> Option<Var> {
        Some(self.output.process_var_or_lit(|var| var, |lit| lit.var()))
    }

    fn max_var(&self) -> Var {
        self.output
            .process_var_or_lit(|var| var, |lit| lit.var())
            .max(self.value.max_var())
    }

    fn dyn_value(&self) -> Option<&DynValue> {
        Some(&self.value)
    }
}
