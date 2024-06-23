//! Workaround to emulate a restricted form of `const fn`s in traits using only stable rust.

use std::marker::PhantomData;

/// A type used to represent a constant value at the type level.
///
/// As this is using associated constants, it comes with fewer restrictions than const generics. In
/// particular it is possible to represent constants derived from other constants by arbitrary
/// const evaluable computations.
pub trait ConstValue {
    /// The type of the represented constant value.
    type Value;

    /// The represented constant value.
    const VALUE: Self::Value;
}

/// A type used to represent a single argument `const fn` at the type level.
///
/// The argument is represented as a trait parameter using the [`ConstValue`] trait.
pub trait ConstFn<Arg: ConstValue> {
    /// The type of the returned value.
    type Output;

    /// The returned value.
    const OUTPUT: Self::Output;
}

/// A [`ConstValue`] that represents the result of applying a [`ConstFn`] to a [`ConstValue`]
pub struct ApplyConstFn<Fn: ConstFn<Arg>, Arg: ConstValue>(PhantomData<(Fn, Arg)>);

impl<F: ConstFn<Arg>, Arg: ConstValue> ConstValue for ApplyConstFn<F, Arg> {
    type Value = <F as ConstFn<Arg>>::Output;

    const VALUE: Self::Value = <F as ConstFn<Arg>>::OUTPUT;
}

/// A [`ConstValue`] that represents a value given by a const generics `usize` parameter.
pub struct FromUsizeParam<const CONST: usize>;

impl<const CONST: usize> ConstValue for FromUsizeParam<CONST> {
    type Value = usize;
    const VALUE: Self::Value = CONST;
}

/// A [`ConstValue`] that represents the minimum of two `usize` const values.
pub(crate) struct MinConstUsize<A: ConstValue<Value = usize>, B: ConstValue<Value = usize>>(
    PhantomData<(A, B)>,
);

impl<A: ConstValue<Value = usize>, B: ConstValue<Value = usize>> ConstValue
    for MinConstUsize<A, B>
{
    type Value = usize;

    const VALUE: Self::Value = {
        if A::VALUE < B::VALUE {
            A::VALUE
        } else {
            B::VALUE
        }
    };
}

/// A [`ConstValue`] that represents the minimum of two `usize` const values.
pub(crate) struct MaxIdIndex<I:crate::Id>(
    PhantomData<I>,
);

impl<I: crate::Id> ConstValue for MaxIdIndex<I> {
    type Value = usize;

    const VALUE: Self::Value = I::MAX_INDEX;
}
