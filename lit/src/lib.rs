//! Numeric identifiers for variables and Boolean literals
#![allow(missing_docs, dead_code)] // FIXME prototyping

pub mod lit;
pub mod pol;
pub mod var;

pub mod var_or_lit {
    // FIXME get rid of this trait

    use std::{fmt::Debug, hash::Hash, ops};

    use super::{lit::Lit, pol::Pol, var::Var};

    mod seal_var_or_lit {
        pub trait Sealed {}

        impl Sealed for super::Var {}
        impl Sealed for super::Lit {}
    }

    /// Unified handling of variables and literals.
    ///
    /// This trait is only implemented by [`Var`] and [`Lit`].
    #[deprecated]
    pub trait VarOrLit:
        Copy
        + Debug
        + Default
        + Eq
        + Ord
        + Hash
        + seal_var_or_lit::Sealed
        + ops::BitXor<Self::Pol, Output = Self>
        + ops::BitXorAssign<Self::Pol>
    {
        /// This is [`Pol`] for [`Lit`] and `()` for [`Var`].
        type Pol: Copy + Debug + Into<Pol>;

        /// Returns a variable or a literal by selecting among closures for constructing either.
        fn build_var_or_lit<T>(
            input: T,
            build_var: impl FnOnce(T) -> Var,
            build_lit: impl FnOnce(T) -> Lit,
        ) -> Self;

        /// Produces a value from a variable or a literal by selecting among closure consuming either.
        fn process_var_or_lit<T>(
            self,
            from_var: impl FnOnce(Var) -> T,
            from_lit: impl FnOnce(Lit) -> T,
        ) -> T;

        /// Produces a value from a default value for variables or from a polarity for literals.
        fn process_pol<T>(
            pol: Self::Pol,
            from_var_pol: impl FnOnce() -> T,
            from_lit_pol: impl FnOnce(Pol) -> T,
        ) -> T;
    }

    impl From<()> for Pol {
        fn from(_: ()) -> Self {
            Pol::Pos
        }
    }

    impl ops::BitXor<()> for Var {
        type Output = Self;

        #[inline(always)]
        fn bitxor(self, _: ()) -> Self::Output {
            self
        }
    }

    impl ops::BitXorAssign<()> for Var {
        #[inline(always)]
        fn bitxor_assign(&mut self, _: ()) {}
    }

    impl VarOrLit for Var {
        type Pol = ();

        fn build_var_or_lit<T>(
            input: T,
            build_var: impl FnOnce(T) -> Var,
            _build_lit: impl FnOnce(T) -> Lit,
        ) -> Self {
            build_var(input)
        }

        fn process_var_or_lit<T>(
            self,
            from_var: impl FnOnce(Var) -> T,
            _from_lit: impl FnOnce(Lit) -> T,
        ) -> T {
            from_var(self)
        }

        fn process_pol<T>(
            (): Self::Pol,
            from_var_pol: impl FnOnce() -> T,
            _from_lit_pol: impl FnOnce(Pol) -> T,
        ) -> T {
            from_var_pol()
        }
    }

    impl VarOrLit for Lit {
        type Pol = Pol;
        fn build_var_or_lit<T>(
            input: T,
            _build_var: impl FnOnce(T) -> Var,
            build_lit: impl FnOnce(T) -> Lit,
        ) -> Self {
            build_lit(input)
        }

        fn process_var_or_lit<T>(
            self,
            _from_var: impl FnOnce(Var) -> T,
            from_lit: impl FnOnce(Lit) -> T,
        ) -> T {
            from_lit(self)
        }

        fn process_pol<T>(
            pol: Self::Pol,
            _from_var_pol: impl FnOnce() -> T,
            from_lit_pol: impl FnOnce(Pol) -> T,
        ) -> T {
            from_lit_pol(pol)
        }
    }
}
