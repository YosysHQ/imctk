//! Incremental Model Checking Toolkit
#![deny(unsafe_op_in_unsafe_fn)]
#![warn(clippy::undocumented_unsafe_blocks)]
#![warn(missing_docs)]

pub mod ir;

pub mod unordered_pair {
    //! Unordered pairs of [`Ord`] elements.
    use std::ops;

    use crate::ir::var::{Lit, Pol, Var};

    /// An unordered pair represented as a sorted `[T; 2]`.
    ///
    /// Note that this does allow a pair containing the same value twice.
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #[repr(transparent)]
    pub struct UnorderedPair<T> {
        values: [T; 2],
    }

    impl<T> ops::Deref for UnorderedPair<T> {
        type Target = [T; 2];

        fn deref(&self) -> &Self::Target {
            &self.values
        }
    }

    impl<T: Ord> From<[T; 2]> for UnorderedPair<T> {
        fn from(values: [T; 2]) -> Self {
            Self::new(values)
        }
    }

    impl<T> From<UnorderedPair<T>> for [T; 2] {
        fn from(pair: UnorderedPair<T>) -> Self {
            pair.values
        }
    }

    impl<T: std::fmt::Debug> std::fmt::Debug for UnorderedPair<T> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_set().entries(&self.values).finish()
        }
    }

    impl<T: Ord> UnorderedPair<T> {
        /// Creates a new unordered pair by sorting two values.
        pub fn new(values: [T; 2]) -> Self {
            let [a, b] = values;

            if a <= b {
                Self { values: [a, b] }
            } else {
                Self { values: [b, a] }
            }
        }

        /// Returns a reference to the smaller of the two elements.
        pub fn min_element(&self) -> &T {
            &self.values[0]
        }

        /// Returns a reference to the larger of the two elements.
        pub fn max_element(&self) -> &T {
            &self.values[1]
        }
    }

    impl UnorderedPair<Lit> {
        /// Applies a variable-to-literal map to an unordered pair of literals.
        // TODO make this fully public?
        pub(crate) fn apply_var_map(&mut self, mut var_map: impl FnMut(Var) -> Lit) {
            *self = Self::new(self.values.map(|lit| lit.map_var_to_lit(&mut var_map)))
        }
    }

    impl UnorderedPair<Var> {
        /// Applies a variable-to-literal map to an unordered pair of variables, returning the
        /// composition of the two polarities discarded in the process.
        // TODO make this fully public?
        pub(crate) fn apply_var_map_compose_pol(
            &mut self,
            mut var_map: impl FnMut(Var) -> Lit,
        ) -> Pol {
            let mut pol = Pol::Pos;
            *self = Self::new(self.values.map(|var| {
                let lit = var_map(var);
                pol ^= lit.pol();
                lit.var()
            }));
            pol
        }
    }
}
