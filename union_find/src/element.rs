//! A trait for "elements" that can be split into an "atom" and a "polarity".

use imctk_lit::{Lit, Var};

/// A trait for "elements" that can be split into an "atom" and a "polarity".
///
/// This lets code generically manipulate variables and literals, and further serves to abstract over their concrete representation.
///
/// The two most common case this trait is used for are:
/// 1) The element and the atom are both `Var`. In this case there is only one polarity and the trait implementation is trivial.
/// 2) The element is `Lit` and the atom is `Var`. Here there are two polarities (`+` and `-`) to keep track of.
///
/// Mathematically, implementing this trait signifies that elements can be written as pairs `(a, p)` with an atom `a` and a polarity `p`.
/// The polarities are assumed to form a group `(P, *, 1)`. The trait operations then correspond to:
/// 1) `from_atom(a) = (a, 1)`
/// 2) `atom((a, p)) = a`
/// 3) `apply_pol_of((a, p), (b, q)) = (a, p * q)`
///
/// Currently, code assumes that `P` is either trivial or isomorphic to `Z_2`.
///
/// Code using this trait may assume the following axioms to hold:
/// 1) `from_atom(atom(x)) == x`
/// 2) `apply_pol_of(atom(x), x) == x`
/// 3) `apply_pol_of(apply_pol_of(x, y), y) == x`
// TODO: add missing axioms
pub trait Element {
    /// Atom type for this element type.
    type Atom;
    /// Constructs an element from an atom by applying positive polarity.
    fn from_atom(atom: Self::Atom) -> Self;
    /// Returns the atom corresponding to an element.
    fn atom(self) -> Self::Atom;
    /// Multiplies `self` by the polarity of `other`, i.e. conceptually `apply_pol_of(self, other) = self ^ pol(other)`.
    fn apply_pol_of(self, other: Self) -> Self;
}

impl Element for u32 {
    type Atom = u32;
    fn from_atom(atom: u32) -> Self {
        atom
    }
    fn atom(self) -> u32 {
        self
    }
    fn apply_pol_of(self, _other: u32) -> Self {
        self
    }
}

impl Element for Var {
    type Atom = Var;
    fn from_atom(atom: Var) -> Self {
        atom
    }
    fn atom(self) -> Var {
        self
    }
    fn apply_pol_of(self, _other: Var) -> Self {
        self
    }
}

impl Element for Lit {
    type Atom = Var;
    fn from_atom(atom: Var) -> Self {
        atom.as_lit()
    }
    fn atom(self) -> Var {
        self.var()
    }
    fn apply_pol_of(self, other: Self) -> Self {
        self ^ other.pol()
    }
}
