//! Numeric identifiers for variables and Boolean literals

use imctk_ids::{GenericId, Id, Id32};
use std::{fmt::Debug, hash::Hash, ops};

/// Numeric identifier for a Boolean-like literal.
///
/// A literal consists of a [variable][`Var`] and a [polarity][`Pol`]. A literal with a positive
/// polarity has the value of its variable while a literal with a negative polarity is obtained by
/// applying the appropriate involution to the variable (usual some form of Boolean negation).
///
/// The variable and polarity can be combined into a single number, called the
/// [`code`][`Self::code`]. The variable index can be obtained by shifting the code one bit to the
/// right with an even code corresponding to a positive and an odd code corresponding to a negative
/// polarity literal.
///
/// Note that the generic [`Id::id_index`] of a literal is the same as the [`code`][`Self::code`],
/// not the [`index`][`Self::index`]. The index of a literal is the same as the corresponding
/// variable's [`index`][`Var::index`].
#[repr(transparent)]
#[derive(Id)]
pub struct Lit(Id32);

/// Numeric identifier for a variable.
#[repr(transparent)]
#[derive(Id)]
pub struct Var(GenericId<{ Lit::MAX_ID_INDEX / 2 }, <Lit as Id>::BaseId>);

/// Ensure that there is an even number of literals
#[allow(clippy::assertions_on_constants)]
const _: () = {
    assert!(Lit::MAX_ID_INDEX & 1 == 1);
};

/// Either the identity function on, or negation of Booleans.
///
/// With only two possible values, this type is itself isomorphic to `bool`, but using `bool`
/// could be done by either representing the identity using `false` with `^` for application, or
/// by representing the identity using `true` with `==` for application. Having a separte type
/// to represent an invertible function on Booleans avoid having to make such an arbitrary
/// choice, and makes the resulting code easier to read and makes it harder to introduce parity
/// errors. In particular it should also prevent us from making different choices in different
/// parts of the code base.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Pol {
    /// Positive polarity, represents the identity function.
    Pos = 0,
    /// Negative polarity, represents Boolean negation.
    Neg = 1,
}

impl std::fmt::Debug for Pol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
    }
}

impl std::fmt::Display for Pol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pos => write!(f, "="),
            Self::Neg => write!(f, "!"),
        }
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var_index = self.index();
        if var_index == 0 {
            return write!(f, "0");
        }
        let rev_var_index = Self::MAX_ID_INDEX - var_index;
        if rev_var_index < var_index {
            write!(f, "r{}", rev_var_index)
        } else {
            write!(f, "v{}", var_index)
        }
    }
}

impl std::fmt::Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let var_index = self.index();
        if var_index == 0 {
            return match self.pol() {
                Pol::Pos => write!(f, "0"),
                Pol::Neg => write!(f, "1"),
            };
        }
        let rev_var_index = Self::MAX_ID_INDEX - var_index;
        let prefix = match self.pol() {
            Pol::Pos => "",
            Pol::Neg => "!",
        };
        if rev_var_index < var_index {
            write!(f, "{}r{}", prefix, rev_var_index)
        } else {
            write!(f, "{}v{}", prefix, var_index)
        }
    }
}

impl Default for Var {
    #[inline(always)]
    fn default() -> Self {
        Self::MIN_ID
    }
}

impl Default for Lit {
    #[inline(always)]
    fn default() -> Self {
        Self::MIN_ID
    }
}

impl Pol {
    /// Given a variable, returns the literal for that variable having this polarity.
    #[inline(always)]
    pub fn lit(self, var: Var) -> Lit {
        var.lit(self)
    }
}

impl ops::BitXor for Pol {
    type Output = Self;

    #[inline(always)]
    fn bitxor(self, rhs: Self) -> Self::Output {
        if (self == Pol::Neg) ^ (rhs == Pol::Neg) {
            Pol::Neg
        } else {
            Pol::Pos
        }
    }
}

impl ops::BitXorAssign for Pol {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: Self) {
        *self = *self ^ rhs
    }
}

impl Lit {
    /// The literal representing constant false/0/low.
    pub const FALSE: Self = Self::MIN_ID;
    /// The literal representing constant true/1/high.
    pub const TRUE: Self = <Self as imctk_ids::ConstIdFromIdIndex<1>>::ID;

    /// The largest valid [`code`][`Self::code`] for a literal.
    pub const MAX_CODE: usize = Self::MAX_ID_INDEX;

    /// Returns the literal for a given code.
    #[inline(always)]
    pub fn from_code(code: usize) -> Self {
        Self::from_id_index(code)
    }

    /// Returns the literal for a given code without bounds checking.
    /// # Safety
    /// The caller needs to ensure that `code <= Lit::MAX_CODE`.
    #[inline(always)]
    pub unsafe fn from_code_unchecked(code: usize) -> Self {
        // SAFETY: forwarding with equivalent documented safety requireemnts
        unsafe { Self::from_id_index_unchecked(code) }
    }

    /// Returns the variable of the literal.
    #[inline(always)]
    pub fn var(self) -> Var {
        // SAFETY: The valid index ranges of Lit and Var are chosen specifically to make this
        // work.
        unsafe { Var::from_index_unchecked(self.code() >> 1) }
    }

    /// Returns the polarity of the literal.
    #[inline(always)]
    pub fn pol(self) -> Pol {
        // SAFETY: We explicitly chose the repr for `Pol` to make this work
        unsafe { std::mem::transmute::<u8, Pol>(self.code() as u8 & 1) }
    }

    /// Returns the positive polarity literal with the same variable.
    #[inline(always)]
    pub fn as_pos(self) -> Self {
        // SAFETY: clearing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() & !1) }
    }

    /// Returns the negative polarity literal with the same variable.
    #[inline(always)]
    pub fn as_neg(self) -> Self {
        // SAFETY: setting the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() | 1) }
    }

    /// Returns whether this literal has positive polarity.
    #[inline(always)]
    pub fn is_pos(self) -> bool {
        self.pol() == Pol::Pos
    }

    /// Returns whether this literal has negative polarity.
    #[inline(always)]
    pub fn is_neg(self) -> bool {
        self.pol() == Pol::Neg
    }

    /// Applies a variable-to-litereal map to this literal's variable.
    ///
    /// This is equivalent to `f(self.var()) ^ self.pol()`.
    #[inline(always)]
    pub fn map_var_to_lit(self, f: impl FnOnce(Var) -> Lit) -> Lit {
        f(self.var()) ^ self.pol()
    }

    /// Returns the index of the literal's variable.
    #[inline(always)]
    pub fn index(self) -> usize {
        self.var().id_index()
    }

    /// Returns the literal's code.
    #[inline(always)]
    pub fn code(self) -> usize {
        self.id_index()
    }

    /// Returns whether this literal is [`Self::FALSE`] or [`Self::TRUE`].
    #[inline(always)]
    pub fn is_const(&self) -> bool {
        self.id_index() < 2
    }
}

impl ops::BitXor<Pol> for Var {
    type Output = Lit;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        self.lit(rhs)
    }
}

impl ops::BitXor<Pol> for Lit {
    type Output = Self;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        // SAFETY: Changing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() ^ (rhs as usize)) }
    }
}

impl ops::BitXorAssign<Pol> for Lit {
    fn bitxor_assign(&mut self, rhs: Pol) {
        *self = *self ^ rhs;
    }
}

impl ops::BitXor<bool> for Lit {
    type Output = Self;

    fn bitxor(self, rhs: bool) -> Self::Output {
        // SAFETY: Changing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() ^ (rhs as usize)) }
    }
}

impl ops::BitXorAssign<bool> for Lit {
    fn bitxor_assign(&mut self, rhs: bool) {
        *self = *self ^ rhs;
    }
}

impl ops::Not for Lit {
    type Output = Self;

    fn not(self) -> Self::Output {
        // SAFETY: Changing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() ^ 1) }
    }
}

impl Var {
    /// The variable representing constant false/0/low.
    pub const FALSE: Self = Self::MIN_ID;

    /// The largest valid [`index`][`Self::index`] for a variable.
    pub const MAX_INDEX: usize = Self::MAX_ID_INDEX;

    /// Returns the variable for a given index.
    #[inline(always)]
    pub fn from_index(index: usize) -> Self {
        Self::from_id_index(index)
    }

    /// Returns the variable for a given index without bounds checking.
    /// # Safety
    /// The caller needs to ensure that `index <= Var::MAX_INDEX`.
    #[inline(always)]
    pub unsafe fn from_index_unchecked(index: usize) -> Self {
        // SAFETY: forwarding with equivalent documented safety requireemnts
        unsafe { Self::from_id_index_unchecked(index) }
    }

    /// Returns the literal for this variable with the given polarity.
    #[inline(always)]
    pub fn lit(self, pol: Pol) -> Lit {
        // SAFETY: The valid index ranges of Lit and Var are chosen specifically to make this
        // work.
        unsafe { Lit::from_code_unchecked((self.index() << 1) | (pol as usize)) }
    }

    /// Returns a positive polarity literal for the variable.
    #[inline(always)]
    pub fn as_pos(self) -> Lit {
        // SAFETY: The valid index ranges of Lit and Var are chosen specifically to make this
        // work.
        unsafe { Lit::from_code_unchecked(self.index() << 1) }
    }

    /// Returns a negative polarity literal for the variable.
    #[inline(always)]
    pub fn as_neg(self) -> Lit {
        // SAFETY: The valid index ranges of Lit and Var are chosen specifically to make this
        // work.
        unsafe { Lit::from_code_unchecked((self.index() << 1) | 1) }
    }

    /// Returns the index of the variable.
    #[inline(always)]
    pub fn index(self) -> usize {
        self.id_index()
    }
}

impl flussab_aiger::Lit for Lit {
    const MAX_CODE: usize = <Self as Id>::MAX_ID_INDEX;

    #[inline(always)]
    fn from_code(code: usize) -> Self {
        Lit::from_code(code)
    }

    #[inline(always)]
    fn code(self) -> usize {
        Lit::code(self)
    }
}

mod seal_var_or_lit {
    pub trait Sealed {}

    impl Sealed for super::Var {}
    impl Sealed for super::Lit {}
}

/// Unified handling of variables and literals.
///
/// This trait is only implemented by [`Var`] and [`Lit`].
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
