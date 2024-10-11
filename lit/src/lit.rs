use imctk_derive::{NewtypeCast, SubtypeCast};
use imctk_ids::{Id, Id32};
use std::ops;

use crate::pol::{Negate, NegateInPlace};

use super::{pol::Pol, var::Var};

/// Numeric identifier for a Boolean-like literal.
///
/// A literal consists of a [variable][`Var`] and a [polarity][`Pol`]. A literal with a positive
/// polarity has the value of its variable while a literal with a negative polarity is obtained by
/// applying the appropriate involution to the variable (usually some form of Boolean negation).
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
#[derive(Id, SubtypeCast, NewtypeCast)]
pub struct Lit(Id32);

unsafe impl bytemuck::NoUninit for Lit {}

/// Ensure that there is an even number of literals
#[allow(clippy::assertions_on_constants)]
const _: () = {
    assert!(Lit::MAX_ID_INDEX & 1 == 1);
};

impl std::fmt::Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
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

impl Default for Lit {
    #[inline(always)]
    fn default() -> Self {
        Self::MIN_ID
    }
}

impl From<Var> for Lit {
    #[inline(always)]
    fn from(var: Var) -> Self {
        var.as_lit()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct NegativePolarityError(pub Var);

impl TryFrom<Lit> for Var {
    type Error = NegativePolarityError;

    fn try_from(lit: Lit) -> Result<Self, Self::Error> {
        if lit.is_pos() {
            Ok(lit.var())
        } else {
            Err(NegativePolarityError(lit.var()))
        }
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

    /// This is equivalent to `f(self.var()) ^ self.pol()`.
    #[inline(always)]
    pub fn lookup<T: Negate>(self, f: impl FnOnce(Var) -> T) -> T::Negated {
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

impl ops::BitXor<Pol> for Lit {
    type Output = Self;

    #[inline(always)]
    fn bitxor(self, rhs: Pol) -> Self::Output {
        // SAFETY: Changing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() ^ (rhs as usize)) }
    }
}

impl ops::BitXorAssign<Pol> for Lit {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: Pol) {
        *self = *self ^ rhs;
    }
}

impl ops::BitXor<bool> for Lit {
    type Output = Self;

    #[inline(always)]
    fn bitxor(self, rhs: bool) -> Self::Output {
        // SAFETY: Changing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() ^ (rhs as usize)) }
    }
}

impl ops::BitXor<Pol> for &'_ Lit {
    type Output = Lit;

    #[inline(always)]
    fn bitxor(self, rhs: Pol) -> Self::Output {
        *self ^ rhs
    }
}

impl ops::BitXorAssign<bool> for Lit {
    #[inline(always)]
    fn bitxor_assign(&mut self, rhs: bool) {
        *self = *self ^ rhs;
    }
}

impl ops::Not for Lit {
    type Output = Self;

    #[inline(always)]
    fn not(self) -> Self::Output {
        // SAFETY: Changing the LSB of the code makes it stay in bounds
        unsafe { Lit::from_code_unchecked(self.code() ^ 1) }
    }
}

impl ops::Not for &'_ Lit {
    type Output = Lit;

    #[inline(always)]
    fn not(self) -> Self::Output {
        !*self
    }
}

impl Negate for Lit {
    type Negated = Lit;
}

impl NegateInPlace for Lit {
    #[inline(always)]
    fn negate_in_place(&mut self) {
        *self = !*self
    }
}

impl Negate for &'_ Lit {
    type Negated = Lit;
}

// FIXME optional dependency
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn polarities() {
        let v = Var::from_index(1);

        let l0 = v.as_lit();
        assert!(l0.is_pos());
        assert!(l0.as_pos().is_pos());
        assert!(l0.as_neg().is_neg());
        let l1 = v.as_neg_lit();
        assert!(l1.is_neg());
        assert!(l1.as_pos().is_pos());
        assert!(l1.as_neg().is_neg());
    }
}
