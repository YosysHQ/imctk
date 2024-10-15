use imctk_derive::{NewtypeCast, SubtypeCast};
use imctk_ids::{GenericId, Id};
use std::ops;

use super::{lit::Lit, pol::Pol};

/// Numeric identifier for a variable.
#[repr(transparent)]
#[derive(Id, SubtypeCast, NewtypeCast)]
pub struct Var(GenericId<{ Lit::MAX_ID_INDEX / 2 }, <Lit as Id>::BaseId>);

impl std::fmt::Debug for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Display::fmt(self, f)
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

impl Default for Var {
    #[inline(always)]
    fn default() -> Self {
        Self::MIN_ID
    }
}

impl ops::BitXor<Pol> for Var {
    type Output = Lit;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        self.lit(rhs)
    }
}

impl ops::BitXor<Pol> for &'_ Var {
    type Output = Lit;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        *self ^ rhs
    }
}

impl ops::BitXor<bool> for Var {
    type Output = Lit;

    fn bitxor(self, rhs: bool) -> Self::Output {
        self.as_lit() ^ rhs
    }
}

impl ops::BitXor<bool> for &'_ Var {
    type Output = Lit;

    fn bitxor(self, rhs: bool) -> Self::Output {
        *self ^ rhs
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
    pub fn as_lit(self) -> Lit {
        // SAFETY: The valid index ranges of Lit and Var are chosen specifically to make this
        // work.
        unsafe { Lit::from_code_unchecked(self.index() << 1) }
    }

    /// Returns a negative polarity literal for the variable.
    #[inline(always)]
    pub fn as_neg_lit(self) -> Lit {
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
