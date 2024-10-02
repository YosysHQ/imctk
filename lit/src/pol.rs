use std::ops;

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

impl std::fmt::Display for Pol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pos => write!(f, "="),
            Self::Neg => write!(f, "!"),
        }
    }
}

impl Pol {
    /// Returns the negative polarity if the given condition is `true` and a positive polarity
    /// otherwise.
    #[inline(always)]
    pub fn neg_if(neg: bool) -> Self {
        // SAFETY: We explicitly chose the repr for `Pol` to make this work
        unsafe { std::mem::transmute::<bool, Pol>(neg) }
    }

    /// Returns the positive polarity if the given condition is `true` and a positive polarity
    /// otherwise.
    #[inline(always)]
    pub fn pos_if(pos: bool) -> Self {
        Self::neg_if(!pos)
    }

    /// Returns `true` when this is the negative polarity.
    #[inline(always)]
    pub fn is_neg(self) -> bool {
        // SAFETY: We explicitly chose the repr for `Pol` to make this work
        unsafe { std::mem::transmute::<Pol, bool>(self) }
    }

    /// Returns `true` when this is the positive polarity.
    #[inline(always)]
    pub fn is_pos(self) -> bool {
        !self.is_neg()
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

impl ops::BitXor<Pol> for bool {
    type Output = bool;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        self ^ (rhs == Pol::Neg)
    }
}

impl ops::BitXor<Pol> for &'_ bool {
    type Output = bool;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        *self ^ rhs
    }
}

impl ops::BitXorAssign<Pol> for bool {
    fn bitxor_assign(&mut self, rhs: Pol) {
        *self ^= rhs == Pol::Neg
    }
}

impl ops::BitXor<Pol> for u64 {
    type Output = u64;

    fn bitxor(self, rhs: Pol) -> Self::Output {
        self ^ match rhs {
            Pol::Pos => 0,
            Pol::Neg => !0,
        }
    }
}

impl ops::Not for Pol {
    type Output = Self;

    fn not(self) -> Self::Output {
        self ^ Pol::Neg
    }
}
