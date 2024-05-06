use crate::{GenericId, Id};

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for usize {
    type Generic = GenericId<{ Self::MAX_INDEX }>;
    type Base = Self;

    #[inline(always)]
    fn index(self) -> usize {
        self
    }

    const MAX_INDEX: usize = Self::MAX;

    const MIN: Self = 0;

    const MAX: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_index_unchecked(index: usize) -> Self {
        index
    }

    #[inline(always)]
    fn from_index(index: usize) -> Self {
        index
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u64 {
    type Generic = GenericId<{ Self::MAX_INDEX }>;
    type Base = Self;

    #[inline(always)]
    fn index(self) -> usize {
        self as usize
    }

    const MAX_INDEX: usize = if (u64::MAX as usize as u64) == u64::MAX {
        u64::MAX as usize
    } else {
        usize::MAX
    };

    const MIN: Self = 0;

    const MAX: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_index_unchecked(index: usize) -> Self {
        index as Self
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u32 {
    type Generic = GenericId<{ Self::MAX_INDEX }>;
    type Base = Self;

    #[inline(always)]
    fn index(self) -> usize {
        self as usize
    }

    const MAX_INDEX: usize = if (u32::MAX as usize as u32) == u32::MAX {
        u32::MAX as usize
    } else {
        usize::MAX
    };

    const MIN: Self = 0;

    const MAX: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_index_unchecked(index: usize) -> Self {
        index as Self
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u16 {
    type Generic = GenericId<{ Self::MAX_INDEX }>;
    type Base = Self;

    #[inline(always)]
    fn index(self) -> usize {
        self as usize
    }

    const MAX_INDEX: usize = u16::MAX as usize;

    const MIN: Self = 0;

    const MAX: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_index_unchecked(index: usize) -> Self {
        index as Self
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u8 {
    type Generic = GenericId<{ Self::MAX_INDEX }>;
    type Base = Self;

    #[inline(always)]
    fn index(self) -> usize {
        self as usize
    }

    const MAX_INDEX: usize = u8::MAX as usize;

    const MIN: Self = 0;

    const MAX: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_index_unchecked(index: usize) -> Self {
        index as Self
    }
}
