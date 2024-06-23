use crate::id::{GenericId, Id};

use super::ConstIdFromIdIndex;

impl<const INDEX: usize> ConstIdFromIdIndex<INDEX> for usize {
    type Id = Self;
    const ID: Self = INDEX;
}

impl<const INDEX: usize> ConstIdFromIdIndex<INDEX> for u64 {
    type Id = Self;
    #[allow(clippy::absurd_extreme_comparisons)]
    const ID: Self = {
        assert!(INDEX <= Self::MAX_ID_INDEX);
        INDEX as Self
    };
}

impl<const INDEX: usize> ConstIdFromIdIndex<INDEX> for u32 {
    type Id = Self;
    #[allow(clippy::absurd_extreme_comparisons)]
    const ID: Self = {
        assert!(INDEX <= Self::MAX_ID_INDEX);
        INDEX as Self
    };
}

impl<const INDEX: usize> ConstIdFromIdIndex<INDEX> for u16 {
    type Id = Self;
    #[allow(clippy::absurd_extreme_comparisons)]
    const ID: Self = {
        assert!(INDEX <= Self::MAX_ID_INDEX);
        INDEX as Self
    };
}

impl<const INDEX: usize> ConstIdFromIdIndex<INDEX> for u8 {
    type Id = Self;
    #[allow(clippy::absurd_extreme_comparisons)]
    const ID: Self = {
        assert!(INDEX <= Self::MAX_ID_INDEX);
        INDEX as Self
    };
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for usize {
    type BaseId = Self;
    type GenericId = GenericId<{ Self::MAX_ID_INDEX }>;
    type FromConstIdIndex<const INDEX: usize> = Self;

    #[inline(always)]
    fn id_index(self) -> usize {
        self
    }

    const MAX_ID_INDEX: usize = Self::MAX;

    const MIN_ID: Self = 0;

    const MAX_ID: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_id_index_unchecked(index: usize) -> Self {
        index
    }

    #[inline(always)]
    fn from_id_index(index: usize) -> Self {
        index
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u64 {
    type BaseId = Self;
    type GenericId = GenericId<{ Self::MAX_ID_INDEX }>;
    type FromConstIdIndex<const INDEX: usize> = Self;

    #[inline(always)]
    fn id_index(self) -> usize {
        self as usize
    }

    const MAX_ID_INDEX: usize = if (u64::MAX as usize as u64) == u64::MAX {
        u64::MAX as usize
    } else {
        usize::MAX
    };

    const MIN_ID: Self = 0;

    const MAX_ID: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_id_index_unchecked(index: usize) -> Self {
        index as Self
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u32 {
    type BaseId = Self;
    type GenericId = GenericId<{ Self::MAX_ID_INDEX }>;
    type FromConstIdIndex<const INDEX: usize> = Self;

    #[inline(always)]
    fn id_index(self) -> usize {
        self as usize
    }

    const MAX_ID_INDEX: usize = if (u32::MAX as usize as u32) == u32::MAX {
        u32::MAX as usize
    } else {
        usize::MAX
    };

    const MIN_ID: Self = 0;

    const MAX_ID: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_id_index_unchecked(index: usize) -> Self {
        index as Self
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u16 {
    type BaseId = Self;
    type GenericId = GenericId<{ Self::MAX_ID_INDEX }>;
    type FromConstIdIndex<const INDEX: usize> = Self;

    #[inline(always)]
    fn id_index(self) -> usize {
        self as usize
    }

    const MAX_ID_INDEX: usize = u16::MAX as usize;

    const MIN_ID: Self = 0;

    const MAX_ID: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_id_index_unchecked(index: usize) -> Self {
        index as Self
    }
}

// SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
// requirements.
unsafe impl Id for u8 {
    type BaseId = Self;
    type GenericId = GenericId<{ Self::MAX_ID_INDEX }>;
    type FromConstIdIndex<const INDEX: usize> = Self;

    #[inline(always)]
    fn id_index(self) -> usize {
        self as usize
    }

    const MAX_ID_INDEX: usize = u8::MAX as usize;

    const MIN_ID: Self = 0;

    const MAX_ID: Self = Self::MAX;

    #[inline(always)]
    unsafe fn from_id_index_unchecked(index: usize) -> Self {
        index as Self
    }
}
