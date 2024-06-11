mod id8 {
    use imctk_transparent::SubtypeCast;

    use crate::id::{u8_range_types::NonMaxHighNibbleU8, GenericId, Id};
    use core::{fmt, fmt::Debug, hash::Hash};

    /// [`Id`] type representing indices in the range `0..0xf0`.
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(transparent)]
    pub struct Id8(NonMaxHighNibbleU8);

    // SAFETY: NonMaxHighNibbleU8 is `#[repr(u8)]`
    unsafe impl SubtypeCast for Id8 {
        type Repr = u8;
    }

    impl Id8 {
        #[inline(always)]
        const fn as_u8(self) -> u8 {
            // SAFETY: transmuting fully initialized data to u8 is always safe
            unsafe { std::mem::transmute::<Self, u8>(self) }
        }

        #[inline(always)]
        const unsafe fn from_u8_unchecked(index: u8) -> Self {
            debug_assert!(index as usize <= Self::MAX_INDEX);
            // SAFETY: delegated to caller
            unsafe { std::mem::transmute::<u8, Self>(index) }
        }

        /// Returns the id with a given index, panicking when the index is invalid.
        ///
        /// Unlike the [`Id::from_index`] this is a `const fn`.
        ///
        /// This panics if and only if `index > Self::MAX_INDEX`.
        #[inline]
        pub const fn from_index_const(index: usize) -> Self {
            assert!(index <= Self::MAX_INDEX);
            // SAFETY: preceding assert checks the precondition
            unsafe { Self::from_u8_unchecked(index as u8) }
        }
    }

    impl PartialEq for Id8 {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self.as_u8() == other.as_u8()
        }
    }

    impl Eq for Id8 {}

    impl PartialOrd for Id8 {
        #[inline(always)]
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }

        #[inline(always)]
        fn lt(&self, other: &Self) -> bool {
            self.as_u8() < other.as_u8()
        }

        #[inline(always)]
        fn le(&self, other: &Self) -> bool {
            self.as_u8() <= other.as_u8()
        }

        #[inline(always)]
        fn gt(&self, other: &Self) -> bool {
            self.as_u8() > other.as_u8()
        }

        #[inline(always)]
        fn ge(&self, other: &Self) -> bool {
            self.as_u8() >= other.as_u8()
        }
    }

    impl Ord for Id8 {
        #[inline(always)]
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.as_u8().cmp(&other.as_u8())
        }

        #[inline(always)]
        fn max(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u8_unchecked(self.as_u8().max(other.as_u8())) }
        }

        #[inline(always)]
        fn min(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u8_unchecked(self.as_u8().min(other.as_u8())) }
        }

        #[inline(always)]
        fn clamp(self, min: Self, max: Self) -> Self
        where
            Self: Sized,
            Self: PartialOrd,
        {
            // SAFETY: returns one of the three values, all known to be a valid index
            unsafe { Self::from_u8_unchecked(self.as_u8().clamp(min.as_u8(), max.as_u8())) }
        }
    }

    impl Debug for Id8 {
        #[inline(always)]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Debug::fmt(&self.as_u8(), f)
        }
    }

    impl Hash for Id8 {
        #[inline(always)]
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            state.write_u8(self.as_u8())
        }

        #[inline(always)]
        fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
        where
            Self: Sized,
        {
            // SAFETY: reading fully initialized data as &[u8] is always safe
            let bytes = unsafe { data.align_to::<u8>().1 };
            state.write(bytes);
        }
    }

    // SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
    // requirements.
    unsafe impl Id for Id8 {
        type Generic = GenericId<{ Self::MAX_INDEX }>;

        #[inline(always)]
        fn index(self) -> usize {
            self.as_u8() as usize
        }

        const MAX_INDEX: usize = 0xef;

        const MIN: Self = {
            // SAFETY: zero is always <= MAX_INDEX
            unsafe { Self::from_u8_unchecked(0) }
        };

        const MAX: Self = {
            // SAFETY: MAX_INDEX is always <= MAX_INDEX
            unsafe { Self::from_u8_unchecked(Self::MAX_INDEX as u8) }
        };

        #[inline(always)]
        unsafe fn from_index_unchecked(index: usize) -> Self {
            // SAFETY: we require index to be <= MAX_INDEX which still holds after the primitive cast
            unsafe { Self::from_u8_unchecked(index as u8) }
        }

        type Base = Self;
    }
}

mod id16 {
    use imctk_transparent::SubtypeCast;

    use crate::id::{u8_range_types::NonMaxU8, GenericId, Id};
    use core::{fmt, fmt::Debug, hash::Hash};

    /// [`Id`] type representing indices in the range `0..0xff00`.
    #[cfg(target_endian = "little")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(C, align(2))]
    pub struct Id16 {
        lsb: u8,
        msb: NonMaxU8,
    }

    #[cfg(target_endian = "big")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(C, align(2))]
    pub struct Id16 {
        msb: NonMaxU8,
        lsb: u8,
    }

    // SAFETY: By using `#[repr(C, align(2))]` we guarantee a compatible representation
    unsafe impl SubtypeCast for Id16 {
        type Repr = u16;
    }

    impl Id16 {
        #[inline(always)]
        const fn as_u16(self) -> u16 {
            // SAFETY: transmuting fully initialized data to u16 is always safe
            unsafe { std::mem::transmute::<Self, u16>(self) }
        }

        #[inline(always)]
        const unsafe fn from_u16_unchecked(index: u16) -> Self {
            debug_assert!(index as usize <= Self::MAX_INDEX);
            // SAFETY: delegated to caller
            unsafe { std::mem::transmute::<u16, Self>(index) }
        }

        /// Returns the id with a given index, panicking when the index is invalid.
        ///
        /// Unlike the [`Id::from_index`] this is a `const fn`.
        ///
        /// This panics if and only if `index > Self::MAX_INDEX`.
        #[inline]
        pub const fn from_index_const(index: usize) -> Self {
            assert!(index <= Self::MAX_INDEX);
            // SAFETY: preceding assert checks the precondition
            unsafe { Self::from_u16_unchecked(index as u16) }
        }
    }

    impl PartialEq for Id16 {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self.as_u16() == other.as_u16()
        }
    }

    impl Eq for Id16 {}

    impl PartialOrd for Id16 {
        #[inline(always)]
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }

        #[inline(always)]
        fn lt(&self, other: &Self) -> bool {
            self.as_u16() < other.as_u16()
        }

        #[inline(always)]
        fn le(&self, other: &Self) -> bool {
            self.as_u16() <= other.as_u16()
        }

        #[inline(always)]
        fn gt(&self, other: &Self) -> bool {
            self.as_u16() > other.as_u16()
        }

        #[inline(always)]
        fn ge(&self, other: &Self) -> bool {
            self.as_u16() >= other.as_u16()
        }
    }

    impl Ord for Id16 {
        #[inline(always)]
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.as_u16().cmp(&other.as_u16())
        }

        #[inline(always)]
        fn max(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u16_unchecked(self.as_u16().max(other.as_u16())) }
        }

        #[inline(always)]
        fn min(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u16_unchecked(self.as_u16().min(other.as_u16())) }
        }

        #[inline(always)]
        fn clamp(self, min: Self, max: Self) -> Self
        where
            Self: Sized,
            Self: PartialOrd,
        {
            // SAFETY: returns one of the three values, all known to be a valid index
            unsafe { Self::from_u16_unchecked(self.as_u16().clamp(min.as_u16(), max.as_u16())) }
        }
    }

    impl Debug for Id16 {
        #[inline(always)]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Debug::fmt(&self.as_u16(), f)
        }
    }

    impl Hash for Id16 {
        #[inline(always)]
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            state.write_u16(self.as_u16())
        }

        #[inline(always)]
        fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
        where
            Self: Sized,
        {
            // SAFETY: reading fully initialized data as &[u8] is always safe
            let bytes = unsafe { data.align_to::<u8>().1 };
            state.write(bytes);
        }
    }

    // SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
    // requirements.
    unsafe impl Id for Id16 {
        type Generic = GenericId<{ Self::MAX_INDEX }>;

        #[inline(always)]
        fn index(self) -> usize {
            self.as_u16() as usize
        }

        const MAX_INDEX: usize = 0xfeff;

        const MIN: Self = {
            // SAFETY: zero is always <= MAX_INDEX
            unsafe { Self::from_u16_unchecked(0) }
        };

        const MAX: Self = {
            // SAFETY: MAX_INDEX is always <= MAX_INDEX
            unsafe { Self::from_u16_unchecked(Self::MAX_INDEX as u16) }
        };

        #[inline(always)]
        unsafe fn from_index_unchecked(index: usize) -> Self {
            // SAFETY: we require index to be <= MAX_INDEX which still holds after the primitive cast
            unsafe { Self::from_u16_unchecked(index as u16) }
        }

        type Base = Self;
    }
}

mod id32 {
    use imctk_transparent::SubtypeCast;

    use crate::id::{u8_range_types::NonMaxU8, GenericId, Id};
    use core::{fmt, fmt::Debug, hash::Hash};

    /// [`Id`] type representing indices in the range `0..0xff00_0000`.
    #[cfg(target_endian = "little")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(C, align(4))]
    pub struct Id32 {
        lsbs: [u8; 3],
        msb: NonMaxU8,
    }

    #[cfg(target_endian = "big")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(C, align(4))]
    pub struct Id32 {
        msb: NonMaxU8,
        lsbs: [u8; 3],
    }

    // SAFETY: By using `#[repr(C, align(4))]` we guarantee a compatible representation
    unsafe impl SubtypeCast for Id32 {
        type Repr = u32;
    }

    impl Id32 {
        #[inline(always)]
        const fn as_u32(self) -> u32 {
            // SAFETY: transmuting fully initialized data to u32 is always safe
            unsafe { std::mem::transmute::<Self, u32>(self) }
        }

        #[inline(always)]
        const unsafe fn from_u32_unchecked(index: u32) -> Self {
            debug_assert!(index as usize <= Self::MAX_INDEX);
            // SAFETY: delegated to caller
            unsafe { std::mem::transmute::<u32, Self>(index) }
        }

        /// Returns the id with a given index, panicking when the index is invalid.
        ///
        /// Unlike the [`Id::from_index`] this is a `const fn`.
        ///
        /// This panics if and only if `index > Self::MAX_INDEX`.
        #[inline]
        pub const fn from_index_const(index: usize) -> Self {
            assert!(index <= Self::MAX_INDEX);
            // SAFETY: preceding assert checks the precondition
            unsafe { Self::from_u32_unchecked(index as u32) }
        }
    }

    impl PartialEq for Id32 {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self.as_u32() == other.as_u32()
        }
    }

    impl Eq for Id32 {}

    impl PartialOrd for Id32 {
        #[inline(always)]
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }

        #[inline(always)]
        fn lt(&self, other: &Self) -> bool {
            self.as_u32() < other.as_u32()
        }

        #[inline(always)]
        fn le(&self, other: &Self) -> bool {
            self.as_u32() <= other.as_u32()
        }

        #[inline(always)]
        fn gt(&self, other: &Self) -> bool {
            self.as_u32() > other.as_u32()
        }

        #[inline(always)]
        fn ge(&self, other: &Self) -> bool {
            self.as_u32() >= other.as_u32()
        }
    }

    impl Ord for Id32 {
        #[inline(always)]
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.as_u32().cmp(&other.as_u32())
        }

        #[inline(always)]
        fn max(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u32_unchecked(self.as_u32().max(other.as_u32())) }
        }

        #[inline(always)]
        fn min(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u32_unchecked(self.as_u32().min(other.as_u32())) }
        }

        #[inline(always)]
        fn clamp(self, min: Self, max: Self) -> Self
        where
            Self: Sized,
            Self: PartialOrd,
        {
            // SAFETY: returns one of the three values, all known to be a valid index
            unsafe { Self::from_u32_unchecked(self.as_u32().clamp(min.as_u32(), max.as_u32())) }
        }
    }

    impl Debug for Id32 {
        #[inline(always)]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Debug::fmt(&self.as_u32(), f)
        }
    }

    impl Hash for Id32 {
        #[inline(always)]
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            state.write_u32(self.as_u32())
        }

        #[inline(always)]
        fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
        where
            Self: Sized,
        {
            // SAFETY: reading fully initialized data as &[u8] is always safe
            let bytes = unsafe { data.align_to::<u8>().1 };
            state.write(bytes);
        }
    }

    // SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
    // requirements.
    unsafe impl Id for Id32 {
        type Generic = GenericId<{ Self::MAX_INDEX }>;

        #[inline(always)]
        fn index(self) -> usize {
            self.as_u32() as usize
        }

        const MAX_INDEX: usize = {
            let max_index = 0xfeff_ffffu32;
            if (max_index as usize as u32) == max_index {
                max_index as usize
            } else {
                usize::MAX
            }
        };

        const MIN: Self = {
            // SAFETY: zero is always <= MAX_INDEX
            unsafe { Self::from_u32_unchecked(0) }
        };

        const MAX: Self = {
            // SAFETY: MAX_INDEX is always <= MAX_INDEX
            unsafe { Self::from_u32_unchecked(Self::MAX_INDEX as u32) }
        };

        #[inline(always)]
        unsafe fn from_index_unchecked(index: usize) -> Self {
            // SAFETY: we require index to be <= MAX_INDEX which still holds after the primitive cast
            unsafe { Self::from_u32_unchecked(index as u32) }
        }

        type Base = Self;
    }
}

mod id64 {
    use imctk_transparent::SubtypeCast;

    use crate::id::{u8_range_types::NonMaxU8, GenericId, Id};
    use core::{fmt, fmt::Debug, hash::Hash};

    /// [`Id`] type representing indices in the range `0..0xff00_0000_0000_0000`.
    #[cfg(target_endian = "little")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(C, align(8))]
    pub struct Id64 {
        lsbs: [u8; 7],
        msb: NonMaxU8,
    }

    #[cfg(target_endian = "big")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[repr(C, align(8))]
    pub struct Id64 {
        msb: NonMaxU8,
        lsbs: [u8; 7],
    }

    // SAFETY: By using `#[repr(C, align(8))]` we guarantee a compatible representation
    unsafe impl SubtypeCast for Id64 {
        type Repr = u64;
    }

    impl Id64 {
        #[inline(always)]
        const fn as_u64(self) -> u64 {
            // SAFETY: transmuting fully initialized data to u64 is always safe
            unsafe { std::mem::transmute::<Self, u64>(self) }
        }

        #[inline(always)]
        const unsafe fn from_u64_unchecked(index: u64) -> Self {
            debug_assert!(index as usize <= Self::MAX_INDEX);
            // SAFETY: delegated to caller
            unsafe { std::mem::transmute::<u64, Self>(index) }
        }

        /// Returns the id with a given index, panicking when the index is invalid.
        ///
        /// Unlike the [`Id::from_index`] this is a `const fn`.
        ///
        /// This panics if and only if `index > Self::MAX_INDEX`.
        #[inline]
        pub const fn from_index_const(index: usize) -> Self {
            assert!(index <= Self::MAX_INDEX);
            // SAFETY: preceding assert checks the precondition
            unsafe { Self::from_u64_unchecked(index as u64) }
        }
    }

    impl PartialEq for Id64 {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self.as_u64() == other.as_u64()
        }
    }

    impl Eq for Id64 {}

    impl PartialOrd for Id64 {
        #[inline(always)]
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }

        #[inline(always)]
        fn lt(&self, other: &Self) -> bool {
            self.as_u64() < other.as_u64()
        }

        #[inline(always)]
        fn le(&self, other: &Self) -> bool {
            self.as_u64() <= other.as_u64()
        }

        #[inline(always)]
        fn gt(&self, other: &Self) -> bool {
            self.as_u64() > other.as_u64()
        }

        #[inline(always)]
        fn ge(&self, other: &Self) -> bool {
            self.as_u64() >= other.as_u64()
        }
    }

    impl Ord for Id64 {
        #[inline(always)]
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.as_u64().cmp(&other.as_u64())
        }

        #[inline(always)]
        fn max(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u64_unchecked(self.as_u64().max(other.as_u64())) }
        }

        #[inline(always)]
        fn min(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_u64_unchecked(self.as_u64().min(other.as_u64())) }
        }

        #[inline(always)]
        fn clamp(self, min: Self, max: Self) -> Self
        where
            Self: Sized,
            Self: PartialOrd,
        {
            // SAFETY: returns one of the three values, all known to be a valid index
            unsafe { Self::from_u64_unchecked(self.as_u64().clamp(min.as_u64(), max.as_u64())) }
        }
    }

    impl Debug for Id64 {
        #[inline(always)]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Debug::fmt(&self.as_u64(), f)
        }
    }

    impl Hash for Id64 {
        #[inline(always)]
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            state.write_u64(self.as_u64())
        }

        #[inline(always)]
        fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
        where
            Self: Sized,
        {
            // SAFETY: reading fully initialized data as &[u8] is always safe
            let bytes = unsafe { data.align_to::<u8>().1 };
            state.write(bytes);
        }
    }

    // SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
    // requirements.
    unsafe impl Id for Id64 {
        type Generic = GenericId<{ Self::MAX_INDEX }>;

        #[inline(always)]
        fn index(self) -> usize {
            self.as_u64() as usize
        }

        const MAX_INDEX: usize = {
            let max_index = 0xfeff_ffff_ffff_ffffu64;
            if (max_index as usize as u64) == max_index {
                max_index as usize
            } else {
                usize::MAX
            }
        };

        const MIN: Self = {
            // SAFETY: zero is always <= MAX_INDEX
            unsafe { Self::from_u64_unchecked(0) }
        };

        const MAX: Self = {
            // SAFETY: MAX_INDEX is always <= MAX_INDEX
            unsafe { Self::from_u64_unchecked(Self::MAX_INDEX as u64) }
        };

        #[inline(always)]
        unsafe fn from_index_unchecked(index: usize) -> Self {
            // SAFETY: we require index to be <= MAX_INDEX which still holds after the primitive cast
            unsafe { Self::from_u64_unchecked(index as u64) }
        }

        type Base = Self;
    }
}

mod id_size {
    use imctk_transparent::SubtypeCast;

    use crate::id::{u8_range_types::NonMaxMsbU8, GenericId, Id};
    use core::{fmt, fmt::Debug, hash::Hash};

    const LSBS: usize = (usize::BITS as usize / 8) - 1;

    /// [`Id`] type representing indices in the range `0..=isize::MAX as usize`.
    #[cfg(target_endian = "little")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[cfg_attr(target_pointer_width = "16", repr(C, align(2)))]
    #[cfg_attr(target_pointer_width = "32", repr(C, align(4)))]
    #[cfg_attr(target_pointer_width = "64", repr(C, align(8)))]
    pub struct IdSize {
        lsbs: [u8; LSBS],
        msb: NonMaxMsbU8,
    }

    #[cfg(target_endian = "big")]
    #[allow(dead_code)] // Only constructed via transmutation and/or pointer casts
    #[derive(Clone, Copy)]
    #[cfg_attr(target_pointer_width = "16", repr(C, align(2)))]
    #[cfg_attr(target_pointer_width = "32", repr(C, align(4)))]
    #[cfg_attr(target_pointer_width = "64", repr(C, align(8)))]
    pub struct IdSize {
        msb: NonMaxMsbU8,
        lsbs: [u8; LSBS],
    }

    // SAFETY: By using `#[repr(C, align(N))]` with N based on the pointer width, we guarantee a
    // compatible representation.
    unsafe impl SubtypeCast for IdSize {
        type Repr = usize;
    }

    impl IdSize {
        #[inline(always)]
        const fn as_usize(self) -> usize {
            // SAFETY: transmuting fully initialized data to u64 is always safe
            unsafe { std::mem::transmute::<Self, usize>(self) }
        }

        #[inline(always)]
        const unsafe fn from_usize_unchecked(index: usize) -> Self {
            debug_assert!(index <= Self::MAX_INDEX);
            // SAFETY: delegated to caller
            unsafe { std::mem::transmute::<usize, Self>(index) }
        }

        /// Returns the id with a given index, panicking when the index is invalid.
        ///
        /// Unlike the [`Id::from_index`] this is a `const fn`.
        ///
        /// This panics if and only if `index > Self::MAX_INDEX`.
        #[inline]
        pub const fn from_index_const(index: usize) -> Self {
            assert!(index <= Self::MAX_INDEX);
            // SAFETY: preceding assert checks the precondition
            unsafe { Self::from_usize_unchecked(index) }
        }
    }

    impl PartialEq for IdSize {
        #[inline(always)]
        fn eq(&self, other: &Self) -> bool {
            self.as_usize() == other.as_usize()
        }
    }

    impl Eq for IdSize {}

    impl PartialOrd for IdSize {
        #[inline(always)]
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }

        #[inline(always)]
        fn lt(&self, other: &Self) -> bool {
            self.as_usize() < other.as_usize()
        }

        #[inline(always)]
        fn le(&self, other: &Self) -> bool {
            self.as_usize() <= other.as_usize()
        }

        #[inline(always)]
        fn gt(&self, other: &Self) -> bool {
            self.as_usize() > other.as_usize()
        }

        #[inline(always)]
        fn ge(&self, other: &Self) -> bool {
            self.as_usize() >= other.as_usize()
        }
    }

    impl Ord for IdSize {
        #[inline(always)]
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.as_usize().cmp(&other.as_usize())
        }

        #[inline(always)]
        fn max(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_usize_unchecked(self.as_usize().max(other.as_usize())) }
        }

        #[inline(always)]
        fn min(self, other: Self) -> Self
        where
            Self: Sized,
        {
            // SAFETY: returns either value, each known to be a valid index
            unsafe { Self::from_usize_unchecked(self.as_usize().min(other.as_usize())) }
        }

        #[inline(always)]
        fn clamp(self, min: Self, max: Self) -> Self
        where
            Self: Sized,
            Self: PartialOrd,
        {
            // SAFETY: returns one of the three values, all known to be a valid index
            unsafe {
                Self::from_usize_unchecked(self.as_usize().clamp(min.as_usize(), max.as_usize()))
            }
        }
    }

    impl Debug for IdSize {
        #[inline(always)]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            Debug::fmt(&self.as_usize(), f)
        }
    }

    impl Hash for IdSize {
        #[inline(always)]
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            state.write_usize(self.as_usize())
        }

        #[inline(always)]
        fn hash_slice<H: std::hash::Hasher>(data: &[Self], state: &mut H)
        where
            Self: Sized,
        {
            // SAFETY: reading fully initialized data as &[u8] is always safe
            let bytes = unsafe { data.align_to::<u8>().1 };
            state.write(bytes);
        }
    }

    // SAFETY: the only purpose of all code for this type is to uphold the documented Id safety
    // requirements.
    unsafe impl Id for IdSize {
        type Generic = GenericId<{ Self::MAX_INDEX }>;

        #[inline(always)]
        fn index(self) -> usize {
            self.as_usize()
        }

        const MAX_INDEX: usize = isize::MAX as usize;

        const MIN: Self = {
            // SAFETY: zero is always <= MAX_INDEX
            unsafe { Self::from_usize_unchecked(0) }
        };

        const MAX: Self = {
            // SAFETY: MAX_INDEX is always <= MAX_INDEX
            unsafe { Self::from_usize_unchecked(Self::MAX_INDEX) }
        };

        #[inline(always)]
        unsafe fn from_index_unchecked(index: usize) -> Self {
            // SAFETY: delegated to caller
            unsafe { Self::from_usize_unchecked(index) }
        }

        type Base = Self;
    }
}

pub use id16::Id16;
pub use id32::Id32;
pub use id64::Id64;
pub use id8::Id8;
pub use id_size::IdSize;
