#![allow(missing_docs)] // test only
use imctk_ids::*;

use std::{hash::BuildHasher, marker::PhantomData};

#[cfg(miri)]
const N: usize = 32;
#[cfg(miri)]
const M: usize = 4;

#[cfg(not(miri))]
const N: usize = 1024;
#[cfg(not(miri))]
const M: usize = 64;

fn scale<T: Id>(i: usize, n: usize) -> usize {
    (((i as u128) * (T::MAX_ID_INDEX as u128)) / (n as u128)) as usize
}

fn basic_tests<T: Id>(forward_debug: bool) {
    let build_hasher = std::hash::RandomState::new();

    for i in 0..=N {
        let index = scale::<usize>(i, N);
        let id = T::try_from_id_index(index);
        assert_eq!(id.is_some(), index <= T::MAX_ID_INDEX);
        if let Some(value) = id {
            assert_eq!(value.id_index(), index);
        }
    }

    for index in T::MAX_ID_INDEX.saturating_sub(N)..=T::MAX_ID_INDEX.saturating_add(N) {
        let id = T::try_from_id_index(index);
        assert_eq!(id.is_some(), index <= T::MAX_ID_INDEX);
        if let Some(value) = id {
            assert_eq!(value.id_index(), index);
        }
    }

    let scale = scale::<T>;

    for index in 0..=T::MAX_ID_INDEX.min(N) {
        let id = T::from_id_index(index);
        assert_eq!(id.id_index(), index);
        if forward_debug {
            assert_eq!(format!("{:?}", id), format!("{:?}", index));
        } else {
            assert!(format!("{:?}", id).contains(&format!("{:?}", index)));
        }
    }
    for index in T::MAX_ID_INDEX.saturating_sub(N)..=T::MAX_ID_INDEX {
        let id = T::from_id_index(index);
        assert_eq!(id.id_index(), index);
        if forward_debug {
            assert_eq!(format!("{:?}", id), format!("{:?}", index));
        } else {
            assert!(format!("{:?}", id).contains(&format!("{:?}", index)));
        }
    }
    for i in 0..=N {
        let index = scale(i, N);
        let id = T::from_id_index(index);
        assert_eq!(id.id_index(), index);
        if forward_debug {
            assert_eq!(format!("{:?}", id), format!("{:?}", index));
        } else {
            assert!(format!("{:?}", id).contains(&format!("{:?}", index)));
        }
    }
    for i in 0..=M {
        let index_i = scale(i, M);
        let id_i = T::from_id_index(index_i);
        for j in 0..=M {
            let index_j = scale(j, M);
            let id_j = T::from_id_index(index_j);
            assert_eq!(index_i < index_j, id_i < id_j);
            assert_eq!(index_i <= index_j, id_i <= id_j);
            assert_eq!(index_i > index_j, id_i > id_j);
            assert_eq!(index_i >= index_j, id_i >= id_j);
            assert_eq!(index_i == index_j, id_i == id_j);
            assert_eq!(index_i.partial_cmp(&index_j), id_i.partial_cmp(&id_j));
            assert_eq!(index_i.max(index_j), id_i.max(id_j).id_index());
            assert_eq!(index_i.min(index_j), id_i.min(id_j).id_index());
            assert!(
                index_i != index_j
                    || ({ build_hasher.hash_one(id_i) }) == { build_hasher.hash_one(id_j) }
            );
            assert!(
                index_i != index_j
                    || ({ build_hasher.hash_one([id_i, id_j]) }) == {
                        build_hasher.hash_one([id_j, id_i])
                    }
            );

            if index_i <= index_j {
                for k in 0..=M {
                    let index_k = scale(k, M);
                    let id_k = T::from_id_index(index_k);
                    assert_eq!(
                        index_k.clamp(index_i, index_j),
                        id_k.clamp(id_i, id_j).id_index()
                    );
                }
            }
        }
    }
}

fn base_conversion_tests<T: Id, U: Id<BaseId = T::BaseId>>() {
    for index in 0..=T::MAX_ID_INDEX.min(N) {
        let id = T::from_id_index(index);
        let id = U::from_base_id(id.into_base_id());
        assert_eq!(id.id_index(), index);
    }
    for index in T::MAX_ID_INDEX.saturating_sub(N)..=T::MAX_ID_INDEX {
        let id = T::from_id_index(index);
        let id = U::from_base_id(id.into_base_id());
        assert_eq!(id.id_index(), index);
    }
    for i in 0..=N {
        let index = scale::<T>(i, N);
        let id = T::from_id_index(index);
        let id = U::from_base_id(id.into_base_id());
        assert_eq!(id.id_index(), index);
    }
}

fn generic_conversion_tests<T: Id, U: Id<GenericId = T::GenericId>>() {
    for index in 0..=T::MAX_ID_INDEX.min(N) {
        let id = U::cast_from_id(T::from_id_index(index));
        let id2 = T::from_id_index(index).cast_into_id();
        assert_eq!(id, id2);
        assert_eq!(id.id_index(), index);
    }
    for index in T::MAX_ID_INDEX.saturating_sub(N)..=T::MAX_ID_INDEX {
        let id = U::cast_from_id(T::from_id_index(index));
        let id2 = T::from_id_index(index).cast_into_id();
        assert_eq!(id, id2);
        assert_eq!(id.id_index(), index);
    }
    for i in 0..=N {
        let index = scale::<T>(i, N);
        let id = U::cast_from_id(T::from_id_index(index));
        let id2 = T::from_id_index(index).cast_into_id();
        assert_eq!(id, id2);
        assert_eq!(id.id_index(), index);
    }
}

#[test]
fn basic_id8() {
    basic_tests::<Id8>(true)
}

#[test]
fn basic_id16() {
    basic_tests::<Id16>(true)
}

#[test]
fn basic_id32() {
    basic_tests::<Id32>(true)
}

#[test]
fn basic_id64() {
    basic_tests::<Id64>(true)
}

#[test]
fn basic_id_size() {
    basic_tests::<IdSize>(true)
}

#[test]
fn basic_id_generic() {
    basic_tests::<GenericId<4321>>(true)
}

#[derive(Id)]
#[repr(transparent)]
pub struct CustomDebug(Id32);

impl core::fmt::Debug for CustomDebug {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.id_index())
    }
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct GenericWrapper<T: Id>(T);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeId8(Id8);

#[test]
fn basic_newtype_id8() {
    basic_tests::<NewtypeId8>(false);
}

#[test]
fn base_convert_id8() {
    base_conversion_tests::<NewtypeId8, Id8>();
    base_conversion_tests::<Id8, NewtypeId8>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeId16(Id16);

#[test]
fn basic_newtype_id16() {
    basic_tests::<NewtypeId16>(false);
}

#[test]
fn base_convert_id16() {
    base_conversion_tests::<NewtypeId16, Id16>();
    base_conversion_tests::<Id16, NewtypeId16>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeId32(Id32);

#[test]
fn basic_newtype_id32() {
    basic_tests::<NewtypeId32>(false);
}

#[test]
fn base_convert_id32() {
    base_conversion_tests::<NewtypeId32, Id32>();
    base_conversion_tests::<Id32, NewtypeId32>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeId64(Id64);

#[test]
fn basic_newtype_id64() {
    basic_tests::<NewtypeId64>(false);
}

#[test]
fn base_convert_id64() {
    base_conversion_tests::<NewtypeId64, Id64>();
    base_conversion_tests::<Id64, NewtypeId64>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeIdSize(IdSize);

#[test]
fn basic_newtype_id_size() {
    basic_tests::<NewtypeIdSize>(false);
}

#[test]
fn base_convert_id_size() {
    base_conversion_tests::<NewtypeIdSize, IdSize>();
    base_conversion_tests::<IdSize, NewtypeIdSize>();
}

#[test]
fn basic_id_custom_debug() {
    basic_tests::<CustomDebug>(true);
}

#[test]
fn basic_id_generic_wrapper() {
    basic_tests::<GenericWrapper<Id32>>(false);
}

#[test]
fn basic_u8() {
    basic_tests::<u8>(true)
}

#[test]
fn basic_u16() {
    basic_tests::<u16>(true)
}

#[test]
fn basic_u32() {
    basic_tests::<u32>(true)
}

#[test]
fn basic_u64() {
    basic_tests::<u64>(true)
}

#[test]
fn basic_usize() {
    basic_tests::<usize>(true)
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct CustomU8(GenericId<{ u8::MAX as usize }>);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeU8(u8);

#[test]
fn conversion_u8() {
    generic_conversion_tests::<CustomU8, u8>();
    generic_conversion_tests::<u8, CustomU8>();
    base_conversion_tests::<NewtypeU8, u8>();
    base_conversion_tests::<u8, NewtypeU8>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct CustomU16(GenericId<{ u16::MAX as usize }>);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeU16(u16);

#[test]
fn conversion_u16() {
    generic_conversion_tests::<CustomU16, u16>();
    generic_conversion_tests::<u16, CustomU16>();
    base_conversion_tests::<NewtypeU16, u16>();
    base_conversion_tests::<u16, NewtypeU16>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct CustomU32(GenericId<{ u32::MAX as usize }>);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeU32(u32);

#[test]
fn conversion_u32() {
    generic_conversion_tests::<CustomU32, u32>();
    generic_conversion_tests::<u32, CustomU32>();
    base_conversion_tests::<NewtypeU32, u32>();
    base_conversion_tests::<u32, NewtypeU32>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct CustomU64(GenericId<{ u64::MAX as usize }>);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeU64(u64);

#[test]
fn conversion_u64() {
    generic_conversion_tests::<CustomU64, u64>();
    generic_conversion_tests::<u64, CustomU64>();
    base_conversion_tests::<NewtypeU64, u64>();
    base_conversion_tests::<u64, NewtypeU64>();
}

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct CustomUsize(GenericId<{ usize::MAX }>);

#[derive(Id, Debug)]
#[repr(transparent)]
pub struct NewtypeUsize(usize);

#[test]
fn conversion_usize() {
    generic_conversion_tests::<CustomUsize, usize>();
    generic_conversion_tests::<usize, CustomUsize>();
    base_conversion_tests::<NewtypeUsize, usize>();
    base_conversion_tests::<usize, NewtypeUsize>();
}

#[derive(Id)]
#[repr(transparent)]
pub struct NewtypePhantom<T: 'static>(usize, PhantomData<T>);

impl<T> std::fmt::Debug for NewtypePhantom<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("NewtypePhantom")
            .field(&self.0)
            .field(&self.1)
            .finish()
    }
}

#[test]
fn basic_phantom() {
    basic_tests::<NewtypePhantom<String>>(false);
}
