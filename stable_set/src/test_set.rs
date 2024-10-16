#![cfg(test)]
#![allow(missing_docs)]
use crate::StableSet;
use indexmap::IndexSet;
use rand::prelude::*;
use std::{
    borrow::Borrow,
    hash::{BuildHasherDefault, Hash},
};
use zwohash::ZwoHasher;

// use u8 for the small indices to exercise the large index code
type ZwoSet<T> = StableSet<T, BuildHasherDefault<ZwoHasher>, u8>;

struct CheckedSet<T> {
    dut: ZwoSet<T>,
    ref_set: IndexSet<T>,
}

impl<T: Hash + Eq + Clone + std::fmt::Debug> CheckedSet<T> {
    fn new() -> Self {
        CheckedSet {
            dut: ZwoSet::new(),
            ref_set: IndexSet::new(),
        }
    }
    fn get_full<Q>(&mut self, value: &Q) -> Option<(usize, &T)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        let ref_result = self.ref_set.get_full(value);
        let dut_result = self.dut.get_full(value);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn insert_full(&mut self, value: T) -> (usize, bool) {
        let ref_result = self.ref_set.insert_full(value.clone());
        let dut_result = self.dut.insert_full(value);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn swap_remove_full<Q>(&mut self, value: &Q) -> Option<(usize, T)>
    where
        T: Borrow<Q>,
        Q: Hash + Eq,
    {
        let ref_result = self.ref_set.swap_remove_full(value);
        let dut_result = self.dut.swap_remove_full(value);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn swap_remove_index(&mut self, index: usize) -> Option<T> {
        let ref_result = self.ref_set.swap_remove_index(index);
        let dut_result = self.dut.swap_remove_index(index);
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn pop(&mut self) -> Option<T> {
        let ref_result = self.ref_set.pop();
        let dut_result = self.dut.pop();
        assert_eq!(ref_result, dut_result);
        ref_result
    }
    fn retain(&mut self, fun: impl Fn(&T) -> bool) {
        let mut ref_iter = self.ref_set.iter();
        self.dut.retain(|item| {
            // make sure that retain visits in the correct order
            assert_eq!(Some(item), ref_iter.next());
            fun(item)
        });
        assert_eq!(None, ref_iter.next());
        self.ref_set.retain(&fun);
        self.check();
    }
    fn check(&mut self) {
        self.dut.check();
        assert!(self.ref_set.iter().eq(&self.dut));
    }
    fn reserve(&mut self, additional: usize) {
        self.dut.reserve(additional);
        self.ref_set.reserve(additional);
    }
}

macro_rules! weighted_choose {
    ($rng:expr, $($name:ident: $weight:expr => $body:expr),+) => {
        {
            enum Branches { $( $name,  )* }
            let weights = [$((Branches::$name, $weight)),+];
            match weights.choose_weighted($rng, |x| x.1).unwrap().0 {
                $(Branches::$name => $body),*
            }
        }
    }
}

fn test_suite<T: Hash + Eq + Clone + std::fmt::Debug, R: Rng + SeedableRng>(
    mut rand_t: impl FnMut(&mut R) -> T,
    retain_fn: impl Fn(&T) -> bool,
) {
    let mut set: CheckedSet<T> = CheckedSet::new();
    let mut rng = R::seed_from_u64(25);
    let mut max_size = 0;
    let verbosity = 1;
    for _ in 0..5000 {
        weighted_choose! {&mut rng,
            Insert: 1.0 => {
                let item = rand_t(&mut rng);
                let result = set.insert_full(item.clone());
                if verbosity > 0 {
                    println!("inserting {item:?} -> {result:?}");
                }
            },
            GetPresent: 0.5 => {
                if let Some(item) = set.ref_set.iter().choose(&mut rng).cloned() {
                    let result = set.get_full(&item);
                    if verbosity > 0 {
                        println!("getting {item:?} -> {result:?}");
                    }
                }
            },
            GetRandom: 0.5 => {
                let item = rand_t(&mut rng);
                let result = set.get_full(&item);
                if verbosity > 0 {
                    println!("getting {item:?} -> {result:?}");
                }
            },
            RemovePresent: 0.3 => {
                if let Some(item) = set.ref_set.iter().choose(&mut rng).cloned() {
                    let result = set.swap_remove_full(&item);
                    if verbosity > 0 {
                        println!("removing {item:?} -> {result:?}");
                    }
                }
            },
            RemoveRandom: 0.5 => {
                let item = rand_t(&mut rng);
                let result = set.swap_remove_full(&item);
                if verbosity > 0 {
                    println!("removing {item:?} -> {result:?}");
                }
            },
            RemoveIndex: 0.2 => {
                let len = set.ref_set.len();
                // try to generate invalid indices sometimes
                let index = rng.gen_range(0..=(len + len.div_ceil(10)));
                let result = set.swap_remove_index(index);
                if verbosity > 0 {
                    println!("removing index {index:?} -> {result:?}");
                }
            },
            Pop: 0.2 => {
                let result = set.pop();
                if verbosity > 0 {
                    println!("popping -> {result:?}");
                }
            },
            Retain: 0.05 => {
                let old_len = set.ref_set.len();
                set.retain(&retain_fn);
                let new_len = set.ref_set.len();
                if verbosity > 0 {
                    println!("retaining, {old_len} -> {new_len}");
                }
            },
            Check: 0.15 => {
                set.check();
                if verbosity > 0 {
                    println!("check");
                }
            }
        };
        max_size = std::cmp::max(max_size, set.ref_set.len());
    }
    set.check();
    println!("max size: {max_size}");
}

#[test]
fn test_suite_usize() {
    test_suite::<usize, rand_pcg::Pcg64>(
        |rng| rng.gen::<usize>() >> rng.gen_range(0..usize::BITS),
        |item| item % 17 < 15,
    );
}

#[test]
fn test_suite_boxed_usize() {
    test_suite::<Box<usize>, rand_pcg::Pcg64>(
        |rng| Box::new(rng.gen::<usize>() >> rng.gen_range(0..usize::BITS)),
        |item| **item % 17 < 15,
    );
}

#[test]
fn test_suite_string() {
    test_suite::<String, rand_pcg::Pcg64>(
        |rng| {
            let len = rng.gen_range(4..32);
            String::from_iter((0..len).map(|_| rng.gen_range('!'..'~')))
        },
        |item| !item.contains('!')
    );
}

#[test]
fn test_reserve() {
    let mut rng = rand_pcg::Pcg64::seed_from_u64(58);
    for size in [10, 50, 100, 200, 500, 1000] {
        // just check if this doesn't cause a crash
        let mut set = CheckedSet::<usize>::new();
        set.reserve(size);
        for _ in 0..size + 5 {
            set.insert_full(rng.gen());
        }
    }
}

#[test]
fn test_from_iter_extend() {
    let rng_start = rand_pcg::Pcg64::seed_from_u64(58);
    let mut rng = rng_start.clone();
    let mut set = (0..1000).map(|_| rng.gen()).collect::<ZwoSet<usize>>();
    set.extend((0..1000).map(|_| rng.gen()));
    set.check();
    rng = rng_start.clone();
    assert!(set.iter().copied().eq((0..2000).map(|_| rng.gen::<usize>())));
}

#[test]
fn test_primes() {
    let mut set: ZwoSet<usize> = (2..200).collect();
    let mut ref_primes = [
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89,
        97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181,
        191, 193, 197, 199,
    ]
    .into_iter();
    while let Some(&k) = set.first() {
        assert_eq!(ref_primes.next(), Some(k));
        set.retain(|&n| n % k != 0);
        set.check();
    }
    assert_eq!(ref_primes.next(), None);
    assert!(set.is_empty());
}
