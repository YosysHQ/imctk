#![allow(dead_code, missing_docs)]

use super::*;
use imctk_ids::id_set_seq::IdSetSeq;
use imctk_lit::{Lit, Var};
use rand::prelude::*;
use std::collections::{HashSet, VecDeque};

#[derive(Default)]
struct CheckedUnionFind<Atom: Id, Elem> {
    dut: UnionFind<Atom, Elem>,
    equivs: IdSetSeq<Atom, Elem>,
}

impl<Atom: Id, Elem: Id + Element<Atom>> UnionFind<Atom, Elem> {
    fn debug_print_tree(
        children: &IdVec<Atom, Vec<Elem>>,
        atom: Atom,
        prefix: &str,
        self_char: &str,
        further_char: &str,
        pol: bool,
    ) {
        println!(
            "{prefix}{self_char}{}{:?}",
            if pol { "!" } else { "" },
            atom
        );
        let my_children = children.get(atom).unwrap();
        for (index, &child) in my_children.iter().enumerate() {
            let last = index == my_children.len() - 1;
            let self_char = if last { "└" } else { "├" };
            let next_further_char = if last { " " } else { "│" };
            Self::debug_print_tree(
                children,
                child.atom(),
                &(prefix.to_string() + further_char),
                self_char,
                next_further_char,
                pol ^ (child != Elem::from_atom(child.atom())),
            );
        }
    }
    fn debug_print(&self) {
        let mut children: IdVec<Atom, Vec<Elem>> = Default::default();
        for atom in self.parent.keys() {
            let parent = self.read_parent(atom);
            children.grow_for_key(atom);
            if atom != parent.atom() {
                children
                    .grow_for_key(parent.atom())
                    .push(Elem::from_atom(atom).apply_pol_of(parent));
            } else {
                assert!(Elem::from_atom(atom) == parent);
            }
        }
        for atom in self.parent.keys() {
            if atom == self.read_parent(atom).atom() {
                Self::debug_print_tree(&children, atom, "", "", " ", false);
            }
        }
    }
}
#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Eq)]
enum VarRel {
    Equiv,
    AntiEquiv,
    NotEquiv,
}

impl<Atom: Id, Elem: Id + Element<Atom>> CheckedUnionFind<Atom, Elem> {
    fn new() -> Self {
        CheckedUnionFind {
            dut: Default::default(),
            equivs: Default::default(),
        }
    }
    fn ref_equal(&mut self, start: Elem, goal: Elem) -> VarRel {
        let mut seen: HashSet<Atom> = Default::default();
        let mut queue: VecDeque<Elem> = [start].into();
        while let Some(place) = queue.pop_front() {
            if place.atom() == goal.atom() {
                if place == goal {
                    return VarRel::Equiv;
                } else {
                    return VarRel::AntiEquiv;
                }
            }
            seen.insert(place.atom());
            for &next in self.equivs.grow_for(place.atom()).iter() {
                if !seen.contains(&next.atom()) {
                    queue.push_back(next.apply_pol_of(place));
                }
            }
        }
        VarRel::NotEquiv
    }
    fn find(&mut self, lit: Elem) -> Elem {
        let out = self.dut.find(lit);
        assert!(self.ref_equal(lit, out) == VarRel::Equiv);
        out
    }
    fn union_full(&mut self, lits: [Elem; 2]) -> (bool, [Elem; 2]) {
        let (ok, [ra, rb]) = self.dut.union_full(lits);
        assert_eq!(self.ref_equal(lits[0], ra), VarRel::Equiv);
        assert_eq!(self.ref_equal(lits[1], rb), VarRel::Equiv);
        assert_eq!(ok, self.ref_equal(lits[0], lits[1]) == VarRel::NotEquiv);
        assert_eq!(self.dut.find_root(lits[0]), ra);
        if ok {
            assert_eq!(self.dut.find_root(lits[1]), ra);
            self.equivs
                .grow_for(lits[0].atom())
                .insert(lits[1].apply_pol_of(lits[0]));
            self.equivs
                .grow_for(lits[1].atom())
                .insert(lits[0].apply_pol_of(lits[1]));
        } else {
            assert_eq!(self.dut.find_root(lits[1]).atom(), ra.atom());
        }
        (ok, [ra, rb])
    }
    fn union(&mut self, lits: [Elem; 2]) -> bool {
        self.union_full(lits).0
    }
    fn make_repr(&mut self, lit: Atom) {
        self.dut.make_repr(lit);
        assert_eq!(
            self.dut.find_root(Elem::from_atom(lit)),
            Elem::from_atom(lit)
        );
        self.check();
    }
    fn check(&mut self) {
        for atom in self.dut.parent.keys() {
            let parent = self.dut.read_parent(atom);
            assert_eq!(self.ref_equal(Elem::from_atom(atom), parent), VarRel::Equiv);
            let root = self.dut.find_root(Elem::from_atom(atom));
            for &child in self.equivs.grow_for(atom).iter() {
                assert_eq!(root, self.dut.find_root(child));
            }
        }
    }
}

#[test]
fn test_suite() {
    let mut u: CheckedUnionFind<Var, Lit> = CheckedUnionFind::new();
    let mut rng = rand_pcg::Pcg64::seed_from_u64(25);
    let max_var = 2000;
    for _ in 0..2000 {
        match rng.gen_range(0..10) {
            0..=4 => {
                let a = Lit::from_code(rng.gen_range(0..=2 * max_var + 1));
                let b = Lit::from_code(rng.gen_range(0..=2 * max_var + 1));
                let result = u.union_full([a, b]);
                println!("union({a}, {b}) = {result:?}");
            }
            5..=7 => {
                let a = Lit::from_code(rng.gen_range(0..=2 * max_var + 1));
                let result = u.find(a);
                println!("find({a}) = {result}");
            }
            8 => {
                u.check();
            }
            9 => {
                let a = Var::from_index(rng.gen_range(0..=max_var));
                u.make_repr(a);
                println!("make_repr({a})");
            }
            _ => {}
        }
    }
    u.check();
    //u.dut.debug_print();
}
