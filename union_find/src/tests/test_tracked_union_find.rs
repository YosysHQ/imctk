#![allow(missing_docs, dead_code)]
use super::*;
use change_tracking::ObserverId;
use imctk_lit::{Lit, Var};
use rand::prelude::*;
use std::collections::HashMap;

fn change_eq<Atom: Id, Elem: Id>(c1: &Change<Atom, Elem>, c2: &Change<Atom, Elem>) -> bool {
    match (c1, c2) {
        (
            Change::Union {
                new_repr: a1,
                merged_repr: b1,
            },
            Change::Union {
                new_repr: a2,
                merged_repr: b2,
            },
        ) => a1 == a2 && b1 == b2,
        (
            Change::MakeRepr {
                new_repr: a1,
                old_repr: b1,
            },
            Change::MakeRepr {
                new_repr: a2,
                old_repr: b2,
            },
        ) => a1 == a2 && b1 == b2,
        (Change::Renumber(r1), Change::Renumber(r2)) => Arc::as_ptr(r1) == Arc::as_ptr(r2),
        _ => false,
    }
}

struct CTUnionFind<Atom, Elem> {
    dut: TrackedUnionFind<Atom, Elem>,
    uf: UnionFind<Atom, Elem>,
    logs: HashMap<ObserverId, Vec<Change<Atom, Elem>>>,
}

impl<Atom: Id, Elem: Id + Element<Atom>> CTUnionFind<Atom, Elem> {
    fn new() -> Self {
        CTUnionFind {
            dut: TrackedUnionFind::new(),
            uf: UnionFind::new(),
            logs: HashMap::new(),
        }
    }
    fn find(&mut self, e: Elem) -> Elem {
        let dut_result = self.dut.find(e);
        let uf_result = self.uf.find(e);
        assert_eq!(dut_result, uf_result);
        uf_result
    }
    fn union_full(&mut self, elems: [Elem; 2]) -> (bool, [Elem; 2]) {
        let (uf_ok, [uf_ra, uf_rb]) = self.uf.union_full(elems);
        if uf_ok {
            let new_repr = uf_ra.atom();
            let merged_repr = uf_rb.apply_pol_of(uf_ra);
            for vec in self.logs.values_mut() {
                vec.push(Change::Union {
                    new_repr,
                    merged_repr,
                });
            }
        }
        let (dut_ok, [dut_ra, dut_rb]) = self.dut.union_full(elems);
        assert_eq!((dut_ok, [dut_ra, dut_rb]), (uf_ok, [uf_ra, uf_rb]));
        (uf_ok, [uf_ra, uf_rb])
    }
    fn make_repr(&mut self, new_repr: Atom) -> Elem {
        let uf_old_repr = self.uf.make_repr(new_repr);
        if uf_old_repr.atom() != new_repr {
            for vec in self.logs.values_mut() {
                vec.push(Change::MakeRepr {
                    new_repr,
                    old_repr: uf_old_repr,
                });
            }
        } else {
            assert!(uf_old_repr == Elem::from_atom(new_repr));
        }
        let dut_old_repr = self.dut.make_repr(new_repr);
        assert_eq!(dut_old_repr, uf_old_repr);
        uf_old_repr
    }
    fn start_observing(&mut self) -> ObserverToken {
        let token = self.dut.start_observing();
        assert!(self.logs.insert(token.observer_id(), Vec::new()).is_none());
        token
    }
    fn clone_token(&mut self, token: &ObserverToken) -> ObserverToken {
        let new_token = self.dut.clone_token(token);
        let cloned_log = self.logs.get(&token.observer_id()).unwrap().clone();
        assert!(self
            .logs
            .insert(new_token.observer_id(), cloned_log)
            .is_none());
        new_token
    }
    fn stop_observing(&mut self, token: ObserverToken) {
        assert!(self.logs.remove(&token.observer_id()).is_some());
        self.dut.stop_observing(token);
    }
    fn drain_changes_with_fn(
        &mut self,
        token: &mut ObserverToken,
        mut f: impl FnMut(&[Change<Atom, Elem>]),
    ) {
        let log = self.logs.get_mut(&token.observer_id()).unwrap();
        let mut log_iter = log.drain(..);
        self.dut.drain_changes_with_fn(token, |changes, _| {
            f(changes);
            for c1 in changes.iter() {
                let Some(c2) = log_iter.next() else {
                    panic!("not enough changes");
                };
                assert!(change_eq(c1, &c2));
            }
        });
        assert!(log_iter.next().is_none());
    }
    fn drain_some_changes(
        &mut self,
        token: &mut ObserverToken,
        calculate_count: impl FnOnce(usize) -> usize,
    ) -> usize {
        let log = self.logs.get_mut(&token.observer_id()).unwrap();
        let count = calculate_count(log.len());
        let log_iter = log.drain(..count);
        let mut gen = token.generation();
        let mut dut_iter = self.dut.drain_changes(token);
        for c2 in log_iter {
            let Some(c1) = dut_iter.next() else {
                panic!("not enough changes");
            };
            assert!(change_eq(c1, &c2));
            if let Change::Renumber(renumbering) = c1 {
                assert_eq!(renumbering.old_generation, gen);
                gen = renumbering.new_generation;
            }
        }
        dut_iter.stop();
        assert_eq!(token.generation(), gen);
        count
    }
    fn repr_reduction(&self) -> (IdVec<Atom, Option<Elem>>, IdVec<Atom, Elem>) {
        let mut forward: IdVec<Atom, Option<Elem>> = IdVec::default();
        let mut reverse: IdVec<Atom, Elem> = IdVec::default();
        for (atom, repr) in self.uf.iter() {
            let new_repr = *forward.grow_for_key(repr.atom()).get_or_insert_with(|| {
                let new_repr = reverse.push(Elem::from_atom(repr.atom())).0;
                Elem::from_atom(new_repr)
            });
            forward
                .grow_for_key(atom)
                .replace(new_repr.apply_pol_of(repr));
        }
        (forward, reverse)
    }
    fn renumber(&mut self) -> Arc<Renumbering<Atom, Elem>> {
        let (forward, reverse) = self.repr_reduction();
        let renumbering = self.dut.renumber(forward, reverse);
        for vec in self.logs.values_mut() {
            vec.push(Change::Renumber(renumbering.clone()));
        }
        self.uf = UnionFind::new();
        renumbering
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

#[test]
fn test_suite() {
    let mut u: CTUnionFind<Var, Lit> = CTUnionFind::new();
    let mut rng = rand_pcg::Pcg64::seed_from_u64(25);
    let mut active_tokens: Vec<ObserverToken> = Vec::new();
    let max_var = 2000;
    let verbosity = 2;
    for _ in 0..2000 {
        weighted_choose! {&mut rng,
            Union: 8.0 => {
                let a = Lit::from_code(rng.gen_range(0..=2 * max_var + 1));
                let b = Lit::from_code(rng.gen_range(0..=2 * max_var + 1));
                let result = u.union_full([a, b]);
                if verbosity > 1 {
                    println!("union({a}, {b}) = {result:?}");
                }
            },
            Find: 1.0 => {
                let a = Lit::from_code(rng.gen_range(0..=2 * max_var + 1));
                let result = u.find(a);
                if verbosity > 1 {
                    println!("find({a}) = {result}");
                }
            },
            MakeRepr: 2.0 => {
                let a = Var::from_index(rng.gen_range(0..=max_var));
                u.make_repr(a);
                if verbosity > 1 {
                    println!("make_repr({a})");
                }
            },
            StartObserving: 1.0 => {
                let token = u.start_observing();
                if verbosity > 0 {
                    println!("start observing {token:?}");
                }
                active_tokens.push(token);
            },
            StopObserving: 1.0 => {
                if let Some(index) = (0..active_tokens.len()).choose(&mut rng) {
                    let token = active_tokens.swap_remove(index);
                    if verbosity > 0 {
                        println!("stop observing {token:?}");
                    }
                    u.stop_observing(token);
                }
            },
            CloneToken: 1.0 => {
                if let Some(token) = active_tokens.iter().choose(&mut rng) {
                    let new_token = u.clone_token(token);
                    if verbosity > 0 {
                        println!("clone {token:?} -> {new_token:?}");
                    }
                    active_tokens.push(new_token);
                }
            },
            DrainAllChanges: 2.0 => {
                if let Some(token) = active_tokens.iter_mut().choose(&mut rng) {
                    let mut count = 0;
                    let mut gen = token.generation();
                    u.drain_changes_with_fn(token, |changes| {
                        for change in changes {
                            if let Change::Renumber(renumbering) = change {
                                assert_eq!(renumbering.old_generation, gen);
                                gen = renumbering.new_generation;
                            }
                            count += 1;
                        }
                    });
                    assert_eq!(gen, token.generation());
                    if verbosity > 0 {
                        println!("drained all ({count}) changes from {token:?}");
                    }
                }
            },
            DrainSomeChanges: 2.0 => {
                if let Some(token) = active_tokens.iter_mut().choose(&mut rng) {
                    let count = u.drain_some_changes(token, |total| (0..=total).choose(&mut rng).unwrap());
                    if verbosity > 0 {
                        println!("drained {count} changes from {token:?}");
                    }
                }
            },
            Renumber: 0.25 => {
                let renumbering = u.renumber();
                if verbosity > 0 {
                    println!("renumbered all variables, now on generation {} ({} old variables, {} new variables)",
                        u.dut.generation().0,
                        renumbering.forward.len(),
                        renumbering.reverse.len()
                    );
                }
            }
        }
    }
}

#[test]
#[should_panic]
fn test_token_error() {
    let mut tuf1: TrackedUnionFind<Var, Lit> = Default::default();
    let mut tuf2: TrackedUnionFind<Var, Lit> = Default::default();
    let mut token = tuf1.start_observing();
    tuf2.drain_changes(&mut token);
}
