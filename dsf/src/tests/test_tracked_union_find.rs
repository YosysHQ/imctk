use super::*;
use imctk_lit::{Lit, Var};

#[test]
fn test() {
    let l = |n| Var::from_index(n).as_lit();
    let mut tuf = TrackedUnionFind::<Var, Lit>::new();
    let mut token = tuf.start_observing();
    tuf.union([l(3), !l(4)]);
    tuf.union([l(8), l(7)]);
    let mut token2 = tuf.start_observing();
    tuf.union([l(4), l(5)]);
    for change in tuf.drain_changes(&mut token).cloned() {
        println!("{change:?}");
    }
    println!("---");
    tuf.union([!l(5), l(6)]);
    tuf.make_repr(l(4).var());
    let renumber: IdVec<Var, Option<Lit>> =
        IdVec::from_vec(vec![Some(l(0)), None, None, Some(l(1)), Some(!l(1)), Some(!l(1)), Some(l(1)), Some(l(2)), Some(l(2))]);
    let reverse = Renumbering::get_reverse(&renumber, &tuf.union_find);
    dbg!(&renumber, &reverse);
    tuf.renumber(renumber, reverse);
    tuf.union([l(0), l(1)]);
    let mut iter = tuf.drain_changes(&mut token);
    println!("{:?}", iter.next());
    iter.stop();
    println!("---");
    for change in tuf.drain_changes(&mut token2).cloned() {
        println!("{change:?}");
    }
}
