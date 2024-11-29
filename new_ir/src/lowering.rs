use imctk_ids::{id_vec::IdVec, IdAlloc};
use imctk_lit::{Lit, Var};
use imctk_paged_storage::index::IndexedTermRef;
use itertools::izip;

use crate::{
    bitlevel::{self, BitlevelTerm},
    ir::{BitIr, WordIr},
    wordlevel::*,
};

type WVar = Var;
type BVar = Var;
type BLit = Lit;
type NodeId = u32;

#[derive(Default)]
struct VarMap {
    inner: IdVec<WVar, Option<Vec<BLit>>>,
}

impl VarMap {
    pub fn get(&self, var: WVar) -> &[BLit] {
        self.inner[var].as_ref().unwrap()
    }
    pub fn allocate(&mut self, bit_ir: &mut BitIr, var: WVar, sort: &Sort) {
        self.inner
            .grow_for_key(var)
            .get_or_insert_with(|| bit_ir.fresh_lits(sort.0 as usize));
    }
    pub fn set(&mut self, bit_ir: &mut BitIr, var: WVar, value: Vec<BLit>) {
        match self.inner.grow_for_key(var) {
            &mut Some(ref existing) => {
                debug_assert_eq!(existing.len(), value.len());
                for (&a, b) in izip!(existing, value) {
                    bit_ir.union_find.union([a, b]);
                }
            }
            r @ &mut None => {
                *r = Some(value);
            }
        }
    }
    pub fn set_bool(&mut self, bit_ir: &mut BitIr, var: WVar, value: BLit) {
        self.set(bit_ir, var, vec![value]);
    }
    pub fn contains(&self, v: Var) -> bool {
        matches!(self.inner.get(v), Some(Some(_)))
    }
}

#[derive(Default)]
pub struct Lowering {
    var_map: VarMap,
    input_alloc: IdAlloc<bitlevel::InputId>,
    steady_input_alloc: IdAlloc<bitlevel::SteadyInputId>,
}

macro_rules! bits {
    ($self:expr, $($a:expr),*) => {
        izip!($($self.var_map.get($a).iter().copied()),*)
    };
}

macro_rules! single_bit {
    ($self:expr, $a:expr) => {{
        let slice = $self.var_map.get($a);
        debug_assert!(slice.len() == 1);
        slice[0]
    }};
}

fn adder_carry(bit_ir: &mut BitIr, a: Lit, b: Lit, c: Lit) -> Lit {
    let q0 = bit_ir.xor(a, b);
    let c0 = bit_ir.and(a, b);
    let c1 = bit_ir.and(q0, c);
    bit_ir.or(c0, c1)
}

fn full_adder(bit_ir: &mut BitIr, a: Lit, b: Lit, c: Lit, q: Lit) -> Lit {
    let q0 = bit_ir.xor(a, b);
    bit_ir.xor_node(q0, c, q);
    let c0 = bit_ir.and(a, b);
    let c1 = bit_ir.and(q0, c);
    bit_ir.or(c0, c1)
}

fn mux(bit_ir: &mut BitIr, c: Lit, a: Lit, b: Lit) -> Lit {
    let ca = bit_ir.and(c, a);
    let cb = bit_ir.and(!c, b);
    bit_ir.or(ca, cb)
}

fn sext(n: usize, mut iter: impl DoubleEndedIterator<Item = Lit>) -> Vec<Lit> {
    let sign = iter.next_back().unwrap();
    iter.chain(std::iter::repeat(sign)).take(n).collect()
}

fn with_temp<R>(
    bit_ir: &mut BitIr,
    n: usize,
    f: impl FnOnce(&mut BitIr, &[BLit]) -> R,
) -> (Vec<BLit>, R) {
    let q = bit_ir.fresh_lits(n);
    let r = f(bit_ir, &q);
    (q, r)
}

fn add(bit_ir: &mut BitIr, a: &[BLit], b: &[BLit], q: &[BLit]) -> BLit {
    debug_assert!(a.len() == b.len() && b.len() == q.len());
    let mut c = BLit::FALSE;
    for (&x, &y, &z) in izip!(a, b, q) {
        c = full_adder(bit_ir, x, y, c, z);
    }
    c
}

fn multiply(bit_ir: &mut BitIr, a: &[BLit], b: &[BLit], q: &[BLit]) -> BLit {
    debug_assert!(a.len() == b.len() && b.len() == q.len());
    if !a.is_empty() {
        bit_ir.and_node(a[0], b[0], q[0]);
    }
    if a.len() > 1 {
        let len = a.len() - 1;
        let (mut q0, o0) = with_temp(bit_ir, len, |bit_ir, q0| {
            multiply(bit_ir, &a[1..], &b[1..], q0)
        });
        let o1 = q0.pop().unwrap();
        q0.insert(0, Lit::FALSE);
        let q1: Vec<_> = b[1..].iter().map(|&x| bit_ir.and(a[0], x)).collect();
        let q2: Vec<_> = a[1..].iter().map(|&x| bit_ir.and(b[0], x)).collect();
        let (q3, o2) = with_temp(bit_ir, len, |bit_ir, q3| add(bit_ir, &q1, &q2, q3));
        let o3 = add(bit_ir, &q0, &q3, &q[1..]);
        bit_ir.reduce_or([o0, o1, o2, o3])
    } else {
        BLit::FALSE
    }
}

impl Lowering {
    fn shift(
        &mut self,
        bit_ir: &mut BitIr,
        args: &[WVar; 2],
        output: WVar,
        right: bool,
        arithmetic: bool,
    ) {
        let mut value = self.var_map.get(args[0]).to_vec();
        let len = value.len();
        let fill = if arithmetic {
            *value.last().unwrap()
        } else {
            Lit::FALSE
        };
        let mut new_value = Vec::with_capacity(len);
        for (i, amount) in bits!(self, args[1]).enumerate() {
            new_value.clear();
            for j in 0..len {
                let a = if i >= 32 {
                    fill
                } else {
                    let index = if right {
                        j.checked_add(1 << i)
                    } else {
                        j.checked_sub(1 << i)
                    };
                    index.and_then(|i| value.get(i).copied()).unwrap_or(fill)
                };
                let b = value[j];
                new_value.push(mux(bit_ir, amount, a, b));
            }
            std::mem::swap(&mut value, &mut new_value);
        }
        self.var_map.set(bit_ir, output, value);
    }
    fn rotate(&mut self, bit_ir: &mut BitIr, args: &[WVar; 2], output: WVar, right: bool) {
        let mut value = self.var_map.get(args[0]).to_vec();
        let len = value.len();
        let mut new_value = Vec::with_capacity(len);
        let mut offset = 1;
        for amount in bits!(self, args[1]) {
            new_value.clear();
            for j in 0..len {
                let index = if right {
                    j + (len - offset)
                } else {
                    j + offset
                };
                let a = value[index % len];
                let b = value[j];
                new_value.push(mux(bit_ir, amount, a, b));
            }
            offset = (offset * 2) % len;
            std::mem::swap(&mut value, &mut new_value);
        }
        self.var_map.set(bit_ir, output, value);
    }
    pub fn lower_node(&mut self, word_ir: &mut WordIr, bit_ir: &mut BitIr, node_id: NodeId) {
        let node: WordlevelNode<WordlevelTerm> = word_ir.egraph.get(node_id);
        match &node.term {
            WordlevelTerm::And(And(args, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                for (a, b, q) in bits!(self, args[0], args[1], node.output) {
                    bit_ir.and_node(a, b, q);
                }
            }
            WordlevelTerm::Or(Or(args, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                for (a, b, q) in bits!(self, args[0], args[1], node.output) {
                    bit_ir.or_node(a, b, q);
                }
            }
            WordlevelTerm::Xor(Xor(args, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                for (a, b, q) in bits!(self, args[0], args[1], node.output) {
                    bit_ir.xor_node(a, b, q);
                }
            }
            WordlevelTerm::Not(Not(arg, _sort)) => {
                let bits = bits!(self, *arg).map(|b| !b).collect();
                self.var_map.set(bit_ir, node.output, bits);
            }
            WordlevelTerm::Add(Add(args, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                let mut c = BLit::FALSE;
                for (a, b, q) in bits!(self, args[0], args[1], node.output) {
                    c = full_adder(bit_ir, a, b, c, q);
                }
            }
            WordlevelTerm::Sub(Sub(args, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                let mut c = BLit::TRUE;
                for (a, b, q) in bits!(self, args[0], args[1], node.output) {
                    c = full_adder(bit_ir, a, !b, c, q);
                }
            }
            WordlevelTerm::UaddOverflow(UaddOverflow(args)) => {
                let mut c = BLit::FALSE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    c = adder_carry(bit_ir, a, b, c);
                }
                self.var_map.set_bool(bit_ir, node.output, c);
            }
            WordlevelTerm::SaddOverflow(SaddOverflow(args)) => {
                let mut c = BLit::FALSE;
                let mut sc = BLit::FALSE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    sc = c;
                    c = adder_carry(bit_ir, a, b, c);
                }
                let value = bit_ir.xor(c, sc);
                self.var_map.set_bool(bit_ir, node.output, value);
            }
            WordlevelTerm::UsubOverflow(UsubOverflow(args)) => {
                let mut c = BLit::TRUE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    c = adder_carry(bit_ir, a, !b, c);
                }
                self.var_map.set_bool(bit_ir, node.output, !c);
            }
            WordlevelTerm::SsubOverflow(SsubOverflow(args)) => {
                let mut c = BLit::TRUE;
                let mut sc = BLit::TRUE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    sc = c;
                    c = adder_carry(bit_ir, a, !b, c);
                }
                let value = bit_ir.xor(c, sc);
                self.var_map.set_bool(bit_ir, node.output, value);
            }
            WordlevelTerm::Concat(Concat(args, _sort)) => {
                let bits = bits!(self, args[0]).chain(bits!(self, args[1])).collect();
                self.var_map.set(bit_ir, node.output, bits);
            }
            WordlevelTerm::Const(Const { mut value, sort }) => {
                let mut bits = Vec::with_capacity(sort.0 as usize);
                for _ in 0..sort.0 {
                    bits.push(BLit::FALSE ^ ((value & 1) != 0));
                    value >>= 1;
                }
                self.var_map.set(bit_ir, node.output, bits);
            }
            WordlevelTerm::Eq(Eq(args)) => {
                let mut result = BLit::TRUE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    let eq = !bit_ir.xor(a, b);
                    result = bit_ir.and(result, eq);
                }
                self.var_map.set_bool(bit_ir, node.output, result);
            }
            WordlevelTerm::Ugt(Ugt(args)) => {
                let mut c = BLit::TRUE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    c = adder_carry(bit_ir, b, !a, c);
                }
                self.var_map.set_bool(bit_ir, node.output, !c);
            }
            WordlevelTerm::Uge(Uge(args)) => {
                let mut c = BLit::TRUE;
                for (a, b) in bits!(self, args[0], args[1]) {
                    c = adder_carry(bit_ir, a, !b, c);
                }
                self.var_map.set_bool(bit_ir, node.output, c);
            }
            WordlevelTerm::Sgt(Sgt(args)) => {
                let mut c = BLit::TRUE;
                let mut iter = bits!(self, args[0], args[1]);
                let (sign_a, sign_b) = iter.next_back().unwrap();
                for (a, b) in iter {
                    c = adder_carry(bit_ir, b, !a, c);
                }
                c = adder_carry(bit_ir, !sign_b, sign_a, c);
                self.var_map.set_bool(bit_ir, node.output, !c);
            }
            WordlevelTerm::Sge(Sge(args)) => {
                let mut c = BLit::TRUE;
                let mut iter = bits!(self, args[0], args[1]);
                let (sign_a, sign_b) = iter.next_back().unwrap();
                for (a, b) in iter {
                    c = adder_carry(bit_ir, a, !b, c);
                }
                c = adder_carry(bit_ir, !sign_a, sign_b, c);
                self.var_map.set_bool(bit_ir, node.output, c);
            }
            WordlevelTerm::Ite(Ite {
                cond,
                then,
                els,
                sort,
            }) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                let c = single_bit!(self, *cond);
                for (a, b, q) in bits!(self, *then, *els, node.output) {
                    let ac = bit_ir.and(c, a);
                    let bc = bit_ir.and(!c, b);
                    bit_ir.or_node(ac, bc, q);
                }
            }
            WordlevelTerm::Input(Input(_id, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                let ids = self.input_alloc.alloc_range(sort.0 as usize);
                for (q, id) in bits!(self, node.output).zip(ids) {
                    bit_ir.node(q, BitlevelTerm::Input(id));
                }
            }
            WordlevelTerm::SteadyInput(SteadyInput(_id, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                let ids = self.steady_input_alloc.alloc_range(sort.0 as usize);
                for (q, id) in bits!(self, node.output).zip(ids) {
                    bit_ir.node(q, BitlevelTerm::SteadyInput(id));
                }
            }
            WordlevelTerm::Reg(Reg { next, init, sort }) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                for (d, i, q) in bits!(self, *next, *init, node.output) {
                    bit_ir.node(q, BitlevelTerm::Reg(bitlevel::Reg { next: d, init: i }));
                }
            }
            WordlevelTerm::Zext(Zext(arg, sort)) => {
                let bits = bits!(self, *arg)
                    .chain(std::iter::repeat(Lit::FALSE))
                    .take(sort.0 as usize)
                    .collect();
                self.var_map.set(bit_ir, node.output, bits);
            }
            WordlevelTerm::Sext(Sext(arg, sort)) => {
                let mut iter = bits!(self, *arg);
                let sign = iter.next_back().unwrap();
                let bits = iter
                    .chain(std::iter::repeat(sign))
                    .take(sort.0 as usize)
                    .collect();
                self.var_map.set(bit_ir, node.output, bits);
            }
            WordlevelTerm::Slice(Slice(arg, offset, sort)) => {
                let bits =
                    self.var_map.get(*arg)[*offset as usize..(*offset + sort.0) as usize].to_vec();
                self.var_map.set(bit_ir, node.output, bits);
            }
            WordlevelTerm::Redxor(Redxor(arg)) => {
                let result = bit_ir.reduce_xor(bits!(self, *arg));
                self.var_map.set_bool(bit_ir, node.output, result);
            }
            WordlevelTerm::LogicalShiftLeft(LogicalShiftLeft(args, _sort)) => {
                self.shift(bit_ir, args, node.output, false, false);
            }
            WordlevelTerm::LogicalShiftRight(LogicalShiftRight(args, _sort)) => {
                self.shift(bit_ir, args, node.output, true, false);
            }
            WordlevelTerm::ArithmeticShiftRight(ArithmeticShiftRight(args, _sort)) => {
                self.shift(bit_ir, args, node.output, true, true);
            }
            WordlevelTerm::RotateLeft(RotateLeft(args, _sort)) => {
                self.rotate(bit_ir, args, node.output, false)
            }
            WordlevelTerm::RotateRight(RotateRight(args, _sort)) => {
                self.rotate(bit_ir, args, node.output, true)
            }
            WordlevelTerm::Mul(Mul(args, sort)) => {
                self.var_map.allocate(bit_ir, node.output, sort);
                multiply(
                    bit_ir,
                    self.var_map.get(args[0]),
                    self.var_map.get(args[1]),
                    self.var_map.get(node.output),
                );
            }
            WordlevelTerm::UmulOverflow(UmulOverflow(args)) => {
                let len = self.var_map.get(args[0]).len();
                let q = bit_ir.fresh_lits(len);
                let result = multiply(
                    bit_ir,
                    self.var_map.get(args[0]),
                    self.var_map.get(args[1]),
                    &q,
                );
                self.var_map.set_bool(bit_ir, node.output, result);
            }
            WordlevelTerm::SmulOverflow(SmulOverflow(args)) => {
                let len = self.var_map.get(args[0]).len();
                let a = sext(2 * len, bits!(self, args[0]));
                let b = sext(2 * len, bits!(self, args[1]));
                let q = bit_ir.fresh_lits(2 * len);
                multiply(bit_ir, &a, &b, &q);
                let all_zero = !bit_ir.reduce_or(q[len - 1..].iter().copied());
                let all_one = bit_ir.reduce_and(q[len - 1..].iter().copied());
                let result = bit_ir.or(all_zero, all_one);
                self.var_map.set_bool(bit_ir, node.output, result);
            }
            WordlevelTerm::Udiv(_)
            | WordlevelTerm::Sdiv(_)
            | WordlevelTerm::SdivOverflow(_)
            | WordlevelTerm::Urem(_)
            | WordlevelTerm::Srem(_)
            | WordlevelTerm::Smod(_) => {
                unimplemented!()
            }
        }
    }
    pub fn lower_var(&mut self, word_ir: &mut WordIr, bit_ir: &mut BitIr, var: Var) {
        let mut queue = vec![var];
        while let Some(var) = queue.pop() {
            let node_id = word_ir.primary_def.def(var);
            let node: WordlevelNode<WordlevelTerm> = word_ir.egraph.get(node_id);
            let missing: Vec<_> = node
                .term
                .use_vars()
                .filter(|&v| !self.var_map.contains(v))
                .collect();
            if missing.is_empty() {
                self.lower_node(word_ir, bit_ir, node_id);
            } else {
                queue.push(var);
                queue.extend(missing);
            }
        }
    }
}
