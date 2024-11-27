use std::collections::HashMap;

use flussab_btor2::{
    btor2::{self, BinaryOp, Line, NodeVariant, TernaryOp, UnaryOp, ValueVariant},
    InnerParseError, Parser,
};
use imctk_lit::Var;
use num_bigint::{BigInt, BigUint};
use num_traits::Num;

use crate::{ir::WordIr, wordlevel::*};

struct Node {
    value: Var,
    sort: Sort,
}

struct State {
    next: Var,
    init: Var,
    next_set: bool,
    init_set: bool,
}

#[derive(Default)]
pub struct BtorImporter {
    sorts: HashMap<btor2::NodeId, Sort>,
    nodes: HashMap<btor2::NodeId, Node>,
    states: HashMap<btor2::NodeId, State>,
}

impl BtorImporter {
    fn sort(&self, id: btor2::NodeId) -> Sort {
        self.sorts[&id].clone()
    }
    fn handle_const(&mut self, ir: &mut WordIr, const_value: btor2::Const, sort: Sort) -> Var {
        match const_value {
            btor2::Const::Binary(binary_const) => {
                let value = BigUint::from_str_radix(&binary_const, 2).unwrap();
                ir.big_constant(&value, sort)
            }
            btor2::Const::Decimal(decimal_const) => {
                let value = BigInt::from_str_radix(&decimal_const, 10).unwrap();
                if value.sign() == num_bigint::Sign::Minus {
                    // TODO: don't emit const-foldable IR
                    let zero = ir.constant(0, sort.clone());
                    let x = ir.big_constant(value.magnitude(), sort.clone());
                    ir.term(WordlevelTerm::Sub(Sub([zero, x], sort)))
                } else {
                    ir.big_constant(value.magnitude(), sort)
                }
            }
            btor2::Const::Hex(hex_const) => {
                let value = BigUint::from_str_radix(&hex_const, 16).unwrap();
                ir.big_constant(&value, sort)
            }
            btor2::Const::One => ir.constant(1, sort),
            btor2::Const::Ones => {
                let zero = ir.constant(0, sort.clone());
                ir.term(WordlevelTerm::Not(Not(zero, sort)))
            }
            btor2::Const::Zero => ir.constant(0, sort),
        }
    }
    fn handle_unary(
        &mut self,
        ir: &mut WordIr,
        op: UnaryOp,
        arg: Var,
        input_sort: Sort,
        output_sort: Sort,
    ) -> WordlevelTerm {
        match op {
            UnaryOp::Uext(extra) => {
                let extra: u32 = extra.try_into().unwrap();
                assert_eq!(output_sort, Sort(input_sort.0 + extra));
                WordlevelTerm::Zext(Zext(arg, output_sort))
            }
            UnaryOp::Sext(extra) => {
                let extra: u32 = extra.try_into().unwrap();
                assert_eq!(output_sort, Sort(input_sort.0 + extra));
                WordlevelTerm::Sext(Sext(arg, output_sort))
            }
            UnaryOp::Slice(u, l) => {
                let u: u32 = u.try_into().unwrap();
                let l: u32 = l.try_into().unwrap();
                assert!(l <= u);
                assert!(u < input_sort.0);
                assert_eq!(output_sort, Sort(u - l + 1));
                WordlevelTerm::Slice(Slice(arg, l, output_sort))
            }
            UnaryOp::Not => {
                assert_eq!(input_sort, output_sort);
                WordlevelTerm::Not(Not(arg, output_sort))
            }
            UnaryOp::Inc => {
                assert_eq!(input_sort, output_sort);
                let c1 = ir.constant(1, output_sort.clone());
                WordlevelTerm::Add(Add([arg, c1].into(), output_sort))
            }
            UnaryOp::Dec => {
                assert_eq!(input_sort, output_sort);
                let c1 = ir.constant(1, output_sort.clone());
                WordlevelTerm::Sub(Sub([arg, c1], output_sort))
            }
            UnaryOp::Neg => {
                assert_eq!(input_sort, output_sort);
                let c0 = ir.constant(0, output_sort.clone());
                WordlevelTerm::Sub(Sub([c0, arg], output_sort))
            }
            UnaryOp::Redand => {
                assert_eq!(output_sort, Sort(1));
                let cn1 = self.handle_const(ir, btor2::Const::Ones, input_sort.clone());
                WordlevelTerm::Eq(Eq([arg, cn1].into()))
            }
            UnaryOp::Redor => {
                assert_eq!(output_sort, Sort(1));
                let c0 = ir.constant(0, input_sort.clone());
                let e = ir.term(WordlevelTerm::Eq(Eq([arg, c0].into())));
                WordlevelTerm::Not(Not(e, Sort(1)))
            }
            UnaryOp::Redxor => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Redxor(Redxor(arg))
            }
        }
    }
    fn check_binary(&mut self, input_sorts: &[Sort; 2], output_sort: &Sort) {
        assert_eq!(input_sorts[0], *output_sort);
        assert_eq!(input_sorts[1], *output_sort);
    }
    fn handle_binary(
        &mut self,
        ir: &mut WordIr,
        op: BinaryOp,
        args: [Var; 2],
        input_sorts: [Sort; 2],
        output_sort: Sort,
    ) -> WordlevelTerm {
        match op {
            BinaryOp::And => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::And(And(args.into(), output_sort))
            }
            BinaryOp::Or => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Or(Or(args.into(), output_sort))
            }
            BinaryOp::Xor => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Xor(Xor(args.into(), output_sort))
            }
            BinaryOp::Nand => {
                self.check_binary(&input_sorts, &output_sort);
                let a = ir.term(WordlevelTerm::And(And(args.into(), output_sort.clone())));
                WordlevelTerm::Not(Not(a, output_sort))
            }
            BinaryOp::Nor => {
                self.check_binary(&input_sorts, &output_sort);
                let a = ir.term(WordlevelTerm::Or(Or(args.into(), output_sort.clone())));
                WordlevelTerm::Not(Not(a, output_sort))
            }
            BinaryOp::Xnor => {
                self.check_binary(&input_sorts, &output_sort);
                let a = ir.term(WordlevelTerm::Xor(Xor(args.into(), output_sort.clone())));
                WordlevelTerm::Not(Not(a, output_sort))
            }
            BinaryOp::Add => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Add(Add(args.into(), output_sort))
            }
            BinaryOp::Sub => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Sub(Sub(args, output_sort))
            }
            BinaryOp::Mul => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Mul(Mul(args.into(), output_sort))
            }
            BinaryOp::Iff => {
                assert_eq!(input_sorts[0], Sort(1));
                assert_eq!(input_sorts[1], Sort(1));
                assert_eq!(output_sort, Sort(1));
                let a = ir.term(WordlevelTerm::Xor(Xor(args.into(), Sort(1))));
                WordlevelTerm::Not(Not(a, Sort(1)))
            }
            BinaryOp::Implies => {
                assert_eq!(input_sorts[0], Sort(1));
                assert_eq!(input_sorts[1], Sort(1));
                assert_eq!(output_sort, Sort(1));
                let a = ir.term(WordlevelTerm::Not(Not(args[0], Sort(1))));
                WordlevelTerm::Or(Or([a, args[1]].into(), Sort(1)))
            }
            BinaryOp::Eq => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Eq(Eq(args.into()))
            }
            BinaryOp::Neq => {
                assert_eq!(output_sort, Sort(1));
                let a = ir.term(WordlevelTerm::Eq(Eq(args.into())));
                WordlevelTerm::Not(Not(a, Sort(1)))
            }
            BinaryOp::Ugt => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Ugt(Ugt(args))
            }
            BinaryOp::Sgt => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Sgt(Sgt(args))
            }
            BinaryOp::Ugte => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Uge(Uge(args))
            }
            BinaryOp::Sgte => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Sge(Sge(args))
            }
            BinaryOp::Ult => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Ugt(Ugt([args[1], args[0]]))
            }
            BinaryOp::Slt => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Sgt(Sgt([args[1], args[0]]))
            }
            BinaryOp::Ulte => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Uge(Uge([args[1], args[0]]))
            }
            BinaryOp::Slte => {
                assert_eq!(output_sort, Sort(1));
                WordlevelTerm::Sge(Sge([args[1], args[0]]))
            }
            BinaryOp::Concat => {
                assert!(output_sort.0 == input_sorts[0].0 + input_sorts[1].0);
                WordlevelTerm::Concat(Concat(args, output_sort))
            }
            BinaryOp::Sll => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::LogicalShiftLeft(LogicalShiftLeft(args, output_sort))
            }
            BinaryOp::Srl => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::LogicalShiftRight(LogicalShiftRight(args, output_sort))
            }
            BinaryOp::Sra => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::ArithmeticShiftRight(ArithmeticShiftRight(args, output_sort))
            }
            BinaryOp::Rol => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::RotateLeft(RotateLeft(args, output_sort))
            }
            BinaryOp::Ror => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::RotateRight(RotateRight(args, output_sort))
            }
            BinaryOp::Udiv => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Udiv(Udiv(args, output_sort))
            }
            BinaryOp::Sdiv => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Sdiv(Sdiv(args, output_sort))
            }
            BinaryOp::Smod => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Smod(Smod(args, output_sort))
            }
            BinaryOp::Urem => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Urem(Urem(args, output_sort))
            }
            BinaryOp::Srem => {
                self.check_binary(&input_sorts, &output_sort);
                WordlevelTerm::Srem(Srem(args, output_sort))
            }
            BinaryOp::Uaddo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::UaddOverflow(UaddOverflow(args.into()))
            }
            BinaryOp::Saddo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::SaddOverflow(SaddOverflow(args.into()))
            }
            BinaryOp::Sdivo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::SdivOverflow(SdivOverflow(args))
            }
            BinaryOp::Umulo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::UmulOverflow(UmulOverflow(args.into()))
            }
            BinaryOp::Smulo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::SmulOverflow(SmulOverflow(args.into()))
            }
            BinaryOp::Usubo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::UsubOverflow(UsubOverflow(args))
            }
            BinaryOp::Ssubo => {
                assert!(input_sorts[0] == input_sorts[1]);
                WordlevelTerm::SsubOverflow(SsubOverflow(args))
            }
            BinaryOp::Read => unimplemented!(),
        }
    }
    fn handle_ternary(
        &mut self,
        _ir: &mut WordIr,
        op: TernaryOp,
        args: [Var; 3],
        input_sorts: [Sort; 3],
        output_sort: Sort,
    ) -> WordlevelTerm {
        match op {
            TernaryOp::Ite => {
                assert_eq!(input_sorts[0], Sort(1));
                assert_eq!(input_sorts[1], output_sort);
                assert_eq!(input_sorts[2], output_sort);
                WordlevelTerm::Ite(Ite {
                    cond: args[0],
                    then: args[1],
                    els: args[2],
                    sort: output_sort,
                })
            }
            TernaryOp::Write => unimplemented!(),
        }
    }
    fn handle_value(&mut self, ir: &mut WordIr, value: btor2::Value, id: btor2::NodeId) -> Var {
        let sort = self.sort(value.sort);
        match value.variant {
            ValueVariant::Const(const_value) => self.handle_const(ir, const_value, sort),
            ValueVariant::Input => {
                let id = InputId(id.0.get().try_into().unwrap());
                ir.term(WordlevelTerm::Input(Input(id, sort)))
            }
            ValueVariant::State => {
                let next = ir.union_find.fresh_atom();
                let init = ir.union_find.fresh_atom();
                self.states.insert(
                    id,
                    State {
                        next,
                        init,
                        next_set: false,
                        init_set: false,
                    },
                );
                ir.term(WordlevelTerm::Reg(Reg { next, init, sort }))
            }
            ValueVariant::Op(btor2::Op::Unary(unary_op, arg)) => {
                let node = &self.nodes[&arg];
                let term = self.handle_unary(ir, unary_op, node.value, node.sort.clone(), sort);
                ir.term(term)
            }
            ValueVariant::Op(btor2::Op::Binary(binary_op, args)) => {
                let nodes = args.map(|id| &self.nodes[&id]);
                let term = self.handle_binary(
                    ir,
                    binary_op,
                    nodes.map(|n| n.value),
                    nodes.map(|n| n.sort.clone()),
                    sort,
                );
                ir.term(term)
            }
            ValueVariant::Op(btor2::Op::Ternary(ternary_op, args)) => {
                let nodes = args.map(|id| &self.nodes[&id]);
                let term = self.handle_ternary(
                    ir,
                    ternary_op,
                    nodes.map(|n| n.value),
                    nodes.map(|n| n.sort.clone()),
                    sort,
                );
                ir.term(term)
            }
        }
    }
    fn handle_node(&mut self, ir: &mut WordIr, node: btor2::Node) {
        match node.variant {
            NodeVariant::Sort(sort) => {
                match sort {
                    btor2::Sort::BitVec(bits) => self
                        .sorts
                        .insert(node.id, Sort(bits.get().try_into().unwrap())),
                    btor2::Sort::Array(_) => unimplemented!(),
                };
            }
            NodeVariant::Value(value) => {
                let sort = self.sort(value.sort);
                let value = self.handle_value(ir, value, node.id);
                self.nodes.insert(node.id, Node { value, sort });
            }
            NodeVariant::Assignment(assignment) => {
                let state = self.states.get_mut(&assignment.state).unwrap();
                let rhs = self.nodes[&assignment.value].value;
                match assignment.kind {
                    btor2::AssignmentKind::Init => {
                        assert!(!std::mem::replace(&mut state.init_set, true));
                        ir.union_find.union([state.init, rhs]);
                    }
                    btor2::AssignmentKind::Next => {
                        assert!(!std::mem::replace(&mut state.next_set, true));
                        ir.union_find.union([state.next, rhs]);
                    }
                }
            }
            NodeVariant::Output(output) => todo!(),
        }
    }
    pub fn import(
        mut self,
        ir: &mut WordIr,
        file: impl std::io::Read,
    ) -> Result<(), Box<InnerParseError>> {
        let mut parser = Parser::from_read(file, Default::default())?;
        while let Some(line) = parser.next_line()? {
            if let Line::Node(node) = line {
                self.handle_node(ir, node);
            }
        }
        Ok(())
    }
}
