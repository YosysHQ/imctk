// TODO this is currently implemented on top of Nodes, but the specific use case would be better
// served with a different representation

use imctk_ids::Id;

use crate::{
    give_take::Take,
    ir::{
        node::{
            builder::{NodeBuilder, NodeBuilderDyn},
            generic::{
                dyn_value_into_dyn_value_wrapper, DynNode, DynValue, Node, Value, ValueWrapper,
            },
            vtables::{DynNodeType, DynValueType, GenericNodeType, GenericValueType},
            NodeId,
        },
        var::{Lit, Var, VarOrLit},
    },
};

#[derive(Debug)]
enum NodeBufEntry {
    Value(NodeId),
    Node(NodeId),
    Equiv([Lit; 2]),
}

#[derive(Default)]
pub struct NodeBuf {
    nodes: super::nodes::Nodes,
    entries: Vec<NodeBufEntry>,
    values: usize,
    recycle: usize,
}

#[derive(Default)]
pub struct NodeBufVarMap {
    map: Vec<Lit>,
}

impl NodeBufVarMap {
    pub fn map_var(&self, var: Var) -> Lit {
        self.map
            .get(Var::MAX_ID_INDEX - var.index())
            .copied()
            .unwrap_or(var.as_pos())
    }
}

impl NodeBuilderDyn for NodeBuf {
    fn dyn_value(&mut self, value: Take<DynValue>) -> Lit {
        let lit = Var::from_index(Var::MAX_ID_INDEX.checked_sub(self.values).unwrap()).as_pos();

        self.entries.push(NodeBufEntry::Value(
            self.nodes
                .insert_dyn(dyn_value_into_dyn_value_wrapper(value))
                .0,
        ));

        self.values += 1;

        lit
    }

    fn dyn_node(&mut self, node: Take<DynNode>) {
        self.entries
            .push(NodeBufEntry::Node(self.nodes.insert_dyn(node).0));
    }

    fn equiv(&mut self, equiv: [Lit; 2]) {
        self.entries.push(NodeBufEntry::Equiv(equiv))
    }
}

impl NodeBuilder for NodeBuf {
    fn value<T: Value>(&mut self, value: T) -> T::Output {
        let lit = Var::from_index(Var::MAX_ID_INDEX.checked_sub(self.values).unwrap()).as_pos();
        self.entries.push(NodeBufEntry::Value(
            self.nodes.insert(ValueWrapper { value }).0,
        ));

        <T::Output as VarOrLit>::build_var_or_lit(lit, |lit| lit.var(), |lit| lit)
    }

    fn node<T: Node>(&mut self, node: T) {
        self.entries
            .push(NodeBufEntry::Node(self.nodes.insert(node).0));
    }
}

impl NodeBuf {
    pub fn drain_into_node_builder(
        &mut self,
        builder: &mut impl NodeBuilder,
        var_map: &mut NodeBufVarMap,
    ) {
        // We multiply by 2 since the builder will start allocating variables for our temporary
        // variables and we need to avoid any overlap until the very end.
        assert!(
            builder.valid_temporary_vars(self.values * 2),
            "NodeBuf uses more temporary variables than available in the target builder"
        );
        var_map.map.clear();
        if self.entries.is_empty() {
            return;
        }

        for entry in self.entries.drain(..) {
            match entry {
                NodeBufEntry::Value(node_id) => {
                    self.nodes
                        .remove_dyn_with(node_id, |node| {
                            let value_type =
                                DynNodeType(node.node_type()).wrapped_value_type().unwrap();
                            // SAFETY: we know (and dynamically verified by calling
                            // `wrapped_value_type`) that the target node is a ValueWrapper which is
                            // a transparent wrapper for a Value. We're also using the correct
                            // `ValueType` obtained from the wrapper's `NodeType`.
                            let mut value = unsafe {
                                Take::from_raw_ptr(
                                    DynValueType(value_type)
                                        .cast_mut_ptr(node.into_raw_ptr() as *mut u8),
                                )
                            };

                            let map_pol = value.dyn_apply_var_map(&mut |var| var_map.map_var(var));

                            let output_lit = builder.dyn_value(value);

                            var_map.map.push(output_lit ^ map_pol);
                        })
                        .unwrap();
                }
                NodeBufEntry::Node(node_id) => {
                    self.nodes
                        .remove_dyn_with(node_id, |mut node| {
                            node.dyn_apply_var_map(&mut |var| var_map.map_var(var));
                            builder.dyn_node(node);
                        })
                        .unwrap();
                }
                NodeBufEntry::Equiv(lits) => {
                    builder.equiv(lits.map(|lit| lit.map_var_to_lit(|var| var_map.map_var(var))));
                }
            }
        }

        // TODO improve `Nodes` memory management under node deletions and/or reimplement
        // `NodeBuf` using bump allocation
        self.recycle += 1;
        if self.recycle == 10000 {
            self.recycle = 0;
            self.nodes = Default::default();
        }
    }
}
