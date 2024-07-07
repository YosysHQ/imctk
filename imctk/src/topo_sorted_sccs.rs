//! Utilities for topological sorting and detecting strongly connected components.
use imctk_ids::{id_vec::IdVec, Id};

use crate::vec_sink::VecSink;

struct DfsEntry<D: DfsIndexMap> {
    node: D::Node,
    incoming_left: D::DfsIndex,
    lowlink: D::DfsIndex,
}

/// Temporary storage for the depth-first-search visit order (DFS index) used by [`TopoSortedSccs`].
///
/// For graphs that use dense sequential node identifiers, this is usually the provided [`IdVec`]
/// implementation.
pub trait DfsIndexMap: Default {
    /// Type used to identify nodes.
    type Node: Copy + Eq;
    /// Type used to represent the DFS index.
    ///
    /// This should be an [`Id`] type that is sufficiently large for enumerating the nodes visited
    /// during topological sorting.
    type DfsIndex: Id;

    /// Get the DFS index for a node.
    fn dfs_index(&self, node: Self::Node) -> Option<Self::DfsIndex>;

    /// Insert, update or delete the DFS index for node.
    fn set_dfs_index(&mut self, node: Self::Node, dfs_index: Option<Self::DfsIndex>);
}

impl<Node: Id, DfsIndex: Id> DfsIndexMap for IdVec<Node, Option<DfsIndex>> {
    type Node = Node;
    type DfsIndex = DfsIndex;

    #[inline]
    fn dfs_index(&self, node: Node) -> Option<Self::DfsIndex> {
        *self.get(node)?
    }

    #[inline]
    fn set_dfs_index(&mut self, node: Node, dfs_index: Option<Self::DfsIndex>) {
        self.grow_for_key(node);
        self[node] = dfs_index
    }
}

/// Performs topological sorting and detects strongly connected components (SCCs).
///
/// This implements Tarjan's linear time strongly connected components algorithm. It uses a
/// non-recursive implementation to support arbitrary large graphs without the risk of causing stack
/// overflows.
#[derive(Default)]
pub struct TopoSortedSccs<D: DfsIndexMap> {
    dfs_index: D,
    dfs_stack: Vec<DfsEntry<D>>,
    incoming_iter_stack: Vec<D::Node>,
    component_stack: Vec<D::Node>,
}

impl<D: DfsIndexMap> TopoSortedSccs<D> {
    /// Checks whether a node and its predecessors have already been processed.
    pub fn processed(&self, node: D::Node) -> bool {
        self.dfs_index.dfs_index(node).is_some()
    }

    /// Processes all newly discovered strongly connected components for a set of nodes and all their
    /// predecessors. Strongly connected components are emitted in topological order.
    ///
    /// SCCs already processed in previous calls to [`process`][Self::process] or
    /// [`process_one`][Self::process_one] will not be processed again. Note that any already
    /// processed SCC must precede any newly discovered SCC in any valid topological order.
    ///
    /// The `nodes` parameter defines the set of nodes to process (including their predecessors).
    ///
    /// The `incoming` parameter is a callback that defines the graph to operatre on. It has to
    /// produce the incoming neighbors for a given node by appending them to the provided
    /// [`VecSink`].
    ///
    /// The `component_callback` parameter is invoked, in topological order, for every newly
    /// discovered SCC.
    pub fn process(
        &mut self,
        nodes: impl IntoIterator<Item = D::Node>,
        mut incoming: impl FnMut(D::Node, VecSink<D::Node>),
        mut component_callback: impl FnMut(&Self, &[D::Node]),
    ) {
        for node in nodes {
            self.process_one_inner(node, &mut incoming, &mut component_callback)
        }
    }

    /// Processes all newly discovered components for a given node and all its predecessors.
    /// Strongly connected components are emitted in topological order.
    ///
    /// This is equivalent to calling [`process`][Self::process] with a nodes parameter producing
    /// the single given node.
    pub fn process_one(
        &mut self,
        node: D::Node,
        mut incoming: impl FnMut(D::Node, VecSink<D::Node>),
        mut component_callback: impl FnMut(&Self, &[D::Node]),
    ) {
        self.process_one_inner(node, &mut incoming, &mut component_callback)
    }

    fn process_one_inner(
        &mut self,
        mut node: D::Node,
        incoming: &mut impl FnMut(D::Node, VecSink<D::Node>),
        component_callback: &mut impl FnMut(&Self, &[D::Node]),
    ) {
        if self.dfs_index.dfs_index(node).is_some() {
            return;
        }

        let mut next_dfs_index = 0;

        'dfs_search: loop {
            // At this point we're visiting the node for the first time during
            // the DFS search

            // We record the timestamp of when we first visited the node as the
            // dfs_index
            let mut lowlink = Id::from_id_index(next_dfs_index);
            next_dfs_index += 1;
            self.dfs_index.set_dfs_index(node, Some(lowlink));

            // And we add the node to the component stack where it will remain
            // until all nodes of the component containing this node are popped
            self.component_stack.push(node);

            // We collect all incoming neighbors on the incoming_iter_stack
            let before = self.incoming_iter_stack.len();
            incoming(node, VecSink::new(&mut self.incoming_iter_stack));
            let mut incoming_iter_left = self.incoming_iter_stack.len() - before;

            // Then we start iterating over the incoming neighbors of this node.
            loop {
                if incoming_iter_left > 0 {
                    let incoming = self.incoming_iter_stack.pop().unwrap();
                    incoming_iter_left -= 1;

                    if let Some(incoming_index) = self.dfs_index.dfs_index(incoming) {
                        // The textbook version guards this update with a check whether the incoming
                        // neighbor is still on the component stack. If the incoming neighbor was
                        //  already visisted but is not on the component stack, it must be part of
                        // an already emitted SCC. We can avoid this check by setting the DFS index
                        // of all nodes in a SCC to Id::MAX_ID when the SCC is emitted.
                        lowlink = lowlink.min(incoming_index);
                    } else {
                        // If the incoming neighbor wasn't visted yet, the DFS recurses into the
                        // neighbor to process all predecessors.

                        // We save the state for this node and make the incoming neighbor the
                        // current node.
                        self.dfs_stack.push(DfsEntry {
                            node,
                            incoming_left: Id::from_id_index(incoming_iter_left),
                            lowlink,
                        });
                        node = incoming;

                        // This gets us to the section corresponding to the function entry
                        // in the recursive version
                        continue 'dfs_search;
                    }
                } else {
                    // When we processed all predecessors, i.e. when we visited the complete DFS
                    // subtree rooted at the current node, we first check whether the current
                    // node is a SCC root
                    //
                    // (Why this check identifies SCC roots is out of scope for this comment,
                    // see other material on Tarjan's SCC algorithm)
                    if self.dfs_index.dfs_index(node) == Some(lowlink) {
                        let mut current = self.component_stack.len();
                        loop {
                            current -= 1;
                            if self.component_stack[current] == node {
                                break;
                            }
                        }

                        component_callback(self, &self.component_stack[current..]);

                        // See the comment for the lowlink update above for why we set the dfs_index
                        // of emitted SCC nodes to Id::MAX
                        for &node in &self.component_stack[current..] {
                            self.dfs_index.set_dfs_index(node, Some(Id::MAX_ID))
                        }

                        self.component_stack.truncate(current);
                    }

                    // After checking for a completed SCC the DFS either continues the search at the
                    // parent node or we finished the full DFS search from the seed node and can
                    // return to the caller.
                    let Some(dfs_top) = self.dfs_stack.pop() else {
                        return;
                    };

                    node = dfs_top.node;
                    incoming_iter_left = dfs_top.incoming_left.id_index();

                    // The parent's lowlink is updated when returning
                    lowlink = lowlink.min(dfs_top.lowlink);

                    // Continue checking the remaining predecessors of the parent node.
                }
            }
        }
    }
}
