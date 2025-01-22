The Whisk Formal System
=======================

.. default-role:: code

This chapter introduces the Whisk formal system.
It consists of a language used for modeling system behavior, specifying properties to check and to represent facts inferred by the model checker, together with a set of inference rules.

.. todo:: some early examples

Objects
-------

In Whisk, we use the term "object" to refer to any value a variable could take or, equivalently, any value that can be a member of a set.
In a pure set theory the only objects would be sets, but in Whisk we also consider the following list of non-set objects:

* sets
* Booleans
* options
* natural numbers
* tuples
* infinite sequences
* functions defined on sets
* opaque named IDs
* constructors, which pair an ID with a wrapped value

.. todo:: some of these kinds should probably use a constructor encoding instead of being builtins

.. note::

   When defining the formal semantics of Whisk, we will also encounter certain non-object values, e.g. optionally defined objects or (higher-order) functions with all objects as domain.
   We can only refer to such values from the meta-level we use to specify Whisk and not from within the formal system itself.

Abstract Syntax
---------------

In this section we will specify Whisk's abstract syntax and a textual representation for it.

Whisk's abstract syntax trees have the form of the following recursive datatype:

.. code:: rust

   enum Term {
      Var(String),
      Id(String),
      Op(Op),
   }

   struct Op {
      name: String,
      arguments: Vec<Block>,
   }

   struct Block {
      bound: Vec<String>,
      body: Term,
   }

The textual representation is given by the following PEG grammar.

.. code:: peg

   Term = NAME                     -- Term::Var($1)
        | "'" NAME                 -- Term::Id($1)
        | Op                       -- Term::Op($1)

   Op = "(" NAME Block* ")"        -- Op { name: $1, arguments: $2 }
      | ":" NAME                   -- Op { name: $1, arguments: vec![] }

   Block = "[" NAME* ":" Term "]"  -- Block { bound: $1, body: $1 }
         | Term                    -- Block { bound: vec![], body: $1 }


.. todo:: specify the used tokenization

.. note::

   This is a very simple textual representation of the *abstract* syntax and not the Whisk surface language.
   It is used within the specification since translating between the abstract syntax and the surface language is neither trivial nor lossless.

Terms are built out of variables, ids and operators.

Variables are named and the textual representation is the variable name itself:

* `x`
* `y`
* `foo`
* `bar`
* etc.

Ids are distinguished by name but are completely opaque from within Whisk.
They are represented by prefixing their name with a single quote.

* `'x`
* `'foo`
* etc.

Operator invocations are represented using parantheses.
Within the parantheses they start with an identifier followed by any number of arguments, including zero arguments for constant operators.
When an operator is used without any arguments, instead of using parantheses the name can be prefixed by a colon instead.

* `(false)` or `:false`
* `(abs x)`
* `(min x y)`
* `(maj a b c)`
* `(eq f g)`
* `(not (not x))`
* `(max lower (min upper x))`
* etc.

Each argument of an operator is a block.
A block can be a raw subterm, as for the examples above, or it can bind variables within the scope of a body subterm.
If it does bind variables, the list of bound variables precedes the body subterm, separated by a colon and the full block is enclosed in square brackets.

* `(forall [x : (eq x x)])`,
* `(exists [x : (eq (mul x x) (2))])`
* `(fun (Int) (Int) (Int) [a b c : (add a (sub b c))])`
* `(set (Int) [x : (gt x 0)] [x : (mul x x)])`
* etc.

Blocks with bound variables are similar to what other languages might call lambda-abstraction, closures or also blocks.

In Whisk, a block on its own isn't a term, it is merely part of the syntax for operator invocation.
Thus a block doesn't have to represent an object-level value.
This is done so that a block's bound variables can have an unrestricted domain ranging over all object-level values,
Whisk cannot allow this for object-level functions, but at the same time it is a requirement for the predicate logic quantifiers.


.. todo:: introduce the following graphical notation

.. graphviz::
   :caption: `(forall [x y : (implies (lt x y) (exists [a : (and (lt x a) (lt a y))]))])`

   digraph {
      graph [ranksep=0.3];
      node [margin=0.05,width=0,height=0,shape=record]
      edge [dir=none];
      subgraph cluster_n1_0 { graph [color=crimson];
         n4 [shape=plain, fontcolor=crimson, label="x"];
         n5 [shape=plain, fontcolor=crimson, label="y"];
         n3 [label="lt|<0>|<1>"];
         n3:0:s -> n4;
         n3:1:s -> n5;
         subgraph cluster_n6_0 { graph [color=dodgerblue4];
            n9 [shape=plain, fontcolor=crimson, label="x"];
            n10 [shape=plain, fontcolor=dodgerblue4, label="a"];
            n8 [label="lt|<0>|<1>"];
            n8:0:s -> n9;
            n8:1:s -> n10;
            n12 [shape=plain, fontcolor=dodgerblue4, label="a"];
            n13 [shape=plain, fontcolor=crimson, label="y"];
            n11 [label="lt|<0>|<1>"];
            n11:0:s -> n12;
            n11:1:s -> n13;
            n7 [label="and|<0>|<1>"];
            n7:0:s -> n8;
            n7:1:s -> n11;
         }
         n6 [label="exists|<0>a"];
         n6:0:s -> n7[color=dodgerblue4];
         n2 [label="implies|<0>|<1>"];
         n2:0:s -> n3;
         n2:1:s -> n6;
      }
      n1 [label="forall|<0>x y"];
      n1:0:s -> n2[color=crimson];
   }

.. todo:: after defining primitives, make sure all examples use these correctly or add a note that they don't

Semantics
---------

This section specifies the semantics of the Whisk term language.
To give meaning to all Whisk terms, we recursively construct an interpretation of each term in some target theory.

There are many suitable target theories for this, and the best choice may vary depending on the use case.
To remain flexible, we will describe an interpretation as a many-sorted first-order theory.
This can then be either axiomatized or interpreted in some other theory, e.g. the simply typed lambda calculus.

Sorts
*****

Our theory will use values of the following sorts:

*  Sorts representing opaque identifiers

   *  `Var` the sort of variable names
   *  `Op` the sort of operator names
   *  `Id` the sort of ids

*  Sorts representing object-level values or higher-order functions on object-level values

   *  `Object` the sort of object-level values
   *  `ClosedTerm` the sort of object-level values and a distinct `undef` value

      *  This comes with a partial order with every object-level value above `undef` and every pair of distinct object-level values being incomparable.

   *  `ClosedBlock[n]` the sort of definable n-ary partial functions on object-level values

      *  This is partially ordered using the subset order of their domains

   *  `ClosedOp[n_1, ..., n_k]` the sort of definable monotone maps from `ClosedBlock[n_1] x ... x ClosedBlock[n_k]` to `ClosedTerm`

*  Sorts for dependencies on free variables

   *  `Env` the sort of variable environments (assignments of object-level values to all variables)
   *  `OpenTerm` the sort of `ClosedTerms` with an additional dependency on a variable environment
   *  `OpenBlock[n]` the sort of `ClosedBlock[n]` with an additionally dependency on a variable environment
   *  `OpenOp[n_1, ..., n_k]` the sort of `ClosedOp[n_1, ..., n_k]` with an additionally dependency on a variable environment

Function Symbols
****************

For the interpretation, we will make use of the following function symbols:

*  `var : Var -> OpenTerm`

   An open term that represents the value of a given variable.

*  `const : Object -> ClosedTerm`

   A closed term with a constant object-level value.

*  `openTerm : ClosedTerm -> OpenTerm`

   The inclusion map of the closed terms in the open terms.

*  `id : Id -> Object`

   The inclusion map of the opaque ids in the object-level values.

*  `bind[n] : Var^n x OpenTerm -> OpenBlock[n]`

   Constructor for a block given a term and a number of variables to bind.

*  `applyOpen[n_1, ..., n_k] : OpenOp[n_1, ..., n_k] x OpenBlock[n_1] x ... x OpenBlock[n_k] -> OpenTerm`

   Application of an operator to open blocks, yields an open term representing the result of the operator.

*  `opDef[n_1, ..., n_k] : Op -> ClosedOp[n_1, ..., n_k]`

   Look up an operator definition for a given signature.

*  `openOp : ClosedOp -> OpenOp`

   The inclusion map of the closed ops in the open ops.

Interpretation
**************

The interpretation of a Whisk term is then obtained by recursion on the Whisk abstract syntax, yielding a first-order term of sort `OpenTerm`.

*  `TERM[ V ] := var(VAR[ V ])`
*  `TERM[ 'I ] := const(id(ID[ 'I ]))`
*  `TERM[ :const ] := TERM[ (const) ]`
*  `TERM[ (O B_1 ... B_k) ] := applyOpen(openOp(opDef(OP[ O ])), BLOCK[ B_1 ], ..., BLOCK[ B_k ])`
*  `BLOCK[ T ] := BLOCK[ [ : T ] ]`
*  `BLOCK[ [ V_1 ... V_k : T ] ] := bind(VAR[ V_1 ], ..., VAR[ V_k ], TERM[ T ])`
*  `VAR[ V ] : Var` a unique distinct constant for every `V`
*  `ID[ 'I ] : Id` a unique distinct constant for every `I`

Axiomatization
**************

To axiomatize the theory we introduce the following additional function symbols

*  `get : Env x Var -> Object` look up a variable value
*  `update : Env x Var x Object -> Env` update a variable value
*  `undef : ClosedTerm` the undefined term
*  `const : Object -> ClosedTerm` a term with a constant value
*  `closeTerm : Env x OpenTerm -> ClosedTerm` close the term over an environment
*  `closeBlock[n] : Env x OpenBlock[n] -> ClosedBlock[n]` close the block over an environment
*  `openBlock[n] : ClosedBlock[n] -> OpenBlock[n]` inclusion of the closed blocks in the open blocks
*  `substitute[n] : ClosedBlock[n] x Object^n -> ClosedTerm` substitute values for the bound variables in a closed block
*  `apply[n_1, ..., n_k] : ClosedOp[n_1, ..., n_k] x ClosedBlock[n_1] x ... x ClosedBlock[n_k] -> ClosedTerm`

The axioms of the theory are:

Environments:

* `(forall v, get(e_1, v) = get(e_2, v)) -> (e_1 = e_2)` extensionality for environments
* `get(update(e, v, o), v) = o` get last updated variable
* `v_1 != v_2 -> get(update(e, v_1, o), v_2) = get(e, v_2)` get other variable

Terms:

* `(forall e, closeTerm(e, t_1) = closeTerm(e, t_2)) -> (t_1 = t_2)` extensionality for open terms
* `closeTerm(e, openTerm(t)) = t` environment independence of closed terms + inclusion
* `closeTerm(e, var(v)) = const(get(e, v))` definition of variable terms
* `const o != undef` defined and undefined values are distinct
* `t = undef \\/ (exists o, t = const o)` every closed term that isn't undefined is a constant object-level value

Blocks:

* `(forall e, closeBlock(e, b_1) = closeBlock(e, b_2)) -> (b_1 = b_2)` extensionality for open blocks
* `closeBlock(e, openBlock(b)) = b` environment independence of closed blocks + inclusion
* `(forall o_1 .. o_n, substitute(b_1, o_1, ..., o_n) = substitute(b_2, o_1, ..., o_n)) -> b_1 = b_2` extensionality for closed blocks
* `substitute(closeBlock(e, bind(v_1, ..., v_n, b)), get(e, v_1), ..., get(e, v_n)) = closeTerm(e, b)`
* .. todo:: double check abstraction and substitution is fully defined

Operators:

* `(forall e, closeOp(e, o_1) = closeOp(e, o_2)) -> (o_1 = o_2)` extensionality for open operators
* `closeTerm(e, applyOpen(o, b_1, ..., b_n)) = apply(closeOp(e), closeBlock(e, b_1), ...,  closeBlock(e, b_n))` operator application is pointwise w.r.t the environment
* `(forall b_1 .. b_n, apply(o_1, b_1, ..., b_n) = apply(o_2, b_1, ..., b_n)) -> o_1 = o_2` extensionality for closed operators
* `b_1 <= b_1' /\\ ... /\\ b_n <= b_n' -> apply(o_1, b_1, ..., b_n) <= apply(o_1, b_1', ..., b_n')` monotonicity wrt definedness

Lambda-Calculus Interpretation
******************************

.. todo:: mapping to simply typed lambda calculus

   Map every sort to a type, every indexed sort to a family of types.

.. todo:: do we need other mappings at this point?

.. todo:: use latex formulas instead of code for this section

Builtin Operators
-----------------

.. todo:: figure out section structure, include mapping from syntax to operator calls, operator definition pseudocode

* Functional operators
   * Equality `<lhs> = <rhs>`
   * Boolean operators
   * Set operations
* Case splitting / optionals
   * `if <cond> then <true-value>`
      * Here `<true-value>` must only be well-formed assuming `<cond>`
      * If `<cond>` is `true` then equal to `some(<true-value>)` else equal to `none`
   * `<optional> else <false-value>`
      * Here `<false-value>` must only be well-formed assuming `<optional> = none`
      * if `<optional>` is `some(<true-value>)` then equal `<true-value>` else equal to `<false-value>`
   * This means in `if <cond> then <true-value> else <false-value>` the requirements are that `<true-value>` is well-formed assuming `<cond>` and `<false-value>` assuming `!<cond>`
* Quantifiers
   * `all x => P(x)` universal quantification
      * `all x : D => P(x)` short for `all x => x : D implies P(x)`
   * `ex x => P(x)` existential quantification
      * `ex x : D => P(x)` short for `ex x => x : D and P(x)`
   * `ex uniq x => P(x)` unique existential quantification
      * `ex uniq x : D => P(x)` short for `ex uniq x => x : D and P(x)`
* Function abstraction
   * `fun x : D => F(x)` lambda term
* Set builders
   * `{ F(x) for x : D where P(x) }` combined
   * `{ F(x) for x : D }` map only
      * short for `{ F(x) for x : D where true }`
   * `{ x : D where P(x) }` filter only
      * short for `{ x for x : D where true }`
* Choice operator
   * `some x => P(x)` nondeterministic choice
      * `some x : D => P(x)` short for `some x => x : D and P(x)`
   * `the x => P(x)` unique choice
      * `the x : D => P(x)` short for `the x => x : D and P(x)`
* Short circuiting Boolean operators
   * `a && b` defined to be `if a then b else false`
   * `a || b` defined to be `if a then true else b`
   * `a --> b` defined to be `if a then b else true`


Rules
-----

.. todo:: write section

Egraphs
-------

.. todo:: figure out section structure

* Interpreting a term in context gives us a dependent value, thus in our egraph the eclasses represent dependent values
* Each enode defines a dependent value in terms of other dependent values
   * This mirrors the inductive structure of term interpretation: enodes correspond to operator interpretations and enode children to the subterm interpretations that are the arguments to the operator interpretation
   * When importing a term AST into the egraph, we add all enodes needed to fully define the interpretation of the term
