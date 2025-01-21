The Whisk Formal System
=======================

.. default-role:: code

This chapter introduces the Whisk formal system.
It consists of a language used for modeling system behavior, specifying properties to check and to represent facts inferred by the model checker, together with a set of inference rules.

.. todo:: some early examples

Values
------

As Whisk is based on set theory, sets play an important role.
Unlike a pure set theory, where there are only sets, Whisk also supports non-set values.

It supports the following kinds of values:

* sets
* Booleans
* options
* natural numbers
* tuples
* infinite sequences
* functions
* opaque named IDs
* constructors, which pair an ID with a wrapped value

.. todo:: some of these kinds should probably use a constructor encoding instead of being builtins

Terms
-----

.. note::

   From here on we will use Haskell like pseudocode to avoid ambiguities.
   Differences from actuall Haskell are that we assume `data` defines inductive datatypes not co-inductive datatypes and that we will use functions that can quantify over infinite types like `forall :: (a -> Bool) -> Bool`.

To specify the terms of the Whisk language, we first define the abstract term grammar as an inductive datatype:

.. code:: haskell

   data Term = Lit Literal
             | Var Name
             | Operator Name [Operand]

   data Operand = Subterm Term
                | Bind Name
                | Token OperandToken

   data Literal = {- ... -}
   data OperandToken = {- ... -}
   data Name = {- ... -}

We give meaning to the terms by specifying an interpretation function `interpretTerm` that assigns values to terms.
Not every term of our grammar will have a well-defined value.
Instead of having to pick a placeholder value or leaving the value of such terms indeterminate, we will make interpretation a partial mapping to values, i.e. the `interpretTerm` function should ultimately produce an `Option Value`.

In general, it is also not possible to interpret a term in isolation as the value can depend on an *environment*, specifying the values of free variables, and on a set of *operator definitions*.
For now, we will defer details on how operators are defined and use an opaque type `OperatorDefs` to represent them.


.. code:: haskell

   type Value = {- ... -}
   type Env = Name -> Value

   type OperatorDefs = {- ... -}


While the value of a term depends on both the environment and the operator definitions, they are used in different ways.
When interpreting a single given term, we usually keep the operator definitions fixed but will often vary the environment.

As such, for the `interpretTerm` function, we will order the arguments starting with the operator definitions, followed by the term and finishing with the environment:


.. code:: haskell

   interpretTerm :: OperatorDefs -> Term -> Env -> Maybe Value

Using partial application of the `interpretTerm` function we can view the interpretation of a term as a map from operator definitions and a term to an *environment dependent value*.
The concept of an environment dependent value, or *dependent value* for short, will come up repeatedly, so we will define a type alias for it:

.. code:: haskell

   type DepValue = Env -> Maybe Value

   interpretTerm :: OperatorDefs -> Term -> DepValue

At this point we can define the interpretation of literals and variables.
The value of a literal doesn't depend on any operator definitions and doesn't include free variables.
This means we can delegate the interpretation of literals to a separate `interpretLiteral :: Literal -> Value`.
The value of a variable can be looked up in the environment but also doesn't depend on any operators.

.. code:: haskell

   interpretTerm :: OperatorDefs -> Term -> DepValue
   interpretTerm _ (Lit l)                  = const (Just (interpretLiteral l))
   interpretTerm _ (Var name)               = \env -> Just (env  name)
   interpretTerm _ (Operator name operands) = {- ... -}

To be able to define the interpretation of operators, we will first define the concept of an operator fingerprint which captures the structure and top-level syntax of an operator but doesn't include the actual subterms or bound variable names:

.. code:: haskell

   data OperatorFingerprint = OperatorFingerprint Name [OperandFingeprint]

   data OperandFingeprint = SubtermOperand | BindOperand | TokenOperand OperandToken

   operatorFingerprint :: Name -> [Operand] -> [OperandFingerprint]
   operatorFingerprint name operands = OperatorFingerprint name (map operandFingerprint operands)

   operandFingerprint :: Operand -> OperandFingerprint
   operandFingerprint (Subterm _) = SubtermOperand
   operandFingerprint (Bind _)    = BindOperand
   operandFingerprint (Token tok) = TokenOperand tok

   operatorBoundNames :: [Operand] -> [Name]
   operatorBoundNames operands = [name | Bind name <- operands]

The operator fingerprint is used as a lookup key for operator definitions:

.. code:: haskell

   type OperatorDef = {- ... -}
   type OperatorDefs = OperatorFingerprint -> Maybe OperatorDef

The interpretation of an operator itself is given as a map from bound variable names and subterm interpretations to the interpretation of the expression containing the operator.

.. code:: haskell

   data OperatorDef = OperatorDef
     { interpretOperator :: [Name] -> [DepValue] -> DepValue
     , {- ... -}
     }

This allows us to complete the definition of the `interpretTerm` function:

.. code:: haskell

   interpretTerm :: OperatorDefs -> Term -> DepValue
   interpretTerm _    (Lit l)                  = const (Just (interpretLiteral l))
   interpretTerm _    (Var name)               = \env -> Just (env name)
   interpretTerm defs (Operator name operands) = \env -> do
      def <- defs (operatorFingerprint name operands)
      interpretOperator def
         [name | Bind name <- operands]
         [interpretTerm defs term | Subterm term <- operands]
         env

While this is sufficient for defining the interpretation of terms, we would like to enforce a bit more structure to simplify reasoning about terms.
We will do that by adding some data to all operator definitions and also by imposing some requirements that operator definitions will have to satisfy.

We add the following data to operator definitions:

.. code:: haskell

   data OperatorDef = OperatorDef
     { interpretOperator :: [Name] -> [DepValue] -> DepValue
     , boundNames :: [Set Integer]
     , subtermContextDependencies :: [Set Integer]
     }

The `boundNames` data specifies, for each subterm in order, which names are bound when interpreting that subterm.
The names are specified as an index into the list of names.

We have the following requirements for operator definitions:

* Monotonicity with respect to definedness
* Invariance under variable renaming and bypassing

  .. todo:: add section for this

* Partial order on subterm context dependencies

  .. todo:: explain `subtermContextDependencies` above and add section for this

Together these requirements ensure that, among other things, reasoning under assumptions, substitution of variables and context inference during subterm traversal are well behaved.

Monotonicity with Respect to Definedness
****************************************

One property that should hold for all operators is that making operand dependent values more defined without changing any already defined value will also make the operator result more defined and also wont change any already defined value.
Equivalently, we can require that all operator interpretation functions must be monotone with respect to a suitable partial order on dependent operand values and the resulting dependent operator value.

For this, we will start by defining a partial order on optional values and extend it pointwise to dependent values and componentwise to tuples of dependent values:

.. code:: haskell

   valueDefinednessCompare :: Maybe Value -> Maybe Value -> Maybe Ordering
   valueDefinednessCompare lhs rhs =
         case (compare (isJust lhs) (isJust rhs), lhs == rhs) of
            (EQ,  False) -> Nothing
            (ord, _)     -> Just (ord)

   depValueDefinednessCompare :: DepValue -> DepValue -> Maybe Ordering
   depValueDefinednessCompare = pointwisePartialOrdCompare valueDefinednessCompare

   operandDefinednessCompare :: [DepValue] -> [DepValue] -> Maybe Ordering
   operandDefinednessCompare = componentwisePartialOrdCompare depValueDefinednessCompare

   pointwisePartialOrdCompare :: (a -> a -> Maybe Ordering) -> (b -> a) -> (b -> a) -> Maybe Ordering
   pointwisePartialOrdCompare = {- ... -}

   componentwisePartialOrdCompare :: (a -> a -> Maybe Ordering) -> [a] -> [a] -> Maybe Ordering
   componentwisePartialOrdCompare = {- ... -}

With this we can define the monotonicity requirement for an operator definition:

.. code:: haskell

   monotoneOperatorRequirement :: OperatorDef -> Bool
   monotoneOperatorRequirement def = forall \boundNames ->
      isMonotoneMap
         operandDefinednessCompare
         depValueDefinednessCompare
         (interpretOperator def boundNames)

   isMonotoneMap :: (a -> a -> Maybe Ordering) -> (b -> b -> Maybe Ordering) -> (a -> b) -> Bool
   isMonotoneMap = {- ... -}

Judgements
----------


.. todo:: figure out section structure

.. code:: haskell

   type Context = Env -> Bool

   definedJ :: OperatorDefs -> Context -> Term -> Bool
   definedJ defs ctx term =
      forall \env -> not (ctx env) || isSome (interpret defs term env)

   eqJ :: OperatorDefs -> Context -> Term -> Term -> Bool
   eqJ defs ctx lhs rhs =
      definedJ defs ctx lhs &&
      forall \env -> not (ctx env) || interpret defs lhs env == interpret defs rhs env

   boolJ :: OperatorDefs -> Context -> Term -> Bool
   boolJ defs ctx term =
      definedJ defs ctx term &&
      forall \env -> not (ctx env) || (fromJust (interpret defs term env) `elem` [trueValue, falseValue])

   trueJ :: OperatorDefs -> Context -> Term -> Bool
   trueJ defs ctx term =
      definedJ defs ctx term &&
      forall \env -> not (ctx env) || interpret defs term env == Just trueValue

   termCtx :: OperatorDefs -> Term -> Context
   termCtx defs term env = interpret defs term env == Just trueValue

   -- [<defs>] |= <term> defined
   alwaysDefinedJ defs term = definedJ defs (const True) term

   -- [<defs>] |= <lhs> == <rhs>
   alwaysEqJ defs lhs rhs = eqJ defs (const True) lhs rhs

   -- [<defs>] |= <term> bool
   alwaysBoolJ defs term = boolJ defs (const True) term

   -- [<defs>] |= <term>
   alwaysTrueJ defs term = trueJ defs (const True) term

   -- [<defs>] <ctx> |= <term> defined
   ctxDefinedJ defs ctx term = alwaysBoolJ defs ctx && definedJ defs (termCtx defs ctx) term

   -- [<defs>] <ctx> |= <lhs> == <rhs>
   ctxEqJ defs ctx lhs rhs = alwaysBoolJ defs ctx && eqJ defs (termCtx defs ctx) lhs rhs

   -- [<defs>] <ctx> |= <term> bool
   ctxBoolJ defs ctx term = alwaysBoolJ defs ctx && boolJ defs (termCtx defs ctx) term

   -- [<defs>] <ctx> |= <term>
   ctxTrueJ defs ctx term = alwaysBoolJ defs ctx && trueJ defs (termCtx defs ctx) term

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
