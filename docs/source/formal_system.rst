The PlaceholderName Formal System
=================================

.. default-role:: code

This chapter introduces the PlaceholderName formal system.
It consists of a language used for modeling system behavior, specifying properties to check and to represent facts inferred by the model checker, together with a set of inference rules.

.. todo:: some early examples

Values
------

As PlaceholderName is based on set theory, sets play an important role.
Unlike a pure set theory, where there are only sets, PlaceholderName also supports non-set values.
In both PlaceholderName and a pure set theory, objects like numbers, tuples, algebraic data types, etc. must be constructed out of sets.
In a pure set theory such objects are defined to be equal to this construction.
In PlaceholderName on the other hand, we can use such a construction to define new non-overlapping classes of non-set values.

To define such a new value class, we still start with a model constructed out of the already defined value classes.
Instead of directly using this construction, we use it to obtain a new class of values together with a bijection between the model and the new value class.
To start defining properties and functions on the new value class, we have to define them in terms of the model first.

.. note:: PlaceholderName's value classes are not the same as sorts in many-sorted logic. In PlaceholderName, quantified variables, function domains and members of a set are not restricted to a single value class.

Contexts
--------

Any term and most judgements in PlaceholderName are interpreted within some context.
The context keeps track of any assumptions or local definitions.
Whether some syntax is considered a well-formed term can also depend on the context, e.g. when it refers to local definitions or uses functions that restrict possible argument values.

.. todo:: figure out section structure

   * Ordered list of:
      * assumptions
      * definitions of
         * constant symbols
         * function symbols (should we add these?)
         * value class symbols
         * some way to abbreviate combiniations of context modifying syntax?
   * No redefinition / shadowing of symbols / value classes
   * Each context defines a sublanguage of well-formed terms
   * Each context defines a class of allowed variable assignments
   * A context assigns a semantic interpretation to the well-formed terms
      * This is a mapping of variable assignments to the value the term would evaluate to under that assignment
   * Two contexts are equal if they have the same well-formed terms and for each well-formed term assign the same semantic interpretation
   * We require two contexts that are the same up to permutation to be equal
      * but not every permutation of a context is a valid context
   * We need to define a partial order of contexts such that extending a context gives you a smaller context and results from a larger context transfer to smaller ones

   * Context syntax
      * Comma separated list of
         * Propositional assumption: `<term>`
            * requires `<ctx_prefix> |= <term> in bool`
         * Equality assumption: `<term_a> == <term_b>`
            * requires `<ctx_prefix> |= <term_a> term` and `<ctx_prefix> |= <term_b> term`
         * Constant definition: `<const_symbol> := <term>`
            * requires `<ctx_prefix> |= <term> term` and `<const_symbol>` not occuring in `<ctx_prefix>`
         * Function definition: TODO?
         * Value class definitions: `<class_name>[v] := <term>`
            * what are the exact requirements?

Terms
-----

.. todo:: figure out section structure

   * Variables
      * including unrestricted variables not appearing in the context
   * Constants
   * Function symbol applications
      * application of value-level functions is done using some form of `apply` function symbol
      * this includes builtins like the equality or logic operators
   * Case splitting
   * Quantifiers
      * Forall
      * Exists
      * Exists-Unique
   * Function abstraction
   * Set comprehension
   * Choice operator
      * Should we have a separate choice and unique choice operator?
   * Short circuiting Boolean operators
      * The RHS is interpreted in a restricted context

Judgments
---------

.. todo:: figure out section structure

   * `<ctx> ctx` meaining `<ctx>` is a well-formed context
   * `<ctx_a> ctx == <ctx_b> ctx` meaning `<ctx_a>` and `<ctx_b>` are equal contexts
   * `<ctx_a> ctx <: <ctx_b> ctx` meaning `<ctx_a>` is a subcontext of `<ctx_b>`
   * `<ctx> |- <term> term` meaining `<term>` is a well-formed term in the well-formed context `<ctx>`
   * `<ctx> |- <term_a> == <term_b>` meaning the well-formed terms `<term_a>` and `<term_b>` evaluate to the same value under all variable assignments allowed by the well-formed context `<ctx>`
   * `<ctx> |- <term>` meaing the well-formed term `<term>` evaluates to `true` under all variable assignments allowed by the well-formed context `<ctx>`

Inference Rules
---------------

.. todo:: figure out what rules are needed and how to structure this section

   * Context formation rules
   * Term formation rules
      * We only allow forming a function application term if the given arguments are known to be in the domain of the function.
   * First order logic with equality
   * Set axioms
   * Context manipulation
   * ...
