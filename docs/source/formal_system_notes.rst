Formal System Design Notes
==========================

.. todo:: write a short introduction
.. todo:: turn more of the bullet points into paragraphs
.. todo:: add links to external sources

The Formal System's Role
------------------------
* Provide a uniform and consistent user visible language
   * for the public API
   * for native file formats
* Provide unambiguous semantics
   * of the public API / native file formats
   * for non-native file formats
      * by defining a (mostly) syntactic translation into the native formal system
* Enable deductive reasoning
   * as a development aid
      * manually verify that implemented methods can be justified by valid deductions within the formal system
   * to track sub-goals and completion of high-level strategies, e.g.
      * when decomposing a model checking problem into a graph of conditional model checking problems
         * manually by the user (aka assume-guarantee)
         * by automated methods (e.g. inner signal equalities for equivalence checking)
   * for verified model checking (eventually)
      * implement methods that justify derived facts using the deductive part of the formal system
      * using a checker that can check derivations in the deductive system
         * an optimized implementation for runtime verification
         * an independent simple and/or formally verified implementation
      * by writing certificates that contain derivations in the deductive system
   * for hybrid automated and interactive verification (eventually)
      * allow the user to provide their own derivations to justify facts that the automated methods cannot handle
         * might need a layer to make manual derivations less tedious, e.g.
            * similar to tactics provided by interactive theorem provers, or
            * at least some syntactic sugar and inference of trivial parts

Requirements for the Formal System
----------------------------------
This list is quite incomplete relative to what was considered.

* Support full classical logic, no restriction to constructive/intuitionistic logic
   * Easier to understand for users with no background in type theory
   * Allows unrestricted use of automated methods that use the law of the excluded middle
* Have a single notion of equality
   * Easier to understand for users with no background in type theory
   * Leibniz equality, i.e. a and b are equal iff they share all properties and thus can be substituted for each other
      * Makes direct application of egraphs valid
* Be many-sorted / typed
   * As hardware description languages and RTLIL distinguish signals of different types (or at least widths)
   * As it reduces boilerplate in definitions and derivations
* Have good support for the sort of fixed-width bitvectors
   * They are ubiquitous in the domain of hardware verification

Evaluation of Prior Art
-----------------------
Discussions between the proponents of different approaches to this can sometimes get quite heated and it seems worth mentioning that this section merely summarizes the reasoning behind some of the choices made specifically for Imctk and should be seen in that context.
The statements below only paint a very incomplete overall picture, include opinions and greatly oversimplify many aspects.

Many-sorted First-order Logic
*****************************
* Used by SMT-LIB and many automated theorem provers
* Cannot natively handle fixed-width bitvectors
   * SMT-LIB defines an indexed family of sorts and indexed families of primitives, but does not allow width parametric reasoning
* Excellent as input and output for automation, but often too limiting as a deductive system
* Excellent compatiblity with egraphs
* Defining functions and datatypes requires postulations whose relative consistency can only be verified outside of the system. Since the logic itself isn't that powerful it seems usually not too hard to avoid unintended consequences in practice.

Higher-order Logic
******************
* Implemented in Isabelle/HOL, HOL4, HOL Light
* First order methods can often be applied directly by encoding some of the higher-order aspects and making the rest opaque
   * That's my very superficial understanding of what Sledgehammer in Isabelle/HOL does
* Makes it awkward to handle fixed-width bitvectors
   * The simple approach is to use the sort of lists and keep track of the width as a property
      * needing more boilerplate and losing type safety
         * really awkward for operations that have constraints on input bit-widths, which are common
   * The alternative is to introduce some form of separate type level natural numbers
      * still can't easily deal with non-trivial bit-widths as neede for slicing, concatenation, repitition etc.
      * having value level naturals and separate type level naturals is confusing and often frustrating
* Defining functions and datatypes can involve postulations whose relative consistency can only be verified outside of the system. Getting the required checks for this right might involve tricky corner cases.

Intensional Type Theory
***********************
* Implemented in Rocq/Coq, Agda and Lean
* Can natively represent fixed-width bit vectors
* Fails the "single notion of equality" requirement in a way that badly interacts with the practical use of fixed-width bitvectors
   * many simple arithmetic identities only hold as propositional equalities ``=`` (more or less the common notion of equality) not as judgemental/definitional equalities ``≡`` (a more restrictive notion)
   * function application requires types that are compatible judgementally
   *  This means the following usually doesn't type check:

      .. code::

         concat : {n : Nat} -> {m : Nat} -> BitVec n -> BitVec m -> BitVec (n + m)
         deinterleave : {n : Nat} -> BitVec (2 * n) -> (BitVec n, BitVec n)

         foo : {n : Nat} -> BitVec n -> (BitVec n, BitVec n)
         foo x = deinterleave (concat a a)

      It will produce some error about not being able to unify ``BitVec (2 * n)`` and ``BitVec (n + n)``.
      This can't be solved by proving the propositional equality ``n + n = 2 * n``, as the judgemental equality ``n + n ≡ 2 * n`` is needed.

      Intentional type theory requires that all judgments can be decided ("decidable type checking") and thus restricts it to terms that are syntactically equal after normalization (more or less evaluation that gets stuck on free variables), this includes judgemental equality.

      Usually there is a relatively simple way to work around this, by e.g. providing a primitive like ``cast : {a : Sort} -> {_ : a = b} -> a -> b`` which would make ``foo x = deinterleave (cast (concat a a))`` valid as long as the required propositional equality has been proved and is known to the elaborator so it can fill in the implicit argument ``{_ : a = b}``.

      For our use cases this isn't a satisfactory solution, though, as it still requries keeping track of definitional vs propositional equality.
      This limits (or at least complicates) the use of egraphs and is also hard to explain to users that don't happen to have prior experience with any of the interactive theorem provers based on intensional type theory.
* .. TODO:: intuitionistic / constructive
* Defining functions and datatypes often involves postulations whose relative consistency can only be verified outside of the system.
   * Getting the required checks for this right seems incredibly difficuilt
   * As far as I can tell this is a relatively common source of soundness bugs in type theory based theorem provers

Extensional Type Theory
***********************
* Can natively represent fixed-width bit vectors
* Takes the rules of extensional type theory but adds a deductive rule that allows inferring judgmental equality ``a ≡ b`` from a proof ``_ : a = b`` of the propositional equality.
   * This makes type checking undecidable, as for ``_ : a = b`` the term in the place of ``_`` is a proof term which contains sufficient information to reconstruct a proof (this is the proofs-as-programs part of the Curry-Howard correspondence) and all that is thrown away when we deduce ``a ≡ b`` from that.
      * This is incompatible with the typical architecture of intensional type theory based proof assistants (AFAIK).
      * A lot of the research in type theory assumes decidable type checking and/or proofs-as-programs is a requirement (AFAICT)
      * Decidable proof checking is still possible but requires a derivation (an object outside of the theory) as input, not only a proof term (an object within the theory itself).
* Not aware of any practical implementation of this that's sufficiently easy to setup and get started with to allow for a practical evaluation

Meta-language with Theory Rules and Axioms
**********************************************

.. TODO:: describe how SMT proof certificate formats like LFSC or Alethe fit this pattern
.. TODO:: describe why this is insufficient for what we want


Axiomatic Set Theory
********************
* Implemented as a "library" in various meta-logics like Metamath or Isabelle/ZF (the meta-logic Isabelle with the ZF axioms)
* Fails the many-sortedness requirement
  * but using sets as sorts can be good enough
* Requires a lot of boilerplate
* Can prove existence of all functions and datatypes from within the system
   * Tiny "Trusted Theory Base" (analogous to "Trusted Computing Base")

Type Theory-like Interface to a Set Theory
******************************************
* Here "interface" means something like syntactic sugar, but not limited to syntax
* Anything that can be defined as set can be used as a type, thus no issue with supporting fixed-width bitvectors
* With set theory being a first order theory, there's no issue with equality
* The user interface takes care of all the boilerplate required to keep track of the propositions that emulate types via set-membership
* Approach taken by Mizar
   * Mizar has a much heavier focus on mathematics, not on program verification, and I haven't used it in practice
* As everything is derived from the underlying set theory, it can also prove existence of all functions and datatypes from within the system
   * Tiny "Trusted Theory Base"
* There's no clear line between an extensional type theory with a set theoretical model and a a type theory like interface to a set theory

Formal System Description
-------------------------

* The formal system can be understood as a type theory that
   * has dependent types
   * is extensional
   * has subtyping
   * type membership and subtyping are always propositional
   * is non-constructive
   * uses sets as types
   * has set-theoretic semantics
* Alternatively it can be seen as a not-only-syntactic sugar on top of a set theory
* The only judgement of the formal system is (untyped) equality: ``<x> === <y>`` meaning the terms ``<x>`` and ``<y>`` are equal
   * Well-formedness of a term is represented by the equality judgment ``<x> === <x>`` which we will also abbriviate as ``<x> WF``
   * True propositions are represented by the equality judgement ``<x> === true`` which we will abbrivate as just ``<x>``
   * Since the inference rules include symmetry and transitivity, the transitive closure of a set of judgements can be represented as an equivalence relation on terms
   * Since the inference rules include congruence (substitution of equal subterms), the congruence closure of a set of judgments can be represented as an egraph
* Value semantics
   * Are given by an encoding of values into sets
      * We use a non-standard set theory, see the "Low-level Set Theory Details" for details
   * The encoding is global, i.e. the set encoding of a value does not depend on any type membership of that value
   * We require that every value is encoded as a pair ``(<label>, <encoded-value>)``
      * This assumes a given set-theory-level pair encoding and a set of disjoint labels
         * We will prefix labels with ``#``.
      * This ensures we can define new disjoint types by picking a unique label without worrying about unintended collisions of encoded values
   * There are a few builtin classes of values
      * Pure sets
         * Represents raw values of the underlying set theory
         * Encoded as ``(#pure-set, <value>)`` with an arbitrary ``<value>``
      * Sets (also called native sets to distinguish them from pure sets)
         * Used as types
         * Represents sets of arbitrary formal system values
         * Encoded as ``(#set, <set-of-encoded-values>)``
      * Pairs
         * Sets of pairs are used for functions and relations
         * Encoded as ``(#pair, (<fst>, <snd>))``
      * Booleans
         * Encoded as ``(#bool, #true)`` and ``(#bool, #false)``
* Term semantics
   * Terms can contain free variables
      * The semantic value of a term can depend on the values of the free variables
      * A term may impose assumptions on the values of free variables
      * By default free variables range over all values, assumptions are used to restrict them to a given type
      * Semantically a term represents a partial function mapping a variable assignment to the terms value under that assignments
         * This function is meant if we refer to "term semantics" or "semantic function of a term"
         * A variable assignment has values for all variables, even those that do not occur in the term
         * A term may only depend on the values assigned to free variables that occur syntactically
      * The equality judgement between terms means that both terms represent the same partial function
         * Both partial functions have the same domain and produce equal values under equal variable assignments
   * Terms are built from primitives which can have subterms
      * Terms have to be well-founded, i.e. you cannot have cyclic terms
      * Semantically a primitive with subterms represents a higher-order function from the subterm semantics to the overall semantics
         * In particular it can only depend on the subterm semantics, not the subterm syntax
            * This justifies substitution of equal subterms
      * Primitives can be functional or context-modifying
         * A functional primitive applies subterm semantic functions only to the unmodified variable assignment
            * This means the value of a functional primitive is a function of the subterms' values under the same variable assignment
            * The domain of a functional primitive's semantic function is the intersection of all subterms' domains
         * A context-modifying primitive is free to make unrestricted use of the subterms' semantic functions
      * Well-formedness of a term requires that all subterms are well-formed
   * Builtin primitives
      * .. TODO:: organize and complete this
      * .. TODO:: when should context restrictions be implicit and when should they be prerequisite
      * Context modifying primitives
         * Context restriction: ``[C] X`` where ``C : Bool``
            * ``<[C] X>(A) = <X>(A) if <X>(A) defined and <C>(A) defined``
         * Function abstraction: ``fun (#X : T) => Y`` where ``is-set T``
            * ``<fun (#X : T) => Y>(A) = (#set, { (#pair, (X, <Y>(A[#X := X]))) | X : <T>(A) })``
         * Unrestricted quantification: ``forall #X => Y`` and ``exists #X => Y``
            * ``<forall #X => Y>(A) = if (forall X where <Y>(A[#X := X]) defined => <Y>(A[#X := X])) then <true>[_] else <false>[_]``
            * same with exists
         * Ordered alternatives: ``X; Y``
            * ``<X; Y>(A) = if <X>(A) defined then <X>(A) else <Y>(A)``
         * Conditional ``if X then Y else Z``
            * Equivalent to ``[X] Y; [!X] Z``
      * Functional primitives
         * ``!X``, ``X & Y``, ``X | Y`` propositional logic
         * ``X = Y`` propositional equality, pointwise equality
         * ``var #X`` the value of the variable with label ``#X``
         * ``is-set X`` proposition checking for the ``#set`` label in the encoding
         * ``is-pure-set X`` proposition checking for the ``pure-set`` label in the encoding
         * ``is-fun X`` proposition checking that ``X`` is a function (set representing a functional binary relation)
         * ``fun-domain X`` where ``is-fun X`` the domain on which a function is defined
         * ``X : Y`` where ``is-set Y``
         * ``bool`` the value encoded as ``(#set, { (#bool, false), (#bool, true) })``
         * ``X Y`` where ``is-fun X`` and ``Y : domain X``, function application



Low-level Set Theory Details
****************************

The points below are written down to make it easier to spot any inconsistencies or other internal problems of the formal system and are not a prerequisite for using and understanding the rest of the formal system itself.
Considering these details is only necessary when extending the formal system itself, to ensure that the overall system remains consistent.

* Note I'm also using ``:`` to denote set membership
* The set theory we're using is Tarski-Grothendieck (TG) set theory with Aczel's anti-foundation axiom
   * Start with the standard Zermelo-Fraenkel (ZF) set theory, optionally including choice (ZFC)
   * Add Tarski's Axiom which states that for every set ``s`` there is a universe that contains ``s``
      * A set is a universe if it contains
         * all subsets of its members
         * all elements of its members
         * all smaller cardinality subsets of the universe itself
      * Note that also every universe is itself contained in a larger universe
         * This allows representing parametric polymorphism (aka generics) within set theory without running into variants of Russell's paradox (Burali-Forti's paradox, Girard's paradox, Hurkens' paradox)
            * Type theory also uses a hierarchy of universes for the same reason
            * Instead of having
         * If we use sets to represent types, a parametric function like ``reverse : (a : Type) -> List a -> List a`` would take a value of type ``Type`` as argument, but we can't have ``Type : Type`` as that would ultimately lead to Russell's paradox
            * Instead we have an infinite hierarchy ``Type = Type 0 : Type 1 : Type 2 : ...``
            * The (dependent) type ``(a : Type) -> List a -> List a`` would itself live in ``Type 1``
   * Remove the axiom of foundation (AF) and replace it with Aczel's anti-foundation axiom (AFA)
      * The axiom of foundation is states that there is no infinite sequence of sets each contained in the preceding set (or something equivalent)
         * This means if we use sets to model our types, we can't natively support codata like infinite streams or infinite trees
            * For modelling reactive systems, such types are really useful and we'd like to avoid awkward alternative encodings
      * We can represent any set as an accessible pointed directed graph
         * Pointed means there is a distinguished root node
         * Accessible means there is a path from the root to every other node
         * The nodes of the graph are the (transitively) contained sets
         * The root is the set itself
         * There is an edge from a to b if b is a member of a
         * The axiom of foundation then is the statement that such a graph can't contain an infinite path
      * If we take any directed graph without infinite paths, we can recover sets from it by "decorating" it
         * Decorating means that for every node ``n``, we assign a set ``d(n)`` such that ``s successor of n`` exactly if ``d(s) : d(n)``
         * We may be forced to assign the same graph to distinct nodes, e.g. every node with no successors will be decorated with the empty set
         * The assignment will be unique
            * .. TODO:: find a a concise argument for this
         * Going from a set to a graph and back results in the set we started with as decoration for the root node
      * Aczel's anti-foundation axiom says that every directed graph has a unique decoration, no restriction on infinite paths
      * In particular there are sets for which ``A : A`` holds, e.g. the set ``A = { A }``
         * There still can't be a set that is as large as its own powerset, as Cantor's theorem still holds
            * This allows us to still avoid all the paradoxes
            * This means we can't have ``Type : Type`` with ``Type`` being closed under arbitrary subsets
            * In fact if ZFC is consistent then ZFA (ZFC - FA + AFA) is consistent and vice versa
               * We can encode ZFA sets as graphs and then prove all encoded ZFA axioms from ZFC
               * Also works for the other direction but it's more a characterization than an encoding
               * I'm fairly confident that the same argument goes through starting from Tarski-Grothendieck set theory
      * AFA allows us to extend most straight forward set-theoretic encoding of algebraic data types to codata



Formal System API
*****************

* Ast
   * refcounted AST
      * Immutable
      * Can be passed freely between threads and components
      * .. TODO:: should this be globally uniquified?
      * .. TODO:: how to optimize the implementation to use less memory than a naive implementation and to have less locking and/or contention
* Term
   * wrapper that ensures an ast represents a well-formed term
* TermEq
   * pair of two well-formed and equivalent terms
* TermEqs
   * partial equivalence relation of well-formed terms
* ProofStep
   * wrapper that ensure an ast represents a well-formed proof step
* ProofLog
   * thread-local sink for proof steps

