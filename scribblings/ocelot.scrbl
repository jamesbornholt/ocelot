#lang scribble/manual

@require[scribble/eval @for-label[ocelot]]

@(define my-eval (make-base-eval #:lang 'racket))
@(my-eval `(require rosette ocelot))

@title{Ocelot: a solver for relational logic}
@author[(author+email "James Bornholt" "bornholt@cs.washington.edu")]

@defmodule[ocelot]

Ocelot provides an embedding of relational logic in 
@link["https://emina.github.io/rosette"]{Rosette},
a solver-aided programming language.
Ocelot enables both @(seclink "check" "verification")
and @(seclink "sketch" "synthesis") of relational logic expressions.

@section{Quick Start}

Using Ocelot involves first @seclink["spec"]{constructing a relational specification},
then @seclink["scope"]{defining a scope} in which to check the specification,
and finally @seclink["check"]{checking properties within that scope}.

@subsection[#:tag "spec"]{Constructing relational specifications}

A relational logic specification consists of:

@itemlist[@item{A universe of discourse}
          @item{Declarations of relations}
          @item{Constraints over those relations}]

@subsubsection{Universe of discourse}

A universe defines the set of atoms over which relations in a specification can range.
All universes are finite.
The @racket[universe] constructor creates a new universe from a list of atoms.

@examples[#:eval my-eval
  (define U (universe '(a b c d)))
  (universe-atoms U)
  ]

@subsubsection{Relation declarations}

A relation declaration defines a new relation,
whose value can later be constrained and then
solved for.
The @racket[declare-relation] procedure creates a new relation of a given arity with a given name.

@examples[#:eval my-eval
  (define File (declare-relation 1 "File"))
  (define Dir (declare-relation 1 "Dir"))
  (define contents (declare-relation 2 "contents"))
  contents
  ]

@subsubsection{Constraints over relations}

Ocelot offers a similar relational algebra to Alloy.
Sentences in Ocelot can be either expressions (i.e., return a relation)
or formulas (i.e., return a boolean).

@examples[#:eval my-eval
  (define DirsAndFiles (+ File Dir))
  (define nonEmptyDir (some (join Dir contents)))
  (define acyclic (no ([d Dir]) (in d (join d (^ contents)))))
  ]

@subsection[#:tag "scope"]{Defining bounds on relations}

By default, Ocelot requires explicit bounds on the possible relations.
A @racket[bound] for a relation consists of two sets of tuples: a lower bound defining tuples that
must be in a relation, and an upper bound defining tuples that may be in a relation.
A @racket[bounds] instance is a list of @racket[bound]s together with a universe of discourse.

@examples[#:eval my-eval
  (define bDir (make-exact Dir '((a))))
  (define bFile (make-bound File '((b)) '((b) (c) (d))))
  (define bContents (make-upper contents (cartesian-product '(a b c d) '(a b c d))))
  (define allBounds (bounds U (list bDir bFile bContents)))
  ]

@subsection[#:tag "check"]{Checking Relational Specifications}

Finally, checking a relational specification involves interpreting the specification
with respect to the bounds. The result is a Rosette expression,
which can then be solved as normal.

The Ocelot interpreter translates Ocelot constraints into Rosette constraints
with respect to a given bounds.

Solving the generated Rosette constraints with @racket[solve] is similar to Alloy's @tt{run} command,
while verifying the constraints with @racket[verify] is similar to Alloy's @tt{check}.

@examples[#:eval my-eval
  (code:comment @#,elem{There is a model in which a directory is non-empty})
  (define formula1 (interpret nonEmptyDir allBounds))
  (define result1 (solve (assert formula1)))
  (sat? result1)

  (code:comment @#,elem{There is a counterexample to acyclicity})
  (define formula2 (interpret acyclic allBounds))
  (define result2 (verify (assert formula2)))
  (sat? result2)
  ]

@subsection[#:tag "sketch"]{Synthesizing Relational Expressions}

TODO

@section{Reference}

Ocelot provides a language for constructing relational formulas,
tools for defining possible values for those formulas,
and an interpreter to reduce those formulas to Rosette terms.

@subsection{Declaring Relations}

@defproc[(declare-relation [arity natural-number/c] [name string?]) node/expr/relation?]{
  Returns a new relation of the given arity and name.}

@subsection{Relational Logic}

The Ocelot DSL embeds relational logic in Rosette.
Many Ocelot operators (e.g., @racket[+]) override their Rosette counterparts to also work
over relations declared with @racket[declare-relation].
These overridden operators should automatically fall back to their Rosette
counterparts if their arguments are not relations.
But this behavior can often be subtle, so
only import the entire @racket[ocelot] module when the enclosing
context will not also be manipulating Rosette expressions.

@subsubsection{Expressions}

@defproc[(+ [a node/expr?] [b node/expr?] ...) node/expr?]{
  Produces the union of two or more relations.}

@defproc[(join [a node/expr?] [b node/expr?] ...) node/expr?]{
  Produces the relational join of two or more relations.}

@defproc[(^ [expr node/expr?]) node/expr?]{
  Produces the irreflexive transitive closure of a relation.}

@subsubsection{Formulas}

@defproc[(! [f node/formula?]) node/formula?]{
  Produces a formula that is true if and only if @racket[f] is false.}

@defproc[(in [a node/expr?] [b node/expr?]) node/formula?]{
  Produces a formula that is true if and only if @racket[a] is a subset of @racket[b].}

@defform[(some maybe-decls body-formula-or-expr)
         #:grammar [(maybe-decls (code:line) (decl ...))
                    (decl [id domain-expr])]
         #:contracts ([body-formula-or-expr (or/c node/formula? node/expr?)]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  If no @racket[decl]s are provided, produces a formula that is true if and only if
  @racket[body-formula-or-expr] (which must be an expression) is not empty.
  
  If @racket[decls] are provided,
  produces a formula that is true if and only if there exists a binding of each @racket[id]
  to a singleton subset of the corresponding @racket[domain-expr] (which must be an expression of arity 1)
  such that @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.}

@defform[(no maybe-decls body-formula-or-expr)
         #:grammar [(maybe-decls (code:line) (decl ...))
                    (decl [id domain-expr])]
         #:contracts ([body-formula-or-expr (or/c node/formula? node/expr?)]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  If no @racket[decl]s are provided, produces a formula that is true if and only if
  @racket[body-formula-or-expr] (which must be an expression) is empty.
  
  If @racket[decls] are provided,
  produces a formula that is true if and only if there exists no binding of each @racket[id]
  to a singleton subset of the corresponding @racket[domain-expr] (which must be an expression of arity 1)
  such that @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.

  In both cases, @racket[(no ...)] is equivalent to the negation @racket[(! (some ...))].
}

@subsection{Scopes}

Ocelot interprets relational expressions with respect to a set of bounds,
which constrain the possible values of relations.
The bounds, in turn, consist of tuples drawn from a universe of discourse.

@subsubsection{Universes}

@defproc[(universe [atoms (listof symbol?)]) universe?]{
  Creates a new universe with the given @racket[atoms].}

@defproc[(universe-atoms [universe universe?]) (listof symbol?)]{
  Returns the atoms for a given universe.}

@subsubsection{Bounds}

@defproc[(make-bound [relation node/expr/relation?]
                     [lower (listof (listof symbol?))]
                     [upper (listof (listof symbol?))]) bound?]{
  Create a new bound for @racket[relation] that constrains it to
  always contain every tuple in @racket[lower], and only contain tuples
  in @racket[upper]. Both @racket[lower] and @racket[upper] must be lists of tuples,
  where a tuple is a list of atoms and has the same length as the arity of @racket[relation].}

@defproc[(make-exact [relation node/expr/relation?]
                     [contents (listof (listof symbol?))]) bound?]{
  Create a new bound for @racket[relation] that constrains it
  contain exactly the tuples in @racket[contents].

  Equivalent to @racket[(make-bound relation contents contents)].}

@defproc[(make-upper [relation node/expr/relation?]
                     [contents (listof (listof symbol?))]) bound?]{
  Create a new bound for @racket[relation] that constrains it
  contain only the tuples in @racket[contents].

  Equivalent to @racket[(make-bound relation '() contents)].}

@defproc[(bounds [universe universe?]
                 [bnds (listof bound?)]) bounds?]{
  Collect a set of relation bounds and a universe together for use by @racket[interpret].}

@subsection{Solving}

Ocelot compiles relational formulas to Rosette constraints,
which can then be used directly with Rosette's solving and verification features.

@defproc[(interpret [formula node/formula?]
                    [bounds bounds?]) term?]{
  Translate @racket[formula] into a Rosette formula that is satisfiable
  if and only if there exists a binding to each relation in @racket[bounds]
  that satisfies its resepctive upper and lower bound
  such that @racket[formula] evaluates to true under that binding.

  The @racket[bounds] must provide a bound for every free relation mentioned in @racket[formula].}

