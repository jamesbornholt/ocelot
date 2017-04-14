#lang scribble/manual

@require[racket/runtime-path racket/require racket/sandbox scribble/eval
         "log.rkt"
         @for-label[ocelot
                   (only-in rosette term? solve)
                   (only-in rosette/base/core/safe assert)]]

@(define-runtime-path root ".")
@(define my-eval (make-log-evaluator (logfile root) 'rosette))
@(my-eval `(require ocelot))

@title{Ocelot: a solver for relational logic}
@author[(author+email "James Bornholt" "bornholt@cs.washington.edu")]

@defmodule[ocelot]

Ocelot provides an embedding of bounded relational logic in 
@link["https://emina.github.io/rosette"]{Rosette},
a solver-aided programming language.
Ocelot enables both @(seclink "check" "verification")
and @(seclink "sketch" "synthesis") of relational logic expressions.

Ocelot's flavor of bounded relational logic draws heavily on
@link["http://alloy.mit.edu/"]{Alloy},
so many concepts and examples from Alloy will also help
in developing Ocelot programs.

@section{Quick Start}

Ocelot is best used with @link["https://emina.github.io/rosette"]{Rosette},
so your file should begin:

@codeblock|{
  #lang rosette
}|

Using Ocelot involves first @seclink["spec"]{constructing a relational specification},
then @seclink["scope"]{defining a scope} in which to check the specification,
and finally @seclink["check"]{checking properties} within that scope.

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
  (define bDir (make-exact-bound Dir '((a))))
  (define bFile (make-bound File '((b)) '((b) (c) (d))))
  (define bContents (make-product-bound contents '(a b c d) '(a b c d)))
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

@;{
@subsection[#:tag "sketch"]{Synthesizing Relational Expressions}

TODO
}

@section{Reference}

Ocelot provides a language for constructing relational formulas,
tools for defining possible values for those formulas,
and an interpreter to reduce those formulas to Rosette terms.

@subsection{Declaring Relations}

@defproc[(declare-relation [arity natural-number/c] [name string?]) relation?]{
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

@defproc[(& [a node/expr?] [b node/expr?] ...) node/expr?]{
  Produces the intersection of two or more relations.}

@defproc[(- [a node/expr?] [b node/expr?] ...) node/expr?]{
 Produces the first relation, but without any tuples present in the remaining relations (i.e., set difference).
 
 @racket[(- a b c)] is equivalent to @racket[(- (- a b) c)].}

@defproc[(-> [a node/expr?] [b node/expr?] ...) node/expr?]{
  Produces the cross product of two or more relations.}

@defproc[(~ [a node/expr?]) node/expr?]{
  Produces the inverse of a relation of arity 2.

  If ⟨@italic{x},@italic{y}⟩ is an element of @racket[a], then ⟨@italic{y},@italic{x}⟩ is an element of @racket[(~ a)].}

@defproc[(join [a node/expr?] [b node/expr?] ...) node/expr?]{
  Produces the relational join of two or more relations.}

@defproc[(<: [a node/expr?] [b node/expr?]) node/expr?]{
 Produces the relation containing all tuples in @racket[b] whose first element
 is contained in @racket[a], which must have arity 1.}

@defproc[(:> [a node/expr?] [b node/expr?]) node/expr?]{
 Produces the relation containing all tuples in @racket[a] whose last element
 is contained in @racket[b], which must have arity 1.}

@defthing[none node/expr?]{
 A relation of arity 1 that contains no tuples.}

@defthing[univ node/expr?]{
 A relation of arity 1 that contains the tuple ⟨@italic{x}⟩ for every atom @italic{x} in the universe.}

@defthing[iden node/expr?]{
 A relation of arity 2 that contains, for every atom @italic{x} in the universe,
 the tuple ⟨@italic{x}, @italic{x}⟩.

 @racket[iden] is equivalent to @racket[(-> univ univ)].}

@defproc[(^ [expr node/expr?]) node/expr?]{
  Produces the irreflexive transitive closure of a relation, which must have arity 2.}

@defproc[(* [expr node/expr?]) node/expr?]{
  Produces the reflexive transitive closure of a relation, which must have arity 2.

  @racket[(* a)] is equivalent to @racket[(+ (^ a) iden)].}

@defform[(set (decl ...) body-formula)
         #:grammar [(decl [id domain-expr])]
         #:contracts ([body-formula node/formula?]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  Produces a set comprehension, which is a relation with arity equal to the number of @racket[decl]s provided.
  The set comprehension contains a tuple ⟨@italic{x}@subscript{1}, ⋯, @italic{x}@subscript{n}⟩
  if and only if each @italic{x}@subscript{i} is an element of the corresponding @racket[domain-expr]@subscript{i}
  (which must be an expression of arity 1) and, when each identifier @racket[id]@subscript{i}
  is bound to the corresponding singleton tuple ⟨@italic{x}@subscript{i}⟩,
  @racket[body-formula] evaluates to true.}


@subsubsection{Formulas}

@defproc[(in [a node/expr?] [b node/expr?]) node/formula?]{
  Produces a formula that is true if and only if @racket[a] is a subset of @racket[b].}

@defproc[(= [a node/expr?] [b node/expr?]) node/formula?]{
  Produces a formula that is true if and only if @racket[a] and @racket[b] contain the same elements.}

@defproc[(and [a node/formula?] [b node/formula?] ...) node/formula?]{
  Produces a formula that is true if and only if every input formula is true.}

@defproc[(or [a node/formula?] [b node/formula?] ...) node/formula?]{
  Produces a formula that is true if and only if at least one of the input formulas is true.}

@defproc[(=> [a node/formula?] [b node/formula?]) node/formula?]{
  Produces a formula that is true if and only if @racket[a] implies @racket[b].

  Equivalent to @racket[(or (! a) b)].}

@deftogether[(
  @defproc[(! [f node/formula?]) node/formula?]
  @defproc[(not [f node/formula?]) node/formula?])]{
  Produces a formula that is true if and only if @racket[f] is false.}




@subsubsection{Quantifiers and Multiplicities}

Most quantifiers each come in two forms.
One form takes no @racket[decl]s, and a body which is an expression,
and evaluates to true if and only if the body has the appropriate cardinality.
The second form takes @racket[decl]s, and a body which is a formula,
and evaluate to true if and only if the number of bindings of the @racket[decl]s
under which the body evaluates to true has the appropriate cardinality.

@defform[(all (decl ...) body-formula)
         #:grammar [(decl [id domain-expr])]
         #:contracts ([body-formula node/formula?]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  Produces a formula that is true if and only if, for every binding of each @racket[id]
  to a singleton subset of the corresponding @racket[domain-expr] (which must be an expression of arity 1),
  @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.}

@defform[(some maybe-decls body-formula-or-expr)
         #:grammar [(maybe-decls (code:line) (decl ...))
                    (decl [id domain-expr])]
         #:contracts ([body-formula-or-expr (or/c node/formula? node/expr?)]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  If no @racket[decl]s are provided, produces a formula that is true if and only if
  @racket[body-formula-or-expr] (which must be an expression) is @italic{not empty}.
  
  If @racket[decls] are provided,
  produces a formula that is true if and only if there exists @italic{some} binding of each @racket[id]
  to a singleton subset of the corresponding @racket[domain-expr] (which must be an expression of arity 1)
  such that @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.}

@defform[(no maybe-decls body-formula-or-expr)
         #:grammar [(maybe-decls (code:line) (decl ...))
                    (decl [id domain-expr])]
         #:contracts ([body-formula-or-expr (or/c node/formula? node/expr?)]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  If no @racket[decl]s are provided, produces a formula that is true if and only if
  @racket[body-formula-or-expr] (which must be an expression) is @italic{empty}.
  
  If @racket[decls] are provided,
  produces a formula that is true if and only if there exists @italic{no} binding of each @racket[id]
  to a singleton subset of the corresponding @racket[domain-expr] (which must be an expression of arity 1)
  such that @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.

  In both cases, @racket[(no ...)] is equivalent to the negation @racket[(! (some ...))].
}

@defform[(one maybe-decls body-formula-or-expr)
         #:grammar [(maybe-decls (code:line) (decl ...))
                    (decl [id domain-expr])]
         #:contracts ([body-formula-or-expr (or/c node/formula? node/expr?)]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  If no @racket[decl]s are provided, produces a formula that is true if and only if
  @racket[body-formula-or-expr] (which must be an expression) contains @italic{exactly one} tuple.
  
  If @racket[decls] are provided,
  produces a formula that is true if and only if there exists @italic{exactly one} binding of the @racket[id]s
  to a singleton subset of the corresponding @racket[domain-expr]s (which must be expressions of arity 1)
  such that @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.
}

@defform[(lone maybe-decls body-formula-or-expr)
         #:grammar [(maybe-decls (code:line) (decl ...))
                    (decl [id domain-expr])]
         #:contracts ([body-formula-or-expr (or/c node/formula? node/expr?)]
                      [domain-expr (and/c node/expr? (equal? (node/expr/arity expr) 1))])]{
  If no @racket[decl]s are provided, produces a formula that is true if and only if
  @racket[body-formula-or-expr] (which must be an expression) contains @italic{at most one} tuple.
  
  If @racket[decls] are provided,
  produces a formula that is true if and only if there exists @italic{at most one} binding of the @racket[id]s
  to a singleton subset of the corresponding @racket[domain-expr]s (which must be expressions of arity 1)
  such that @racket[body-formula-or-expr] (which must be a formula) evaluates to true under that binding.

  In both cases, @racket[(lone ...)] is equivalent to the disjunction @racket[(or (no ...) (one ...))].
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

@defproc[(make-bound [relation relation?]
                     [lower (listof (listof symbol?))]
                     [upper (listof (listof symbol?))]) bound?]{
  Create a new bound for @racket[relation] that constrains it to
  always contain every tuple in @racket[lower], and only contain tuples
  in @racket[upper]. Both @racket[lower] and @racket[upper] must be lists of tuples,
  where a tuple is a list of atoms and has the same length as the arity of @racket[relation].}

@defproc[(make-exact-bound [relation relation?]
                           [contents (listof (listof symbol?))]) bound?]{
  Create a new bound for @racket[relation] that constrains it to
  contain exactly the tuples in @racket[contents].

  Equivalent to @racket[(make-bound relation contents contents)].}

@defproc[(make-upper-bound [relation relation?]
                           [contents (listof (listof symbol?))]) bound?]{
  Create a new bound for @racket[relation] that constrains it to
  contain only the tuples in @racket[contents].

  Equivalent to @racket[(make-bound relation '() contents)].}

@defproc[(make-product-bound [relation relation?]
                             [atoms (listof symbol?)] ...)
                             bound?]{
  Create a new bound for @racket[relation] that constrains it to
  contain only the tuples in the cartesian product of the @racket[atoms].
  @racket[relation] must have the same arity as the number of @racket[atoms].

  Equivalent to @racket[(make-upper-bound relation (cartesian-product atoms ...))].}

@defproc[(bounds [universe universe?]
                 [bnds (listof bound?)]) bounds?]{
  Collect a set of relation bounds and a universe together for use by @racket[interpret].}

@defproc[(bounds? [a any/c]) boolean?]{
  Return true if and only if @racket[a] is an instance of @racket[bounds].}


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

@defproc[(interpret* [formula node/formula?]
                     [interp interpretation?]) term?]{
  Like @racket[interpret], but takes as input an interpretation instead of bounds.
       
  @racket[interpret*], together with @racket[instantiate-bounds] and @racket[interpretation->relations],
  are useful for lifting the results of a satisfiable Rosette query to a model for relations
  (see @secref{Interpretations} below).

  The @racket[interpretation] must provide an interpretation for every free relation mentioned in @racket[formula].}

@subsubsection{Interpretations}

Ocelot reduces relational formulas to boolean formulas by way of interpretations,
which assign boolean variables for the presence of each possible tuple in a relation.
Interpretations can be used to list a satisfying model from a Rosette query
back to the relations that defined the solved formula.

@defproc[(instantiate-bounds [bounds bounds?]) interpretation?]{
  Create an interpretation for each relation bound by @racket[bounds].
  An inteerpretation of a relation @racket[a] is a set of boolean variables,
  one for each potential tuple in the relation, that are true if and only if
  the corresponding tuple is present in @racket[a].}

@defproc[(interpretation->relations [interp interpretation?])
         (hash/c relation? (listof (listof symbol?)))]{
  Returns a hash table that maps each relation bound by @racket[interp]
  to a list of tuples contained in that relation under the interpretation @racket[interp].

  @racket[interp] must be fully concrete (contains no symbolic boolean variables).}

@examples[#:eval my-eval
  (code:comment @#,elem{Declare a cats relation and create an interpretation for it})
  (define cats (declare-relation 1 "cats"))
  (define bCats (make-upper-bound cats '((a) (b) (c) (d))))
  (define allCatBounds (bounds U (list bCats)))
  (define iCats (instantiate-bounds allCatBounds))

  (code:comment @#,elem{Find an interesting model for the cats relation})
  (define F (and (some cats) (some (- univ cats))))
  (define resultCats (solve (assert (interpret* F iCats))))
  (sat? resultCats)

  (code:comment @#,elem{Lift the model to lists of tuples for each relation})
  (define catsModel (interpretation->relations (evaluate iCats resultCats)))
  (hash-ref catsModel cats)
  ]

@subsection[#:tag "sketch"]{Sketching and Synthesis}

@defproc[(expression-sketch [depth natural-number/c]
                            [arity natural-number/c]
                            [operators (listof procedure?)]
                            [terminals (listof node/expr?)]) node/expr?]{
  Constructs an expression sketch, which is an unknown relational expression.
  Possible completions of the expression sketch are all relational expressions
  of arity @racket[arity] whose AST is @racket[depth] levels deep,
  with leaf nodes drawn from @racket[terminals]
  and non-leaf nodes drawn from @racket[operators].
}
  
@(kill-evaluator my-eval)