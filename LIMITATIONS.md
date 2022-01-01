LIMITATIONS
-----------

The current implementation does not yet completely implement the
Mercury language. The main limitations of the current implementation
are the following:

* We do not allow definite aliasing in the mode system. That is, we do not
  allow the unification of two free variables. Without this, partially
  instantiated terms (i.e. terms which contain both function symbols and
  free variables) are unusable, and so are nested unique modes :-(
  When partial instantiation appears to work, it is only by chance.

  The detailed explanation is:

  > If you unify a free variable (say `X`) and an argument of a term
  > which is also free at the time (say the third argument of `g` in `g(a, b, Y)`),
  > then when either the variable `X` or the argument `Y` is bound, the other
  > should be bound to the same thing. In the absence of a recorded
  > alias between `X` and `Y`, this won't happen. The only situation in which
  > this is ok is if "the other" does not exist anymore, which the current
  > compiler can be sure about only if its scope is restricted to the
  > unification (i.e. if whichever of `X` and `Y` is "the other" appears
  > *only* in the unification, and nowhere else).

  > Even for code in which a free variable's scope is confined to its
  > unification with a free variable in a term, the code generators are
  > not guaranteed to handle the resulting partially instantiated terms
  > correctly, simply because they weren't *designed* to. Mantis bug 311
  > shows this very clearly. Though I can imagine the different backends
  > misbehaving differently on such code. Some may even generate correct
  > code by accident :-)

  This does not affect support for instantiation state subtyping.

* The compiler does not yet use structure reuse or compile-time
  garbage collection to exploit unique modes :-(

* Type inference and mode inference are a bit imperfect.

We hope to eliminate all of these problems.

In addition, design decisions in this implementation have imposed the
following fixed limits:

* Predicates and functions can have at most about 1000 arguments.

* Higher-order terms are limited to arity of about 500.

These limits can be lifted (with some effort), but would possibly incur
performance penalties. Contact the Mercury team (<mercury@mercurylang.org>)
if you find these limits are affecting your application.
