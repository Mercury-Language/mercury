% Some versions of the compiler get the solution context wrong: they think
% that since the call to p2 and its check unification are inside a cut,
% the call to p1 ought to be inside a cut as well.

:- module soln_context.

:- interface.

:- pred q is semidet.

:- implementation.

q :- p1(X, Y), p2(X, Y).

:- pred p1(int, int).
:- mode p1(free >> free, out) is nondet.

:- pred p2(int, int).
:- mode p2(out, in) is nondet.

:- external(p1/2).
:- external(p2/2).
