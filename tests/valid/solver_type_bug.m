% This is a regression test.
% Previous versions of the Mercury compiler would
% incorrectly infer the determinism of p2 as semidet,
% resulting in a warning (and invalid code generation).

:- module solver_type_bug.
:- interface.

:- solver type foo1.
:- pragma foreign_type("C", foo1, "int").
:- pragma foreign_type("IL", foo1, "int32").
:- pragma foreign_type("Java", foo1, "int").

:- type foo2 ---> foo2(foo1).

:- pred p1(foo1).
:- mode p1(in(any)) is nondet.

:- pred p2(foo2).
:- mode p2(in(any)) is nondet.

:- implementation.

p1(X) :- q1(X).

p2(X) :- q2(X).

:- pred q1(foo1).
:- mode q1(in(any)) is nondet.
:- external(q1/1).

:- pred q2(foo2).
:- mode q2(in(any)) is nondet.
:- external(q2/1).
