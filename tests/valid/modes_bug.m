/*
Regression test.

$ mc modes_bug.m
In clause for `'__Unify__'((bound(foo(unique(42))) -> bound(foo(unique(42)))), (bound(foo(unique(42))) -> bound(foo(unique(42)))))':
  mode error: argument 1 did not get sufficiently instantiated.
  Final instantiatedness of `V_1' was `bound(foo(bound(42)))',
  expected final instantiatedness was `bound(foo(unique(42)))'.
Software error: mode error in compiler-generated unification predicate
*/
:- module modes_bug.

:- interface.

:- type foo ---> foo(int).
:- type bar ---> bar(foo) ; baz.

:- pred test(bar::out) is det.

:- implementation.
test(Y) :-
	Y = bar(_),
	Z = 42,
	Y2 = foo(Z),
	Y = bar(Y2),
	Y2 = foo(_),
	Y = bar(Y2).
