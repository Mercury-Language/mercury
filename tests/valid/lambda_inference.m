% regression test for inference and lambda modes.
:- module lambda_inference.

:- interface.


:- pred ok(int, pred(int)).
:- mode ok(in, free -> pred(in) is semidet) is semidet.

:- implementation.

ok(Var, (pred(X::in) is semidet :- X = 5)) :-
	inferred(Var).

:- pred inferred(int).

inferred(5).
