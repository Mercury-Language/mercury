:- module higher_order_implied_mode.
:- interface.
:- pred p is semidet.
:- implementation.
p :- (X = 1 ; X = 2), r(q(X)).

:- pred q(int::in, int::out) is det.
q(X, X).

:- pred r(pred(int)).
:- mode r(pred(out) is det) is semidet.
r(P) :- call(P, 42).
