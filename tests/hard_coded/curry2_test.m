% This module defines a bunch of predicates used to test the handling
% of currying.

:- module curry2_test.
:- interface.

:- pred n(pred(int, T2, T3), pred(T2, T3)).
:- mode n(pred(in, in, out) is det, free >> (pred(in, out) is det)) is det.
:- pred p(pred(int, T2, T3), pred(T2, T3)).
:- mode p(pred(in, in, out) is det, free >> (pred(in, out) is det)) is det.
:- pred q(pred(int, T2, T3), pred(T2, T3)).
:- mode q(pred(in, in, out) is det, free >> (pred(in, out) is det)) is semidet.
:- pred r(pred(int, int, int), int, pred(int, int)).
:- mode r(pred(in, in, out) is det, in, free >> (pred(in, out) is det)) is det.
:- pred s(pred(int, int, int), int, pred(int, int)).
:- mode s(pred(in, out, in) is det, in, free >> (pred(out, in) is det)) is det.
:- pred t(pred(int, int, int), int, pred(int, int)).
:- mode t(pred(in, out, in) is det, in, free >> (pred(in, out) is det)) is det.

:- implementation.

n(Pred, Pred2) :-
	Pred2 = (pred(B::in, C::out) is det :- call(Pred, 42, B, C)).
p(Pred, Pred2) :-
	Arg = 42,
	Pred2 = (pred(B::in, C::out) is det :- call(Pred, Arg, B, C)).
q(Pred, Pred2) :- % semidet
	Arg = 42,
	Pred2 = (pred(B::in, C::out) is det :- call(Pred, Arg, B, C)).
r(Pred, X, Pred2) :-
	Pred2 = (pred(B::in, C::out) is det :- call(Pred, X, B, C)).
s(Pred, X, Pred2) :-
	Pred2 = (pred(B::out, C::in) is det :- call(Pred, X, B, C)).
t(Pred, X, Pred2) :-
	Pred2 = (pred(B::in, C::out) is det :- call(Pred, X, C, B)).
