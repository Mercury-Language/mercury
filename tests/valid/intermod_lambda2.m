% Regression test for higher-order terms exported using 
% inter-module optimization.
:- module intermod_lambda2.

:- interface.

:- import_module list, set.

%-----------------------------------------------------------------------------%

:- pred sol(pred(T), list(T)).
:- mode sol(pred(out) is det, out) is det.
:- mode sol(pred(out) is det, out) is det.

:- implementation.

sol(Generator, List) :-
	t(Generator, cons, [], List).

:- pred cons(T::in, list(T)::in, list(T)::out) is det.
cons(H, T, [H|T]).

:- pred t(pred(T), pred(T,T2,T2), T2, T2).
:- mode t(pred(out) is det, pred(in,in,out) is det, in, out) is det.

t(_, _, A, A).

%-----------------------------------------------------------------------------%
