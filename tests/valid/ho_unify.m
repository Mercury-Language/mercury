% The compiler of 20/9/1999 aborted on this test case
% due to higher_order.m erroneously adding type-info arguments
% when specializing the call to unify/2 into a call to
% builtin_unify_pred/2.
:- module ho_unify.

:- interface.

:- pred ho_unify(pred(int), pred(int)). 
:- mode ho_unify(pred(in) is semidet, pred(in) is semidet) is semidet.

:- implementation.

ho_unify(X, Y) :-
	unify(X, Y).
