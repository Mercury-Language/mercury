%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
% utils.nl - some utility functions that use higher order predicates.

% main author: conway.

%----------------------------------------------------------------------------%
:- module utils.

:- interface.

:- import_module list, std_util.

:- pred map(pred(T1, T2), list(T1), list(T2)).
:- mode map(pred_call(I, O), list_skel(I), list_skel(O)).
%	map(Pred, InputList, OutputList)
%		applies Pred to each member of InputList to yeild the 
%		corresponding member of OutputList.

:- pred fold(pred(T, T, T), T, list(T), T).
:- mode fold(pred_call(in, in, out), in, in, out).
%	fold(Pred, Identity, Inputs, Result)
%		fold applies Pred to each member of Inputs and accumulates
%		the result. The base case is given as the Identity.
%		for example:
%			fold((+), 0, IntList, Sum) yields the sum of
%		the members of IntList in Sum.
%		NOTE: the next value to be accumulated is always passed
%		as the first argument to Pred, and the current accumulation
%		second.

%----------------------------------------------------------------------------%

:- implementation.

map(Pred, [], []).
map(Pred, [X|Xs], [Y|Ys]) :-
	call(Pred, X, Y),
	map(Pred, Xs, Ys).

fold(Pred, Identity, [], Identity).
fold(Pred, Identity, [X|Xs], Acc) :-
	fold(Pred, Identity, Xs, Acc0),
	call(Pred, X, Acc0, Acc).




%----------------------------------------------------------------------------%
