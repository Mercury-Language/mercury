%----------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%

% utils.m - some utility functions that use higher order predicates.

% NB We need to do quite a bit of work on polymorphic modes
% and higher-order preds before this code will work.

% main author: conway.

%----------------------------------------------------------------------------%
:- module utils.

:- interface.

:- import_module list, std_util.

:- mode in(I) :: I -> I.
:- mode out(I) :: free -> I.

:- pred map(pred(T1, T2), list(T1), list(T2)).
:- mode map(det_call(in, out),	in(list_skel), out(list_skel)) is det.
:- mode map(semidet_call(in, out), in(list_skel), out(list_skel)) is semidet.
:- mode map(nondet_call(in, out), in(list_skel), out(list_skel)) is nondet.

%	map(Pred, InputList, OutputList)
%		applies Pred to each member of InputList to yeild the 
%		corresponding member of OutputList.

:- pred fold(pred(T, T, T), T, list(T), T).
:- mode fold(det_call(in, in, out), in, in, out) is det.
:- mode fold(semidet_call(in, in, out), in, in, out) is semidet.
:- mode fold(nondet_call(in, in, out), in, in, out) is nondet.
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

map(_Pred, [], []).
map(Pred, [X|Xs], [Y|Ys]) :-
	call(Pred, X, Y),
	map(Pred, Xs, Ys).

fold(_Pred, Identity, [], Identity).
fold(Pred, Identity, [X|Xs], Acc) :-
	fold(Pred, Identity, Xs, Acc0),
	call(Pred, X, Acc0, Acc).

%----------------------------------------------------------------------------%
