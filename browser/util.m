%---------------------------------------------------------------------------%
% Copyright (C) 1998 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

:- module util.

:- interface.

:- import_module list.

:- pred util__zip_with(pred(T1, T2, T3), list(T1), list(T2), list(T3)).
:- mode util__zip_with(pred(in, in, out) is det, in, in, out) is det.

	% Apply predicate to argument repeatedly until the result
	% remains the same.
:- pred util__limit(pred(list(T), list(T)), list(T), list(T)).
:- mode util__limit(pred(in,out) is det, in, out) is det.

%---------------------------------------------------------------------------%
:- implementation.

:- import_module list, int, require.

util__zip_with(Pred, XXs, YYs, Zipped) :-
	( (XXs = [], YYs = []) ->
		Zipped = []
	; (XXs = [X|Xs], YYs = [Y|Ys]) ->
		Pred(X,Y,PXY),
		Zipped = [PXY|Rest],
		util__zip_with(Pred, Xs, Ys, Rest)
	;
		error("zip_with: list arguments are of unequal length")
	).

util__limit(Pred, Xs, Ys) :-
	Pred(Xs, Zs),
	( Xs = Zs ->
		Ys = Zs
	;
		util__limit(Pred, Zs, Ys)
	).

%---------------------------------------------------------------------------%
