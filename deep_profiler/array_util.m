%-----------------------------------------------------------------------------%
% Copyright (C) 2001 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains utility predicates for handling arrays.

:- module array_util.

:- interface.

:- import_module array, list.

:- func u(T) = T.
:- mode (u(in) = array_uo) is det.

:- pred array_foldl(pred(int, T, U, U), array(T), U, U).
:- mode array_foldl(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl(pred(in, in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.
:- mode array_foldl(pred(in, in, in, out) is det, in, in, out) is det.

:- pred array_foldl0(pred(int, T, U, U), array(T), U, U).
:- mode array_foldl0(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl0(pred(in, in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.
:- mode array_foldl0(pred(in, in, in, out) is det, in, in, out) is det.

:- pred array_foldl(int, int, pred(int, T, U, U), array(T), U, U).
:- mode array_foldl(in, in, pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl(in, in, pred(in, in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.
:- mode array_foldl(in, in, pred(in, in, in, out) is det, in, in, out) is det.

:- pred array_foldl2(pred(int, T, U, U, V, V), array(T), U, U, V, V).
:- mode array_foldl2(pred(in, in, di, uo, di, uo) is det, in, di, uo, di, uo)
	is det.
:- mode array_foldl2(pred(in, in, array_di, array_uo, array_di, array_uo)
	is det, in, array_di, array_uo, array_di, array_uo)
	is det.
:- mode array_foldl2(pred(in, in, in, out, di, uo) is det, in, in, out, di, uo)
	is det.

:- pred array_foldl2(int, int, pred(int, T, U, U, V, V), array(T), U, U, V, V).
:- mode array_foldl2(in, in, pred(in, in, di, uo, di, uo) is det, in,
	di, uo, di, uo) is det.
:- mode array_foldl2(in, in, pred(in, in,
	array_di, array_uo, array_di, array_uo) is det, in,
	array_di, array_uo, array_di, array_uo) is det.
:- mode array_foldl2(in, in, pred(in, in, in, out, di, uo) is det, in,
	in, out, di, uo) is det.

:- pred array_list_foldl(pred(T, array(U), array(U)), list(T),
	array(U), array(U)).
:- mode array_list_foldl(pred(in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.

:- pred array_list_foldl2(pred(T, array(U), array(U), array(V), array(V)),
	list(T), array(U), array(U), array(V), array(V)).
:- mode array_list_foldl2(pred(in, array_di, array_uo, array_di, array_uo)
	is det, in, array_di, array_uo, array_di, array_uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, string.

:- pragma foreign_proc("C", u(A::in) = (B::array_uo),
	[will_not_call_mercury, thread_safe],
	"B = A;"
).

array_foldl(P, A, U0, U) :-
	array__max(A, Max),
	array_foldl(1, Max, P, A, U0, U).

array_foldl0(P, A, U0, U) :-
	array__max(A, Max),
	array_foldl(0, Max, P, A, U0, U).

array_foldl(N, Max, P, A, U0, U) :-
	( N =< Max ->
		array__lookup(A, N, E),
		call(P, N, E, U0, U1),
		array_foldl(N + 1, Max, P, A, U1, U)
	;
		U = U0
	).

array_foldl2(P, A, U0, U, V0, V) :-
	array__max(A, Max),
	array_foldl2(1, Max, P, A, U0, U, V0, V).

array_foldl2(N, Max, P, A, U0, U, V0, V) :-
	( N =< Max ->
		array__lookup(A, N, E),
		call(P, N, E, U0, U1, V0, V1),
		array_foldl2(N + 1, Max, P, A, U1, U, V1, V)
	;
		U = U0,
		V = V0
	).

array_list_foldl(_, [], Acc, Acc).
array_list_foldl(P, [X | Xs], Acc0, Acc) :-
	call(P, X, Acc0, Acc1),
	array_list_foldl(P, Xs, Acc1, Acc).

array_list_foldl2(_, [], AccU, AccU, AccV, AccV).
array_list_foldl2(P, [X | Xs], AccU0, AccU, AccV0, AccV) :-
	call(P, X, AccU0, AccU1, AccV0, AccV1),
	array_list_foldl2(P, Xs, AccU1, AccU, AccV1, AccV).
