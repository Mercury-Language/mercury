%-----------------------------------------------------------------------------%
% Copyright (C) 2001, 2005 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains utility predicates for handling arrays.

:- module array_util.

:- interface.

:- import_module array.
:- import_module list.

	% Perform a mode cast on the given array, making the compiler believe
	% that the ground array is unique. Should be used only if the only use
	% of the old value is as input to the upcoming destructive operation
	% that needs the array to be unique. Otherwise, calling this function
	% is dangerous.
:- func u(T) = T.
:- mode (u(in) = array_uo) is det.

	% Performs a foldl on all the elements of the given array,
	% starting at index 1.
:- pred array_foldl_from_1(pred(int, T, U, U), array(T), U, U).
:- mode array_foldl_from_1(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl_from_1(pred(in, in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.
:- mode array_foldl_from_1(pred(in, in, in, out) is det, in, in, out) is det.

	% Performs a foldl on all the elements of the given array,
	% starting at index 0.
:- pred array_foldl_from_0(pred(int, T, U, U), array(T), U, U).
:- mode array_foldl_from_0(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl_from_0(pred(in, in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.
:- mode array_foldl_from_0(pred(in, in, in, out) is det, in, in, out) is det.

	% Performs a foldl on all the elements of the given array
	% between the two index values given by the first two arguments,
	% both inclusive.
:- pred array_foldl(int, int, pred(int, T, U, U), array(T), U, U).
:- mode array_foldl(in, in, pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl(in, in, pred(in, in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.
:- mode array_foldl(in, in, pred(in, in, in, out) is det, in, in, out) is det.

	% Performs a foldl2 on all the elements of the given array,
	% starting at index 1.
:- pred array_foldl2_from_1(pred(int, T, U, U, V, V), array(T), U, U, V, V).
:- mode array_foldl2_from_1(pred(in, in, di, uo, di, uo) is det,
	in, di, uo, di, uo) is det.
:- mode array_foldl2_from_1(pred(in, in, array_di, array_uo,
	array_di, array_uo) is det,
	in, array_di, array_uo, array_di, array_uo) is det.
:- mode array_foldl2_from_1(pred(in, in, array_di, array_uo, in, out) is det,
	in, array_di, array_uo, in, out) is det.
:- mode array_foldl2_from_1(pred(in, in, in, out, di, uo) is det,
	in, in, out, di, uo) is det.
:- mode array_foldl2_from_1(pred(in, in, in, out, in, out) is det,
	in, in, out, in, out) is det.

	% Performs a foldl2 on all the elements of the given array
	% between the two index values given by the first two arguments,
	% both inclusive.
:- pred array_foldl2(int, int, pred(int, T, U, U, V, V), array(T), U, U, V, V).
:- mode array_foldl2(in, in, pred(in, in, di, uo, di, uo) is det, in,
	di, uo, di, uo) is det.
:- mode array_foldl2(in, in, pred(in, in,
	array_di, array_uo, array_di, array_uo) is det, in,
	array_di, array_uo, array_di, array_uo) is det.
:- mode array_foldl2(in, in, pred(in, in,
	array_di, array_uo, in, out) is det, in,
	array_di, array_uo, in, out) is det.
:- mode array_foldl2(in, in, pred(in, in, in, out, di, uo) is det, in,
	in, out, di, uo) is det.
:- mode array_foldl2(in, in, pred(in, in, in, out, in, out) is det, in,
	in, out, in, out) is det.

	% Performs the same computation as list__foldl; the only difference
	% is that the accumulator is an array and has an array mode.
:- pred array_list_foldl(pred(T, array(U), array(U)), list(T),
	array(U), array(U)).
:- mode array_list_foldl(pred(in, array_di, array_uo) is det, in,
	array_di, array_uo) is det.

	% Performs the same computation as list__foldl2; the only difference
	% is that the accumulators are arrays and have array modes.
:- pred array_list_foldl2(pred(T, array(U), array(U), array(V), array(V)),
	list(T), array(U), array(U), array(V), array(V)).
:- mode array_list_foldl2(pred(in, array_di, array_uo, array_di, array_uo)
	is det, in, array_di, array_uo, array_di, array_uo) is det.

	% Performs a map on all the elements of the given array,
	% starting at index 0.
:- pred array_map_from_0(pred(T, T), array(T), array(T)).
:- mode array_map_from_0(pred(in, out) is det, array_di, array_uo) is det.

	% Performs a map on all the elements of the given array,
	% starting at index 1.
:- pred array_map_from_1(pred(T, T), array(T), array(T)).
:- mode array_map_from_1(pred(in, out) is det, array_di, array_uo) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module string.

:- pragma foreign_proc("C",
	u(A::in) = (B::array_uo),
	[will_not_call_mercury, thread_safe, promise_pure],
"
	B = A;
").

array_foldl_from_1(P, A, U0, U) :-
	array__max(A, Max),
	array_foldl(1, Max, P, A, U0, U).

array_foldl_from_0(P, A, U0, U) :-
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

array_foldl2_from_1(P, A, U0, U, V0, V) :-
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

array_map_from_0(P, U0, U) :-
	array__max(U0, Max),
	array_map(0, Max, P, U0, U).

array_map_from_1(P, U0, U) :-
	array__max(U0, Max),
	array_map(1, Max, P, U0, U).

:- pred array_map(int, int, pred(T, T), array(T), array(T)).
:- mode array_map(in, in, pred(in, out) is det, array_di, array_uo) is det.

array_map(N, Size, Closure, Array0, Array) :-
	( N >= Size ->
		Array = Array0
	;
		array__lookup(Array0, N, OldElem),
		Closure(OldElem, NewElem),
		array__set(Array0, N, NewElem, Array1),
		array_map(N + 1, Size, Closure, Array1, Array)
	).
