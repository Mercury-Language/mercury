%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001, 2005-2006, 2008, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Authors: conway, zs.
%
% This module contains utility predicates for handling arrays.

:- module array_util.

:- interface.

:- import_module array.
:- import_module list.

%---------------------------------------------------------------------------%

    % Perform a mode cast on the given array, making the compiler believe that
    % the ground array is unique. Should be used only if the only use of the
    % old value is as input to the upcoming destructive operation that needs
    % the array to be unique. Otherwise, calling this function is dangerous.
    %
:- func u(T) = T.
:- mode (u(in) = array_uo) is det.

    % Performs a foldl on all the elements of the given array, starting at
    % index 1.
    %
:- pred array_foldl_from_1(pred(int, T, U, U), array(T), U, U).
:- mode array_foldl_from_1(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl_from_1(pred(in, in, array_di, array_uo) is det, in,
    array_di, array_uo) is det.
:- mode array_foldl_from_1(pred(in, in, in, out) is det, in, in, out) is det.

    % Performs a foldl on all the elements of the given array, starting at
    % index 0.
    %
:- pred array_foldl_from_0(pred(int, T, U, U), array(T), U, U).
:- mode array_foldl_from_0(pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl_from_0(pred(in, in, array_di, array_uo) is det, in,
    array_di, array_uo) is det.
:- mode array_foldl_from_0(pred(in, in, in, out) is det, in, in, out) is det.

    % Performs a foldl on all the elements of the given array between the two
    % index values given by the first two arguments, both inclusive.
    %
:- pred array_foldl(int, int, pred(int, T, U, U), array(T), U, U).
:- mode array_foldl(in, in, pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode array_foldl(in, in, pred(in, in, array_di, array_uo) is det, in,
    array_di, array_uo) is det.
:- mode array_foldl(in, in, pred(in, in, in, out) is det, in, in, out) is det.

    % Performs a foldl2 on all the elements of the given array, starting at
    % index 1.
    %
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

    % Performs a foldl2 on all the elements of the given array between the two
    % index values given by the first two arguments, both inclusive.
    %
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

    % Performs a foldl3 on all the elements of the given array, starting at
    % index 1.
    %
:- pred array_foldl3_from_1(pred(int, T, U, U, V, V, W, W), array(T), U, U,
    V, V, W, W).
:- mode array_foldl3_from_1(pred(in, in, array_di, array_uo,
    array_di, array_uo, array_di, array_uo) is det,
    in, array_di, array_uo, array_di, array_uo, array_di, array_uo) is det.

    % Performs the same computation as list.foldl; the only difference is
    % that the accumulator is an array and has an array mode.
    %
:- pred array_list_foldl(pred(T, array(U), array(U)), list(T),
    array(U), array(U)).
:- mode array_list_foldl(pred(in, array_di, array_uo) is det, in,
    array_di, array_uo) is det.

    % Performs the same computation as list.foldl2; the only difference is
    % that the accumulators are arrays and have array modes.
    %
:- pred array_list_foldl2(pred(T, array(U), array(U), array(V), array(V)),
    list(T), array(U), array(U), array(V), array(V)).
:- mode array_list_foldl2(pred(in, array_di, array_uo, array_di, array_uo)
    is det, in, array_di, array_uo, array_di, array_uo) is det.

    % Performs a map on all the elements of the given array, starting at index
    % 0.
    %
:- pred array_map_from_0(pred(T, T), array(T), array(T)).
:- mode array_map_from_0(pred(in, out) is det, array_di, array_uo) is det.

    % Performs a map on all the elements of the given array, starting at index
    % 1.
    %
:- pred array_map_from_1(pred(T, T), array(T), array(T)).
:- mode array_map_from_1(pred(in, out) is det, array_di, array_uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

%---------------------------------------------------------------------------%

:- pragma foreign_proc("C",
    u(A::in) = (B::array_uo),
    [will_not_call_mercury, thread_safe, promise_pure],
"
    B = A;
").

array_foldl_from_1(P, A, !AccU) :-
    array.max(A, Max),
    do_array_foldl(1, Max, P, A, !AccU).

array_foldl_from_0(P, A, !AccU) :-
    array.max(A, Max),
    do_array_foldl(0, Max, P, A, !AccU).

array_foldl(N, Max, P, A, !AccU) :-
    do_array_foldl(N, Max, P, A, !AccU).

    % This clone of array_foldl exists to allow looping to take place
    % within a shallow traced predicate in debug grades. This avoids
    % blowing the stack with deep recursion.
    %
:- pred do_array_foldl(int, int, pred(int, T, U, U), array(T), U, U).
:- mode do_array_foldl(in, in, pred(in, in, di, uo) is det, in, di, uo) is det.
:- mode do_array_foldl(in, in, pred(in, in, array_di, array_uo) is det, in,
    array_di, array_uo) is det.
:- mode do_array_foldl(in, in, pred(in, in, in, out) is det, in,
    in, out) is det.

do_array_foldl(N, Max, P, A, !AccU) :-
    ( N =< Max ->
        array.lookup(A, N, E),
        P(N, E, !AccU),
        do_array_foldl(N + 1, Max, P, A, !AccU)
    ;
        true
    ).

array_foldl2_from_1(P, A, !AccU, !AccV) :-
    array.max(A, Max),
    do_array_foldl2(1, Max, P, A, !AccU, !AccV).

array_foldl2(N, Max, P, A, !AccU, !AccV) :-
    do_array_foldl2(N, Max, P, A, !AccU, !AccV).

    % This clone of array_foldl2 exists to allow looping to take place
    % within a shallow traced predicate in debug grades. This avoids
    % blowing the stack with deep recursion.
    %
:- pred do_array_foldl2(int, int, pred(int, T, U, U, V, V), array(T),
    U, U, V, V).
:- mode do_array_foldl2(in, in, pred(in, in, di, uo, di, uo) is det, in,
    di, uo, di, uo) is det.
:- mode do_array_foldl2(in, in, pred(in, in,
    array_di, array_uo, array_di, array_uo) is det, in,
    array_di, array_uo, array_di, array_uo) is det.
:- mode do_array_foldl2(in, in, pred(in, in,
    array_di, array_uo, in, out) is det, in,
    array_di, array_uo, in, out) is det.
:- mode do_array_foldl2(in, in, pred(in, in, in, out, di, uo) is det, in,
    in, out, di, uo) is det.
:- mode do_array_foldl2(in, in, pred(in, in, in, out, in, out) is det, in,
    in, out, in, out) is det.

do_array_foldl2(N, Max, P, A, !AccU, !AccV) :-
    ( N =< Max ->
        array.lookup(A, N, E),
        P(N, E, !AccU, !AccV),
        do_array_foldl2(N + 1, Max, P, A, !AccU, !AccV)
    ;
        true
    ).

%---------------------------------------------------------------------------%

array_foldl3_from_1(P, Array, !A, !B, !C) :-
    array.max(Array, Max),
    do_array_foldl3(1, Max, P, Array, !A, !B, !C).

:- pred do_array_foldl3(int, int, pred(int, T, U, U, V, V, W, W), array(T),
    U, U, V, V, W, W).
:- mode do_array_foldl3(in, in, pred(in, in, array_di, array_uo,
    array_di, array_uo, array_di, array_uo) is det, in,
    array_di, array_uo, array_di, array_uo, array_di, array_uo) is det.

do_array_foldl3(N, Max, P, Array, !A, !B, !C) :-
    ( N =< Max ->
        array.lookup(Array, N, E),
        P(N, E, !A, !B, !C),
        do_array_foldl3(N + 1, Max, P, Array, !A, !B, !C)
    ;
        true
    ).

%---------------------------------------------------------------------------%

array_list_foldl(_, [], !Acc).
array_list_foldl(P, [X | Xs], !Acc) :-
    P(X, !Acc),
    array_list_foldl(P, Xs, !Acc).

array_list_foldl2(_, [], !AccU, !AccV).
array_list_foldl2(P, [X | Xs], !AccU, !AccV) :-
    P(X, !AccU, !AccV),
    array_list_foldl2(P, Xs, !AccU, !AccV).

array_map_from_0(P, !AccU) :-
    array.max(!.AccU, Max),
    array_map(0, Max, P, !AccU).

array_map_from_1(P, !AccU) :-
    array.max(!.AccU, Max),
    array_map(1, Max, P, !AccU).

:- pred array_map(int, int, pred(T, T), array(T), array(T)).
:- mode array_map(in, in, pred(in, out) is det, array_di, array_uo) is det.

array_map(N, Size, Closure, !Array) :-
    ( N >= Size ->
        true
    ;
        array.lookup(!.Array, N, OldElem),
        Closure(OldElem, NewElem),
        array.set(N, NewElem, !Array),
        array_map(N + 1, Size, Closure, !Array)
    ).

%---------------------------------------------------------------------------%
:- end_module array_util.
%---------------------------------------------------------------------------%
