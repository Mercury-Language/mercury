%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2001, 2003, 2005-2006, 2010-2011 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% Author: conway.
%
% This module provides an ADT for storing dense sets of small integers.
% The sets are represented as bit vectors, which are implemented as arrays
% of integers.
%
% We should think about replacing this module with library/bitmap.m.
%
%---------------------------------------------------------------------------%

:- module dense_bitset.
:- interface.

:- import_module array.

%---------------------------------------------------------------------------%

% XXX should be abstract type; it is exported only as a workaround.
:- type dense_bitset == array(int).

:- func init = dense_bitset.
:- mode (init = array_uo) is det.

:- pred member(int, dense_bitset).
:- mode member(in, array_ui) is semidet.

:- func insert(dense_bitset, int) = dense_bitset.
:- mode (insert(array_di, in) = array_uo) is det.

:- func delete(dense_bitset, int) = dense_bitset.
:- mode (delete(array_di, in) = array_uo) is det.

:- func union(dense_bitset, dense_bitset) = dense_bitset.
:- mode (union(array_di, array_di) = array_uo) is det.

% Not yet implemented.
% :- func intersection(dense_bitset, dense_bitset) = dense_bitset.
% :- mode (intersection(array_di, array_di) = array_uo) is det.

% Not yet implemented.
% :- func difference(dense_bitset, dense_bitset) = dense_bitset.
% :- mode (difference(array_di, array_di) = array_uo) is det.

:- pred foldl(pred(int, T, T), dense_bitset, T, T).
:- mode foldl(pred(in, in, out) is det, array_ui, in, out) is det.
:- mode foldl(pred(in, di, uo) is det, array_ui, di, uo) is det.
:- mode foldl(pred(in, array_di, array_uo) is det, array_ui,
        array_di, array_uo) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module require.

%---------------------------------------------------------------------------%

init = array([0]).

member(I, A) :-
    max(A, Max),
    ( word(I) >= 0, word(I) =< Max ->
        lookup(A, word(I), Word),
        bit(I) /\ Word \= 0
    ;
        fail
    ).

insert(A0, I) = A :-
    max(A0, Max),
    ( word(I) > Max ->
        resize((Max + 1) * 2, 0, A0, A1),
        A = insert(A1, I)
    ; I >= 0 ->
        lookup(A0, word(I), Word0),
        Word = Word0 \/ bit(I),
        set(word(I), Word, A0, A)
    ;
        unexpected($module, $pred, "cannot use indexes < 0")
    ).

delete(A0, I) = A :-
    max(A0, Max),
    ( I > Max ->
        A = A0
    ; I >= 0 ->
        lookup(A0, word(I), Word0),
        Word = Word0 /\ \ bit(I),
        set(word(I), Word, A0, A)
    ;
        unexpected($module, $pred, "cannot use indexes < 0")
    ).

union(A, B) = C :-
    dense_bitset.foldl((pred(I::in, C0::in, C1::out) is det :-
        C1 = insert(C0, I)
    ), A, B, C).

foldl(P, A0, !Acc) :-
    max(A0, Max),
    foldl1(0, Max, P, A0, !Acc).

:- pred foldl1(int, int, pred(int, T, T), dense_bitset, T, T).
:- mode foldl1(in, in, pred(in, in, out) is det, array_ui, in, out) is det.
:- mode foldl1(in, in, pred(in, di, uo) is det, array_ui, di, uo) is det.
:- mode foldl1(in, in, pred(in, array_di, array_uo) is det, array_ui,
        array_di, array_uo) is det.

foldl1(Min, Max, P, A0, !Acc) :-
    ( Min =< Max ->
        foldl2(0, Min, P, A0, !Acc),
        foldl1(Min + 1, Max, P, A0, !Acc)
    ;
        true
    ).

:- pred foldl2(int, int, pred(int, T, T), dense_bitset, T, T).
:- mode foldl2(in, in, pred(in, in, out) is det, array_ui, in, out) is det.
:- mode foldl2(in, in, pred(in, di, uo) is det, array_ui, di, uo) is det.
:- mode foldl2(in, in, pred(in, array_di, array_uo) is det, array_ui,
        array_di, array_uo) is det.

foldl2(B, W, P, A0, !Acc) :-
    ( B =< 31 ->
        lookup(A0, W, Word),
        ( (1 << B) /\ Word \= 0 ->
            I = B + W * 32,
            P(I, !Acc)
        ;
            true
        ),
        foldl2(B + 1, W, P, A0, !Acc)
    ;
        true
    ).

:- func word(int) = int.

word(I) = I // 32.

:- func bit(int) = int.

bit(I) = (1 << (I /\ 31)).

%---------------------------------------------------------------------------%
:- end_module dense_bitset.
%---------------------------------------------------------------------------%
