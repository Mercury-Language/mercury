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

:- pred insert(int::in, dense_bitset::array_di, dense_bitset::array_uo) is det.

:- pred delete(int::in, dense_bitset::array_di, dense_bitset::array_uo) is det.

:- pred union(dense_bitset::array_di, dense_bitset::array_di,
    dense_bitset::array_uo) is det.

% Not yet implemented.
% :- func intersection(dense_bitset, dense_bitset) = dense_bitset.
% :- mode (intersection(array_di, array_di) = array_uo) is det.

% Not yet implemented.
% :- func difference(dense_bitset, dense_bitset) = dense_bitset.
% :- mode (difference(array_di, array_di) = array_uo) is det.

:- pred foldl(pred(int, T, T), dense_bitset, T, T).
:- mode foldl(in(pred(in, in, out) is det),
    array_ui, in, out) is det.
:- mode foldl(in(pred(in, di, uo) is det),
    array_ui, di, uo) is det.
:- mode foldl(in(pred(in, array_di, array_uo) is det),
    array_ui, array_di, array_uo) is det.

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
    ( if word(I) >= 0, word(I) =< Max then
        lookup(A, word(I), Word),
        bit(I) /\ Word \= 0
    else
        fail
    ).

insert(I, A0, A) :-
    max(A0, Max),
    ( if word(I) > Max then
        resize((Max + 1) * 2, 0, A0, A1),
        insert(I, A1, A)
    else if I >= 0 then
        lookup(A0, word(I), Word0),
        Word = Word0 \/ bit(I),
        set(word(I), Word, A0, A)
    else
        unexpected($pred, "cannot use indexes < 0")
    ).

delete(I, A0, A) :-
    max(A0, Max),
    ( if I > Max then
        A = A0
    else if I >= 0 then
        lookup(A0, word(I), Word0),
        Word = Word0 /\ \ bit(I),
        set(word(I), Word, A0, A)
    else
        unexpected($pred, "cannot use indexes < 0")
    ).

union(A, B, C) :-
    dense_bitset.foldl(dense_bitset.insert, A, B, C).

foldl(P, A0, !Acc) :-
    max(A0, Max),
    foldl1(0, Max, P, A0, !Acc).

:- pred foldl1(int, int, pred(int, T, T), dense_bitset, T, T).
:- mode foldl1(in, in, in(pred(in, in, out) is det),
    array_ui, in, out) is det.
:- mode foldl1(in, in, in(pred(in, di, uo) is det),
    array_ui, di, uo) is det.
:- mode foldl1(in, in, in(pred(in, array_di, array_uo) is det),
    array_ui, array_di, array_uo) is det.

foldl1(Min, Max, P, A0, !Acc) :-
    ( if Min =< Max then
        foldl2(0, Min, P, A0, !Acc),
        foldl1(Min + 1, Max, P, A0, !Acc)
    else
        true
    ).

:- pred foldl2(int, int, pred(int, T, T), dense_bitset, T, T).
:- mode foldl2(in, in, in(pred(in, in, out) is det),
    array_ui, in, out) is det.
:- mode foldl2(in, in, in(pred(in, di, uo) is det),
    array_ui, di, uo) is det.
:- mode foldl2(in, in, in(pred(in, array_di, array_uo) is det),
    array_ui, array_di, array_uo) is det.

foldl2(B, W, P, A0, !Acc) :-
    ( if B =< 31 then
        lookup(A0, W, Word),
        ( if (1 << B) /\ Word \= 0 then
            I = B + W * 32,
            P(I, !Acc)
        else
            true
        ),
        foldl2(B + 1, W, P, A0, !Acc)
    else
        true
    ).

:- func word(int) = int.

word(I) = I // 32.

:- func bit(int) = int.

bit(I) = (1 << (I /\ 31)).

%---------------------------------------------------------------------------%
:- end_module dense_bitset.
%---------------------------------------------------------------------------%
