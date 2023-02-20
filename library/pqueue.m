%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 1999, 2003-2007, 2009 The University of
% Melbourne.
% Copyright (C) 2013-2018 The Mercury team.
% This file is distributed under the terms specified in COPYING.LIB.
%---------------------------------------------------------------------------%
%
% File: pqueue.m.
% Main author: conway.
% Stability: high.
%
% This module implements a priority queue ADT.
%
% A pqueue is a priority queue. A priority queue holds a collection
% of key-value pairs; the interface provides operations to create
% an empty priority queue, to insert a key-value pair into a priority
% queue, and to remove the element with the lowest key.
%
% Insertion/removal is not guaranteed to be "stable"; that is,
% if you insert two values with the same key, the order in which
% they will be removed is unspecified.
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module pqueue.
:- interface.

:- import_module assoc_list.

%---------------------------------------------------------------------------%

:- type pqueue(K, V).

    % Create an empty priority queue.
    %
:- func init = pqueue(K, V).
:- pred init(pqueue(K, V)::out) is det.

    % Succeed iff the priority queue is empty.
    %
:- pred is_empty(pqueue(K, V)::in) is semidet.

    % Extract the smallest key-value pair from the priority queue without
    % removing it. Fails if the priority queue is empty.
    %
:- pred peek(pqueue(K, V)::in, K::out, V::out) is semidet.

    % Extract the smallest key from the priority queue without removing it.
    % Fail if the priority queue is empty.
    %
:- pred peek_key(pqueue(K, V)::in, K::out) is semidet.

    % Extract the smallest value from the priority queue without removing it.
    % Fail if the priority queue is empty.
    %
:- pred peek_value(pqueue(K, V)::in, V::out) is semidet.

    % As above, but call error/1 if the priority queue is empty.
    %
:- pred det_peek(pqueue(K, V)::in, K::out, V::out) is det.
:- func det_peek_key(pqueue(K, V)) = K.
:- func det_peek_value(pqueue(K, V)) = V.

    % Insert a value V with key K into the given priority queue,
    % and return the updated priority queue.
    %
:- func insert(pqueue(K, V), K, V) = pqueue(K, V).
:- pred insert(K::in, V::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

    % Remove the smallest item from the priority queue.
    % Fail if the priority queue is empty.
    %
:- pred remove(K::out, V::out, pqueue(K, V)::in, pqueue(K, V)::out)
    is semidet.

    % As above, but calls error/1 if the priority queue is empty.
    %
:- pred det_remove(K::out, V::out, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

    % Merge all the entries of one priority queue with another,
    % returning the merged list.
    %
:- func merge(pqueue(K, V), pqueue(K, V)) = pqueue(K, V).
:- pred merge(pqueue(K, V)::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

    % Extract all the items from a priority queue by repeated removal,
    % and place them in an association list.
    %
:- func to_assoc_list(pqueue(K, V)) = assoc_list(K, V).
:- pred to_assoc_list(pqueue(K, V)::in, assoc_list(K, V)::out)
    is det.

    % Insert all the key-value pairs in an association list
    % into a priority queue.
    %
:- func assoc_list_to_pqueue(assoc_list(K, V)) = pqueue(K, V).
:- pred assoc_list_to_pqueue(assoc_list(K, V)::in, pqueue(K, V)::out)
    is det.

    % A synonym for assoc_list_to_pqueue/1.
    %
:- func from_assoc_list(assoc_list(K, V)) = pqueue(K, V).

    % length(PQueue) = Length.
    %
    % Length is the number of items in PQueue.
    %
:- func length(pqueue(K, V)) = int.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.
:- import_module list.
:- import_module pair.
:- import_module require.

%---------------------------------------------------------------------------%

:- type pqueue(K, V)
    --->    empty
    ;       pqueue(int, K, V, pqueue(K, V), pqueue(K, V)).

%---------------------------------------------------------------------------%

init = PQ :-
    pqueue.init(PQ).

init(empty).

%---------------------------------------------------------------------------%

is_empty(empty).

%---------------------------------------------------------------------------%

peek(pqueue(_, K, V, _, _), K, V).
peek_key(pqueue(_, K, _, _, _), K).
peek_value(pqueue(_, _, V, _, _), V).

%---------------------------------------------------------------------------%

det_peek(PQ, K, V) :-
    ( if pqueue.peek(PQ, KPrime, VPrime) then
        K = KPrime,
        V = VPrime
    else
        unexpected($file, $pred, "empty priority queue")
    ).

det_peek_key(PQ) = K :-
    pqueue.det_peek(PQ, K, _).
det_peek_value(PQ) = V :-
    pqueue.det_peek(PQ, _, V).

%---------------------------------------------------------------------------%

insert(!.PQ, K, V) = !:PQ :-
    pqueue.insert(K, V, !PQ).

insert(K, V, empty, pqueue(0, K, V, empty, empty)).
insert(K, V, pqueue(D0, K0, V0, L0, R0), PQ) :-
    D = D0 + 1,
    compare(CMP, K, K0),
    ( if CMP = (<) then
        K1 = K,
        V1 = V,
        pqueue.insert_2(K0, V0, L0, R0, L, R)
    else
        K1 = K0,
        V1 = V0,
        pqueue.insert_2(K, V, L0, R0, L, R)
    ),
    PQ = pqueue(D, K1, V1, L, R).

:- pred insert_2(K::in, V::in, pqueue(K, V)::in, pqueue(K, V)::in,
    pqueue(K, V)::out, pqueue(K, V)::out) is det.

insert_2(K, V, empty, empty, pqueue(0, K, V, empty, empty), empty).
insert_2(K, V, pqueue(D0, K0, V0, L0, R0), empty,
        pqueue(D0, K0, V0, L0, R0), pqueue(0, K, V, empty, empty)).
insert_2(K, V, empty, pqueue(D0, K0, V0, L0, R0),
        pqueue(0, K, V, empty, empty), pqueue(D0, K0, V0, L0, R0)).
insert_2(K, V, pqueue(D0, K0, V0, L0, R0),
        pqueue(D1, K1, V1, L1, R1), PQ1, PQ2) :-
    ( if D0 > D1 then
        pqueue.insert(K, V, pqueue(D1, K1, V1, L1, R1), PQ2),
        PQ1 = pqueue(D0, K0, V0, L0, R0)
    else
        pqueue.insert(K, V, pqueue(D0, K0, V0, L0, R0), PQ1),
        PQ2 = pqueue(D1, K1, V1, L1, R1)
    ).

%---------------------------------------------------------------------------%

remove(K, V, pqueue(_, K, V, L0, R0), PQ) :-
    pqueue.remove_2(L0, R0, PQ).

:- pred remove_2(pqueue(K, V)::in, pqueue(K, V)::in, pqueue(K, V)::out) is det.

remove_2(empty, empty, empty).
remove_2(empty, pqueue(D, K, V, L, R), pqueue(D, K, V, L, R)).
remove_2(pqueue(D, K, V, L, R), empty, pqueue(D, K, V, L, R)).
remove_2(pqueue(D0, K0, V0, L0, R0), pqueue(D1, K1, V1, L1, R1), PQ) :-
    compare(CMP, K0, K1),
    ( if CMP = (<) then
        D0M1 = D0 - 1,
        int.max(D0M1, D1, D),
        pqueue.remove_2(L0, R0, PQ0),
        PQ = pqueue(D, K0, V0, PQ0, pqueue(D1, K1, V1, L1, R1))
    else
        D1M1 = D0 - 1,
        int.max(D1M1, D1, D),
        pqueue.remove_2(L1, R1, PQ1),
        PQ = pqueue(D, K1, V1, PQ1, pqueue(D0, K0, V0, L0, R0))
    ).

det_remove(K, V, !PQ) :-
    ( if pqueue.remove(K0, V0, !PQ) then
        K = K0,
        V = V0
    else
        unexpected($file, $pred, "empty priority queue")
    ).

%---------------------------------------------------------------------------%

merge(A, B) = C :-
    merge(A, B, C).

merge(A, B, C) :-
    ( if length(A) =< length(B) then
        do_merge(A, B, C)
    else
        do_merge(B, A, C)
    ).

:- pred do_merge(pqueue(K, V)::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

do_merge(A, B, C) :-
    (
        A = empty,
        C = B
    ;
        A = pqueue(_, K, V, L, R),
        (
            B = empty,
            C = A
        ;
            B = pqueue(_, _, _, _, _),
            do_merge(L, B, C0),
            do_merge(R, C0, C1),
            insert(K, V, C1, C)
        )
    ).

%---------------------------------------------------------------------------%

to_assoc_list(PQ) = AL :-
    pqueue.to_assoc_list(PQ, AL).

to_assoc_list(Q0, L) :-
    ( if pqueue.remove(K, V, Q0, Q1) then
        pqueue.to_assoc_list(Q1, L0),
        L = [K - V | L0]
    else
        L = []
    ).

assoc_list_to_pqueue(AL) = PQ2 :-
    pqueue.assoc_list_to_pqueue(AL, PQ2).

assoc_list_to_pqueue([], Q) :-
    pqueue.init(Q).
assoc_list_to_pqueue([K - V | L], Q) :-
    pqueue.assoc_list_to_pqueue(L, Q0),
    pqueue.insert(K, V, Q0, Q).

from_assoc_list(List) = PQueue :-
    pqueue.assoc_list_to_pqueue(List, PQueue).

%---------------------------------------------------------------------------%

length(empty) = 0.
length(pqueue(D, _, _, _, _)) = D + 1.

%---------------------------------------------------------------------------%
:- end_module pqueue.
%---------------------------------------------------------------------------%
