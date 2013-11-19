%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et wm=0 tw=0
%---------------------------------------------------------------------------%
% Copyright (C) 1994-1995, 1997, 1999, 2003-2007, 2009 The University of
% Melbourne.
%
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: pqueue.m.
% Main author: conway.
% Stability: high.
%
% This module implements a priority queue ADT.
%
% A pqueue is a priority queue.  A priority queue holds a collection
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
:- func pqueue.init = pqueue(K, V).
:- pred pqueue.init(pqueue(K, V)::out) is det.

    % Test if a priority queue is empty.
    %
:- pred pqueue.is_empty(pqueue(K, V)::in) is semidet.

    % Insert a value V with key K into a priority queue
    % and return the new priority queue.
    %
:- func pqueue.insert(pqueue(K, V), K, V) = pqueue(K, V).
:- pred pqueue.insert(K::in, V::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

    % Extract the smallest key/value pair from the priority queue without
    % removing it.  Fails if the priority queue is empty.
    %
:- pred pqueue.peek(pqueue(K, V)::in, K::out, V::out) is semidet.

    % Extract the smallest key from the priority queue without removing it.
    % Fails if the priority queue is empty.
    %
:- pred pqueue.peek_key(pqueue(K, V)::in, K::out) is semidet.

    % Extract the smallest value from the priority queue without removing
    % it.  Fails if the priority queue is empty.
    %
:- pred pqueue.peek_value(pqueue(K, V)::in, V::out) is semidet.

    % As above, but calls error/1 if the priority queue is empty.
    %
:- pred pqueue.det_peek(pqueue(K, V)::in, K::out, V::out) is det.
:- func pqueue.det_peek_key(pqueue(K, V)) = K.
:- func pqueue.det_peek_value(pqueue(K, V)) = V.

    % Remove the smallest item from the priority queue.
    % Fails if the priority queue is empty.
    %
:- pred pqueue.remove(K::out, V::out, pqueue(K, V)::in, pqueue(K, V)::out)
    is semidet.

    % As above, but calls error/1 if the priority queue is empty.
    %
:- pred pqueue.det_remove(K::out, V::out, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

    % Merges all the entries of one priority queue with another, returning
    % the merged list.
:- func pqueue.merge(pqueue(K, V), pqueue(K, V)) = pqueue(K, V).
:- pred pqueue.merge(pqueue(K, V)::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

    % Extract all the items from a priority queue by repeated
    % removal, and place them in an association list.
    %
:- func pqueue.to_assoc_list(pqueue(K, V)) = assoc_list(K, V).
:- pred pqueue.to_assoc_list(pqueue(K, V)::in, assoc_list(K, V)::out)
    is det.

    % Insert all the key-value pairs in an association list
    % into a priority queue.
    %
:- func pqueue.assoc_list_to_pqueue(assoc_list(K, V)) = pqueue(K, V).
:- pred pqueue.assoc_list_to_pqueue(assoc_list(K, V)::in, pqueue(K, V)::out)
    is det.

    % A synonym for pqueue.assoc_list_to_pqueue/1.
    %
:- func pqueue.from_assoc_list(assoc_list(K, V)) = pqueue(K, V).

    % length(PQueue) = Length.
    %
    % Length is the number of items in PQueue
    %
:- func pqueue.length(pqueue(K, V)) = int.

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

pqueue.init = PQ :-
    pqueue.init(PQ).

pqueue.init(empty).

%---------------------------------------------------------------------------%

pqueue.is_empty(empty).

%---------------------------------------------------------------------------%

pqueue.peek(pqueue(_, K, V, _, _), K, V).
pqueue.peek_key(pqueue(_, K, _, _, _), K).
pqueue.peek_value(pqueue(_, _, V, _, _), V).

%---------------------------------------------------------------------------%

pqueue.det_peek(PQ, K, V) :-
    ( pqueue.peek(PQ, J, T) ->
        K = J, V = T
    ;
        unexpected($file, $pred, "empty priority queue")
    ).

pqueue.det_peek_key(PQ) = K :- pqueue.det_peek(PQ, K, _).
pqueue.det_peek_value(PQ) = V :- pqueue.det_peek(PQ, _, V).

%---------------------------------------------------------------------------%

pqueue.insert(!.PQ, K, V) = !:PQ :-
    pqueue.insert(K, V, !PQ).

pqueue.insert(K, V, empty, pqueue(0, K, V, empty, empty)).
pqueue.insert(K, V, pqueue(D0, K0, V0, L0, R0), PQ) :-
    D = D0 + 1,
    compare(CMP, K, K0),
    ( CMP = (<) ->
        K1 = K,
        V1 = V,
        pqueue.insert_2(K0, V0, L0, R0, L, R)
    ;
        K1 = K0,
        V1 = V0,
        pqueue.insert_2(K, V, L0, R0, L, R)
    ),
    PQ = pqueue(D, K1, V1, L, R).

:- pred pqueue.insert_2(K::in, V::in, pqueue(K, V)::in, pqueue(K, V)::in,
    pqueue(K, V)::out, pqueue(K, V)::out) is det.

pqueue.insert_2(K, V, empty, empty, pqueue(0, K, V, empty, empty), empty).
pqueue.insert_2(K, V, pqueue(D0, K0, V0, L0, R0), empty,
        pqueue(D0, K0, V0, L0, R0), pqueue(0, K, V, empty, empty)).
pqueue.insert_2(K, V, empty, pqueue(D0, K0, V0, L0, R0),
        pqueue(0, K, V, empty, empty), pqueue(D0, K0, V0, L0, R0)).
pqueue.insert_2(K, V, pqueue(D0, K0, V0, L0, R0),
                pqueue(D1, K1, V1, L1, R1), PQ1, PQ2) :-
    ( D0 > D1 ->
        pqueue.insert(K, V, pqueue(D1, K1, V1, L1, R1), PQ2),
        PQ1 = pqueue(D0, K0, V0, L0, R0)
    ;
        pqueue.insert(K, V, pqueue(D0, K0, V0, L0, R0), PQ1),
        PQ2 = pqueue(D1, K1, V1, L1, R1)
    ).

%---------------------------------------------------------------------------%

pqueue.det_remove(K, V, !PQ) :-
    ( if pqueue.remove(K0, V0, !PQ) then
        K = K0,
        V = V0
      else
        unexpected($file, $pred, "empty priority queue")
    ).

pqueue.remove(K, V, pqueue(_, K, V, L0, R0), PQ) :-
    pqueue.remove_2(L0, R0, PQ).

:- pred pqueue.remove_2(pqueue(K, V)::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

pqueue.remove_2(empty, empty, empty).
pqueue.remove_2(empty, pqueue(D, K, V, L, R), pqueue(D, K, V, L, R)).
pqueue.remove_2(pqueue(D, K, V, L, R), empty, pqueue(D, K, V, L, R)).
pqueue.remove_2(pqueue(D0, K0, V0, L0, R0), pqueue(D1, K1, V1, L1, R1), PQ) :-
    compare(CMP, K0, K1),
    ( CMP = (<) ->
        D0M1 = D0 - 1,
        int.max(D0M1, D1, D),
        pqueue.remove_2(L0, R0, PQ0),
        PQ = pqueue(D, K0, V0, PQ0, pqueue(D1, K1, V1, L1, R1))
    ;
        D1M1 = D0 - 1,
        int.max(D1M1, D1, D),
        pqueue.remove_2(L1, R1, PQ1),
        PQ = pqueue(D, K1, V1, PQ1, pqueue(D0, K0, V0, L0, R0))
    ).

%---------------------------------------------------------------------------%

pqueue.merge(A, B) = C :-
    pqueue.merge(A, B, C).

pqueue.merge(A, B, C) :-
  ( pqueue.length(A) =< pqueue.length(B) ->
      pqueue.merge2(A, B, C)
  ;
      pqueue.merge2(B, A, C) ).

:- pred pqueue.merge2(pqueue(K, V)::in, pqueue(K, V)::in, pqueue(K, V)::out)
    is det.

pqueue.merge2(empty,                   B,     B).
pqueue.merge2(A@pqueue(_, _, _, _, _), empty, A).
pqueue.merge2(pqueue(_, K, V, L, R),   !.PQ@pqueue(_, _, _, _, _), !:PQ) :-
    pqueue.merge2(L, !PQ),
    pqueue.merge2(R, !PQ),
    pqueue.insert(K, V, !PQ).

%---------------------------------------------------------------------------%

pqueue.to_assoc_list(PQ) = AL :-
    pqueue.to_assoc_list(PQ, AL).

pqueue.to_assoc_list(Q0, L) :-
    ( pqueue.remove(K, V, Q0, Q1) ->
        pqueue.to_assoc_list(Q1, L0),
        L = [K - V | L0]
    ;
        L = []
    ).

pqueue.assoc_list_to_pqueue(AL) = PQ2 :-
    pqueue.assoc_list_to_pqueue(AL, PQ2).

pqueue.assoc_list_to_pqueue([], Q) :-
    pqueue.init(Q).
pqueue.assoc_list_to_pqueue([K - V | L], Q) :-
    pqueue.assoc_list_to_pqueue(L, Q0),
    pqueue.insert(K, V, Q0, Q).

pqueue.from_assoc_list(List) = PQueue :-
    pqueue.assoc_list_to_pqueue(List, PQueue).

%---------------------------------------------------------------------------%

pqueue.length(empty) = 0.
pqueue.length(pqueue(D, _, _, _, _)) = D + 1.

%---------------------------------------------------------------------------%
:- end_module pqueue.
%---------------------------------------------------------------------------%
