%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% file pqueue.m - implements a priority queue ADT.
% main author: conway.
% stability: high.
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

:- type pqueue(_K, _V).

	% Create an empty priority queue
:- pred pqueue__init(pqueue(_K, _V)).
:- mode pqueue__init(out) is det.

	% Insert a value V with key K into a priority queue
	% and return the new priority queue.
:- pred pqueue__insert(pqueue(K, V), K, V, pqueue(K, V)).
:- mode pqueue__insert(in, in, in, out) is det.

	% Remove the smallest item from the priority queue.
:- pred pqueue__remove(pqueue(K, V), K, V, pqueue(K, V)).
:- mode pqueue__remove(in, out, out, out) is semidet.

	% Extract all the items from a priority queue by
	% repeated removal, and place them in an association
	% list.
:- pred pqueue__to_assoc_list(pqueue(K, V), assoc_list(K, V)).
:- mode pqueue__to_assoc_list(in, out) is det.

	% Insert all the key-value pairs in an association list
	% into a priority queue.
:- pred pqueue__assoc_list_to_pqueue(assoc_list(K, V), pqueue(K, V)).
:- mode pqueue__assoc_list_to_pqueue(in, out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int, list, std_util.

:- type pqueue(K, V)	--->	empty
			;	pqueue(int, K, V, pqueue(K, V), pqueue(K, V)).

%---------------------------------------------------------------------------%

pqueue__init(empty).

%---------------------------------------------------------------------------%

pqueue__insert(empty, K, V, pqueue(0, K, V, empty, empty)).
pqueue__insert(pqueue(D0, K0, V0, L0, R0), K, V, PQ) :-
	D is D0 + 1,
	compare(CMP, K, K0),
	(
		CMP = (<)
	->
		K1 = K,
		V1 = V,
		pqueue__insert_2(K0, V0, L0, R0, L, R)
	;
		K1 = K0,
		V1 = V0,
		pqueue__insert_2(K, V, L0, R0, L, R)
	),
	PQ = pqueue(D, K1 ,V1, L, R).

:- pred pqueue__insert_2(K, V, pqueue(K, V), pqueue(K, V),
						pqueue(K, V), pqueue(K, V)).
:- mode pqueue__insert_2(in, in, in, in, out, out) is det.

pqueue__insert_2(K, V, empty, empty, pqueue(0, K, V, empty, empty), empty).
pqueue__insert_2(K, V, pqueue(D0, K0, V0, L0, R0), empty,
		pqueue(D0, K0, V0, L0, R0), pqueue(0, K, V, empty, empty)).
pqueue__insert_2(K, V, empty, pqueue(D0, K0, V0, L0, R0),
		pqueue(0, K, V, empty, empty), pqueue(D0, K0, V0, L0, R0)).
pqueue__insert_2(K, V, pqueue(D0, K0, V0, L0, R0), pqueue(D1, K1, V1, L1, R1),
								PQ1, PQ2) :-
	(
		D0 > D1
	->
		pqueue__insert(pqueue(D1, K1, V1, L1, R1), K, V, PQ2),
		PQ1 = pqueue(D0, K0, V0, L0, R0)
	;
		pqueue__insert(pqueue(D0, K0, V0, L0, R0), K, V, PQ1),
		PQ2 = pqueue(D1, K1, V1, L1, R1)
	).


%---------------------------------------------------------------------------%

pqueue__remove(pqueue(_, K, V, L0, R0), K, V, PQ) :-
	pqueue__remove_2(L0, R0, PQ).

:- pred pqueue__remove_2(pqueue(K, V), pqueue(K, V), pqueue(K, V)).
:- mode pqueue__remove_2(in, in, out) is det.

pqueue__remove_2(empty, empty, empty).
pqueue__remove_2(empty, pqueue(D, K, V, L, R), pqueue(D, K, V, L, R)).
pqueue__remove_2(pqueue(D, K, V, L, R), empty, pqueue(D, K, V, L, R)).
pqueue__remove_2(pqueue(D0, K0, V0, L0, R0), pqueue(D1, K1, V1, L1, R1), PQ) :-
	compare(CMP, K0, K1),
	(
		CMP = (<)
	->
		D0M1 is D0 - 1,
		int__max(D0M1, D1, D),
		pqueue__remove_2(L0, R0, PQ0),
		PQ = pqueue(D, K0, V0, PQ0, pqueue(D1, K1, V1, L1, R1))
	;
		D1M1 is D0 - 1,
		int__max(D1M1, D1, D),
		pqueue__remove_2(L1, R1, PQ1),
		PQ = pqueue(D, K1, V1, PQ1, pqueue(D0, K0, V0, L0, R0))
	).

%---------------------------------------------------------------------------%

pqueue__to_assoc_list(Q0, L) :-
	(
		pqueue__remove(Q0, K, V, Q1)
	->
		pqueue__to_assoc_list(Q1, L0),
		L = [K - V | L0]
	;
		L = []
	).

pqueue__assoc_list_to_pqueue([], Q) :-
	pqueue__init(Q).
pqueue__assoc_list_to_pqueue([K - V | L], Q) :-
	pqueue__assoc_list_to_pqueue(L, Q0),
	pqueue__insert(Q0, K, V, Q).

%---------------------------------------------------------------------------%
