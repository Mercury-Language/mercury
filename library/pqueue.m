%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%
%
% file pqueue.nl - implements a priority queue.
%
% A qpueue is a priority queue DDT with the smallest element at the root.
%
% stability?
%
%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- module pqueue.

:- interface.

:- import_module list, std_util.

:- type pqueue(_K, _V).

:- pred pqueue__init(pqueue(_K, _V)).
:- mode pqueue__init(out).

:- pred pqueue__insert(pqueue(K, V), K, V, pqueue(K, V)).
:- mode pqueue__insert(in, in, in, out).

:- pred pqueue__remove(pqueue(K, V), K, V, pqueue(K, V)).
:- mode pqueue__remove(in, out, out, out).

:- pred pqueue__to_assoc_list(pqueue(K, V), assoc_list(K, V)).
:- mode pqueue__to_assoc_list(in, out).

:- pred pqueue__assoc_list_to_pqueue(assoc_list(K, V), pqueue(K, V)).
:- mode pqueue__assoc_list_to_pqueue(in, out).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module int.

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
:- mode pqueue__insert_2(in, in, in, in, out, out).

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
:- mode pqueue__remove_2(in, in, out).

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

