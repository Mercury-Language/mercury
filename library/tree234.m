%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% tree234 - implements a map (dictionary) using 2-3-4 trees.
% main author: conway (blame him for the lack of documentation! - fjh ;-).

%---------------------------------------------------------------------------%

:- module tree234.

:- interface.

:- import_module list, std_util.

:- type tree234(K, V).

:- pred tree234__init(tree234(K, V)).
:- mode tree234__init(out) is det.

:- pred tree234__search(tree234(K, V), K, V).
:- mode tree234__search(in, in, out) is semidet.

:- pred tree234__lookup(tree234(K, V), K, V).
:- mode tree234__lookup(in, in, out) is det.

:- pred tree234__insert(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__insert(in, in, in, out) is semidet.

:- pred tree234__set(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__set(in, in, in, out) is det.

:- pred tree234__delete(tree234(K, V), K, tree234(K, V)).
:- mode tree234__delete(in, in, out) is det.

:- pred tree234__remove(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__remove(in, in, out, out) is semidet.

:- pred tree234__keys(tree234(K, V), list(K)).
:- mode tree234__keys(in, out) is det.

:- pred tree234__values(tree234(K, V), list(V)).
:- mode tree234__values(in, out) is det.

:- pred tree234__update(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__update(in, in, in, out) is semidet.

	% count the number of elements in a tree
:- pred tree234__count(tree234(K, V), int).
:- mode tree234__count(in, out) is det.

:- pred tree234__assoc_list_to_tree234(assoc_list(K, V), tree234(K, V)).
:- mode tree234__assoc_list_to_tree234(in, out) is det.

:- pred tree234__tree234_to_assoc_list(tree234(K, V), assoc_list(K, V)).
:- mode tree234__tree234_to_assoc_list(in, out) is det.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
:- implementation.

:- import_module int, require.

:- type tree234(K, V)	--->
		empty
	;	two(K, V, tree234(K, V), tree234(K, V))
	;	three(K, V, K, V, tree234(K, V), tree234(K, V), tree234(K, V))
	;	four(K, V, K, V, K, V, tree234(K, V), tree234(K, V),
						tree234(K, V), tree234(K, V)).

%------------------------------------------------------------------------------%

tree234__init(empty).

%------------------------------------------------------------------------------%

tree234__search(empty, _K, _V) :- fail.
tree234__search(two(K0, V0, T0, T1), K, V) :-
	compare(Result, K, K0),
	(
		Result = (<),
		tree234__search(T0, K, V)
	;
		Result = (=),
		V = V0
	;
		Result = (>),
		tree234__search(T1, K, V)
	).
tree234__search(three(K0, V0, K1, V1, T0, T1, T2), K, V) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__search(T0, K, V)
	;
		Result0 = (=),
		V = V0
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__search(T1, K, V)
		;
			Result1 = (=),
			V = V1
		;
			Result1 = (>),
			tree234__search(T2, K, V)
		)
	).
tree234__search(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__search(T0, K, V)
	;
		Result0 = (=),
		V = V0
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__search(T1, K, V)
		;
			Result1 = (=),
			V = V1
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234__search(T2, K, V)
			;
				Result2 = (=),
				V = V2
			;
				Result2 = (>),
				tree234__search(T3, K, V)
			)
		)
	).

%------------------------------------------------------------------------------%

tree234__update(empty, _K, _V, _T) :- fail.
tree234__update(two(K0, V0, T0, T1), K, V, T) :-
	compare(Result, K, K0),
	(
		Result = (<),
		tree234__update(T0, K, V, T2),
		T = two(K0, V0, T2, T1)
	;
		Result = (=),
		T = two(K, V, T0, T1)
	;
		Result = (>),
		tree234__update(T1, K, V, T2),
		T = two(K0, V0, T0, T2)
	).
tree234__update(three(K0, V0, K1, V1, T0, T1, T2), K, V, T) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__update(T0, K, V, T3),
		T = three(K0, V0, K1, V1, T3, T1, T2)
	;
		Result0 = (=),
		T = three(K, V, K1, V1, T0, T1, T2)
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__update(T1, K, V, T3),
			T = three(K0, V0, K1, V1, T0, T3, T2)
		;
			Result1 = (=),
			T = three(K0, V0, K, V, T0, T1, T2)
		;
			Result1 = (>),
			tree234__update(T2, K, V, T3),
			T = three(K0, V0, K1, V1, T0, T1, T3)
		)
	).
tree234__update(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V, T) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__update(T0, K, V, T4),
		T = four(K0, V0, K1, V1, K2, V2, T4, T1, T2, T3)
	;
		Result0 = (=),
		T = four(K, V, K1, V1, K2, V2, T0, T1, T2, T3)
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__update(T1, K, V, T4),
			T = four(K0, V0, K1, V1, K2, V2, T0, T4, T2, T3)
		;
			Result1 = (=),
			T = four(K0, V0, K, V, K2, V2, T0, T1, T2, T3)
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234__update(T2, K, V, T4),
				T = four(K0, V0, K1, V1, K2, V2, T0, T1, T4, T3)
			;
				Result2 = (=),
				T = four(K0, V0, K1, V1, K, V, T0, T1, T2, T3)
			;
				Result2 = (>),
				tree234__update(T3, K, V, T4),
				T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T4)
			)
		)
	).

%------------------------------------------------------------------------------%

tree234__lookup(T, K, V) :-
	(
		tree234__search(T, K, V0)
	->
		V = V0
	;
		error("tree234__lookup: key not found.")
	).

%------------------------------------------------------------------------------%

tree234__insert(empty, K, V, two(K, V, empty, empty)).

tree234__insert(two(K0, V0, T0, T1), K, V, Tree) :-
	(
		T0 = empty,
		T1 = empty
	->
		compare(Result, K, K0),
		(
			Result = (<),
			Tree = three(K, V, K0, V0, empty, empty, empty)
		;
			Result = (=),
			fail
		;
			Result = (>),
			Tree = three(K0, V0, K, V, empty, empty, empty)
		)
	;
		compare(Result, K, K0),
		(
			Result = (<),
			(
				tree234__four(T0, K1, V1, T2, T3)
			->
				T4 = three(K1, V1, K0, V0, T2, T3, T1),
				tree234__insert(T4, K, V, Tree)
			;
				tree234__insert(T0, K, V, T2),
				Tree = two(K0, V0, T2, T1)
			)
		;
			Result = (=),
			fail
		;
			Result = (>),
			(
				tree234__four(T1, K1, V1, T2, T3)
			->
				T4 = three(K0, V0, K1, V1, T0, T2, T3),
				tree234__insert(T4, K, V, Tree)
			;
				tree234__insert(T1, K, V, T2),
				Tree = two(K0, V0, T0, T2)
			)
		)
	).

tree234__insert(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tree) :-
	(
		T0 = empty,
		T1 = empty,
		T2 = empty
	->
		compare(Result0, K, K0),
		(
			Result0 = (<),
			Tree = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty)
		;
			Result0 = (=),
			fail
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tree = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty)
			;
				Result1 = (=),
				fail
			;
				Result1 = (>),
				Tree = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty)
			)
		)
	;
		compare(Result0, K, K0),
		(
			Result0 = (<),
			(
				tree234__four(T0, K2, V2, T3, T4)
			->
				T5 = four(K2, V2, K0, V0, K1, V1, T3, T4, T1, T2),
				tree234__insert(T5, K, V, Tree)
			;
				tree234__insert(T0, K, V, T3),
				Tree = three(K0, V0, K1, V1, T3, T1, T2)
			)
		;
			Result0 = (=),
			fail
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				(
					tree234__four(T1, K2, V2, T3, T4)
				->
					T5 = four(K0, V0, K2, V2, K1, V1, T0, T3, T4, T2),
					tree234__insert(T5, K, V, Tree)
				;
					tree234__insert(T1, K, V, T3),
					Tree = three(K0, V0, K1, V1, T0, T3, T2)
				)
			;
				Result1 = (=),
				fail
			;
				Result1 = (>),
				(
					tree234__four(T2, K2, V2, T3, T4)
				->
					T5 = four(K0, V0, K1, V1, K2, V2, T0, T1, T3, T4),
					tree234__insert(T5, K, V, Tree)
				;
					tree234__insert(T2, K, V, T3),
					Tree = three(K0, V0, K1, V1, T0, T1, T3)
				)
			)
		)
	).

tree234__insert(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V, Tree) :-
	T4 = two(K1, V1,
		two(K0, V0, T0, T1),
		two(K2, V2, T2, T3)
	),
	tree234__insert(T4, K, V, Tree).

%------------------------------------------------------------------------------%

tree234__set(empty, K, V, two(K, V, empty, empty)).

tree234__set(two(K0, V0, T0, T1), K, V, Tree) :-
	(
		T0 = empty,
		T1 = empty
	->
		compare(Result, K, K0),
		(
			Result = (<),
			Tree = three(K, V, K0, V0, empty, empty, empty)
		;
			Result = (=),
			Tree = two(K, V, T0, T1)
		;
			Result = (>),
			Tree = three(K0, V0, K, V, empty, empty, empty)
		)
	;
		compare(Result, K, K0),
		(
			Result = (<),
			(
				tree234__four(T0, K1, V1, T2, T3)
			->
				T4 = three(K1, V1, K0, V0, T2, T3, T1),
				tree234__set(T4, K, V, Tree)
			;
				tree234__set(T0, K, V, T2),
				Tree = two(K0, V0, T2, T1)
			)
		;
			Result = (=),
			Tree = two(K, V, T0, T1)
		;
			Result = (>),
			(
				tree234__four(T1, K1, V1, T2, T3)
			->
				T4 = three(K0, V0, K1, V1, T0, T2, T3),
				tree234__set(T4, K, V, Tree)
			;
				tree234__set(T1, K, V, T2),
				Tree = two(K0, V0, T0, T2)
			)
		)
	).

tree234__set(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tree) :-
	(
		T0 = empty,
		T1 = empty,
		T2 = empty
	->
		compare(Result0, K, K0),
		(
			Result0 = (<),
			Tree = four(K, V, K0, V0, K1, V1, empty, empty, empty, empty)
		;
			Result0 = (=),
			Tree = three(K, V, K1, V1, empty, empty, empty)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tree = four(K0, V0, K, V, K1, V1, empty, empty, empty, empty)
			;
				Result1 = (=),
				Tree = three(K0, V0, K, V, empty, empty, empty)
			;
				Result1 = (>),
				Tree = four(K0, V0, K1, V1, K, V, empty, empty, empty, empty)
			)
		)
	;
		compare(Result0, K, K0),
		(
			Result0 = (<),
			(
				tree234__four(T0, K2, V2, T3, T4)
			->
				T5 = four(K2, V2, K0, V0, K1, V1, T3, T4, T1, T2),
				tree234__set(T5, K, V, Tree)
			;
				tree234__set(T0, K, V, T3),
				Tree = three(K0, V0, K1, V1, T3, T1, T2)
			)
		;
			Result0 = (=),
			Tree = three(K, V, K1, V1, T0, T1, T2)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				(
					tree234__four(T1, K2, V2, T3, T4)
				->
					T5 = four(K0, V0, K2, V2, K1, V1, T0, T3, T4, T2),
					tree234__set(T5, K, V, Tree)
				;
					tree234__set(T1, K, V, T3),
					Tree = three(K0, V0, K1, V1, T0, T3, T2)
				)
			;
				Result1 = (=),
				Tree = three(K0, V0, K, V, T0, T1, T2)
			;
				Result1 = (>),
				(
					tree234__four(T2, K2, V2, T3, T4)
				->
					T5 = four(K0, V0, K1, V1, K2, V2, T0, T1, T3, T4),
					tree234__set(T5, K, V, Tree)
				;
					tree234__set(T2, K, V, T3),
					Tree = three(K0, V0, K1, V1, T0, T1, T3)
				)
			)
		)
	).

tree234__set(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V, Tree) :-
	T4 = two(K1, V1,
		two(K0, V0, T0, T1),
		two(K2, V2, T2, T3)
	),
	tree234__set(T4, K, V, Tree).

%------------------------------------------------------------------------------%

:- pred tree234__four(tree234(K, V), K, V, tree234(K, V), tree234(K, V)).
:- mode tree234__four(in, out, out, out, out) is semidet.

tree234__four(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		K1, V1, two(K0, V0, T0, T1), two(K2, V2, T2, T3)).

%------------------------------------------------------------------------------%

tree234__delete(Tree0, K, Tree) :-
	(
		tree234__remove(Tree0, K, _V, Tree1)
	->
		Tree = Tree1
	;
		Tree = Tree0
	).

%------------------------------------------------------------------------------%

% tree234__remove(empty, _K, _V, empty) :- fail.
tree234__remove(empty, _K, _V, _Tree) :- fail.

tree234__remove(two(K0, V0, T0, T1), K, V, Tree) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__remove(T0, K, V, T2),
		Tree = two(K0, V0, T2, T1)
	;
		Result0 = (=),
		tree234__glue(T0, T1, Tree),
		V = V0
	;
		Result0 = (>),
		tree234__remove(T1, K, V, T2),
		Tree = two(K0, V0, T0, T2)
	).

tree234__remove(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tree) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__remove(T0, K, V, T3),
		Tree = three(K0, V0, K1, V1, T3, T1, T2)
	;
		Result0 = (=),
		tree234__glue(T0, T1, T3),
		Tree = two(K1, V1, T3, T2),
		V = V0
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__remove(T1, K, V, T3),
			Tree = three(K0, V0, K1, V1, T0, T3, T2)
		;
			Result1 = (=),
			tree234__glue(T1, T2, T3),
			Tree = two(K0, V0, T0, T3),
			V = V1
		;
			Result1 = (>),
			tree234__remove(T2, K, V, T3),
			Tree = three(K0, V0, K1, V1, T0, T1, T3)
		)
	).

tree234__remove(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V, Tree) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__remove(T0, K, V, T4),
		Tree = four(K0, V0, K1, V1, K2, V2, T4, T1, T2, T3)
	;
		Result0 = (=),
		tree234__glue(T0, T1, T4),
		Tree = three(K1, V1, K2, V2, T4, T2, T3),
		V = V0
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__remove(T1, K, V, T4),
			Tree = four(K0, V0, K1, V1, K2, V2, T0, T4, T2, T3)
		;
			Result1 = (=),
			tree234__glue(T1, T2, T4),
			Tree = three(K0, V0, K2, V2, T0, T4, T3),
			V = V1
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234__remove(T2, K, V, T4),
				Tree = four(K0, V0, K1, V1, K2, V2, T0, T1, T4, T3)
			;
				Result2 = (=),
				tree234__glue(T2, T3, T4),
				Tree = three(K0, V0, K1, V1, T0, T1, T4),
				V = V2
			;
				Result2 = (>),
				tree234__remove(T3, K, V, T4),
				Tree = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T4)
			)
		)
	).

%------------------------------------------------------------------------------%

:- pred tree234__glue(tree234(K, V), tree234(K, V), tree234(K, V)).
:- mode tree234__glue(in, in, out) is det.

tree234__glue(empty, T, T).
tree234__glue(two(K0, V0, T0, T1), T, two(K0, V0, T0, T2)) :-
	tree234__glue(T1, T, T2).
tree234__glue(three(K0, V0, K1, V1, T0, T1, T2), T, three(K0, V0, K1, V1, T0, T1, T3)) :-
	tree234__glue(T2, T, T3).
tree234__glue(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), T, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T4)) :-
	tree234__glue(T3, T, T4).

%------------------------------------------------------------------------------%

tree234__keys(empty, []).
tree234__keys(two(K0, _V0, T0, T1), L) :-
	tree234__keys(T0, L0),
	tree234__keys(T1, L1),
	list__append(L0, [K0|L1], L).
tree234__keys(three(K0, _V0, K1, _V1, T0, T1, T2), L) :-
	tree234__keys(T0, L0),
	tree234__keys(T1, L1),
	tree234__keys(T2, L2),
	list__append(L0, [K0|L1], L3),
	list__append(L3, [K1|L2], L).
tree234__keys(four(K0, _V0, K1, _V1, K2, _V2, T0, T1, T2, T3), L) :-
	tree234__keys(T0, L0),
	tree234__keys(T1, L1),
	tree234__keys(T2, L2),
	tree234__keys(T3, L3),
	list__append(L0, [K0|L1], L4),
	list__append(L4, [K1|L2], L5),
	list__append(L5, [K2|L3], L).

tree234__values(empty, []).
tree234__values(two(_K0, V0, T0, T1), L) :-
	tree234__values(T0, L0),
	tree234__values(T1, L1),
	list__append(L0, [V0|L1], L).
tree234__values(three(_K0, V0, _K1, V1, T0, T1, T2), L) :-
	tree234__values(T0, L0),
	tree234__values(T1, L1),
	tree234__values(T2, L2),
	list__append(L0, [V0|L1], L3),
	list__append(L3, [V1|L2], L).
tree234__values(four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), L) :-
	tree234__values(T0, L0),
	tree234__values(T1, L1),
	tree234__values(T2, L2),
	tree234__values(T3, L3),
	list__append(L0, [V0|L1], L4),
	list__append(L4, [V1|L2], L5),
	list__append(L5, [V2|L3], L).



%------------------------------------------------------------------------------%

tree234__assoc_list_to_tree234([], empty).
tree234__assoc_list_to_tree234([K - V|T], Tree) :-
	tree234__assoc_list_to_tree234(T, Tree0),
	tree234__set(Tree0, K, V, Tree).

%------------------------------------------------------------------------------%

tree234__tree234_to_assoc_list(empty, []).
tree234__tree234_to_assoc_list(two(K0, V0, T0, T1), L) :-
	tree234__tree234_to_assoc_list(T0, L0),
	tree234__tree234_to_assoc_list(T1, L1),
	list__append(L0, [K0 - V0|L1], L).
tree234__tree234_to_assoc_list(three(K0, V0, K1, V1, T0, T1, T2), L) :-
	tree234__tree234_to_assoc_list(T0, L0),
	tree234__tree234_to_assoc_list(T1, L1),
	tree234__tree234_to_assoc_list(T2, L2),
	list__append(L0, [K0 - V0|L1], L3),
	list__append(L3, [K1 - V1|L2], L).
tree234__tree234_to_assoc_list(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), L) :-
	tree234__tree234_to_assoc_list(T0, L0),
	tree234__tree234_to_assoc_list(T1, L1),
	tree234__tree234_to_assoc_list(T2, L2),
	tree234__tree234_to_assoc_list(T3, L3),
	list__append(L0, [K0 - V0|L1], L4),
	list__append(L4, [K1 - V1|L2], L5),
	list__append(L5, [K2 - V2|L3], L).

%------------------------------------------------------------------------------%

	% count the number of elements in a tree
tree234__count(empty, 0).
tree234__count(two(_, _, T0, T1), N) :-
	tree234__count(T0, N0),
	tree234__count(T1, N1),
	N is 1 + N0 + N1.
tree234__count(three(_, _, _, _, T0, T1, T2), N) :-
	tree234__count(T0, N0),
	tree234__count(T1, N1),
	tree234__count(T2, N2),
	N is 2 + N0 + N1 + N2.
tree234__count(four(_, _, _, _, _, _, T0, T1, T2, T3), N) :-
	tree234__count(T0, N0),
	tree234__count(T1, N1),
	tree234__count(T2, N2),
	tree234__count(T3, N3),
	N is 3 + N0 + N1 + N2 + N3.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%
