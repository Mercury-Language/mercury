%---------------------------------------------------------------------------%
% Copyright (C) 1995 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% tree234 - implements a map (dictionary) using 2-3-4 trees.
% main author: conway.
% stability: medium.

% See map.m for documentation.

%---------------------------------------------------------------------------%

:- module tree234.

:- interface.

:- import_module list, std_util, assoc_list.

:- type tree234(K, V).

:- pred tree234__init(tree234(K, V)).
:- mode tree234__init(uo) is det.

:- pred tree234__member(tree234(K, V), K, V).
:- mode tree234__member(in, out, out) is nondet.

:- pred tree234__search(tree234(K, V), K, V).
:- mode tree234__search(in, in, out) is semidet.

:- pred tree234__lookup(tree234(K, V), K, V).
:- mode tree234__lookup(in, in, out) is det.

:- pred tree234__insert(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__insert(in, in, in, out) is semidet.

:- pred tree234__set(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__set(di, di, di, uo) is det.
:- mode tree234__set(di_tree234, in, in, uo_tree234) is det.
:- mode tree234__set(in, in, in, out) is det.

:- pred tree234__delete(tree234(K, V), K, tree234(K, V)).
:- mode tree234__delete(di, in, uo) is det.
:- mode tree234__delete(di_tree234, in, uo_tree234) is det.
:- mode tree234__delete(in, in, out) is det.

:- pred tree234__remove(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__remove(di, in, uo, uo) is semidet.
:- mode tree234__remove(di_tree234, in, out, uo_tree234) is semidet.
:- mode tree234__remove(in, in, out, out) is semidet.

:- pred tree234__remove_smallest(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__remove_smallest(di, uo, uo, uo) is semidet.
:- mode tree234__remove_smallest(di_tree234, out, out, uo_tree234) is semidet.
:- mode tree234__remove_smallest(in, out, out, out) is semidet.

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

:- interface.
:- inst uniq_tree234(K,V) =
	unique((
		empty
	;	two(K, V, uniq_tree234(K, V), uniq_tree234(K, V))
	;	three(K, V, K, V, uniq_tree234(K, V), uniq_tree234(K, V),
			uniq_tree234(K, V))
	;	four(K, V, K, V, K, V, uniq_tree234(K, V), uniq_tree234(K, V),
			uniq_tree234(K, V), uniq_tree234(K, V))
	)).

:- mode di_tree234(K, V) :: uniq_tree234(K, V) -> dead.
:- mode di_tree234       :: uniq_tree234(ground, ground) -> dead.
:- mode uo_tree234(K, V) :: free -> uniq_tree234(K, V).
:- mode uo_tree234       :: free -> uniq_tree234(ground, ground).

:- implementation.

%------------------------------------------------------------------------------%

tree234__init(empty).

%------------------------------------------------------------------------------%

tree234__member(empty, _K, _V) :- fail.
tree234__member(two(K0, V0, T0, T1), K, V) :-
	(
		K = K0,
		V = V0
	;
		tree234__member(T0, K, V)
	;
		tree234__member(T1, K, V)
	).
tree234__member(three(K0, V0, K1, V1, T0, T1, T2), K, V) :-
	(
		K = K0,
		V = V0
	;
		K = K1,
		V = V1
	;
		tree234__member(T0, K, V)
	;
		tree234__member(T1, K, V)
	;
		tree234__member(T2, K, V)
	).
tree234__member(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V) :-
	(
		K = K0,
		V = V0
	;
		K = K1,
		V = V1
	;
		K = K2,
		V = V2
	;
		tree234__member(T0, K, V)
	;
		tree234__member(T1, K, V)
	;
		tree234__member(T2, K, V)
	;
		tree234__member(T3, K, V)
	).

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
		tree234__update(T0, K, V, NewT0),
		T = two(K0, V0, NewT0, T1)
	;
		Result = (=),
		T = two(K, V, T0, T1)
	;
		Result = (>),
		tree234__update(T1, K, V, NewT1),
		T = two(K0, V0, T0, NewT1)
	).
tree234__update(three(K0, V0, K1, V1, T0, T1, T2), K, V, T) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__update(T0, K, V, NewT0),
		T = three(K0, V0, K1, V1, NewT0, T1, T2)
	;
		Result0 = (=),
		T = three(K, V, K1, V1, T0, T1, T2)
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__update(T1, K, V, NewT1),
			T = three(K0, V0, K1, V1, T0, NewT1, T2)
		;
			Result1 = (=),
			T = three(K0, V0, K, V, T0, T1, T2)
		;
			Result1 = (>),
			tree234__update(T2, K, V, NewT2),
			T = three(K0, V0, K1, V1, T0, T1, NewT2)
		)
	).
tree234__update(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V, T) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__update(T0, K, V, NewT0),
		T = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3)
	;
		Result0 = (=),
		T = four(K, V, K1, V1, K2, V2, T0, T1, T2, T3)
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__update(T1, K, V, NewT1),
			T = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3)
		;
			Result1 = (=),
			T = four(K0, V0, K, V, K2, V2, T0, T1, T2, T3)
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234__update(T2, K, V, NewT2),
				T = four(K0, V0, K1, V1, K2, V2,
					T0, T1, NewT2, T3)
			;
				Result2 = (=),
				T = four(K0, V0, K1, V1, K, V,
					T0, T1, T2, T3)
			;
				Result2 = (>),
				tree234__update(T3, K, V, NewT3),
				T = four(K0, V0, K1, V1, K2, V2,
					T0, T1, T2, NewT3)
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

% tree234__insert is implemented using the simple top-down
% approach described in eg Sedgwick which splits 4 nodes into
% two 2 nodes on the downward traversal of the tree as we
% search for the right place to insert the new key-value pair.
% We know we have the right place if the subtrees of the node
% are empty (in which case we expand the node - which will always
% work because we already split 4 nodes into 2 nodes), or if the
% tree itself is empty.
% This algorithm is O(lgN).

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
				T0 = four(_, _, _, _, _, _, _, _, _, _)
			->
				tree234__four(T0, K1, V1, T2, T3),
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
				T1 = four(_, _, _, _, _, _, _, _, _, _)
			->
				tree234__four(T1, K1, V1, T2, T3),
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
			Tree = four(K, V, K0, V0, K1, V1,
					empty, empty, empty, empty)
		;
			Result0 = (=),
			fail
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tree = four(K0, V0, K, V, K1, V1,
					empty, empty, empty, empty)
			;
				Result1 = (=),
				fail
			;
				Result1 = (>),
				Tree = four(K0, V0, K1, V1, K, V,
					empty, empty, empty, empty)
			)
		)
	;
		compare(Result0, K, K0),
		(
			Result0 = (<),
			(
				T0 = four(_, _, _, _, _, _, _, _, _, _)
			->
				tree234__four(T0, K2, V2, T3, T4),
				T5 = four(K2, V2, K0, V0, K1, V1,
						T3, T4, T1, T2),
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
					T1 = four(_, _, _, _, _, _, _, _, _, _)
				->
					tree234__four(T1, K2, V2, T3, T4),
					T5 = four(K0, V0, K2, V2, K1, V1,
						T0, T3, T4, T2),
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
					T2 = four(_, _, _, _, _, _, _, _, _, _)
				->
					tree234__four(T2, K2, V2, T3, T4),
					T5 = four(K0, V0, K1, V1, K2, V2,
							T0, T1, T3, T4),
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

% tree234__set uses the same algorithm as used for tree234__insert,
% except that instead of failing for equal keys, we replace the value.

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
				T0 = four(_, _, _, _, _, _, _, _, _, _)
			->
				tree234__four(T0, K1, V1, T2, T3),
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
				T1 = four(_, _, _, _, _, _, _, _, _, _)
			->
				tree234__four(T1, K1, V1, T2, T3),
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
			Tree = four(K, V, K0, V0, K1, V1,
				empty, empty, empty, empty)
		;
			Result0 = (=),
			Tree = three(K, V, K1, V1, empty, empty, empty)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tree = four(K0, V0, K, V, K1, V1,
						empty, empty, empty, empty)
			;
				Result1 = (=),
				Tree = three(K0, V0, K, V, empty, empty, empty)
			;
				Result1 = (>),
				Tree = four(K0, V0, K1, V1, K, V,
					empty, empty, empty, empty)
			)
		)
	;
		compare(Result0, K, K0),
		(
			Result0 = (<),
			(
				T0 = four(_, _, _, _, _, _, _, _, _, _)
			->
				tree234__four(T0, K2, V2, T3, T4),
				T5 = four(K2, V2, K0, V0, K1, V1,
					T3, T4, T1, T2),
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
					T1 = four(_, _, _, _, _, _, _, _, _, _)
				->
					tree234__four(T1, K2, V2, T3, T4),
					T5 = four(K0, V0, K2, V2, K1, V1,
						T0, T3, T4, T2),
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
					T2 = four(_, _, _, _, _, _, _, _, _, _)
				->
					tree234__four(T2, K2, V2, T3, T4),
					T5 = four(K0, V0, K1, V1, K2, V2,
						T0, T1, T3, T4),
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

:- inst four(K, V, T) =
	bound(
		four(K, V, K, V, K, V, T, T, T, T)
	).

:- inst uniq_four(K, V, T) =
	unique(
		four(K, V, K, V, K, V, T, T, T, T)
	).

:- mode fdi_four :: di(uniq_four(unique, unique, unique)).
:- mode di_four :: di(uniq_four(ground, ground, uniq_tree234(ground, ground))).
:- mode in_four :: in(four(ground, ground, ground)).

:- pred tree234__four(tree234(K, V), K, V, tree234(K, V), tree234(K, V)).
:- mode tree234__four(fdi_four, uo, uo, uo, uo) is det.
:- mode tree234__four(di_four, out, out, uo_tree234, uo_tree234) is det.
:- mode tree234__four(in_four, out, out, out, out) is det.

tree234__four(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		K1, V1, two(K0, V0, T0, T1), two(K2, V2, T2, T3)).

%------------------------------------------------------------------------------%

tree234__delete(empty, _K, empty).

tree234__delete(two(K0, V0, T0, T1), K, Tree) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__delete(T0, K, T2),
		Tree = two(K0, V0, T2, T1)
	;
		Result0 = (=),
		tree234__glue(T0, T1, Tree)
	;
		Result0 = (>),
		tree234__delete(T1, K, T2),
		Tree = two(K0, V0, T0, T2)
	).

tree234__delete(three(K0, V0, K1, V1, T0, T1, T2), K, Tree) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__delete(T0, K, T3),
		Tree = three(K0, V0, K1, V1, T3, T1, T2)
	;
		Result0 = (=),
		tree234__glue(T0, T1, T3),
		Tree = two(K1, V1, T3, T2)
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__delete(T1, K, T3),
			Tree = three(K0, V0, K1, V1, T0, T3, T2)
		;
			Result1 = (=),
			tree234__glue(T1, T2, T3),
			Tree = two(K0, V0, T0, T3)
		;
			Result1 = (>),
			tree234__delete(T2, K, T3),
			Tree = three(K0, V0, K1, V1, T0, T1, T3)
		)
	).

tree234__delete(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, Tree) :-
	compare(Result0, K, K0),
	(
		Result0 = (<),
		tree234__delete(T0, K, NewT0),
		Tree = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3)
	;
		Result0 = (=),
		tree234__glue(T0, T1, T0_and_1),
		Tree = three(K1, V1, K2, V2, T0_and_1, T2, T3)
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__delete(T1, K, NewT1),
			Tree = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3)
		;
			Result1 = (=),
			tree234__glue(T1, T2, T1_and_2),
			Tree = three(K0, V0, K2, V2, T0, T1_and_2, T3)
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234__delete(T2, K, NewT2),
				Tree = four(K0, V0, K1, V1, K2, V2,
						T0, T1, NewT2, T3)
			;
				Result2 = (=),
				tree234__glue(T2, T3, T2_and_3),
				Tree = three(K0, V0, K1, V1, T0, T1, T2_and_3)
			;
				Result2 = (>),
				tree234__delete(T3, K, NewT3),
				Tree = four(K0, V0, K1, V1, K2, V2,
						T0, T1, T2, NewT3)
			)
		)
	).


%------------------------------------------------------------------------------%

tree234__remove(empty, _K, _V, empty) :- fail.

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
		tree234__remove(T0, K, V, NewT0),
		Tree = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3)
	;
		Result0 = (=),
		tree234__glue(T0, T1, T0_and_1),
		Tree = three(K1, V1, K2, V2, T0_and_1, T2, T3),
		V = V0
	;
		Result0 = (>),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			tree234__remove(T1, K, V, NewT1),
			Tree = four(K0, V0, K1, V1, K2, V2, T0, NewT1, T2, T3)
		;
			Result1 = (=),
			tree234__glue(T1, T2, T1_and_2),
			Tree = three(K0, V0, K2, V2, T0, T1_and_2, T3),
			V = V1
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234__remove(T2, K, V, NewT2),
				Tree = four(K0, V0, K1, V1, K2, V2,
						T0, T1, NewT2, T3)
			;
				Result2 = (=),
				tree234__glue(T2, T3, T2_and_3),
				Tree = three(K0, V0, K1, V1, T0, T1, T2_and_3),
				V = V2
			;
				Result2 = (>),
				tree234__remove(T3, K, V, NewT3),
				Tree = four(K0, V0, K1, V1, K2, V2,
						T0, T1, T2, NewT3)
			)
		)
	).

%------------------------------------------------------------------------------%

tree234__remove_smallest(empty, _K, _V, empty) :- fail.

tree234__remove_smallest(two(K0, V0, T0, T1), K, V, Tree) :-
	(
		T0 = empty
	->
		K = K0,
		V = V0,
		Tree = T1
	;
		tree234__remove_smallest(T0, K, V, NewT0),
		Tree = two(K0, V0, NewT0, T1)
	).

tree234__remove_smallest(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tree) :-
	(
		T0 = empty
	->
		K = K0,
		V = V0,
		Tree = two(K1, V1, T1, T2)
	;
		tree234__remove_smallest(T0, K, V, NewT0),
		Tree = three(K0, V0, K1, V1, NewT0, T1, T2)
	).

tree234__remove_smallest(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
								K, V, Tree) :-
	(
		T0 = empty
	->
		K = K0,
		V = V0,
		Tree = three(K1, V1, K2, V2, T1, T2, T3)
	;
		tree234__remove_smallest(T0, K, V, NewT0),
		Tree = four(K0, V0, K1, V1, K2, V2, NewT0, T1, T2, T3)
	).

%------------------------------------------------------------------------------%

% tree234__glue(A, B, C) is true iff C is a 234-tree which is the
% same as the 234-tree A with the 234-tree B attatched to the right-most
% node. It is used when removing a node from the a tree to glue the two
% resulting fragments together.
% XXX a better algorithm could be devised that leaves the tree more
% balanced (this algorithm is not the *proper* way to join 2 234 trees).

:- pred tree234__glue(tree234(K, V), tree234(K, V), tree234(K, V)).
:- mode tree234__glue(di, di, uo) is det.
:- mode tree234__glue(di_tree234, di_tree234, uo_tree234) is det.
:- mode tree234__glue(in, in, out) is det.

tree234__glue(empty, T, T).
tree234__glue(two(K0, V0, T0, T1), T, two(K0, V0, T0, NewT1)) :-
	tree234__glue(T1, T, NewT1).
tree234__glue(three(K0, V0, K1, V1, T0, T1, T2), T,
		three(K0, V0, K1, V1, T0, T1, NewT2)) :-
	tree234__glue(T2, T, NewT2).
tree234__glue(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), T,
		four(K0, V0, K1, V1, K2, V2, T0, T1, T2, NewT3)) :-
	tree234__glue(T3, T, NewT3).

%------------------------------------------------------------------------------%

tree234__keys(Tree, Keys) :-
	tree234__keys_2(Tree, [], Keys).

:- pred tree234__keys_2(tree234(K, V), list(K), list(K)).
:- mode tree234__keys_2(in, in, out) is det.

tree234__keys_2(empty, List, List).
tree234__keys_2(two(K0, _V0, T0, T1), L0, L) :-
	tree234__keys_2(T1, L0, L1),
	tree234__keys_2(T0, [K0|L1], L).
tree234__keys_2(three(K0, _V0, K1, _V1, T0, T1, T2), L0, L) :-
	tree234__keys_2(T2, L0, L1),
	tree234__keys_2(T1, [K1|L1], L2),
	tree234__keys_2(T0, [K0|L2], L).
tree234__keys_2(four(K0, _V0, K1, _V1, K2, _V2, T0, T1, T2, T3), L0, L) :-
	tree234__keys_2(T3, L0, L1),
	tree234__keys_2(T2, [K2|L1], L2),
	tree234__keys_2(T1, [K1|L2], L3),
	tree234__keys_2(T0, [K0|L3], L).

%------------------------------------------------------------------------------%

tree234__values(Tree, Values) :-
	tree234__values_2(Tree, [], Values).

:- pred tree234__values_2(tree234(K, V), list(V), list(V)).
:- mode tree234__values_2(in, in, out) is det.

tree234__values_2(empty, List, List).
tree234__values_2(two(_K0, V0, T0, T1), L0, L) :-
	tree234__values_2(T1, L0, L1),
	tree234__values_2(T0, [V0|L1], L).
tree234__values_2(three(_K0, V0, _K1, V1, T0, T1, T2), L0, L) :-
	tree234__values_2(T2, L0, L1),
	tree234__values_2(T1, [V1|L1], L2),
	tree234__values_2(T0, [V0|L2], L).
tree234__values_2(four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), L0, L) :-
	tree234__values_2(T3, L0, L1),
	tree234__values_2(T2, [V2|L1], L2),
	tree234__values_2(T1, [V1|L2], L3),
	tree234__values_2(T0, [V0|L3], L).

%------------------------------------------------------------------------------%

tree234__assoc_list_to_tree234(AssocList, Tree) :-
	tree234__assoc_list_to_tree234_2(AssocList, empty, Tree).

:- pred tree234__assoc_list_to_tree234_2(assoc_list(K, V), tree234(K, V),
					tree234(K, V)).
:- mode tree234__assoc_list_to_tree234_2(in, in, out) is det.

tree234__assoc_list_to_tree234_2([], Tree, Tree).
tree234__assoc_list_to_tree234_2([K - V | Rest], Tree0, Tree) :-
	tree234__set(Tree0, K, V, Tree1),
	tree234__assoc_list_to_tree234_2(Rest, Tree1, Tree).

%------------------------------------------------------------------------------%

tree234__tree234_to_assoc_list(Tree, AssocList) :-
	tree234__tree234_to_assoc_list_2(Tree, [], AssocList).

:- pred tree234__tree234_to_assoc_list_2(tree234(K, V), assoc_list(K, V),
						assoc_list(K, V)).
:- mode tree234__tree234_to_assoc_list_2(in, in, out) is det.

tree234__tree234_to_assoc_list_2(empty, List, List).
tree234__tree234_to_assoc_list_2(two(K0, V0, T0, T1), L0, L) :-
	tree234__tree234_to_assoc_list_2(T1, L0, L1),
	tree234__tree234_to_assoc_list_2(T0, [K0 - V0 | L1], L).
tree234__tree234_to_assoc_list_2(three(K0, V0, K1, V1, T0, T1, T2), L0, L) :-
	tree234__tree234_to_assoc_list_2(T2, L0, L1),
	tree234__tree234_to_assoc_list_2(T1, [K1 - V1 | L1], L2),
	tree234__tree234_to_assoc_list_2(T0, [K0 - V0 | L2], L).
tree234__tree234_to_assoc_list_2(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
					L0, L) :-
	tree234__tree234_to_assoc_list_2(T3, L0, L1),
	tree234__tree234_to_assoc_list_2(T2, [K2 - V2 | L1], L2),
	tree234__tree234_to_assoc_list_2(T1, [K1 - V1 | L2], L3),
	tree234__tree234_to_assoc_list_2(T0, [K0 - V0 | L3], L).

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
