%---------------------------------------------------------------------------%
% Copyright (C) 1995 University of Melbourne.
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
% :- mode tree234__insert(di_tree234, in, in, uo_tree234) is semidet.
% :- mode tree234__insert(in, in, in, out) is semidet.

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
% :- mode tree234__update(di_tree234, in, in, uo_tree234) is det.
% :- mode tree234__update(di, di, di, uo) is semidet.

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

:- inst uniq_tree234(K, V) =
	unique((
		empty
	;	two(K, V, uniq_tree234(K, V), uniq_tree234(K, V))
	;	three(K, V, K, V, uniq_tree234(K, V), uniq_tree234(K, V),
			uniq_tree234(K, V))
	;	four(K, V, K, V, K, V, uniq_tree234(K, V), uniq_tree234(K, V),
			uniq_tree234(K, V), uniq_tree234(K, V))
	)).

:- inst uniq_tree234_gg =
	unique((
		empty
	;	two(ground, ground, uniq_tree234_gg, uniq_tree234_gg)
	;	three(ground, ground, ground, ground,
			uniq_tree234_gg, uniq_tree234_gg, uniq_tree234_gg)
	;	four(ground, ground, ground, ground, ground, ground,
			uniq_tree234_gg, uniq_tree234_gg,
			uniq_tree234_gg, uniq_tree234_gg)
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

tree234__search(T, K, V) :-
	(
		T = empty,
		fail
	;
		T = two(K0, _, _, _),
		compare(Result, K, K0),
		(
			Result = (<),
			T = two(_, _, T0, _),
			tree234__search(T0, K, V)
		;
			Result = (=),
			T = two(_, V0, _, _),
			V = V0
		;
			Result = (>),
			T = two(_, _, _, T1),
			tree234__search(T1, K, V)
		)
	;
		T = three(K0, _, _, _, _, _, _),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			T = three(_, _, _, _, T0, _, _),
			tree234__search(T0, K, V)
		;
			Result0 = (=),
			T = three(_, V0, _, _, _, _, _),
			V = V0
		;
			Result0 = (>),
			T = three(_, _, K1, _, _, _, _),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				T = three(_, _, _, _, _, T1, _),
				tree234__search(T1, K, V)
			;
				Result1 = (=),
				T = three(_, _, _, V1, _, _, _),
				V = V1
			;
				Result1 = (>),
				T = three(_, _, _, _, _, _, T2),
				tree234__search(T2, K, V)
			)
		)
	;
		T = four(_, _, K1, _, _, _, _, _, _, _),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			T = four(K0, _, _, _, _, _, _, _, _, _),
			compare(Result0, K, K0),
			(
				Result0 = (<),
				T = four(_, _, _, _, _, _, T0, _, _, _),
				tree234__search(T0, K, V)
			;
				Result0 = (=),
				T = four(_, V0, _, _, _, _, _, _, _, _),
				V = V0
			;
				Result0 = (>),
				T = four(_, _, _, _, _, _, _, T1, _, _),
				tree234__search(T1, K, V)
			)
		;
			Result1 = (=),
			T = four(_, _, _, V1, _, _, _, _, _, _),
			V = V1
		;
			Result1 = (>),
			T = four(_, _, _, _, K2, _, _, _, _, _),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				T = four(_, _, _, _, _, _, _, _, T2, _),
				tree234__search(T2, K, V)
			;
				Result2 = (=),
				T = four(_, _, _, _, _, V2, _, _, _, _),
				V = V2
			;
				Result2 = (>),
				T = four(_, _, _, _, _, _, _, _, _, T3),
				tree234__search(T3, K, V)
			)
		)
	).

%------------------------------------------------------------------------------%

tree234__update(Tin, K, V, Tout) :-
	(
		Tin = empty,
		fail
	;
		Tin = two(K0, _, _, _),
		compare(Result, K, K0),
		(
			Result = (<),
			Tin = two(_, _, T0, _),
			tree234__update(T0, K, V, NewT0),
			Tin = two(_, V0, _, T1),
			Tout = two(K0, V0, NewT0, T1)
		;
			Result = (=),
			Tin = two(_, _, T0, T1),
			Tout = two(K0, V, T0, T1)
		;
			Result = (>),
			Tin = two(_, _, _, T1),
			tree234__update(T1, K, V, NewT1),
			Tin = two(_, V0, T0, _),
			Tout = two(K0, V0, T0, NewT1)
		)
	;
		Tin = three(K0, _, _, _, _, _, _),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			Tin = three(_, _, _, _, T0, _, _),
			tree234__update(T0, K, V, NewT0),
			Tin = three(_, V0, K1, V1, _, T1, T2),
			Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
		;
			Result0 = (=),
			Tin = three(_, _, K1, V1, T0, T1, T2),
			Tout = three(K0, V, K1, V1, T0, T1, T2)
		;
			Result0 = (>),
			Tin = three(_, _, K1, _, _, _, _),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tin = three(_, _, _, _, _, T1, _),
				tree234__update(T1, K, V, NewT1),
				Tin = three(_, V0, _, V1, T0, _, T2),
				Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
			;
				Result1 = (=),
				Tin = three(_, V0, _, _, T0, T1, T2),
				Tout = three(K0, V0, K1, V, T0, T1, T2)
			;
				Result1 = (>),
				Tin = three(_, _, _, _, _, _, T2),
				tree234__update(T2, K, V, NewT2),
				Tin = three(_, V0, _, V1, T0, T1, _),
				Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
			)
		)
	;
		Tin = four(_, _, K1, _, _, _, _, _, _, _),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			Tin = four(K0, _, _, _, _, _, _, _, _, _),
			compare(Result0, K, K0),
			(
				Result0 = (<),
				Tin = four(_, _, _, _, _, _, T0, _, _, _),
				tree234__update(T0, K, V, NewT0),
				Tin = four(_, V0, _, V1, K2, V2, _, T1, T2, T3),
				Tout = four(K0, V0, K1, V1, K2, V2,
					NewT0, T1, T2, T3)
			;
				Result0 = (=),
				Tin = four(_, _, _, V1, K2, V2, T0, T1, T2, T3),
				Tout = four(K0, V, K1, V1, K2, V2,
					T0, T1, T2, T3)
			;
				Result0 = (>),
				Tin = four(_, _, _, _, _, _, _, T1, _, _),
				tree234__update(T1, K, V, NewT1),
				Tin = four(_, V0, _, V1, K2, V2, T0, _, T2, T3),
				Tout = four(K0, V0, K1, V1, K2, V2,
					T0, NewT1, T2, T3)
			)
		;
			Result1 = (=),
			Tin = four(K0, V0, _, _, K2, V2, T0, T1, T2, T3),
			Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
		;
			Result1 = (>),
			Tin = four(_, _, _, _, K2, _, _, _, _, _),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				Tin = four(_, _, _, _, _, _, _, _, T2, _),
				tree234__update(T2, K, V, NewT2),
				Tin = four(K0, V0, _, V1, _, V2, T0, T1, _, T3),
				Tout = four(K0, V0, K1, V1, K2, V2,
					T0, T1, NewT2, T3)
			;
				Result2 = (=),
				Tin = four(K0, V0, _, V1, _, _, T0, T1, T2, T3),
				Tout = four(K0, V0, K1, V1, K2, V,
					T0, T1, T2, T3)
			;
				Result2 = (>),
				Tin = four(_, _, _, _, _, _, _, _, _, T3),
				tree234__update(T3, K, V, NewT3),
				Tin = four(K0, V0, _, V1, _, V2, T0, T1, T2, _),
				Tout = four(K0, V0, K1, V1, K2, V2,
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

:- inst two(K, V, T) =
	bound(
		two(K, V, T, T)
	).

:- inst uniq_two(K, V, T) =
	unique(
		two(K, V, T, T)
	).

:- inst three(K, V, T) =
	bound(
		three(K, V, K, V, T, T, T)
	).

:- inst uniq_three(K, V, T) =
	unique(
		three(K, V, K, V, T, T, T)
	).

:- inst four(K, V, T) =
	bound(
		four(K, V, K, V, K, V, T, T, T, T)
	).

:- inst uniq_four(K, V, T) =
	unique(
		four(K, V, K, V, K, V, T, T, T, T)
	).

:- mode uo_two :: out(uniq_two(unique, unique, unique)).
:- mode suo_two :: out(uniq_two(ground, ground, uniq_tree234_gg)).
:- mode out_two :: out(two(ground, ground, ground)).

:- mode di_two :: di(uniq_two(unique, unique, unique)).
:- mode sdi_two :: di(uniq_two(ground, ground, uniq_tree234_gg)).
:- mode in_two :: in(two(ground, ground, ground)).

:- mode di_three :: di(uniq_three(unique, unique, unique)).
:- mode sdi_three :: di(uniq_three(ground, ground, uniq_tree234_gg)).
:- mode in_three :: in(three(ground, ground, ground)).

:- mode di_four :: di(uniq_four(unique, unique, unique)).
:- mode sdi_four :: di(uniq_four(ground, ground, uniq_tree234_gg)).
:- mode in_four :: in(four(ground, ground, ground)).

%------------------------------------------------------------------------------%

:- pred tree234__split_four(tree234(K, V), K, V, tree234(K, V), tree234(K, V)).
:- mode tree234__split_four(di_four, uo, uo, uo_two, uo_two) is det.
:- mode tree234__split_four(sdi_four, out, out, suo_two, suo_two) is det.
:- mode tree234__split_four(in_four, out, out, out_two, out_two) is det.

tree234__split_four(Tin, MidK, MidV, Sub0, Sub1) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	Sub0 = two(K0, V0, T0, T1),
	MidK = K1,
	MidV = V1,
	Sub1 = two(K2, V2, T2, T3).

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

tree234__insert(Tin, K, V, Tout) :-
	(
		Tin = empty,
		Tout = two(K, V, empty, empty)
	;
		Tin = two(_, _, _, _),
		tree234__insert2(Tin, K, V, Tout)
	;
		Tin = three(_, _, _, _, _, _, _),
		tree234__insert3(Tin, K, V, Tout)
	;
		Tin = four(_, _, _, _, _, _, _, _, _, _),
		tree234__split_four(Tin, MidK, MidV, Sub0, Sub1),
		compare(Result1, K, MidK),
		(
			Result1 = (<),
			tree234__insert2(Sub0, K, V, NewSub0),
			Tout = two(MidK, MidV, NewSub0, Sub1)
		;
			Result1 = (=),
			fail
		;
			Result1 = (>),
			tree234__insert2(Sub1, K, V, NewSub1),
			Tout = two(MidK, MidV, Sub0, NewSub1)
		)
	).

:- pred tree234__insert2(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__insert2(di_two, di, di, uo) is semidet.
:- mode tree234__insert2(sdi_two, in, in, uo_tree234) is semidet.
:- mode tree234__insert2(in_two, in, in, out) is semidet.

tree234__insert2(two(K0, V0, T0, T1), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty
	->
		compare(Result, K, K0),
		(
			Result = (<),
			Tout = three(K, V, K0, V0, empty, empty, empty)
		;
			Result = (=),
			fail
		;
			Result = (>),
			Tout = three(K0, V0, K, V, empty, empty, empty)
		)
	;
		compare(Result, K, K0),
		(
			Result = (<),
			(
				T0 = four(_, _, _, _, _, _, _, _, _, _),
				tree234__split_four(T0, MT0K, MT0V, T00, T01),
				compare(Result1, K, MT0K),
				(
					Result1 = (<),
					tree234__insert2(T00, K, V, NewT00),
					Tout = three(MT0K, MT0V, K0, V0,
						NewT00, T01, T1)
				;
					Result1 = (=),
					fail
				;
					Result1 = (>),
					tree234__insert2(T01, K, V, NewT01),
					Tout = three(MT0K, MT0V, K0, V0,
						T00, NewT01, T1)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234__insert3(T0, K, V, NewT0),
				Tout = two(K0, V0, NewT0, T1)
			;
				T0 = two(_, _, _, _),
				tree234__insert2(T0, K, V, NewT0),
				Tout = two(K0, V0, NewT0, T1)
			;
				T0 = empty,
				NewT0 = two(K, V, empty, empty),
				Tout = two(K0, V0, NewT0, T1)
			)
		;
			Result = (=),
			fail
		;
			Result = (>),
			(
				T1 = four(_, _, _, _, _, _, _, _, _, _),
				tree234__split_four(T1, MT1K, MT1V, T10, T11),
				compare(Result1, K, MT1K),
				(
					Result1 = (<),
					tree234__insert2(T10, K, V, NewT10),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, NewT10, T11)
				;
					Result1 = (=),
					fail
				;
					Result1 = (>),
					tree234__insert2(T11, K, V, NewT11),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, T10, NewT11)
				)
			;
				T1 = three(_, _, _, _, _, _, _),
				tree234__insert3(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = two(_, _, _, _),
				tree234__insert2(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = empty,
				NewT1 = two(K, V, empty, empty),
				Tout = two(K0, V0, T0, NewT1)
			)
		)
	).

:- pred tree234__insert3(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__insert3(di_three, di, di, uo) is semidet.
:- mode tree234__insert3(sdi_three, in, in, uo_tree234) is semidet.
:- mode tree234__insert3(in_three, in, in, out) is semidet.

tree234__insert3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty,
		T2 = empty
	->
		compare(Result0, K, K0),
		(
			Result0 = (<),
			Tout = four(K, V, K0, V0, K1, V1,
				empty, empty, empty, empty)
		;
			Result0 = (=),
			fail
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tout = four(K0, V0, K, V, K1, V1,
					empty, empty, empty, empty)
			;
				Result1 = (=),
				fail
			;
				Result1 = (>),
				Tout = four(K0, V0, K1, V1, K, V,
					empty, empty, empty, empty)
			)
		)
	;
		compare(Result0, K, K0),
		(
			Result0 = (<),
			(
				T0 = four(_, _, _, _, _, _, _, _, _, _),
				tree234__split_four(T0, MT0K, MT0V, T00, T01),
				compare(ResultM, K, MT0K),
				(
					ResultM = (<),
					tree234__insert2(T00, K, V, NewT00),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						NewT00, T01, T1, T2)
				;
					ResultM = (=),
					fail
				;
					ResultM = (>),
					tree234__insert2(T01, K, V, NewT01),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						T00, NewT01, T1, T2)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234__insert3(T0, K, V, NewT0),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			;
				T0 = two(_, _, _, _),
				tree234__insert2(T0, K, V, NewT0),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			;
				T0 = empty,
				NewT0 = two(K, V, empty, empty),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
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
					T1 = four(_, _, _, _, _, _, _, _, _, _),
					tree234__split_four(T1, MT1K, MT1V,
						T10, T11),
					compare(ResultM, K, MT1K),
					(
						ResultM = (<),
						tree234__insert2(T10, K, V,
							NewT10),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, NewT10, T11, T2)
					;
						ResultM = (=),
						fail
					;
						ResultM = (>),
						tree234__insert2(T11, K, V,
							NewT11),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, T10, NewT11, T2)
					)
				;
					T1 = three(_, _, _, _, _, _, _),
					tree234__insert3(T1, K, V, NewT1),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				;
					T1 = two(_, _, _, _),
					tree234__insert2(T1, K, V, NewT1),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				;
					T1 = empty,
					NewT1 = two(K, V, empty, empty),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				)
			;
				Result1 = (=),
				fail
			;
				Result1 = (>),
				(
					T2 = four(_, _, _, _, _, _, _, _, _, _),
					tree234__split_four(T2, MT2K, MT2V,
						T20, T21),
					compare(ResultM, K, MT2K),
					(
						ResultM = (<),
						tree234__insert2(T20, K, V,
							NewT20),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, NewT20, T21)
					;
						ResultM = (=),
						fail
					;
						ResultM = (>),
						tree234__insert2(T21, K, V,
							NewT21),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, T20, NewT21)
					)
				;
					T2 = three(_, _, _, _, _, _, _),
					tree234__insert3(T2, K, V, NewT2),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				;
					T2 = two(_, _, _, _),
					tree234__insert2(T2, K, V, NewT2),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				;
					T2 = empty,
					NewT2 = two(K, V, empty, empty),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				)
			)
		)
	).

%------------------------------------------------------------------------------%

% tree234__set uses the same algorithm as used for tree234__insert,
% except that instead of failing for equal keys, we replace the value.

tree234__set(Tin, K, V, Tout) :-
	(
		Tin = empty,
		Tout = two(K, V, empty, empty)
	;
		Tin = two(_, _, _, _),
		tree234__set2(Tin, K, V, Tout)
	;
		Tin = three(_, _, _, _, _, _, _),
		tree234__set3(Tin, K, V, Tout)
	;
		Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			Sub0 = two(K0, V0, T0, T1),
			Sub1 = two(K2, V2, T2, T3),
			tree234__set2(Sub0, K, V, NewSub0),
			Tout = two(K1, V1, NewSub0, Sub1)
		;
			Result1 = (=),
			Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
		;
			Result1 = (>),
			Sub0 = two(K0, V0, T0, T1),
			Sub1 = two(K2, V2, T2, T3),
			tree234__set2(Sub1, K, V, NewSub1),
			Tout = two(K1, V1, Sub0, NewSub1)
		)
	).

:- pred tree234__set2(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__set2(di_two, di, di, uo) is det.
:- mode tree234__set2(sdi_two, in, in, uo_tree234) is det.
:- mode tree234__set2(in_two, in, in, out) is det.

tree234__set2(two(K0, V0, T0, T1), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty
	->
		compare(Result, K, K0),
		(
			Result = (<),
			Tout = three(K, V, K0, V0, empty, empty, empty)
		;
			Result = (=),
			Tout = two(K, V, T0, T1)
		;
			Result = (>),
			Tout = three(K0, V0, K, V, empty, empty, empty)
		)
	;
		compare(Result, K, K0),
		(
			Result = (<),
			(
				T0 = four(_, _, _, _, _, _, _, _, _, _),
				tree234__split_four(T0, MT0K, MT0V, T00, T01),
				compare(Result1, K, MT0K),
				(
					Result1 = (<),
					tree234__set2(T00, K, V, NewT00),
					Tout = three(MT0K, MT0V, K0, V0,
						NewT00, T01, T1)
				;
					Result1 = (=),
					Tout = three(MT0K, V, K0, V0,
						T00, T01, T1)
				;
					Result1 = (>),
					tree234__set2(T01, K, V, NewT01),
					Tout = three(MT0K, MT0V, K0, V0,
						T00, NewT01, T1)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234__set3(T0, K, V, NewT0),
				Tout = two(K0, V0, NewT0, T1)
			;
				T0 = two(_, _, _, _),
				tree234__set2(T0, K, V, NewT0),
				Tout = two(K0, V0, NewT0, T1)
			;
				T0 = empty,
				NewT0 = two(K, V, empty, empty),
				Tout = two(K0, V0, NewT0, T1)
			)
		;
			Result = (=),
			Tout = two(K, V, T0, T1)
		;
			Result = (>),
			(
				T1 = four(_, _, _, _, _, _, _, _, _, _),
				tree234__split_four(T1, MT1K, MT1V, T10, T11),
				compare(Result1, K, MT1K),
				(
					Result1 = (<),
					tree234__set2(T10, K, V, NewT10),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, NewT10, T11)
				;
					Result1 = (=),
					Tout = three(K0, V0, MT1K, V,
						T0, T10, T11)
				;
					Result1 = (>),
					tree234__set2(T11, K, V, NewT11),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, T10, NewT11)
				)
			;
				T1 = three(_, _, _, _, _, _, _),
				tree234__set3(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = two(_, _, _, _),
				tree234__set2(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = empty,
				NewT1 = two(K, V, empty, empty),
				Tout = two(K0, V0, T0, NewT1)
			)
		)
	).

:- pred tree234__set3(tree234(K, V), K, V, tree234(K, V)).
:- mode tree234__set3(di_three, di, di, uo) is det.
:- mode tree234__set3(sdi_three, in, in, uo_tree234) is det.
:- mode tree234__set3(in_three, in, in, out) is det.

tree234__set3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
	(
		T0 = empty,
		T1 = empty,
		T2 = empty
	->
		compare(Result0, K, K0),
		(
			Result0 = (<),
			Tout = four(K, V, K0, V0, K1, V1,
				empty, empty, empty, empty)
		;
			Result0 = (=),
			Tout = three(K0, V, K1, V1,
				empty, empty, empty)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				Tout = four(K0, V0, K, V, K1, V1,
					empty, empty, empty, empty)
			;
				Result1 = (=),
				Tout = three(K0, V0, K1, V,
					empty, empty, empty)
			;
				Result1 = (>),
				Tout = four(K0, V0, K1, V1, K, V,
					empty, empty, empty, empty)
			)
		)
	;
		compare(Result0, K, K0),
		(
			Result0 = (<),
			(
				T0 = four(_, _, _, _, _, _, _, _, _, _),
				tree234__split_four(T0, MT0K, MT0V, T00, T01),
				compare(ResultM, K, MT0K),
				(
					ResultM = (<),
					tree234__set2(T00, K, V, NewT00),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						NewT00, T01, T1, T2)
				;
					ResultM = (=),
					Tout = four(MT0K, V, K0, V0, K1, V1,
						T00, T01, T1, T2)
				;
					ResultM = (>),
					tree234__set2(T01, K, V, NewT01),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						T00, NewT01, T1, T2)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234__set3(T0, K, V, NewT0),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			;
				T0 = two(_, _, _, _),
				tree234__set2(T0, K, V, NewT0),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			;
				T0 = empty,
				NewT0 = two(K, V, empty, empty),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			)
		;
			Result0 = (=),
			Tout = three(K0, V, K1, V1, T0, T1, T2)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				(
					T1 = four(_, _, _, _, _, _, _, _, _, _),
					tree234__split_four(T1, MT1K, MT1V,
						T10, T11),
					compare(ResultM, K, MT1K),
					(
						ResultM = (<),
						tree234__set2(T10, K, V,
							NewT10),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, NewT10, T11, T2)
					;
						ResultM = (=),
						Tout = four(K0, V0, MT1K, V,
							K1, V1,
							T0, T10, T11, T2)
					;
						ResultM = (>),
						tree234__set2(T11, K, V,
							NewT11),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, T10, NewT11, T2)
					)
				;
					T1 = three(_, _, _, _, _, _, _),
					tree234__set3(T1, K, V, NewT1),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				;
					T1 = two(_, _, _, _),
					tree234__set2(T1, K, V, NewT1),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				;
					T1 = empty,
					NewT1 = two(K, V, empty, empty),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				)
			;
				Result1 = (=),
				Tout = three(K0, V0, K, V, T0, T1, T2)
			;
				Result1 = (>),
				(
					T2 = four(_, _, _, _, _, _, _, _, _, _),
					tree234__split_four(T2, MT2K, MT2V,
						T20, T21),
					compare(ResultM, K, MT2K),
					(
						ResultM = (<),
						tree234__set2(T20, K, V,
							NewT20),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, NewT20, T21)
					;
						ResultM = (=),
						Tout = four(K0, V0, K1, V1,
							MT2K, V,
							T0, T1, T20, T21)
					;
						ResultM = (>),
						tree234__set2(T21, K, V,
							NewT21),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, T20, NewT21)
					)
				;
					T2 = three(_, _, _, _, _, _, _),
					tree234__set3(T2, K, V, NewT2),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				;
					T2 = two(_, _, _, _),
					tree234__set2(T2, K, V, NewT2),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				;
					T2 = empty,
					NewT2 = two(K, V, empty, empty),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				)
			)
		)
	).

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
