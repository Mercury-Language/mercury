%---------------------------------------------------------------------------%
% Copyright (C) 1994-1997,1999-2000,2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%---------------------------------------------------------------------------%

% tree234x - implements a map (dictionary) using 2-3-4 trees.
% main author: conway.
% stability: medium.

% See map.m for documentation.

%---------------------------------------------------------------------------%

:- module tree234x.

:- interface.

:- import_module list, assoc_list.
:- import_module term.	% for var/1.

:- type tree234x(K, V).

:- pred tree234x__init(tree234x(K, V)).
:- mode tree234x__init(uo) is det.

:- func tree234x__init = tree234x(K, V).

:- pred tree234x__is_empty(tree234x(K, V)).
:- mode tree234x__is_empty(in) is semidet.

:- pred tree234x__member(tree234x(K, V), K, V).
:- mode tree234x__member(in, out, out) is nondet.

:- pred tree234x__search(tree234x(K, V), K, V).
:- mode tree234x__search(in, in, out) is semidet.

:- pred tree234x__lookup(tree234x(K, V), K, V).
:- mode tree234x__lookup(in, in, out) is det.

:- func tree234x__lookup(tree234x(K, V), K) = V.

:- pred tree234x__lower_bound_search(tree234x(K, V), K, K, V).
:- mode tree234x__lower_bound_search(in, in, out, out) is semidet.

:- pred tree234x__lower_bound_lookup(tree234x(K, V), K, K, V).
:- mode tree234x__lower_bound_lookup(in, in, out, out) is det.

:- pred tree234x__upper_bound_search(tree234x(K, V), K, K, V).
:- mode tree234x__upper_bound_search(in, in, out, out) is semidet.

:- pred tree234x__upper_bound_lookup(tree234x(K, V), K, K, V).
:- mode tree234x__upper_bound_lookup(in, in, out, out) is det.

:- pred tree234x__insert(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__insert(in, in, in, out) is semidet.
% :- mode tree234x__insert(di_tree234x, in, in, uo_tree234x) is semidet.
% :- mode tree234x__insert(in, in, in, out) is semidet.

:- pred tree234x__set(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__set(di, di, di, uo) is det.
% :- mode tree234x__set(di_tree234x, in, in, uo_tree234x) is det.
:- mode tree234x__set(in, in, in, out) is det.

:- func tree234x__set(tree234x(K, V), K, V) = tree234x(K, V).

:- pred tree234x__delete(tree234x(K, V), K, tree234x(K, V)).
:- mode tree234x__delete(di, in, uo) is det.
% :- mode tree234x__delete(di_tree234x, in, uo_tree234x) is det.
:- mode tree234x__delete(in, in, out) is det.

:- func tree234x__delete(tree234x(K, V), K) = tree234x(K, V).

:- pred tree234x__remove(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__remove(di, in, uo, uo) is semidet.
% :- mode tree234x__remove(di_tree234x, in, out, uo_tree234x) is semidet.
:- mode tree234x__remove(in, in, out, out) is semidet.

:- pred tree234x__remove_smallest(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__remove_smallest(di, uo, uo, uo) is semidet.
% :- mode tree234x__remove_smallest(di_tree234x, out, out, uo_tree234x)
%	is semidet.
:- mode tree234x__remove_smallest(in, out, out, out) is semidet.

	% Given a tree234x, return a list of all the keys in the tree.
	% The list that is returned is in sorted order.
:- pred tree234x__keys(tree234x(K, V), list(K)).
:- mode tree234x__keys(in, out) is det.

:- func tree234x__keys(tree234x(K, V)) = list(K).

:- pred tree234x__values(tree234x(K, V), list(V)).
:- mode tree234x__values(in, out) is det.

:- func tree234x__values(tree234x(K, V)) = list(V).

:- pred tree234x__update(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__update(in, in, in, out) is semidet.
% :- mode tree234x__update(di_tree234x, in, in, uo_tree234x) is det.
% :- mode tree234x__update(di, di, di, uo) is semidet.

	% count the number of elements in a tree
:- pred tree234x__count(tree234x(K, V), int).
:- mode tree234x__count(in, out) is det.

:- func tree234x__count(tree234x(K, V)) = int.

:- pred tree234x__assoc_list_to_tree234x(assoc_list(K, V), tree234x(K, V)).
:- mode tree234x__assoc_list_to_tree234x(in, out) is det.

:- func tree234x__assoc_list_to_tree234x(assoc_list(K, V)) = tree234x(K, V).

	% Given a tree234x, return an association list of all the
	% keys and values in the tree.  The association list that
	% is returned is sorted on the keys.
:- pred tree234x__tree234x_to_assoc_list(tree234x(K, V), assoc_list(K, V)).
:- mode tree234x__tree234x_to_assoc_list(in, out) is det.

:- func tree234x__tree234x_to_assoc_list(tree234x(K, V)) = assoc_list(K, V).

:- pred tree234x__foldl(pred(K, V, T, T), tree234x(K, V), T, T).
:- mode tree234x__foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode tree234x__foldl(pred(in, in, in, out) is semidet, in, in, out)
		is semidet.
:- mode tree234x__foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

:- func tree234x__foldl(func(K, V, T) = T, tree234x(K, V), T) = T.

:- pred tree234x__foldl2(pred(K, V, T, T, U, U), tree234x(K, V), T, T, U, U).
:- mode tree234x__foldl2(pred(in, in, in, out, in, out) is det, 
		in, in, out, in, out) is det.
:- mode tree234x__foldl2(pred(in, in, in, out, in, out) is semidet, 
		in, in, out, in, out) is semidet.
:- mode tree234x__foldl2(pred(in, in, in, out, di, uo) is det,
		in, in, out, di, uo) is det.
:- mode tree234x__foldl2(pred(in, in, di, uo, di, uo) is det,
		in, di, uo, di, uo) is det.


:- pred tree234x__map_values(pred(K, V, W), tree234x(K, V), tree234x(K, W)).
:- mode tree234x__map_values(pred(in, in, out) is det, in, out) is det.
:- mode tree234x__map_values(pred(in, in, out) is semidet, in, out) is semidet.

:- func tree234x__map_values(func(K, V) = W, tree234x(K, V)) = tree234x(K, W).

:- pred tree234x__map_foldl(pred(K, V, W, A, A), tree234x(K, V), tree234x(K, W),
	A, A).
:- mode tree234x__map_foldl(pred(in, in, out, in, out) is det,
	in, out, in, out) is det.
:- mode tree234x__map_foldl(pred(in, in, out, in, out) is semidet,
	in, out, in, out) is semidet.

%------------------------------------------------------------------------------%
%------------------------------------------------------------------------------%

:- implementation.

% Everything below here is not intended to be part of the public interface,
% and will not be included in the Mercury library reference manual.

:- interface.

:- pragma type_spec(tree234x__search/3, K = var(_)).
:- pragma type_spec(tree234x__search/3, K = int).

:- pragma type_spec(tree234x__lookup/3, K = var(_)).

:- pragma type_spec(tree234x__set(in, in, in, out), K = var(_)).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, require, bool, std_util.

:- type tree234x(K, V)	--->
		empty
	;	two(K, V, tree234x(K, V), tree234x(K, V))
	;	three(K, V, K, V, tree234x(K, V), tree234x(K, V), tree234x(K, V))
	;	four(K, V, K, V, K, V, tree234x(K, V), tree234x(K, V),
			tree234x(K, V), tree234x(K, V)).

:- interface.

:- inst uniq_tree234x(K, V) =
	unique((
		empty
	;	two(K, V, uniq_tree234x(K, V), uniq_tree234x(K, V))
	;	three(K, V, K, V, uniq_tree234x(K, V), uniq_tree234x(K, V),
			uniq_tree234x(K, V))
	;	four(K, V, K, V, K, V, uniq_tree234x(K, V), uniq_tree234x(K, V),
			uniq_tree234x(K, V), uniq_tree234x(K, V))
	)).

:- inst uniq_tree234x_gg =
	unique((
		empty
	;	two(ground, ground, uniq_tree234x_gg, uniq_tree234x_gg)
	;	three(ground, ground, ground, ground,
			uniq_tree234x_gg, uniq_tree234x_gg, uniq_tree234x_gg)
	;	four(ground, ground, ground, ground, ground, ground,
			uniq_tree234x_gg, uniq_tree234x_gg,
			uniq_tree234x_gg, uniq_tree234x_gg)
	)).

:- mode di_tree234x(K, V) :: uniq_tree234x(K, V) -> dead.
:- mode di_tree234x       :: uniq_tree234x(ground, ground) -> dead.
:- mode uo_tree234x(K, V) :: free -> uniq_tree234x(K, V).
:- mode uo_tree234x       :: free -> uniq_tree234x(ground, ground).

:- implementation.

%------------------------------------------------------------------------------%

tree234x__init(empty).

tree234x__is_empty(Tree) :-
	Tree = empty.

%------------------------------------------------------------------------------%

tree234x__member(empty, _K, _V) :- fail.
tree234x__member(two(K0, V0, T0, T1), K, V) :-
	(
		K = K0,
		V = V0
	;
		tree234x__member(T0, K, V)
	;
		tree234x__member(T1, K, V)
	).
tree234x__member(three(K0, V0, K1, V1, T0, T1, T2), K, V) :-
	(
		K = K0,
		V = V0
	;
		K = K1,
		V = V1
	;
		tree234x__member(T0, K, V)
	;
		tree234x__member(T1, K, V)
	;
		tree234x__member(T2, K, V)
	).
tree234x__member(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), K, V) :-
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
		tree234x__member(T0, K, V)
	;
		tree234x__member(T1, K, V)
	;
		tree234x__member(T2, K, V)
	;
		tree234x__member(T3, K, V)
	).

%------------------------------------------------------------------------------%

tree234x__search(T, K, V) :-
	(
		T = empty,
		fail
	;
		T = two(K0, V0, T0, T1),
		compare(Result, K, K0),
		(
			Result = (<),
			tree234x__search(T0, K, V)
		;
			Result = (=),
			V = V0
		;
			Result = (>),
			tree234x__search(T1, K, V)
		)
	;
		T = three(K0, V0, K1, V1, T0, T1, T2),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			tree234x__search(T0, K, V)
		;
			Result0 = (=),
			V = V0
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				tree234x__search(T1, K, V)
			;
				Result1 = (=),
				V = V1
			;
				Result1 = (>),
				tree234x__search(T2, K, V)
			)
		)
	;
		T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			compare(Result0, K, K0),
			(
				Result0 = (<),
				tree234x__search(T0, K, V)
			;
				Result0 = (=),
				V = V0
			;
				Result0 = (>),
				tree234x__search(T1, K, V)
			)
		;
			Result1 = (=),
			V = V1
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234x__search(T2, K, V)
			;
				Result2 = (=),
				V = V2
			;
				Result2 = (>),
				tree234x__search(T3, K, V)
			)
		)
	).

tree234x__lookup(T, K, V) :-
	( tree234x__search(T, K, V0) ->
		V = V0
	;
		report_lookup_error("tree234x__lookup: key not found.", K, V)
	).

%------------------------------------------------------------------------------%

tree234x__lower_bound_search(T, SearchK, K, V) :-
	(
		T = empty,
		fail
	;
		T = two(K0, V0, T0, T1),
		compare(Result, SearchK, K0),
		(
			Result = (<),
			tree234x__lower_bound_search(T0, SearchK, K, V)
		;
			Result = (=),
			K = SearchK,
			V = V0
		;
			Result = (>),
			( tree234x__lower_bound_search(T1, SearchK, Kp, Vp) ->
				K = Kp,
				V = Vp
			;
				T = two(_, V0, _, _),
				K = K0,
				V = V0
			)
		)
	;
		T = three(K0, V0, K1, V1, T0, T1, T2),
		compare(Result0, SearchK, K0),
		(
			Result0 = (<),
			tree234x__lower_bound_search(T0, SearchK, K, V)
		;
			Result0 = (=),
			K = SearchK,
			V = V0
		;
			Result0 = (>),
			compare(Result1, SearchK, K1),
			(
				Result1 = (<),
				( tree234x__lower_bound_search(T1, SearchK,
					Kp, Vp)
				-> 
					K = Kp,
					V = Vp
				;
					T = three(_, V0, _, _, _, _, _),
					K = K0,
					V = V0
				)
			;
				Result1 = (=),
				K = SearchK,
				V = V1
			;
				Result1 = (>),
				( tree234x__lower_bound_search(T2, SearchK,
					Kp, Vp)
				-> 
					K = Kp,
					V = Vp
				;
					K = K1,
					V = V1
				)
			)
		)
	;
		T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, SearchK, K1),
		(
			Result1 = (<),
			compare(Result0, SearchK, K0),
			(
				Result0 = (<),
				tree234x__lower_bound_search(T0, SearchK, K, V)
			;
				Result0 = (=),
				K = SearchK,
				V = V0
			;
				Result0 = (>),
				( tree234x__lower_bound_search(T1, SearchK,
					Kp, Vp)
				-> 
					K = Kp,
					V = Vp
				;
					K = K0,
					V = V0
				)
			)
		;
			Result1 = (=),
			K = SearchK,
			V = V1
		;
			Result1 = (>),
			compare(Result2, SearchK, K2),
			(
				Result2 = (<),
				( tree234x__lower_bound_search(T2, SearchK,
					Kp, Vp)
				-> 
					K = Kp,
					V = Vp
				;
					K = K1,
					V = V1
				)
			;
				Result2 = (=),
				K = SearchK,
				V = V2
			;
				Result2 = (>),
				( tree234x__lower_bound_search(T3, SearchK,
					Kp, Vp)
				-> 
					K = Kp,
					V = Vp
				;
					K = K2,
					V = V2
				)
			)
		)
	).

tree234x__lower_bound_lookup(T, SearchK, K, V) :-
	( tree234x__lower_bound_search(T, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("tree234x__lower_bound_lookup: key not found.",
			SearchK, V)
	).

%------------------------------------------------------------------------------%

tree234x__upper_bound_search(T, SearchK, K, V) :-
	(
		T = empty,
		fail
	;
		T = two(K0, V0, T0, T1),
		compare(Result, SearchK, K0),
		(
			Result = (<),
			( tree234x__upper_bound_search(T0, SearchK, Kp, Vp) -> 
				K = Kp,
				V = Vp
			;
				T = two(_, V0, _, _),
				K = K0,
				V = V0
			)
		;
			Result = (=),
			K = SearchK,
			V = V0
		;
			Result = (>),
			tree234x__upper_bound_search(T1, SearchK, K, V)
		)
	;
		T = three(K0, V0, K1, V1, T0, T1, T2),
		compare(Result0, SearchK, K0),
		(
			Result0 = (<),
			( tree234x__upper_bound_search(T0, SearchK, Kp, Vp) ->
				K = Kp,
				V = Vp
			;
				K = K0,
				V = V0
			)
		;
			Result0 = (=),
			K = SearchK,
			V = V0
		;
			Result0 = (>),
			compare(Result1, SearchK, K1),
			(
				Result1 = (<),
				( tree234x__upper_bound_search(T1, SearchK,
					Kp, Vp)
				->
					K = Kp,
					V = Vp
				;
					K = K1,
					V = V1
				)
			;
				Result1 = (=),
				K = SearchK,
				V = V1
			;
				Result1 = (>),
				tree234x__upper_bound_search(T2, SearchK, K, V)
			)
		)
	;
		T = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, SearchK, K1),
		(
			Result1 = (<),
			compare(Result0, SearchK, K0),
			(
				Result0 = (<),
				( tree234x__upper_bound_search(T0, SearchK,
					Kp, Vp)
				->
					K = Kp,
					V = Vp
				;
					K = K0,
					V = V0
				)
			;
				Result0 = (=),
				K = SearchK,
				V = V0
			;
				Result0 = (>),
				( tree234x__upper_bound_search(T1, SearchK,
					Kp, Vp)
				->
					K = Kp,
					V = Vp
				;
					K = K1,
					V = V1
				)
			)
		;
			Result1 = (=),
			K = SearchK,
			V = V1
		;
			Result1 = (>),
			compare(Result2, SearchK, K2),
			(
				Result2 = (<),
				( tree234x__upper_bound_search(T2, SearchK,
					Kp, Vp)
				->
					K = Kp,
					V = Vp
				;
					K = K2,
					V = V2
				)
			;
				Result2 = (=),
				K = SearchK,
				V = V2
			;
				Result2 = (>),
				tree234x__upper_bound_search(T3, SearchK, K, V)
			)
		)
	).

tree234x__upper_bound_lookup(T, SearchK, K, V) :-
	( tree234x__upper_bound_search(T, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("tree234x__upper_bound_lookup: key not found.",
			SearchK, V)
	).

%------------------------------------------------------------------------------%

tree234x__update(Tin, K, V, Tout) :-
	(
		Tin = empty,
		fail
	;
		Tin = two(K0, V0, T0, T1),
		compare(Result, K, K0),
		(
			Result = (<),
			tree234x__update(T0, K, V, NewT0),
			Tout = two(K0, V0, NewT0, T1)
		;
			Result = (=),
			Tout = two(K0, V, T0, T1)
		;
			Result = (>),
			tree234x__update(T1, K, V, NewT1),
			Tout = two(K0, V0, T0, NewT1)
		)
	;
		Tin = three(K0, V0, K1, V1, T0, T1, T2),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			tree234x__update(T0, K, V, NewT0),
			Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
		;
			Result0 = (=),
			Tout = three(K0, V, K1, V1, T0, T1, T2)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				tree234x__update(T1, K, V, NewT1),
				Tout = three(K0, V0, K1, V1, T0, NewT1, T2)
			;
				Result1 = (=),
				Tout = three(K0, V0, K1, V, T0, T1, T2)
			;
				Result1 = (>),
				tree234x__update(T2, K, V, NewT2),
				Tout = three(K0, V0, K1, V1, T0, T1, NewT2)
			)
		)
	;
		Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			compare(Result0, K, K0),
			(
				Result0 = (<),
				tree234x__update(T0, K, V, NewT0),
				Tout = four(K0, V0, K1, V1, K2, V2,
					NewT0, T1, T2, T3)
			;
				Result0 = (=),
				Tout = four(K0, V, K1, V1, K2, V2,
					T0, T1, T2, T3)
			;
				Result0 = (>),
				tree234x__update(T1, K, V, NewT1),
				Tout = four(K0, V0, K1, V1, K2, V2,
					T0, NewT1, T2, T3)
			)
		;
			Result1 = (=),
			Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234x__update(T2, K, V, NewT2),
				Tout = four(K0, V0, K1, V1, K2, V2,
					T0, T1, NewT2, T3)
			;
				Result2 = (=),
				Tout = four(K0, V0, K1, V1, K2, V,
					T0, T1, T2, T3)
			;
				Result2 = (>),
				tree234x__update(T3, K, V, NewT3),
				Tout = four(K0, V0, K1, V1, K2, V2,
					T0, T1, T2, NewT3)
			)
		)
	).

%------------------------------------------------------------------------------%
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
:- mode suo_two :: out(uniq_two(ground, ground, uniq_tree234x_gg)).
:- mode out_two :: out(two(ground, ground, ground)).

:- mode di_two :: di(uniq_two(unique, unique, unique)).
:- mode sdi_two :: di(uniq_two(ground, ground, uniq_tree234x_gg)).
:- mode in_two :: in(two(ground, ground, ground)).

:- mode di_three :: di(uniq_three(unique, unique, unique)).
:- mode sdi_three :: di(uniq_three(ground, ground, uniq_tree234x_gg)).
:- mode in_three :: in(three(ground, ground, ground)).

:- mode di_four :: di(uniq_four(unique, unique, unique)).
:- mode sdi_four :: di(uniq_four(ground, ground, uniq_tree234x_gg)).
:- mode in_four :: in(four(ground, ground, ground)).

%------------------------------------------------------------------------------%

:- pred tree234x__split_four(tree234x(K, V), K, V, tree234x(K, V), tree234x(K, V)).
:- mode tree234x__split_four(di_four, uo, uo, uo_two, uo_two) is det.
% :- mode tree234x__split_four(sdi_four, out, out, suo_two, suo_two) is det.
:- mode tree234x__split_four(in_four, out, out, out_two, out_two) is det.

tree234x__split_four(Tin, MidK, MidV, Sub0, Sub1) :-
	Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
	Sub0 = two(K0, V0, T0, T1),
	MidK = K1,
	MidV = V1,
	Sub1 = two(K2, V2, T2, T3).

%------------------------------------------------------------------------------%

% tree234x__insert is implemented using the simple top-down
% approach described in eg Sedgwick which splits 4 nodes into
% two 2 nodes on the downward traversal of the tree as we
% search for the right place to insert the new key-value pair.
% We know we have the right place if the subtrees of the node
% are empty (in which case we expand the node - which will always
% work because we already split 4 nodes into 2 nodes), or if the
% tree itself is empty.
% This algorithm is O(lgN).

tree234x__insert(Tin, K, V, Tout) :-
	(
		Tin = empty,
		Tout = two(K, V, empty, empty)
	;
		Tin = two(_, _, _, _),
		tree234x__insert2(Tin, K, V, Tout)
	;
		Tin = three(_, _, _, _, _, _, _),
		tree234x__insert3(Tin, K, V, Tout)
	;
		Tin = four(_, _, _, _, _, _, _, _, _, _),
		tree234x__split_four(Tin, MidK, MidV, Sub0, Sub1),
		compare(Result1, K, MidK),
		(
			Result1 = (<),
			tree234x__insert2(Sub0, K, V, NewSub0),
			Tout = two(MidK, MidV, NewSub0, Sub1)
		;
			Result1 = (=),
			fail
		;
			Result1 = (>),
			tree234x__insert2(Sub1, K, V, NewSub1),
			Tout = two(MidK, MidV, Sub0, NewSub1)
		)
	).

:- pred tree234x__insert2(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__insert2(di_two, di, di, uo) is semidet.
% :- mode tree234x__insert2(sdi_two, in, in, uo_tree234x) is semidet.
:- mode tree234x__insert2(in_two, in, in, out) is semidet.

tree234x__insert2(two(K0, V0, T0, T1), K, V, Tout) :-
	(
		T0 = empty
		% T1 = empty implied by T0 = empty
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
				tree234x__split_four(T0, MT0K, MT0V, T00, T01),
				compare(Result1, K, MT0K),
				(
					Result1 = (<),
					tree234x__insert2(T00, K, V, NewT00),
					Tout = three(MT0K, MT0V, K0, V0,
						NewT00, T01, T1)
				;
					Result1 = (=),
					fail
				;
					Result1 = (>),
					tree234x__insert2(T01, K, V, NewT01),
					Tout = three(MT0K, MT0V, K0, V0,
						T00, NewT01, T1)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234x__insert3(T0, K, V, NewT0),
				Tout = two(K0, V0, NewT0, T1)
			;
				T0 = two(_, _, _, _),
				tree234x__insert2(T0, K, V, NewT0),
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
				tree234x__split_four(T1, MT1K, MT1V, T10, T11),
				compare(Result1, K, MT1K),
				(
					Result1 = (<),
					tree234x__insert2(T10, K, V, NewT10),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, NewT10, T11)
				;
					Result1 = (=),
					fail
				;
					Result1 = (>),
					tree234x__insert2(T11, K, V, NewT11),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, T10, NewT11)
				)
			;
				T1 = three(_, _, _, _, _, _, _),
				tree234x__insert3(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = two(_, _, _, _),
				tree234x__insert2(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = empty,
				NewT1 = two(K, V, empty, empty),
				Tout = two(K0, V0, T0, NewT1)
			)
		)
	).

:- pred tree234x__insert3(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__insert3(di_three, di, di, uo) is semidet.
% :- mode tree234x__insert3(sdi_three, in, in, uo_tree234x) is semidet.
:- mode tree234x__insert3(in_three, in, in, out) is semidet.

tree234x__insert3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
	(
		T0 = empty
		% T1 = empty implied by T0 = empty
		% T2 = empty implied by T0 = empty
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
				tree234x__split_four(T0, MT0K, MT0V, T00, T01),
				compare(ResultM, K, MT0K),
				(
					ResultM = (<),
					tree234x__insert2(T00, K, V, NewT00),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						NewT00, T01, T1, T2)
				;
					ResultM = (=),
					fail
				;
					ResultM = (>),
					tree234x__insert2(T01, K, V, NewT01),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						T00, NewT01, T1, T2)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234x__insert3(T0, K, V, NewT0),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			;
				T0 = two(_, _, _, _),
				tree234x__insert2(T0, K, V, NewT0),
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
					tree234x__split_four(T1, MT1K, MT1V,
						T10, T11),
					compare(ResultM, K, MT1K),
					(
						ResultM = (<),
						tree234x__insert2(T10, K, V,
							NewT10),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, NewT10, T11, T2)
					;
						ResultM = (=),
						fail
					;
						ResultM = (>),
						tree234x__insert2(T11, K, V,
							NewT11),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, T10, NewT11, T2)
					)
				;
					T1 = three(_, _, _, _, _, _, _),
					tree234x__insert3(T1, K, V, NewT1),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				;
					T1 = two(_, _, _, _),
					tree234x__insert2(T1, K, V, NewT1),
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
					tree234x__split_four(T2, MT2K, MT2V,
						T20, T21),
					compare(ResultM, K, MT2K),
					(
						ResultM = (<),
						tree234x__insert2(T20, K, V,
							NewT20),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, NewT20, T21)
					;
						ResultM = (=),
						fail
					;
						ResultM = (>),
						tree234x__insert2(T21, K, V,
							NewT21),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, T20, NewT21)
					)
				;
					T2 = three(_, _, _, _, _, _, _),
					tree234x__insert3(T2, K, V, NewT2),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				;
					T2 = two(_, _, _, _),
					tree234x__insert2(T2, K, V, NewT2),
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

% tree234x__set uses the same algorithm as used for tree234x__insert,
% except that instead of failing for equal keys, we replace the value.

tree234x__set(Tin, K, V, Tout) :-
	(
		Tin = empty,
		Tout = two(K, V, empty, empty)
	;
		Tin = two(_, _, _, _),
		tree234x__set2(Tin, K, V, Tout)
	;
		Tin = three(_, _, _, _, _, _, _),
		tree234x__set3(Tin, K, V, Tout)
	;
		Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			Sub0 = two(K0, V0, T0, T1),
			Sub1 = two(K2, V2, T2, T3),
			tree234x__set2(Sub0, K, V, NewSub0),
			Tout = two(K1, V1, NewSub0, Sub1)
		;
			Result1 = (=),
			Tout = four(K0, V0, K1, V, K2, V2, T0, T1, T2, T3)
		;
			Result1 = (>),
			Sub0 = two(K0, V0, T0, T1),
			Sub1 = two(K2, V2, T2, T3),
			tree234x__set2(Sub1, K, V, NewSub1),
			Tout = two(K1, V1, Sub0, NewSub1)
		)
	).

:- pred tree234x__set2(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__set2(di_two, di, di, uo) is det.
% :- mode tree234x__set2(sdi_two, in, in, uo_tree234x) is det.
:- mode tree234x__set2(in_two, in, in, out) is det.
:- pragma type_spec(tree234x__set2(in_two, in, in, out), K = var(_)).

tree234x__set2(two(K0, V0, T0, T1), K, V, Tout) :-
	(
		T0 = empty
		% T1 = empty implied by T0 = empty
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
				tree234x__split_four(T0, MT0K, MT0V, T00, T01),
				compare(Result1, K, MT0K),
				(
					Result1 = (<),
					tree234x__set2(T00, K, V, NewT00),
					Tout = three(MT0K, MT0V, K0, V0,
						NewT00, T01, T1)
				;
					Result1 = (=),
					Tout = three(MT0K, V, K0, V0,
						T00, T01, T1)
				;
					Result1 = (>),
					tree234x__set2(T01, K, V, NewT01),
					Tout = three(MT0K, MT0V, K0, V0,
						T00, NewT01, T1)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234x__set3(T0, K, V, NewT0),
				Tout = two(K0, V0, NewT0, T1)
			;
				T0 = two(_, _, _, _),
				tree234x__set2(T0, K, V, NewT0),
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
				tree234x__split_four(T1, MT1K, MT1V, T10, T11),
				compare(Result1, K, MT1K),
				(
					Result1 = (<),
					tree234x__set2(T10, K, V, NewT10),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, NewT10, T11)
				;
					Result1 = (=),
					Tout = three(K0, V0, MT1K, V,
						T0, T10, T11)
				;
					Result1 = (>),
					tree234x__set2(T11, K, V, NewT11),
					Tout = three(K0, V0, MT1K, MT1V,
						T0, T10, NewT11)
				)
			;
				T1 = three(_, _, _, _, _, _, _),
				tree234x__set3(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = two(_, _, _, _),
				tree234x__set2(T1, K, V, NewT1),
				Tout = two(K0, V0, T0, NewT1)
			;
				T1 = empty,
				NewT1 = two(K, V, empty, empty),
				Tout = two(K0, V0, T0, NewT1)
			)
		)
	).

:- pred tree234x__set3(tree234x(K, V), K, V, tree234x(K, V)).
:- mode tree234x__set3(di_three, di, di, uo) is det.
% :- mode tree234x__set3(sdi_three, in, in, uo_tree234x) is det.
:- mode tree234x__set3(in_three, in, in, out) is det.
:- pragma type_spec(tree234x__set3(in_three, in, in, out), K = var(_)).

tree234x__set3(three(K0, V0, K1, V1, T0, T1, T2), K, V, Tout) :-
	(
		T0 = empty
		% T1 = empty implied by T0 = empty
		% T2 = empty implied by T0 = empty
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
				tree234x__split_four(T0, MT0K, MT0V, T00, T01),
				compare(ResultM, K, MT0K),
				(
					ResultM = (<),
					tree234x__set2(T00, K, V, NewT00),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						NewT00, T01, T1, T2)
				;
					ResultM = (=),
					Tout = four(MT0K, V, K0, V0, K1, V1,
						T00, T01, T1, T2)
				;
					ResultM = (>),
					tree234x__set2(T01, K, V, NewT01),
					Tout = four(MT0K, MT0V, K0, V0, K1, V1,
						T00, NewT01, T1, T2)
				)
			;
				T0 = three(_, _, _, _, _, _, _),
				tree234x__set3(T0, K, V, NewT0),
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2)
			;
				T0 = two(_, _, _, _),
				tree234x__set2(T0, K, V, NewT0),
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
					tree234x__split_four(T1, MT1K, MT1V,
						T10, T11),
					compare(ResultM, K, MT1K),
					(
						ResultM = (<),
						tree234x__set2(T10, K, V,
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
						tree234x__set2(T11, K, V,
							NewT11),
						Tout = four(K0, V0, MT1K, MT1V,
							K1, V1,
							T0, T10, NewT11, T2)
					)
				;
					T1 = three(_, _, _, _, _, _, _),
					tree234x__set3(T1, K, V, NewT1),
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2)
				;
					T1 = two(_, _, _, _),
					tree234x__set2(T1, K, V, NewT1),
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
					tree234x__split_four(T2, MT2K, MT2V,
						T20, T21),
					compare(ResultM, K, MT2K),
					(
						ResultM = (<),
						tree234x__set2(T20, K, V,
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
						tree234x__set2(T21, K, V,
							NewT21),
						Tout = four(K0, V0, K1, V1,
							MT2K, MT2V,
							T0, T1, T20, NewT21)
					)
				;
					T2 = three(_, _, _, _, _, _, _),
					tree234x__set3(T2, K, V, NewT2),
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2)
				;
					T2 = two(_, _, _, _),
					tree234x__set2(T2, K, V, NewT2),
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
%------------------------------------------------------------------------------%

tree234x__delete(Tin, K, Tout) :-
	tree234x__delete_2(Tin, K, Tout, _).

	% When deleting an item from a tree, the height of the tree may be
	% reduced by one. The last argument says whether this has occurred.

:- pred tree234x__delete_2(tree234x(K, V), K, tree234x(K, V), bool).
:- mode tree234x__delete_2(di, in, uo, out) is det.
:- mode tree234x__delete_2(in, in, out, out) is det.

tree234x__delete_2(Tin, K, Tout, RH) :-
	(
		Tin = empty,
		Tout = empty,
		RH = no
	;
		Tin = two(K0, V0, T0, T1),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			tree234x__delete_2(T0, K, NewT0, RHT0),
			( RHT0 = yes ->
				fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
			;
				Tout = two(K0, V0, NewT0, T1),
				RH = no
			)
		;
			Result0 = (=),
			(
				tree234x__remove_smallest_2(T1, ST1K, ST1V,
					NewT1, RHT1)
			->
				( RHT1 = yes ->
					fix_2node_t1(ST1K, ST1V, T0, NewT1,
						Tout, RH)
				;
					Tout = two(ST1K, ST1V, T0, NewT1),
					RH = no
				)
			;
				% T1 must be empty
				Tout = T0,
				RH = yes
			)
		;
			Result0 = (>),
			tree234x__delete_2(T1, K, NewT1, RHT1),
			( RHT1 = yes ->
				fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
			;
				Tout = two(K0, V0, T0, NewT1),
				RH = no
			)
		)
	;
		Tin = three(K0, V0, K1, V1, T0, T1, T2),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			tree234x__delete_2(T0, K, NewT0, RHT0),
			( RHT0 = yes ->
				fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
					Tout, RH)
			;
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
				RH = no
			)
		;
			Result0 = (=),
			(
				tree234x__remove_smallest_2(T1, ST1K, ST1V,
					NewT1, RHT1)
			->
				( RHT1 = yes ->
					fix_3node_t1(ST1K, ST1V, K1, V1,
						T0, NewT1, T2, Tout, RH)
				;
					Tout = three(ST1K, ST1V, K1, V1,
						T0, NewT1, T2),
					RH = no
				)
			;
				% T1 must be empty
				Tout = two(K1, V1, T0, T2),
				RH = no
			)
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				tree234x__delete_2(T1, K, NewT1, RHT1),
				( RHT1 = yes ->
					fix_3node_t1(K0, V0, K1, V1,
						T0, NewT1, T2, Tout, RH)
				;
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2),
					RH = no
				)
			;
				Result1 = (=),
				(
					tree234x__remove_smallest_2(T2,
						ST2K, ST2V, NewT2, RHT2)
				->
					( RHT2 = yes ->
						fix_3node_t2(K0, V0, ST2K, ST2V,
							T0, T1, NewT2, Tout, RH)
					;
						Tout = three(K0, V0, ST2K, ST2V,
							T0, T1, NewT2),
						RH = no
					)
				;
					% T2 must be empty
					Tout = two(K0, V0, T0, T1),
					RH = no
				)
			;
				Result1 = (>),
				tree234x__delete_2(T2, K, NewT2, RHT2),
				( RHT2 = yes ->
					fix_3node_t2(K0, V0, K1, V1,
						T0, T1, NewT2, Tout, RH)
				;
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2),
					RH = no
				)
			)
		)
	;
		Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			compare(Result0, K, K0),
			(
				Result0 = (<),
				tree234x__delete_2(T0, K, NewT0, RHT0),
				( RHT0 = yes ->
					fix_4node_t0(K0, V0, K1, V1, K2, V2,
						NewT0, T1, T2, T3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						NewT0, T1, T2, T3),
					RH = no
				)
			;
				Result0 = (=),
				(
					tree234x__remove_smallest_2(T1,
						ST1K, ST1V, NewT1, RHT1)
				->
					( RHT1 = yes ->
						fix_4node_t1(ST1K, ST1V, K1, V1,
							K2, V2,
							T0, NewT1, T2, T3,
							Tout, RH)
					;
						Tout = four(ST1K, ST1V, K1, V1,
							K2, V2,
							T0, NewT1, T2, T3),
						RH = no
					)
				;
					% T1 must be empty
					Tout = three(K1, V1, K2, V2,
						T0, T2, T3),
					RH = no
				)
			;
				Result0 = (>),
				tree234x__delete_2(T1, K, NewT1, RHT1),
				( RHT1 = yes ->
					fix_4node_t1(K0, V0, K1, V1, K2, V2,
						T0, NewT1, T2, T3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						T0, NewT1, T2, T3),
					RH = no
				)
			)
		;
			Result1 = (=),
			(
				tree234x__remove_smallest_2(T2, ST2K, ST2V,
					NewT2, RHT2)
			->
				( RHT2 = yes ->
					fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
						T0, T1, NewT2, T3, Tout, RH)
				;
					Tout = four(K0, V0, ST2K, ST2V, K2, V2,
						T0, T1, NewT2, T3),
					RH = no
				)
			;
				% T2 must be empty
				Tout = three(K0, V0, K2, V2, T0, T1, T3),
				RH = no
			)
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234x__delete_2(T2, K, NewT2, RHT2),
				( RHT2 = yes ->
					fix_4node_t2(K0, V0, K1, V1, K2, V2,
						T0, T1, NewT2, T3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						T0, T1, NewT2, T3),
					RH = no
				)
			;
				Result2 = (=),
				(
					tree234x__remove_smallest_2(T3,
						ST3K, ST3V, NewT3, RHT3)
				->
					( RHT3 = yes ->
						fix_4node_t3(K0, V0, K1, V1,
							ST3K, ST3V,
							T0, T1, T2, NewT3,
							Tout, RH)
					;
						Tout = four(K0, V0, K1, V1,
							ST3K, ST3V,
							T0, T1, T2, NewT3),
						RH = no
					)
				;
					% T3 must be empty
					Tout = three(K0, V0, K1, V1,
						T0, T1, T2),
					RH = no
				)
			;
				Result2 = (>),
				tree234x__delete_2(T3, K, NewT3, RHT3),
				( RHT3 = yes ->
					fix_4node_t3(K0, V0, K1, V1, K2, V2,
						T0, T1, T2, NewT3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						T0, T1, T2, NewT3),
					RH = no
				)
			)
		)
	).

%------------------------------------------------------------------------------%

	% We use the same algorithm as tree234x__delete.

tree234x__remove(Tin, K, V, Tout) :-
	tree234x__remove_2(Tin, K, V, Tout, _).

:- pred tree234x__remove_2(tree234x(K, V), K, V, tree234x(K, V), bool).
:- mode tree234x__remove_2(di, in, uo, uo, out) is semidet.
:- mode tree234x__remove_2(in, in, out, out, out) is semidet.

tree234x__remove_2(Tin, K, V, Tout, RH) :-
	(
		Tin = empty,
		fail
	;
		Tin = two(K0, V0, T0, T1),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			tree234x__remove_2(T0, K, V, NewT0, RHT0),
			( RHT0 = yes ->
				fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
			;
				Tout = two(K0, V0, NewT0, T1),
				RH = no
			)
		;
			Result0 = (=),
			(
				tree234x__remove_smallest_2(T1, ST1K, ST1V,
					NewT1, RHT1)
			->
				( RHT1 = yes ->
					fix_2node_t1(ST1K, ST1V, T0, NewT1,
						Tout, RH)
				;
					Tout = two(ST1K, ST1V, T0, NewT1),
					RH = no
				)
			;
				% T1 must be empty
				Tout = T0,
				RH = yes
			),
			V = V0
		;
			Result0 = (>),
			tree234x__remove_2(T1, K, V, NewT1, RHT1),
			( RHT1 = yes ->
				fix_2node_t1(K0, V0, T0, NewT1, Tout, RH)
			;
				Tout = two(K0, V0, T0, NewT1),
				RH = no
			)
		)
	;
		Tin = three(K0, V0, K1, V1, T0, T1, T2),
		compare(Result0, K, K0),
		(
			Result0 = (<),
			tree234x__remove_2(T0, K, V, NewT0, RHT0),
			( RHT0 = yes ->
				fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
					Tout, RH)
			;
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
				RH = no
			)
		;
			Result0 = (=),
			(
				tree234x__remove_smallest_2(T1, ST1K, ST1V,
					NewT1, RHT1)
			->
				( RHT1 = yes ->
					fix_3node_t1(ST1K, ST1V, K1, V1,
						T0, NewT1, T2, Tout, RH)
				;
					Tout = three(ST1K, ST1V, K1, V1,
						T0, NewT1, T2),
					RH = no
				)
			;
				% T1 must be empty
				Tout = two(K1, V1, T0, T2),
				RH = no
			),
			V = V0
		;
			Result0 = (>),
			compare(Result1, K, K1),
			(
				Result1 = (<),
				tree234x__remove_2(T1, K, V, NewT1, RHT1),
				( RHT1 = yes ->
					fix_3node_t1(K0, V0, K1, V1,
						T0, NewT1, T2, Tout, RH)
				;
					Tout = three(K0, V0, K1, V1,
						T0, NewT1, T2),
					RH = no
				)
			;
				Result1 = (=),
				(
					tree234x__remove_smallest_2(T2,
						ST2K, ST2V, NewT2, RHT2)
				->
					( RHT2 = yes ->
						fix_3node_t2(K0, V0, ST2K, ST2V,
							T0, T1, NewT2, Tout, RH)
					;
						Tout = three(K0, V0, ST2K, ST2V,
							T0, T1, NewT2),
						RH = no
					)
				;
					% T2 must be empty
					Tout = two(K0, V0, T0, T1),
					RH = no
				),
				V = V1
			;
				Result1 = (>),
				tree234x__remove_2(T2, K, V, NewT2, RHT2),
				( RHT2 = yes ->
					fix_3node_t2(K0, V0, K1, V1,
						T0, T1, NewT2, Tout, RH)
				;
					Tout = three(K0, V0, K1, V1,
						T0, T1, NewT2),
					RH = no
				)
			)
		)
	;
		Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		compare(Result1, K, K1),
		(
			Result1 = (<),
			compare(Result0, K, K0),
			(
				Result0 = (<),
				tree234x__remove_2(T0, K, V, NewT0, RHT0),
				( RHT0 = yes ->
					fix_4node_t0(K0, V0, K1, V1, K2, V2,
						NewT0, T1, T2, T3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						NewT0, T1, T2, T3),
					RH = no
				)
			;
				Result0 = (=),
				(
					tree234x__remove_smallest_2(T1,
						ST1K, ST1V, NewT1, RHT1)
				->
					( RHT1 = yes ->
						fix_4node_t1(ST1K, ST1V, K1, V1,
							K2, V2,
							T0, NewT1, T2, T3,
							Tout, RH)
					;
						Tout = four(ST1K, ST1V, K1, V1,
							K2, V2,
							T0, NewT1, T2, T3),
						RH = no
					)
				;
					% T1 must be empty
					Tout = three(K1, V1, K2, V2,
						T0, T2, T3),
					RH = no
				),
				V = V0
			;
				Result0 = (>),
				tree234x__remove_2(T1, K, V, NewT1, RHT1),
				( RHT1 = yes ->
					fix_4node_t1(K0, V0, K1, V1, K2, V2,
						T0, NewT1, T2, T3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						T0, NewT1, T2, T3),
					RH = no
				)
			)
		;
			Result1 = (=),
			(
				tree234x__remove_smallest_2(T2, ST2K, ST2V,
					NewT2, RHT2)
			->
				( RHT2 = yes ->
					fix_4node_t2(K0, V0, ST2K, ST2V, K2, V2,
						T0, T1, NewT2, T3, Tout, RH)
				;
					Tout = four(K0, V0, ST2K, ST2V, K2, V2,
						T0, T1, NewT2, T3),
					RH = no
				)
			;
				% T2 must be empty
				Tout = three(K0, V0, K2, V2, T0, T1, T3),
				RH = no
			),
			V = V1
		;
			Result1 = (>),
			compare(Result2, K, K2),
			(
				Result2 = (<),
				tree234x__remove_2(T2, K, V, NewT2, RHT2),
				( RHT2 = yes ->
					fix_4node_t2(K0, V0, K1, V1, K2, V2,
						T0, T1, NewT2, T3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						T0, T1, NewT2, T3),
					RH = no
				)
			;
				Result2 = (=),
				(
					tree234x__remove_smallest_2(T3,
						ST3K, ST3V, NewT3, RHT3)
				->
					( RHT3 = yes ->
						fix_4node_t3(K0, V0, K1, V1,
							ST3K, ST3V,
							T0, T1, T2, NewT3,
							Tout, RH)
					;
						Tout = four(K0, V0, K1, V1,
							ST3K, ST3V,
							T0, T1, T2, NewT3),
						RH = no
					)
				;
					% T3 must be empty
					Tout = three(K0, V0, K1, V1,
						T0, T1, T2),
					RH = no
				),
				V = V2
			;
				Result2 = (>),
				tree234x__remove_2(T3, K, V, NewT3, RHT3),
				( RHT3 = yes ->
					fix_4node_t3(K0, V0, K1, V1, K2, V2,
						T0, T1, T2, NewT3, Tout, RH)
				;
					Tout = four(K0, V0, K1, V1, K2, V2,
						T0, T1, T2, NewT3),
					RH = no
				)
			)
		)
	).

%------------------------------------------------------------------------------%

	% The algorithm we use similar to tree234x__delete, except that we
	% always go down the left subtree.

tree234x__remove_smallest(Tin, K, V, Tout) :-
	tree234x__remove_smallest_2(Tin, K, V, Tout, _).

:- pred tree234x__remove_smallest_2(tree234x(K, V), K, V, tree234x(K, V), bool).
:- mode tree234x__remove_smallest_2(di, uo, uo, uo, out) is semidet.
:- mode tree234x__remove_smallest_2(in, out, out, out, out) is semidet.

tree234x__remove_smallest_2(Tin, K, V, Tout, RH) :-
	(
		Tin = empty,
		fail
	;
		Tin = two(K0, V0, T0, T1),
		(
			T0 = empty
		->
			K = K0,
			V = V0,
			Tout = T1,
			RH = yes
		;
			tree234x__remove_smallest_2(T0, K, V, NewT0, RHT0),
			( RHT0 = yes ->
				fix_2node_t0(K0, V0, NewT0, T1, Tout, RH)
			;
				Tout = two(K0, V0, NewT0, T1),
				RH = no
			)
		)
	;
		Tin = three(K0, V0, K1, V1, T0, T1, T2),
		(
			T0 = empty
		->
			K = K0,
			V = V0,
			Tout = two(K1, V1, T1, T2),
			RH = no
		;
			tree234x__remove_smallest_2(T0, K, V, NewT0, RHT0),
			( RHT0 = yes ->
				fix_3node_t0(K0, V0, K1, V1, NewT0, T1, T2,
					Tout, RH)
			;
				Tout = three(K0, V0, K1, V1, NewT0, T1, T2),
				RH = no
			)
		)
	;
		Tin = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		(
			T0 = empty
		->
			K = K0,
			V = V0,
			Tout = three(K1, V1, K2, V2, T1, T2, T3),
			RH = no
		;
			tree234x__remove_smallest_2(T0, K, V, NewT0, RHT0),
			( RHT0 = yes ->
				fix_4node_t0(K0, V0, K1, V1, K2, V2,
					NewT0, T1, T2, T3, Tout, RH)
			;
				Tout = four(K0, V0, K1, V1, K2, V2,
					NewT0, T1, T2, T3),
				RH = no
			)
		)
	).

%------------------------------------------------------------------------------%

	% The input to the following group of predicates are the components
	% of a two-, three- or four-node in which the height of the indicated
	% subtree is one less that it should be. If it is possible to increase
	% the height of that subtree by moving into it elements from its
	% neighboring subtrees, do so, and return the resulting tree with RH
	% set to no. Otherwise, return a balanced tree whose height is reduced
	% by one, with RH set to yes to indicate the reduced height.

:- pred fix_2node_t0(K, V, tree234x(K, V), tree234x(K, V), tree234x(K, V), bool).
:- mode fix_2node_t0(di, di, di, di, uo, out) is det.
:- mode fix_2node_t0(in, in, in, in, out, out) is det.

fix_2node_t0(K0, V0, T0, T1, Tout, RH) :-
	(
		% steal T1's leftmost subtree and combine it with T0
		T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
		NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
		Node = two(K0, V0, T0, T10),
		Tout = two(K10, V10, Node, NewT1),
		RH = no
	;
		% steal T1's leftmost subtree and combine it with T0
		T1 = three(K10, V10, K11, V11, T10, T11, T12),
		NewT1 = two(K11, V11, T11, T12),
		Node = two(K0, V0, T0, T10),
		Tout = two(K10, V10, Node, NewT1),
		RH = no
	;
		% move T0 one level down and combine it with the subtrees of T1
		% this reduces the depth of the tree
		T1 = two(K10, V10, T10, T11),
		Tout = three(K0, V0, K10, V10, T0, T10, T11),
		RH = yes
	;
		T1 = empty,
		error("unbalanced 234 tree")
		% Tout = two(K0, V0, T0, T1),
		% RH = yes
	).

:- pred fix_2node_t1(K, V, tree234x(K, V), tree234x(K, V), tree234x(K, V), bool).
:- mode fix_2node_t1(di, di, di, di, uo, out) is det.
:- mode fix_2node_t1(in, in, in, in, out, out) is det.

fix_2node_t1(K0, V0, T0, T1, Tout, RH) :-
	(
		% steal T0's leftmost subtree and combine it with T1
		T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
		NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
		Node = two(K0, V0, T03, T1),
		Tout = two(K02, V02, NewT0, Node),
		RH = no
	;
		% steal T0's leftmost subtree and combine it with T1
		T0 = three(K00, V00, K01, V01, T00, T01, T02),
		NewT0 = two(K00, V00, T00, T01),
		Node = two(K0, V0, T02, T1),
		Tout = two(K01, V01, NewT0, Node),
		RH = no
	;
		% move T1 one level down and combine it with the subtrees of T0
		% this reduces the depth of the tree
		T0 = two(K00, V00, T00, T01),
		Tout = three(K00, V00, K0, V0, T00, T01, T1),
		RH = yes
	;
		T0 = empty,
		error("unbalanced 234 tree")
		% Tout = two(K0, V0, T0, T1),
		% RH = yes
	).

:- pred fix_3node_t0(K, V, K, V, tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_3node_t0(di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_3node_t0(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t0(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	(
		% steal T1's leftmost subtree and combine it with T0
		T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
		NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
		Node = two(K0, V0, T0, T10),
		Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
		RH = no
	;
		% steal T1's leftmost subtree and combine it with T0
		T1 = three(K10, V10, K11, V11, T10, T11, T12),
		NewT1 = two(K11, V11, T11, T12),
		Node = two(K0, V0, T0, T10),
		Tout = three(K10, V10, K1, V1, Node, NewT1, T2),
		RH = no
	;
		% move T0 one level down to become the leftmost subtree of T1
		T1 = two(K10, V10, T10, T11),
		NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
		Tout = two(K1, V1, NewT1, T2),
		RH = no
	;
		T1 = empty,
		error("unbalanced 234 tree")
		% Tout = three(K0, V0, K1, V1, T0, T1, T2),
		% The heights of T1 and T2 are unchanged
		% RH = no
	).

:- pred fix_3node_t1(K, V, K, V, tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_3node_t1(di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_3node_t1(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t1(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	(
		% steal T0's rightmost subtree and combine it with T1
		T0 = four(K00, V00, K01, V01, K02, V02, T00, T01, T02, T03),
		NewT0 = three(K00, V00, K01, V01, T00, T01, T02),
		Node = two(K0, V0, T03, T1),
		Tout = three(K02, V02, K1, V1, NewT0, Node, T2),
		RH = no
	;
		% steal T0's rightmost subtree and combine it with T1
		T0 = three(K00, V00, K01, V01, T00, T01, T02),
		NewT0 = two(K00, V00, T00, T01),
		Node = two(K0, V0, T02, T1),
		Tout = three(K01, V01, K1, V1, NewT0, Node, T2),
		RH = no
	;
		% move T1 one level down to become the rightmost subtree of T0
		T0 = two(K00, V00, T00, T01),
		NewT0 = three(K00, V00, K0, V0, T00, T01, T1),
		Tout = two(K1, V1, NewT0, T2),
		RH = no
	;
		T0 = empty,
		error("unbalanced 234 tree")
		% Tout = three(K0, V0, K1, V1, T0, T1, T2),
		% The heights of T0 and T2 are unchanged
		% RH = no
	).

:- pred fix_3node_t2(K, V, K, V, tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_3node_t2(di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_3node_t2(in, in, in, in, in, in, in, out, out) is det.

fix_3node_t2(K0, V0, K1, V1, T0, T1, T2, Tout, RH) :-
	(
		% steal T1's rightmost subtree and combine it with T2
		T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
		NewT1 = three(K10, V10, K11, V11, T10, T11, T12),
		Node = two(K1, V1, T13, T2),
		Tout = three(K0, V0, K12, V12, T0, NewT1, Node),
		RH = no
	;
		% steal T1's rightmost subtree and combine it with T2
		T1 = three(K10, V10, K11, V11, T10, T11, T12),
		NewT1 = two(K10, V10, T10, T11),
		Node = two(K1, V1, T12, T2),
		Tout = three(K0, V0, K11, V11, T0, NewT1, Node),
		RH = no
	;
		% move T2 one level down to become the rightmost subtree of T1
		T1 = two(K10, V10, T10, T11),
		NewT1 = three(K10, V10, K1, V1, T10, T11, T2),
		Tout = two(K0, V0, T0, NewT1),
		RH = no
	;
		T1 = empty,
		error("unbalanced 234 tree")
		% Tout = three(K0, V0, K1, V1, T0, T1, T2),
		% The heights of T0 and T1 are unchanged
		% RH = no
	).

:- pred fix_4node_t0(K, V, K, V, K, V,
	tree234x(K, V), tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_4node_t0(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t0(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t0(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	(
		% steal T1's leftmost subtree and combine it with T0
		T1 = four(K10, V10, K11, V11, K12, V12, T10, T11, T12, T13),
		NewT1 = three(K11, V11, K12, V12, T11, T12, T13),
		Node = two(K0, V0, T0, T10),
		Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
		RH = no
	;
		% steal T1's leftmost subtree and combine it with T0
		T1 = three(K10, V10, K11, V11, T10, T11, T12),
		NewT1 = two(K11, V11, T11, T12),
		Node = two(K0, V0, T0, T10),
		Tout = four(K10, V10, K1, V1, K2, V2, Node, NewT1, T2, T3),
		RH = no
	;
		% move T0 one level down to become the leftmost subtree of T1
		T1 = two(K10, V10, T10, T11),
		NewT1 = three(K0, V0, K10, V10, T0, T10, T11),
		Tout = three(K1, V1, K2, V2, NewT1, T2, T3),
		RH = no
	;
		T1 = empty,
		error("unbalanced 234 tree")
		% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		% The heights of T1, T2 and T3 are unchanged
		% RH = no
	).

:- pred fix_4node_t1(K, V, K, V, K, V,
	tree234x(K, V), tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_4node_t1(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t1(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t1(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	(
		% steal T2's leftmost subtree and combine it with T1
		T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
		NewT2 = three(K21, V21, K22, V22, T21, T22, T23),
		Node = two(K1, V1, T1, T20),
		Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
		RH = no
	;
		% steal T2's leftmost subtree and combine it with T1
		T2 = three(K20, V20, K21, V21, T20, T21, T22),
		NewT2 = two(K21, V21, T21, T22),
		Node = two(K1, V1, T1, T20),
		Tout = four(K0, V0, K20, V20, K2, V2, T0, Node, NewT2, T3),
		RH = no
	;
		% move T1 one level down to become the leftmost subtree of T2
		T2 = two(K20, V20, T20, T21),
		NewT2 = three(K1, V1, K20, V20, T1, T20, T21),
		Tout = three(K0, V0, K2, V2, T0, NewT2, T3),
		RH = no
	;
		T2 = empty,
		error("unbalanced 234 tree")
		% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		% The heights of T0, T2 and T3 are unchanged
		% RH = no
	).

:- pred fix_4node_t2(K, V, K, V, K, V,
	tree234x(K, V), tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_4node_t2(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t2(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t2(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	(
		% steal T3's leftmost subtree and combine it with T2
		T3 = four(K30, V30, K31, V31, K32, V32, T30, T31, T32, T33),
		NewT3 = three(K31, V31, K32, V32, T31, T32, T33),
		Node = two(K2, V2, T2, T30),
		Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
		RH = no
	;
		% steal T3's leftmost subtree and combine it with T2
		T3 = three(K30, V30, K31, V31, T30, T31, T32),
		NewT3 = two(K31, V31, T31, T32),
		Node = two(K2, V2, T2, T30),
		Tout = four(K0, V0, K1, V1, K30, V30, T0, T1, Node, NewT3),
		RH = no
	;
		% move T2 one level down to become the leftmost subtree of T3
		T3 = two(K30, V30, T30, T31),
		NewT3 = three(K2, V2, K30, V30, T2, T30, T31),
		Tout = three(K0, V0, K1, V1, T0, T1, NewT3),
		RH = no
	;
		T3 = empty,
		error("unbalanced 234 tree")
		% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		% The heights of T0, T1 and T3 are unchanged
		% RH = no
	).

:- pred fix_4node_t3(K, V, K, V, K, V,
	tree234x(K, V), tree234x(K, V), tree234x(K, V), tree234x(K, V),
	tree234x(K, V), bool).
:- mode fix_4node_t3(di, di, di, di, di, di, di, di, di, di, uo, out) is det.
:- mode fix_4node_t3(in, in, in, in, in, in, in, in, in, in, out, out) is det.

fix_4node_t3(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3, Tout, RH) :-
	(
		% steal T2's rightmost subtree and combine it with T3
		T2 = four(K20, V20, K21, V21, K22, V22, T20, T21, T22, T23),
		NewT2 = three(K20, V20, K21, V21, T20, T21, T22),
		Node = two(K2, V2, T23, T3),
		Tout = four(K0, V0, K1, V1, K22, V22, T0, T1, NewT2, Node),
		RH = no
	;
		% steal T2's rightmost subtree and combine it with T3
		T2 = three(K20, V20, K21, V21, T20, T21, T22),
		NewT2 = two(K20, V20, T20, T21),
		Node = two(K2, V2, T22, T3),
		Tout = four(K0, V0, K1, V1, K21, V21, T0, T1, NewT2, Node),
		RH = no
	;
		% move T3 one level down to become the rightmost subtree of T2
		T2 = two(K20, V20, T20, T21),
		NewT2 = three(K20, V20, K2, V2, T20, T21, T3),
		Tout = three(K0, V0, K1, V1, T0, T1, NewT2),
		RH = no
	;
		T2 = empty,
		error("unbalanced 234 tree")
		% Tout = four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
		% The heights of T0, T1 and T2 are unchanged
		% RH = no
	).

%------------------------------------------------------------------------------%

tree234x__keys(Tree, Keys) :-
	tree234x__keys_2(Tree, [], Keys).

:- pred tree234x__keys_2(tree234x(K, V), list(K), list(K)).
:- mode tree234x__keys_2(in, in, out) is det.

tree234x__keys_2(empty, List, List).
tree234x__keys_2(two(K0, _V0, T0, T1), L0, L) :-
	tree234x__keys_2(T1, L0, L1),
	tree234x__keys_2(T0, [K0 | L1], L).
tree234x__keys_2(three(K0, _V0, K1, _V1, T0, T1, T2), L0, L) :-
	tree234x__keys_2(T2, L0, L1),
	tree234x__keys_2(T1, [K1 | L1], L2),
	tree234x__keys_2(T0, [K0 | L2], L).
tree234x__keys_2(four(K0, _V0, K1, _V1, K2, _V2, T0, T1, T2, T3), L0, L) :-
	tree234x__keys_2(T3, L0, L1),
	tree234x__keys_2(T2, [K2 | L1], L2),
	tree234x__keys_2(T1, [K1 | L2], L3),
	tree234x__keys_2(T0, [K0 | L3], L).

%------------------------------------------------------------------------------%

tree234x__values(Tree, Values) :-
	tree234x__values_2(Tree, [], Values).

:- pred tree234x__values_2(tree234x(K, V), list(V), list(V)).
:- mode tree234x__values_2(in, in, out) is det.

tree234x__values_2(empty, List, List).
tree234x__values_2(two(_K0, V0, T0, T1), L0, L) :-
	tree234x__values_2(T1, L0, L1),
	tree234x__values_2(T0, [V0 | L1], L).
tree234x__values_2(three(_K0, V0, _K1, V1, T0, T1, T2), L0, L) :-
	tree234x__values_2(T2, L0, L1),
	tree234x__values_2(T1, [V1 | L1], L2),
	tree234x__values_2(T0, [V0 | L2], L).
tree234x__values_2(four(_K0, V0, _K1, V1, _K2, V2, T0, T1, T2, T3), L0, L) :-
	tree234x__values_2(T3, L0, L1),
	tree234x__values_2(T2, [V2 | L1], L2),
	tree234x__values_2(T1, [V1 | L2], L3),
	tree234x__values_2(T0, [V0 | L3], L).

%------------------------------------------------------------------------------%

tree234x__assoc_list_to_tree234x(AssocList, Tree) :-
	tree234x__assoc_list_to_tree234x_2(AssocList, empty, Tree).

:- pred tree234x__assoc_list_to_tree234x_2(assoc_list(K, V), tree234x(K, V),
					tree234x(K, V)).
:- mode tree234x__assoc_list_to_tree234x_2(in, in, out) is det.

tree234x__assoc_list_to_tree234x_2([], Tree, Tree).
tree234x__assoc_list_to_tree234x_2([K - V | Rest], Tree0, Tree) :-
	tree234x__set(Tree0, K, V, Tree1),
	tree234x__assoc_list_to_tree234x_2(Rest, Tree1, Tree).

%------------------------------------------------------------------------------%

tree234x__tree234x_to_assoc_list(Tree, AssocList) :-
	tree234x__tree234x_to_assoc_list_2(Tree, [], AssocList).

:- pred tree234x__tree234x_to_assoc_list_2(tree234x(K, V), assoc_list(K, V),
						assoc_list(K, V)).
:- mode tree234x__tree234x_to_assoc_list_2(in, in, out) is det.

tree234x__tree234x_to_assoc_list_2(empty, List, List).
tree234x__tree234x_to_assoc_list_2(two(K0, V0, T0, T1), L0, L) :-
	tree234x__tree234x_to_assoc_list_2(T1, L0, L1),
	tree234x__tree234x_to_assoc_list_2(T0, [K0 - V0 | L1], L).
tree234x__tree234x_to_assoc_list_2(three(K0, V0, K1, V1, T0, T1, T2), L0, L) :-
	tree234x__tree234x_to_assoc_list_2(T2, L0, L1),
	tree234x__tree234x_to_assoc_list_2(T1, [K1 - V1 | L1], L2),
	tree234x__tree234x_to_assoc_list_2(T0, [K0 - V0 | L2], L).
tree234x__tree234x_to_assoc_list_2(four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3),
					L0, L) :-
	tree234x__tree234x_to_assoc_list_2(T3, L0, L1),
	tree234x__tree234x_to_assoc_list_2(T2, [K2 - V2 | L1], L2),
	tree234x__tree234x_to_assoc_list_2(T1, [K1 - V1 | L2], L3),
	tree234x__tree234x_to_assoc_list_2(T0, [K0 - V0 | L3], L).

%------------------------------------------------------------------------------%

tree234x__foldl(_Pred, empty, Acc, Acc).
tree234x__foldl(Pred, two(K, V, T0, T1), Acc0, Acc) :-
	tree234x__foldl(Pred, T0, Acc0, Acc1),
	call(Pred, K, V, Acc1, Acc2),
	tree234x__foldl(Pred, T1, Acc2, Acc).
tree234x__foldl(Pred, three(K0, V0, K1, V1, T0, T1, T2), Acc0, Acc) :-
	tree234x__foldl(Pred, T0, Acc0, Acc1),
	call(Pred, K0, V0, Acc1, Acc2),
	tree234x__foldl(Pred, T1, Acc2, Acc3),
	call(Pred, K1, V1, Acc3, Acc4),
	tree234x__foldl(Pred, T2, Acc4, Acc).
tree234x__foldl(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), Acc0, Acc) :-
	tree234x__foldl(Pred, T0, Acc0, Acc1),
	call(Pred, K0, V0, Acc1, Acc2),
	tree234x__foldl(Pred, T1, Acc2, Acc3),
	call(Pred, K1, V1, Acc3, Acc4),
	tree234x__foldl(Pred, T2, Acc4, Acc5),
	call(Pred, K2, V2, Acc5, Acc6),
	tree234x__foldl(Pred, T3, Acc6, Acc).

tree234x__foldl2(_Pred, empty, A, A) --> [].
tree234x__foldl2(Pred, two(K, V, T0, T1), A0, A) -->
	tree234x__foldl2(Pred, T0, A0, A1),
	call(Pred, K, V, A1, A2),
	tree234x__foldl2(Pred, T1, A2, A).
tree234x__foldl2(Pred, three(K0, V0, K1, V1, T0, T1, T2), A0, A) -->
	tree234x__foldl2(Pred, T0, A0, A1),
	call(Pred, K0, V0, A1, A2),
	tree234x__foldl2(Pred, T1, A2, A3),
	call(Pred, K1, V1, A3, A4),
	tree234x__foldl2(Pred, T2, A4, A).
tree234x__foldl2(Pred, four(K0, V0, K1, V1, K2, V2, T0, T1, T2, T3), A0, A) -->
	tree234x__foldl2(Pred, T0, A0, A1),
	call(Pred, K0, V0, A1, A2),
	tree234x__foldl2(Pred, T1, A2, A3),
	call(Pred, K1, V1, A3, A4),
	tree234x__foldl2(Pred, T2, A4, A5),
	call(Pred, K2, V2, A5, A6),
	tree234x__foldl2(Pred, T3, A6, A).

%------------------------------------------------------------------------------%

tree234x__map_values(_Pred, empty, empty).
tree234x__map_values(Pred, Tree0, Tree) :-
	Tree0 = two(K0, V0, Left0, Right0),
	Tree  = two(K0, W0, Left, Right),
	call(Pred, K0, V0, W0),
	tree234x__map_values(Pred, Left0, Left),
	tree234x__map_values(Pred, Right0, Right).
tree234x__map_values(Pred, Tree0, Tree) :-
	Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
	Tree  = three(K0, W0, K1, W1, Left, Middle, Right),
	call(Pred, K0, V0, W0),
	call(Pred, K1, V1, W1),
	tree234x__map_values(Pred, Left0, Left),
	tree234x__map_values(Pred, Middle0, Middle),
	tree234x__map_values(Pred, Right0, Right).
tree234x__map_values(Pred, Tree0, Tree) :-
	Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
	Tree  = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right),
	call(Pred, K0, V0, W0),
	call(Pred, K1, V1, W1),
	call(Pred, K2, V2, W2),
	tree234x__map_values(Pred, Left0, Left),
	tree234x__map_values(Pred, LMid0, LMid),
	tree234x__map_values(Pred, RMid0, RMid),
	tree234x__map_values(Pred, Right0, Right).

%------------------------------------------------------------------------------%

tree234x__map_foldl(_Pred, empty, empty, A, A).
tree234x__map_foldl(Pred, Tree0, Tree, A0, A) :-
	Tree0 = two(K0, V0, Left0, Right0),
	Tree  = two(K0, W0, Left, Right),
	tree234x__map_foldl(Pred, Left0, Left, A0, A1),
	call(Pred, K0, V0, W0, A1, A2),
	tree234x__map_foldl(Pred, Right0, Right, A2, A).
tree234x__map_foldl(Pred, Tree0, Tree, A0, A) :-
	Tree0 = three(K0, V0, K1, V1, Left0, Middle0, Right0),
	Tree  = three(K0, W0, K1, W1, Left, Middle, Right),
	tree234x__map_foldl(Pred, Left0, Left, A0, A1),
	call(Pred, K0, V0, W0, A1, A2),
	tree234x__map_foldl(Pred, Middle0, Middle, A2, A3),
	call(Pred, K1, V1, W1, A3, A4),
	tree234x__map_foldl(Pred, Right0, Right, A4, A).
tree234x__map_foldl(Pred, Tree0, Tree, A0, A) :-
	Tree0 = four(K0, V0, K1, V1, K2, V2, Left0, LMid0, RMid0, Right0),
	Tree  = four(K0, W0, K1, W1, K2, W2, Left, LMid, RMid, Right),
	tree234x__map_foldl(Pred, Left0, Left, A0, A1),
	call(Pred, K0, V0, W0, A1, A2),
	tree234x__map_foldl(Pred, LMid0, LMid, A2, A3),
	call(Pred, K1, V1, W1, A3, A4),
	tree234x__map_foldl(Pred, RMid0, RMid, A4, A5),
	call(Pred, K2, V2, W2, A5, A6),
	tree234x__map_foldl(Pred, Right0, Right, A6, A).

%------------------------------------------------------------------------------%

	% count the number of elements in a tree
tree234x__count(empty, 0).
tree234x__count(two(_, _, T0, T1), N) :-
	tree234x__count(T0, N0),
	tree234x__count(T1, N1),
	N is 1 + N0 + N1.
tree234x__count(three(_, _, _, _, T0, T1, T2), N) :-
	tree234x__count(T0, N0),
	tree234x__count(T1, N1),
	tree234x__count(T2, N2),
	N is 2 + N0 + N1 + N2.
tree234x__count(four(_, _, _, _, _, _, T0, T1, T2, T3), N) :-
	tree234x__count(T0, N0),
	tree234x__count(T1, N1),
	tree234x__count(T2, N2),
	tree234x__count(T3, N3),
	N is 3 + N0 + N1 + N2 + N3.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 30/04/99
%       Function forms added.

tree234x__init = T :-
	tree234x__init(T).

tree234x__lookup(T, K) = V :-
	tree234x__lookup(T, K, V).

tree234x__set(T1, K, V) = T2 :-
	tree234x__set(T1, K, V, T2).

tree234x__delete(T1, K) = T2 :-
	tree234x__delete(T1, K, T2).

tree234x__keys(T) = Ks :-
	tree234x__keys(T, Ks).

tree234x__values(T) = Vs :-
	tree234x__values(T, Vs).

tree234x__count(T) = N :-
	tree234x__count(T, N).

tree234x__assoc_list_to_tree234x(AL) = T :-
	tree234x__assoc_list_to_tree234x(AL, T).

tree234x__tree234x_to_assoc_list(T) = AL :-
	tree234x__tree234x_to_assoc_list(T, AL).

tree234x__foldl(F, T, A) = B :-
	P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
	tree234x__foldl(P, T, A, B).

tree234x__map_values(F, T1) = T2 :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	tree234x__map_values(P, T1, T2).
