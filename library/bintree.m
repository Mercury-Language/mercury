%---------------------------------------------------------------------------%
% Copyright (C) 1993-1995, 1997, 1999, 2002 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: bintree.m.
% Main author: conway.
% Stability: medium (obsolete).
%
% This module exists primarily for historical reasons.  It is unlikely
% to be useful, and may not be supported in future releases.
% You should use `map' instead.
%
% This file provides a straight-forward binary search tree implementation of
% a map (dictionary).
%
% bintree__insert, bintree__update, and
% bintree__set differ only in how they handle the case where the value
% being inserted already exists in the tree.  `insert' will only insert
% new keys, and will fail if you attempt to insert an existing key into
% the tree. `update' will only allow you to modify the data for existing
% keys, and will fail if the key isn't already in the tree.  `set' will
% always succeed; it will replace the old value for that key if the key
% was already in the tree, or insert a new node into the tree if the key
% wasn't already present.
% 
%-----------------------------------------------------------------------------%

:- module bintree.
:- interface.
:- import_module list, assoc_list.

:- type bintree(K, V).

:- pred bintree__init(bintree(K,V)).
:- mode bintree__init(uo) is det.

:- pred bintree__insert(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__insert(in, in, in, out) is semidet.

:- pred bintree__update(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__update(in, in, in, out) is semidet.

:- pred bintree__set(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__set(di, di, di, uo) is det.
:- mode bintree__set(in, in, in, out) is det.

:- func bintree__set(bintree(K,V), K, V) = bintree(K,V).

:- pred bintree__search(bintree(K,V), K, V).
:- mode bintree__search(in, in, in) is semidet.	% implied
:- mode bintree__search(in, in, out) is semidet.

:- pred bintree__lookup(bintree(K,V), K, V).
:- mode bintree__lookup(in, in, out) is det.

:- func bintree__lookup(bintree(K,V), K) = V.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next lower key instead.
	% Fails if there is no key with the given or lower value.
:- pred bintree__lower_bound_search(bintree(K,V), K, K, V).
:- mode bintree__lower_bound_search(in, in, out, out) is semidet.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next lower key instead.
	% Aborts if there is no key with the given or lower value.
:- pred bintree__lower_bound_lookup(bintree(K,V), K, K, V).
:- mode bintree__lower_bound_lookup(in, in, out, out) is det.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next higher key instead.
	% Fails if there is no key with the given or higher value.
:- pred bintree__upper_bound_search(bintree(K,V), K, K, V).
:- mode bintree__upper_bound_search(in, in, out, out) is semidet.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next higher key instead.
	% Aborts if there is no key with the given or higher value.
:- pred bintree__upper_bound_lookup(bintree(K,V), K, K, V).
:- mode bintree__upper_bound_lookup(in, in, out, out) is det.

:- pred bintree__delete(bintree(K,V), K, bintree(K,V)).
:- mode bintree__delete(in, in, out) is det.

:- func bintree__delete(bintree(K,V), K) = bintree(K,V).

:- pred bintree__remove(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__remove(in, in, out, out) is semidet.

:- pred bintree__keys(bintree(K,_V), list(K)).
:- mode bintree__keys(in, out) is det.

:- func bintree__keys(bintree(K,_V)) = list(K).

:- pred bintree__values(bintree(_K,V), list(V)).
:- mode bintree__values(in, out) is det.

:- func bintree__values(bintree(_K,V)) = list(V).

:- pred bintree__from_list(assoc_list(K,V), bintree(K,V)).
:- mode bintree__from_list(in, out) is det.

:- func bintree__from_list(assoc_list(K,V)) = bintree(K,V).

:- pred bintree__from_sorted_list(assoc_list(K,V), bintree(K,V)).
:- mode bintree__from_sorted_list(in, out) is det.

:- func bintree__from_sorted_list(assoc_list(K,V)) = bintree(K,V).

:- pred bintree__from_corresponding_lists(list(K), list(V), bintree(K,V)).
:- mode bintree__from_corresponding_lists(in, in, out) is det.

:- func bintree__from_corresponding_lists(list(K), list(V)) = bintree(K,V).

:- pred bintree__to_list(bintree(K,V), assoc_list(K,V)).
:- mode bintree__to_list(in, out) is det.

:- func bintree__to_list(bintree(K,V)) = assoc_list(K,V).

	% count the number of elements in a tree
:- pred bintree__count(bintree(_K,_V), int).
:- mode bintree__count(in, out) is det.

:- func bintree__count(bintree(_K,_V)) = int.

	% count the depth of a tree
:- pred bintree__depth(bintree(_K,_V), int).
:- mode bintree__depth(in, out) is det.

:- func bintree__depth(bintree(_K,_V)) = int.

:- pred bintree__branching_factor(bintree(_K,_V), int, int).
:- mode bintree__branching_factor(in, out, out) is det.

:- pred bintree__balance(bintree(K, V), bintree(K, V)).
:- mode bintree__balance(in, out) is det.

:- func bintree__balance(bintree(K, V)) = bintree(K, V).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module require, int, std_util.

:- type bintree(K,V)		--->	empty
				;	tree(
						K,
						V,
						bintree(K,V),
						bintree(K,V)
					).

%-----------------------------------------------------------------------------%

bintree__init(empty).

%-----------------------------------------------------------------------------%

bintree__insert(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree__insert(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(
		Result = (=)
	->
		fail
	;
		Result = (<)
	->
		bintree__insert(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	;
		bintree__insert(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__update(empty, _Key, _Value, _Tree) :-
	fail.
bintree__update(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(
		Result = (=)
	->
		Tree = tree(Key0, Value, Left, Right)
	;
		Result = (<)
	->
		bintree__update(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	;
		bintree__update(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__set(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree__set(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(
		Result = (=)
	->
		Tree = tree(Key0, Value, Left, Right)
	;
		Result = (<)
	->
		bintree__set(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	;
		bintree__set(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__search(tree(K0, V0, Left, Right), K, V) :-
	compare(Result, K0, K),
	(
		Result = (=)
	->
		V = V0
	;
		Result = (<)
	->
		bintree__search(Right, K, V)
	;
		bintree__search(Left, K, V)
	).

%-----------------------------------------------------------------------------%

bintree__lookup(Tree, K, V) :-
	( bintree__search(Tree, K, V0) ->
		V = V0
	;
		report_lookup_error("bintree__lookup: key not found", K, V)
	).

%-----------------------------------------------------------------------------%

bintree__lower_bound_search(tree(K0, V0, Left, Right), SearchK, K, V) :-
	compare(Result, K0, SearchK),
	(
		Result = (=)
	->
		K = K0,
		V = V0
	;
		Result = (<)
	->
		( bintree__lower_bound_search(Right, SearchK, Kp, Vp) ->
			K = Kp,
			V = Vp
		;
			K = K0,
			V = V0
		)
	;
		bintree__lower_bound_search(Left, SearchK, K, V)
	).

bintree__lower_bound_lookup(Tree, SearchK, K, V) :-
	( bintree__lower_bound_search(Tree, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("bintree__lower_bound_lookup: key not found",
			SearchK, V)
	).

%-----------------------------------------------------------------------------%

bintree__upper_bound_search(tree(K0, V0, Left, Right), SearchK, K, V) :-
	compare(Result, K0, SearchK),
	(
		Result = (=)
	->
		K = K0,
		V = V0
	;
		Result = (<)
	->
		bintree__upper_bound_search(Right, SearchK, K, V)
	;
		( bintree__upper_bound_search(Left, SearchK, Kp, Vp) ->
			K = Kp,
			V = Vp
		;
			K = K0,
			V = V0
		)
	).

bintree__upper_bound_lookup(Tree, SearchK, K, V) :-
	( bintree__upper_bound_search(Tree, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("bintree__lower_bound_lookup: key not found",
			SearchK, V)
	).

%-----------------------------------------------------------------------------%

bintree__delete(empty, _K, empty).
bintree__delete(tree(K0, V0, Left, Right), K, Tree) :-
	compare(Result, K0, K),
	(
		Result = (=)
	->
		bintree__fixup(Left, Right, Tree)
	;
		Result = (<)
	->
		bintree__delete(Right, K, Tree1),
		Tree = tree(K0, V0, Left, Tree1)
	;
		bintree__delete(Left, K, Tree1),
		Tree = tree(K0, V0, Tree1, Right)
	).

%-----------------------------------------------------------------------------%

bintree__remove(tree(K0, V0, Left, Right), K, V, Tree) :-
	compare(Result, K0, K),
	(
		Result = (=)
	->
		V = V0,
		bintree__fixup(Left, Right, Tree)
	;
		Result = (<)
	->
		bintree__remove(Right, K, V, Tree1),
		Tree = tree(K0, V0, Left, Tree1)
	;
		bintree__remove(Left, K, V, Tree1),
		Tree = tree(K0, V0, Tree1, Right)
	).

%-----------------------------------------------------------------------------%

:- pred bintree__fixup(bintree(K,V), bintree(K,V), bintree(K,V)).
:- mode bintree__fixup(in, in, out) is det.

bintree__fixup(Left, Right, Tree) :-
	(
		Left = empty
	->
		Tree = Right
	;
		Right = empty
	->
		Tree = Left
	;
		bintree__right_depth(Left, LD),
		bintree__left_depth(Right, RD),
		(
			LD > RD
		->
			bintree__knock_left(Left, K, V, Left1),
			Right1 = Right
		;
			bintree__knock_right(Right, K, V, Right1),
			Left1 = Left
		),
		Tree = tree(K, V, Left1, Right1)
	).

:- pred bintree__right_depth(bintree(_K,_V), int).
:- mode bintree__right_depth(in, out) is det.

bintree__right_depth(empty, 0).
bintree__right_depth(tree(_K, _V, _Left, Right), N) :-
	bintree__right_depth(Right, M),
	N is M + 1.

:- pred bintree__left_depth(bintree(_K,_V), int).
:- mode bintree__left_depth(in, out) is det.

bintree__left_depth(empty, 0).
bintree__left_depth(tree(_K, _V, Left, _Right), N) :-
	bintree__left_depth(Left, M),
	N is M + 1.

:- pred bintree__knock_left(bintree(K,V), K, V, bintree(K, V)).
:- mode bintree__knock_left(in, out, out, out) is det.

bintree__knock_left(empty, _, _, _) :-
	error("bintree__knock_left: empty tree").
bintree__knock_left(tree(K0, V0, Left, Right), K, V, Tree) :-
	(
		Right = empty
	->
		K = K0,
		V = V0,
		Tree = Left
	;
		bintree__knock_left(Right, K, V, Right1),
		Tree = tree(K0, V0, Left, Right1)
	).

:- pred bintree__knock_right(bintree(K,V), K, V, bintree(K, V)).
:- mode bintree__knock_right(in, out, out, out) is det.

bintree__knock_right(empty, _, _, _) :-
	error("bintree__knock_right: empty tree").
bintree__knock_right(tree(K0, V0, Left, Right), K, V, Tree) :-
	(
		Left = empty
	->
		K = K0,
		V = V0,
		Tree = Right
	;
		bintree__knock_right(Left, K, V, Left1),
		Tree = tree(K0, V0, Left1, Right)
	).

%-----------------------------------------------------------------------------%

bintree__from_list(List, Tree) :-
	bintree__from_list_2(List, empty, Tree).

:- pred bintree__from_list_2(assoc_list(K,V), bintree(K,V), bintree(K,V)).
:- mode bintree__from_list_2(in, in, out) is det.

bintree__from_list_2([], Tree, Tree).
bintree__from_list_2([K - V | List], Tree0, Tree) :-
	( bintree__insert(Tree0, K, V, Tree1) ->
		Tree2 = Tree1
	;
		report_lookup_error("bintree__from_list: key already present",
			K, V)
	),
	bintree__from_list_2(List, Tree2, Tree).

%-----------------------------------------------------------------------------%

bintree__from_sorted_list(List, Tree) :-
	list__length(List, Length),
	bintree__from_sorted_list_2(Length, List, Tree, _).

:- pred bintree__from_sorted_list_2(int, assoc_list(K,V),
				bintree(K,V), assoc_list(K, V)).
:- mode bintree__from_sorted_list_2(in, in, out, out) is det.

bintree__from_sorted_list_2(Num, List0, Tree, List) :-
	( Num = 0 ->
		List = List0,
		Tree = empty
	;
		Num1 is Num - 1,
		SmallHalf is Num1 // 2,
		BigHalf is Num1 - SmallHalf,
		bintree__from_sorted_list_2(SmallHalf, List0, LeftSubTree,
				List1),
		( List1 = [HeadKey - HeadValue | List2] ->
			Tree = tree(HeadKey, HeadValue, LeftSubTree,
				RightSubTree),
			bintree__from_sorted_list_2(BigHalf, List2,
				RightSubTree, List)
		;
			error("bintree__from_sorted_list_2")
		)
	).

%-----------------------------------------------------------------------------%

bintree__balance(Tree0, Tree) :-
	bintree__to_list(Tree0, List),
	bintree__from_sorted_list(List, Tree).

%-----------------------------------------------------------------------------%

bintree__from_corresponding_lists(Keys, Values, Tree) :-
	( bintree__from_corresponding_lists_2(Keys, Values, empty, Tree0) ->
		Tree = Tree0
	;
		error("bintree__from_corresponding_lists: lists are of different lengths")
	).

:- pred bintree__from_corresponding_lists_2(list(K), list(V), bintree(K,V),
						bintree(K,V)).
:- mode bintree__from_corresponding_lists_2(in, in, in, out) is semidet.

bintree__from_corresponding_lists_2([], [], Tree, Tree).
bintree__from_corresponding_lists_2([K | Ks], [V | Vs], Tree0, Tree) :-
	( bintree__insert(Tree0, K, V, Tree1) ->
		Tree2 = Tree1
	;
		report_lookup_error("bintree__from_corresponding_lists: key already present", K, V)
	),
	bintree__from_corresponding_lists_2(Ks, Vs, Tree2, Tree).

%-----------------------------------------------------------------------------%

bintree__to_list(Tree, List) :-
	bintree__to_list_2(Tree, [], List).

:- pred bintree__to_list_2(bintree(K,V), assoc_list(K,V), assoc_list(K,V)).
:- mode bintree__to_list_2(in, in, out) is det.

bintree__to_list_2(empty, List, List).
bintree__to_list_2(tree(K, V, Left, Right), List0, List) :-
	bintree__to_list_2(Right, List0, List1),
	bintree__to_list_2(Left, [K - V | List1], List).

%-----------------------------------------------------------------------------%

bintree__keys(Tree, List) :-
	bintree__keys_2(Tree, [], List).

:- pred bintree__keys_2(bintree(K, _V), list(K), list(K)).
:- mode bintree__keys_2(in, in, out) is det.

bintree__keys_2(empty, List, List).
bintree__keys_2(tree(K, _V, Left, Right), List0, List) :-
	bintree__keys_2(Right, List0, List1),
	bintree__keys_2(Left, [K | List1], List).

%-----------------------------------------------------------------------------%

bintree__values(Tree, List) :-
	bintree__values_2(Tree, [], List).

:- pred bintree__values_2(bintree(_K, V), list(V), list(V)).
:- mode bintree__values_2(in, in, out) is det.

bintree__values_2(empty, List, List).
bintree__values_2(tree(_K, V, Left, Right), List0, List) :-
	bintree__values_2(Right, List0, List1),
	bintree__values_2(Left, [V | List1], List).

%-----------------------------------------------------------------------------%

bintree__count(empty, 0).
bintree__count(tree(_K, _V, Left, Right), Count) :-
	bintree__count(Right, RightCount),
	bintree__count(Left, LeftCount),
	ChildCount is LeftCount + RightCount,
	Count is ChildCount + 1.

bintree__depth(empty, 0).
bintree__depth(tree(_K, _V, Left, Right), Depth) :-
	bintree__depth(Right, RightDepth),
	bintree__depth(Left, LeftDepth),
	int__max(LeftDepth, RightDepth, SubDepth),
	Depth is SubDepth + 1.

bintree__branching_factor(empty, 0, 0).
bintree__branching_factor(tree(_K, _V, L, R), Ones, Twos) :-
	(
		L = empty
	->
		(
			R = empty
		->
			Ones = 0,
			Twos = 0
		;
			bintree__branching_factor(R, Ones0, Twos),
			Ones is Ones0 + 1
		)
	;
		(
			R = empty
		->
			bintree__branching_factor(L, Ones0, Twos),
			Ones is Ones0 + 1
		;
			bintree__branching_factor(L, Ones1, Twos1),
			bintree__branching_factor(R, Ones2, Twos2),
			Ones is Ones1 + Ones2,
			Twos0 is Twos1 + Twos2,
			Twos is Twos0 + 1
		)
	).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Function forms added.

bintree__set(BT1, K, V) = BT2 :-
	bintree__set(BT1, K, V, BT2).

bintree__lookup(BT, K) = V :-
	bintree__lookup(BT, K, V).

bintree__delete(BT1, K) = BT2 :-
	bintree__delete(BT1, K, BT2).

bintree__keys(BT) = Ks :-
	bintree__keys(BT, Ks).

bintree__values(BT) = Vs :-
	bintree__values(BT, Vs).

bintree__from_list(AL) = BT :-
	bintree__from_list(AL, BT).

bintree__from_sorted_list(AL) = BT :-
	bintree__from_sorted_list(AL, BT).

bintree__from_corresponding_lists(Ks, Vs) = BT :-
	bintree__from_corresponding_lists(Ks, Vs, BT).

bintree__to_list(BT) = AL :-
	bintree__to_list(BT, AL).

bintree__count(BT) = N :-
	bintree__count(BT, N).

bintree__depth(BT) = N :-
	bintree__depth(BT, N).

bintree__balance(BT1) = BT2 :-
	bintree__balance(BT1, BT2).

