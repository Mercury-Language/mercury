%-----------------------------------------------------------------------------%
%
% Main author: conway.
%
% This file provides a straight-forward binary search tree implementation of
% a map (dictionary).
%
% bintree__insert, bintree__search_insert, bintree__update, and
% bintree__set differ only in how they handle the case where the value
% being inserted already exists in the tree.  `insert' will only insert
% new keys, and will fail if you attempt to insert an existing key into
% the tree.  `search_insert' will fail if you attempt to insert an
% existing key into the key unless the data you are inserting is exactly
% the same as (i.e. unifies with) the existing data associated with that
% key.  `update' will only allow you to modify the data for existing
% keys, and will fail if the key isn't already in the tree.  `set' will
% always succeed; it will replace the old value for that key if the key
% was already in the tree, or insert a new node into the tree if the key
% wasn't already present.
% 
%-----------------------------------------------------------------------------%

:- module bintree.
:- interface.
:- import_module list, std_util.

:- type bintree(_K, _V).

:- pred bintree__init(bintree(_K,_V)).
:- mode bintree__init(out).

:- pred bintree__insert(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__insert(in, in, in, out).

:- pred bintree__update(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__update(in, in, in, out).

:- pred bintree__set(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__set(in, in, in, out).

:- pred bintree__search_insert(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__search_insert(in, in, in, out).

:- pred bintree__search(bintree(K,V), K, V).
:- mode bintree__search(in, in, out).

:- pred bintree__delete(bintree(K,V), K, bintree(K,V)).
:- mode bintree__delete(in, in, out).

:- pred bintree__keys(bintree(K,_V), list(K)).
:- mode bintree__keys(in, out).

:- pred bintree__from_list(list(pair(K,V)), bintree(K,V)).
:- mode bintree__from_list(in, out).

:- pred bintree__from_corresponding_lists(list(K), list(V), bintree(K,V)).
:- mode bintree__from_corresponding_lists(in, in, out).

:- pred bintree__to_list(bintree(K,V), list(pair(K,V))).
:- mode bintree__to_list(in, out).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int.

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

bintree__search_insert(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree__search_insert(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(
		Result = (=)
	->
		(
			Value = Value0
		->
			Tree = tree(Key0, Value0, Left, Right)
		;
			fail
		)
	;
		Result = (<)
	->
		bintree__search_insert(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	;
		bintree__search_insert(Left, Key, Value, NewLeft),
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
		V0 = V
	;
		Result = (<)
	->
		bintree__search(Right, K, V)
	;
		bintree__search(Left, K, V)
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

:- pred bintree__fixup(bintree(K,V), bintree(K,V), bintree(K,V)).
:- mode bintree__fixup(in, in, out).

bintree__fixup(Left, Right, Tree) :-
	(
		Left = empty
	->
		Tree = Right
	;
		Right = empty
	->
		Tree = Right
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
:- mode bintree__right_depth(in, out).

bintree__right_depth(empty, 0).
bintree__right_depth(tree(_K, _V, _Left, Right), N) :-
	bintree__right_depth(Right, M),
	N is M + 1.

:- pred bintree__left_depth(bintree(_K,_V), int).
:- mode bintree__left_depth(in, out).

bintree__left_depth(empty, 0).
bintree__left_depth(tree(_K, _V, Left, _Right), N) :-
	bintree__left_depth(Left, M),
	N is M + 1.

:- pred bintree__knock_left(bintree(K,V), K, V, bintree(K, V)).
:- mode bintree__knock_left(in, in, in, out).

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
:- mode bintree__knock_right(in, in, in, out).

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

:- pred bintree__from_list_2(list(pair(K,V)), bintree(K,V), bintree(K,V)).
:- mode bintree__from_list_2(in, in, out).

bintree__from_list_2([], Tree, Tree).
bintree__from_list_2([K - V | List], Tree0, Tree) :-
	bintree__insert(Tree0, K, V, Tree1),
	bintree__from_list_2(List, Tree1, Tree).

%-----------------------------------------------------------------------------%

bintree__from_corresponding_lists(Keys, Values, Tree) :-
	bintree__from_corresponding_lists_2(Keys, Values, empty, Tree).

:- pred bintree__from_corresponding_lists_2(list(K), list(V), bintree(K,V),
						bintree(K,V)).
:- mode bintree__from_corresponding_lists_2(in, in, in, out).

bintree__from_corresponding_lists_2([], [], Tree, Tree).
bintree__from_corresponding_lists_2([K | Ks], [V | Vs], Tree0, Tree) :-
	bintree__insert(Tree0, K, V, Tree1),
	bintree__from_corresponding_lists_2(Ks, Vs, Tree1, Tree).

%-----------------------------------------------------------------------------%

bintree__to_list(Tree, List) :-
	bintree__to_list_2(Tree, [], List).

:- pred bintree__to_list_2(bintree(K,V), list(pair(K,V)), list(pair(K,V))).
:- mode bintree__to_list_2(in, in, out).

bintree__to_list_2(empty, List, List).
bintree__to_list_2(tree(K, V, Left, Right), List0, List) :-
	bintree__to_list_2(Right, List0, List1),
	bintree__to_list_2(Left, [K - V | List1], List).

%-----------------------------------------------------------------------------%

bintree__keys(Tree, List) :-
	bintree__keys_2(Tree, [], List).

:- pred bintree__keys_2(bintree(K, _V), list(K), list(K)).
:- mode bintree__keys_2(in, in, out).

bintree__keys_2(empty, List, List).
bintree__keys_2(tree(K, _V, Left, Right), List0, List) :-
	bintree__keys_2(Right, List0, List1),
	bintree__keys_2(Left, [K | List1], List).

%-----------------------------------------------------------------------------%
