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

:- pred bintree__init(bintree(K,V)).
:- mode bintree__init(output).

:- pred bintree__insert(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__insert(input, input, input, output).

:- pred bintree__update(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__update(input, input, input, output).

:- pred bintree__set(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__set(input, input, input, output).

:- pred bintree__search_insert(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__search_insert(input, input, input, output).

:- pred bintree__search(bintree(K,V), K, V).
:- mode bintree__search(input, input, output).

:- pred bintree__delete(bintree(K,V), K, bintree(K,V)).
:- mode bintree__delete(input, input, output).

:- pred bintree__keys(bintree(K,V), list(K)).
:- mode bintree__keys(input, output).

:- pred bintree__from_list(list(pair(K,V)), bintree(K,V)).
:- mode bintree__from_list(input, output).

:- pred bintree__from_corresponding_lists(list(K), list(V), bintree(K,V)).
:- mode bintree__from_corresponding_lists(input, input, output).

:- pred bintree__to_list(bintree(K,V), list(pair(K,V))).
:- mode bintree__to_list(input, output).

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
	(if
		Result = (=)
	then
		fail
	else if
		Result = (<)
	then
		bintree__insert(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	else
		bintree__insert(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__search_insert(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree__search_insert(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(if
		Result = (=)
	then
		(if Value = Value0 then
			Tree = tree(Key0, Value0, Left, Right)
		else
			fail
		)
	else if
		Result = (<)
	then
		bintree__search_insert(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	else
		bintree__search_insert(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__update(empty, _Key, _Value, _Tree) :-
	fail.
bintree__update(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(if
		Result = (=)
	then
		Tree = tree(Key0, Value, Left, Right)
	else if
		Result = (<)
	then
		bintree__update(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	else
		bintree__update(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__set(empty, Key, Value, tree(Key, Value, empty, empty)).
bintree__set(tree(Key0, Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(if
		Result = (=)
	then
		Tree = tree(Key0, Value, Left, Right)
	else if
		Result = (<)
	then
		bintree__set(Right, Key, Value, NewRight),
		Tree = tree(Key0, Value0, Left, NewRight)
	else
		bintree__set(Left, Key, Value, NewLeft),
		Tree = tree(Key0, Value0, NewLeft, Right)
	).

%-----------------------------------------------------------------------------%

bintree__search(tree(K0, V0, Left, Right), K, V) :-
	compare(Result, K0, K),
	(if
		Result = (=)
	then
		V0 = V
	else if
		Result = (<)
	then
		bintree__search(Right, K, V)
	else
		bintree__search(Left, K, V)
	).

%-----------------------------------------------------------------------------%

bintree__delete(empty, _K, empty).
bintree__delete(tree(K0, V0, Left, Right), K, Tree) :-
	compare(Result, K0, K),
	(if
		Result = (=)
	then
		bintree__fixup(Left, Right, Tree)
	else if
		Result = (<)
	then
		bintree__delete(Right, K, Tree1),
		Tree = tree(K0, V0, Left, Tree1)
	else
		bintree__delete(Left, K, Tree1),
		Tree = tree(K0, V0, Tree1, Right)
	).

:- pred bintree__fixup(bintree(K,V), bintree(K,V), bintree(K,V)).
:- mode bintree__fixup(input, input, output).

bintree__fixup(Left, Right, Tree) :-
	(if
		Left = empty
	then
		Tree = Right
	else if
		Right = empty
	then
		Tree = Right
	else
		bintree__right_depth(Left, LD),
		bintree__left_depth(Right, RD),
		(if
			LD > RD
		then
			bintree__knock_left(Left, K, V, Left1),
			Right1 = Right
		else
			bintree__knock_right(Right, K, V, Right1),
			Left1 = Left
		),
		Tree = tree(K, V, Left1, Right1)
	).

:- pred bintree__right_depth(bintree(K,V), int).
:- mode bintree__right_depth(input, output).

bintree__right_depth(empty, 0).
bintree__right_depth(tree(_K, _V, _Left, Right), N) :-
	bintree__right_depth(Right, M),
	N is M + 1.

:- pred bintree__left_depth(bintree(K,V), int).
:- mode bintree__left_depth(input, output).

bintree__left_depth(empty, 0).
bintree__left_depth(tree(_K, _V, Left, _Right), N) :-
	bintree__left_depth(Left, M),
	N is M + 1.

:- pred bintree__knock_left(bintree(K,V), K, V, bintree(K, V)).
:- mode bintree__knock_left(input, input, input, output).

bintree__knock_left(tree(K0, V0, Left, Right), K, V, Tree) :-
	(if
		Right = empty
	then
		K = K0,
		V = V0,
		Tree = Left
	else
		bintree__knock_left(Right, K, V, Right1),
		Tree = tree(K0, V0, Left, Right1)
	).

:- pred bintree__knock_right(bintree(K,V), K, V, bintree(K, V)).
:- mode bintree__knock_right(input, input, input, output).

bintree__knock_right(tree(K0, V0, Left, Right), K, V, Tree) :-
	(if
		Left = empty
	then
		K = K0,
		V = V0,
		Tree = Right
	else
		bintree__knock_right(Left, K, V, Left1),
		Tree = tree(K0, V0, Left1, Right)
	).

%-----------------------------------------------------------------------------%

bintree__from_list(List, Tree) :-
	bintree__from_list_2(List, empty, Tree).

:- pred bintree__from_list_2(list(pair(K,V)), bintree(K,V), bintree(K,V)).
:- mode bintree__from_list_2(input, input, output).

bintree__from_list_2([], Tree, Tree).
bintree__from_list_2([K - V | List], Tree0, Tree) :-
	bintree__insert(Tree0, K, V, Tree1),
	bintree__from_list_2(List, Tree1, Tree).

%-----------------------------------------------------------------------------%

bintree__from_corresponding_lists(Keys, Values, Tree) :-
	bintree__from_corresponding_lists_2(Keys, Values, empty, Tree).

:- pred bintree__from_corresponding_lists_2(list(K), list(V), bintree(K,V),
						bintree(K,V)).
:- mode bintree__from_corresponding_lists_2(input, input, input, output).

bintree__from_corresponding_lists_2([], [], Tree, Tree).
bintree__from_corresponding_lists_2([K | Ks], [V | Vs], Tree0, Tree) :-
	bintree__insert(Tree0, K, V, Tree1),
	bintree__from_corresponding_lists_2(Ks, Vs, Tree1, Tree).

%-----------------------------------------------------------------------------%

bintree__to_list(Tree, List) :-
	bintree__to_list_2(Tree, [], List).

:- pred bintree__to_list_2(bintree(K,V), list(pair(K,V)), list(pair(K,V))).
:- mode bintree__to_list_2(input, input, output).

bintree__to_list_2(empty, List, List).
bintree__to_list_2(tree(K, V, Left, Right), List0, List) :-
	bintree__to_list_2(Right, List0, List1),
	bintree__to_list_2(Left, [K - V | List1], List).

%-----------------------------------------------------------------------------%

bintree__keys(Tree, List) :-
	bintree__keys_2(Tree, [], List).

:- pred bintree__keys_2(bintree(K, _V), list(K), list(K)).
:- mode bintree__keys_2(input, input, output).

bintree__keys_2(empty, List, List).
bintree__keys_2(tree(K, _V, Left, Right), List0, List) :-
	bintree__keys_2(Right, List0, List1),
	bintree__keys_2(Left, [K | List1], List).

%-----------------------------------------------------------------------------%
