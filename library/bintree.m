%-----------------------------------------------------------------------------%
%
% Main author: conway.
%
% This file provides a straight-forward binary search tree implementation of
% an associative map.
%
%-----------------------------------------------------------------------------%

:- module bintree.

:- interface.
:- type bintree(K,V)		--->	empty
				;	tree(
						pair(K,V),
						bintree(K,V),
						bintree(K,V)
					).

:- implementation.

:- pred bintree__init(bintree(K,V)).
:- mode bintree__init(output).

bintree__init(empty).

:- pred bintree__insert(bintree(K,V), K, V, bintree(K,V)).
:- mode bintree__insert(input, input, input, output).

bintree__insert(empty, Key, Value, tree(Key - Value, empty, empty)).
bintree__insert(tree(Key0 - Value0, Left, Right), Key, Value, Tree) :-
	compare(Result, Key0, Key),
	(if
		Result = (=)
	then
		Tree = tree(Key0 - Value, Left, Right)
	else if
		Result = (<)
	then
		bintree__insert(Right, Key, Value, NewRight),
		Tree = tree(Key0 - Value0, Left, NewRight)
	else
		bintree__insert(Left, Key, Value, NewLeft),
		Tree = tree(Key0 - Value0, NewLeft, Right)
	).

:- pred bintree__search(bintree(K,V), K, V).
:- mode bintree__search(input, input, output).

bintree__search(tree(K0 - V0, Left, Right), K, V) :-
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

:- pred bintree__delete(bintree(K,V), K, bintree(K,V)).
:- mode bintree__delete(input, input, output).

bintree__delete(empty, K, empty).
bintree__delete(tree(K0 - V0, Left, Right), K, Tree) :-
	compare(Result, K0, K),
	(if
		Result = (=)
	then
		bintree__fixup(Left, Right, Tree)
	else if
		Result = (<)
	then
		bintree__delete(Right, K, Tree1),
		Tree = tree(K0 - V0, Left, Tree1)
	else
		bintree__delete(Left, K, Tree1),
		Tree = tree(K0 - V0, Tree1, Right)
	).

:- pred bintree__fixup(bintree(K,V), bintree(K,V), bintree(K,V)).
:- mode bintree__fixup(input, input, output).

bintree__fixup(empty, Right, Right).
bintree__fixup(Left, empty, Left).
bintree__fixup(Left, Right, Tree) :-
	bintree__right_depth(Left, LD),
	bintree__left_depth(Right, RD),
	(if
		LD > RD
	then
		bintree__knock_left(Left, Node, Left1),
		Right1 = Right
	else
		bintree__knock_right(Right, Node, Right1),
		Left1 = Left
	),
	Tree = tree(Node, Left1, Right1).

:- pred bintree__right_depth(bintree(K,V), int).
:- mode bintree__right_depth(input, output).

bintree__right_depth(empty, 0).
bintree__right_depth(tree(Node, Left, Right), N) :-
	bintree__right_depth(Right, M),
	N is M + 1.

:- pred bintree__left_depth(bintree(K,V), int).
:- mode bintree__left_depth(input, output).

bintree__left_depth(empty, 0).
bintree__left_depth(tree(Node, Left, Right), N) :-
	bintree__left_depth(Left, M),
	N is M + 1.

:- pred bintree__knock_left(bintree(K,V), pair(K, V), bintree(K, V)).
:- mode bintree__knock_left(input, input, output).

bintree__knock_left(tree(Node0, Left, Right), Node, Tree) :-
	(if
		Right = empty
	then
		Node = Node0,
		Tree = Left
	else
		bintree__knock_left(Right, Node, Right1),
		Tree = tree(Node0, Left, Right1)
	).

:- pred bintree__knock_right(bintree(K,V), pair(K, V), bintree(K, V)).
:- mode bintree__knock_right(input, input, output).

bintree__knock_right(tree(Node0, Left, Right), Node, Tree) :-
	(if
		Left = empty
	then
		Node = Node0,
		Tree = Right
	else
		bintree__knock_right(Left, Node, Left1),
		Tree = tree(Node0, Left1, Right)
	).

:- pred bintree__from_list(list(pair(K,V)), bintree(K,V)).
:- mode bintree__from_list(input, output).

bintree__from_list(List, Tree) :-
	bintree__from_list_2(List, empty, Tree).

:- pred bintree__from_list_2(list(pair(K,V)), bintree(K,V), bintree(K,V)).
:- mode bintree__from_list_2(input, input, output).

bintree__from_list_2([], Tree, Tree).
bintree__from_list_2([K - V | List], Tree0, Tree) :-
	bintree__insert(Tree0, K, V, Tree1),
	bintree__from_list_2(List, Tree1, Tree).

:- pred bintree__to_list(bintree(K,V), list(pair(K,V))).
:- mode bintree__to_list(input, output).

bintree__to_list(Tree, List) :-
	bintree__to_list_2(Tree, [], List).

:- pred bintree__to_list_2(bintree(K,V), list(pair(K,V)), list(pair(K,V))).
:- mode bintree__to_list_2(input, input, output).

bintree__to_list_2(empty, List, List).
bintree__to_list_2(tree(Node, Left, Right), List0, List) :-
	bintree__to_list_2(Right, List0, List1),
	bintree__to_list_2(Left, [Node | List1], List).

%-----------------------------------------------------------------------------%
