%-----------------------------------------------------------------------------%
% This file provides a straight-forward binary search tree implementation of
% an associative map.
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
		bintree_delete(Right, K, Tree1),
		Tree = tree(K0 - V0, Left, Tree1)
	else
		bintree_delete(Left, K, Tree1),
		Tree = tree(K0 - V0, Tree1, Right)
	).

bintree__fixup(empty, Right, Right).
bintree__fixup(tree(Node, Left, Right0), Right, Tree) :-
	bintree__fixup(Right0, Right, Right1),
	Tree = tree(Node, Left, Right1).

:- pred bintree__from_list(list(pair(K,V)), bintree(K,V)).
:- mode bintree__from_list(input, output).

bintree__from_list(List, Tree) :-
	bintree__from_list_2(List, empty, Tree).

bintree__from_list_2([], Tree, Tree).
bintree__from_list_2([K - V | List], Tree0, Tree) :-
	bintree__insert(Tree0, K, V, Tree1),
	bintree__from_list_2(List, Tree1, Tree).

:- pred bintree__to_list(bintree(K,V), list(pair(K,V))).
:- mode bintree__to_list(input, output).

bintree__to_list(Tree, List) :-
	bintree__to_list_2(Tree, [], List).

bintree__to_list_2(empty, List, List).
bintree__to_list_2(tree(Node, Left, Right), List0, List) :-
	bintree__to_list_2(Right, List0, List1),
	bintree__to_list_2(Left, [Node | List1], List).

