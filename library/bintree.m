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
	them
		bintree__search(Right, K, V)
	else
		bintree__search(Left, K, V)
	).


