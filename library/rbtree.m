%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2000, 2003 The University of Melbourne.
% This file may only be copied under the terms of the GNU Library General
% Public License - see the file COPYING.LIB in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
%  Red-black tree module.
%  Main author: petdr.
%  Stability: medium.
%
%  Contains an implementation of red black trees.
%
% *** Exit conditions of main predicates ***
% insert:
%	fails if key already in tree.
% update:
%	changes value of key already in tree.  fails if key doesn't exist.
% set:
%	inserts or updates. Never fails.
%
% insert_duplicate:
%	inserts duplicate keys into the tree, never fails.  Search doesn't
%	yet support looking for duplicates.
%
% delete:
%	deletes a node from the tree if it exists.
% remove:
%	fails if node to remove doesn't exist in the tree.
%
% lookup:
%	Aborts program if key looked up doesn't exist.
% search:
%	Fails if key looked up doesn't exist.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module rbtree.
:- interface.

:- import_module list, assoc_list.

:- type rbtree(Key, Value).

	% Initialise the data structure.
:- pred rbtree__init(rbtree(K, V)).
:- mode rbtree__init(uo) is det.

:- func rbtree__init = rbtree(K, V).

	% Check whether a tree is empty.
:- pred rbtree__is_empty(rbtree(K, V)).
:- mode rbtree__is_empty(in) is semidet.

	% Inserts a new key-value pair into the tree.  Fails if key 
	% already in the tree.
:- pred rbtree__insert(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__insert(in, in, in, out) is semidet.

	% Updates the value associated with a key.  Fails if the key 
	% doesn't exist.
:- pred rbtree__update(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__update(in, in, in, out) is semidet.

	% Sets a value irregardless of whether key exists or not.  Never
	% fails.
:- pred rbtree__set(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__set(di, di, di, uo) is det.
:- mode rbtree__set(in, in, in, out) is det.

:- func rbtree__set(rbtree(K, V), K, V) = rbtree(K, V).

	% Insert a duplicate key into the tree.  Never fails.
:- pred rbtree__insert_duplicate(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__insert_duplicate(in, in, in, out) is det.

:- func rbtree__insert_duplicate(rbtree(K, V), K, V) = rbtree(K, V).

:- pred rbtree__member(rbtree(K, V), K, V).
:- mode rbtree__member(in, out, out) is nondet.

	% Search for a key-value pair using the key.  Fails if key doesn't
	% exist.
:- pred rbtree__search(rbtree(K, V), K, V).
:- mode rbtree__search(in, in, out) is semidet.

	% Lookup a value associated with a key.  Program aborts if key
	% doesn't exist.
:- pred rbtree__lookup(rbtree(K, V), K, V).
:- mode rbtree__lookup(in, in, out) is det.

:- func rbtree__lookup(rbtree(K, V), K) = V.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next lower key instead.
	% Fails if there is no key with the given or lower value.
:- pred rbtree__lower_bound_search(rbtree(K, V), K, K, V).
:- mode rbtree__lower_bound_search(in, in, out, out) is semidet.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next lower key instead.
	% Aborts if there is no key with the given or lower value.
:- pred rbtree__lower_bound_lookup(rbtree(K, V), K, K, V).
:- mode rbtree__lower_bound_lookup(in, in, out, out) is det.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next higher key instead.
	% Fails if there is no key with the given or higher value.
:- pred rbtree__upper_bound_search(rbtree(K, V), K, K, V).
:- mode rbtree__upper_bound_search(in, in, out, out) is semidet.

	% Search for a key-value pair using the key.  If there is no entry
	% for the given key, returns the pair for the next higher key instead.
	% Aborts if there is no key with the given or higher value.
:- pred rbtree__upper_bound_lookup(rbtree(K, V), K, K, V).
:- mode rbtree__upper_bound_lookup(in, in, out, out) is det.

	% Delete the key value pair associated with a key.  Does nothing
	% if the key doesn't exist.
:- pred rbtree__delete(rbtree(K, V), K, rbtree(K, V)).
:- mode rbtree__delete(di, in, uo) is det.
:- mode rbtree__delete(in, in, out) is det.

:- func rbtree__delete(rbtree(K, V), K) = rbtree(K, V).

	% Remove the key value pair associated with a key.  Fails
	% if the key doesn't exist.
:- pred rbtree__remove(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__remove(di, in, uo, uo) is semidet.
:- mode rbtree__remove(in, in, out, out) is semidet.

	% Same as above, except this version does not return the value
	% corresponding to the key.  Its use is deprecated, but it is
	% kept for compatibility with older versions of this library.
:- pred rbtree__remove(rbtree(K, V), K, rbtree(K, V)).
:- mode rbtree__remove(in, in, out) is semidet.

	% Deletes the node with the minimum K from the tree, and returns
	% the key and value fields.
:- pred rbtree__remove_smallest(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__remove_smallest(di, uo, uo, uo) is semidet.
:- mode rbtree__remove_smallest(in, out, out, out) is semidet.

	% Deletes the node with the maximum K from the tree, and returns
	% the key and value fields.
:- pred rbtree__remove_largest(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__remove_largest(di, uo, uo, uo) is semidet.
:- mode rbtree__remove_largest(in, out, out, out) is semidet.

	% Returns an in-order list of all the keys in the rbtree.
:- pred rbtree__keys(rbtree(K, V), list(K)).
:- mode rbtree__keys(in, out) is det.

:- func rbtree__keys(rbtree(K, V)) = list(K).

	% Returns a list of values such that the keys associated with the
	% values are in-order.
:- pred rbtree__values(rbtree(K, V), list(V)).
:- mode rbtree__values(in, out) is det.

:- func rbtree__values(rbtree(K, V)) = list(V).

	% Count the number of elements in the tree
:- pred rbtree__count(rbtree(K, V), int).
:- mode rbtree__count(in, out) is det.

:- func rbtree__count(rbtree(K, V)) = int.

:- pred rbtree__assoc_list_to_rbtree(assoc_list(K, V), rbtree(K, V)).
:- mode rbtree__assoc_list_to_rbtree(in, out) is det.

:- func rbtree__assoc_list_to_rbtree(assoc_list(K, V)) = rbtree(K, V).

:- pred rbtree__rbtree_to_assoc_list(rbtree(K, V), assoc_list(K, V)).
:- mode rbtree__rbtree_to_assoc_list(in, out) is det.

:- func rbtree__rbtree_to_assoc_list(rbtree(K, V)) = assoc_list(K, V).

:- pred rbtree__foldl(pred(K, V, T, T), rbtree(K, V), T, T).
:- mode rbtree__foldl(pred(in, in, in, out) is det, in, in, out) is det.
:- mode rbtree__foldl(pred(in, in, in, out) is semidet, in, in, out)
		is semidet.
:- mode rbtree__foldl(pred(in, in, di, uo) is det, in, di, uo) is det.

:- func rbtree__foldl(func(K, V, T) = T, rbtree(K, V), T) = T.

:- pred rbtree__map_values(pred(K, V, W), rbtree(K, V), rbtree(K, W)).
:- mode rbtree__map_values(pred(in, in, out) is det, in, out) is det.
:- mode rbtree__map_values(pred(in, in, out) is semidet, in, out) is semidet.

:- func rbtree__map_values(func(K, V) = W, rbtree(K, V)) = rbtree(K, W).

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module bool, int, require, std_util.


:- type rbtree(K,V)	 --->	empty
			;	red(K, V, rbtree(K,V), rbtree(K,V))
			;	black(K, V, rbtree(K, V), rbtree(K, V)).

%-----------------------------------------------------------------------------%

rbtree__init(empty).

rbtree__is_empty(Tree) :-
	Tree = empty.

%-----------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees.
%	* The root node must be black.
%	* There can never be 2 red nodes in a row.

rbtree__insert(empty, K, V, black(K, V, empty, empty)).
rbtree__insert(red(_, _, _, _), _K, _V, _Tree) :-
	error("rbtree__insert: root node cannot be red!").
rbtree__insert(black(K0, V0, L0, R0), K, V, Tree) :-
	rbtree__insert_2(black(K0, V0, L0, R0), K, V, Tree0),
	% Ensure that the root of the tree is black.
	(
		Tree0 = red(K1, V1, L1, R1)
	->
		Tree = black(K1, V1, L1, R1)
	;
		Tree = Tree0
	).


:- pred rbtree__insert_2(rbtree(K, V), K, V, rbtree(K, V)). 
:- mode rbtree__insert_2(in, in, in, out) is semidet.

% rbtree__insert_2:
%	We traverse down the tree until we find the correct spot to insert.
%	Then as we fall back out of the recursions we look for possible
%	rotation cases.

% Red node always inserted at the bottom as it will be rotated into the 
% correct place as we move back up the tree.
rbtree__insert_2(empty, K, V, red(K, V, empty, empty)).
rbtree__insert_2(red(K0, V0, L0, R0), K, V, Tree) :-
	% Work out which side of the rbtree to insert.
	compare(Result, K, K0),
	(
		Result = (<)
	->
		rbtree__insert_2(L0, K, V, NewL),
		Tree = red(K0, V0, NewL, R0)
	;
		Result = (>)
	->
		rbtree__insert_2(R0, K, V, NewR),
		Tree = red(K0, V0, L0, NewR)
	;
		fail
	).
% Only ever need to look for a possible rotation if we are in a black node.
% The rotation criteria is when there is 2 red nodes in a row.
rbtree__insert_2(black(K0, V0, L0, R0), K, V, Tree) :-
	(
		L0 = red(LK, LV, LL, LR),
		R0 = red(RK, RV, RL, RR)
	->
		% On the way down the rbtree we split any 4-nodes we find.
		% This converts the current node to a red node, so we call
		% the red node version of rbtree__insert_2/4.
		L = black(LK, LV, LL, LR),
		R = black(RK, RV, RL, RR),
		Tree0 = red(K0, V0, L, R),
		rbtree__insert_2(Tree0, K, V, Tree)
	;
		% Work out which side of the rbtree to insert.
		compare(Result, K, K0),
		( 	
			Result = (<)
		->
			rbtree__insert_2(L0, K, V, NewL),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewL = red(LK, LV, LL, LR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					LL = red(_LLK, _LLV, _LLL, _LLR) 
				->
					TreeR = red(K0, V0, LR, R0),
					Tree = black(LK, LV, LL, TreeR)
				; 
					LR = red(LRK, LRV, LRL, LRR) 
				->
					TreeL = red(LK, LV, LL, LRL),
					TreeR = red(K0, V0, LRR, R0),
					Tree = black(LRK, LRV, TreeL, TreeR)
				;
					Tree = black(K0, V0, NewL, R0)
				)
			;
				Tree = black(K0, V0, NewL, R0)
			)
		;
			Result = (>)
		->
			rbtree__insert_2(R0, K, V, NewR),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewR = red(RK, RV, RL, RR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					RL = red(RLK, RLV, RLL, RLR) 
				->
					TreeL = red(K0, V0, L0, RLL),
					TreeR = red(RK, RV, RLR, RR),
					Tree = black(RLK, RLV, TreeL, TreeR)
				; 
					RR = red(_RRK, _RRV, _RRL, _RRR) 
				->
					TreeL = red(K0, V0, L0, RL),
					Tree = black(RK, RV, TreeL, RR)
				;
					Tree = black(K0, V0, L0, NewR)
				)
			;
				Tree = black(K0, V0, L0, NewR)
			)
		;
			fail
		)
	).

%-----------------------------------------------------------------------------%

rbtree__update(empty, _K, _V, _T) :-
	fail.
rbtree__update(red(K0, V0, L, R), K, V, Tree) :-
	compare(Result, K, K0),
	(
		Result = (=)
	->
		Tree = red(K, V, L, R)
	;
		Result = (<)
	->
		rbtree__update(L, K, V, NewL),
		Tree = red(K0, V0, NewL, R)
	;
		rbtree__update(R, K, V, NewR),
		Tree = red(K0, V0, L, NewR)
	).
rbtree__update(black(K0, V0, L, R), K, V, Tree) :-
	compare(Result, K, K0),
	(
		Result = (=)
	->
		Tree = black(K, V, L, R)
	;
		Result = (<)
	->
		rbtree__update(L, K, V, NewL),
		Tree = black(K0, V0, NewL, R)
	;
		rbtree__update(R, K, V, NewR),
		Tree = black(K0, V0, L, NewR)
	).

%-----------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees.
%	* The root node must be black.
%	* There can never be 2 red nodes in a row.

rbtree__set(empty, K, V, black(K, V, empty, empty)).
rbtree__set(red(_, _, _, _), _K, _V, _Tree) :-
	error("rbtree__set: root node cannot be red!").
rbtree__set(black(K0, V0, L0, R0), K, V, Tree) :-
	rbtree__set_2(black(K0, V0, L0, R0), K, V, Tree0),
	% Ensure that the root of the tree is black.
	(
		Tree0 = red(K1, V1, L1, R1)
	->
		Tree = black(K1, V1, L1, R1)
	;
		Tree = Tree0
	).


:- pred rbtree__set_2(rbtree(K, V), K, V, rbtree(K, V)). 
:- mode rbtree__set_2(di, di, di, uo) is det.
:- mode rbtree__set_2(in, in, in, out) is det.

% rbtree__set_2:
%	We traverse down the tree until we find the correct spot to insert, or
%	update.  Then as we fall back out of the recursions we look for possible
%	rotation cases.

% Red node always inserted at the bottom as it will be rotated into the 
% correct place as we move back up the tree.
rbtree__set_2(empty, K, V, red(K, V, empty, empty)).
rbtree__set_2(red(K0, V0, L0, R0), K, V, Tree) :-
	% Work out which side of the rbtree to insert.
	compare(Result, K, K0),
	(
		Result = (=)
	->
		Tree = red(K, V, L0, R0)
	;
		Result = (<)
	->
		rbtree__set_2(L0, K, V, NewL),
		Tree = red(K0, V0, NewL, R0)
	;
		rbtree__set_2(R0, K, V, NewR),
		Tree = red(K0, V0, L0, NewR)
	).
rbtree__set_2(black(K0, V0, L0, R0), K, V, Tree) :-
	(
		L0 = red(LK, LV, LL, LR),
		R0 = red(RK, RV, RL, RR)
	->
		% On the way down the rbtree we split any 4-nodes we find.
		L = black(LK, LV, LL, LR),
		R = black(RK, RV, RL, RR),
		Tree0 = red(K0, V0, L, R),
		rbtree__set_2(Tree0, K, V, Tree)
	;
		% Work out which side of the rbtree to insert.
		compare(Result, K, K0),
		( 	
			Result = (=)
		->
			Tree = black(K, V, L0, R0)
		;
			Result = (<)
		->
			rbtree__set_2(L0, K, V, NewL),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewL = red(LK, LV, LL, LR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					LL = red(_LLK, _LLV, _LLL, _LLR) 
				->
					TreeR = red(K0, V0, LR, R0),
					Tree = black(LK, LV, LL, TreeR)
				; 
					LR = red(LRK, LRV, LRL, LRR) 
				->
					TreeL = red(LK, LV, LL, LRL),
					TreeR = red(K0, V0, LRR, R0),
					Tree = black(LRK, LRV, TreeL, TreeR)
				;
					% NewL2 == NewL, but this hack 
					% needed for unique modes to work.
					NewL2 = red(LK, LV, LL, LR),
					Tree = black(K0, V0, NewL2, R0)
				)
			;
				Tree = black(K0, V0, NewL, R0)
			)
		;
			rbtree__set_2(R0, K, V, NewR),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewR = red(RK, RV, RL, RR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					RL = red(RLK, RLV, RLL, RLR) 
				->
					TreeL = red(K0, V0, L0, RLL),
					TreeR = red(RK, RV, RLR, RR),
					Tree = black(RLK, RLV, TreeL, TreeR)
				; 
					RR = red(_RRK, _RRV, _RRL, _RRR) 
				->
					TreeL = red(K0, V0, L0, RL),
					Tree = black(RK, RV, TreeL, RR)
				;
					% NewR2 == NewR, but this hack 
					% needed for unique modes to work.
					NewR2 = red(RK, RV, RL, RR),
					Tree = black(K0, V0, L0, NewR2)
				)
			;
				Tree = black(K0, V0, L0, NewR)
			)
		)
	).

%-----------------------------------------------------------------------------%

% Special conditions that must be satisfied by Red-Black trees.
%	* The root node must be black.
%	* There can never be 2 red nodes in a row.

rbtree__insert_duplicate(empty, K, V, black(K, V, empty, empty)).
rbtree__insert_duplicate(red(_, _, _, _), _K, _V, _Tree) :-
	error("rbtree__insert_duplicate: root node cannot be red!").
rbtree__insert_duplicate(black(K0, V0, L0, R0), K, V, Tree) :-
	rbtree__insert_duplicate_2(black(K0, V0, L0, R0), K, V, Tree0),
	% Ensure that the root of the tree is black.
	(
		Tree0 = red(K1, V1, L1, R1)
	->
		Tree = black(K1, V1, L1, R1)
	;
		Tree = Tree0
	).


:- pred rbtree__insert_duplicate_2(rbtree(K, V), K, V, rbtree(K, V)). 
:- mode rbtree__insert_duplicate_2(in, in, in, out) is det.

% rbtree__insert_duplicate_2:
%	We traverse down the tree until we find the correct spot to insert.
%	Then as we fall back out of the recursions we look for possible
%	rotation cases.

% Red node always inserted at the bottom as it will be rotated into the 
% correct place as we move back up the tree.
rbtree__insert_duplicate_2(empty, K, V, red(K, V, empty, empty)).
rbtree__insert_duplicate_2(red(K0, V0, L0, R0), K, V, Tree) :-
	% Work out which side of the rbtree to insert.
	compare(Result, K, K0),
	(
		Result = (<),
		rbtree__insert_duplicate_2(L0, K, V, NewL),
		Tree = red(K0, V0, NewL, R0)
	;
		Result = (>),
		rbtree__insert_duplicate_2(R0, K, V, NewR),
		Tree = red(K0, V0, L0, NewR)
	;
		Result = (=),
		rbtree__insert_duplicate_2(L0, K, V, NewL),
		Tree = red(K0, V0, NewL, R0)
	).
% Only ever need to look for a possible rotation if we are in a black node.
% The rotation criteria is when there is 2 red nodes in a row.
rbtree__insert_duplicate_2(black(K0, V0, L0, R0), K, V, Tree) :-
	(
		L0 = red(LK, LV, LL, LR),
		R0 = red(RK, RV, RL, RR)
	->
		% On the way down the rbtree we split any 4-nodes we find.
		% This converts the current node to a red node, so we call
		% the red node version of rbtree__insert_duplicate_2/4.
		L = black(LK, LV, LL, LR),
		R = black(RK, RV, RL, RR),
		Tree0 = red(K0, V0, L, R),
		rbtree__insert_duplicate_2(Tree0, K, V, Tree)
	;
		% Work out which side of the rbtree to insert.
		compare(Result, K, K0),
		( 	
			Result = (<),
			rbtree__insert_duplicate_2(L0, K, V, NewL),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewL = red(LK, LV, LL, LR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					LL = red(_LLK1, _LLV1, _LLL1, _LLR1) 
				->
					TreeR = red(K0, V0, LR, R0),
					Tree = black(LK, LV, LL, TreeR)
				; 
					LR = red(LRK, LRV, LRL, LRR) 
				->
					TreeL = red(LK, LV, LL, LRL),
					TreeR = red(K0, V0, LRR, R0),
					Tree = black(LRK, LRV, TreeL, TreeR)
				;
					Tree = black(K0, V0, NewL, R0)
				)
			;
				Tree = black(K0, V0, NewL, R0)
			)
		;
			Result = (>),
			rbtree__insert_duplicate_2(R0, K, V, NewR),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewR = red(RK, RV, RL, RR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					RL = red(RLK, RLV, RLL, RLR) 
				->
					TreeL = red(K0, V0, L0, RLL),
					TreeR = red(RK, RV, RLR, RR),
					Tree = black(RLK, RLV, TreeL, TreeR)
				; 
					RR = red(_RRK, _RRV, _RRL, _RRR) 
				->
					TreeL = red(K0, V0, L0, RL),
					Tree = black(RK, RV, TreeL, RR)
				;
					Tree = black(K0, V0, L0, NewR)
				)
			;
				Tree = black(K0, V0, L0, NewR)
			)
		;
			Result = (=),
			rbtree__insert_duplicate_2(L0, K, V, NewL),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and its
				% new child red. 
				NewL = red(LK, LV, LL, LR)
			->
				% Check to see if a grandchild is red and if so
				% rotate.
				( 
					LL = red(_LLK2, _LLV2, _LLL2, _LLR2) 
				->
					TreeR = red(K0, V0, LR, R0),
					Tree = black(LK, LV, LL, TreeR)
				; 
					LR = red(LRK, LRV, LRL, LRR) 
				->
					TreeL = red(LK, LV, LL, LRL),
					TreeR = red(K0, V0, LRR, R0),
					Tree = black(LRK, LRV, TreeL, TreeR)
				;
					Tree = black(K0, V0, NewL, R0)
				)
			;
				Tree = black(K0, V0, NewL, R0)
			)
		)
	).

%-----------------------------------------------------------------------------%

rbtree__member(empty, _K, _V) :- fail.
rbtree__member(red(K0, V0, Left, Right), K, V) :-
	(
		K = K0,
		V = V0
	;
		rbtree__member(Left, K, V)
	;
		rbtree__member(Right, K, V)
	).
rbtree__member(black(K0, V0, Left, Right), K, V) :-
	(
		K = K0,
		V = V0
	;
		rbtree__member(Left, K, V)
	;
		rbtree__member(Right, K, V)
	).

%-----------------------------------------------------------------------------%

rbtree__search(Tree, K, V) :-
	( Tree = red(K0, V0, Left, Right)
	; Tree = black(K0, V0, Left, Right)
	),
	compare(Result, K, K0),
	(
		Result = (=)
	->
		V = V0
	;
		Result = (<)
	->
		rbtree__search(Left, K, V)
	;
		rbtree__search(Right, K, V)
	).

rbtree__lookup(T, K, V) :-
	( rbtree__search(T, K, V0) ->
		V = V0
	;
		report_lookup_error("rbtree__lookup: key not found", K, V)
	).

%-----------------------------------------------------------------------------%

rbtree__lower_bound_search(Tree, SearchK, K, V) :-
	( Tree = red(K0, V0, Left, Right)
	; Tree = black(K0, V0, Left, Right)
	),
	compare(Result, SearchK, K0),
	(
		Result = (=)
	->
		K = K0,
		V = V0
	;
		Result = (<)
	->
		rbtree__lower_bound_search(Left, SearchK, K, V)
	;
		( rbtree__lower_bound_search(Right, SearchK, Kp, Vp) ->
			K = Kp,
			V = Vp
		;
			K = K0,
			V = V0
		)
	).

rbtree__lower_bound_lookup(T, SearchK, K, V) :-
	( rbtree__lower_bound_search(T, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("rbtree__lower_bound_lookup: key not found",
			SearchK, V)
	).

%-----------------------------------------------------------------------------%

rbtree__upper_bound_search(Tree, SearchK, K, V) :-
	( Tree = red(K0, V0, Left, Right)
	; Tree = black(K0, V0, Left, Right)
	),
	compare(Result, SearchK, K0),
	(
		Result = (=)
	->
		K = K0,
		V = V0
	;
		Result = (<)
	->
		( rbtree__upper_bound_search(Left, SearchK, Kp, Vp) ->
			K = Kp,
			V = Vp
		;
			K = K0,
			V = V0
		)
	;
		rbtree__upper_bound_search(Right, SearchK, K, V)
	).

rbtree__upper_bound_lookup(T, SearchK, K, V) :-
	( rbtree__upper_bound_search(T, SearchK, K0, V0) ->
		K = K0,
		V = V0
	;
		report_lookup_error("rbtree__upper_bound_lookup: key not found",
			SearchK, V)
	).

%-----------------------------------------------------------------------------%

rbtree__delete(Tree0, K, Tree) :-
	rbtree__delete_2(Tree0, K, no, _, Tree).

% rbtree__delete_2(Tree0, Key, MustRemove, MaybeValue, Tree):
%	Search the tree Tree0, looking for a node with key Key to delete.
%	If MustRemove is `yes' and we don't find the key, fail.
%	If we find the key, return it in MaybeValue and delete the node.
%	Tree is the resulting tree, whether a node was removed or not.
%
% Deletion algorithm:
%	Search down the tree, looking for the node to delete.  O(log N)
%	When we find it, there are 4 possible conditions ->
%		* Leaf node
%		    Remove node  O(1)
%		* Left subtree of node to be deleted exists
%		    Move maximum key of Left subtree to current node. O(log N)
%		* Right subtree of node to be deleted exists
%		    Move minimum key of Right subtree to current node. O(log N)
%		* Both left and right subtrees of node to be deleted exist
%		    Move maximum key of Left subtree to current node. O(log N)
%
%	Algorithm O(log N).

:- pred rbtree__delete_2(rbtree(K, V), K, bool, maybe(V), rbtree(K, V)).
:- mode rbtree__delete_2(di, in, in, uo, uo) is semidet.
:- mode rbtree__delete_2(di, in, in(bound(no)), uo, uo) is det.
:- mode rbtree__delete_2(in, in, in, out, out) is semidet.
:- mode rbtree__delete_2(in, in, in(bound(no)), out, out) is det.

rbtree__delete_2(empty, _K, no, no, empty).
rbtree__delete_2(red(K0, V0, L, R), K, MustRemove, MaybeV, Tree) :-
	compare(Result, K, K0),
	(	
		Result = (=)
	->
		(
			rbtree__remove_largest(L, NewK, NewV, NewL)
		->
			Tree = red(NewK, NewV, NewL, R)
		;
			% L must be empty
			(
				rbtree__remove_smallest(R, NewK, NewV, NewR)
			->
				Tree = red(NewK, NewV, empty, NewR)
			;
				% R must be empty
				Tree = empty
			)
		),
		MaybeV = yes(V0)
	;

		Result = (<)
	->
		rbtree__delete_2(L, K, MustRemove, MaybeV, NewL),
		Tree = red(K0, V0, NewL, R)
	;
		rbtree__delete_2(R, K, MustRemove, MaybeV, NewR),
		Tree = red(K0, V0, L, NewR)
	).
rbtree__delete_2(black(K0, V0, L, R), K, MustRemove, MaybeV, Tree) :-
	compare(Result, K, K0),
	(	
		Result = (=)
	->
		(
			rbtree__remove_largest(L, NewK, NewV, NewL)
		->
			Tree = black(NewK, NewV, NewL, R)
		;
			% L must be empty
			(
				rbtree__remove_smallest(R, NewK, NewV, NewR)
			->
				Tree = black(NewK, NewV, empty, NewR)
			;
				% R must be empty
				Tree = empty
			)
		),
		MaybeV = yes(V0)
	;

		Result = (<)
	->
		rbtree__delete_2(L, K, MustRemove, MaybeV, NewL),
		Tree = black(K0, V0, NewL, R)
	;
		rbtree__delete_2(R, K, MustRemove, MaybeV, NewR),
		Tree = black(K0, V0, L, NewR)
	).

%-----------------------------------------------------------------------------%

rbtree__remove(Tree0, K, V, Tree) :-
	rbtree__delete_2(Tree0, K, yes, yes(V), Tree).

rbtree__remove(Tree0, K, Tree) :-
	rbtree__remove(Tree0, K, _, Tree).

% rbtree__remove_largest:
%	Deletes the node with the maximum K from the tree, and returns the
%	key and value fields.

rbtree__remove_largest(empty, _K, _V, _Tree) :-
	fail.
rbtree__remove_largest(red(K0, V0, L, R), NewK, NewV, Tree) :-
	(
		R = empty
	->
		NewK = K0,
		NewV = V0,
		Tree = L
	;
		rbtree__remove_largest(R, NewK, NewV, NewR),
		Tree = red(K0, V0, L, NewR)
	).
rbtree__remove_largest(black(K0, V0, L, R), NewK, NewV, Tree) :-
	(
		R = empty
	->
		NewK = K0,
		NewV = V0,
		Tree = L
	;
		rbtree__remove_largest(R, NewK, NewV, NewR),
		Tree = black(K0, V0, L, NewR)
	).

% rbtree__remove_smallest:
%	Deletes the node with the minimum K from the tree, and returns the
%	key and value fields.

rbtree__remove_smallest(empty, _K, _V, _Tree) :-
	fail.
rbtree__remove_smallest(red(K0, V0, L, R), NewK, NewV, Tree) :-
	(
		L = empty
	->
		NewK = K0,
		NewV = V0,
		Tree = R
	;
		rbtree__remove_smallest(L, NewK, NewV, NewL),
		Tree = red(K0, V0, NewL, R)
	).
rbtree__remove_smallest(black(K0, V0, L, R), NewK, NewV, Tree) :-
	(
		L = empty
	->
		NewK = K0,
		NewV = V0,
		Tree = R
	;
		rbtree__remove_smallest(L, NewK, NewV, NewL),
		Tree = black(K0, V0, NewL, R)
	).



%-----------------------------------------------------------------------------%

rbtree__keys(empty, []).
rbtree__keys(red(K0, _V0, L, R), List) :-
	rbtree__keys(L, List0),
	rbtree__keys(R, List1),
	list__append(List0, [K0 | List1], List).
rbtree__keys(black(K0, _V0, L, R), List) :-
	rbtree__keys(L, List0),
	rbtree__keys(R, List1),
	list__append(List0, [K0 | List1], List).
	
%-----------------------------------------------------------------------------%

rbtree__values(empty, []).
rbtree__values(red(_K0, V0, L, R), List) :-
	rbtree__values(L, List0),
	rbtree__values(R, List1),
	list__append(List0, [V0 | List1], List).
rbtree__values(black(_K0, V0, L, R), List) :-
	rbtree__values(L, List0),
	rbtree__values(R, List1),
	list__append(List0, [V0 | List1], List).
	
%-----------------------------------------------------------------------------%

rbtree__count(empty, 0).
rbtree__count(red(_K, _V, L, R), N) :-
	rbtree__count(L, NO),
	rbtree__count(R, N1),
	N = 1 + NO + N1.
rbtree__count(black(_K, _V, L, R), N) :-
	rbtree__count(L, NO),
	rbtree__count(R, N1),
	N = 1 + NO + N1.

%-----------------------------------------------------------------------------%

rbtree__assoc_list_to_rbtree([], empty).
rbtree__assoc_list_to_rbtree([K - V|T], Tree) :-
	rbtree__assoc_list_to_rbtree(T, Tree0),
	rbtree__set(Tree0, K, V, Tree).

%-----------------------------------------------------------------------------%

rbtree__rbtree_to_assoc_list(empty, []).
rbtree__rbtree_to_assoc_list(red(K0, V0, Left, Right), L) :-
	rbtree__rbtree_to_assoc_list(Left, L0),
	rbtree__rbtree_to_assoc_list(Right, L1),
	list__append(L0, [K0 - V0|L1], L).
rbtree__rbtree_to_assoc_list(black(K0, V0, Left, Right), L) :-
	rbtree__rbtree_to_assoc_list(Left, L0),
	rbtree__rbtree_to_assoc_list(Right, L1),
	list__append(L0, [K0 - V0|L1], L).

%-----------------------------------------------------------------------------%

rbtree__foldl(_Pred, empty, Acc, Acc).
rbtree__foldl(Pred, red(K, V, Left, Right), Acc0, Acc) :-
	rbtree__foldl(Pred, Left, Acc0, Acc1),
	call(Pred, K, V, Acc1, Acc2),
	rbtree__foldl(Pred, Right, Acc2, Acc).
rbtree__foldl(Pred, black(K, V, Left, Right), Acc0, Acc) :-
	rbtree__foldl(Pred, Left, Acc0, Acc1),
	call(Pred, K, V, Acc1, Acc2),
	rbtree__foldl(Pred, Right, Acc2, Acc).

%-----------------------------------------------------------------------------%

rbtree__map_values(_Pred, empty, empty).
rbtree__map_values(Pred, Tree0, Tree) :-
	Tree0 = red(K0, V0, Left0, Right0),
	Tree  = red(K0, W0, Left, Right),
	call(Pred, K0, V0, W0),
	rbtree__map_values(Pred, Left0, Left),
	rbtree__map_values(Pred, Right0, Right).
rbtree__map_values(Pred, Tree0, Tree) :-
	Tree0 = black(K0, V0, Left0, Right0),
	Tree  = black(K0, W0, Left, Right),
	call(Pred, K0, V0, W0),
	rbtree__map_values(Pred, Left0, Left),
	rbtree__map_values(Pred, Right0, Right).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
% Ralph Becket <rwab1@cl.cam.ac.uk> 29/04/99
% 	Function forms added.

rbtree__init = RBT :-
	rbtree__init(RBT).

rbtree__set(RBT1, K, V) = RBT2 :-
	rbtree__set(RBT1, K, V, RBT2).

rbtree__insert_duplicate(RBT1, K, V) = RBT2 :-
	rbtree__insert_duplicate(RBT1, K, V, RBT2).

rbtree__lookup(RBT, K) = V :-
	rbtree__lookup(RBT, K, V).

rbtree__delete(RBT1, K) = RBT2 :-
	rbtree__delete(RBT1, K, RBT2).

rbtree__keys(RBT) = Ks :-
	rbtree__keys(RBT, Ks).

rbtree__values(RBT) = Vs :-
	rbtree__values(RBT, Vs).

rbtree__count(RBT) = N :-
	rbtree__count(RBT, N).

rbtree__assoc_list_to_rbtree(AL) = RBT :-
	rbtree__assoc_list_to_rbtree(AL, RBT).

rbtree__rbtree_to_assoc_list(RBT) = AL :-
	rbtree__rbtree_to_assoc_list(RBT, AL).

rbtree__foldl(F, T, A) = B :-
	P = ( pred(W::in, X::in, Y::in, Z::out) is det :- Z = F(W, X, Y) ),
	rbtree__foldl(P, T, A, B).

rbtree__map_values(F, T1) = T2 :-
	P = ( pred(X::in, Y::in, Z::out) is det :- Z = F(X, Y) ),
	rbtree__map_values(P, T1, T2).

