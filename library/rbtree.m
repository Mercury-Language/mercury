%-----------------------------------------------------------------------------%
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
%	insert's or update's. Never fails.
%
% insert_duplicate:
%	insert's duplicate keys into the tree, never fails.  Search doesn't
%	yet support looking for duplicates.
%
% delete:
%	delete's a node from the tree if it exists.
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
:- mode rbtree__init(out) is det.

	% Insert's a new key-value pair into the tree.  Fails if key 
	% already in the tree.
:- pred rbtree__insert(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__insert(in, in, in, out) is semidet.

	% Update's the value associated with a key.  Fails if the key 
	% doesn't exist.
:- pred rbtree__update(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__update(in, in, in, out) is semidet.

	% Set's a value irregardless of whether key exists or not.  Never
	% fails.
:- pred rbtree__set(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__set(in, in, in, out) is det.

	% Insert a duplicate key into the tree.  Never fails.
:- pred rbtree__insert_duplicate(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__insert_duplicate(in, in, in, out) is det.

	% Lookup a value associated with a key.  Program abort's if key
	% doesn't exist.
:- pred rbtree__lookup(rbtree(K, V), K, V).
:- mode rbtree__lookup(in, in, out) is det.

	% Search for a key-value pair using the key.  Fails if key doesn't
	% exist.
:- pred rbtree__search(rbtree(K, V), K, V).
:- mode rbtree__search(in, in, out) is semidet.

	% Delete the key value pair associated with a key.  Does nothing
	% if the key doesn't exist.
:- pred rbtree__delete(rbtree(K, V), K, rbtree(K, V)).
:- mode rbtree__delete(in, in, out) is det.

	% Remove the key value pair associated with a key.  Fails
	% if the key doesn't exist.
:- pred rbtree__remove(rbtree(K, V), K, rbtree(K, V)).
:- mode rbtree__remove(in, in, out) is semidet.

	% Return's an in-order list of all the key's in the rbtree.
:- pred rbtree__keys(rbtree(K, V), list(K)).
:- mode rbtree__keys(in, out) is det.

	% Return's a list of values such that the key's associated with the
	% values are in-order.
:- pred rbtree__values(rbtree(K, V), list(V)).
:- mode rbtree__values(in, out) is det.

	% Count the number of elements in the tree
:- pred rbtree__count(rbtree(K, V), int).
:- mode rbtree__count(in, out) is det.

:- pred rbtree__assoc_list_to_rbtree(assoc_list(K, V), rbtree(K, V)).
:- mode rbtree__assoc_list_to_rbtree(in, out) is det.

:- pred rbtree__rbtree_to_assoc_list(rbtree(K, V), assoc_list(K, V)).
:- mode rbtree__rbtree_to_assoc_list(in, out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module int, require, std_util.


:- type rbtree(K,V)	 --->	empty
			;	red(K, V, rbtree(K,V), rbtree(K,V))
			;	black(K, V, rbtree(K, V), rbtree(K, V)).

%-----------------------------------------------------------------------------%

rbtree__init(empty).

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
				% if the current node is black(known), and it's
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
				% if the current node is black(known), and it's
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
				% if the current node is black(known), and it's
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
			rbtree__set_2(R0, K, V, NewR),
			(
				% Only need to start looking for a rotation case
				% if the current node is black(known), and it's
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
				% if the current node is black(known), and it's
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
				% if the current node is black(known), and it's
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
				% if the current node is black(known), and it's
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

rbtree__lookup(T, K, V) :-
        (
                rbtree__search(T, K, V0)
        ->
                V = V0
        ;
                error("rbtree__lookup: key not found.")
        ).

%-----------------------------------------------------------------------------%

rbtree__search(empty, _K, _V) :-
	fail.
rbtree__search(red(K0, V0, Left, Right), K, V) :-
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
rbtree__search(black(K0, V0, Left, Right), K, V) :-
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

%-----------------------------------------------------------------------------%

rbtree__delete(Tree0, K, Tree) :-
	(
		rbtree__remove(Tree0, K, Tree1)
	->
		Tree = Tree1
	;
		Tree = Tree0
	).
	
%-----------------------------------------------------------------------------%

% rbtree_remove:
%	Search down the tree, looking for the node to remove.  O(log N)
%	When we find it, there are 4 possible conditions ->
%		* Leaf node
%		    Remove node  O(1)
%		* Left subtree of node to be deleted exists
%		    Move maximum key of Left subtree to current node. O(log N)
%		* Right subtree of node to be deleted exists
%		    Move minimum key of Right subtree to current node. O(log N)
%		* Both of node to be deleted left and right subtree exist
%		    Move maximum key of Left subtree to current node. O(log N)
%
%	Algorithm O(log N).

rbtree__remove(empty, _K, _Tree) :-
	fail.
rbtree__remove(red(K0, V0, L, R), K, Tree) :-
	compare(Result, K, K0),
	(	
		Result = (=)
	->
		(
			L = empty
		->	
			(
				R = empty
			->
				Tree = empty
			;
				rbtree__get_tree_min(R, NewK, NewV, NewR),
				Tree = red(NewK, NewV, L, NewR)
			)
		;
			rbtree__get_tree_max(L, NewK, NewV, NewL),
			Tree = red(NewK, NewV, NewL, R)
		)
	;

		Result = (<)
	->
		rbtree__remove(L, K, NewL),
		Tree = red(K0, V0, NewL, R)
	;
		rbtree__remove(R, K, NewR),
		Tree = red(K0, V0, L, NewR)
	).
rbtree__remove(black(K0, V0, L, R), K, Tree) :-
	compare(Result, K, K0),
	(	
		Result = (=)
	->
		(
			L = empty
		->	
			(
				R = empty
			->
				Tree = empty
			;
				rbtree__get_tree_min(R, NewK, NewV, NewR),
				Tree = black(NewK, NewV, L, NewR)
			)
		;
			rbtree__get_tree_max(L, NewK, NewV, NewL),
			Tree = black(NewK, NewV, NewL, R)
		)
	;

		Result = (<)
	->
		rbtree__remove(L, K, NewL),
		Tree = black(K0, V0, NewL, R)
	;
		rbtree__remove(R, K, NewR),
		Tree = black(K0, V0, L, NewR)
	).

% rbtree__get_tree_max:
%	Delete's the node with the maximum K from the tree, and returns the
%	key and value fields.

:- pred rbtree__get_tree_max(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__get_tree_max(in, out, out, out) is det.

rbtree__get_tree_max(empty, _K, _V, _Tree) :-
	error("rbtree__get_tree_max: attempted to get K+V from empty tree").
rbtree__get_tree_max(red(K0, V0, L, R), NewK, NewV, Tree) :-
	(
		R = empty
	->
		NewK = K0,
		NewV = V0,
		Tree = L
	;
		rbtree__get_tree_max(R, NewK, NewV, NewR),
		Tree = red(K0, V0, L, NewR)
	).
rbtree__get_tree_max(black(K0, V0, L, R), NewK, NewV, Tree) :-
        (
                R = empty
        ->
                NewK = K0,
                NewV = V0,
                Tree = L
        ;
                rbtree__get_tree_max(R, NewK, NewV, NewR),
                Tree = black(K0, V0, L, NewR)
        ).

% rbtree__get_tree_min:
%	Delete's the node with the minimum K from the tree, and returns the
%	key and value fields.

:- pred rbtree__get_tree_min(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__get_tree_min(in, out, out, out) is det.

rbtree__get_tree_min(empty, _K, _V, _Tree) :-
	error("rbtree__get_tree_min: attempted to get K+V from empty tree").
rbtree__get_tree_min(red(K0, V0, L, R), NewK, NewV, Tree) :-
	(
		L = empty
	->
		NewK = K0,
		NewV = V0,
		Tree = R
	;
		rbtree__get_tree_min(L, NewK, NewV, NewL),
		Tree = red(K0, V0, NewL, R)
	).
rbtree__get_tree_min(black(K0, V0, L, R), NewK, NewV, Tree) :-
        (
                L = empty
        ->
                NewK = K0,
                NewV = V0,
                Tree = R
        ;
                rbtree__get_tree_min(L, NewK, NewV, NewL),
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
	N is 1 + NO + N1.
rbtree__count(black(_K, _V, L, R), N) :-
	rbtree__count(L, NO),
	rbtree__count(R, N1),
	N is 1 + NO + N1.

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
