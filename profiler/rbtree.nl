%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%
%
%  Red-black tree module.
%  Main author: petdr
%
%  Contains an implementation of red black trees.
%
%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- module rbtree.
:- interface.

:- type rbtree(K,V)
	--->	empty
	;	rbtree(color, K, V, rbtree(K,V), rbtree(K,V)).

:- type color
	--->	red
	;	black.

:- pred rbtree__init(rbtree(K, V)).
:- mode rbtree__init(out) is det.

:- pred rbtree__insert(rbtree(K, V), K, V, rbtree(K, V)).
:- mode rbtree__insert(in, in, in, out) is semidet.

:- pred rbtree__search(rbtree(K, V), K, V).
:- mode rbtree__search(in, in, out) is semidet.

%-----------------------------------------------------------------------------%
:- implementation.

%-----------------------------------------------------------------------------%

rbtree__init(empty).

%-----------------------------------------------------------------------------%

rbtree__insert(empty, K, V, rbtree(black, K, V, empty, empty)).
rbtree__insert(rbtree(Color, K1, V1, L1, R1), K, V, Tree) :-
	rbtree__insert_2(rbtree(Color, K1, V1, L1, R1), K, V, Tree1),
	% Ensure that the root of the tree is black.
	(
		Tree1 = rbtree(red, K0, V0, L0, R0)
	->
		Tree = rbtree(black, K0, V0, L0, R0)
	;
		Tree = Tree1
	).

:- pred rbtree__insert_2(rbtree(K, V), K, V, rbtree(K, V)). 
:- mode rbtree__insert_2(in, in, in, out) is semidet.

rbtree__insert_2(empty, K, V, rbtree(red, K, V, empty, empty)).
rbtree__insert_2(Tree0, K, V, Tree) :-
	Tree0 = rbtree(_Color, K0, V0, L0, R0),

	% On the way down the rbtree we split any 4-nodes we find.
	( 	
		L0 = rbtree(red, LK0, LV0, LL0, LR0),
	  	R0 = rbtree(red, RK0, RV0, RL0, RR0)
	->
		L1 = rbtree(black, LK0, LV0, LL0, LR0),
		R1 = rbtree(black, RK0, RV0, RL0, RR0),
		Tree1 = rbtree(red, K0, V0, L1, R1)
	;
		Tree1 = Tree0
	),

	% Work out which side of the rbtree to insert.
	Tree1 = rbtree(Color, K2, V2, L2, R2),
	compare(Result, K, K2),
	( 	
		Result = (<)
	->
		rbtree__insert_2(L2, K, V, L),
		(
			% Only need to start looking for a rotation case if
			% the current node is black, and it's child red. 
			Color = black,
			L = rbtree(red, LK, LV, LL, LR)
		->
			% Check if a Child's child is red
			( LL = rbtree(red, _LLK, _LLV, _LLL, _LLR) ->
				TreeR = rbtree(red, K2, V2, LR, R2),
				Tree = rbtree(black, LK, LV, LL, TreeR)
			; LR = rbtree(red, LRK, LRV, LRL, LRR) ->
				TreeL = rbtree(red, LK, LV, LL, LRL),
				TreeR = rbtree(red, K2, V2, LRR, R2),
				Tree = rbtree(black, LRK, LRV, TreeL, TreeR)
			;
				Tree = rbtree(Color, K2, V2, L, R2)
			)
		;
			Tree = rbtree(Color, K2, V2, L, R2)
		)
	;
		Result = (>)
	->
		rbtree__insert_2(R2, K, V, R),
		(
			Color = black,
			R = rbtree(red, RK, RV, RL, RR)
		->
			% Check if a Child's child is red
			( RL = rbtree(red, RLK, RLV, RLL, RLR) ->
				TreeL = rbtree(red, K2, V2, L2, RLL),
				TreeR = rbtree(red, RK, RV, RLR, RR),
				Tree = rbtree(black, RLK, RLV, TreeL, TreeR)
			; RR = rbtree(red, _RRK, _RRV, _RRL, _RRR) ->
				TreeL = rbtree(red, K2, V2, L2, RL),
				Tree = rbtree(black, RK, RV, TreeL, RR)
			;
				Tree = rbtree(Color, K2, V2, L2, R)
			)
		;
			Tree = rbtree(Color, K2, V2, L2, R)
		)
	;
		fail
	).

%-----------------------------------------------------------------------------%

rbtree__search(empty, _K, _V) :-
	fail.
rbtree__search(rbtree(_Color, K0, V0, Left, Right), K, V) :-
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

:- pred rbtree__color(rbtree(K, V), color).
:- mode rbtree__color(in, out) is semidet.

rbtree__color(rbtree(Color, _, _, _, _), Color).

