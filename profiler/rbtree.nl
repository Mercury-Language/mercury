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

:- type tree(K,V)
	--->	empty
	;	tree(color, K, V, tree(K,V), tree(K,V)).

:- type color
	--->	red
	;	black.

:- pred rb_insert(tree(K, V), K, V, tree(K, V)).
:- mode rb_insert(in, in, in, out) is semidet.

%-----------------------------------------------------------------------------%
:- implementation.

rb_insert(empty, K, V, tree(red, K, V, empty, empty)).
rb_insert(Tree0, K, V, Tree) :-
	Tree0 = tree(_Color, K0, V0, L0, R0),

	% On the way down the tree we split any 4-nodes we find.
	( 	
		L0 = tree(red, LK0, LV0, LL0, LR0),
	  	R0 = tree(red, RK0, RV0, RL0, RR0)
	->
		L1 = tree(black, LK0, LV0, LL0, LR0),
		R1 = tree(black, RK0, RV0, RL0, RR0),
		Tree1 = tree(red, K0, V0, L1, R1)
	;
		Tree1 = Tree0
	),

	% Work out which side of the tree to insert.
	Tree1 = tree(Color, K2, V2, L2, R2),
	compare(Result, K, K2),
	( 	
		Result = (<),
		rb_insert(L2, K, V, L),
		(
			% Only need to start looking for a rotation case if
			% the current node is black, and it's child red. 
			Color = black,
			L = tree(red, LK, LV, LL, LR)
		->
			% Check if a Child's child is red
			( LL = tree(red, LLK, LLV, LLL, LLR) ->
				Tree = tree(black, LK, LV, LL, TreeR),
				TreeR = tree(red, K2, V2, LR, R2)
			; LR = tree(red, LRK, LRV, LRL, LRR) ->
				Tree = tree(black, LRK, LRV, TreeL, TreeR),
				TreeL = tree(red, LK, LV, LL, LRL),
				TreeR = tree(red, K2, V2, LRR, R2)
			;
				Tree = tree(Color, K2, V2, L, R2)
			)
		;
			Tree = tree(Color, K2, V2, L, R2)
		)
	;
		Result = (>),
		rb_insert(R2, K, V, R),
		(
			Color = black,
			R = tree(red, RK, RV, RL, RR)
		->
			% Check if a Child's child is red
			( RL = tree(red, RLK, RLV, RL, RR) ->
				Tree = tree(black, RLK, RLV, TreeL, TreeR),
				TreeL = tree(red, K2, V2, L2, RLL),
				TreeR = tree(red, RK, RV, RLR, RR)
			; RR = tree(red, RRK, RRV, RL, RR) ->
				Tree = tree(black, RK, RV, TreeL, RR),
				TreeL = tree(red, K2, V2, L2, RL)
			;
				Tree = tree(Color, K2, V2, L2, R)
			)
		;
			Tree = tree(Color, K2, V2, L2, R)
		)
	;
		fail
	).

:- pred rb_color(tree(K, V), color).
:- mode rb_color(in, out) is semidet.

rb_color(tree(Color, _, _, _, _), Color).

