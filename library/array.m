
:- type subarray(T)	--->	node(T)
			;	two(
					integer,
					integer,
					array(T),
					array(T)
				)
			;	three(
					integer,
					integer,
					array(T),
					array(T),
					array(T)
				).

:- pred array__init(integer, array(T)).
:- mode array__init(input, output).	% want an array_skeleton?

array__init(Low, High, Array) :-
	Size0 is High - Low,
	Size is Size0 + 1,
	(if
		Size = 0
	then
		require(fail, "Cannot have a zero length array")
	else if
		Size = 1
	then
		Array = node(_)
	else if
		Size = 2
	then
		Array = two(Low, High, node(_), node(_))
	else if
		Size = 3
	then
		Array = three(Low, High, node(_), node(_), node(_))
	else if
		some [N] (
			0 is Size mod 2,
			N is Size mod 3,
			not(N = 3)
		)
	then
		N is Size // 2,
		L2 is Low + N,
		H1 is L2 - 1,
		array__init(Low, H1, A1),
		array__init(L2, High, A2),
		Array = two(Low, High, A1, A2)
	else 
		N is Size // 3,
		L2 is Low + N,
		L3 is L1 + N,
		H1 is L2 - 1,
		H2 is L3 - 1,
		array__init(Low, H1, A1),
		array__init(L2, H2, A2),
		array__init(L3, High, A3),
		Array = three(Low, High, A1, A2, A3)
	).

:- pred array__search(array(T), integer, T).
:- mode array__search(input, input, output).

array__search(node(Item), Index, Item).
array__search(two(Low, High, Left, Right), Index, Item) :-
	Size is High - Low,
	Half is Size / 2,
	Mid is Low + Half,
	(if
		Index < Mid
	then
		array__search(Left, Index, Item)
	else
		array__search(Right, Index, Item)
	).
array__search(three(Low, High, Left, Middle, Right), Index, Item) :-
	Size is High - Low,
	Third is Size / 3,
	Mid1 is Low + Third,
	Mid2 is Mid1 + Third,
	(if
		Index < Mid1
	then
		array__search(Left, Index, Item)
	else if
		Index < Mid2
	then
		array__search(Middle, Index, Item)
	else
		array__search(Right, Index, Item)
	).

:- pred array__set(array(T), integer, T, array(T)).
:- mode array__set(input, input, input, output).

array__set(node(_), Index, Item, node(Item)).
array__set(two(Low, High, Left, Right), Index, Item, A) :-
	Size is High - Low,
	Half is Size / 2,
	Mid is Low + Half,
	(if
		Index < Mid
	then
		array__set(Left, Index, Item, Left1),
		A = two(Low, High, Left1, Right)
	else
		array__set(Right, Index, Item, Right1),
		A = two(Low, High, Left, Right1)
	).
array__set(three(Low, High, Left, Middle, Right), Index, Item, A) :-
	Size is High - Low,
	Third is Size / 3,
	Mid1 is Low + Third,
	Mid2 is Mid1 + Third,
	(if
		Index < Mid1
	then
		array__set(Left, Index, Item, Left1),
		A = three(Low, High, Left1, Middle, Right)
	else if
		Index < Mid2
	then
		array__set(Middle, Index, Item, Middle1),
		A = three(Low, High, Left, Middle1, Right)
	else
		array__set(Right, Index, Item, Right1),
		A = three(Low, High, Left, Middle, Right1)
	).

