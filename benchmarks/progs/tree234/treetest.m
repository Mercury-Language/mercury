:- module treetest.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, require, tree234x.

% -- POINTS ------------------------------------------------------------------

:- type inttree == tree234x(int, int).

main -->
	{ repeat_test(num_repetitions) }.

:- func num_repetitions = int.
:- func tree_size = int.

num_repetitions = 100.
tree_size = 100000.

:- pred repeat_test(int::in) is det.

repeat_test(NumLeft) :-
	( NumLeft > 0 ->
		test(tree_size),
		repeat_test(NumLeft - 1)
	;
		true
	).

:- pred test(int::in) is det.

test(Size) :-
	T0 = tree234x__init,
	test_insert(1, Size, T0, T),
	( test_check(1, Size, T) ->
		true
	;
		error("mismatch")
	).

:- pred test_insert(int::in, int::in, inttree::in, inttree::out) is det.

test_insert(Cur, Max, T0, T) :-
	( Cur < Max ->
		tree234x__set(T0, Cur, Cur, T1),
		test_insert(Cur + 1, Max, T1, T)
	;
		T = T0
	).

:- pred test_check(int::in, int::in, inttree::in) is semidet.

test_check(Cur, Max, T) :-
	( Cur < Max ->
		tree234x__lookup(T, Cur, Val),
		Cur = Val,
		test_check(Cur + 1, Max, T)
	;
		true
	).
