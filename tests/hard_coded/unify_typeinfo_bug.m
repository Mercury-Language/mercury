:- module unify_typeinfo_bug.  
:- interface.

:- import_module io, list.

:- pred main(io__state::di, io__state::uo) is det.

:- pred unify_bug(list(T)::in, list(T)::in) is semidet.

:- pred exist_unify_bug(list(T)::in, list(T)::in) is semidet.

:- type set_bbbtree(T).

:- pred singleton_set(set_bbbtree(T), T).
:- mode singleton_set(in, in) is semidet.

:- implementation.

main -->
	( { unify_bug([1], [1]) } ->
		io__write_string("Succeeded\n")
	;
		io__write_string("Failed\n")
	),
	( { exist_unify_bug([1], [1]) } ->
		io__write_string("Succeeded\n")
	;
		io__write_string("Failed\n")
	),
	( { exist_unify_bug([1], [1]) } ->
		io__write_string("Succeeded\n")
	;
		io__write_string("Failed\n")
	),
	( { singleton_set(tree([1], 1, empty, empty), [1]) } ->
		io__write_string("Succeeded\n")
	;
		io__write_string("Failed\n")
	).

unify_bug(A, B) :-
	A = [H | _],
	B = [H | _].

exist_unify_bug(A, B) :-
	C = D,
	exist_id(A, B, C, D).

:- some [U] pred exist_id(T::in, T::in, U::out, U::out) is det.

exist_id(A, B, A, B).

:- type set_bbbtree(T)
	--->	empty
	;	tree(T, int, set_bbbtree(T), set_bbbtree(T)).

singleton_set(tree(V, 1, empty, empty), V).
