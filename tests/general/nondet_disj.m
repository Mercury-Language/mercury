% This tests the case of a nondet call followed by a disjunction.

:- module nondet_disj.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, list.

main -->
	{ solutions(lambda([Pair::out] is multi,
		(Pair = X-Y, q(X, Y))), List) },
	print_list(List).

:- pred print_list(list(pair(int))::in, io__state::di, io__state::uo) is det.

print_list([]) --> [].
print_list([X-Y|Rest]) -->
	io__write_string("X = "),
	io__write_int(X),
	io__write_string(", Y = "),
	io__write_int(Y),
	io__write_string("\n"),
	print_list(Rest).

:- pred q(int::out, int::out) is multidet.

q(X, Y) :-
	p(X),
	(
		Y = 41
	;
		Y = 42
	).

:- pred p(int::out) is multidet.

p(1).
p(2).

