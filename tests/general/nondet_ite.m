% nondet_ite.m: test nondet if-then-else with nondet condition.

:- module nondet_ite.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, int, std_util.

:- pred q(int::out, int::out) is nondet.

q(X, Y) :-
	p(X),
	(if some [Y1] p(Y1) then
		Y = Y1
	else
		Y = 42
	).

:- pred p(int::out) is nondet.

p(0).
p(1).
p(2).

:- pred r(int::out) is nondet.

r(Z) :-
	q(X, Y),
	Z is X * 100 + Y.

main -->
	{ solutions(r, List) },
	write_int_list(List).

:- pred write_int_list(list(int)::in, io__state::di, io__state::uo) is det.

write_int_list([]) --> [].
write_int_list([X|Xs]) -->
	io__write_int(X),
	io__write_string("\n"),
	write_int_list(Xs).

