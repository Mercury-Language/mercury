% nondet_ite_3.m: test nondet if-then-else with semidet condition.

:- module nondet_ite_3.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, int, std_util.

:- pred q(int::out) is nondet.

q(X) :-
	(if semidet_fail then
		X = 41
	else
		(
			X = 42
		;
			X = 43
		)
	).

main -->
	{ solutions(q, List) },
	write_int_list(List).

:- pred write_int_list(list(int)::in, io__state::di, io__state::uo) is det.

write_int_list([]) --> [].
write_int_list([X|Xs]) -->
	io__write_int(X),
	io__write_string("\n"),
	write_int_list(Xs).

