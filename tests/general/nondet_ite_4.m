% nondet_ite_4.m: test nondet if-then-else with det `else' part
% followed by a test, in a context with a known failure cont.
% Same as nondet_ite_2, except that the call to p/1 has been
% manually inlined.
% (This is a regression test - the compiler used to fail this
% test case.)

:- module nondet_ite_4.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module list, int, std_util.

:- pred q(int::out, int::out) is nondet.

q(X, Y) :-
	p(X),
	(
		(if some [Y1] (Y1 = 0 ; Y1 = 1; Y1 = 2) then
			Y = Y1
		else
			Y = 42
		),
		Y > 0
	;
		Y = 44
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

