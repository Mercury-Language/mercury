:- module compare_representation.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module std_util.

main -->
	test(d1, d1),
	test(d1, dm),
	test(dm, dm),
	test(dm, dt),
	test(df1, df2).

:- type val == pair(string, univ).

:- func d1 = val.
d1 = "1 : int" - univ(1).

:- func dm = val.
dm = "main : pred(io__state, io__state)" - univ(main).

:- func dt = val.
dt = "test(d1, dm) : pred(io__state, io__state)" - univ(test(d1, dm)).

:- func df1 = val.
df1 = "foo(1) : func(int) = int" - univ(foo(1)).

:- func df2 = val.
df2 = "foo(2) : func(int) = int" - univ(foo(2)).

:- func foo(int, int) = int.
foo(_, Z) = Z.

:- pred test(val::in, val::in, io__state::di, io__state::uo) is cc_multi.
test(SA - A, SB - B) -->
	io__write_string(SA),
	io__nl,
	io__write_string(SB),
	io__nl,
	{ std_util__compare_representation(Res, A, B) },
	io__write_string("Result = "),
	io__write(Res),
	io__write_string(".\n\n").

