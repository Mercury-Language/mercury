:- module compare_rep_usereq.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module std_util.

main -->
	test(da, da),
	test(da, db).

:- type val == pair(string, univ).

:- func da = val.
da = "aa : foo" - univ(aa).

:- func db = val.
db = "bb : foo" - univ(bb).

:- type foo ---> aa ; bb
	where equality is foo_eq.

:- pred foo_eq(foo::in, foo::in) is semidet.
foo_eq(_, _) :-
	semidet_succeed.

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

