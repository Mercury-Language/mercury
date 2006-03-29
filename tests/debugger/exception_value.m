:- module exception_value.

:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.

:- import_module pair, exception, list.

main -->
	{ test1(Res1) },
	print(Res1),
	io__nl,
	{ test2(Res2) },
	print(Res2),
	io__nl.

:- pred test1(exception_result(int)::out) is cc_multi.
test1(Res) :-
	try(p, Res).

:- pred test2(exception_result(int)::out) is cc_multi.
test2(Res) :-
	try(q, Res).

:- pred p(int::out) is det.

p(_) :-
	throw("p exception").

:- pred q(int::out) is det.

q(_) :-
	throw("q oops" - [1, 2, 3]).

