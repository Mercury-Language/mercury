% This is a test of a bunch of different cases of currying.
% It uses the predicates defined in curry2_test.m.
% This is specifically aimed at testing the code
% which optimizes currying in compiler/unify_gen.m.

:- module curry2.
:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module int.
:- import_module curry2_test.

main -->
	{ n(foo, NFoo) }, do_test(NFoo),
	{ p(foo, PFoo) }, do_test(PFoo),
	( { q(foo, QFoo) } -> do_test(QFoo)
	; io__write_string("q/2 failed\n")
	),
	{ r(foo, 42, RFoo) }, do_test(RFoo),
	{ s(foo2, 42, SFoo) }, do_test2(SFoo),
	{ t(foo2, 42, TFoo) }, do_test(TFoo),
	{ n(bar(7), NBar) }, do_test(NBar),
	{ p(bar(7), PBar) }, do_test(PBar),
	( { q(bar(7), QBar) } -> do_test(QBar)
	; io__write_string("q/2 failed\n")
	),
	{ r(bar(7), 42, RBar) }, do_test(RBar),
	{ s(bar2(7), 42, SBar) }, do_test2(SBar),
	{ t(bar2(7), 42, TBar) }, do_test(TBar).
	
:- pred do_test(pred(int, int)::in(pred(in, out) is det),
		io__state::di, io__state::uo).

do_test(Pred) -->
	{ call(Pred, 3, X) },
	{ call(Pred, 3, Y) },
	io__write_int(X),
	io__write_string(" "),
	io__write_int(Y),
	io__write_string("\n").

:- pred do_test2(pred(int, int)::in(pred(out, in) is det),
		io__state::di, io__state::uo).

do_test2(Pred) -->
	{ call(Pred, X, 3) },
	{ call(Pred, Y, 3) },
	io__write_int(X),
	io__write_string(" "),
	io__write_int(Y),
	io__write_string("\n").


:- pred foo(int, int, int).
:- mode foo(in, in, out) is det.

foo(X, Y, Z) :-
	Z = 100 * X + 10 * Y.

:- pred bar(int, int, int, int).
:- mode bar(in, in, in, out) is det.

bar(A, B, C, D) :-
	D = 1000 * A + 100 * B + 10 * C.

:- pred foo2(int, int, int).
:- mode foo2(in, out, in) is det.

foo2(X, Z, Y) :-
	Z = 100 * X + 10 * Y.

:- pred bar2(int, int, int, int).
:- mode bar2(in, in, out, in) is det.

bar2(A, B, D, C) :-
	D = 1000 * A + 100 * B + 10 * C.
