% This tests that the compiler handles deterministic complicated
% unifications on enums and compound types correctly.
% (Version 0.4 of the compiler failed this test.)

:- module det_complicated_unify2.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type foo ---> foo(bar).
:- type foo2 ---> foo2(bar) ; yyy(int).
:- type bar ---> bar ; zzz(int).

main -->
	{ p(foo(bar),foo(bar)), p2(foo2(bar),foo2(bar)), q(bar, bar) },
	io__write_string("worked\n").

:- pred p(foo::in(bound(foo(bound(bar)))), foo::in(bound(foo(bound(bar)))))
	is det.
p(X, X).

:- pred p2(foo2::in(bound(foo2(bound(bar)))),
	foo2::in(bound(foo2(bound(bar))))) is det.
p2(X, X).

:- pred q(bar::in(bound(bar)), bar::in(bound(bar))) is det.
q(X, X).
