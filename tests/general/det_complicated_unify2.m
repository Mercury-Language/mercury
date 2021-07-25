%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests that the compiler handles deterministic complicated
% unifications on enums and compound types correctly.
% Version 0.4 of the compiler failed this test, as did some versions of
% an optimization in unify_proc__generate_du_unify_clauses.

:- module det_complicated_unify2.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo
    --->    foo(bar).
:- type foo2
    --->    foo2(bar)
    ;       yyy(int).
:- type fum
    --->    fum(baz).
:- type fum2
    --->    fum2(baz)
    ;       yyy(int).
:- type bar
    --->    bar
    ;       zzz(int).
:- type baz
    --->    baz1
    ;       baz2
    ;       zzz(int).

main(!IO) :-
    p(foo(bar), foo(bar)),
    p1(foo2(bar), foo2(bar)),
    q(bar, bar),
    r(fum(baz1), fum(baz1)),
    r1(fum2(baz1), fum2(baz1)),
    r2(fum2(baz2), fum2(baz2)),
    s1(baz1, baz1),
    s2(baz2, baz2),
    io__write_string("worked\n", !IO).

:- pred p(foo::in(bound(foo(bound(bar)))), foo::in(bound(foo(bound(bar)))))
    is det.

p(X, X).

:- pred p1(foo2::in(bound(foo2(bound(bar)))),
    foo2::in(bound(foo2(bound(bar))))) is det.

p1(X, X).

:- pred q(bar::in(bound(bar)), bar::in(bound(bar))) is det.

q(X, X).

:- pred r(fum::in(bound(fum(bound(baz1)))), fum::in(bound(fum(bound(baz1)))))
    is det.

r(X, X).

:- pred r1(fum2::in(bound(fum2(bound(baz1)))),
    fum2::in(bound(fum2(bound(baz1))))) is det.

r1(X, X).

:- pred r2(fum2::in(bound(fum2(bound(baz2)))),
    fum2::in(bound(fum2(bound(baz2))))) is det.

r2(X, X).

:- pred s1(baz::in(bound(baz1)), baz::in(bound(baz1))) is det.

s1(X, X).

:- pred s2(baz::in(bound(baz2)), baz::in(bound(baz2))) is det.

s2(X, X).
