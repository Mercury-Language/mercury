%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This tests that the compiler handles deterministic complicated
% unifications on enums and compound types correctly.
% (Version 0.4 of the compiler failed this test.)

:- module det_complicated_unify_1.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- type foo
    --->    foo(bar).
:- type bar
    --->    bar.

main(!IO) :-
    p(foo(bar), foo(bar)), q(bar, bar),
    io.write_string("worked\n", !IO).

:- pred p(foo::in(bound(foo(bound(bar)))), foo::in(bound(foo(bound(bar)))))
    is det.

p(X, X).

:- pred q(bar::in(bound(bar)), bar::in(bound(bar))) is det.

q(X, X).
