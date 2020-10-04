%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test of a bunch of different cases of currying.
% It uses the predicates defined in curry2_test.m.
% This is specifically aimed at testing the code
% which optimizes currying in compiler/closure_gen.m.
%

:- module curry2.
:- interface.

:- import_module io.
:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module curry2_test.
:- import_module int.

main(!IO) :-
    n(foo, NFoo),
    do_test(NFoo, !IO),
    p(foo, PFoo),
    do_test(PFoo, !IO),
    ( if q(foo, QFoo) then
        do_test(QFoo, !IO)
    else
        io.write_string("q/2 failed\n", !IO)
    ),
    r(foo, 42, RFoo),
    do_test(RFoo, !IO),
    s(foo2, 42, SFoo),
    do_test2(SFoo, !IO),
    t(foo2, 42, TFoo),
    do_test(TFoo, !IO),
    n(bar(7), NBar),
    do_test(NBar, !IO),
    p(bar(7), PBar),
    do_test(PBar, !IO),
    ( if q(bar(7), QBar) then
        do_test(QBar, !IO)
    else
        io.write_string("q/2 failed\n", !IO)
    ),
    r(bar(7), 42, RBar),
    do_test(RBar, !IO),
    s(bar2(7), 42, SBar),
    do_test2(SBar, !IO),
    t(bar2(7), 42, TBar),
    do_test(TBar, !IO).

:- pred do_test(pred(int, int)::in(pred(in, out) is det),
    io::di, io::uo) is det.

do_test(Pred, !IO) :-
    call(Pred, 3, X),
    call(Pred, 3, Y),
    io.write_int(X, !IO),
    io.write_string(" ", !IO),
    io.write_int(Y, !IO),
    io.write_string("\n", !IO).

:- pred do_test2(pred(int, int)::in(pred(out, in) is det),
    io::di, io::uo) is det.

do_test2(Pred, !IO) :-
    call(Pred, X, 3),
    call(Pred, Y, 3),
    io.write_int(X, !IO),
    io.write_string(" ", !IO),
    io.write_int(Y, !IO),
    io.write_string("\n", !IO).

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
