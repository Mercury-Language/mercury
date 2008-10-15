% vim: ft=mercury ts=4 sw=4 et
%
% This is a regression test. The version of the compiler before Oct 7, 2008
% used to abort on this code with the error message
%
%   instmap.m: Unexpected: merge_instmapping_delta_2: error merging var 17

:- module consume_in_some_branches_and_after.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.

main(!IO) :-
    (
        X1 = 1
    &
        Y1 = 2,
        q(X1, Y1, Z1)
    ),
    io.write_int(Z1, !IO),
    io.nl(!IO),
    (
        X2 = 1
    &
        Y2 = 2,
        r(X2, Y2, Z2)
    ),
    io.write_int(Z2, !IO),
    io.nl(!IO),
    (
        X3 = 1
    &
        Y3 = 3,
        s(X3, Y3, Z3)
    ),
    io.write_int(Z3, !IO),
    io.nl(!IO).

:- pred q(int::in, int::in, int::out) is det.
:- pragma no_inline(q/3).

q(X, Y, Z) :-
    ( Y = 2 ->
        A = Y
    ;
        A = X
    ),
    Z = X + A.

:- pred r(int::in, int::in, int::out) is det.
:- pragma no_inline(r/3).

r(X, Y, Z) :-
    ( Y = 2 ->
        A = Y,
        W = A
    ;
        A = X,
        W = A
    ),
    Z = X + W.

:- pred s(int::in, int::in, int::out) is det.
:- pragma no_inline(s/3).

s(X, Y, Z) :-
    ( Y = 2 ->
        (
            A = Y
        &
            W = A
        )
    ;
        (
            A = X
        &
            B = X
        &
            C = Y
        ),
        W = A + B + C
    ),
    Z = X + W.
