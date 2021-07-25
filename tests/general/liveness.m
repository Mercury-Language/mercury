%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for a bug in liveness.m.
%
% When stuffing liveness residues after goals, which we do for variables
% which are nondet-live in one arm of a branched goal but not in the other,
% make sure that we do not add post-births for variables that are already live
% at the end of the goal that we are doing the stuffing into. (If we do that,
% then the compiler assumes that they have become automagically live, and
% so just assumes they are in some random register.)

:- module liveness.
:- interface.
:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module bool.
:- import_module int.

main(!IO) :-
    p1(X1), io.write_int(X1, !IO), io.nl(!IO),
    p2(X2), io.write_int(X2, !IO), io.nl(!IO),
    p3(X3), io.write_int(X3, !IO), io.nl(!IO).

:- pred p1(int::out) is multi.

p1(X) :-
    q(FindMe),
    ( if u(41, 42, 43, 44, 45) then
        Z = 1
    else
        ( r(Z)
        ; s(FindMe, Z)
        )
    ),
    t(FindMe, Z, X).

:- pred p2(int::out) is multi.

p2(X) :-
    q(Y2),
    (
        ( if u(41, 42, 43, 44, 45) then
            Z = 1
        else
            Z = 111
        )
    ;
        ( r(Z)
        ; s(Y2, Z)
        )
    ),
    t(Y2, Z, X).

:- pred p3(int::out) is multi.

p3(X) :-
    q(Y3),
    v(Bool),
    (
        Bool = yes,
        ( if u(41, 42, 43, 44, 45) then
            Z = 1
        else
            Z = 111
        )
    ;
        Bool = no,
        ( r(Z)
        ; s(Y3, Z)
        )
    ),
    t(Y3, Z, X).

:- pred q(int::out) is det.
q(2).

:- pred r(int::out) is det.
r(3).

:- pred s(int::in, int::out) is det.
s(X, Y4) :-
    Y4 = X + 10.

:- pred t(int::in, int::in, int::out) is det.
t(X, Y5, Z) :-
    Z = X * 100 + Y5.

:- pred u(int::in, int::in, int::in, int::in, int::in) is semidet.
u(A, B, C, D, E) :-
    Sum = A + B + C + D + E,
    Sum > 200.

:- pred v(bool::out) is det.
v(yes).
