%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module reserved.

:- interface.

:- type int(A, B, C)
    --->    f1(A)
    ;       f2(B)
    ;       f3(C).

:- type t(A, B, C)
    --->    g1(A)
    ;       g2(B)
    ;       g3(C).

:- inst unique(A, B, C)
    --->    f1(A)
    ;       f2(B)
    ;       f3(C).

:- inst u(A, B, C)
    --->    g1(A)
    ;       g2(B)
    ;       g3(C).

:- mode is(A, B, C) == (free >> u(A, B, C)).
