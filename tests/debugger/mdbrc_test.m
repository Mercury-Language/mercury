%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a regression test for Mantis bug #45.

:- module mdbrc_test.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

main(!IO) :-
    p(0 .. 100, L),
    io.write(L, !IO),
    nl(!IO).

:- pred p(T::in, T::out) is det.

p(A, B) :-
    B = A.
