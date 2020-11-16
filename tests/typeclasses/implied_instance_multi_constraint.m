%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module implied_instance_multi_constraint.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- typeclass a(T) where [
    pred a(T::in, io::di, io::uo) is det
].
:- typeclass b(T) where [
    pred b(T::in, io::di, io::uo) is det
].
:- typeclass c(T) where [
    pred c(T::in, io::di, io::uo) is det
].
:- typeclass d(T) where [
    pred d(T::in, io::di, io::uo) is det
].

:- type foo(A, B, C)
    --->    foo(A, B, C).

:- instance a(int) where [
    pred(a/3) is int_a
].

:- instance b(int) where [
    pred(b/3) is int_b
].

:- instance c(int) where [
    pred(c/3) is int_c
].

:- instance d(foo(A, B, C)) <= (a(A), b(B), c(C)) where [
    pred(d/3) is my_d
].

:- pred my_d(foo(A, B, C)::in, io::di, io::uo) is det <= (a(A), b(B), c(C)).

my_d(foo(A, B, C), !IO) :-
    a(A, !IO),
    b(B, !IO),
    c(C, !IO).

:- pred int_a(int::in, io::di, io::uo) is det.

int_a(_, !IO) :-
    io.write_string("A\n", !IO).

:- pred int_b(int::in, io::di, io::uo) is det.

int_b(_, !IO) :-
    io.write_string("B\n", !IO).

:- pred int_c(int::in, io::di, io::uo) is det.

int_c(_, !IO) :-
    io.write_string("C\n", !IO).

main(!IO) :-
    d(foo(7, 42, 69), !IO).
