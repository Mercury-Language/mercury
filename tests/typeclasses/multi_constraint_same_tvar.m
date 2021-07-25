%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multi_constraint_same_tvar.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- typeclass c1(T) where [
    pred p1(T::in, io::di, io::uo) is det
].

:- instance c1(int) where [
    pred(p1/3) is io.write_int
].

:- typeclass c2(T) where [
    pred p2(T::in, io::di, io::uo) is det
].

:- instance c2(int) where [
    pred(p2/3) is io.write_int
].

main(!IO) :-
    foo(42, !IO).

:- pred foo(T::in, io::di, io::uo) is det <= (c1(T), c2(T)).

foo(X, !IO) :-
    p1(X, !IO),
    p2(X, !IO),
    io.nl(!IO).
