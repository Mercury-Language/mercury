%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module multi_constraint_diff_tvar.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- typeclass c(T) where [
    pred p(T::in, io::di, io::uo) is det
].

:- instance c(int) where [
    pred(p/3) is io.write_int
].

:- pred foo(T1::in, T2::in, io::di, io::uo) is det <= (c(T1), c(T2)).

foo(X, Y, !IO) :-
    p(X, !IO),
    p(Y, !IO),
    io.nl(!IO).

main(!IO) :-
    foo(42, 24, !IO).
