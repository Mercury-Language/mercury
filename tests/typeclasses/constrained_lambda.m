%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module constrained_lambda.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.
:- import_module int.

main(!IO) :-
    list.map(
        ( pred(A::in, B::out) is det :-
            p(A, B)
        ), [1, 2], X),
    io.write_line(X, !IO).

:- typeclass foo(T) where [
    pred p(T::in, T::out) is det
].

:- instance foo(int) where [
    pred(p/2) is blah
].

:- pred blah(int::in, int::out) is det.

blah(X, X + 1).
