%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module operator_classname.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- typeclass +(T) where [
    pred p(T::in, io::di, io::uo) is det
].

:- implementation.

:- instance +(int) where [
    pred(p/3) is io.write_int
].

main(!IO) :-
    foo(1, !IO),
    io.nl(!IO).

:- pred foo(T::in, io::di, io::uo) is det <= +(T).

foo(X, !IO) :-
    p(X, !IO).
