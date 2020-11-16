%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module implied_instance_poly.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list.

:- typeclass f(T) where [
    pred p(T::in, io::di, io::uo) is det
].

:- instance f(int) where [
    pred(p/3) is io.write_int
].

:- instance f(list(T)) <= f(T) where [
    pred(p/3) is my_write_list
].

main(!IO) :-
    foo(1, !IO),
    io.nl(!IO).

:- pred my_write_list(list(T)::in, io::di, io::uo) is det <= f(T).

my_write_list(X, !IO) :-
    io.write_list(X, ", ", p, !IO).

:- pred foo(T::in, io::di, io::uo) is det <= f(T).

foo(X, !IO) :-
    p([X, X, X], !IO).
