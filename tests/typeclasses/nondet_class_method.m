%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module nondet_class_method.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module int.

:- typeclass c(T) where [
    pred a(T::out) is multi
].

:- instance c(int) where [
    pred(a/1) is foo
].

main(!IO) :-
    ( if
        b(X),
        X > 3
    then
        io.write_int(X, !IO)
    else
        io.write_string("failed", !IO)
    ),
    io.nl(!IO).

:- pred b(T::out) is multi <= c(T).
:- pragma no_inline(b/1).

b(X) :-
    a(X).

:- pred foo(int::out) is multi.

foo(1).
foo(2).
foo(3).
foo(4).
foo(5).
foo(6).
