% vim: ft=mercury ts=4 sts=4 sw=4 et
%
% This is a regression test for Mantis bug #371.

:- module string_format_in_instance.
:- interface.

:- import_module list.

:- type foo
    --->    foo(int).

:- typeclass cons(T) where [
    pred cons(T::in, list(string)::in, list(string)::out) is det
].

:- instance cons(foo).

:- implementation.

:- import_module string.

:- instance cons(foo) where [
    cons(foo(X), A, [S | A]) :-
        string.format("%d", [i(X)], S)
].
