%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module abstract_typeclass.
:- interface.

:- import_module io.
:- import_module list.
:- import_module string.

:- typeclass foo(T).

:- instance foo(int).
:- instance foo(string).
:- instance foo(list(T)) <= foo(T).

:- pred p(T, io, io) <= foo(T).
:- mode p(in, di, uo) is det.

:- some [T] pred q(T) => foo(T).
:-          mode q(out) is det.

:- implementation.

:- typeclass foo(T) where [
    func bar(T) = string
].

:- instance foo(int) where [
    (bar(_) = "an integer")
].

:- instance foo(string) where [
    (bar(_) = "a string")
].

:- instance foo(list(T)) <= foo(T) where [
    (bar([]) = "an empty list"),
    (bar([H | _]) = string.append("a list, and its head is ", bar(H)))
].

p(T, !IO) :-
    io.write(T, !IO),
    io.write_strings([" is ", bar(T), ".\n"], !IO).

:- type quux
    --->    tchok.

:- instance foo(quux) where [
    (bar(_) = "a quux")
].

q(tchok).
