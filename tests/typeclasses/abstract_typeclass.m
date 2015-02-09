:- module abstract_typeclass.
:- interface.
:- import_module io, list, string.

:- typeclass foo(T).

:- instance foo(int).
:- instance foo(string).
:- instance foo(list(T)) <= foo(T).

:- pred p(T, io__state, io__state) <= foo(T).
:- mode p(in, di, uo) is det.

:- some [T] pred q(T) => foo(T).
:- 	    mode q(out) is det.

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
	(bar([H | _]) = string__append("a list, and its head is ", bar(H)))
].

p(T) -->
	io__write(T),
	io__write_strings([" is ", bar(T), ".\n"]).

:- type quux
	--->	tchok.

:- instance foo(quux) where [
	(bar(_) = "a quux")
].

q(tchok).

