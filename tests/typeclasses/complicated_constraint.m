% vim: ft=mercury ts=4 sw=4 et

:- module complicated_constraint.

:- interface.
:- import_module io.
:- import_module list.

:- typeclass printable(A) where [
	pred p(A::in, io.state::di, io.state::uo) is det
].
:- typeclass foo(A) <= printable(A) where [
	pred b(A::in) is semidet
].

:- instance printable(int).
:- instance foo(int).
:- instance printable(list(T)) <= foo(T).
:- instance foo(list(T)) <= foo(T).

:- pred main(io::di, io::uo) is det.

:- implementation.
:- import_module int.

:- instance printable(int) where [
	pred(p/3) is io.write_int
].

:- instance foo(int) where [
	pred(b/1) is foo_b
].
:- instance foo(list(T)) <= foo(T) where [
	pred(b/1) is list_b
].
:- instance printable(list(T)) <= foo(T) where [
	pred(p/3) is p_list
].

:- pred p_list(list(T), state, state) <= printable(T).
:- mode p_list(in, di, uo) is det.

p_list(Xs) -->
	list.foldl(p, Xs).

main -->
	p(42),
	io.write_string("\n"),
	p_list([1,2,3]),
	io.write_string("\n"),
	p([1,2,3]),
	io.write_string("\n"),
	blah(101),
	io.write_string("\n").

:- pred list_b(list(T)::in) is semidet <= foo(T).

list_b(List) :-
	list.map((pred(A::in, A::out) is semidet :- b(A)), List, _).

:- pred foo_b(int::in) is semidet.

foo_b(1).

% This tests complicated constraints of the form `foo(bar(T))'.

:- pred blah(T, io, io) <= (foo(list(T)), printable(list(T))).
:- mode blah(in, di, uo) is det.

blah(X) -->
	(
		% This also tests the semidet class method call mechanism
		{ b([X, X]) }
	->
		io__write_string("true\n")
	;
		io__write_string("false\n")
	),
	p([X]).
