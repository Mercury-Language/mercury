:- module implied_instance.

:- interface.

:- pred main(io__state::di, io__state::uo) is det.

:- import_module io.

:- implementation.

:- import_module list.

:- typeclass printable(A) where [
	pred p(A::in, io__state::di, io__state::uo) is det
].

:- instance printable(int) where [
	pred(p/3) is io__write_int
].

:- instance printable(list(T)) <= printable(T) where [
	pred(p/3) is my_write_list
].

main -->
	p(2),
	io__write_string("\n"),
	p([42, 24, 1, 2, 3]),
	io__write_string("\n").


:- pred my_write_list(list(T), io__state, io__state) <= printable(T).
:- mode my_write_list(in, di, uo) is det.

my_write_list([]) --> 
	io__write_string("[]").
my_write_list([X|Xs]) --> 
	io__write_string("[\n"),
	my_write_list_2([X|Xs]),
	io__write_string("]").

:- pred my_write_list_2(list(T), io__state, io__state) <= printable(T).
:- mode my_write_list_2(in, di, uo) is det.

my_write_list_2([]) --> [].
my_write_list_2([X|Xs]) --> 
	p(X),
	io__write_string("\n"),
	my_write_list_2(Xs).

