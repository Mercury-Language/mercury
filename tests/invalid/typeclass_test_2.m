:- module typeclass_test_2.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

main -->
	io__write_int(
		type_num(42)		% type error here
					% (due to the syntax error below)
	),
	nl.

:- typeclass numbered_type(T) where [
	func type_num(T) = int
].

:- instance numbered_type(int) where [
	type_num/0 is foo_type_num	% syntax error here
].

foo_type_num(_) = 42.

