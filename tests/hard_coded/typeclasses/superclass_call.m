:- module superclass_call.

:- interface.

:- pred main(io__state::di, io__state::uo) is det.

:- import_module io.

:- implementation.

:- typeclass printable(A) where [
	pred p(A::in, io__state::di, io__state::uo) is det
].

:- typeclass foo(A) <= printable(A) where [
	pred b(A::in) is semidet
].

:- instance printable(int) where [
	pred(p/3) is io__write_int
].

:- instance foo(int) where [
	pred(b/1) is foo_b
].

main -->
	p(42), 
	io__write_string("\n"),
	blah(101),
	io__write_string("\n").


:- pred foo_b(int::in) is semidet.
foo_b(1).

:- pred blah(T, io__state, io__state) <= foo(T).
:- mode blah(in, di, uo) is det.

blah(X) -->
	(
		% This also tests the semidet class method call mechanism
		{ b(X) }
	->
		io__write_string("true\n")
	;
		io__write_string("false\n")
	),

	% at this call to the superclass method, the printable typeclass_info
	% gets extracted from the foo typeclass_info.
	p(X).


