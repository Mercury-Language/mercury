:- module comp_gen.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.

main -->
	{ data(A, B) },
	(
		{ p(A, B) }
	->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- type foo(X) ---> f(X).

:- pred data(foo(int), foo(int)).
:- mode data(out, out) is det.

data(f(1), f(2)).

:- pred p(T, T).
:- mode p(in, in) is semidet.

p(X, X).

