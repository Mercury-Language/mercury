:- module unify_expression.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require.

:- type t
	--->	f(int, int)
	;	g(t).

main -->
	( { p(g(f(1, 2)), X) } ->
		io__write(X),
		io__nl
	;
		io__write_string("failed\n")
	).

:- pred p(t::in, t::out) is semidet.

p(X @ f(_, _), X).
p(g(X @ f(_, _)), X).
p(g(g(_)), _) :- error("p").
