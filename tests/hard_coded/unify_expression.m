:- module unify_expression.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module require, std_util.

:- type t
	--->	f(int, int)
	;	g(t).

main -->
	( { p(g(f(1, 2)), X) } ->
		io__write(X),
		io__nl
	;
		io__write_string("Error: p failed\n")
	),
	( { q(1, 2) } ->
		print("Error: q succeeded"), nl
	;
		print("q failed (as expected)"), nl
	),

	( { r(1, 2) } ->
		print("Error: r succeeded"), nl
	;
		print("r failed (as expected)"), nl
	).

:- pred p(t::in, t::out) is semidet.

p(X @ f(_, _), X).
p(g(X @ f(_, _)), X).

:- pred q(int::in, int::in) is semidet.
q(X, X @ g(_, _)).

:- pred r(int::in, int::in) is semidet.
r(X, X @ g(1, 2)).

:- func g(int, int) = int.
:- mode g(in, in) = out is semidet.
:- mode g(out, out) = in is semidet.
g(1, 2) = X :-
	( semidet_succeed ->
		error("g called")
	;
		X = 3
	).

