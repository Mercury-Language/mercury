:- module neg_conj.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int.

main -->
	( { p(0) } ->
		io__write_string("yes.\n")
	;
		io__write_string("no.\n")
	).

:- pred p(int).
:- mode p(in) is semidet.

p(X) :-
	\+ (
		q(X, Y),
		r(Y)
	).

:- pred q(int, int).
:- mode q(in, out) is nondet.

q(0, 0).
q(0, 1).

:- pred r(int).
:- mode r(in) is semidet.

r(Y) :-
	Y > 1.
