% Test tracking the origin of a sub-term from a failed if-then-else condition.
:- module failed_cond.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

main(!IO) :-
	p(X),
	write(X, !IO),
	nl(!IO).

:- type t ---> a ; b ; c.

:- pred p(t::out) is det.

p(Y) :- X = c, q(X, Y).

:- pred q(t::in, t::out) is det.

q(X, Y) :-
	(
		r(X)
	->
		Y = a
	;
		Y = b
	).

:- pred r(t::in) is semidet.

r(a).
