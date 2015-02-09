:- module remember_modes.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module list, int.

:- pred p(int, int).
:- mode p(in, in) is semidet.
:- mode p(in, out) is semidet.
:- mode p(out, in) is semidet.
:- mode p(out, out) is det.

p(1, 2).

:- pred q(int::in, int::out, int::out, int::out, int::out) is semidet.

q(V, W, X, Y, Z) :-
	p(V, 2),
	p(W, 2),
	p(1, X),
	p(Y, Z).

main(!IO) :- 
	(
		q(1, W, X, Y, Z) 
	->
		write(W, !IO), nl(!IO),
		write(X, !IO), nl(!IO), 
		write(Y, !IO), nl(!IO), 
		write(Z, !IO), nl(!IO)
	;
		write_string("failed\n", !IO)
	).
