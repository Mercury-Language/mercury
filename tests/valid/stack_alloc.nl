:- module stack_alloc.

:- pred in(int::in) is semidet.
:- pred out(int::out) is det.

:- pred p is semidet.

p :-
	(
		out(X),
		out(Y),
		p,
		in(X),
		in(Y)
	;
		out(A),
		out(B),
		p,
		in(A),
		in(B)
	).

