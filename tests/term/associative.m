:- module associative.

:- interface.

:- type	expr	--->	base ; op(expr, expr).

:- pred normal_form(expr, expr).
:- mode normal_form(in, out) is det.

:- implementation.

normal_form(F, N) :-
	( rewrite(F, F1) ->
		normal_form(F1, N)
	;
		N = F
	).

:- pred rewrite(expr, expr).
:- mode rewrite(in, out) is semidet.

rewrite(In, Out) :-
	( In = op(op(A, B), C) ->
		Out = op(A, op(B, C))
	;
		In = op(A, op(B, C)),
		Out = op(A, L),
		rewrite(op(B, C), L)
	).
