	%
	% Highoder functions cannot use accumulator recursion because we
	% don't know anything about the assocativity of P.
	%
:- module highorder.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module int, list.

main -->
	io__write_string("foldr: "),
	{ highorder__foldr(minus, [1,10,100], 0, ListA) },
	io__write(ListA),
	io__nl.
	
:- pred minus(int::in, int::in, int::out) is det.
 
minus(A, B, C) :-
	C is A - B.

	% highorder__foldr(Pred, List, Start, End) calls Pred with each
	% element of List (working right-to-left) and an accumulator
	% (with the initial value of Start), and returns the final
	% value in End.
:- pred highorder__foldr(pred(X, Y, Y), list(X), Y, Y).
:- mode highorder__foldr(pred(in, in, out) is det, in, in, out) is det.

highorder__foldr(_, [], Acc, Acc).
highorder__foldr(P, [H|T], Acc0, Acc) :-
	highorder__foldr(P, T, Acc0, Acc1),
	call(P, H, Acc1, Acc).
