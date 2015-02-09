	%
	% The call in the compose section isn't assocative.
	%
:- module simple.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- import_module list, int.

main -->
	io__write_string("foldr: "),
	{ foldr([1,10,100], 0, Ans) },
	io__write(Ans),
	io__nl.

:- pred foldr(list(int), int, int).
:- mode foldr(in, in, out) is det.

foldr([], Acc, Acc).
foldr(X,Acc0,Acc) :-
	X = [H|T],
	foldr(T,Acc0,Acc1),
	Acc is H - Acc1.
