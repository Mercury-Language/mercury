% Test the warning for infinite recursion.
:- module infinite_recursion.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.
:- import_module std_util, list.

main -->
	( { funny_append([1,2,3], [4,5,6], [5,6,7]) } ->
		main
	;
		{ loop }
	).

:- pred loop is det.

loop :-
	( semidet_succeed ->
		loop
	;
		true
	).

:- pred funny_append(list(T), list(T), list(T)).
:- mode funny_append(in, in, out) is det.

funny_append(L1, L2, L3) :-
	L1 = [], L2 = L3
	;
	L1 = [X|_Xs], L3 = [X|Zs],
	funny_append(L1, L2, Zs).	% L1 should be _Xs.
