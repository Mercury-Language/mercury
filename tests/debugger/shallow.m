:- module shallow.

:- interface.

:- import_module io.

:- pred main(io__state, io__state).
:- mode main(di, uo) is cc_multi.

:- implementation.

:- import_module shallow2.
:- import_module list, int.

main -->
	( { data(Data), queen(Data, Out) } ->
		print_list(Out)
	;
		io__write_string("No solution\n")
	).

:- pred data(list(int)).
:- mode data(out) is det.

:- pred queen(list(int), list(int)).
:- mode queen(in, out) is nondet.

:- pred qperm(list(T), list(T)).
:- mode qperm(in, out) is nondet.

:- pred qdelete(T, list(T), list(T)).
:- mode qdelete(out, in, out) is nondet.

data([1,2,3,4,5]).

queen(Data, Out) :-
	qperm(Data, Out),
	safe(Out).

qperm([], []).
qperm([X|Y], K) :-
	qdelete(U, [X|Y], Z),
	K = [U|V],
	qperm(Z, V).

qdelete(A, [A|L], L).
qdelete(X, [A|Z], [A|R]) :-
	qdelete(X, Z, R).

:- pred print_list(list(int), io__state, io__state).
:- mode print_list(in, di, uo) is det.

print_list(Xs) -->
	(
		{ Xs = [] }
	->
		io__write_string("[]\n")
	;
		io__write_string("["),
		print_list_2(Xs),
		io__write_string("]\n")
	).

:- pred print_list_2(list(int), io__state, io__state).
:- mode print_list_2(in, di, uo) is det.

print_list_2([]) --> [].
print_list_2([X|Xs]) --> 
	io__write_int(X),
	(
		{ Xs = [] }
	->
		[]
	;
		io__write_string(", "),
		print_list_2(Xs)
	).
