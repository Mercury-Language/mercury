	% simple test of arbitrary precision rationals.
:- module rational_test.

:- interface.

:- import_module io.

:- pred main(io.state, io.state).
:- mode main(di, uo) is det.

:- implementation.

:- import_module rational, int, integer, string, list, io.

main -->
	io.write_string(rat2s(cf2rat(root2_cf(80)))), io.nl,
	io.write_string(rat2s(cf2rat(e_cf(20)))), io.nl.

:- func rat2s(rational) = string.
rat2s(Rat) = S :-
	Num = numer(Rat),
	Den = denom(Rat),
	NS = integer.to_string(Num),
	ND = integer.to_string(Den),
	string.append_list([NS," / ",ND],S).

:- func cf2rat(list(int)) = rational.
cf2rat([]) = one.
cf2rat([N|Ns]) = rational(N,1) + rational__reciprocal(CF) :-
	CF = cf2rat(Ns).

	% Continued fraction expansion of Euler's constant `e'.
:- func e_cf(int) = list(int).
e_cf(N) = CF :-
	list.append([2,1,2],Rest,CF),
	Rest = e_aux(N,4).

:- func e_aux(int, int) = list(int).
e_aux(N,A) = List :-
	( N =< 0 ->
		List = []
	;
		List = [1,1,A|e_aux(N-1,A+2)]
	).
		


:- func root2_cf(int) = list(int).
root2_cf(N) = [1|Rest] :-
	Rest = n_of(N,2).

:- func n_of(int, T) = list(T).
n_of(N, A) = 
	( N =< 0 ->
		[]
	;
		[A|n_of(N-1,A)]
	).

