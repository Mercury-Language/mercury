:- module shallow2.

:- interface.

:- import_module list.

:- pred safe(list(int)).
:- mode safe(in) is semidet.

:- implementation.

:- import_module int.

safe([]).
safe([N|L]) :-
	nodiag(N, 1, L),
	safe(L).

:- pred nodiag(int, int, list(int)).
:- mode nodiag(in, in, in) is semidet.

nodiag(_, _, []).
nodiag(B, D, [N|L]) :-
	NmB is N - B,
	BmN is B - N,
	( D = NmB ->
		fail
	; D = BmN ->
		fail
	;
		true
	),
	D1 is D + 1,
	nodiag(B, D1, L).
