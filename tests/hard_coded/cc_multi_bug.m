:- module cc_multi_bug.

:- interface.

:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module io.

:- type nat ---> zero ; s(nat).

:- pred plus(nat,nat,nat).
:- mode plus(out,out,in) is multi.
:- mode plus(out,out,in) is cc_multi.

plus(zero,zero,zero).
plus(zero,s(N),s(N)).
plus(s(N),zero,s(N)).
plus(s(N1),s(N2),s(N3)):-
	plus(N1,s(N2),N3).


main -->
        {plus(N1,N2,s(zero))},
	print(N1), nl,
	print(N2), nl.

