:- module ho4.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module list.

main -->
	{
		p(1)
	->
		S1 = ""
	;
		S1 = "\\+ "
	},
	{
		p(2)
	->
		S2 = ""
	;
		S2 = "\\+ "
	},
	io__write_strings([S1, "p(1).\n", S2, "p(2).\n"]).

:- pred p(int).
:- mode p(in) is semidet.

p(N) :-
	q(r(5), M),
	M = N.

:- pred q(pred(int), int).
:- mode q(pred(out) is multi, out) is multi.

q(R, N) :-
	R(N).

:- pred r(int, int).
:- mode r(in, out) is multi.

r(A, A).
r(_, 0).

