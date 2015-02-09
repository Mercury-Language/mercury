:- module ho3.
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
	q(0, Q),
	Q(N).

:- pred q(int, pred(int)).
:- mode q(in, out(pred(in) is semidet)) is multi.

q(N, x(N)).
q(N, y(N)).

:- pred x(int, int).
:- mode x(in, in) is semidet.

x(A, A).

:- pred y(int, int).
:- mode y(in, in) is semidet.

y(_, -1).

