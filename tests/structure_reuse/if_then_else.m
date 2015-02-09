%-----------------------------------------------------------------------------%
% A regression test.
% We test that the determination of possible cons_ids a cell can have is
% correct for branched goal structures, in this case the if_then_else.
%-----------------------------------------------------------------------------%
:- module if_then_else.

:- interface.

:- import_module io.

:- pred main(io__state::di, io__state::uo) is det.

:- implementation.

:- type t
	--->	empty
	;	one(int)
	;	two(int, int).

main -->
	{ X = two(1, 2) },
	{ p(X, Y) },
	io__write(Y),
	io__nl.

:- pred p(t::in, t::out) is det.

p(X, Y) :-
	( X = empty ->
		Y = X
	;
		(
			X = two(A, B),
			Y = two(B, A)
		;
			X = one(A),
			Y = one(A)
		;
			X = empty,
			Y = empty
		)
	).
