:- module aadebug.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int.

main -->
	( { p(1, -1) } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred p(int, int).
:- mode p(in, out) is nondet.

p(A, D) :-
	q(A, B),
	(
		r(B, C)
	->
		(
			s(C, D)
		;
			D = 1
		)
	;
		not q(B, _),
		D = 2
	).

:- pred q(int, int).
:- mode q(in, out) is nondet.

q(1, 1).
q(1, 2).
q(2, 4).

:- pred r(int, int).
:- mode r(in, out) is semidet.

r(1, 9).

:- pred s(int, int).
:- mode s(in, out) is det.

s(A, B) :-
	B = A * 3.

