:- module aadebug.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module int, std_util.

main -->
	( { p('a', X), test(X) } ->
		io__write_string("yes\n")
	;
		io__write_string("no\n")
	).

:- pred test(int).
:- mode test(in) is semidet.

test(_) :-
	semidet_fail.

:- pred p(character, int).
:- mode p(in, out) is nondet.

p(A, D) :-
	q(A, B),
	(
		r(B, C)
	->
		(
			s(C, D)
		;
			D = 31
		)
	;
		not(
			q(B, _)
		),
		D = 32
	).

:- pred q(character, character).
:- mode q(in, out) is nondet.

q('a', 'a').
q('a', 'b').
q('c', 'c').

:- pred r(character, int).
:- mode r(in, out) is semidet.

r('a', 10).

:- pred s(int, int).
:- mode s(in, out) is det.

s(N, 3 * N).

