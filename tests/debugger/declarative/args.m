:- module args.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is cc_multi.
:- implementation.
:- import_module std_util, int.

main -->
	(
		{ p(1, X, 3, Y, 5) },
		{ my_fail }
	->
		io__write_int(X),
		io__nl,
		io__write_int(Y),
		io__nl
	;
		io__write_string("no.\n")
	).

:- pred p(int, int, int, int, int).
:- mode p(in, out, in, out, in) is nondet.

p(A, A + (B * C), B, (A + B) * C, C) :-
	my_succeed.
p(A, A - B, B, C - B, C) :-
	my_succeed.

	% The purpose of the following two procedures is to ensure
	% that the test cases work consistently in both debugging
	% and non-debugging grades.
	%
:- pred my_succeed is semidet.
my_succeed :-
	semidet_succeed.

:- pred my_fail is semidet.
my_fail :-
	semidet_fail.

