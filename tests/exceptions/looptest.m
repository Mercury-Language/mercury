% This module is a regression test;
% it tests that we do correct tail recursion optimization
% for procedures with output arguments and determinism
% of `erroneous' or `failure', such as loop//1 below.

% This test is designed so that if the compiler doesn't
% do tail recursion optimization, then the program will
% overflow the limit on stack size.

:- module looptest.
:- interface.
:- import_module io.

:- pred main(io__state::di, io__state::uo) is cc_multi.

:- implementation.
:- import_module int, string, exception.

main --> 
	{ try(do_loop, R) },
	io__print(R), nl.

:- mode do_loop(out) is det.
do_loop(X) :-
	loop(100000000, 42, X).
	
loop(N) -->
	( { N = 0 } ->
		{ throw("finished") }
	;
		loop(N - 1)
	).

