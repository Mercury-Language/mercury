:- module func_call.
:- interface.
:- import_module io.
:- pred main(io__state::di, io__state::uo) is det.
:- implementation.
:- import_module library_forwarding.

main -->
	io__write_int(fib(6)),
	io__nl.

:- func fib(int) = int.

fib(N) =
	(
		N =< 1
	->
		1
	;
		fib(N - 1) +
		  fib(N - 3)	% Oops.
	).

