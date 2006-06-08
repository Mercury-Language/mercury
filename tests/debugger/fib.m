:- module fib.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module benchmarking, require, int.

main(!IO) :-
	perform_trial(23, !IO),
	table_reset_for_mfib_2(!IO),
	perform_trial(23, !IO).

:- pred perform_trial(int::in, io::di, io::uo) is det.

perform_trial(N, !IO) :-
	trial(N, _Time, _MTime),
	io__write_string("got same results\n", !IO).

:- pred trial(int::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
	benchmark_det(fib, N, Res, 1, Time),
	benchmark_det(mfib, N, MRes, 1, MTime),
	require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
	( N < 2 ->
		F = 1
	;
		fib(N - 1, F1),
		fib(N - 2, F2),
		F = F1 + F2
	).

:- pred mfib(int::in, int::out) is det.
:- pragma memo(mfib/2, [allow_reset]).

mfib(N, F) :-
	( N < 2 ->
		F = 1
	;
		mfib(N - 1, F1),
		mfib(N - 2, F2),
		F = F1 + F2
	).
