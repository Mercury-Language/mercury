:- module fib.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking, require, int.

main(!IO) :-
	perform_trials(20, !IO),
	reset(!IO),
	perform_trials(20, !IO).

:- pred perform_trials(int::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
	trial(N, Time, MTime),
	% io__write_int(N, !IO),
	% io__write_string(": ", !IO),
	% io__write_int(Time, !IO),
	% io__write_string("ms vs ", !IO),
	% io__write_int(MTime, !IO),
	% io__write_string("ms\n", !IO),
	(
		(
			Time > 10 * MTime,
			MTime > 0	% untabled takes ten times as long
		;
			Time > 100,	% untabled takes at least 100 ms
			MTime < 1	% while tabled takes at most 1 ms
		)
	->
		io__write_string("tabling works\n", !IO)
	;
		Time > 10000	% Untabled takes at least 10 seconds
	->
		io__write_string("tabling does not appear to work\n", !IO)
	;
		% We couldn't get a measurable result with N,
		% and it looks like we can afford a bigger trial
		perform_trials(N+3, !IO)
	).

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
:- pragma memo(mfib/2).

mfib(N, F) :-
	( N < 2 ->
		F = 1
	;
		mfib(N - 1, F1),
		mfib(N - 2, F2),
		F = F1 + F2
	).

:- pred reset(io::di, io::uo) is det.

:- pragma foreign_proc("C",
	reset(IO0::di, IO::uo),
	[will_not_call_mercury, promise_pure],
"
	/* IO0, IO */
	extern void mercury__fib__reset_tables(void);
	mercury__fib__reset_tables();
").
