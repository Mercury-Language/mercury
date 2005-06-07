:- module fast_loose.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module require.

main(!IO) :-
	perform_trials(80, !IO).

:- pred perform_trials(int::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
	trial(N, STime, FLTime),
	% io__write_int(N, !IO),
	% io__write_string(": ", !IO),
	% io__write_int(STime, !IO),
	% io__write_string("ms vs ", !IO),
	% io__write_int(FLTime, !IO),
	% io__write_string("ms\n", !IO),
	(
		(
			STime > 10 * FLTime,
			FLTime > 0	% strict takes ten times as long
		;
			STime > 100,	% strict takes at least 100 ms
			FLTime < 1	% while fast_loose takes at most 1 ms
		)
	->
		io__write_string("fast_loose works\n", !IO)
	;
		STime > 10000	% Strict takes at least 10 seconds
	->
		io__write_string("fast_loose does not appear to work\n", !IO)
	;
		% We couldn't get a measurable result with N,
		% and it looks like we can afford a bigger trial.
		perform_trials(N+5, !IO)
	).

:- pred trial(int::in, int::out, int::out) is cc_multi.

trial(N, STime, FLTime) :-
	benchmark_det(strict, N, SRes, 1, STime),
	benchmark_det(fast_loose, N, FLRes, 1, FLTime),
	require(unify(SRes, FLRes), "tabling produces wrong answer").

:- pred strict(int::in, int::out) is det.

strict(N, R) :-
	strict_sum(iota(N), R).

:- pred strict_sum(list(int)::in, int::out) is det.
:- pragma memo(strict_sum/2).

strict_sum([], 0).
strict_sum([H | T], H + TS) :-
	strict_sum(T, TS).

:- pred fast_loose(int::in, int::out) is det.

fast_loose(N, R) :-
	fast_loose_sum(iota(N), R).

:- pred fast_loose_sum(list(int)::in, int::out) is det.
:- pragma fast_loose_memo(fast_loose_sum/2).

fast_loose_sum([], 0).
fast_loose_sum([H | T], H + TS) :-
	fast_loose_sum(T, TS).

:- func iota(int) = list(int).

iota(N) = 
	( N =< 0 ->
		[]
	;
		[N | iota(N - 1)]
	).
