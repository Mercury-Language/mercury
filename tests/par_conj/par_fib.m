% vim: ts=4 sw=4 et ft=mercury

:- module par_fib.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module require.
:- import_module string.

main(!IO) :-
	perform_trial(35, Res),
    io.write_int(Res, !IO),
    io.nl(!IO).

:- pred perform_trial(int::in, int::out) is cc_multi.

perform_trial(N, Res) :-
	trial(N, Res, SeqTime, ParTime),
    trace [compile_time(flag("show_times")), io(!IO)] (
        io.format("fib(%d): sequential %d vs parallel %d (user times)\n",
            [i(N), i(SeqTime), i(ParTime)], !IO)
    ).

:- pred trial(int::in, int::out, int::out, int::out) is cc_multi.

trial(N, SeqRes, SeqTime, ParTime) :-
	benchmark_det(seq_fib, N, SeqRes, 1, SeqTime),
	benchmark_det(par_fib, N, ParRes, 1, ParTime),
	require(unify(SeqRes, ParRes), "parallelism produces wrong answer").

:- pred seq_fib(int::in, int::out) is det.

seq_fib(N, F) :-
	( N < 2 ->
		F = 1
	;
		seq_fib(N - 1, F1),
		seq_fib(N - 2, F2),
		F = F1 + F2
	).

:- pred par_fib(int::in, int::out) is det.

par_fib(N, F) :-
	( N < 2 ->
		F = 1
	;
		( par_fib(N - 1, F1)
		& par_fib(N - 2, F2)
		),
		F = F1 + F2
	).
