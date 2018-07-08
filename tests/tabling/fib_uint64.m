% vim: ts=4 sw=4 et ft=mercury

:- module fib_uint64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module uint64.
:- import_module require.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials(20u64, !IO).

:- pred perform_trials(uint64::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    trial(N, Time, MTime),
    trace [compiletime(flag("progress")), io(!S)] (
        io.write_string("trial ", !S),
        io.write_uint64(N, !S),
        io.write_string(": ", !S),
        io.write_int(Time, !S),
        io.write_string("ms nonmemoed vs ", !S),
        io.write_int(MTime, !S),
        io.write_string("ms memoed\n", !S)
    ),
    ( if
        (
            Time > 10 * MTime,
            MTime > 0   % untabled takes ten times as long
        ;
            Time > 100, % untabled takes at least 100 ms
            MTime < 1   % while tabled takes at most 1 ms
        )
    then
        io.write_string("tabling works\n", !IO)
    else if
        Time > 10000    % Untabled takes at least 10 seconds
    then
        io.write_string("tabling does not appear to work\n", !IO)
    else
        % We couldn't get a measurable result with N,
        % and it looks like we can afford a bigger trial
        perform_trials(N + 3u64, !IO)
    ).

:- pred trial(uint64::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
    benchmark_det(fib, N, Res, 1, Time),
    benchmark_det(mfib, N, MRes, 1, MTime),
    require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(uint64::in, uint64::out) is det.

fib(N, F) :-
    ( if N < 2u64 then
        F = 1u64
    else
        fib(N - 1u64, F1),
        fib(N - 2u64, F2),
        F = F1 + F2
    ).

:- pred mfib(uint64::in, uint64::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
    ( if N < 2u64 then
        F = 1u64
    else
        mfib(N - 1u64, F1),
        mfib(N - 2u64, F2),
        F = F1 + F2
    ).
