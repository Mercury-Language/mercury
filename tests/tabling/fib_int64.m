% vim: ts=4 sw=4 et ft=mercury

:- module fib_int64.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module int64.
:- import_module require.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials(20i64, !IO).

:- pred perform_trials(int64::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    trial(N, Time, MTime),
    trace [compiletime(flag("progress")), io(!S)] (
        io.write_string("trial ", !S),
        io.write_int64(N, !S),
        io.write_string(": ", !S),
        io.write_int(Time, !S),
        io.write_string("ms nonmemoed vs ", !S),
        io.write_int(MTime, !S),
        io.write_string("ms memoed\n", !S)
    ),
    (
        (
            Time > 10 * MTime,
            MTime > 0   % untabled takes ten times as long
        ;
            Time > 100, % untabled takes at least 100 ms
            MTime < 1   % while tabled takes at most 1 ms
        )
    ->
        io.write_string("tabling works\n", !IO)
    ;
        Time > 10000    % Untabled takes at least 10 seconds
    ->
        io.write_string("tabling does not appear to work\n", !IO)
    ;
        % We couldn't get a measurable result with N,
        % and it looks like we can afford a bigger trial
        perform_trials(N + 3i64, !IO)
    ).

:- pred trial(int64::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
    benchmark_det(fib, N, Res, 1, Time),
    benchmark_det(mfib, N, MRes, 1, MTime),
    require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(int64::in, int64::out) is det.

fib(N, F) :-
    ( N < 2i64 ->
        F = 1i64
    ;
        fib(N - 1i64, F1),
        fib(N - 2i64, F2),
        F = F1 + F2
    ).

:- pred mfib(int64::in, int64::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
    ( N < 2i64 ->
        F = 1i64
    ;
        mfib(N - 1i64, F1),
        mfib(N - 2i64, F2),
        F = F1 + F2
    ).
