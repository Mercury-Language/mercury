% vim: ts=4 sw=4 et ft=mercury

:- module fib_uint.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module uint.
:- import_module require.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials(20u, !IO).

:- pred perform_trials(uint::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    trial(N, Time, MTime),
    trace [compiletime(flag("progress")), io(!S)] (
        io.write_string("trial ", !S),
        io.write_uint(N, !S),
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
        perform_trials(N + 3u, !IO)
    ).

:- pred trial(uint::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
    benchmark_det(fib, N, Res, 1, Time),
    benchmark_det(mfib, N, MRes, 1, MTime),
    require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(uint::in, uint::out) is det.

fib(N, F) :-
    ( if N < 2u then
        F = 1u
    else
        fib(N - 1u, F1),
        fib(N - 2u, F2),
        F = F1 + F2
    ).

:- pred mfib(uint::in, uint::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
    ( if N < 2u then
        F = 1u
    else
        mfib(N - 1u, F1),
        mfib(N - 2u, F2),
        F = F1 + F2
    ).
