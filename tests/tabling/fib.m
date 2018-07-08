% vim: ts=4 sw=4 et ft=mercury

:- module fib.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module require.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials(20, !IO).

:- pred perform_trials(int::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    trial(N, Time, MTime),
    trace [compiletime(flag("progress")), io(!S)] (
        io.write_string("trial ", !S),
        io.write_int(N, !S),
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
        perform_trials(N+3, !IO)
    ).

:- pred trial(int::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
    benchmark_det(fib, N, Res, 1, Time),
    benchmark_det(mfib, N, MRes, 1, MTime),
    require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(int::in, int::out) is det.

fib(N, F) :-
    ( if N < 2 then
        F = 1
    else
        fib(N - 1, F1),
        fib(N - 2, F2),
        F = F1 + F2
    ).

:- pred mfib(int::in, int::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
    ( if N < 2 then
        F = 1
    else
        mfib(N - 1, F1),
        mfib(N - 2, F2),
        F = F1 + F2
    ).
