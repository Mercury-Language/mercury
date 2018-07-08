%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fib_float.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module float.
:- import_module int.
:- import_module require.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials(20.0, !IO).

:- pred perform_trials(float::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    trial(N, Time, MTime),
    % io.write_float(N, !IO),
    % io.write_string(": ", !IO),
    % io.write_int(Time, !IO),
    % io.write_string("ms vs ", !IO),
    % io.write_int(MTime, !IO),
    % io.write_string("ms\n", !IO),
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
        Time > 10000        % untabled takes at least 10 seconds
    then
        io.write_string("tabling does not appear to work\n", !IO)
    else
        % We couldn't get a measurable result with N,
        % and it looks like we can afford a bigger trial
        perform_trials(N + 3.0, !IO)
    ).

:- pred trial(float::in, int::out, int::out) is cc_multi.

trial(N, Time, MTime) :-
    benchmark_det(fib, N, Res, 1, Time),
    benchmark_det(mfib, N, MRes, 1, MTime),
    require(unify(Res, MRes), "tabling produces wrong answer").

:- pred fib(float::in, float::out) is det.

fib(N, F) :-
    ( if N < 2.0 then
        F = 1.0
    else
        fib(N - 1.0, F1),
        fib(N - 2.0, F2),
        F = F1 + F2
    ).

:- pred mfib(float::in, float::out) is det.
:- pragma memo(mfib/2).

mfib(N, F) :-
    ( if N < 2.0 then
        F = 1.0
    else
        mfib(N - 1.0, F1),
        mfib(N - 2.0, F2),
        F = F1 + F2
    ).
