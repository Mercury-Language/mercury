%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module fast_loose.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is cc_multi.

:- implementation.

:- import_module benchmarking.
:- import_module int.
:- import_module list.
:- import_module require.

:- pragma require_feature_set([memo]).

main(!IO) :-
    perform_trials(80, !IO).

:- pred perform_trials(int::in, io::di, io::uo) is cc_multi.

perform_trials(N, !IO) :-
    table_reset_for_strict_sum_2(!IO),
    table_reset_for_fast_loose_sum_2(!IO),
    trial(N, STime, FLTime),
    % io.write_int(N, !IO),
    % io.write_string(": ", !IO),
    % io.write_int(STime, !IO),
    % io.write_string("ms vs ", !IO),
    % io.write_int(FLTime, !IO),
    % io.write_string("ms\n", !IO),
    ( if
        (
            STime > 10 * FLTime,
            FLTime > 0  % strict takes ten times as long
        ;
            STime > 100,    % strict takes at least 100 ms
            FLTime < 1  % while fast_loose takes at most 1 ms
        )
    then
        io.write_string("fast_loose works\n", !IO)
    else if
        STime > 10000   % Strict takes at least 10 seconds
    then
        io.write_string("fast_loose does not appear to work\n", !IO)
    else
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
:- pragma memo(strict_sum/2, [allow_reset]).

strict_sum([], 0).
strict_sum([H | T], H + TS) :-
    strict_sum(T, TS).

:- pred fast_loose(int::in, int::out) is det.

fast_loose(N, R) :-
    fast_loose_sum(iota(N), R).

:- pred fast_loose_sum(list(int)::in, int::out) is det.
:- pragma memo(fast_loose_sum/2, [fast_loose, allow_reset]).

fast_loose_sum([], 0).
fast_loose_sum([H | T], H + TS) :-
    fast_loose_sum(T, TS).

:- func iota(int) = list(int).

iota(N) =
    ( if N =< 0 then
        []
    else
        [N | iota(N - 1)]
    ).
