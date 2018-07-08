% vim: ts=4 sw=4 et ft=mercury

:- module fib_stats.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module list.
:- import_module string.
:- import_module table_statistics.

:- pragma require_feature_set([memo]).

main(!IO) :-
    test(1, 10, !IO),
    test(11, 20, !IO).

:- pred test(int::in, int::in, io::di, io::uo) is det.

test(Cur, Max, !IO) :-
    test_loop(Cur, Max, !IO),
    table_statistics_for_mfib_2(Info, !IO),
    Info = proc_table_statistics(CallTableStatsCurPrev,
        _AnswerTableStatsCurPrev),
    CallTableStatsCurPrev = table_stats_curr_prev(CallTableStatsCur,
        CallTableStatsPrev),
    io.nl(!IO),
    io.format("Previous call table for mfib, test %d to %d:\n",
        [i(Cur), i(Max)], !IO),
    write_table_stats(CallTableStatsPrev, !IO),
    io.nl(!IO),
    io.format("Current call table for mfib, test %d to %d:\n",
        [i(Cur), i(Max)], !IO),
    write_table_stats(CallTableStatsCur, !IO),
    io.nl(!IO),
    io.format("Call table difference (curr - prev) for mfib, test %d to %d:\n",
        [i(Cur), i(Max)], !IO),
    CallTableStatsDiff =
        table_stats_difference(CallTableStatsCur, CallTableStatsPrev),
    write_table_stats(CallTableStatsDiff, !IO),
    io.nl(!IO).

:- pred test_loop(int::in, int::in, io::di, io::uo) is det.

test_loop(Cur, Max, !IO) :-
    ( if Cur =< Max then
        mfib(Cur, Fib),
        io.format("fib(%d): %d\n", [i(Cur), i(Fib)], !IO),
        test_loop(Cur + 1, Max, !IO)
    else
        true
    ).

:- pred mfib(int::in, int::out) is det.
:- pragma memo(mfib/2, [statistics]).

mfib(N, F) :-
    ( if N < 2 then
        F = 1
    else
        mfib(N - 1, F1),
        mfib(N - 2, F2),
        F = F1 + F2
    ).
