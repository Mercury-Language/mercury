% vim: ts=4 sw=4 expandtab ft=mercury 

% This program used to get compiler errors, because the table statistics
% and reset predicates that the pragma memo asks for were not generated
% in Erlang grades. This was Mantis bug #368.

:- module table_aux_preds_erlang.
:- interface.

:- import_module io.

:- pred test(io::di, io::uo) is det.

:- implementation.

:- import_module maybe.
:- import_module table_statistics.

test(!IO) :-
    table_reset_for_p_2(!IO),
    table_statistics_for_p_2(ProcStats, !IO),
    ProcStats =
        proc_table_statistics(CallStatsCurrPrev, MaybeAnswerStatsCurrPrev),
    CallStatsCurrPrev = table_stats_curr_prev(CallStats, _),
    write_table_stats(CallStats, !IO),
    (
        MaybeAnswerStatsCurrPrev = no
    ;
        MaybeAnswerStatsCurrPrev = yes(AnswerStatsCurrPrev),
        AnswerStatsCurrPrev = table_stats_curr_prev(AnswerStats, _),
        write_table_stats(AnswerStats, !IO)
    ).

:- pred p(int::in, int::out) is det.
:- pragma memo(p/2, [allow_reset, statistics]).

p(X, X).
