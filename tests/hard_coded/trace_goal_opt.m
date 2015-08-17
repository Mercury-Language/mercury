%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
% Check that trace goal conditions are written to .opt files.

:- module trace_goal_opt.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module trace_goal_opt_2.

main(!IO) :-
    % We do not meet the conditions of the trace goal in this predicate
    % so it should not execute.
    require_lt(2, 1),
    io.write_string("done.\n", !IO).
