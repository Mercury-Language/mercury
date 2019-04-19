%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%
%
% This is a test case for what error message the compiler generates
% when a trace goal attempts to bind a nonlocal variable.
%
%---------------------------------------------------------------------------%

:- module trace_goal_dupl_defn.

:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

:- implementation.

:- import_module int.
:- import_module string.

main(!IO) :-
    p(42, _, X),
    io.write_string(X, !IO),
    io.nl(!IO).

:- pred p(int::in, int::out, string::out) is det.

p(!N, S) :-
    trace [compiletime(flag("abc")), io(!IO)] (
        ( if is_even(!.N) then
            % M is the nonlocal we attempt to bind. It is nonlocal here
            % because it also appears in the second trace goal.
            M = !.N,
            io.write_string("<abc>", !IO),
            io.write_int(M, !IO),
            io.write_string("<abc>\n", !IO)
        else
            true
        )
    ),
    S = "xx",
    trace [compiletime(flag("abc")), io(!IO)] (
        ( if is_even(!.N) then
            M = !.N,
            io.write_string("<abc>", !IO),
            io.write_int(M, !IO),
            io.write_string("<abc>\n", !IO)
        else
            true
        )
    ).

:- pred is_even(int::in) is semidet.

is_even(N) :-
    N mod 2 = 0.
