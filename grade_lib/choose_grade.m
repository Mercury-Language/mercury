%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module choose_grade.
:- interface.

:- import_module io.

:- pred main(io::di, io::uo) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module grade_setup.
:- import_module grade_solver.
:- import_module grade_state.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    setup_solver_info(SolverInfo0),
    io.command_line_arguments(Args, !IO),
    process_arguments(Args, cord.init, BadArgLinesCord,
        SolverInfo0, SolverInfo1),
    BadArgLines = cord.list(BadArgLinesCord),
    (
        BadArgLines = [_ | _],
        io.write_string("unrecognized arguments:\n", !IO),
        list.foldl(io.write_string, BadArgLines, !IO)
    ;
        BadArgLines = [],
        trace [compile_time(flag("debug_choose_grade")), io(!TIO)] (
            io.write_string("AFTER SETUP\n", !TIO),
            SolverVarMap1 = SolverInfo1 ^ si_solver_var_map,
            io.write_string(solver_var_map_to_str("    ", SolverVarMap1), !TIO)
        ),
        solve(SolverInfo1, _SolveCounts, Soln),
        io.write_string(soln_to_str("", Soln), !IO)
    ).

%---------------------------------------------------------------------------%

:- pred process_arguments(list(string)::in,
    cord(string)::in, cord(string)::out,
    solver_info::in, solver_info::out) is det.

process_arguments([], !BadArgLines, !SolverInfo).
process_arguments([Arg | Args], !BadArgLines, !SolverInfo) :-
    ( if string.remove_prefix("CONFIG", Arg, ArgSuffixPrime) then
        WhyNot = npw_config,
        ArgSuffix = ArgSuffixPrime
    else
        WhyNot = npw_user,
        ArgSuffix = Arg
    ),
    ( if
        EqComponents = string.split_at_string("=", ArgSuffix),
        EqComponents = [Var, Value]
    then
        SetTo = set_to_true,
        set_solver_var(Var, Value, SetTo, WhyNot, MaybeError, !SolverInfo)
    else if
        NeqComponents = string.split_at_string("!=", ArgSuffix),
        NeqComponents = [Var, Value]
    then
        SetTo = set_to_false,
        set_solver_var(Var, Value, SetTo, WhyNot, MaybeError, !SolverInfo)
    else
        string.format("unrecognized setting %s\n", [s(Arg)], ArgMsg),
        MaybeError = error(ArgMsg)
    ),
    (
        MaybeError = ok
    ;
        MaybeError = error(Msg),
        !:BadArgLines = cord.snoc(!.BadArgLines, Msg)
    ),
    process_arguments(Args, !BadArgLines, !SolverInfo).

%---------------------------------------------------------------------------%
:- end_module choose_grade.
%---------------------------------------------------------------------------%
