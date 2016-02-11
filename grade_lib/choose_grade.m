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
:- import_module grade_spec.

:- import_module bool.
:- import_module cord.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module string.

main(!IO) :-
    setup_solver_vars(SolverVarMap0, PrioSolverVarNames),
    setup_requirements(SolverVarMap0, Requirements),
    io.command_line_arguments(Args, !IO),
    process_arguments(Args, cord.init, BadArgLinesCord,
        SolverVarMap0, SolverVarMap1),
    BadArgLines = cord.list(BadArgLinesCord),
    (
        BadArgLines = [_ | _],
        io.write_string("unrecognized arguments:\n", !IO),
        list.foldl(io.write_string, BadArgLines, !IO)
    ;
        BadArgLines = [],
        trace [compile_time(flag("debug_choose_grade")), io(!TIO)] (
            io.write_string("AFTER SETUP\n", !TIO),
            io.write_string(solver_var_map_to_str(SolverVarMap1), !TIO)
        ),
        solve(Requirements, PrioSolverVarNames, SolverVarMap1, 1, Soln),
        io.write_string(soln_to_str(Soln), !IO)
    ).

%---------------------------------------------------------------------------%

:- pred process_arguments(list(string)::in,
    cord(string)::in, cord(string)::out,
    solver_var_map::in, solver_var_map::out) is det.

process_arguments([], !BadArgLines, !SolverVarMap).
process_arguments([Arg | Args], !BadArgLines, !SolverVarMap) :-
    ( if string.remove_prefix("CONFIG", Arg, ArgSuffixPrime) then
        WhyNot = npw_config,
        ArgSuffix = ArgSuffixPrime
    else
        WhyNot = npw_user,
        ArgSuffix = Arg
    ),
    ( if
        EqComponents = string.split_at_string("=", ArgSuffix),
        EqComponents = [VarStr, ValueStr]
    then
        SetTo = set_to_true,
        set_solver_var(VarStr, ValueStr, SetTo, WhyNot, MaybeError,
            !SolverVarMap)
    else if
        NeqComponents = string.split_at_string("!=", ArgSuffix),
        NeqComponents = [VarStr, ValueStr]
    then
        SetTo = set_to_false,
        set_solver_var(VarStr, ValueStr, SetTo, WhyNot, MaybeError,
            !SolverVarMap)
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
    process_arguments(Args, !BadArgLines, !SolverVarMap).

%---------------------------------------------------------------------------%
:- end_module choose_grade.
%---------------------------------------------------------------------------%
