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

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

main(!IO) :-
    setup_solver_vars(SolverVarMap0, PrioSolverVarNames),
    setup_requirements(SolverVarMap0, Requirements),
    io.command_line_arguments(Args, !IO),
    process_arguments(Args, BadArgLines, SolverVarMap0, SolverVarMap1),
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

:- pred process_arguments(list(string)::in, list(string)::out,
    solver_var_map::in, solver_var_map::out) is det.

process_arguments([], [], !SolverVarMap).
process_arguments([Arg | Args], BadArgLines, !SolverVarMap) :-
    ( if string.remove_prefix("CONFIG", Arg, ArgSuffixPrime) then
        WhyNot = npw_config,
        ArgSuffix = ArgSuffixPrime
    else
        WhyNot = npw_user,
        ArgSuffix = Arg
    ),
    ( if
        NeqComponents = string.split_at_string("!=", ArgSuffix),
        NeqComponents = [VarStr, ValueStr]
    then
        VarName = sv_name(VarStr),
        ValueName = svv_name(ValueStr),
        ( if
            map.search(!.SolverVarMap, VarName, SolverVar0),
            SolverVar0 = solver_var(CntAll, CntPoss0, Values0),
            set_value_to_false(WhyNot, ValueName, Values0, Values)
        then
            IsBad = no,
            CntPoss = CntPoss0 - 1,
            SolverVar = solver_var(CntAll, CntPoss, Values),
            map.det_update(VarName, SolverVar, !SolverVarMap)
        else
            IsBad = yes
        )
    else if
        EqComponents = string.split_at_string("=", ArgSuffix),
        EqComponents = [VarStr, ValueStr]
    then
        VarName = sv_name(VarStr),
        ValueName = svv_name(ValueStr),
        ( if
            map.search(!.SolverVarMap, VarName, SolverVar0),
            SolverVar0 = solver_var(CntAll, _CntPoss0, Values0),
            set_value_to_true(WhyNot, ValueName, 0, Matches, Values0, Values),
            Matches = 1
        then
            IsBad = no,
            CntPoss = 1,
            SolverVar = solver_var(CntAll, CntPoss, Values),
            map.det_update(VarName, SolverVar, !SolverVarMap)
        else
            IsBad = yes
        )
    else
        IsBad = yes
    ),
    process_arguments(Args, BadArgLinesTail, !SolverVarMap),
    (
        IsBad = no,
        BadArgLines = BadArgLinesTail
    ;
        IsBad = yes,
        BadArgLines = [Arg ++ "\n" | BadArgLinesTail]
    ).

%---------------------------------------------------------------------------%

:- func soln_to_str(solution) = string.

soln_to_str(soln_failure) = "FAILURE\n".
soln_to_str(soln_success(SuccessValues)) = Str :-
    SuccessStr = "SUCCESS\n",
    SuccessValueStrs = list.map(success_value_to_str, SuccessValues),
    Str = SuccessStr ++ string.append_list(SuccessValueStrs).

:- func success_value_to_str(pair(solver_var_name, solver_var_value_name))
    = string.

success_value_to_str(VarName - ValueName) = Str :-
    VarName = sv_name(VarNameStr),
    ValueName = svv_name(ValueNameStr),
    string.format("%s = %s\n", [s(VarNameStr), s(ValueNameStr)], Str).

%---------------------------------------------------------------------------%
:- end_module choose_grade.
%---------------------------------------------------------------------------%
