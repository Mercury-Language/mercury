%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_setup.
:- interface.

:- import_module grade_spec.

:- import_module list.
:- import_module maybe.

%---------------------------------------------------------------------------%

:- pred setup_solver_vars(solver_var_map::out, list(solver_var_name)::out)
    is det.

%---------------------------------------------------------------------------%

:- type solver_var_set_to
    --->    set_to_false
    ;       set_to_true.

:- pred set_solver_var(string::in, string::in, solver_var_set_to::in,
    not_possible_why::in, maybe_error::out,
    solver_var_map::in, solver_var_map::out) is det.

%---------------------------------------------------------------------------%

:- pred setup_requirements(solver_var_map::in, list(requirement)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

setup_solver_vars(SolverVarMap, PrioSolverVarNames) :-
    SolverVars0 = init_solver_vars,
    assoc_list.keys(SolverVars0, PrioSolverVarNames),
    init_solver_vars(SolverVars0, map.init, SolverVarMap).

:- pred init_solver_vars(assoc_list(solver_var_name, solver_var)::in,
    solver_var_map::in, solver_var_map::out) is det.

init_solver_vars([], !SolverVarMap).
init_solver_vars([Pair0 | Pairs0], !SolverVarMap) :-
    Pair0 = SolverVarName - SolverVar0,
    SolverVar0 = solver_var(_, _, Values0),
    init_solver_var_values(0, NumValues, Values0, Values),
    SolverVar = solver_var(NumValues, NumValues, Values),
    expect(isnt(unify(0), NumValues), $pred, "no values for solver var"),
    map.det_insert(SolverVarName, SolverVar, !SolverVarMap),
    init_solver_vars(Pairs0, !SolverVarMap).

:- pred init_solver_var_values(int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

init_solver_var_values(CurNumValues, CurNumValues, [], []).
init_solver_var_values(CurNumValues, NumValues,
        [Value0 | Values0], [Value | Values]) :-
    Value0 = solver_var_value(Name, _),
    Value = solver_var_value(Name, is_possible),
    init_solver_var_values(CurNumValues + 1, NumValues, Values0, Values).

%---------------------------------------------------------------------------%

set_solver_var(VarStr, ValueStr, SetTo, WhyNot, MaybeError, !SolverVarMap) :-
    VarName = sv_name(VarStr),
    ValueName = svv_name(ValueStr),
    ( if map.search(!.SolverVarMap, VarName, SolverVar0) then
        SolverVar0 = solver_var(CntAll, CntPoss0, Values0),
        (
            SetTo = set_to_true,
            set_value_to_true(ValueName, WhyNot, [], OldPossibles,
                Values0, Values)
        ;
            SetTo = set_to_false,
            set_value_to_false(ValueName, WhyNot, [], OldPossibles,
                Values0, Values)
        ),
        (
            OldPossibles = [],
            string.format("solver variable %s has no value named %s\n",
                [s(VarStr), s(ValueStr)], Msg),
            MaybeError = error(Msg)
        ;
            OldPossibles = [OldPossible],
            (
                SetTo = set_to_true,
                (
                    OldPossible = is_possible,
                    % This is the expected case. VarName may still be
                    % ValueName, but now it cannot be any other value.
                    MaybeError = ok,
                    CntPoss = 1,
                    SolverVar = solver_var(CntAll, CntPoss, Values),
                    map.det_update(VarName, SolverVar, !SolverVarMap)
                ;
                    OldPossible = not_possible(_),
                    % Other parameter settings have already ruled out
                    % VarName = ValueName, so this is an inconsistency
                    % between those earlier settings and this one.
                    string.format(
                        "inconsistent settings for solver variable %s\n",
                        [s(VarStr)], Msg),
                    MaybeError = error(Msg)
                )
            ;
                SetTo = set_to_false,
                (
                    OldPossible = is_possible,
                    % If the variable was set to true before, that is expected.
                    MaybeError = ok,
                    CntPoss = CntPoss0 - 1,
                    SolverVar = solver_var(CntAll, CntPoss, Values),
                    map.det_update(VarName, SolverVar, !SolverVarMap)
                ;
                    OldPossible = not_possible(_),
                    % If the variable was set to false before, our setting it
                    % to false is redundant, but ok.
                    MaybeError = ok
                )
            )
        ;
            OldPossibles = [_, _ | _],
            string.format(
                "solver var %s has more than one copy of value %s\n",
                [s(VarStr), s(ValueStr)], Msg),
            % This should have caught during setup.
            unexpected($pred, Msg)
        )
    else
        string.format("there is no solver variable named %s\n",
            [s(VarStr)], Msg),
        MaybeError = error(Msg)
    ).

:- pred set_value_to_false(solver_var_value_name::in, not_possible_why::in,
    list(solver_var_value_possible)::in, list(solver_var_value_possible)::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_false(_SetName, _WhyNot, !OldPossibles, [], []).
set_value_to_false(SetName, WhyNot, !OldPossibles,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if SetName = Name then
        !:OldPossibles = [Possible0 | !.OldPossibles],
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Name, Possible)
    else
        HeadValue = HeadValue0
    ),
    set_value_to_false(SetName, WhyNot, !OldPossibles,
        TailValues0, TailValues).

:- pred set_value_to_true(solver_var_value_name::in, not_possible_why::in,
    list(solver_var_value_possible)::in, list(solver_var_value_possible)::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_true(_SetName, _WhyNot, !OldPossibles, [], []).
set_value_to_true(SetName, WhyNot, !OldPossibles,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if SetName = Name then
        !:OldPossibles = [Possible0 | !.OldPossibles],
        Possible = is_possible,
        HeadValue = solver_var_value(Name, Possible)
    else
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Name, Possible)
    ),
    set_value_to_true(SetName, WhyNot, !OldPossibles,
        TailValues0, TailValues).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

setup_requirements(SolverVarMap, Requirements) :-
    Requirements0 = init_requirements,
    fill_in_and_check_requirements(0, SolverVarMap,
        Requirements0, Requirements, BadRequirements),
    (
        BadRequirements = []
    ;
        BadRequirements = [_ | _],
        trace [io(!IO)] (
            io.write(BadRequirements, !IO)
        ),
        unexpected($pred, "some bad requirements")
    ).

:- pred fill_in_and_check_requirements(int::in, solver_var_map::in,
    list(requirement)::in, list(requirement)::out, list(requirement)::out)
    is det.

fill_in_and_check_requirements(_CurId, _SolverVarMap, [], [], []).
fill_in_and_check_requirements(CurId, SolverVarMap,
        [Requirement0 | Requirements0], OkRequirements, BadRequirements) :-
    fill_in_and_check_requirements(CurId + 1, SolverVarMap, Requirements0,
        OkRequirementsTail, BadRequirementsTail),
    Requirement0 = requirement(_, ReqDesc,
        IfVar, IfValue, ThenVar, ThenValues),
    Requirement = requirement(requirement_id(CurId), ReqDesc,
        IfVar, IfValue, ThenVar, ThenValues),
    ( if
        % Both variable names in the requirement must be in SolverMap,
        % and all the value names must be in the SolverMap entries
        % of their respective variables. If they are not, the requirement
        % does not make sense.
        known_var_values(SolverVarMap, IfVar, [IfValue]),
        known_var_values(SolverVarMap, ThenVar, ThenValues),

        % Requirement represents the implication:
        %
        %   (IfVar = IfValue) -> (ThenVar in ThenValues)
        %
        % If ThenValues = [], then the requirement is unsatisfiable.
        ThenValues = [_ | _]
    then
        OkRequirements = [Requirement | OkRequirementsTail],
        BadRequirements = BadRequirementsTail
    else
        OkRequirements = OkRequirementsTail,
        BadRequirements = [Requirement | BadRequirementsTail]
    ).

:- pred known_var_values(solver_var_map::in,
    solver_var_name::in, list(solver_var_value_name)::in) is semidet.

known_var_values(SolverVarMap, SearchVarName, SearchValueNames) :-
    map.search(SolverVarMap, SearchVarName, SolverVar),
    SolverVar = solver_var(_, _, VarValues),
    set.list_to_set(SearchValueNames, SearchSet),
    ValueNames = list.map(solver_var_value_project_name, VarValues),
    set.list_to_set(ValueNames, Set),
    set.subset(SearchSet, Set).

:- func solver_var_value_project_name(solver_var_value)
    = solver_var_value_name.

solver_var_value_project_name(SolverVarValue) = SolverVarValue ^ svv_name.

%---------------------------------------------------------------------------%
:- end_module grade_setup.
%---------------------------------------------------------------------------%
