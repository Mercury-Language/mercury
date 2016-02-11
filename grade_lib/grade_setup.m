%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_setup.
:- interface.

:- import_module grade_spec.

:- import_module list.

%---------------------------------------------------------------------------%

:- pred setup_solver_vars(solver_var_map::out, list(solver_var_name)::out)
    is det.

:- pred set_value_to_false(not_possible_why::in, solver_var_value_name::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is semidet.

:- pred set_value_to_true(not_possible_why::in, solver_var_value_name::in,
    int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is semidet.

%---------------------------------------------------------------------------%

:- pred setup_requirements(solver_var_map::in, list(requirement)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

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

set_value_to_false(WhyNot, SetName, [HeadValue0 | TailValues0], Values) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if SetName = Name then
        % Fail if this value was set to not_possible even before this.
        Possible0 = is_possible,
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Name, Possible),
        Values = [HeadValue | TailValues0]
    else
        set_value_to_false(WhyNot, SetName, TailValues0, TailValues),
        Values = [HeadValue0 | TailValues]
    ).

set_value_to_true(_WhyNot, _SetName, !Matches, [], []).
set_value_to_true(WhyNot, SetName, !Matches,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if SetName = Name then
        !:Matches = !.Matches + 1,
        % Fail if this value was set to not_possible previously.
        Possible0 = is_possible,
        HeadValue = HeadValue0
    else
        Possible = not_possible(WhyNot),
        HeadValue = solver_var_value(Name, Possible)
    ),
    set_value_to_true(WhyNot, SetName, !Matches, TailValues0, TailValues).

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
