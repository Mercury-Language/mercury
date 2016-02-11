%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_solver.
:- interface.

:- import_module grade_spec.

:- import_module assoc_list.
:- import_module list.

:- type solution
    --->    soln_failure
    ;       soln_success(
                assoc_list(solver_var_name, solver_var_value_name)
            ).

:- pred solve(list(requirement)::in, list(solver_var_name)::in,
    solver_var_map::in, int::in, solution::out) is det.

%---------------------------------------------------------------------------%

:- func solver_var_map_to_str(solver_var_map) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module std_util.
:- import_module string.

%---------------------------------------------------------------------------%

solve(Requirements, PrioSolverVarNames, !.SolverVarMap, !.Pass, Soln) :-
    propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass),
    at_solution(PrioSolverVarNames, !.SolverVarMap, MaybeSoln),
    (
        MaybeSoln = yes(Soln)
    ;
        MaybeSoln = no,
        label_step(PrioSolverVarNames, !SolverVarMap),
        solve(Requirements, PrioSolverVarNames, !.SolverVarMap, !.Pass, Soln)
    ).

%---------------------------------------------------------------------------%

:- type maybe_changed
    --->    not_changed
    ;       changed.

:- pred propagate_to_fixpoint(list(requirement)::in,
    solver_var_map::in, solver_var_map::out, int::in, int::out) is det.

propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass) :-
    propagate_pass(Requirements, !SolverVarMap, not_changed, Changed),
    trace [compile_time(flag("debug_choose_grade")), io(!IO)] (
        io.format("AFTER PROPAGATE PASS %d\n", [i(!.Pass)], !IO),
        io.write_string(solver_var_map_to_str(!.SolverVarMap), !IO)
    ),
    !:Pass = !.Pass + 1,
    (
        Changed = not_changed
    ;
        Changed = changed,
        propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass)
    ).

:- pred propagate_pass(list(requirement)::in,
    solver_var_map::in, solver_var_map::out,
    maybe_changed::in, maybe_changed::out) is det.

propagate_pass([], !SolverVarMap, !Changed).
propagate_pass([Requirement | Requirements], !SolverVarMap, !Changed) :-
    Requirement = requirement(ReqId, _Desc,
        IfVarName, IfValueName, ThenVarName, ReqThenValueNames),
    % The requirement represents the implication:
    %
    %   (IfVarName = IfValueName) -> (ThenVarName in ReqThenValueNames)
    %
    map.lookup(!.SolverVarMap, IfVarName, IfVar0),
    IfVar0 = solver_var(_IfCntAll, IfCntPoss0, IfValues0),
    ( if is_value_possible(IfValues0, IfValueName, is_possible) then
        map.lookup(!.SolverVarMap, ThenVarName, ThenVar0),
        ThenVar0 = solver_var(_ThenCntAll, ThenCntPoss0, ThenValues0),
        ( if some_value_is_possible(ThenValues0, ReqThenValueNames) then
            ( if IfCntPoss0 = 1 then
                % We know that (IfVarName = IfValueName). We can therefore
                % impose (ThenVarName in ReqThenValueNames).
                %
                restrict_possibilities_to(ReqThenValueNames, ReqId,
                    0, ThenCntPoss, ThenValues0, ThenValues),
                ( if ThenCntPoss < ThenCntPoss0 then
                    ThenVar1 = ThenVar0 ^ sv_cnt_possible := ThenCntPoss,
                    ThenVar = ThenVar1 ^ sv_values := ThenValues,
                    map.det_update(ThenVarName, ThenVar, !SolverVarMap),
                    !:Changed = changed
                else
                    % The condition (ThenVarName in ReqThenValueNames)
                    % already held, so nothing has changed.
                    true
                )
            else
                true
            )
        else
            % Since (ThenVarName in ReqThenValueNames) is false,
            % we can impose (IfVarName = IfValueName) being false as well.
            %
            IfCntPoss = IfCntPoss0 - 1,
            set_value_to_not_possible(IfValueName, ReqId, IfValues0, IfValues),
            IfVar1 = IfVar0 ^ sv_cnt_possible := IfCntPoss,
            IfVar = IfVar1 ^ sv_values := IfValues,
            map.det_update(IfVarName, IfVar, !SolverVarMap),
            !:Changed = changed
        )
    else
        % We already know that (IfVarName = IfValueName) is false.
        % Since false implies anything, the requirement is already met.
        true
    ),
    propagate_pass(Requirements, !SolverVarMap, !Changed).

:- pred some_value_is_possible(list(solver_var_value)::in,
    list(solver_var_value_name)::in) is semidet.

some_value_is_possible(Values, [SearchName | SearchNames]) :-
    ( if is_value_possible(Values, SearchName, is_possible) then
        true
    else
        some_value_is_possible(Values, SearchNames)
    ).

:- pred is_value_possible(list(solver_var_value)::in,
    solver_var_value_name::in, solver_var_value_possible::out) is det.

is_value_possible([], _, _) :-
    unexpected($pred, "did not find value").
is_value_possible([Value | Values], SearchName, Possible) :-
    Value = solver_var_value(ValueName, ValuePossible),
    ( if ValueName = SearchName then
        Possible = ValuePossible
    else
        is_value_possible(Values, SearchName, Possible)
    ).

:- pred set_value_to_not_possible(solver_var_value_name::in,
    requirement_id::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_not_possible(_SetName, _ReqId, [], _) :-
    unexpected($pred, "value not found").
set_value_to_not_possible(SetName, ReqId, [HeadValue0 | TailValues0],
        Values) :-
    HeadValue0 = solver_var_value(Name, _Possible0),
    ( if SetName = Name then
        Possible = not_possible(npw_requirement(ReqId)),
        HeadValue = solver_var_value(Name, Possible),
        Values = [HeadValue | TailValues0]
    else
        set_value_to_not_possible(SetName, ReqId, TailValues0, TailValues),
        Values = [HeadValue0 | TailValues]
    ).

:- pred restrict_possibilities_to(list(solver_var_value_name)::in,
    requirement_id::in, int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

restrict_possibilities_to(!.SetNames, _ReqId, !CntPoss, [], []) :-
    expect(unify(!.SetNames, []), $pred, "value not found").
restrict_possibilities_to(!.SetNames, ReqId, !CntPoss,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(Name, Possible0),
    ( if list.delete_first(!.SetNames, Name, !:SetNames) then
        (
            Possible0 = is_possible,
            !:CntPoss = !.CntPoss + 1
        ;
            Possible0 = not_possible(_)
        ),
        HeadValue = HeadValue0
    else
        Possible = not_possible(npw_requirement(ReqId)),
        HeadValue = solver_var_value(Name, Possible)
    ),
    restrict_possibilities_to(!.SetNames, ReqId, !CntPoss,
        TailValues0, TailValues).

%---------------------------------------------------------------------------%

:- pred label_step(list(solver_var_name)::in,
    solver_var_map::in, solver_var_map::out) is det.

label_step(PrioSolverVarNames, SolverMap0, SolverMap) :-
    find_first_undetermined_var(PrioSolverVarNames, SolverMap0,
        FirstVarName, FirstVar0),
    FirstVar0 = solver_var(_CntAll, _CntPoss, FirstValues0),
    find_first_possible_value_and_commit(FirstVarName,
        FirstValues0, FirstValues),
    FirstVar1 = FirstVar0 ^ sv_cnt_possible := 1,
    FirstVar = FirstVar1 ^ sv_values := FirstValues,
    map.det_update(FirstVarName, FirstVar, SolverMap0, SolverMap).

:- pred find_first_undetermined_var(list(solver_var_name)::in,
    solver_var_map::in, solver_var_name::out, solver_var::out) is det.

find_first_undetermined_var([], _, _, _) :-
    unexpected($pred, "no undetermined var").
find_first_undetermined_var([SolverVarName | SolverVarNames], SolverMap,
        FirstVarName, FirstVar) :-
    map.lookup(SolverMap, SolverVarName, SolverVar),
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss > 1 then
        FirstVarName = SolverVarName,
        FirstVar = SolverVar
    else
        find_first_undetermined_var(SolverVarNames, SolverMap,
            FirstVarName, FirstVar)
    ).

:- pred find_first_possible_value_and_commit(solver_var_name::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

find_first_possible_value_and_commit(_VarName, [], []) :-
    unexpected($pred, "no possible value").
find_first_possible_value_and_commit(VarName, [Value0 | Values0],
        [Value | Values]) :-
    Value0 = solver_var_value(ValueName, Possible0),
    (
        Possible0 = is_possible,
        VarName = sv_name(VarNameStr),
        ValueName = svv_name(ValueNameStr),
        trace [compile_time(flag("debug_choose_grade")), io(!TIO)] (
            io.format("LABELING sets %s to %s\n",
                [s(VarNameStr), s(ValueNameStr)], !TIO)
        ),
        Value = Value0,
        set_values_not_possible_labeling(Values0, Values)
    ;
        Possible0 = not_possible(_),
        Value = Value0,
        find_first_possible_value_and_commit(VarName, Values0, Values)
    ).

:- pred set_values_not_possible_labeling(
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_values_not_possible_labeling([], []).
set_values_not_possible_labeling([Value0 | Values0], [Value | Values]) :-
    Value0 = solver_var_value(Name, Possible0),
    (
        Possible0 = is_possible,
        Possible = not_possible(npw_labeling),
        Value = solver_var_value(Name, Possible)
    ;
        Possible0 = not_possible(_),
        Value = Value0
    ),
    set_values_not_possible_labeling(Values0, Values).

%---------------------------------------------------------------------------%

:- pred at_solution(list(solver_var_name)::in, solver_var_map::in,
    maybe(solution)::out) is det.

at_solution(PrioSolverVarNames, SolverVarMap, MaybeSoln) :-
    map.foldl2_values(cnt_poss_classes, SolverVarMap, 0, NumZero, 0, NumMore),
    ( if NumZero > 0 then
        MaybeSoln = yes(soln_failure)
    else if NumMore = 0 then
        list.map(project_to_one_poss_value(SolverVarMap),
            PrioSolverVarNames, SuccessSoln),
        MaybeSoln = yes(soln_success(SuccessSoln))
    else
        MaybeSoln = no
    ).

:- pred cnt_poss_classes(solver_var::in,
    int::in, int::out, int::in, int::out) is det.

cnt_poss_classes(SolverVar, !NumZero, !NumMore) :-
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss = 0 then
        !:NumZero = !.NumZero + 1
    else if CntPoss = 1 then
        true
    else
        !:NumMore = !.NumMore + 1
    ).

:- pred project_to_one_poss_value(solver_var_map::in, solver_var_name::in,
    pair(solver_var_name, solver_var_value_name)::out) is det.

project_to_one_poss_value(SolverVarMap, VarName, VarName - ValueName) :-
    map.lookup(SolverVarMap, VarName, SolverVar),
    SolverVar = solver_var(_CntAll, _CntPoss, Values),
    get_poss_values(Values, PossValueNames),
    ( if PossValueNames = [ValueNamePrime] then
        ValueName = ValueNamePrime
    else
        unexpected($pred, "number of possible values is not one")
    ).

:- pred get_poss_values(list(solver_var_value)::in,
    list(solver_var_value_name)::out) is det.

get_poss_values([], []).
get_poss_values([SolverVarValue | SolverVarValues], PossValueNames) :-
    get_poss_values(SolverVarValues, PossValueNamesTail),
    SolverVarValue = solver_var_value(Name, Possible),
    (
        Possible = is_possible,
        PossValueNames = [Name | PossValueNamesTail]
    ;
        Possible = not_possible(_),
        PossValueNames = PossValueNamesTail
    ).

%---------------------------------------------------------------------------%

solver_var_map_to_str(SolverVarMap) = Str :-
    map.to_sorted_assoc_list(SolverVarMap, SolverAssocList),
    SolverVarStrs = list.map(solver_var_pair_to_str, SolverAssocList),
    Str = string.append_list(SolverVarStrs).

:- func solver_var_pair_to_str(pair(solver_var_name, solver_var)) = string.

solver_var_pair_to_str(SolverVarName - SolverVar) = Str :-
    SolverVar = solver_var(_CntAll, _CntPoss, _Values),
    Str = solver_var_to_str(SolverVarName, SolverVar).

:- func solver_var_to_str(solver_var_name, solver_var) = string.

solver_var_to_str(SolverVarName, SolverVar) = Str :-
    SolverVar = solver_var(_CntAll, CntPoss, Values),
    SolverVarName = sv_name(NameStr),
    ValuesStrs = list.map(solver_var_value_to_str(CntPoss), Values),
    ValuesStr = string.join_list(", ", ValuesStrs),
    expect(unify(CntPoss, count_possible_values(Values)), $pred,
        "CntPoss mismatch"),
    string.format("solver var %s: %d possible\n\t%s\n",
        [s(NameStr), i(CntPoss), s(ValuesStr)], Str).

:- func solver_var_value_to_str(int, solver_var_value) = string.

solver_var_value_to_str(CntPoss, SolverVarValue) = Str :-
    SolverVarValue = solver_var_value(Name, Possible),
    Name = svv_name(NameStr),
    (
        Possible = is_possible,
        ( if CntPoss = 1 then
            string.format("%s yes", [s(NameStr)], Str)
        else
            string.format("%s maybe", [s(NameStr)], Str)
        )
    ;
        Possible = not_possible(WhyNot),
        (
            WhyNot = npw_config,
            string.format("%s no (config)", [s(NameStr)], Str)
        ;
            WhyNot = npw_user,
            string.format("%s no (user)", [s(NameStr)], Str)
        ;
            WhyNot = npw_requirement(requirement_id(ReqIdNum)),
            string.format("%s no (req %d)", [s(NameStr), i(ReqIdNum)], Str)
        ;
            WhyNot = npw_labeling,
            string.format("%s no (labeling)", [s(NameStr)], Str)
        )
    ).

:- func count_possible_values(list(solver_var_value)) = int.

count_possible_values([]) = 0.
count_possible_values([SolverVarValue | SolverVarValues]) = N :-
    NTail = count_possible_values(SolverVarValues),
    SolverVarValue = solver_var_value(_Name, Possible),
    (
        Possible = is_possible,
        N = NTail + 1
    ;
        Possible = not_possible(_),
        N = NTail
    ).

%---------------------------------------------------------------------------%
:- end_module grade_solver.
%---------------------------------------------------------------------------%
