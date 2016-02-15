%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_solver.
:- interface.

:- import_module grade_spec.
:- import_module grade_state.

:- import_module map.

:- type success_soln_map == map(solver_var_id, solver_var_value_id).

:- type solution
    --->    soln_failure
    ;       soln_success(success_soln_map).

:- pred solve(solver_info::in, solution::out) is det.

%---------------------------------------------------------------------------%

    % solver_var_map_to_str(Prefix, SolverVarMap):
    %
    % Print the contents of the given solver map to a string.
    % For each solver variable, and each potential value of that variable,
    % print whether the value is still possible, and if not, why not.
    % Add the given string as a prefix before every line.
    %
    % Abort if we detect an invariant of the solver map data structure
    % that does not hold.
    %
:- func solver_var_map_to_str(string, solver_var_map) = string.

    % solver_var_map_to_str(Prefix, Soln):
    %
    % Print whether the solution is a success or a failure. If a success,
    % print the selected values of the solver variables as a sequence of lines
    % of the form SolverVar = SolverVarValue.
    % Add the given string as a prefix before every line.
    %
:- func soln_to_str(string, solution) = string.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

solve(SolverInfo, Soln) :-
    SolverInfo = solver_info(Requirements, SolverVarPriorities, SolverVarMap0),
    solve_loop(Requirements, SolverVarPriorities, SolverVarMap0, 1, Soln).

:- pred solve_loop(list(requirement)::in, list(solver_var_id)::in,
    solver_var_map::in, int::in, solution::out) is det.

solve_loop(Requirements, SolverVarPriorities, !.SolverVarMap, !.Pass, Soln) :-
    propagate_to_fixpoint(Requirements, !SolverVarMap, !Pass),
    at_solution(SolverVarPriorities, !.SolverVarMap, MaybeSoln),
    (
        MaybeSoln = yes(Soln)
    ;
        MaybeSoln = no,
        label_step(SolverVarPriorities, !SolverVarMap),
        solve_loop(Requirements, SolverVarPriorities, !.SolverVarMap,
            !.Pass, Soln)
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
        io.write_string(solver_var_map_to_str("    ", !.SolverVarMap), !IO)
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
        IfVarId, IfValueId, ThenVarId, ReqThenValueIds),
    % The requirement represents the implication:
    %
    %   (IfVarId = IfValueId) -> (ThenVarId in ReqThenValueIds)
    %
    map.lookup(!.SolverVarMap, IfVarId, IfVar0),
    IfVar0 = solver_var(_IfCntAll, IfCntPoss0, IfValues0),
    ( if is_value_possible(IfValues0, IfValueId, is_possible) then
        map.lookup(!.SolverVarMap, ThenVarId, ThenVar0),
        ThenVar0 = solver_var(_ThenCntAll, ThenCntPoss0, ThenValues0),
        ( if some_value_is_possible(ThenValues0, ReqThenValueIds) then
            ( if IfCntPoss0 = 1 then
                % We know that (IfVarId = IfValueId). We can therefore
                % impose (ThenVarId in ReqThenValueIds).
                %
                restrict_possibilities_to(ReqThenValueIds, ReqId,
                    0, ThenCntPoss, ThenValues0, ThenValues),
                ( if ThenCntPoss < ThenCntPoss0 then
                    ThenVar1 = ThenVar0 ^ sv_cnt_possible := ThenCntPoss,
                    ThenVar = ThenVar1 ^ sv_values := ThenValues,
                    map.det_update(ThenVarId, ThenVar, !SolverVarMap),
                    !:Changed = changed
                else
                    % The condition (ThenVarId in ReqThenValueIds)
                    % already held, so nothing has changed.
                    true
                )
            else
                true
            )
        else
            % Since (ThenVarId in ReqThenValueIds) is false,
            % we can impose (IfVarId = IfValueId) being false as well.
            %
            IfCntPoss = IfCntPoss0 - 1,
            set_value_to_not_possible(IfValueId, ReqId, IfValues0, IfValues),
            IfVar1 = IfVar0 ^ sv_cnt_possible := IfCntPoss,
            IfVar = IfVar1 ^ sv_values := IfValues,
            map.det_update(IfVarId, IfVar, !SolverVarMap),
            !:Changed = changed
        )
    else
        % We already know that (IfVarId = IfValueId) is false.
        % Since false implies anything, the requirement is already met.
        true
    ),
    propagate_pass(Requirements, !SolverVarMap, !Changed).

:- pred some_value_is_possible(list(solver_var_value)::in,
    list(solver_var_value_id)::in) is semidet.

some_value_is_possible(Values, [SearchValueId | SearchValueIds]) :-
    ( if is_value_possible(Values, SearchValueId, is_possible) then
        true
    else
        some_value_is_possible(Values, SearchValueIds)
    ).

:- pred is_value_possible(list(solver_var_value)::in,
    solver_var_value_id::in, solver_var_value_possible::out) is det.

is_value_possible([], _, _) :-
    unexpected($pred, "did not find value").
is_value_possible([Value | Values], SearchValueId, Possible) :-
    Value = solver_var_value(ValueId, ValuePossible),
    ( if ValueId = SearchValueId then
        Possible = ValuePossible
    else
        is_value_possible(Values, SearchValueId, Possible)
    ).

:- pred set_value_to_not_possible(solver_var_value_id::in,
    requirement_id::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_not_possible(_SetValueId, _ReqId, [], _) :-
    unexpected($pred, "value not found").
set_value_to_not_possible(SetValueId, ReqId, [HeadValue0 | TailValues0],
        Values) :-
    HeadValue0 = solver_var_value(ValueId, _Possible0),
    ( if SetValueId = ValueId then
        Possible = not_possible(npw_requirement(ReqId)),
        HeadValue = solver_var_value(ValueId, Possible),
        Values = [HeadValue | TailValues0]
    else
        set_value_to_not_possible(SetValueId, ReqId, TailValues0, TailValues),
        Values = [HeadValue0 | TailValues]
    ).

:- pred restrict_possibilities_to(list(solver_var_value_id)::in,
    requirement_id::in, int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

restrict_possibilities_to(!.SetValueIds, _ReqId, !CntPoss, [], []) :-
    expect(unify(!.SetValueIds, []), $pred, "value not found").
restrict_possibilities_to(!.SetValueIds, ReqId, !CntPoss,
        [HeadValue0 | TailValues0], [HeadValue | TailValues]) :-
    HeadValue0 = solver_var_value(ValueId, Possible0),
    ( if list.delete_first(!.SetValueIds, ValueId, !:SetValueIds) then
        (
            Possible0 = is_possible,
            !:CntPoss = !.CntPoss + 1
        ;
            Possible0 = not_possible(_)
        ),
        HeadValue = HeadValue0
    else
        Possible = not_possible(npw_requirement(ReqId)),
        HeadValue = solver_var_value(ValueId, Possible)
    ),
    restrict_possibilities_to(!.SetValueIds, ReqId, !CntPoss,
        TailValues0, TailValues).

%---------------------------------------------------------------------------%

:- pred label_step(list(solver_var_id)::in,
    solver_var_map::in, solver_var_map::out) is det.

label_step(SolverVarPriorities, SolverMap0, SolverMap) :-
    find_first_undetermined_var(SolverVarPriorities, SolverMap0,
        FirstVarId, FirstVar0),
    FirstVar0 = solver_var(_CntAll, _CntPoss, FirstValues0),
    find_first_possible_value_and_commit(FirstVarId,
        FirstValues0, FirstValues),
    FirstVar1 = FirstVar0 ^ sv_cnt_possible := 1,
    FirstVar = FirstVar1 ^ sv_values := FirstValues,
    map.det_update(FirstVarId, FirstVar, SolverMap0, SolverMap).

:- pred find_first_undetermined_var(list(solver_var_id)::in,
    solver_var_map::in, solver_var_id::out, solver_var::out) is det.

find_first_undetermined_var([], _, _, _) :-
    unexpected($pred, "no undetermined var").
find_first_undetermined_var([SolverVarId | SolverVarIds], SolverMap,
        FirstVarId, FirstVar) :-
    map.lookup(SolverMap, SolverVarId, SolverVar),
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss > 1 then
        FirstVarId = SolverVarId,
        FirstVar = SolverVar
    else
        find_first_undetermined_var(SolverVarIds, SolverMap,
            FirstVarId, FirstVar)
    ).

:- pred find_first_possible_value_and_commit(solver_var_id::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

find_first_possible_value_and_commit(_VarId, [], []) :-
    unexpected($pred, "no possible value").
find_first_possible_value_and_commit(VarId, [Value0 | Values0],
        [Value | Values]) :-
    Value0 = solver_var_value(ValueId, Possible0),
    (
        Possible0 = is_possible,
        trace [compile_time(flag("debug_choose_grade")), io(!TIO)] (
            solver_var_name(VarName, VarId),
            solver_var_value_name(ValueName, ValueId),
            io.format("LABELING sets %s to %s\n",
                [s(VarName), s(ValueName)], !TIO)
        ),
        Value = Value0,
        set_values_not_possible_labeling(Values0, Values)
    ;
        Possible0 = not_possible(_),
        Value = Value0,
        find_first_possible_value_and_commit(VarId, Values0, Values)
    ).

:- pred set_values_not_possible_labeling(
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_values_not_possible_labeling([], []).
set_values_not_possible_labeling([Value0 | Values0], [Value | Values]) :-
    Value0 = solver_var_value(ValudId, Possible0),
    (
        Possible0 = is_possible,
        Possible = not_possible(npw_labeling),
        Value = solver_var_value(ValudId, Possible)
    ;
        Possible0 = not_possible(_),
        Value = Value0
    ),
    set_values_not_possible_labeling(Values0, Values).

%---------------------------------------------------------------------------%

:- pred at_solution(list(solver_var_id)::in, solver_var_map::in,
    maybe(solution)::out) is det.

at_solution(SolverVarPriorities, SolverVarMap, MaybeSoln) :-
    map.foldl2_values(cnt_poss_classes, SolverVarMap, 0, NumZero, 0, NumMore),
    ( if NumZero > 0 then
        MaybeSoln = yes(soln_failure)
    else if NumMore = 0 then
        list.foldl(project_to_one_poss_value(SolverVarMap),
            SolverVarPriorities, map.init, SuccessSoln),
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

:- pred project_to_one_poss_value(solver_var_map::in, solver_var_id::in,
    success_soln_map::in, success_soln_map::out) is det.

project_to_one_poss_value(SolverVarMap, VarId, !SolnMap) :-
    map.lookup(SolverVarMap, VarId, SolverVar),
    SolverVar = solver_var(_CntAll, _CntPoss, Values),
    get_poss_values(Values, PossValueIds),
    ( if PossValueIds = [ValueId] then
        map.det_insert(VarId, ValueId, !SolnMap)
    else
        unexpected($pred, "number of possible values is not one")
    ).

:- pred get_poss_values(list(solver_var_value)::in,
    list(solver_var_value_id)::out) is det.

get_poss_values([], []).
get_poss_values([SolverVarValue | SolverVarValues], PossValueIds) :-
    get_poss_values(SolverVarValues, PossValueIdsTail),
    SolverVarValue = solver_var_value(ValueId, Possible),
    (
        Possible = is_possible,
        PossValueIds = [ValueId | PossValueIdsTail]
    ;
        Possible = not_possible(_),
        PossValueIds = PossValueIdsTail
    ).

%---------------------------------------------------------------------------%

solver_var_map_to_str(Prefix, SolverVarMap) = Str :-
    map.to_sorted_assoc_list(SolverVarMap, SolverAssocList),
    SolverVarStrs = list.map(solver_var_pair_to_str(Prefix), SolverAssocList),
    Str = string.append_list(SolverVarStrs).

:- func solver_var_pair_to_str(string, pair(solver_var_id, solver_var))
    = string.

solver_var_pair_to_str(Prefix, SolverVarName - SolverVar) = Str :-
    SolverVar = solver_var(_CntAll, _CntPoss, _Values),
    Str = Prefix ++ solver_var_to_str(SolverVarName, SolverVar).

:- func solver_var_to_str(solver_var_id, solver_var) = string.

solver_var_to_str(VarId, SolverVar) = Str :-
    SolverVar = solver_var(_CntAll, CntPoss, Values),
    solver_var_name(VarName, VarId),
    ValuesStrs = list.map(solver_var_value_to_str(CntPoss), Values),
    ValuesStr = string.join_list(", ", ValuesStrs),
    expect(unify(CntPoss, count_possible_values(Values)), $pred,
        "CntPoss mismatch"),
    string.format("solver var %s: %d possible\n\t%s\n",
        [s(VarName), i(CntPoss), s(ValuesStr)], Str).

:- func solver_var_value_to_str(int, solver_var_value) = string.

solver_var_value_to_str(CntPoss, SolverVarValue) = Str :-
    SolverVarValue = solver_var_value(ValueId, Possible),
    solver_var_value_name(ValueName, ValueId),
    (
        Possible = is_possible,
        ( if CntPoss = 1 then
            string.format("%s yes", [s(ValueName)], Str)
        else
            string.format("%s maybe", [s(ValueName)], Str)
        )
    ;
        Possible = not_possible(WhyNot),
        (
            WhyNot = npw_config,
            string.format("%s no (config)", [s(ValueName)], Str)
        ;
            WhyNot = npw_user,
            string.format("%s no (user)", [s(ValueName)], Str)
        ;
            WhyNot = npw_requirement(requirement_id(ReqIdNum)),
            string.format("%s no (req %d)", [s(ValueName), i(ReqIdNum)], Str)
        ;
            WhyNot = npw_labeling,
            string.format("%s no (labeling)", [s(ValueName)], Str)
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

soln_to_str(Prefix, soln_failure) = Prefix ++ "FAILURE\n".
soln_to_str(Prefix, soln_success(SuccMap)) = Str :-
    map.to_assoc_list(SuccMap, SuccessValues),
    SuccessStr = Prefix ++ "SUCCESS\n",
    SuccessValueStrs = list.map(success_value_to_str(Prefix), SuccessValues),
    Str = SuccessStr ++ string.append_list(SuccessValueStrs).

:- func success_value_to_str(string,
    pair(solver_var_id, solver_var_value_id)) = string.

success_value_to_str(Prefix, VarId - ValueId) = Str :-
    solver_var_name(VarName, VarId),
    solver_var_value_name(ValueName, ValueId),
    string.format("%s%s = %s\n", [s(Prefix), s(VarName), s(ValueName)], Str).

%---------------------------------------------------------------------------%
:- end_module grade_solver.
%---------------------------------------------------------------------------%
