%---------------------------------------------------------------------------%
% vim: ts=4 sw=4 et ft=mercury
%---------------------------------------------------------------------------%

:- module grade_solver.
:- interface.

:- import_module grade_spec.
:- import_module grade_state.

:- import_module list.
:- import_module map.

:- type solve_counts
    --->    solve_counts(
                sc_num_label_steps      :: int,
                sc_num_passes           :: int
            ).

:- type failure_info.

:- type success_soln_map == map(solver_var_id, solver_var_value_id).

:- type solution
    --->    soln_failure(
                failure_info
            )
    ;       soln_success(
                success_soln_map
            ).

:- pred solve(solver_info::in, solve_counts::out, solution::out) is det.

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

:- type failure_tree
    --->    failure_tree(
                solver_var_id,
                list(why_var_is_not_value)
            ).

:- type why_var_is_not_value
    --->    why_var_is_not_value(
                solver_var_value_id,
                requirement_id,
                string,
                list(failure_tree)
            ).

:- func failure_info_to_failure_trees(failure_info)
    = one_or_more(failure_tree).

%---------------------------------------------------------------------------%

:- implementation.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.

%---------------------------------------------------------------------------%

solve(SolverInfo, SolveCounts, Soln) :-
    SolverInfo = solver_info(Requirements, SolverVarPriorities, SolverVarMap0),
    SolveCounts0 = solve_counts(0, 0),
    solve_loop(Requirements, Requirements, _FinalRequirements,
        SolverVarPriorities, SolverVarMap0, SolveCounts0, SolveCounts, Soln).

:- pred solve_loop(list(requirement)::in,
    list(requirement)::in, list(requirement)::out,
    list(solver_var_id)::in, solver_var_map::in,
    solve_counts::in, solve_counts::out, solution::out) is det.

solve_loop(AllRequirements, !CurRequirements, !.SolverVarPriorities,
        !.SolverVarMap, !SolveCounts, Soln) :-
    NumPasses0 = !.SolveCounts ^ sc_num_passes,
    propagate_to_fixpoint(!CurRequirements, !SolverVarMap,
        NumPasses0, NumPasses),
    !SolveCounts ^ sc_num_passes := NumPasses,
    at_solution(AllRequirements, !.SolverVarMap, MaybeSoln),
    (
        MaybeSoln = yes(Soln)
    ;
        MaybeSoln = no,
        NumLabelSteps0 = !.SolveCounts ^ sc_num_label_steps,
        !SolveCounts ^ sc_num_label_steps := NumLabelSteps0 + 1,
        label_step(!SolverVarPriorities, !SolverVarMap),
        solve_loop(AllRequirements, !CurRequirements, !.SolverVarPriorities,
            !.SolverVarMap, !SolveCounts, Soln)
    ).

%---------------------------------------------------------------------------%

:- type maybe_changed
    --->    not_changed
    ;       changed.

:- type maybe_found_failure
    --->    havent_found_failure
    ;       found_failure.

:- pred propagate_to_fixpoint(list(requirement)::in, list(requirement)::out,
    solver_var_map::in, solver_var_map::out, int::in, int::out) is det.

propagate_to_fixpoint(Requirements0, Requirements, !SolverVarMap,
        !NumPasses) :-
    !:NumPasses = !.NumPasses + 1,
    propagate_pass(Requirements0, [], RevRequirements1, !SolverVarMap,
        not_changed, Changed, havent_found_failure, FoundFailure),
    list.reverse(RevRequirements1, Requirements1),
    trace [compile_time(flag("debug_solver")), io(!IO)] (
        (
            FoundFailure = havent_found_failure,
            FoundFailureSuffix = ""
        ;
            FoundFailure = found_failure,
            FoundFailureSuffix = " (found failure)"
        ),
        io.format("\nAFTER PROPAGATE PASS %d%s\n",
            [i(!.NumPasses), s(FoundFailureSuffix)], !IO),
        io.write_string(solver_var_map_to_str("    ", !.SolverVarMap), !IO),
        io.nl(!IO)
    ),
    (
        FoundFailure = found_failure,
        % The next pass may have one of two results. Either it will return
        % not_changed, in which case the pass is wasted, or it will return
        % changed, with some of the changes being implications of the failure
        % we have already found. (If x = x1 implies Y in {y1,y2}), then
        % having no possible value left for Y in this pass will leave
        % no possible value for X in the next pass.) However, no further
        % pass can turn the failure back to a success, so there is no point
        % in doing such passes. There is point in NOT doing them: it allows
        % a failure error message to report the actual cause of the failure,
        % not its implications in later passes.
        % XXX Should we try to eliminate recording the implications of
        % the first failure in the *same pass*? And how would we distinguish
        % them from unrelated failures in the same pass?
        Requirements = Requirements1
    ;
        FoundFailure = havent_found_failure,
        (
            Changed = not_changed,
            Requirements = Requirements1
        ;
            Changed = changed,
            propagate_to_fixpoint(Requirements1, Requirements, !SolverVarMap,
                !NumPasses)
        )
    ).

:- pred propagate_pass(list(requirement)::in,
    list(requirement)::in, list(requirement)::out,
    solver_var_map::in, solver_var_map::out,
    maybe_changed::in, maybe_changed::out,
    maybe_found_failure::in, maybe_found_failure::out) is det.

propagate_pass([], !RevRequirements, !SolverVarMap, !Changed, !FoundFailure).
propagate_pass([Requirement | Requirements], !RevRequirements,
        !SolverVarMap, !Changed, !FoundFailure) :-
    Requirement = requirement(ReqId, ReqDesc,
        IfVarId, IfValueId, ThenVarId, ReqThenValueIds),
    trace [compile_time(flag("debug_solver")), io(!IO)] (
        ReqId = requirement_id(ReqIdNum0),
        io.format("Considering Req %d (%s)\n",
            [i(ReqIdNum0), s(ReqDesc)], !IO)
    ),

    % The requirement represents the implication:
    %
    %   (IfVarId = IfValueId) -> (ThenVarId in ReqThenValueIds)
    %
    map.lookup(!.SolverVarMap, IfVarId, IfVar0),
    IfVar0 = solver_var(IfCntAll, IfCntPoss0, IfValues0),
    ( if is_value_possible(IfValues0, IfValueId, is_possible) then
        map.lookup(!.SolverVarMap, ThenVarId, ThenVar0),
        ThenVar0 = solver_var(ThenCntAll, ThenCntPoss0, ThenValues0),
        ( if some_value_is_possible(ThenValues0, ReqThenValueIds) then
            ( if IfCntPoss0 = 1 then
                % We know that (IfVarId = IfValueId). We can therefore
                % impose (ThenVarId in ReqThenValueIds). This won't need
                % to be imposed again.
                %
                KeepReq = no,
                ReqAppl = requirement_application(ReqId, ReqDesc,
                    narrow_then_values),
                restrict_possibilities_to(ReqThenValueIds, ReqAppl,
                    0, ThenCntPoss, ThenValues0, ThenValues),
                ( if ThenCntPoss < ThenCntPoss0 then
                    trace [compile_time(flag("debug_solver")), io(!IO)] (
                        ReqId = requirement_id(ReqIdNum),
                        ThenVarName = string.string(ThenVarId),
                        ThenValueNames0 = list_poss_values(ThenValues0),
                        ThenValueNames = list_poss_values(ThenValues),
                        io.format("Req %d (%s) updates then_var %s\n",
                            [i(ReqIdNum), s(ReqDesc), s(ThenVarName)], !IO),
                        io.format("  %s -> %s\n\n",
                            [s(ThenValueNames0), s(ThenValueNames)], !IO)
                    ),
                    ThenVar = solver_var(ThenCntAll, ThenCntPoss, ThenValues),
                    map.det_update(ThenVarId, ThenVar, !SolverVarMap),
                    !:Changed = changed,
                    ( if ThenCntPoss = 0 then
                        !:FoundFailure = found_failure
                    else
                        true
                    )
                else
                    % The condition (ThenVarId in ReqThenValueIds)
                    % already held, so nothing has changed.

                    trace [compile_time(flag("debug_solver")), io(!IO)] (
                        ReqId = requirement_id(ReqIdNum),
                        ThenVarName = string.string(ThenVarId),
                        ThenValueNames0 = list_poss_values(ThenValues0),
                        ThenValueNames = list_poss_values(ThenValues),
                        io.format("Req %d (%s) pre-holds for then_var %s\n",
                            [i(ReqIdNum), s(ReqDesc), s(ThenVarName)], !IO),
                        io.format("  %s -> %s\n\n",
                            [s(ThenValueNames0), s(ThenValueNames)], !IO)
                    )
                )
            else
                KeepReq = yes
            )
        else
            % Since (ThenVarId in ReqThenValueIds) is false,
            % we can impose (IfVarId = IfValueId) being false as well.
            % This won't need to be imposed again.
            %
            KeepReq = no,
            IfCntPoss = IfCntPoss0 - 1,
            ReqAppl = requirement_application(ReqId, ReqDesc, delete_if_value),
            set_value_to_not_possible(IfValueId, ReqAppl, IfValues0, IfValues),
            trace [compile_time(flag("debug_solver")), io(!IO)] (
                ReqId = requirement_id(ReqIdNum),
                IfVarName = string.string(IfVarId),
                IfValueNames0 = list_poss_values(IfValues0),
                IfValueNames = list_poss_values(IfValues),
                io.format("Req %d (%s) updates if_var %s\n",
                    [i(ReqIdNum), s(ReqDesc), s(IfVarName)], !IO),
                io.format("  %s -> %s\n\n",
                    [s(IfValueNames0), s(IfValueNames)], !IO)
            ),
            IfVar = solver_var(IfCntAll, IfCntPoss, IfValues),
            map.det_update(IfVarId, IfVar, !SolverVarMap),
            !:Changed = changed,
            ( if IfCntPoss = 0 then
                !:FoundFailure = found_failure
            else
                true
            )
        )
    else
        % We already know that (IfVarId = IfValueId) is false.
        % Since false implies anything, the requirement is already met.
        trace [compile_time(flag("debug_solver")), io(!IO)] (
            ReqId = requirement_id(ReqIdNum),
            IfVarName = string.string(IfVarId),
            IfValueName = string.string(IfValueId),
            io.format("Deleting Req %d (%s): %s is not %s\n\n",
                [i(ReqIdNum), s(ReqDesc), s(IfVarName), s(IfValueName)], !IO)
        ),
        KeepReq = no
    ),
    (
        KeepReq = no
    ;
        KeepReq = yes,
        !:RevRequirements = [Requirement | !.RevRequirements]
    ),
    propagate_pass(Requirements, !RevRequirements, !SolverVarMap,
        !Changed, !FoundFailure).

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
    requirement_application::in,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

set_value_to_not_possible(_SetValueId, _ReqAppl, [], _) :-
    unexpected($pred, "value not found").
set_value_to_not_possible(SetValueId, ReqAppl,
        [HeadValue0 | TailValues0], Values) :-
    HeadValue0 = solver_var_value(ValueId, _Possible0),
    ( if SetValueId = ValueId then
        Possible = not_possible(npw_requirement(ReqAppl)),
        HeadValue = solver_var_value(ValueId, Possible),
        Values = [HeadValue | TailValues0]
    else
        set_value_to_not_possible(SetValueId, ReqAppl,
            TailValues0, TailValues),
        Values = [HeadValue0 | TailValues]
    ).

:- pred restrict_possibilities_to(list(solver_var_value_id)::in,
    requirement_application::in, int::in, int::out,
    list(solver_var_value)::in, list(solver_var_value)::out) is det.

restrict_possibilities_to(!.SetValueIds, _ReqAppl, !CntPoss, [], []) :-
    expect(unify(!.SetValueIds, []), $pred, "value not found").
restrict_possibilities_to(!.SetValueIds, ReqAppl, !CntPoss,
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
        Possible = not_possible(npw_requirement(ReqAppl)),
        HeadValue = solver_var_value(ValueId, Possible)
    ),
    restrict_possibilities_to(!.SetValueIds, ReqAppl, !CntPoss,
        TailValues0, TailValues).

%---------------------------------------------------------------------------%

:- func list_poss_values(list(solver_var_value)) = string.

list_poss_values(SolverVarValues) = Str :-
    accumulate_poss_value_strings(SolverVarValues, [], RevPossStrs),
    list.reverse(RevPossStrs, PossStrs),
    Str = "[" ++ string.join_list(", ", PossStrs) ++ "]".

:- pred accumulate_poss_value_strings(list(solver_var_value)::in,
    list(string)::in, list(string)::out) is det.

accumulate_poss_value_strings([], !RevPossStrs).
accumulate_poss_value_strings([SolverVarValue | SolverVarValues],
        !RevPossStrs) :-
    SolverVarValue = solver_var_value(ValueId, Possible),
    (
        Possible = is_possible,
        !:RevPossStrs = [string.string(ValueId) | !.RevPossStrs]
    ;
        Possible = not_possible(_)
    ),
    accumulate_poss_value_strings(SolverVarValues, !RevPossStrs).

%---------------------------------------------------------------------------%

:- pred label_step(list(solver_var_id)::in, list(solver_var_id)::out,
    solver_var_map::in, solver_var_map::out) is det.

label_step(!SolverVarPriorities, !SolverMap) :-
    find_first_undetermined_var(!SolverVarPriorities, !.SolverMap,
        FirstVarId, FirstVar0),
    FirstVar0 = solver_var(_CntAll, _CntPoss, FirstValues0),
    find_first_possible_value_and_commit(FirstVarId,
        FirstValues0, FirstValues),
    FirstVar1 = FirstVar0 ^ sv_cnt_possible := 1,
    FirstVar = FirstVar1 ^ sv_values := FirstValues,
    map.det_update(FirstVarId, FirstVar, !SolverMap).

:- pred find_first_undetermined_var(
    list(solver_var_id)::in, list(solver_var_id)::out,
    solver_var_map::in, solver_var_id::out, solver_var::out) is det.

find_first_undetermined_var([], _, _, _, _) :-
    unexpected($pred, "no undetermined var").
find_first_undetermined_var([SolverVarId | SolverVarIds], LaterSolverVarIds,
        SolverMap, FirstVarId, FirstVar) :-
    map.lookup(SolverMap, SolverVarId, SolverVar),
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss > 1 then
        FirstVarId = SolverVarId,
        FirstVar = SolverVar,
        LaterSolverVarIds = SolverVarIds
    else
        find_first_undetermined_var(SolverVarIds, LaterSolverVarIds,
            SolverMap, FirstVarId, FirstVar)
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
        trace [compile_time(flag("debug_solver")), io(!TIO)] (
            solver_var_name(VarName, VarId),
            solver_var_value_name(ValueName, ValueId),
            io.format("\nLABELING sets %s to %s\n",
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

:- pred at_solution(list(requirement)::in, solver_var_map::in,
    maybe(solution)::out) is det.

at_solution(Requirements, SolverVarMap, MaybeSoln) :-
    map.foldl2(cnt_poss_classes, SolverVarMap, [], ZeroCntVarIds,
        0, NumMore),
    (
        ZeroCntVarIds = [HeadZeroCntVarId | TailZeroCntVarIds],
        FailureInfo = failure_info(Requirements, SolverVarMap,
            one_or_more(HeadZeroCntVarId, TailZeroCntVarIds)),
        MaybeSoln = yes(soln_failure(FailureInfo))
    ;
        ZeroCntVarIds = [],
        ( if NumMore = 0 then
            map.foldl(accumulate_the_one_poss_value, SolverVarMap,
                [], OnePossValues),
            map.from_rev_sorted_assoc_list(OnePossValues, SuccessSoln),
            MaybeSoln = yes(soln_success(SuccessSoln))
        else
            MaybeSoln = no
        )
    ).

:- pred cnt_poss_classes(solver_var_id::in, solver_var::in,
    list(solver_var_id)::in, list(solver_var_id)::out,
    int::in, int::out) is det.

cnt_poss_classes(SolverVarId, SolverVar, !ZeroCntVarIds, !NumMore) :-
    SolverVar = solver_var(_CntAll, CntPoss, _Values),
    ( if CntPoss = 0 then
        !:ZeroCntVarIds = [SolverVarId | !.ZeroCntVarIds]
    else if CntPoss = 1 then
        true
    else
        !:NumMore = !.NumMore + 1
    ).

:- pred accumulate_the_one_poss_value(solver_var_id::in, solver_var::in,
    assoc_list(solver_var_id, solver_var_value_id)::in,
    assoc_list(solver_var_id, solver_var_value_id)::out) is det.

accumulate_the_one_poss_value(VarId, SolverVar, !OnePossValues) :-
    SolverVar = solver_var(_CntAll, _CntPoss, Values),
    get_poss_values(Values, PossValueIds),
    ( if PossValueIds = [ValueId] then
        !:OnePossValues = [VarId - ValueId | !.OnePossValues]
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
            WhyNot = npw_requirement(ReqAppl),
            ReqAppl = requirement_application(ReqId, _ReqDesc, ReqDir),
            ReqId = requirement_id(ReqIdNum), 
            ( ReqDir = narrow_then_values, ReqDirStr = "narrow_then_values"
            ; ReqDir = delete_if_value, ReqDirStr = "delete_if_value"
            ),
            string.format("%s no (req %d, %s)",
                [s(ValueName), i(ReqIdNum), s(ReqDirStr)], Str)
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

soln_to_str(Prefix, Soln) = Str :-
    (
        Soln = soln_failure(FailureInfo),
        FailureStr = Prefix ++ "FAILURE\n",
        ( if Prefix = "" then
            IndentStr = "    "
        else
            IndentStr = Prefix
        ),
        FailureTrees = failure_info_to_failure_trees(FailureInfo),
        FailureTrees = one_or_more(HeadFailureTree, TailFailureTrees),
        HeadFailureStr = failure_tree_to_string(Prefix, IndentStr,
            HeadFailureTree),
        TailFailureStrs = list.map(failure_tree_to_string(Prefix, IndentStr),
            TailFailureTrees),
        Str = FailureStr ++ HeadFailureStr ++
            string.append_list(TailFailureStrs) ++ "\n"
    ;
        Soln = soln_success(SuccMap),
        SuccessStr = Prefix ++ "SUCCESS\n",
        map.to_assoc_list(SuccMap, SuccessValues),
        SuccessValueStrs =
            list.map(success_value_to_str(Prefix), SuccessValues),
        Str = SuccessStr ++ string.append_list(SuccessValueStrs)
    ).

:- func failure_tree_to_string(string, string, failure_tree) = string.

failure_tree_to_string(CurIndentStr, EachIndentStr, FailureTree) = Str :-
    FailureTree = failure_tree(VarId, WhyNots),
    solver_var_name(VarName, VarId),
    (
        WhyNots = [],
        string.format(
            "%sconfiguration and user settings leave no valid value for %s\n",
            [s(CurIndentStr), s(VarName)], Str)
    ;
        WhyNots = [HeadWhyNot | TailWhyNots],
        (
            TailWhyNots = [],
            HeadWhyNot = why_var_is_not_value(_ValueId, _ReqId, ReqDesc,
                _SubTree),
            % solver_var_value_name(ValueName, ValueId),
            Str = CurIndentStr ++ ReqDesc ++ "\n"
        ;
            TailWhyNots = [_ | _],
            HeadStr = failure_tree_why_not_to_string(CurIndentStr,
                EachIndentStr, VarId, VarName, HeadWhyNot),
            TailStrs = list.map(
                failure_tree_why_not_to_string(CurIndentStr, EachIndentStr,
                    VarId, VarName),
                TailWhyNots),
            Str = string.append_list([HeadStr | TailStrs])
        )
    ).

:- func failure_tree_why_not_to_string(string, string, solver_var_id,
    string, why_var_is_not_value) = string.

failure_tree_why_not_to_string(CurIndentStr, _EachIndentStr, _VarId, VarName,
        WhyNot) = Str :-
    WhyNot = why_var_is_not_value(ValueId, _ReqId, ReqDesc, _SubTree),
    solver_var_value_name(ValueName, ValueId),
    string.format("%s%s may not be %s because %s\n",
        [s(CurIndentStr), s(VarName), s(ValueName), s(ReqDesc)], Str).

/*
:- type failure_tree
    --->    failure_tree(
                solver_var_id,
                list(why_var_is_not_value)
            ).

:- type why_var_is_not_value
    --->    why_var_is_not_value(
                solver_var_value_id,
                requirement_id,
                string,
                failure_tree
            ).
*/

:- func success_value_to_str(string,
    pair(solver_var_id, solver_var_value_id)) = string.

success_value_to_str(Prefix, VarId - ValueId) = Str :-
    solver_var_name(VarName, VarId),
    solver_var_value_name(ValueName, ValueId),
    string.format("%s%s = %s\n", [s(Prefix), s(VarName), s(ValueName)], Str).

%---------------------------------------------------------------------------%

:- type failure_info
    --->    failure_info(
                list(requirement),
                solver_var_map,
                one_or_more(solver_var_id)
            ).

failure_info_to_failure_trees(FailureInfo) = FailureTrees :-
    FailureInfo = failure_info(_Requirements, _SolverVarMap, ZeroCntVarIds),
    ZeroCntVarIds = one_or_more(HeadZeroCntVarId, TailZeroCntVarIds),
    zero_count_var_to_failure_tree(FailureInfo,
        HeadZeroCntVarId, HeadFailureTree),
    list.map(zero_count_var_to_failure_tree(FailureInfo),
        TailZeroCntVarIds, TailFailureTrees),
    FailureTrees = one_or_more(HeadFailureTree, TailFailureTrees).

:- pred zero_count_var_to_failure_tree(failure_info::in,
    solver_var_id::in, failure_tree::out) is det.

zero_count_var_to_failure_tree(FailureInfo, ZeroCntVarId, FailureTree) :-
    FailureInfo = failure_info(_Requirements, SolverVarMap, _ZeroCntVarIds),
    map.lookup(SolverVarMap, ZeroCntVarId, SolverVar),
    SolverVar = solver_var(_CntAll, _CntPoss, Values),
    list.foldl(accumulate_why_var_is_not_values(FailureInfo, ZeroCntVarId),
        Values, [], RevWhyNots),
    list.reverse(RevWhyNots, WhyNots),
    FailureTree = failure_tree(ZeroCntVarId, WhyNots).

:- pred accumulate_why_var_is_not_values(failure_info::in,
    solver_var_id::in, solver_var_value::in,
    list(why_var_is_not_value)::in, list(why_var_is_not_value)::out) is det.

accumulate_why_var_is_not_values(_FailureInfo, _VarId, VarValue,
        !RevWhyNots) :-
    VarValue = solver_var_value(ValueId, Possible),
    (
        Possible = is_possible,
        unexpected($pred, "is_possible")
    ;
        Possible = not_possible(NotPossibleWhy),
        (
            NotPossibleWhy = npw_user
            % Nothing to report; the user should already know
            % why the solver var cannot be this value.
        ;
            NotPossibleWhy = npw_config
            % XXX Should we report this, and if yes, how?
        ;
            NotPossibleWhy = npw_labeling,
            % We should get here only if labelling has converted 
            % a solvable problem into an unsolvable one.
            unexpected($pred, "npw_labeling")
        ;
            NotPossibleWhy = npw_requirement(ReqAppl),
            ReqAppl = requirement_application(ReqId, ReqDesc, _ReqDir),
            SubTrees = [],
            WhyNot = why_var_is_not_value(ValueId, ReqId, ReqDesc, SubTrees),
            !:RevWhyNots = [WhyNot | !.RevWhyNots]
        )
    ).

%---------------------------------------------------------------------------%
:- end_module grade_solver.
%---------------------------------------------------------------------------%
