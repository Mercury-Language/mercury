%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: simplify_goal_conj.m.
%
% This module handles simplification of both plain and parallel conjunctions.
%
%---------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_conj.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.

:- import_module list.

    % Handle simplification of plain conjunctions.
    %
:- pred simplify_goal_plain_conj(list(hlds_goal)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Handle simplification of parallel conjunctions.
    %
:- pred simplify_goal_parallel_conj(list(hlds_goal)::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.simplify.simplify_goal.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module libs.
:- import_module libs.trace_params.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_db.
:- import_module parse_tree.var_table.

:- import_module bool.
:- import_module cord.
:- import_module int.
:- import_module map.
:- import_module one_or_more.
:- import_module set.
:- import_module string.
:- import_module uint.

%---------------------------------------------------------------------------%

simplify_goal_plain_conj(Goals0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, !Common, !Info) :-
    ( if simplify_do_excess_assign(!.Info) then
        excess_assigns_in_conj(GoalInfo0, Goals0, Goals1, !Info)
    else
        Goals1 = Goals0
    ),
    simplify_conj(cord.empty, Goals1, Goals, GoalInfo0,
        NestedContext0, InstMap0, !Common, !Info),
    (
        Goals = [],
        Context = goal_info_get_context(GoalInfo0),
        hlds_goal(GoalExpr, GoalInfo) = true_goal_with_context(Context)
    ;
        Goals = [SingleGoal],
        % A singleton conjunction is equivalent to the goal itself.
        SingleGoal = hlds_goal(SingleGoalExpr, SingleGoalInfo),
        simplify_maybe_wrap_goal(GoalInfo0, SingleGoalInfo, SingleGoalExpr,
            GoalExpr, GoalInfo, !Info)
    ;
        Goals = [_, _ | _],
        % Conjunctions that cannot produce solutions may nevertheless contain
        % nondet and multi goals. If this happens, we put the conjunction
        % inside a commit scope, since the code generators need to know
        % where they should change the code's execution mechanism.
        Detism = goal_info_get_determinism(GoalInfo0),
        ( if
            % The condition that we expect to fail most frequently is first.
            determinism_components(Detism, CanFail, at_most_zero),
            simplify_do_mark_code_model_changes(!.Info),
            contains_multisoln_goal(Goals)
        then
            determinism_components(InnerDetism, CanFail, at_most_many),
            goal_info_set_determinism(InnerDetism, GoalInfo0, InnerInfo),
            InnerGoal = hlds_goal(conj(plain_conj, Goals), InnerInfo),
            GoalExpr = scope(commit(dont_force_pruning), InnerGoal),
            % We may have deleted goals that contain what could be the
            % last references to variables. This may require adjustments
            % to the nonlocals sets of not only this goal, but of the other
            % goals containing it. Likewise, it may require adjustments
            % of the instmap_deltas of such containing goals.
            simplify_info_set_rerun_quant_instmap_delta(!Info)
        else
            GoalExpr = conj(plain_conj, Goals)
        ),
        GoalInfo = GoalInfo0
    ),
    trace [compile_time(flag("debug_simplify_conj")), io(!IO)] (
        simplify_info_get_module_info(!.Info, ModuleInfo),
        get_debug_output_stream(ModuleInfo, Stream, !IO),
        simplify_info_get_var_table(!.Info, VarTable),
        simplify_info_get_tvarset(!.Info, TVarSet),
        simplify_info_get_inst_varset(!.Info, InstVarSet),
        VarNameSrc = vns_var_table(VarTable),
        DumpGoalNl = dump_goal_nl(Stream, ModuleInfo, VarNameSrc,
            TVarSet, InstVarSet),
        io.write_string(Stream, "\n------------------------\n", !IO),
        io.write_string(Stream, "\nBEFORE SIMPLIFY_GOAL_PLAIN_CONJ\n\n", !IO),
        list.foldl(DumpGoalNl, Goals0, !IO),
        io.write_string(Stream, "\nAFTER EXCESS ASSIGN\n\n", !IO),
        list.foldl(DumpGoalNl, Goals1, !IO),
        io.write_string(Stream, "\nAFTER SIMPLIFY_CONJ\n\n", !IO),
        list.foldl(DumpGoalNl, Goals, !IO),
        io.flush_output(Stream, !IO)
    ).

:- pred contains_multisoln_goal(list(hlds_goal)::in) is semidet.

contains_multisoln_goal([Goal | Goals]) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, _, SolnCount),
    ( if SolnCount = at_most_many then
        true
    else
        contains_multisoln_goal(Goals)
    ).

%---------------------------------------------------------------------------%

:- pred simplify_conj(cord(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out, hlds_goal_info::in,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

simplify_conj(!.PrevGoals, [], Goals, _ConjInfo,
        _NestedContext0, _InstMap0, !Common, !Info) :-
    Goals = cord.list(!.PrevGoals).
simplify_conj(!.PrevGoals, [HeadGoal0 | TailGoals0], Goals, ConjInfo,
        NestedContext0, InstMap0, !Common, !Info) :-
    % Flatten nested conjunctions in the original code.
    ( if HeadGoal0 = hlds_goal(conj(plain_conj, HeadSubGoals0), _) then
        HeadTailGoals1 = HeadSubGoals0 ++ TailGoals0,
        simplify_conj(!.PrevGoals, HeadTailGoals1, Goals, ConjInfo,
            NestedContext0, InstMap0, !Common, !Info)
    else
        Common0 = !.Common,
        Info0 = !.Info,
        simplify_goal(HeadGoal0, HeadGoal1, NestedContext0, InstMap0,
            !Common, !Info),
        HeadGoal1 = hlds_goal(HeadGoalExpr1, HeadGoalInfo1),
        ( if
            % Flatten nested conjunctions in the transformed code.
            HeadGoalExpr1 = conj(plain_conj, HeadSubGoals1)
        then
            % Note that this simplifies everything inside Goal1 AGAIN.
            % We want some of this (for example, structures recorded
            % in Common while processing HeadSubGoals1 can sometimes be used
            % to optimize TailGoals0), but probably most of the work done
            % by this resimplification is wasted.
            %
            % XXX Look for a way to test for the simplifications enabled
            % by the change from HeadGoal0 to HeadGoal1, without trying to redo
            % simplifications unaffected by that change.
            % For example, we could record the goal_ids of goals that
            % simplify.m left untouched in this pass, and return immediately
            % if asked to simplify them again. Unfortunately, just figuring
            % out which goals are touched and which are not is not easy.
            % (Due to the rebuilding of hlds_goal structures from exprs and
            % infos, the address may change even if the content does not.)
            HeadTailGoals1 = HeadSubGoals1 ++ TailGoals0,
            % Note that we cannot process HeadTailGoals1 using the Common
            % derived from processing HeadGoal0. If we did that, and HeadGoal0
            % contained X = f(Y1), then Common would remember that,
            % and when processing HeadTailGoals1, simplify_conj would replace
            % that unification with X := X, thus "defining" X in terms of X.
            !:Common = Common0,
            simplify_conj(!.PrevGoals, HeadTailGoals1, Goals, ConjInfo,
                NestedContext0, InstMap0, !Common, !Info)
        else
            update_instmap(HeadGoal1, InstMap0, InstMap1),
            ( if
                % Delete unreachable goals.
                (
                    % This test is here mostly for the sake of completeness.
                    % It rarely finds anything to delete, because
                    % - we get InstMap1 mostly from Goal1's instmap delta,
                    % - the delta is created during mode analysis, and
                    % - mode analysis itself deletes the unreachable conjuncts
                    %   after a conjunct whose instmap delta is unreachable.
                    instmap_is_unreachable(InstMap1)
                ;
                    Detism1 = goal_info_get_determinism(HeadGoalInfo1),
                    determinism_components(Detism1, _, at_most_zero)
                )
            then
                HeadGoal0 = hlds_goal(_, HeadGoalInfo0),
                HeadGoalContext0 = goal_info_get_context(HeadGoalInfo0),
                delete_tail_unreachable_goals(!.PrevGoals, HeadGoalContext0,
                    HeadGoal1, TailGoals0, Goals, !Info),
                % We have deleted goals that contain what could be the last
                % references to variables. This may require adjustments
                % to the nonlocals sets of not only this goal, but of the
                % other goals containing it. Likewise, it may require
                % adjustments of the instmap_deltas of such containing goals.
                simplify_info_set_rerun_quant_instmap_delta(!Info)
            else
                ( if
                    simplify_do_merge_code_after_switch(!.Info),
                    HeadGoal1 = hlds_goal(HeadGoalExpr1, HeadGoalInfo1),
                    HeadGoalExpr1 = switch(_, _, _),
                    TailGoals0 = [HeadTailGoal0 | TailTailGoals0],
                    HeadTailGoal0 =
                        hlds_goal(HeadTailGoalExpr0, HeadTailGoalInfo0),
                    ( HeadTailGoalExpr0 = unify(_, _, _, _, _)
                    ; HeadTailGoalExpr0 = switch(_, _, _)
                    )
                then
                    (
                        HeadTailGoalExpr0 = unify(_, _, _, _, _),
                        try_to_merge_unify_after_switch(!.Info, ConjInfo,
                            HeadGoalExpr1, HeadGoalInfo1,
                            HeadTailGoalExpr0, HeadTailGoalInfo0,
                            TailTailGoals0, MergeResult)
                    ;
                        HeadTailGoalExpr0 = switch(_, _, _),
                        % Invoking try_to_merge_switch_after_switch with
                        % HeadGoalExpr1/HeadGoalInfo1 leads to Mantis bug #567.
                        % The reason for that is that the call to
                        % try_to_merge_switch_after_switch can return
                        % either merge_unsuccessful or
                        % merge_successful_new_code_not_simplified.
                        % In the latter case, we start again with Common0,
                        % which is from *before* we simplified HeadGoal0
                        % into HeadGoal1. This effectively throws away
                        % any updates made to !Common since we took Common0
                        % as a snapshot.
                        %
                        % If HeadGoal0 contains some code that constructs
                        % some ground terms, then the call to simplify_goal
                        % above can replace that code with a reference to
                        % a ground_term_const. By keeping the reference
                        % to this newly allocated ground_term_const in
                        % HeadGoal1 but throwing away any reference to it
                        % in !.Common, the reference becomes dangling.
                        % It can result in a compiler abort, if the code
                        % generator goes to look up the value of the
                        % ground_term_const and does not find it, or
                        % it can result in the dangling ground_term_const's
                        % id number being reused to hold some other ground
                        % term, resulting in the dangling reference being
                        % quietly redirected to another ground term, which is
                        % extremely unlikely to have the right value and is
                        % not even all that likely to have the right type.
                        % (In tests/hard_coded/bug567.m, the ground_term_const
                        % id is reused, for a term of the right type but wrong
                        % value.)
                        HeadGoal0 = hlds_goal(HeadGoalExpr0, HeadGoalInfo0),
                        ( if HeadGoalExpr0 = switch(_, _, _) then
                            try_to_merge_switch_after_switch(!.Info,
                                HeadGoalExpr0, HeadGoalInfo0,
                                HeadTailGoalExpr0, HeadTailGoalInfo0,
                                MergeResult)
                        else
                            MergeResult = merge_unsuccessful
                        )
                    ),
                    (
                        MergeResult = merge_unsuccessful,
                        !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal1),
                        TailGoals1 = TailGoals0,
                        TailInstMap = InstMap1
                    ;
                        MergeResult = merge_successful_new_code_simplified(
                            HeadGoal2, !:Info),
                        !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal2),
                        % Let the recursive call to simplify_conj below
                        % process TailTailGoals0 instead of TailGoals0,
                        % since the effect of HeadTailGoal0 has now been
                        % folded into HeadGoal2.
                        TailGoals1 = TailTailGoals0,
                        TailInstMap = InstMap1
                    ;
                        MergeResult = merge_successful_new_code_not_simplified(
                            HeadGoal2),
                        % HeadGoal2 replaces both HeadGoal1 and HeadTailGoal0
                        % in front of TailTailGoals0. We have processed
                        % the HeadGoal1 part of HeadGoal2, but not the
                        % HeadTailGoal0 part, so we must process HeadGoal2
                        % again. That means starting again, with the initial
                        % versions of the instmap, the common structure,
                        % and the simplify_info structure.
                        TailGoals1 = [HeadGoal2 | TailTailGoals0],
                        TailInstMap = InstMap0,
                        !:Common = Common0,
                        !:Info = Info0,
                        % We do know that the merge of the two switches
                        % requires the recomputation of nonlocals sets and
                        % of instmap deltas, and that sometimes the
                        % recomputed determinisms can be more precise as well.
                        simplify_info_set_rerun_quant_instmap_delta(!Info),
                        simplify_info_set_rerun_det(!Info)
                    )
                else
                    !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal1),
                    TailGoals1 = TailGoals0,
                    TailInstMap = InstMap1
                ),
                simplify_conj(!.PrevGoals, TailGoals1, Goals, ConjInfo,
                    NestedContext0, TailInstMap, !Common, !Info)
            )
        )
    ).

%---------------------%

:- pred delete_tail_unreachable_goals(cord(hlds_goal)::in,
    prog_context::in, hlds_goal::in, list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_info::in, simplify_info::out) is det.

delete_tail_unreachable_goals(!.PrevGoals, HeadGoalContext0, HeadGoal1,
        TailGoals0, Goals, !Info) :-
    simplify_info_get_deleted_call_callees(!.Info, DeletedCallCallees0),
    SubGoalCalledProcs = goals_proc_refs(TailGoals0),
    set.union(SubGoalCalledProcs, DeletedCallCallees0, DeletedCallCallees),
    simplify_info_set_deleted_call_callees(DeletedCallCallees, !Info),

    !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal1),
    ( if
        ( HeadGoal1 = hlds_goal(disj([]), _)
        ; TailGoals0 = []
        )
    then
        % XXX If TailGoals0 = [], why don't we add an explicit failure?
        true
    else
        % We insert an explicit failure at the end of the non-succeeding
        % conjunction. This is necessary, since the unreachability of the
        % instmap could have been derived using inferred determinism
        % information. Without the explicit fail goal, mode errors could
        % result if mode analysis is rerun, since according to the language
        % specification, mode analysis does not use inferred determinism
        % information when deciding what can never succeed.
        FailGoal = fail_goal_with_context(HeadGoalContext0),
        !:PrevGoals = cord.snoc(!.PrevGoals, FailGoal)
    ),
    Goals = cord.list(!.PrevGoals).

%---------------------%

:- type merge_code_after_switch_result
    --->    merge_unsuccessful
    ;       merge_successful_new_code_simplified(
                hlds_goal,
                simplify_info
            )
    ;       merge_successful_new_code_not_simplified(
                hlds_goal
            ).

    % Look for situations like this:
    %
    %   (
    %       ( X = a
    %       ; X = b
    %       ),
    %       Result = yes
    %   ;
    %       ( X = c
    %       ; X = d(_)
    %       ),
    %       Result = no
    %   ),
    %   Result = no
    %
    % and transform them into
    %
    %   ( X = a
    %   ; X = b
    %   )
    %
    % The idea is to avoid the performance overhead of setting Result
    % in such switches and testing it afterward.
    %
    % The point of switches like this, in which each arm does nothing
    % except set a flag that is tested after the switch, is that if the
    % type of X gets a new functor added to it, they get a message if
    %
    % - either the --inform-incomplete-switch option is given, or
    % - the switch is wrapped in a require_complete_switch scope.
    %
    % In the latter case, the simplification of the scope containing the
    % switch will remove the scope wrapper.
    %
:- pred try_to_merge_unify_after_switch(simplify_info::in, hlds_goal_info::in,
    hlds_goal_expr::in(goal_expr_switch), hlds_goal_info::in,
    hlds_goal_expr::in(goal_expr_unify), hlds_goal_info::in,
    list(hlds_goal)::in, merge_code_after_switch_result::out) is det.

try_to_merge_unify_after_switch(!.Info, ConjInfo,
        HeadGoalExpr1, HeadGoalInfo1,
        HeadTailGoalExpr0, _HeadTailGoalInfo0, TailTailGoals0, Result) :-
    HeadGoalExpr1 = switch(SwitchVar, SwitchCanFail1, Cases1),
    HeadTailGoalExpr0 = unify(_LHSVar, _RHS, _UniMode,
        Unification, _UnifyContext),
    ( if
        Unification = deconstruct(TestVar, TestConsId, TestArgs, _ArgModes,
            DeconstructCanFail, _CanCGC),
        TestArgs = [],
        DeconstructCanFail = can_fail,
        all_cases_construct_test_var(Cases1, TestVar, TestConsId,
            [], RevTruncatedSameCases, [], RevDiffCases),

        % If the procedure body can refer to TestVar anywhere other than
        % in HeadGoal0 or HeadTailGoal0, then we cannot eliminate the
        % assignment to TestVar. Since the mode system does not permit
        % conjuncts before HeadGoal0 to refer to TestVar, the places
        % we need to check are the conjuncts after HeadTailGoal0 and
        % the code outside the conjunction as a whole.
        %
        % If there are outside references to TestVar, we could still delete
        % RevDiffCases from the switch, while keeping the assignment of
        % TestConsId to TestVar, either in the non-truncated originals
        % of the RevTruncatedSameCases, or in a construct unification
        % after the switch that would replace the original deconstruction
        % in HeadTailGoal0. However, a bootcheck found no situations
        % with outside references to TestVar, so that situation is probably
        % too rare to be worth trying to optimize.

        ConjNonLocals = goal_info_get_nonlocals(ConjInfo),
        not set_of_var.contains(ConjNonLocals, TestVar),
        no_conjunct_refers_to_var(TailTailGoals0, TestVar)
    then
        (
            RevDiffCases = [],
            TruncatedSwitchCanFail = SwitchCanFail1
        ;
            RevDiffCases = [_ | _],
            TruncatedSwitchCanFail = can_fail
        ),
        list.reverse(RevTruncatedSameCases, TruncatedSameCases),
        HeadGoalExpr2 =
            switch(SwitchVar, TruncatedSwitchCanFail, TruncatedSameCases),
        HeadGoal2 = hlds_goal(HeadGoalExpr2, HeadGoalInfo1),
        % We need to update the determinism fields of the goals.
        % We could try to do that here, but it is simpler to use
        % the existing code for the job.
        simplify_info_set_rerun_det(!Info),
        Result = merge_successful_new_code_simplified(HeadGoal2, !.Info)
    else
        Result = merge_unsuccessful
    ).

    % See the comment above the call to this predicate.
    %
:- pred all_cases_construct_test_var(list(case)::in, prog_var::in, cons_id::in,
    list(case)::in, list(case)::out, list(case)::in, list(case)::out)
    is semidet.

all_cases_construct_test_var([], _, _, !RevTruncatedSameCases, !RevDiffCases).
all_cases_construct_test_var([Case | Cases], TestVar, TestConsId,
        !RevTruncatedSameCases, !RevDiffCases) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    GoalExpr = unify(_LHSVar, _RHS, _UniMode, Unification, _UnifyContext),
    Unification = construct(TestVar, CaseConsId, CaseArgs,
        _ArgModes, _HowToConstruct, _Unique, _SubInfo),
    ( if
        CaseConsId = TestConsId,
        CaseArgs = []
    then
        Context = goal_info_get_context(GoalInfo),
        TrueGoal = true_goal_with_context(Context),
        TruncatedCase = case(MainConsId, OtherConsIds, TrueGoal),
        !:RevTruncatedSameCases = [TruncatedCase | !.RevTruncatedSameCases]
    else
        !:RevDiffCases = [Case | !.RevDiffCases]
    ),
    all_cases_construct_test_var(Cases, TestVar, TestConsId,
        !RevTruncatedSameCases, !RevDiffCases).

:- pred no_conjunct_refers_to_var(list(hlds_goal)::in, prog_var::in)
    is semidet.

no_conjunct_refers_to_var([], _).
no_conjunct_refers_to_var([Goal | Goals], TestVar) :-
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo),
    not set_of_var.contains(NonLocals, TestVar),
    no_conjunct_refers_to_var(Goals, TestVar).

%---------------------%

:- inst merge_switch_switch for merge_code_after_switch_result/0
    --->    merge_unsuccessful
    ;       merge_successful_new_code_not_simplified(ground).

    % Look for situations like this:
    %
    %   (
    %       ( X = a
    %       ; X = b
    %       ),
    %       ... code fragment 1 ...
    %   ;
    %       ( X = c
    %       ; X = d(_)
    %       ),
    %       ... code fragment 2 ...
    %   ),
    %   (
    %       ( X = a
    %       ; X = b
    %       ),
    %       ... code fragment 3 ...
    %   ;
    %       X = c
    %       ... code fragment 4 ...
    %   ;
    %       X = d(_)
    %       ... code fragment 5 ...
    %   ),
    %
    % and transform them into
    %
    %   (
    %       ( X = a
    %       ; X = b
    %       ),
    %       ... code fragment 1 ...
    %       ... code fragment 3 ...
    %   ;
    %       X = c,
    %       ... code fragment 2 ...
    %       ... code fragment 4 ...
    %   ;
    %       X = d(_),
    %       ... code fragment 2 ...
    %       ... code fragment 5 ...
    %   ),
    %
    % The idea is that the one merged switch should require fewer branch
    % instructions, and therefore less time, than the two original switches,
    % if the two original switches have any commonality at all.
    %
:- pred try_to_merge_switch_after_switch(simplify_info::in,
    hlds_goal_expr::in(goal_expr_switch), hlds_goal_info::in,
    hlds_goal_expr::in(goal_expr_switch), hlds_goal_info::in,
    merge_code_after_switch_result::out(merge_switch_switch)) is det.

try_to_merge_switch_after_switch(!.Info, FirstGoalExpr, FirstGoalInfo,
        SecondGoalExpr0, SecondGoalInfo0, Result) :-
    FirstGoalExpr = switch(FirstSwitchVar, FirstSwitchCanFail, FirstCases),
    SecondGoalExpr0 =
        switch(SecondSwitchVar, SecondSwitchCanFail, SecondCases),
    ( if
        SecondSwitchVar = FirstSwitchVar,
        build_maps_first_switch(FirstCases, 1u,
            map.init, FirstSwitchGoalMap,
            map.init, FirstSwitchConsIdMap),
        build_maps_second_switch(SecondCases, 1u, FirstSwitchConsIdMap,
            map.init, SecondSwitchGoalMap, map.init, CasesConsIdsMap),
        list.length(FirstCases, FirstSwitchNumCases),
        list.length(SecondCases, SecondSwitchNumCases),
        map.count(CasesConsIdsMap, MergedNumCases),
        MergedNumCases < FirstSwitchNumCases * SecondSwitchNumCases
    then
        ( if
            FirstSwitchCanFail = cannot_fail,
            SecondSwitchCanFail = cannot_fail
        then
            CanFail = cannot_fail
        else
            CanFail = can_fail
        ),
        map.foldl(
            construct_and_add_merged_switch_case(!.Info,
                FirstSwitchGoalMap, SecondSwitchGoalMap),
            CasesConsIdsMap, [], Cases0),
        list.sort(Cases0, Cases),
        GoalExpr = switch(FirstSwitchVar, CanFail, Cases),
        compute_goal_info_for_merged_goal(FirstGoalInfo, SecondGoalInfo0,
            GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),
        Result = merge_successful_new_code_not_simplified(Goal)
    else
        Result = merge_unsuccessful
    ).

%---------------------%

:- pred build_maps_first_switch(list(case)::in, uint::in,
    map(uint, hlds_goal)::in, map(uint, hlds_goal)::out,
    map(cons_id, uint)::in, map(cons_id, uint)::out) is det.

build_maps_first_switch([], _, !FirstSwitchGoalMap, !FirstSwitchConsIdMap).
build_maps_first_switch([Case | Cases], CurCaseNum,
        !FirstSwitchGoalMap, !FirstSwitchConsIdMap) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    map.det_insert(CurCaseNum, Goal, !FirstSwitchGoalMap),
    map_rev_insert(CurCaseNum, MainConsId, !FirstSwitchConsIdMap),
    list.foldl(map_rev_insert(CurCaseNum), OtherConsIds,
        !FirstSwitchConsIdMap),
    build_maps_first_switch(Cases, CurCaseNum + 1u,
        !FirstSwitchGoalMap, !FirstSwitchConsIdMap).

:- pred map_rev_insert(V::in, K::in, map(K, V)::in, map(K, V)::out) is det.

map_rev_insert(Value, Key, !Map) :-
    map.det_insert(Key, Value, !Map).

:- pred build_maps_second_switch(list(case)::in, uint::in,
    map(cons_id, uint)::in,
    map(uint, hlds_goal)::in, map(uint, hlds_goal)::out,
    map({uint, uint}, one_or_more(cons_id))::in,
    map({uint, uint}, one_or_more(cons_id))::out) is det.

build_maps_second_switch([], _, _, !SecondSwitchGoalMap, !CasesConsIdsMap).
build_maps_second_switch([Case | Cases], CurCaseNum, FirstSwitchConsIdMap,
        !SecondSwitchGoalMap, !CasesConsIdsMap) :-
    Case = case(MainConsId, OtherConsIds, Goal),
    map.det_insert(CurCaseNum, Goal, !SecondSwitchGoalMap),
    build_maps_second_switch_cons_id(FirstSwitchConsIdMap, CurCaseNum,
        MainConsId, !CasesConsIdsMap),
    list.foldl(
        build_maps_second_switch_cons_id(FirstSwitchConsIdMap, CurCaseNum),
        OtherConsIds, !CasesConsIdsMap),
    build_maps_second_switch(Cases, CurCaseNum + 1u, FirstSwitchConsIdMap,
        !SecondSwitchGoalMap, !CasesConsIdsMap).

:- pred build_maps_second_switch_cons_id(map(cons_id, uint)::in, uint::in,
    cons_id::in,
    map({uint, uint}, one_or_more(cons_id))::in,
    map({uint, uint}, one_or_more(cons_id))::out) is det.

build_maps_second_switch_cons_id(FirstSwitchConsIdMap, SecondSwitchCaseNum,
        ConsId, !CasesConsIdsMap) :-
    map.lookup(FirstSwitchConsIdMap, ConsId, FirstSwitchCaseNum),
    CaseNums = {FirstSwitchCaseNum, SecondSwitchCaseNum},
    ( if map.search(!.CasesConsIdsMap, CaseNums, OldConsIds) then
        NewConsIds = one_or_more.cons(ConsId, OldConsIds),
        map.det_update(CaseNums, NewConsIds, !CasesConsIdsMap)
    else
        map.det_insert(CaseNums, one_or_more(ConsId, []), !CasesConsIdsMap)
    ).

%---------------------%

:- pred construct_and_add_merged_switch_case(simplify_info::in,
    map(uint, hlds_goal)::in, map(uint, hlds_goal)::in,
    {uint, uint}::in, one_or_more(cons_id)::in,
    list(case)::in, list(case)::out) is det.

construct_and_add_merged_switch_case(Info,
        FirstSwitchGoalMap, SecondSwitchGoalMap,
        {FirstSwitchCaseNum, SecondSwitchCaseNum}, OoMConsIds, !Cases) :-
    one_or_more.sort(OoMConsIds, SortedOoMConsIds),
    SortedOoMConsIds = one_or_more(MainConsId, OtherConsIds),
    map.lookup(FirstSwitchGoalMap, FirstSwitchCaseNum, FirstGoal),
    map.lookup(SecondSwitchGoalMap, SecondSwitchCaseNum, SecondGoal),
    FirstGoal = hlds_goal(FirstGoalExpr, FirstGoalInfo),
    SecondGoal = hlds_goal(SecondGoalExpr, SecondGoalInfo),
    ( if FirstGoalExpr = conj(plain_conj, FirstSubGoals) then
        FirstGoals = FirstSubGoals
    else
        FirstGoals = [FirstGoal]
    ),
    ( if SecondGoalExpr = conj(plain_conj, SecondSubGoals) then
        SecondGoals = SecondSubGoals
    else
        SecondGoals = [SecondGoal]
    ),
    GoalExpr = conj(plain_conj, FirstGoals ++ SecondGoals),
    compute_goal_info_for_merged_goal(FirstGoalInfo, SecondGoalInfo, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo),

    trace [compile_time(flag("simplify_merge_switch")), io(!IO)] (
        io.stderr_stream(StdErr, !IO),
        simplify_info_get_module_info(Info, ModuleInfo),
        simplify_info_get_var_table(Info, VarTable),
        VarNameSrc = vns_var_table(VarTable),
        simplify_info_get_tvarset(Info, TVarSet),
        simplify_info_get_inst_varset(Info, InstVarSet),

        io.write_string(StdErr, "\nFirstGoal\n\n", !IO),
        dump_goal_nl(StdErr, ModuleInfo, VarNameSrc, TVarSet, InstVarSet,
            FirstGoal, !IO),
        io.write_string(StdErr, "\nSecondGoal\n\n", !IO),
        dump_goal_nl(StdErr, ModuleInfo, VarNameSrc, TVarSet, InstVarSet,
            SecondGoal, !IO),
        io.write_string(StdErr, "\nMERGED GOAL\n\n", !IO),
        dump_goal_nl(StdErr, ModuleInfo, VarNameSrc, TVarSet, InstVarSet,
            Goal, !IO)
    ),

    Case = case(MainConsId, OtherConsIds, Goal),
    !:Cases = [Case | !.Cases].

:- pred compute_goal_info_for_merged_goal(
    hlds_goal_info::in, hlds_goal_info::in, hlds_goal_info::out) is det.

compute_goal_info_for_merged_goal(FirstGoalInfo, SecondGoalInfo, !:GoalInfo) :-
    FirstDetism =  goal_info_get_determinism(FirstGoalInfo),
    SecondDetism = goal_info_get_determinism(SecondGoalInfo),
    FirstPurity =  goal_info_get_purity(FirstGoalInfo),
    SecondPurity = goal_info_get_purity(SecondGoalInfo),
    FirstInstMapDelta =  goal_info_get_instmap_delta(FirstGoalInfo),
    SecondInstMapDelta = goal_info_get_instmap_delta(SecondGoalInfo),
    FirstNonLocals =  goal_info_get_nonlocals(FirstGoalInfo),
    SecondNonLocals = goal_info_get_nonlocals(SecondGoalInfo),
    FirstFeatures =  goal_info_get_features(FirstGoalInfo),
    SecondFeatures = goal_info_get_features(SecondGoalInfo),

    % When we are computing the goal_info for the merged switch as a whole,
    % it is possible for Detism to be too conservative, though still correct.
    %
    % Consider a situation where
    %
    % - the first switch is det, having N det arms and one erroneous arm.
    % - the second switch is semidet, having N det arms and one semidet arm;
    %
    % The conjunction of the two switches will be considered semidet.
    % However, in the process of merging the two switches, we may discover
    % that in the merged switch, the erroneous arm of the first switch
    % is always followed by the semidet arm of the second switch.
    % The determinism of the combined arm will then be erroneous.
    % Since execution cannot reach the end of that combined arm,
    % and since all the combined arms whose ends *can* be reached are det,
    % the determinism of the merged switch is actually det.
    %
    % This over-conservatism should not affect the rest of the simplification
    % pass, and code higher up in the call tree will ask determinism analysis
    % to be rerun to provide accurate info for later passes.
    det_conjunction_detism(FirstDetism, SecondDetism, Detism),
    Purity = worst_purity(FirstPurity, SecondPurity),
    instmap_delta_apply_instmap_delta(FirstInstMapDelta, SecondInstMapDelta,
        test_size, InstMapDelta),
    % The NonLocals we compute here may be an overestimate, since it is
    % possible for a variable to occur ONLY in FirstGoal and in SecondGoal.
    % The code of the simplification pass should not mind this, and code
    % higher up in the call tree will ask for quantification to be rerun
    % to provide accurate info for later passes.
    set_of_var.union(FirstNonLocals, SecondNonLocals, NonLocals),
    set.union(FirstFeatures, SecondFeatures, Features),

    % It does not matter whether we take FirstGoalInfo or SecondGoalInfo
    % as the basis for GoalInfo, because we explicitly set up all of
    % the fields that the rest of the compiler invocation can pay attention to.
    %
    % - The egi_context field is not used by the rest of the simplification
    %   pass, because it belongs to a compound goal (either a merged
    %   conjunction or a merged switch) that we don't generate warnings for.
    %   And all the compiler passes after the first invocation of
    %   simplification that generate error or warning messages do so for
    %   for either procedures or calls to procedures, not compound goals.
    %
    % - The gi_goal_id and egi_rev_goal_path fields are considered valid
    %   only bwtween a pass that fills them in, and the next pass that
    %   can make any changes to the procedure's body goal. Simplification
    %   can make changes, so it would invalidate those fields even if
    %   we did merge switches.
    %
    % - The egi_goal_mode and egi_maybe_mode_constr field are not yet used.
    %
    % - The gi_code_gen_info field is used only by the LLDS backend,
    %   It is filled in only after the last invocation of simplification.
    %
    % - the egi_ho_value_map field is filled in by the closure analysis pass,
    %   and read by the exception analysis and termination passes. There are
    %   no invocations of the simplification pass between those passes.
    %
    % - the egi_maybe_ctgc, egi_maybe_rbmm and egi_maybe_dp fields
    %   are used only within their respective passes, and are not meaningful
    %   outside those passes.
    !:GoalInfo = FirstGoalInfo,
    goal_info_set_determinism(Detism, !GoalInfo),
    goal_info_set_purity(Purity, !GoalInfo),
    goal_info_set_instmap_delta(InstMapDelta, !GoalInfo),
    goal_info_set_nonlocals(NonLocals, !GoalInfo),
    goal_info_set_features(Features, !GoalInfo).

%---------------------------------------------------------------------------%

% XXX move below its callers

:- type var_renaming == map(prog_var, prog_var).

:- pred find_renamed_var(var_renaming::in, prog_var::in, prog_var::out) is det.

find_renamed_var(Subn, Var0, Var) :-
    ( if map.search(Subn, Var0, Var1) then
        find_renamed_var(Subn, Var1, Var)
    else
        Var = Var0
    ).

    % Collapse chains of renamings.
    %
:- pred renaming_transitive_closure(var_renaming::in, var_renaming::out)
    is det.

renaming_transitive_closure(VarRenaming0, VarRenaming) :-
    map.map_values_only(find_renamed_var(VarRenaming0),
        VarRenaming0, VarRenaming).

%---------------------------------------------------------------------------%

:- pred excess_assigns_in_conj(hlds_goal_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_info::in, simplify_info::out) is det.

excess_assigns_in_conj(ConjInfo, Goals0, Goals, !Info) :-
    ConjNonLocals = goal_info_get_nonlocals(ConjInfo),
    map.init(Subn0),
    find_excess_assigns_in_conj(!.Info, ConjNonLocals, Goals0,
        [], RevGoals, Subn0, Subn1),
    ( if map.is_empty(Subn1) then
        Goals = Goals0
    else
        list.reverse(RevGoals, Goals1),

        % NOTE Instead of doing the renaming here, we could assign
        % a unique goal id to the conjunction and record the fact that
        % we should perform Subn on this goal id, and then use
        % incremental_rename_vars_in_goal to do all the renamings for
        % all conjunctions in the procedure body at once.
        %
        % This would have the advantage of guaranteeing that the rename
        % will NOT visit any part of the procedure body more than once.
        % However, it would have the disadvantage of guaranteeing that
        % the rename WILL visit EVERY part of the procedure body once.
        % This would be a slowdown in the average case, because the average
        % number of times that the current code visits a goal in the procedure
        % body is significantly less than one, since most conjunctions
        % do not have excess assigments. (The profiling data I am looking at
        % shows less than 10,000 times we get to this branch in a compiler
        % invocation that called simplify_proc_return_msgs more than 50,000
        % times, which implies that the average number of times we get here
        % per procedure simplification is less than 0.2.)
        %
        % Until we see a piece of code whose compilation suffers from
        % the potential worst case of this approach being realized,
        % we prefer to get its better performance in the usual case.

        renaming_transitive_closure(Subn1, Subn),
        rename_vars_in_goals(need_not_rename, Subn, Goals1, Goals),
        map.sorted_keys(Subn0, RemovedVars),

        simplify_info_get_var_table(!.Info, VarTable0),
        delete_sorted_var_entries(RemovedVars, VarTable0, VarTable),
        simplify_info_set_var_table(VarTable, !Info),

        simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        apply_substitutions_to_rtti_varmaps(map.init, map.init, Subn,
            RttiVarMaps0, RttiVarMaps),
        simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ).

:- pred find_excess_assigns_in_conj(simplify_info::in, set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::in, list(hlds_goal)::out,
    var_renaming::in, var_renaming::out) is det.

find_excess_assigns_in_conj(_, _, [], !RevGoals, !Subn).
find_excess_assigns_in_conj(Info, ConjNonLocals, [Goal | Goals],
        !RevGoals, !Subn) :-
    ( if goal_is_excess_assign(Info, ConjNonLocals, Goal, !Subn) then
        true
    else
        !:RevGoals = [Goal | !.RevGoals]
    ),
    find_excess_assigns_in_conj(Info, ConjNonLocals, Goals, !RevGoals, !Subn).

:- pred goal_is_excess_assign(simplify_info::in, set_of_progvar::in,
    hlds_goal::in, var_renaming::in, var_renaming::out) is semidet.

goal_is_excess_assign(Info, ConjNonLocals, Goal0, !Subn) :-
    Goal0 = hlds_goal(unify(_, _, _, Unif, _), _),
    Unif = assign(LeftVar0, RightVar0),

    % Check if we have already substituted one or both of the variables.
    find_renamed_var(!.Subn, LeftVar0, LeftVar),
    find_renamed_var(!.Subn, RightVar0, RightVar),

    CanElimLeft =
        ( if set_of_var.member(ConjNonLocals, LeftVar) then no else yes ),
    CanElimRight =
        ( if set_of_var.member(ConjNonLocals, RightVar) then no else yes ),

    simplify_info_get_var_table(Info, VarTable),
    (
        CanElimLeft = yes,
        CanElimRight = yes,
        % If we have a choice, try to eliminate an unnamed variable.
        ( if var_is_named(VarTable, LeftVar) then
            ElimVar = RightVar,
            ReplacementVar = LeftVar
        else
            ElimVar = LeftVar,
            ReplacementVar = RightVar
        )
    ;
        CanElimLeft = yes,
        CanElimRight = no,
        ElimVar = LeftVar,
        ReplacementVar = RightVar
    ;
        CanElimLeft = no,
        CanElimRight = yes,
        ElimVar = RightVar,
        ReplacementVar = LeftVar
    ;
        CanElimLeft = no,
        CanElimRight = no,
        fail
    ),

    simplify_info_get_eff_trace_level_optimized(Info, EffTraceLevel,
        TraceOptimized),
    % If the module is being compiled with `--trace deep' and
    % `--no-trace-optimized', don't replace a meaningful variable name
    % with `HeadVar__n' or an anonymous variable.
    not (
        eff_trace_level_needs_meaningful_var_names(EffTraceLevel) = yes,
        TraceOptimized = not_trace_optimized,
        var_is_named(VarTable, ElimVar),
        not var_is_named(VarTable, ReplacementVar)
    ),

    map.det_insert(ElimVar, ReplacementVar, !Subn).

:- pred var_is_named(var_table::in, prog_var::in) is semidet.

var_is_named(VarTable, Var) :-
    lookup_var_entry(VarTable, Var, Entry),
    Name = Entry ^ vte_name,
    Name \= "",
    not (
        string.append("HeadVar__", Suffix, Name),
        string.to_int(Suffix, _)
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

simplify_goal_parallel_conj(Goals0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, !Common, !Info) :-
    (
        Goals0 = [],
        Context = goal_info_get_context(GoalInfo0),
        hlds_goal(GoalExpr, GoalInfo) = true_goal_with_context(Context)
    ;
        Goals0 = [SingleGoal0],
        simplify_goal(SingleGoal0, hlds_goal(SingleGoal, SingleGoalInfo),
            NestedContext0, InstMap0, !Common, !Info),
        simplify_maybe_wrap_goal(GoalInfo0, SingleGoalInfo, SingleGoal,
            GoalExpr, GoalInfo, !Info)
    ;
        Goals0 = [_, _ | _],
        ( if simplify_do_ignore_par_conjunctions(!.Info) then
            simplify_goal_plain_conj(Goals0, GoalExpr, GoalInfo0, GoalInfo,
                NestedContext0, InstMap0, !Common, !Info)
        else
            GoalInfo = GoalInfo0,
            simplify_par_conjuncts(Goals0, Goals,
                NestedContext0, InstMap0, !.Common, !Info),
            GoalExpr = conj(parallel_conj, Goals),
            simplify_info_set_has_parallel_conj(has_parallel_conj, !Info)
        )
    ).

    % Simplify each conjunct in a parallel conjunction.
    %
    % We need the InitialCommonInfo because we want to start the simplification
    % of each conjunct with the common_info from the start of the parallel
    % conjunction as a whole. If we used the common_info from the end of a
    % previous conjunct, then common.m could optimize away e.g. duplicate
    % cell constructs and calls in the later conjunct, but it would
    % replace them with cross-conjunct assignments. This would not only
    % incur the need for extra synchronization code, but the later conjunct
    % would also BLOCK on this synchronization code until the relevant
    % previous conjunct reached the same synchronization point. It is mostly
    % this blocking that we want to avoid. Not just the duplicate creation
    % of a structure on the heap, but also the duplicate execution of e.g.
    % a 10ms call may well be worthwhile if it avoid the need to block
    % for 500ms.
    %
    % XXX: We should consider changing this decision, and allow the
    % introduction of dependencies between conjuncts (with the attendant
    % extra synchronization cost) if the payoff is large enough, and if
    % we risk of blocking is low enough. This would require accurate
    % profiling information.
    %
    % XXX: Even if we do not optimize away duplicate calls, we should
    % generate warnings for them.
    %
:- pred simplify_par_conjuncts(list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_nested_context::in, instmap::in, common_info::in,
    simplify_info::in, simplify_info::out) is det.

simplify_par_conjuncts([], [], _NestedContext0, _InstMap0, _Common0, !Info).
simplify_par_conjuncts([Goal0 |Goals0], [Goal | Goals],
        NestedContext0, InstMap0, Common0, !Info) :-
    simplify_goal(Goal0, Goal,
        NestedContext0, InstMap0, Common0, _Common1, !Info),
    update_instmap(Goal, InstMap0, InstMap1),
    simplify_par_conjuncts(Goals0, Goals,
        NestedContext0, InstMap1, Common0, !Info).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_conj.
%---------------------------------------------------------------------------%
