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
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.trace_params.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module set.
:- import_module string.
:- import_module varset.

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
            simplify_do_mark_code_model_changes(!.Info),
            determinism_components(Detism, CanFail, at_most_zero),
            contains_multisoln_goal(Goals)
        then
            determinism_components(InnerDetism, CanFail, at_most_many),
            goal_info_set_determinism(InnerDetism, GoalInfo0, InnerInfo),
            InnerGoal = hlds_goal(conj(plain_conj, Goals), InnerInfo),
            GoalExpr = scope(commit(dont_force_pruning), InnerGoal)
        else
            GoalExpr = conj(plain_conj, Goals)
        ),
        GoalInfo = GoalInfo0
    ).

:- pred contains_multisoln_goal(list(hlds_goal)::in) is semidet.

contains_multisoln_goal(Goals) :-
    list.member(hlds_goal(_GoalExpr, GoalInfo), Goals),
    Detism = goal_info_get_determinism(GoalInfo),
    determinism_components(Detism, _, at_most_many).

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
            %
            % Note that we cannot process HeadTailGoals1 using the Common
            % derived from processing Goal1. If we did that, and HeadGoal1
            % contained X = f(Y1), then that common would remember that,
            % and when processing HeadTailGoals1, simplify_conj would replace
            % that unification with X = X, thus "optimising out" the common
            % structure.
            HeadTailGoals1 = HeadSubGoals1 ++ TailGoals0,
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
                simplify_info_get_deleted_call_callees(!.Info,
                    DeletedCallCallees0),
                SubGoalCalledProcs = goals_proc_refs(TailGoals0),
                set.union(SubGoalCalledProcs,
                    DeletedCallCallees0, DeletedCallCallees),
                simplify_info_set_deleted_call_callees(DeletedCallCallees,
                    !Info),

                !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal1),
                ( if
                    ( HeadGoal1 = hlds_goal(disj([]), _)
                    ; TailGoals0 = []
                    )
                then
                    % XXX If TailGoals0 = [], why don't we add
                    % an explicit failure?
                    true
                else
                    % We insert an explicit failure at the end of the
                    % non-succeeding conjunction. This is necessary, since
                    % the unreachability of the instmap could have been derived
                    % using inferred determinism information. Without the
                    % explicit fail goal, mode errors could result if mode
                    % analysis is rerun, since according to the language
                    % specification, mode analysis does not use inferred
                    % determinism information when deciding what can never
                    % succeed.
                    HeadGoal0 = hlds_goal(_, HeadGoalInfo0),
                    Context = goal_info_get_context(HeadGoalInfo0),
                    FailGoal = fail_goal_with_context(Context),
                    !:PrevGoals = cord.snoc(!.PrevGoals, FailGoal)
                ),
                Goals = cord.list(!.PrevGoals)
            else
                ( if
                    simplify_do_test_after_switch(!.Info),
                    % Look for situations like this:
                    %
                    %   (
                    %       ( X = a
                    %       ; X = b
                    %       ),
                    %       Res = yes
                    %   ;
                    %       ( X = c
                    %       ; X = d(_)
                    %       ),
                    %       Res = no
                    %   ),
                    %   Res = no
                    %
                    % and transform them into
                    %
                    %   ( X = a
                    %   ; X = b
                    %   )
                    %
                    % The idea is to avoid the performance overhead of
                    % setting and testing Res in such switches.
                    %
                    % The point of switches like this, in which each arm
                    % does nothing except set a flag that is tested
                    % after the switch, is that if the type of X gets
                    % a new functor added to it, they get a message if
                    %
                    % - either the --inform-incomplete-switch option is given,
                    %   or
                    % - the switch is wrapped in a require_complete_switch
                    %   scope.
                    %
                    % In the latter case, the simplification of HeadGoal0
                    % into HeadGoal1 will remove the scope wrapper.
                    %
                    HeadGoalExpr1 = switch(SwitchVar, SwitchCanFail1, Cases1),
                    TailGoals0 = [HeadTailGoal0 | TailTailGoals0],
                    HeadTailGoal0 = hlds_goal(HeadTailGoalExpr0, _),
                    HeadTailGoalExpr0 = unify(_LHSVar, _RHS, _UniMode,
                        Unification, _UnifyContext),
                    Unification = deconstruct(TestVar, TestConsId, TestArgs,
                        _ArgModes, DeconstructCanFail, _CanCGC),
                    TestArgs = [],
                    DeconstructCanFail = can_fail,
                    all_cases_construct_test_var(Cases1, TestVar, TestConsId,
                        [], RevTruncatedSameCases, [], RevDiffCases),

                    % If the procedure body can refer to TestVar anywhere
                    % other than in HeadGoal0 or HeadTailGoal0, then we
                    % cannot eliminate the assignment to TestVar. Since
                    % the mode system does not permit conjuncts before
                    % HeadGoal0 to refer to TestVar, the places we need
                    % to check are the conjuncts after HeadTailGoal0 and
                    % the code outside the conjunction as a whole.
                    %
                    % If there are outside references to TestVar, we could
                    % still delete RevDiffCases from the switch, while keeping
                    % the assignment of TestConsId to TestVar, either in
                    % the non-truncated originals of the RevTruncatedSameCases,
                    % or in a construct unification after the switch that would
                    % replace the original deconstruction in HeadTailGoal0.
                    % However, a bootcheck found no situations with outside
                    % references to TestVar, so that situation is probably
                    % too rare to be worth trying to optimize.

                    ConjNonLocals = goal_info_get_nonlocals(ConjInfo),
                    not set_of_var.contains(ConjNonLocals, TestVar),
                    no_conjunct_refers_to_var(TailTailGoals0, TestVar)
                then
                    (
                        RevDiffCases = [],
                        SwitchCanFail2 = SwitchCanFail1
                    ;
                        RevDiffCases = [_ | _],
                        SwitchCanFail2 = can_fail
                    ),
                    % We need to update the determinism fields of the goals.
                    % We could try to do that here, but it is simpler to use
                    % the existing code for the job.
                    simplify_info_set_should_rerun_det(!Info),

                    list.reverse(RevTruncatedSameCases, TruncatedSameCases),
                    HeadGoalExpr2 = switch(SwitchVar, SwitchCanFail2,
                        TruncatedSameCases),
                    HeadGoal2 = hlds_goal(HeadGoalExpr2, HeadGoalInfo1),
                    !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal2),
                    % We pass TailTailGoals0 instead of TailGoals, since
                    % the effect of HeadTailGoal0 has now been folded into
                    % HeadGoal2.
                    simplify_conj(!.PrevGoals, TailTailGoals0, Goals, ConjInfo,
                        NestedContext0, InstMap1, !Common, !Info)
                else
                    !:PrevGoals = cord.snoc(!.PrevGoals, HeadGoal1),
                    simplify_conj(!.PrevGoals, TailGoals0, Goals, ConjInfo,
                        NestedContext0, InstMap1, !Common, !Info)
                )
            )
        )
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

%---------------------------------------------------------------------------%

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
        map.keys(Subn0, RemovedVars),

        simplify_info_get_varset(!.Info, VarSet0),
        varset.delete_vars(RemovedVars, VarSet0, VarSet),
        simplify_info_set_varset(VarSet, !Info),

        simplify_info_get_var_types(!.Info, VarTypes0),
        delete_var_types(RemovedVars, VarTypes0, VarTypes),
        simplify_info_set_var_types(VarTypes, !Info),

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

    simplify_info_get_varset(Info, VarSet),
    (
        CanElimLeft = yes,
        CanElimRight = yes,
        % If we have a choice, try to eliminate an unnamed variable.
        ( if var_is_named(VarSet, LeftVar) then
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

    simplify_info_get_trace_level_optimized(Info, TraceLevel, TraceOptimized),
    % If the module is being compiled with `--trace deep' and
    % `--no-trace-optimized', don't replace a meaningful variable name
    % with `HeadVar__n' or an anonymous variable.
    not (
        trace_level_needs_meaningful_var_names(TraceLevel) = yes,
        TraceOptimized = no,
        var_is_named(VarSet, ElimVar),
        not var_is_named(VarSet, ReplacementVar)
    ),

    map.det_insert(ElimVar, ReplacementVar, !Subn).

:- pred var_is_named(prog_varset::in, prog_var::in) is semidet.

var_is_named(VarSet, Var) :-
    varset.search_name(VarSet, Var, Name),
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
