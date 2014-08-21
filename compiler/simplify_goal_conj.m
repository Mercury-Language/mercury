%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_goal_conj.m.
%
% This module handles simplification of both plain and parallel conjunctions.
%
%----------------------------------------------------------------------------%

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

%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.simplify.simplify_goal.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.make_goal.
:- import_module libs.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module libs.trace_params.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module cord.
:- import_module map.
:- import_module string.
:- import_module varset.

simplify_goal_plain_conj(Goals0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, !Common, !Info) :-
    ( simplify_do_excess_assign(!.Info) ->
        excess_assigns_in_conj(GoalInfo0, Goals0, Goals1, !Info)
    ;
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
        (
            simplify_do_mark_code_model_changes(!.Info),
            determinism_components(Detism, CanFail, at_most_zero),
            contains_multisoln_goal(Goals)
        ->
            determinism_components(InnerDetism, CanFail, at_most_many),
            goal_info_set_determinism(InnerDetism, GoalInfo0, InnerInfo),
            InnerGoal = hlds_goal(conj(plain_conj, Goals), InnerInfo),
            GoalExpr = scope(commit(dont_force_pruning), InnerGoal)
        ;
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
simplify_conj(!.PrevGoals, [Goal0 | Goals0], Goals, ConjInfo,
        NestedContext0, InstMap0, !Common, !Info) :-
    % Flatten nested conjunctions in the original code.
    ( Goal0 = hlds_goal(conj(plain_conj, SubGoals), _) ->
        Goals1 = SubGoals ++ Goals0,
        simplify_conj(!.PrevGoals, Goals1, Goals, ConjInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        Common0 = !.Common,
        simplify_goal(Goal0, Goal1, NestedContext0, InstMap0, !Common, !Info),
        (
            % Flatten nested conjunctions in the transformed code.
            Goal1 = hlds_goal(conj(plain_conj, SubGoals1), _)
        ->
            % Note that this simplifies everything inside Goal1 AGAIN.
            % We want some of this (for example, structures recorded
            % in Common while processing SubGoals1 can sometimes be used
            % to optimize Goals0), but probably most of the work done
            % by this resimplification is wasted.
            %
            % XXX Look for a way to test for the simplifications enabled
            % by the change from Goal0 to Goal1, without trying to redo
            % simplifications unaffected by that change.
            % For example, we could record the goal_ids of goals that
            % simplify.m left untouched in this pass, and return immediately
            % if asked to simplify them again. Unfortunately, just figuring
            % out which goals are touched and which are not is not easy.
            % (Due to the rebuilding of hlds_goal structures from exprs and
            % infos, the address may change even if the content does not.)
            %
            % Note that we cannot process Goals1 using the Common derived
            % from processing Goal1. If we did that, and Goal1 contained
            % X = f(Y1), then that common would remember that, and when
            % processing Goals1, simplify_conj would replace that unification
            % with X = X, thus "optimising out" the common structure.
            Goals1 = SubGoals1 ++ Goals0,
            !:Common = Common0,
            simplify_conj(!.PrevGoals, Goals1, Goals, ConjInfo,
                NestedContext0, InstMap0, !Common, !Info)
        ;
            update_instmap(Goal1, InstMap0, InstMap1),
            (
                % Delete unreachable goals.
                (
                    % This test is here mostly for the sake of completeness.
                    % It rarely finds anything to delete, because
                    % - we InstMap1 from Goal1's instmap delta,
                    % - the delta is created during mode analysis, and
                    % - mode analysis itself deletes the unreachable conjuncts
                    %   after a conjunct whose instmap delta is unreachable.
                    instmap_is_unreachable(InstMap1)
                ;
                    Goal1 = hlds_goal(_, GoalInfo1),
                    Detism1 = goal_info_get_determinism(GoalInfo1),
                    determinism_components(Detism1, _, at_most_zero)
                )
            ->
                !:PrevGoals = cord.snoc(!.PrevGoals, Goal1),
                (
                    ( Goal1 = hlds_goal(disj([]), _)
                    ; Goals0 = []
                    )
                ->
                    % XXX If Goals0 = [], why don't we add an explicit failure?
                    true
                ;
                    % We insert an explicit failure at the end of the
                    % non-succeeding conjunction. This is necessary, since
                    % the unreachability of the instmap could have been derived
                    % using inferred determinism information. Without the
                    % explicit fail goal, mode errors could result if mode
                    % analysis is rerun, since according to the language
                    % specification, mode analysis does not use inferred
                    % determinism information when deciding what can never
                    % succeed.
                    Goal0 = hlds_goal(_, GoalInfo0),
                    Context = goal_info_get_context(GoalInfo0),
                    FailGoal = fail_goal_with_context(Context),
                    !:PrevGoals = cord.snoc(!.PrevGoals, FailGoal)
                ),
                Goals = cord.list(!.PrevGoals)
            ;
                !:PrevGoals = cord.snoc(!.PrevGoals, Goal1),
                simplify_conj(!.PrevGoals, Goals0, Goals, ConjInfo,
                    NestedContext0, InstMap1, !Common, !Info)
            )
        )
    ).

%-----------------------------------------------------------------------------%

:- type var_renaming == map(prog_var, prog_var).

:- pred find_renamed_var(var_renaming::in, prog_var::in, prog_var::out) is det.

find_renamed_var(Subn, Var0, Var) :-
    ( map.search(Subn, Var0, Var1) ->
        find_renamed_var(Subn, Var1, Var)
    ;
        Var = Var0
    ).

    % Collapse chains of renamings.
    %
:- pred renaming_transitive_closure(var_renaming::in, var_renaming::out)
    is det.

renaming_transitive_closure(VarRenaming0, VarRenaming) :-
    map.map_values_only(find_renamed_var(VarRenaming0),
        VarRenaming0, VarRenaming).

%-----------------------------------------------------------------------------%

:- pred excess_assigns_in_conj(hlds_goal_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_info::in, simplify_info::out) is det.

excess_assigns_in_conj(ConjInfo, Goals0, Goals, !Info) :-
    ConjNonLocals = goal_info_get_nonlocals(ConjInfo),
    map.init(Subn0),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_trace_level(Globals, TraceLevel),
    globals.lookup_bool_option(Globals, trace_optimized, TraceOptimized),
    simplify_info_get_varset(!.Info, VarSet0),
    find_excess_assigns_in_conj(TraceLevel, TraceOptimized,
        VarSet0, ConjNonLocals, Goals0, [], RevGoals, Subn0, Subn1),
    ( map.is_empty(Subn1) ->
        Goals = Goals0
    ;
        list.reverse(RevGoals, Goals1),
        % XXX Can we delay this until we return to the top level?
        renaming_transitive_closure(Subn1, Subn),
        rename_vars_in_goals(need_not_rename, Subn, Goals1, Goals),
        map.keys(Subn0, RemovedVars),
        varset.delete_vars(RemovedVars, VarSet0, VarSet),
        simplify_info_set_varset(VarSet, !Info),
        simplify_info_get_rtti_varmaps(!.Info, RttiVarMaps0),
        apply_substitutions_to_rtti_varmaps(map.init, map.init, Subn,
            RttiVarMaps0, RttiVarMaps),
        simplify_info_set_rtti_varmaps(RttiVarMaps, !Info)
    ).

:- pred find_excess_assigns_in_conj(trace_level::in, bool::in,
    prog_varset::in, set_of_progvar::in, list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    var_renaming::in, var_renaming::out) is det.

find_excess_assigns_in_conj(_, _, _, _, [], !RevGoals, !Subn).
find_excess_assigns_in_conj(Trace, TraceOptimized, VarSet, ConjNonLocals,
        [Goal | Goals], !RevGoals, !Subn) :-
    (
        goal_is_excess_assign(Trace, TraceOptimized, VarSet, ConjNonLocals,
            Goal, !Subn)
    ->
        true
    ;
        !:RevGoals = [Goal | !.RevGoals]
    ),
    find_excess_assigns_in_conj(Trace, TraceOptimized, VarSet, ConjNonLocals,
        Goals, !RevGoals, !Subn).

:- pred goal_is_excess_assign(trace_level::in, bool::in, prog_varset::in,
    set_of_progvar::in, hlds_goal::in,
    var_renaming::in, var_renaming::out) is semidet.

goal_is_excess_assign(Trace, TraceOptimized, VarSet, ConjNonLocals, Goal0,
        !Subn) :-
    Goal0 = hlds_goal(unify(_, _, _, Unif, _), _),
    Unif = assign(LeftVar0, RightVar0),

    % Check if we have already substituted one or both of the variables.
    find_renamed_var(!.Subn, LeftVar0, LeftVar),
    find_renamed_var(!.Subn, RightVar0, RightVar),

    CanElimLeft = ( set_of_var.member(ConjNonLocals, LeftVar) -> no ; yes ),
    CanElimRight = ( set_of_var.member(ConjNonLocals, RightVar) -> no ; yes ),

    (
        CanElimLeft = yes,
        CanElimRight = yes,
        % If we have a choice, try to eliminate an unnamed variable.
        ( var_is_named(VarSet, LeftVar) ->
            ElimVar = RightVar,
            ReplacementVar = LeftVar
        ;
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
    map.det_insert(ElimVar, ReplacementVar, !Subn),

    % If the module is being compiled with `--trace deep' and
    % `--no-trace-optimized' don't replace a meaningful variable name
    % with `HeadVar__n' or an anonymous variable.
    \+ (
        trace_level_needs_meaningful_var_names(Trace) = yes,
        TraceOptimized = no,
        var_is_named(VarSet, ElimVar),
        \+ var_is_named(VarSet, ReplacementVar)
    ).

:- pred var_is_named(prog_varset::in, prog_var::in) is semidet.

var_is_named(VarSet, Var) :-
    varset.search_name(VarSet, Var, Name),
    \+ (
        string.append("HeadVar__", Suffix, Name),
        string.to_int(Suffix, _)
    ).

%---------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

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
        ( simplify_do_ignore_par_conjunctions(!.Info) ->
            simplify_goal_plain_conj(Goals0, GoalExpr, GoalInfo0, GoalInfo,
                NestedContext0, InstMap0, !Common, !Info)
        ;
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
