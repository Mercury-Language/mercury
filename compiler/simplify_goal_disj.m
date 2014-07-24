%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_goal_disj.m.
%
% This module handles simplification of disjunctions and atomic goals
% (whose retry structure resembles disjunctions).
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_disj.
:- interface.

:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

    % Handle simplifications of disjunctions.
    %
:- pred simplify_goal_disj(
    hlds_goal_expr::in(goal_expr_disj), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Handle simplifications of atomic goals.
    %
:- pred simplify_goal_atomic_goal(atomic_goal_type::in,
    atomic_interface_vars::in, atomic_interface_vars::in,
    maybe(list(prog_var))::in, hlds_goal::in, list(hlds_goal)::in,
    list(atomic_interface_vars)::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

:- implementation.

:- import_module check_hlds.simplify.simplify_goal.
:- import_module hlds.instmap.
:- import_module libs.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module require.

simplify_goal_disj(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, !Info) :-
    GoalExpr0 = disj(Disjuncts0),
    simplify_info_get_instmap(!.Info, InstMap0),
    simplify_disj(Disjuncts0, [], Disjuncts, [], InstMaps, !.Info, !Info),
    (
        Disjuncts = [],
        Context = goal_info_get_context(GoalInfo0),
        hlds_goal(GoalExpr, GoalInfo) = fail_goal_with_context(Context)
    ;
        Disjuncts = [SingleGoal],
        % A singleton disjunction is equivalent to the goal itself.
        SingleGoal = hlds_goal(Goal1, GoalInfo1),
        simplify_maybe_wrap_goal(GoalInfo0, GoalInfo1, Goal1,
            GoalExpr, GoalInfo, !Info)
    ;
        Disjuncts = [_, _ | _],
        GoalExpr = disj(Disjuncts),
        ( goal_info_has_feature(GoalInfo0, feature_mode_check_clauses_goal) ->
            % Recomputing the instmap delta would take very long
            % and is very unlikely to get any better precision.
            GoalInfo = GoalInfo0
        ;
            simplify_info_get_module_info(!.Info, ModuleInfo1),
            NonLocals = goal_info_get_nonlocals(GoalInfo0),
            simplify_info_get_var_types(!.Info, VarTypes),
            merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMaps,
                NewDelta, ModuleInfo1, ModuleInfo2),
            simplify_info_set_module_info(ModuleInfo2, !Info),
            goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo)
        )
    ),
    list.length(Disjuncts, DisjunctsLength),
    list.length(Disjuncts0, Disjuncts0Length),
    ( DisjunctsLength \= Disjuncts0Length ->
        % If we pruned some disjuncts, variables used by those disjuncts
        % may no longer be non-local to the disjunction. Also, the
        % determinism may have changed (especially if we pruned all the
        % disjuncts). If the disjunction now can't succeed, we have to
        % recompute instmap_deltas and rerun determinism analysis
        % to avoid aborts in the code generator because the disjunction
        % now cannot produce variables it did before.

        simplify_info_set_should_requantify(!Info),
        simplify_info_set_should_rerun_det(!Info)
    ;
        true
    ).

%---------------------------------------------------------------------------%

:- pred simplify_disj(list(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out,
    list(instmap_delta)::in, list(instmap_delta)::out,
    simplify_info::in, simplify_info::in, simplify_info::out) is det.

simplify_disj([], RevGoals, Goals, !PostBranchInstMaps, _, !Info) :-
    list.reverse(RevGoals, Goals).
simplify_disj([Goal0 | Goals0], RevGoals0, Goals, !PostBranchInstMaps,
        Info0, !Info) :-
    simplify_goal(Goal0, Goal, !Info),
    Goal = hlds_goal(_, GoalInfo),
    Purity = goal_info_get_purity(GoalInfo),

    (
        % Don't prune or warn about impure disjuncts that can't succeed.
        Purity \= purity_impure,
        Detism = goal_info_get_determinism(GoalInfo),
        determinism_components(Detism, _CanFail, MaxSolns),
        MaxSolns = at_most_zero
    ->
        (
            simplify_do_warn_simple_code(!.Info),
            % Don't warn where the initial goal was fail, since that can result
            % from mode analysis pruning away cases in a switch which cannot
            % succeed due to sub-typing in the modes.
            Goal0 \= hlds_goal(disj([]), _),
            % Don't warn if the code was duplicated, since it is quite likely
            % to be spurious: though the disjunct cannot succeed in this arm of
            % the switch, it likely can succeed in other arms that derive from
            % the exact same piece of source code.
            simplify_info_get_inside_duplicated_for_switch(!.Info,
                 InsideDuplForSwitch),
            InsideDuplForSwitch = no
        ->
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Warning: this disjunct"),
                words("will never have any solutions.")],
            Msg = simple_msg(Context,
                [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
            Severity = severity_conditional(warn_simple_code, yes,
                severity_warning, no),
            Spec = error_spec(Severity,
                phase_simplify(report_only_if_in_all_modes), [Msg]),
            simplify_info_add_simple_code_spec(Spec, !Info)
        ;
            true
        ),

        % Prune away non-succeeding disjuncts where possible.
        (
            (
                Goal0 = hlds_goal(disj([]), _)
            ;
                % Only remove disjuncts that might loop
                % or call error/1 if --no-fully-strict.
                simplify_info_get_fully_strict(!.Info, no)
            )
        ->
            RevGoals1 = RevGoals0
        ;
            RevGoals1 = [Goal | RevGoals0],
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            !:PostBranchInstMaps = [InstMapDelta | !.PostBranchInstMaps]
        )
    ;
        RevGoals1 = [Goal | RevGoals0],
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        !:PostBranchInstMaps = [InstMapDelta | !.PostBranchInstMaps]
    ),

    simplify_info_post_branch_update(Info0, !Info),
    simplify_disj(Goals0, RevGoals1, Goals, !PostBranchInstMaps, Info0, !Info).

    % Disjunctions that cannot succeed more than once when viewed from the
    % outside generally need some fixing up, and/or some warnings to be issued.
    %
    % We previously converted them all to if-then-elses using the code below,
    % however converting disjs that have output variables but that nevertheless
    % cannot succeed more than one (e.g. cc_nondet or cc_multi disjs) into
    % if-then-elses may cause problems with other parts of the compiler that
    % assume that an if-then-else is mode-correct, i.e. that the condition
    % doesn't bind variables.
    %
    %       goal_info_get_determinism(GoalInfo, Detism),
    %       determinism_components(Detism, _CanFail, MaxSoln),
    %       MaxSoln \= at_most_many
    %   ->
    %       goal_info_get_instmap_delta(GoalInfo, DeltaInstMap),
    %       goal_info_get_nonlocals(GoalInfo, NonLocalVars),
    %       (
    %           det_no_output_vars(NonLocalVars, InstMap0,
    %               DeltaInstMap, DetInfo)
    %       ->
    %           OutputVars = no
    %       ;
    %           OutputVars = yes
    %       ),
    %       fixup_disj(Disjuncts, Detism, OutputVars, GoalInfo, InstMap0,
    %           DetInfo, Goal, MsgsA, Msgs)
    %   ;
    %
:- pred fixup_disj(list(hlds_goal)::in, determinism::in, bool::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    simplify_info::in, simplify_info::out) is det.

fixup_disj(Disjuncts, _, _OutputVars, GoalInfo, Goal, !Info) :-
    det_disj_to_ite(Disjuncts, GoalInfo, IfThenElse),
    simplify_goal(IfThenElse, Simplified, !Info),
    Simplified = hlds_goal(Goal, _).

    % det_disj_to_ite is used to transform disjunctions that occur
    % in prunable contexts into if-then-elses.
    % For example, it would transform
    %
    %   ( Disjunct1
    %   ; Disjunct2
    %   ; Disjunct3
    %   )
    % into
    %   ( Disjunct1 ->
    %       true
    %   ; Disjunct2 ->
    %       true
    %   ;
    %       Disjunct3
    %   ).
    %
:- pred det_disj_to_ite(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out) is det.

det_disj_to_ite([], _GoalInfo, _) :-
    unexpected($module, $pred, "reached base case").
det_disj_to_ite([Disjunct | Disjuncts], GoalInfo, Goal) :-
    (
        Disjuncts = [],
        Goal = Disjunct
    ;
        Disjuncts = [_ | _],
        Cond = Disjunct,
        Cond = hlds_goal(_CondGoal, CondGoalInfo),

        Then = true_goal,

        det_disj_to_ite(Disjuncts, GoalInfo, Rest),
        Rest = hlds_goal(_RestGoal, RestGoalInfo),

        CondNonLocals = goal_info_get_nonlocals(CondGoalInfo),
        RestNonLocals = goal_info_get_nonlocals(RestGoalInfo),
        set_of_var.union(CondNonLocals, RestNonLocals, NonLocals),
        goal_info_set_nonlocals(NonLocals, GoalInfo, NewGoalInfo0),

        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo),
        instmap_delta_restrict(NonLocals, InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, NewGoalInfo0, NewGoalInfo1),

        CondDetism = goal_info_get_determinism(CondGoalInfo),
        RestDetism = goal_info_get_determinism(RestGoalInfo),
        determinism_components(CondDetism, CondCanFail, CondMaxSoln),
        determinism_components(RestDetism, RestCanFail, RestMaxSoln),
        det_disjunction_canfail(CondCanFail, RestCanFail, CanFail),
        det_disjunction_maxsoln(CondMaxSoln, RestMaxSoln, MaxSoln0),
        ( MaxSoln0 = at_most_many ->
            MaxSoln = at_most_one
        ;
            MaxSoln = MaxSoln0
        ),
        determinism_components(Detism, CanFail, MaxSoln),
        goal_info_set_determinism(Detism, NewGoalInfo1, NewGoalInfo),

        Goal = hlds_goal(if_then_else([], Cond, Then, Rest), NewGoalInfo)
    ).

%---------------------------------------------------------------------------%

simplify_goal_atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
        MainGoal0, OrElseGoals0, OrElseInners, GoalExpr, !GoalInfo, !Info) :-
    % XXX STM: At the moment we do not simplify the inner goals as there is
    % a chance that the outer and inner variables will change which will
    % cause problems during expansion of STM constructs. This will be
    % fixed eventually.
    MainGoal = MainGoal0,
    OrElseGoals = OrElseGoals0,
    % simplify_goal(MainGoal0, MainGoal, !Info),
    % simplify_or_else_goals(OrElseGoals0, OrElseGoals, !Info),
    ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
        MainGoal, OrElseGoals, OrElseInners),
    GoalExpr = shorthand(ShortHand).

:- pred simplify_or_else_goals(list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_info::in, simplify_info::out) is det.

simplify_or_else_goals([], [], !Info).
simplify_or_else_goals([Goal0 | Goals0], [Goal | Goals], !Info) :-
    simplify_goal(Goal0, Goal, !Info),
    simplify_or_else_goals(Goals0, Goals, !Info).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_disj.
%---------------------------------------------------------------------------%
