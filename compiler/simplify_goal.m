%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_goal.m.
%
% This module handles simplifications that apply to all goals, and then
% distributes the goal-type-specific work to other submodules of simplify.m.
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.

    % Handle simplification of all goals.
    %
    % This involves trying to simplify the interaction between the given goal
    % and its surrounding goals before invoking the goal-type-specific
    % simplification code on it.
    %
    % simplify_goal and its goal-type-specific subcontractor predicates
    % pass around information about the context surrounding the goal
    % currently being analyzed in the simplify_nested_context argument.
    % They pass around information about the program point just before
    % the current goal in the instmap and common_info arguments.
    % They pass around information that is global to the simplification
    % process as a whole in the simplify_info arguments.
    %
    % These predicates return the common_info appropriate to the program
    % point after the goal that was just analyzed in the common_info output
    % argument. They do not return the new instmap, because it is not
    % necessary. The two predicates that need to know the instmap
    % after the goal that was just analyzed (simplify_goal_conj, after
    % a conjunct, and simplify_goal_ite, after the condition)
    % apply the goal's instmap delta themselves.
    %
:- pred simplify_goal(hlds_goal::in, hlds_goal::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Invoke the goal-type-specific simplification code on the given goal.
    %
:- pred simplify_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

%----------------------------------------------------------------------------%
%
% Utility predicates needed by the simplifications of several kinds of goals.
%

    % When removing a level of wrapping around a goal, if the determinisms
    % are not the same, we really need to rerun determinism analysis on the
    % procedure. I think this is a similar situation to inlining of erroneous
    % goals. The safe thing to do is to wrap a `scope' around the inner goal
    % if the inner and outer determinisms are not the same. It probably
    % won't happen that often.
    %
:- pred simplify_maybe_wrap_goal(hlds_goal_info::in, hlds_goal_info::in,
    hlds_goal_expr::in, hlds_goal_expr::out, hlds_goal_info::out,
    simplify_info::in, simplify_info::out)  is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.simplify.simplify_goal_call.
:- import_module check_hlds.simplify.simplify_goal_conj.
:- import_module check_hlds.simplify.simplify_goal_disj.
:- import_module check_hlds.simplify.simplify_goal_ite.
:- import_module check_hlds.simplify.simplify_goal_scope.
:- import_module check_hlds.simplify.simplify_goal_switch.
:- import_module check_hlds.simplify.simplify_goal_unify.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.make_goal.
:- import_module libs.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_spec.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.pd_cost.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module require.

simplify_goal(Goal0, Goal, NestedContext0, InstMap0, !Common, !Info) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    ( if goal_info_has_feature(GoalInfo0, feature_duplicated_for_switch) then
        NestedContext = NestedContext0 ^ snc_inside_dupl_for_switch := yes
    else
        NestedContext = NestedContext0
    ),
    ( if goal_info_has_feature(GoalInfo0, feature_contains_trace) then
        simplify_info_set_found_contains_trace(yes, !Info),
        Goal0ContainsTrace = contains_trace_goal
    else
        Goal0ContainsTrace = contains_no_trace_goal
    ),
    Detism = goal_info_get_determinism(GoalInfo0),
    simplify_info_get_module_info(!.Info, ModuleInfo0),
    goal_can_loop_or_throw_imaf(Goal0, Goal0CanLoopOrThrow,
        ModuleInfo0, ModuleInfo),
    simplify_info_set_module_info(ModuleInfo, !Info),
    Purity = goal_info_get_purity(GoalInfo0),
    ( if
        % If --no-fully-strict, replace goals with determinism failure
        % with `fail'.

        Detism = detism_failure,
        ( Purity = purity_pure
        ; Purity = purity_semipure
        ),
        Goal0ContainsTrace = contains_no_trace_goal,
        ( simplify_info_get_fully_strict(!.Info, not_fully_strict)
        ; Goal0CanLoopOrThrow = cannot_loop_or_throw
        )
    then
        % Warn about this, unless the goal was an explicit `fail', call to
        % `builtin.false/0' or  some goal containing `fail' or a call to
        % `builtin.false/0'.

        Context = goal_info_get_context(GoalInfo0),
        ( if
            simplify_do_warn_simple_code(!.Info),
            not (
                goal_contains_goal(Goal0, SubGoal),
                ( SubGoal = hlds_goal(disj([]), _)
                ; goal_is_call_to_builtin_false(SubGoal)
                )
            )
        then
            MainPieces = [words("Warning: this goal cannot succeed.")],
            VerbosePieces =
                [words("The compiler will optimize away this goal,"),
                words("replacing it with"), quote("fail"), suffix(".")],
            Msg = simple_msg(Context,
                [always(MainPieces),
                verbose_only(verbose_always, VerbosePieces)]),
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_simplify(report_only_if_in_all_modes),
                [Msg]),
            simplify_info_add_message(Spec, !Info)
        else
            true
        ),

        % If the goal had any non-locals, we should requantify.
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        ( if set_of_var.is_empty(NonLocals0) then
            true
        else
            simplify_info_set_rerun_quant_instmap_delta(!Info)
        ),
        goal_cost(Goal0, CostDelta),
        simplify_info_incr_cost_delta(CostDelta, !Info),
        Goal1 = fail_goal_with_context(Context)
    else if
        % If --no-fully-strict, replace goals which cannot fail and have
        % no output variables with `true'. However, we don't do this for
        % erroneous goals, since these may occur in conjunctions where there
        % are no producers for some variables, and the code generator would
        % fail for these.

        determinism_components(Detism, cannot_fail, MaxSoln),
        MaxSoln \= at_most_zero,
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo0),
        NonLocalVars = goal_info_get_nonlocals(GoalInfo0),
        simplify_info_get_module_info(!.Info, ModuleInfo),
        simplify_info_get_var_table(!.Info, VarTable),
        instmap_delta_no_output_vars(ModuleInfo, VarTable,
            InstMap0, InstMapDelta, NonLocalVars),
        ( Purity = purity_pure
        ; Purity = purity_semipure
        ),
        Goal0ContainsTrace = contains_no_trace_goal,
        ( simplify_info_get_fully_strict(!.Info, not_fully_strict)
        ; Goal0CanLoopOrThrow = cannot_loop_or_throw
        )
    then
% The following warning is disabled, because it often results in spurious
% warnings. Sometimes predicate calls are used just to constrain the types,
% to avoid type ambiguities or unbound type variables, and in such cases,
% it is perfectly legitimate for a call to be det and to have no outputs.
% There's no simple way of telling those cases from cases for which we
% really ought to warn.
% XXX This hasn't really been true since we added `with_type`.
%
%       % warn about this, if the goal wasn't `true', wasn't `!',
%       % and wasn't a deconstruction unification.
%       % We don't warn about deconstruction unifications
%       % with no outputs that always succeed, because that
%       % would result in bogus warnings, since switch detection
%       % converts deconstruction unifications that can fail
%       % into ones that always succeed by moving the test into
%       % the switch.
%       % We also don't warn about conjunctions or existential
%       % quantifications, because it seems that warnings in those
%       % cases are usually spurious.
%       ( if
%           simplify_do_warn_simple_code(!.Info),
%           % Goal0 \= conj(plain_conj, []) - _,
%           not (Goal0 = call(_, _, _, _, _, SymName) - _,
%               unqualify_name(SymName, "!")),
%           Goal0 \= conj(plain_conj, _) - _,
%           Goal0 \= some(_, _) - _,
%           not (Goal0 = unify(_, _, _, Unification, _) - _,
%               Unification = deconstruct(_, _, _, _, _))
%       then
%           Msg = det_goal_has_no_outputs,
%           ContextMsg = context_det_msg(Context, Msg),
%           simplify_info_add_simple_code_spec(ContextMsg, !Info)
%       else
%           true
%       ),

        % If the goal had any non-locals, we should requantify.
        NonLocals0 = goal_info_get_nonlocals(GoalInfo0),
        ( if set_of_var.is_empty(NonLocals0) then
            true
        else
            simplify_info_set_rerun_quant_instmap_delta(!Info)
        ),
        goal_cost(Goal0, CostDelta),
        simplify_info_incr_cost_delta(CostDelta, !Info),
        Context = goal_info_get_context(GoalInfo0),
        Goal1 = true_goal_with_context(Context)
    else
        Goal1 = Goal0
    ),

    % Remove unnecessary explicit quantifications before working out
    % whether the goal can cause a stack flush.

    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    ( if GoalExpr1 = scope(Reason1, SomeGoal1) then
        try_to_merge_nested_scopes(Reason1, SomeGoal1, GoalInfo1, Goal2)
    else
        Goal2 = Goal1
    ),
    ( if
        simplify_do_elim_removable_scopes(!.Info),
        Goal2 = hlds_goal(scope(Reason2, SomeGoal2), _GoalInfo2),
        (
            Reason2 = barrier(removable)
        ;
            Reason2 = from_ground_term(_, Kind),
            Kind = from_ground_term_other
        )
    then
        Goal3 = SomeGoal2
    else
        Goal3 = Goal2
    ),
    Goal3 = hlds_goal(GoalExpr3, GoalInfo3),
    maybe_handle_stack_flush(before, GoalExpr3, !Common),
    simplify_goal_expr(GoalExpr3, GoalExpr4, GoalInfo3, GoalInfo4,
        NestedContext, InstMap0, !Common, !Info),
    maybe_handle_stack_flush(after, GoalExpr4, !Common),
    enforce_unreachability_invariant(GoalInfo4, GoalInfo, !Info),
    Goal = hlds_goal(GoalExpr4, GoalInfo).

%----------------------------------------------------------------------------%

:- pred goal_is_call_to_builtin_false(hlds_goal::in) is semidet.

goal_is_call_to_builtin_false(hlds_goal(GoalExpr, _)) :-
    GoalExpr = plain_call(_, _, _, _, _, SymName),
    SymName = qualified(mercury_public_builtin_module, "false").

%----------------------------------------------------------------------------%

simplify_goal_expr(!GoalExpr, !GoalInfo, NestedContext0,
        InstMap0, !Common, !Info) :-
    (
        !.GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj,
            simplify_goal_plain_conj(Goals, !:GoalExpr, !GoalInfo,
                NestedContext0, InstMap0, !Common, !Info)
        ;
            ConjType = parallel_conj,
            simplify_goal_parallel_conj(Goals, !:GoalExpr, !GoalInfo,
                NestedContext0, InstMap0, !Common, !Info)
        )
    ;
        !.GoalExpr = disj(_),
        simplify_goal_disj(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = switch(_, _, _),
        simplify_goal_switch(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = if_then_else(_, _, _, _),
        simplify_goal_ite(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = negation(_),
        simplify_goal_neg(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = scope(_, _),
        simplify_goal_scope(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = unify(_, _, _, _, _),
        simplify_goal_unify(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = plain_call(_, _, _, _, _, _),
        simplify_goal_plain_call(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = generic_call(_, _, _, _, _),
        simplify_goal_generic_call(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        simplify_goal_foreign_proc(!GoalExpr, !GoalInfo,
            NestedContext0, InstMap0, !Common, !Info)
    ;
        !.GoalExpr = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(GoalType, Outer, Inner,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
            simplify_goal_atomic_goal(GoalType, Outer, Inner,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners,
                !:GoalExpr, !GoalInfo,
                NestedContext0, InstMap0, !Common, !Info)
        ;
            ShortHand0 = try_goal(_, _, _),
            % These should have been expanded out by now.
            unexpected($pred, "try_goal")
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

%----------------------------------------------------------------------------%

:- type before_after
    --->    before
    ;       after.

:- pred maybe_handle_stack_flush(before_after::in, hlds_goal_expr::in,
    common_info::in, common_info::out) is det.

maybe_handle_stack_flush(BeforeAfter, GoalExpr, !Common) :-
    WillFlush = will_flush(BeforeAfter, GoalExpr),
    (
        WillFlush = yes,
        common_info_stack_flush(!Common)
    ;
        WillFlush = no
    ).

    % Return `no' if execution of the given goal cannot encounter a context
    % that causes any variable to be flushed to its stack slot or to a
    % register at the specified time, and `yes' otherwise.
    %
:- func will_flush(before_after, hlds_goal_expr) = bool.

will_flush(BeforeAfter, GoalExpr) = WillFlush :-
    (
        GoalExpr = unify(_, _, _, Unify, _),
        ( if Unify = complicated_unify(_, _, _) then
            WillFlush = yes
        else
            WillFlush = no
        )
    ;
        GoalExpr = plain_call(_, _, _, BuiltinState, _, _),
        (
            BuiltinState = inline_builtin,
            WillFlush = no
        ;
            BuiltinState = not_builtin,
            (
                BeforeAfter = before,
                WillFlush = no
            ;
                BeforeAfter = after,
                WillFlush = yes
            )
        )
    ;
        GoalExpr = generic_call(GenericCall, _, _, _, _),
        (
            BeforeAfter = before,
            WillFlush = no
        ;
            BeforeAfter = after,
            (
                ( GenericCall = higher_order(_, _, _, _)
                ; GenericCall = class_method(_, _, _, _)
                ),
                WillFlush = yes
            ;
                ( GenericCall = event_call(_)
                ; GenericCall = cast(_)
                ),
                WillFlush = no
            )
        )
    ;
        GoalExpr = call_foreign_proc(_, _, _, _, _, _, _),
        (
            BeforeAfter = before,
            WillFlush = no
        ;
            BeforeAfter = after,
            WillFlush = yes
        )
    ;
        GoalExpr = conj(ConjType, _),
        (
            ConjType = plain_conj,
            WillFlush = no
        ;
            ConjType = parallel_conj,
            WillFlush = yes
        )
    ;
        GoalExpr = switch(_, _, _),
        WillFlush = no
    ;
        GoalExpr = disj(_),
        (
            BeforeAfter = before,
            WillFlush = yes
        ;
            BeforeAfter = after,
            WillFlush = no
        )
    ;
        GoalExpr = if_then_else(_, _, _, _),
        (
            BeforeAfter = before,
            WillFlush = yes
        ;
            BeforeAfter = after,
            WillFlush = no
        )
    ;
        GoalExpr = negation(_),
        WillFlush = yes
    ;
        GoalExpr = scope(_, _),
        WillFlush = no
    ;
        GoalExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, _MainGoal, _OrElseGoals, _),
            WillFlush = yes
        ;
            ShortHand = try_goal(_, _, _),
            WillFlush = yes
        ;
            ShortHand = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        )
    ).

%----------------------------------------------------------------------------%

    % Ensure that the mode information and the determinism information
    % say consistent things about unreachability.
    %
:- pred enforce_unreachability_invariant(
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_info::in, simplify_info::out) is det.

enforce_unreachability_invariant(GoalInfo0, GoalInfo, !Info) :-
    Determinism0 = goal_info_get_determinism(GoalInfo0),
    InstmapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
    determinism_components(Determinism0, CanFail0, NumSolns0),
    ( if
        NumSolns0 = at_most_zero,
        instmap_delta_is_reachable(InstmapDelta0)
    then
        instmap_delta_init_unreachable(UnreachableInstMapDelta),
        goal_info_set_instmap_delta(UnreachableInstMapDelta,
            GoalInfo0, GoalInfo),
        simplify_info_set_rerun_det(!Info)
    else if
        instmap_delta_is_unreachable(InstmapDelta0),
        NumSolns0 \= at_most_zero
    then
        determinism_components(Determinism, CanFail0, at_most_zero),
        goal_info_set_determinism(Determinism, GoalInfo0, GoalInfo),
        simplify_info_set_rerun_det(!Info)
    else
        GoalInfo = GoalInfo0
    ).

%---------------------------------------------------------------------------%

simplify_maybe_wrap_goal(OuterGoalInfo, InnerGoalInfo, GoalExpr1,
        GoalExpr, GoalInfo, !Info) :-
    InnerDet = goal_info_get_determinism(InnerGoalInfo),
    OuterDet = goal_info_get_determinism(OuterGoalInfo),
    ( if InnerDet = OuterDet then
        GoalExpr = GoalExpr1,
        GoalInfo = InnerGoalInfo
    else
        % XXX There are some inner goals for which it does not make sense
        % to wrap a scope around it. This includes disj([]), which can be
        % created by e.g. delete_tail_unreachable_goals.
        GoalExpr = scope(commit(dont_force_pruning),
            hlds_goal(GoalExpr1, InnerGoalInfo)),
        GoalInfo = OuterGoalInfo,
        simplify_info_set_rerun_det(!Info)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal.
%---------------------------------------------------------------------------%
