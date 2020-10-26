%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%----------------------------------------------------------------------------%
%
% File: simplify_goal_ite.m.
%
% This module handles simplification of if-then-else goals and negations.
% (A negation is just like an if-then-else with `fail' as the then-part
% and `true' as the else-part).
%
%----------------------------------------------------------------------------%

:- module check_hlds.simplify.simplify_goal_ite.
:- interface.

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.

    % Handle simplifications of if-then-else goals.
    %
:- pred simplify_goal_ite(
    hlds_goal_expr::in(goal_expr_ite), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Handle simplifications of negations.
    %
:- pred simplify_goal_neg(
    hlds_goal_expr::in(goal_expr_neg), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

:- implementation.

:- import_module check_hlds.simplify.simplify_goal.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.make_goal.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_detism.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module one_or_more.
:- import_module require.
:- import_module set.
:- import_module varset.

%----------------------------------------------------------------------------%

simplify_goal_ite(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, Common0, Common, !Info) :-
    % (if A then B else C) is logically equivalent to (A, B ; ~A, C).
    % If the determinism of A means that one of these disjuncts cannot succeed,
    % then we replace the if-then-else with the other disjunct. (We could
    % also eliminate A, but we leave that to the recursive invocations.)
    %
    % Note however that rerunning determinism analysis, which we do
    % at the end of simplification, may introduce more occurrences of these;
    % since we don't iterate simplification and determinism analysis until
    % a fixpoint is reached, we don't guarantee to eliminate all such
    % if-then-elses. Hence the code generator must be prepared to handle
    % the case when the condition of an if-then-else has determinism
    % `det' or `failure'.
    %
    % The conjunction operator in the remaining disjunct ought to be
    % a sequential conjunction, because Mercury's if-then-else always
    % guarantees sequentiality, whereas conjunction only guarantees
    % sequentiality if the --no-reorder-conj option is enabled.
    %
    % However, currently reordering is only done in mode analysis,
    % not in the code generator, so we don't yet need a sequential
    % conjunction construct. This will change when constraint pushing
    % is finished, or when we start doing coroutining.

    GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
    Cond0 = hlds_goal(_, CondInfo0),
    CondDetism0 = goal_info_get_determinism(CondInfo0),
    determinism_components(CondDetism0, CondCanFail0, CondSolns0),
    (
        CondCanFail0 = cannot_fail,
        goal_to_conj_list(Cond0, CondGoals),
        goal_to_conj_list(Then0, ThenGoals),
        Goals = CondGoals ++ ThenGoals,
        simplify_goal(hlds_goal(conj(plain_conj, Goals), GoalInfo0),
            hlds_goal(GoalExpr, GoalInfo), NestedContext0, InstMap0,
            Common0, Common, !Info),
        maybe_warn_about_condition(GoalInfo0, NestedContext0, "cannot fail",
            !Info),

        simplify_info_get_deleted_call_callees(!.Info, DeletedCallCallees0),
        SubGoalCalledProcs = goal_proc_refs(Else0),
        set.union(SubGoalCalledProcs,
            DeletedCallCallees0, DeletedCallCallees),
        simplify_info_set_deleted_call_callees(DeletedCallCallees, !Info)
    ;
        CondCanFail0 = can_fail,
        (
            CondSolns0 = at_most_zero,
            % Optimize away the condition and the `then' part.
            det_negation_det(CondDetism0, MaybeNegDetism),
            ( if
                Cond0 = hlds_goal(negation(NegCond), _),
                % XXX BUG! This optimization is only safe if it preserves mode
                % correctness, which means in particular that the negated goal
                % must not clobber any variables. For now I've just disabled
                % the optimization.
                semidet_fail
            then
                Cond = NegCond
            else
                ( if
                    MaybeNegDetism = yes(NegDetism1),
                    (
                        NegDetism1 = detism_erroneous,
                        instmap_delta_init_unreachable(NegInstMapDelta1)
                    ;
                        NegDetism1 = detism_det,
                        instmap_delta_init_reachable(NegInstMapDelta1)
                    )
                then
                    NegDetism = NegDetism1,
                    NegInstMapDelta = NegInstMapDelta1
                else
                    unexpected($pred, "cannot get negated determinism")
                ),
                goal_info_set_determinism(NegDetism, CondInfo0, NegCondInfo0),
                goal_info_set_instmap_delta(NegInstMapDelta,
                    NegCondInfo0, NegCondInfo),
                Cond = hlds_goal(negation(Cond0), NegCondInfo)
            ),
            goal_to_conj_list(Else0, ElseList),
            List = [Cond | ElseList],
            simplify_goal(hlds_goal(conj(plain_conj, List), GoalInfo0),
                hlds_goal(GoalExpr, GoalInfo), NestedContext0, InstMap0,
                Common0, Common, !Info),
            maybe_warn_about_condition(GoalInfo0, NestedContext0,
                "cannot succeed", !Info),

            simplify_info_get_deleted_call_callees(!.Info,
                DeletedCallCallees0),
            SubGoalCalledProcs = goal_proc_refs(Then0),
            set.union(SubGoalCalledProcs,
                DeletedCallCallees0, DeletedCallCallees),
            simplify_info_set_deleted_call_callees(DeletedCallCallees, !Info)
        ;
            ( CondSolns0 = at_most_one
            ; CondSolns0 = at_most_many
            ; CondSolns0 = at_most_many_cc
            ),
            ( if Else0 = hlds_goal(disj([]), _) then
                % (if Cond then Then else fail) is equivalent to (Cond, Then)
                goal_to_conj_list(Cond0, CondGoals),
                goal_to_conj_list(Then0, ThenGoals),
                Goals = CondGoals ++ ThenGoals,
                simplify_goal(hlds_goal(conj(plain_conj, Goals), GoalInfo0),
                    hlds_goal(GoalExpr, GoalInfo), NestedContext0, InstMap0,
                    Common0, Common, !Info),
                simplify_info_set_rerun_quant_instmap_delta(!Info),
                simplify_info_set_rerun_det(!Info)
            else
                simplify_goal_ordinary_ite(Vars, Cond0, Then0, Else0, GoalExpr,
                    GoalInfo0, GoalInfo, NestedContext0, InstMap0,
                    Common0, Common, !Info)
            )
        )
    ).

:- pred maybe_warn_about_condition(hlds_goal_info::in,
    simplify_nested_context::in, string::in,
    simplify_info::in, simplify_info::out) is det.

maybe_warn_about_condition(GoalInfo0, NestedContext0, Problem, !Info) :-
    InsideDuplForSwitch = NestedContext0 ^ snc_inside_dupl_for_switch,
    (
        InsideDuplForSwitch = yes
        % Do not generate the warning, since it is quite likely to be
        % spurious: though the condition cannot fail/succeed in this arm
        % of the switch, it likely can fail/succeed in other arms
        % that derive from the exact same piece of source code.
    ;
        InsideDuplForSwitch = no,
        Context = goal_info_get_context(GoalInfo0),
        Pieces = [words("Warning: the condition of this if-then-else"),
            words(Problem), suffix("."), nl],
        Spec = conditional_spec($pred, warn_simple_code, yes,
            severity_warning, phase_simplify(report_only_if_in_all_modes),
            [simplest_msg(Context, Pieces)]),
        simplify_info_add_message(Spec, !Info)
    ),
    simplify_info_set_rerun_quant_instmap_delta(!Info),
    simplify_info_set_rerun_det(!Info).

%----------------------------------------------------------------------------%

:- pred simplify_goal_ordinary_ite(list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

simplify_goal_ordinary_ite(Vars, Cond0, Then0, Else0, GoalExpr,
        GoalInfo0, GoalInfo, NestedContext0, InstMap0, Common0, Common,
        !Info) :-
    % Recursively simplify the sub-goals, and rebuild the resulting
    % if-then-else.

    simplify_goal(Cond0, Cond, NestedContext0, InstMap0,
        Common0, AfterCondCommon, !Info),
    update_instmap(Cond, InstMap0, AfterCondInstMap0),
    simplify_goal(Then0, Then, NestedContext0, AfterCondInstMap0,
        AfterCondCommon, _AfterThenCommon, !Info),

    simplify_goal(Else0, Else, NestedContext0, InstMap0,
        Common0, _AfterElseCommon, !Info),

    Cond = hlds_goal(_, CondInfo),
    CondDelta = goal_info_get_instmap_delta(CondInfo),
    Then = hlds_goal(_, ThenInfo),
    ThenDelta = goal_info_get_instmap_delta(ThenInfo),
    instmap_delta_apply_instmap_delta(CondDelta, ThenDelta, test_size,
        CondThenDelta),
    Else = hlds_goal(_, ElseInfo),
    ElseDelta = goal_info_get_instmap_delta(ElseInfo),
    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    some [!ModuleInfo] (
        simplify_info_get_module_info(!.Info, !:ModuleInfo),
        simplify_info_get_var_types(!.Info, VarTypes),
        merge_instmap_deltas(InstMap0, NonLocals, VarTypes,
            [CondThenDelta, ElseDelta], NewDelta, !ModuleInfo),
        simplify_info_set_module_info(!.ModuleInfo, !Info)
    ),
    goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo1),
    IfThenElseExpr = if_then_else(Vars, Cond, Then, Else),

    IfThenElseDetism0 = goal_info_get_determinism(GoalInfo0),
    determinism_components(IfThenElseDetism0, IfThenElseCanFail,
        IfThenElseNumSolns),

    CondDetism = goal_info_get_determinism(CondInfo),
    determinism_components(CondDetism, CondCanFail, CondSolns),
    ( if
        % Check again if we can apply one of the above simplifications
        % after having simplified the sub-goals (we need to do this
        % to ensure that the goal is fully simplified, to maintain the
        % invariants that the MLDS backend depends on).
        ( CondCanFail = cannot_fail
        ; CondSolns = at_most_zero
        ; Else = hlds_goal(disj([]), _)
        )
    then
        simplify_goal_expr(IfThenElseExpr, GoalExpr, GoalInfo1, GoalInfo,
            NestedContext0, InstMap0, Common0, Common, !Info)
    else
        % Any structures generated in one branch won't be available after the
        % if-the-else as a whole if execution takes the other branch.
        Common = Common0,

        simplify_info_get_module_info(!.Info, ModuleInfo),
        warn_switch_for_ite_cond(ModuleInfo, VarTypes, Cond,
            cond_can_switch_uncommitted, CanSwitch),
        (
            CanSwitch = cond_can_switch_on(SwitchVar),
            Context = goal_info_get_context(CondInfo),
            simplify_info_get_varset(!.Info, VarSet),
            Pieces0 = [words("Warning: this if-then-else"),
                words("could be replaced by a switch")],
            ( if varset.search_name(VarSet, SwitchVar, SwitchVarName) then
                OnPieces = [words("on"), quote(SwitchVarName)]
            else
                OnPieces = []
            ),
            Pieces = Pieces0 ++ OnPieces ++ [suffix("."), nl],
            Spec = conditional_spec($pred, inform_ite_instead_of_switch, yes,
                severity_informational, phase_simplify(report_in_any_mode),
                [simplest_msg(Context, Pieces)]),
            simplify_info_add_message(Spec, !Info)
        ;
            CanSwitch = cond_can_switch_uncommitted
        ;
            CanSwitch = cond_cannot_switch
        ),
        ( if
            % If-then-elses that are det or semidet may nevertheless contain
            % nondet or multi conditions. If this happens, we put the
            % if-then-else inside a commit scope, since the code generators
            % need to know where they should change the code's execution
            % mechanism.

            simplify_do_mark_code_model_changes(!.Info),
            CondSolns = at_most_many,
            IfThenElseNumSolns \= at_most_many
        then
            determinism_components(InnerDetism, IfThenElseCanFail,
                at_most_many),
            goal_info_set_determinism(InnerDetism, GoalInfo1, InnerInfo),
            GoalExpr = scope(commit(dont_force_pruning),
                hlds_goal(IfThenElseExpr, InnerInfo))
        else
            GoalExpr = IfThenElseExpr
        ),
        GoalInfo = GoalInfo1
    ).

:- type cond_can_switch
    --->    cond_can_switch_uncommitted
    ;       cond_can_switch_on(prog_var)
    ;       cond_cannot_switch.

:- pred warn_switch_for_ite_cond(module_info::in, vartypes::in, hlds_goal::in,
    cond_can_switch::in, cond_can_switch::out) is det.

warn_switch_for_ite_cond(ModuleInfo, VarTypes, Cond, !CondCanSwitch) :-
    Cond = hlds_goal(CondExpr, _CondInfo),
    (
        CondExpr = unify(_LHSVar, _RHS, _Mode, Unification, _UContext),
        (
            ( Unification = construct(_, _, _, _, _, _, _)
            ; Unification = assign(_, _)
            ; Unification = simple_test(_, _)
            ),
            !:CondCanSwitch = cond_cannot_switch
        ;
            Unification = deconstruct(LHSVar, _ConsId, _Args, _ArgModes,
                _CanFail, _CanCGC),
            lookup_var_type(VarTypes, LHSVar, LHSVarType),
            ( if type_to_type_defn_body(ModuleInfo, LHSVarType, TypeBody) then
                CanSwitchOnType = can_switch_on_type(TypeBody),
                (
                    CanSwitchOnType = no,
                    !:CondCanSwitch = cond_cannot_switch
                ;
                    CanSwitchOnType = yes,
                    (
                        !.CondCanSwitch = cond_can_switch_uncommitted,
                        !:CondCanSwitch = cond_can_switch_on(LHSVar)
                    ;
                        !.CondCanSwitch = cond_can_switch_on(SwitchVar),
                        ( if SwitchVar = LHSVar then
                            true
                        else
                            !:CondCanSwitch = cond_cannot_switch
                        )
                    ;
                        !.CondCanSwitch = cond_cannot_switch
                    )
                )
            else
                % You cannot have a switch on a type with no body (e.g. a
                % builtin type such as int).
                !:CondCanSwitch = cond_cannot_switch
            )
        ;
            Unification = complicated_unify(_, _, _),
            unexpected($pred, "complicated unify")
        )
    ;
        CondExpr = disj(Disjuncts),
        list.foldl(warn_switch_for_ite_cond(ModuleInfo, VarTypes), Disjuncts,
            !CondCanSwitch)
    ;
        CondExpr = negation(SubGoal),
        (
            !.CondCanSwitch = cond_can_switch_uncommitted,
            warn_switch_for_ite_cond(ModuleInfo, VarTypes, SubGoal,
                !CondCanSwitch)
        ;
            !.CondCanSwitch = cond_can_switch_on(_),
            % The condition cannot do both.
            !:CondCanSwitch = cond_cannot_switch
        ;
            !.CondCanSwitch = cond_cannot_switch
        )
    ;
        ( CondExpr = plain_call(_, _, _, _, _, _)
        ; CondExpr = generic_call(_, _, _, _, _)
        ; CondExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; CondExpr = conj(_, _)
        ; CondExpr = switch(_, _, _)
        ; CondExpr = scope(_, _)
        ; CondExpr = if_then_else(_, _, _, _)
        ),
        !:CondCanSwitch = cond_cannot_switch
    ;
        CondExpr = shorthand(ShortHand),
        (
            ShortHand = atomic_goal(_, _, _, _, _, _, _),
            !:CondCanSwitch = cond_cannot_switch
        ;
            ShortHand = try_goal(_, _, _),
            !:CondCanSwitch = cond_cannot_switch
        ;
            ShortHand = bi_implication(_, _),
            unexpected($pred, "shorthand")
        )
    ).

:- func can_switch_on_type(hlds_type_body) = bool.

can_switch_on_type(TypeBody) = CanSwitchOnType :-
    (
        TypeBody = hlds_du_type(Ctors, _MaybeUserEq, _MaybeRepn,
            _MaybeForeignType),
        % We don't care about _MaybeUserEq, since the unification with *any*
        % functor of the type indicates that we are deconstructing the physical
        % representation, not the logical value.
        %
        % We don't care about _MaybeRepn, since that contains only
        % implementation details (which may be of interest when a code
        % generator decides *how* to implement the switch, but which cannot
        % affected *whether* we can implement the switch).
        %
        % We don't care about _MaybeForeignType, since the unification with
        % *any* functor of the type means that either there is no foreign type
        % version, or we are using the Mercury version of the type.
        %
        % We *do* care that the type has at least two constructors.
        ( if Ctors = one_or_more(_, [_ | _]) then
            CanSwitchOnType = yes
        else
            CanSwitchOnType = no
        )
    ;
        TypeBody = hlds_eqv_type(_),
        % The type of the variable should have had any equivalences expanded
        % out of it before simplify.
        unexpected($pred, "eqv type")
    ;
        TypeBody = hlds_foreign_type(_),
        % If the type is foreign, how can a Mercury unification use it?
        unexpected($pred, "foreign type")
    ;
        TypeBody = hlds_abstract_type(_),
        % If the type is abstract, how can a Mercury unification use it?
        unexpected($pred, "abstract type")
    ;
        TypeBody = hlds_solver_type(_),
        % Any unifications on constrained variables should be done on the
        % representation type, and the type of the variable in the unification
        % should be the representation type, not the solver type.
        unexpected($pred, "solver type")
    ).

%---------------------------------------------------------------------------%

simplify_goal_neg(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = negation(SubGoal0),
    % Can't use calls or unifications seen within a negation,
    % since non-local variables may not be bound within the negation.
    simplify_goal(SubGoal0, SubGoal1, NestedContext0, InstMap0,
        Common0, _Common1, !Info),
    InsideDuplForSwitch = NestedContext0 ^ snc_inside_dupl_for_switch,
    Context = goal_info_get_context(GoalInfo0),
    (
        InsideDuplForSwitch = no,
        SubGoal1 = hlds_goal(_, SubGoalInfo1),
        Detism = goal_info_get_determinism(SubGoalInfo1),
        determinism_components(Detism, CanFail, MaxSoln),
        ( if CanFail = cannot_fail then
            Pieces = [words("Warning: the negated goal cannot fail.")],
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_simplify(report_only_if_in_all_modes),
                [simplest_msg(Context, Pieces)]),
            simplify_info_add_message(Spec, !Info)
        else if MaxSoln = at_most_zero then
            Pieces = [words("Warning: the negated goal cannot succeed.")],
            Spec = conditional_spec($pred, warn_simple_code, yes,
                severity_warning, phase_simplify(report_only_if_in_all_modes),
                [simplest_msg(Context, Pieces)]),
            simplify_info_add_message(Spec, !Info)
        else
            true
        )
    ;
        InsideDuplForSwitch = yes
        % We don't want to generate either of the above warnings for code
        % that has been duplicated for a switch, since the warned-about
        % condition may not (and typically does not) exist in the other
        % copies.
    ),
    ( if
        % Replace `not true' with `fail'.
        SubGoal1 = hlds_goal(conj(plain_conj, []), _)
    then
        hlds_goal(GoalExpr, GoalInfo) = fail_goal_with_context(Context)
    else if
        % Replace `not fail' with `true'.
        SubGoal1 = hlds_goal(disj([]), _)
    then
        hlds_goal(GoalExpr, GoalInfo) = true_goal_with_context(Context)
    else if
        % Remove double negation.
        SubGoal1 =
            hlds_goal(negation(hlds_goal(SubSubGoal, SubSubGoalInfo)), _),
        % XXX BUG! This optimization is only safe if it preserves
        % mode correctness, which means in particular that the
        % the negated goal must not clobber any variables.
        % For now I've just disabled the optimization.
        semidet_fail
    then
        simplify_maybe_wrap_goal(GoalInfo0, SubSubGoalInfo, SubSubGoal,
            GoalExpr, GoalInfo, !Info)
    else
        GoalExpr = negation(SubGoal1),
        GoalInfo = GoalInfo0
    ),
    % Execution continues after the negation scope iff the goal inside the
    % scope failed. In general, we don't know when it failed, so we cannot
    % depend on it having created any structures.
    Common = Common0.

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_ite.
%---------------------------------------------------------------------------%
