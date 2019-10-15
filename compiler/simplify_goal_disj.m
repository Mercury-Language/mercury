%----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%----------------------------------------------------------------------------%
% Copyright (C) 2014-2015 The Mercury team.
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

:- import_module check_hlds.simplify.common.
:- import_module check_hlds.simplify.simplify_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.instmap.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

    % Handle simplifications of disjunctions.
    %
:- pred simplify_goal_disj(
    hlds_goal_expr::in(goal_expr_disj), hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

    % Handle simplifications of atomic goals.
    %
:- pred simplify_goal_atomic_goal(atomic_goal_type::in,
    atomic_interface_vars::in, atomic_interface_vars::in,
    maybe(list(prog_var))::in, hlds_goal::in, list(hlds_goal)::in,
    list(atomic_interface_vars)::in,
    hlds_goal_expr::out, hlds_goal_info::in, hlds_goal_info::out,
    simplify_nested_context::in, instmap::in,
    common_info::in, common_info::out,
    simplify_info::in, simplify_info::out) is det.

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.simplify.simplify_goal.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_module.
:- import_module hlds.make_goal.
:- import_module libs.
:- import_module libs.options.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.set_of_var.

:- import_module bool.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module set.
:- import_module term.
:- import_module varset.

simplify_goal_disj(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo,
        NestedContext0, InstMap0, Common0, Common, !Info) :-
    GoalExpr0 = disj(Disjuncts0),
    simplify_disj(Disjuncts0, [], Disjuncts, NestedContext0, InstMap0, Common0,
        [], InstMapDeltas, !Info),
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
        ( if
            goal_info_has_feature(GoalInfo0, feature_mode_check_clauses_goal)
        then
            % Recomputing the instmap delta would take very long
            % and is very unlikely to get any better precision.
            GoalInfo = GoalInfo0
        else
            simplify_info_get_module_info(!.Info, ModuleInfo1),
            NonLocals = goal_info_get_nonlocals(GoalInfo0),
            simplify_info_get_var_types(!.Info, VarTypes),
            merge_instmap_deltas(InstMap0, NonLocals, VarTypes, InstMapDeltas,
                NewDelta, ModuleInfo1, ModuleInfo2),
            simplify_info_set_module_info(ModuleInfo2, !Info),
            goal_info_set_instmap_delta(NewDelta, GoalInfo0, GoalInfo),

            ( if
                simplify_do_after_front_end(!.Info),
                % If the surrounding procedure cannot have more than one
                % solution, then any warning about not being able to compute
                % the set of all solutions would be meaningless and confusing.
                NestedContext0 ^ snc_proc_is_model_non = yes(Innermost)
            then
                warn_about_any_problem_partial_vars(Innermost, GoalInfo0,
                    InstMap0, NewDelta, !Info)
            else
                true
            )
        )
    ),
    % Any information that is in the updated Common at the end of a disjunct
    % is valid only for that disjunct. We cannot use that information after
    % the disjunction as a whole unless the disjunction turns out to have
    % only one disjunct. Currently, simplify_disj does not bother returning
    % the commons at the ends of disjuncts, since we expect one-disjunct
    % disjunctions to be so rare that they are not worth optimizing.
    Common = Common0,

    list.length(Disjuncts, DisjunctsLength),
    list.length(Disjuncts0, Disjuncts0Length),
    ( if DisjunctsLength = Disjuncts0Length then
        true
    else
        % If we pruned some disjuncts, variables used by those disjuncts
        % may no longer be non-local to the disjunction. Also, the
        % determinism may have changed (especially if we pruned all the
        % disjuncts). If the disjunction now can't succeed, we have to
        % recompute instmap_deltas and rerun determinism analysis
        % to avoid aborts in the code generator because the disjunction
        % now cannot produce variables it did before.

        simplify_info_set_should_requantify(!Info),
        simplify_info_set_should_rerun_det(!Info)
    ).

    % Look for the kind of bug represented by tests/invalid/bug311.m.
    % For a detailed description of the problem, see that test case.
    %
:- pred warn_about_any_problem_partial_vars(innermost_proc::in,
    hlds_goal_info::in, instmap::in, instmap_delta::in,
    simplify_info::in, simplify_info::out) is det.

warn_about_any_problem_partial_vars(Innermost, GoalInfo, InstMap0,
        InstMapDelta, !Info) :-
    instmap_delta_to_assoc_list(InstMapDelta, InstMapDeltaChanges),
    simplify_info_get_module_info(!.Info, ModuleInfo),
    list.filter_map(is_var_a_problem_partial_var(ModuleInfo, InstMap0),
        InstMapDeltaChanges, ProblemPartialVars),
    (
        ProblemPartialVars = []
    ;
        ProblemPartialVars = [_ | _],
        (
            Innermost = imp_whole_proc,
            ProcStr = "the procedure"
        ;
            Innermost = imp_lambda(LambdaContext),
            % If the lambda expression does not leave the scope of its
            % defining procedure and does not have an all-solutions predicate
            % invoked on it, the warning we generate could be a bit misleading.
            % Unfortunately, without an escape analysis, I (zs) see no way
            % to avoid this while still generating the warning in cases
            % where the program *does* invoke an all-solutions predicate
            % on the closure generated by the lambda expression.
            term.context_file(LambdaContext, LambdaFileName),
            term.context_line(LambdaContext, LambdaLineNum),
            ( if LambdaFileName = "" then
                string.format("the lambda expression at line %d",
                    [i(LambdaLineNum)], ProcStr)
            else
                string.format("the lambda expression in %s at line %d",
                    [s(LambdaFileName), i(LambdaLineNum)], ProcStr)
            )
        ),
        simplify_info_get_varset(!.Info, VarSet),
        list.map(varset.lookup_name(VarSet), ProblemPartialVars,
            ProblemPartialVarNames),
        ProblemPartialVarPieces = list_to_pieces(ProblemPartialVarNames),
        Context = goal_info_get_context(GoalInfo),
        Pieces = [words("Warning: this disjunction further instantiates"),
            words("the already partially instantiated"),
            words(choose_number(ProblemPartialVars, "variable", "variables"))]
            ++ ProblemPartialVarPieces ++ [suffix("."), nl] ++
            [words(choose_number(ProblemPartialVars,
                "Since the memory cell of this variable
                    is allocated *before* the disjunction,",
                "Since the memory cells of these variables
                    are allocated *before* the disjunction,")),
            words("the different disjuncts will return"),
            words("their potentially different solutions"),
            words(choose_number(ProblemPartialVars,
                "for this variable", "for each of these variables")),
            words("in the same memory cell,"),
            words("which will cause any all-solutions predicate"),
            words("to think that the different solutions"),
            words("(since they are at the same address)"),
            words("are in fact all the same"),
            words("when invoked on"), words(ProcStr), suffix("."), nl],
        Spec = simplest_spec(severity_warning,
            phase_simplify(report_in_any_mode), Context, Pieces),
        simplify_info_add_message(Spec, !Info)
    ).

    % Check whether a variable suffers from the problem of bug 311.
    %
:- pred is_var_a_problem_partial_var(module_info::in, instmap::in,
    pair(prog_var, mer_inst)::in, prog_var::out) is semidet.

is_var_a_problem_partial_var(ModuleInfo, InstMap0, Var - FinalInst, Var) :-
    instmap_lookup_var(InstMap0, Var, InitInst),
    ( if inst_is_free(ModuleInfo, InitInst) then
        % No problem: the cell containing the variable is NOT allocated
        % before the disjunction, so its address won't be the same in
        % different arms.
        fail
    else if inst_is_ground(ModuleInfo, InitInst) then
        % No problem: the variable's initial value cannot be changed
        % by the disjunction.
        fail
    else if inst_matches_final(FinalInst, InitInst, ModuleInfo) then
        % No problem: the variable's value, even though it is not initially
        % ground, is not changed by the disjunction, though the disjunction
        % may discover e.g. that a particular part of Var is bound to a
        % particular function symbol.
        %
        % We do this test last, because it is much the slowest, and covers
        % the rarest case.
        fail
    else
        % Problem: the disjunction DOES make further bindings to this
        % already partially instantiated variable.
        true
    ).

%---------------------------------------------------------------------------%

:- pred simplify_disj(list(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::out,
    simplify_nested_context::in, instmap::in, common_info::in,
    list(instmap_delta)::in, list(instmap_delta)::out,
    simplify_info::in, simplify_info::out) is det.

simplify_disj([], RevGoals, Goals,
        _NestedContext0, _InstMap0, _Common0, !PostBranchInstMaps, !Info) :-
    list.reverse(RevGoals, Goals).
simplify_disj([Goal0 | Goals0], RevGoals0, Goals,
        NestedContext0, InstMap0, Common0, !PostBranchInstMaps, !Info) :-
    simplify_goal(Goal0, Goal, NestedContext0, InstMap0,
        Common0, _Common1, !Info),
    Goal = hlds_goal(_, GoalInfo),
    Purity = goal_info_get_purity(GoalInfo),

    ( if
        % Don't prune or warn about impure disjuncts that can't succeed.
        Purity \= purity_impure,
        Detism = goal_info_get_determinism(GoalInfo),
        determinism_components(Detism, _CanFail, MaxSolns),
        MaxSolns = at_most_zero
    then
        ( if
            simplify_do_warn_simple_code(!.Info),
            % Don't warn where the initial goal was fail, since that can result
            % from mode analysis pruning away cases in a switch which cannot
            % succeed due to sub-typing in the modes.
            Goal0 \= hlds_goal(disj([]), _),
            % Don't warn if the code was duplicated, since it is quite likely
            % to be spurious: though the disjunct cannot succeed in this arm of
            % the switch, it likely can succeed in other arms that derive from
            % the exact same piece of source code.
            NestedContext0 ^ snc_inside_dupl_for_switch = no
        then
            Context = goal_info_get_context(GoalInfo),
            Pieces = [words("Warning: this disjunct"),
                words("will never have any solutions.")],
            Msg = simple_msg(Context,
                [option_is_set(warn_simple_code, yes, [always(Pieces)])]),
            Severity = severity_conditional(warn_simple_code, yes,
                severity_warning, no),
            Spec = error_spec(Severity,
                phase_simplify(report_only_if_in_all_modes), [Msg]),
            simplify_info_add_message(Spec, !Info)
        else
            true
        ),

        % Prune away non-succeeding disjuncts where possible.
        ( if
            (
                Goal0 = hlds_goal(disj([]), _)
            ;
                % Only remove disjuncts that might loop
                % or call error/1 if --no-fully-strict.
                simplify_info_get_fully_strict(!.Info, no)
            )
        then
            simplify_info_get_deleted_call_callees(!.Info,
                DeletedCallCallees0),
            SubGoalCalledProcs = goal_proc_refs(Goal),
            set.union(SubGoalCalledProcs,
                DeletedCallCallees0, DeletedCallCallees),
            simplify_info_set_deleted_call_callees(DeletedCallCallees, !Info),
            RevGoals1 = RevGoals0
        else
            RevGoals1 = [Goal | RevGoals0],
            InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
            !:PostBranchInstMaps = [InstMapDelta | !.PostBranchInstMaps]
        )
    else
        RevGoals1 = [Goal | RevGoals0],
        InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
        !:PostBranchInstMaps = [InstMapDelta | !.PostBranchInstMaps]
    ),
    simplify_disj(Goals0, RevGoals1, Goals, NestedContext0, InstMap0, Common0,
        !PostBranchInstMaps, !Info).

    % Disjunctions that cannot succeed more than once when viewed from the
    % outside generally need some fixing up, and/or some warnings to be issued.
    %
    % We previously converted them all to if-then-elses using the code below,
    % however converting disjs that have output variables but that nevertheless
    % cannot succeed more than once (e.g. cc_nondet or cc_multi disjs) into
    % if-then-elses may cause problems with other parts of the compiler that
    % assume that an if-then-else is mode-correct, i.e. that the condition
    % doesn't bind variables.
    %
    %       goal_info_get_determinism(GoalInfo, Detism),
    %       determinism_components(Detism, _CanFail, MaxSoln),
    %       MaxSoln \= at_most_many
    %   then
    %       goal_info_get_instmap_delta(GoalInfo, DeltaInstMap),
    %       goal_info_get_nonlocals(GoalInfo, NonLocalVars),
    %       ( if
    %           det_no_output_vars(NonLocalVars, InstMap0,
    %               DeltaInstMap, DetInfo)
    %       then
    %           OutputVars = no
    %       else
    %           OutputVars = yes
    %       ),
    %       fixup_disj(Disjuncts, Detism, OutputVars, GoalInfo, InstMap0,
    %           DetInfo, Goal, MsgsA, Msgs)
    %   else
    %
:- pred fixup_disj(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal_expr::out,
    simplify_nested_context::in, instmap::in, common_info::in,
    simplify_info::in, simplify_info::out) is det.
:- pragma consider_used(fixup_disj/8).

fixup_disj(Disjuncts, GoalInfo, Goal, NestedContext0, InstMap0, Common0,
        !Info) :-
    det_disj_to_ite(Disjuncts, GoalInfo, IfThenElse),
    simplify_goal(IfThenElse, Simplified, NestedContext0, InstMap0,
        Common0, _Common, !Info),
    Simplified = hlds_goal(Goal, _).

    % det_disj_to_ite is used to transform disjunctions that occur
    % in prunable contexts into if-then-elses.
    % For example, it would transform
    %
    %   ( Disjunct1
    %   ; Disjunct2
    %   ; Disjunct3
    %   )
    %
    % into
    %
    %   ( if Disjunct1 then
    %       true
    %   else if Disjunct2 then
    %       true
    %   else
    %       Disjunct3
    %   ).
    %
:- pred det_disj_to_ite(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal::out) is det.
:- pragma consider_used(det_disj_to_ite/3).

det_disj_to_ite([], _GoalInfo, _) :-
    unexpected($pred, "reached base case").
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
        ( if MaxSoln0 = at_most_many then
            MaxSoln = at_most_one
        else
            MaxSoln = MaxSoln0
        ),
        determinism_components(Detism, CanFail, MaxSoln),
        goal_info_set_determinism(Detism, NewGoalInfo1, NewGoalInfo),

        Goal = hlds_goal(if_then_else([], Cond, Then, Rest), NewGoalInfo)
    ).

%---------------------------------------------------------------------------%

simplify_goal_atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
        MainGoal0, OrElseGoals0, OrElseInners, GoalExpr, !GoalInfo,
        _NestedContext0, _InstMap0, Common0, Common0, !Info) :-
    % XXX STM: At the moment we do not simplify the inner goals as there is
    % a chance that the outer and inner variables will change which will
    % cause problems during expansion of STM constructs. This will be
    % fixed eventually.
    MainGoal = MainGoal0,
    OrElseGoals = OrElseGoals0,
    % simplify_goal(MainGoal0, MainGoal,
    %   NestedContext0, InstMap0, Common0, _AfterMainCommon, !Info),
    % simplify_or_else_goals(OrElseGoals0, OrElseGoals,
    %   NestedContext0, InstMap0, Common0, !Info),
    ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
        MainGoal, OrElseGoals, OrElseInners),
    GoalExpr = shorthand(ShortHand).

:- pred simplify_or_else_goals(list(hlds_goal)::in, list(hlds_goal)::out,
    simplify_nested_context::in, instmap::in, common_info::in,
    simplify_info::in, simplify_info::out) is det.
:- pragma consider_used(simplify_or_else_goals/7).

simplify_or_else_goals([], [], _NestedContext0, _InstMap0, _Common0, !Info).
simplify_or_else_goals([Goal0 | Goals0], [Goal | Goals],
        NestedContext0, InstMap0, Common0, !Info) :-
    simplify_goal(Goal0, Goal, NestedContext0, InstMap0,
        Common0, _Common1, !Info),
    simplify_or_else_goals(Goals0, Goals,
        NestedContext0, InstMap0, Common0, !Info).

%---------------------------------------------------------------------------%
:- end_module check_hlds.simplify.simplify_goal_disj.
%---------------------------------------------------------------------------%
