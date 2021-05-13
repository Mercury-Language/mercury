%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% Copyright (C) 2014-2019, 2021 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: modecheck_goal.m.
% Main author: fjh.
%
% To mode-analyse a goal:
% If goal is
%   (a) a disjunction
%       Mode-analyse the sub-goals;
%       check that the final insts of all the non-local variables are the same
%       for all the sub-goals.
%   (b) a conjunction
%       Attempt to schedule each sub-goal. If a sub-goal can be scheduled,
%       then schedule it, otherwise delay it. Continue with the remaining
%       subgoals until there are no goals left. Every time a variable gets
%       bound, see whether we should wake up a delayed goal, and if so,
%       wake it up next time we get back to the conjunction. If there are
%       still delayed goals hanging around at the end of the conjunction,
%       report a mode error.
%   (c) a negation
%       Mode-check the sub-goal.
%       Check that the sub-goal does not further instantiate any nonlocal
%       variables. (Actually, rather than doing this check after we
%       mode-analyse the subgoal, we instead "lock" the non-local variables,
%       and disallow binding of locked variables.)
%   (d) a unification
%       Check that the unification doesn't attempt to unify two free variables
%       (or in general two free sub-terms) unless one of them is dead.
%       Split unifications up if necessary to avoid complicated
%       sub-unifications. We also figure out at this point whether or not each
%       unification can fail.
%   (e) a predicate call
%       Check that there is a mode declaration for the predicate which matches
%       the current instantiation of the arguments. (Also handle calls
%       to implied modes.) If the called predicate is one for which
%       we must infer the modes, then create a new mode for the called
%       predicate whose initial insts are the result of normalising
%       the current inst of the arguments.
%   (f) an if-then-else
%       Attempt to schedule the condition. If successful, then check that
%       it doesn't further instantiate any nonlocal variables, modecheck
%       the `then' part and the `else' part, and then check that the final
%       insts match. (Perhaps also think about expanding if-then-elses so that
%       they can be run backwards, if the condition can't be scheduled?)
%
% To attempt to schedule a goal, first mode-check the goal. If mode-checking
% succeeds, then scheduling succeeds. If mode-checking would report an error
% due to the binding of a local variable, then scheduling fails.
% (If mode-checking would report an error due to the binding of a *local*
% variable, we could report the error right away -- but this idea has
% not yet been implemented.)
%
% Note that the notion of liveness used here is different to that used
% in liveness.m and the code generator. Here, we consider a variable live
% if its value will be used later on in the computation.
%
% XXX We ought to allow unification of free with free even when both
% *variables* are live, if one of the particular *sub-nodes* is dead
% (causes problems handling e.g. `list.same_length').
%
% XXX We ought to break unifications into "micro-unifications", because
% some code can't be scheduled without splitting up unifications.
% For example, `p(X) :- X = f(A, B), B is A + 1.', where p is declared as
% `:- mode p(bound(f(ground,free))->ground).'.
%
% XXX At the moment we don't check for circular modes or insts.
% If they aren't used, the compiler will probably not detect the error;
% if they are, it will probably go into an infinite loop.
%
%-----------------------------------------------------------------------------%

:- module check_hlds.modecheck_goal.
:- interface.

:- import_module check_hlds.mode_info.
:- import_module hlds.
:- import_module hlds.hlds_goal.

    % Modecheck a goal by abstractly interpreting it, as explained
    % at the top of this file.
    %
    % Input-output:
    %  InstMap          Stored in ModeInfo
    %  DelayInfo        Stored in ModeInfo
    %  Goal             Passed as an argument pair
    % Input only:
    %  ModuleInfo       Stored in ModeInfo (constant)
    %  Context          Stored in ModeInfo (changing as we go along the clause)
    % Output only:
    %  Error Messages   Stored in ModeInfo (updated as we find errors)
    %
:- pred modecheck_goal(hlds_goal::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

    % Mode-check a single goal-expression.
    %
:- pred modecheck_goal_expr(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_util.
:- import_module check_hlds.mode_debug.
:- import_module check_hlds.mode_errors.
:- import_module check_hlds.modecheck_call.
:- import_module check_hlds.modecheck_coerce.
:- import_module check_hlds.modecheck_conj.
:- import_module check_hlds.modecheck_unify.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.polymorphism_type_info.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_data.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.pred_table.
:- import_module hlds.vartypes.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_event.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_event.
:- import_module parse_tree.prog_mode.
:- import_module parse_tree.prog_rename.
:- import_module parse_tree.set_of_var.

:- import_module bag.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module std_util.
:- import_module string.
:- import_module term.
:- import_module uint.
:- import_module varset.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

modecheck_goal(Goal0, Goal, !ModeInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    % Note: any changes here may need to be duplicated in unique_modes.m.

    % Store the current context in the mode_info.
    Context = goal_info_get_context(GoalInfo0),
    ( if is_dummy_context(Context) then
        true
    else
        mode_info_set_context(Context, !ModeInfo)
    ),
    ( if goal_info_has_feature(GoalInfo0, feature_duplicated_for_switch) then
        mode_info_get_in_dupl_for_switch(!.ModeInfo, InDuplForSwitch),
        mode_info_set_in_dupl_for_switch(in_dupl_for_switch, !ModeInfo),
        modecheck_goal_2(GoalExpr0, GoalInfo0, Goal, !ModeInfo),
        mode_info_set_in_dupl_for_switch(InDuplForSwitch, !ModeInfo)
    else
        modecheck_goal_2(GoalExpr0, GoalInfo0, Goal, !ModeInfo)
    ).

:- pred modecheck_goal_2(hlds_goal_expr::in, hlds_goal_info::in,
    hlds_goal::out, mode_info::in, mode_info::out) is det.
:- pragma inline(modecheck_goal_2/5).

modecheck_goal_2(GoalExpr0, GoalInfo0, Goal, !ModeInfo) :-
    % Modecheck the goal, and then store the changes in instantiation
    % of the vars in the delta_instmap in the goal's goal_info.
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    modecheck_goal_expr(GoalExpr0, GoalInfo0, GoalExpr, !ModeInfo),
    compute_goal_instmap_delta(InstMap0, GoalExpr, GoalInfo0, GoalInfo,
        !ModeInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

modecheck_goal_expr(GoalExpr0, GoalInfo0, GoalExpr, !ModeInfo) :-
    % XXX The predicates we call here should have their definitions
    % in the same order as this switch.
    (
        GoalExpr0 = unify(LHS0, RHS0, _UniMode, Unification0, UnifyContext),
        modecheck_goal_unify(LHS0, RHS0, Unification0, UnifyContext, GoalInfo0,
            GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = plain_call(PredId, ProcId0, Args0, _Builtin,
            MaybeCallUnifyContext, PredName),
        modecheck_goal_plain_call(PredId, ProcId0, Args0,
            MaybeCallUnifyContext, PredName, GoalInfo0, GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = generic_call(GenericCall, Args0, Modes0, _MaybeArgRegs,
            _Detism),
        modecheck_goal_generic_call(GenericCall, Args0, Modes0, GoalInfo0,
            GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = call_foreign_proc(Attributes, PredId, ProcId0,
            Args0, ExtraArgs, MaybeTraceRuntimeCond, PragmaCode),
        modecheck_goal_call_foreign_proc(Attributes, PredId, ProcId0,
            Args0, ExtraArgs, MaybeTraceRuntimeCond, PragmaCode,
            GoalInfo0, GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = conj(ConjType, Goals),
        modecheck_goal_conj(ConjType, Goals, GoalInfo0, GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = disj(Goals),
        modecheck_goal_disj(Goals, GoalInfo0, GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        modecheck_goal_switch(Var, CanFail, Cases0, GoalInfo0, GoalExpr,
            !ModeInfo)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        modecheck_goal_if_then_else(Vars, Cond0, Then0, Else0, GoalInfo0,
            GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        modecheck_goal_negation(SubGoal0, GoalInfo0, GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        modecheck_goal_scope(Reason, SubGoal0, GoalInfo0, GoalExpr, !ModeInfo)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        modecheck_goal_shorthand(ShortHand0, GoalInfo0, GoalExpr, !ModeInfo)
    ).

%-----------------------------------------------------------------------------%
%
% Modecheck conjunctions. Most of the work is done by modecheck_conj.m.
%

:- pred modecheck_goal_conj(conj_type::in, list(hlds_goal)::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_conj(ConjType, Goals0, GoalInfo0, GoalExpr, !ModeInfo) :-
    (
        ConjType = plain_conj,
        mode_checkpoint(enter, "conj", !ModeInfo),
        (
            Goals0 = [],
            % Optimize the common case for efficiency.
            GoalExpr = conj(plain_conj, [])
        ;
            Goals0 = [_ | _],
            modecheck_conj_list(ConjType, Goals0, Goals, !ModeInfo),
            conj_list_to_goal(Goals, GoalInfo0, hlds_goal(GoalExpr, _GoalInfo))
        ),
        mode_checkpoint(exit, "conj", !ModeInfo)
    ;
        ConjType = parallel_conj,
        mode_checkpoint(enter, "par_conj", !ModeInfo),
        % Empty parallel conjunction should not be a common case.
        modecheck_conj_list(ConjType, Goals0, Goals, !ModeInfo),
        par_conj_list_to_goal(Goals, GoalInfo0, Goal),
        Goal = hlds_goal(GoalExpr, _GoalInfo),
        mode_checkpoint(exit, "par_conj", !ModeInfo)
    ).

%-----------------------------------------------------------------------------%
%
% Modecheck disjunctions.
%

:- pred modecheck_goal_disj(list(hlds_goal)::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out) is det.

modecheck_goal_disj(Disjuncts0, GoalInfo0, GoalExpr, !ModeInfo) :-
    mode_checkpoint(enter, "disj", !ModeInfo),
    (
        Disjuncts0 = [],    % for efficiency, optimize common case
        GoalExpr = disj(Disjuncts0),
        instmap.init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        % If you modify this code, you may also need to modify
        % modecheck_clause_disj or the code that calls it.
        Disjuncts0 = [_ | _],
        mode_info_get_pred_var_multimode_error_map(!.ModeInfo,
            MultiModeErrorMap0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        LargeFlatConstructs0 = NonLocals,
        mode_info_get_instmap(!.ModeInfo, InstMap0),
        modecheck_disjuncts(MultiModeErrorMap0, InstMap0,
            Disjuncts0, Disjuncts1, ArmInstMaps,
            LargeFlatConstructs0, LargeFlatConstructs, !ModeInfo),
        mode_info_set_instmap(InstMap0, !ModeInfo),
        merge_disj_branches(NonLocals, LargeFlatConstructs,
            Disjuncts1, Disjuncts2, ArmInstMaps, !ModeInfo),
        % Since merge_disj_branches depends on each disjunct in Disjuncts2
        % having a corresponding instmap in InstMaps, we can flatten disjuncts
        % only *after* merge_disj_branches has done its job.
        flatten_disj(Disjuncts2, Disjuncts),
        disj_list_to_goal(Disjuncts, GoalInfo0, hlds_goal(GoalExpr, _GoalInfo))
    ),
    mode_checkpoint(exit, "disj", !ModeInfo).

:- pred modecheck_disjuncts(pred_var_multimode_error_map::in, instmap::in,
    list(hlds_goal)::in, list(hlds_goal)::out, list(arm_instmap)::out,
    set_of_progvar::in, set_of_progvar::out,
    mode_info::in, mode_info::out) is det.

modecheck_disjuncts(_, _, [], [], [], !LargeFlatConstructs, !ModeInfo).
modecheck_disjuncts(MultiModeErrorMap0, InstMap0,
        [Disjunct0 | Disjuncts0], [Disjunct | Disjuncts],
        [ArmInstMap | ArmInstMaps], !LargeFlatConstructs, !ModeInfo) :-
    mode_info_set_pred_var_multimode_error_map(MultiModeErrorMap0, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_goal(Disjunct0, Disjunct, !ModeInfo),
    accumulate_large_flat_constructs(Disjunct, !LargeFlatConstructs),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    Context = goal_info_get_context(Disjunct ^ hg_info),
    ArmInstMap = arm_instmap(Context, InstMap),
    modecheck_disjuncts(MultiModeErrorMap0, InstMap0, Disjuncts0, Disjuncts,
        ArmInstMaps, !LargeFlatConstructs, !ModeInfo).

:- pred merge_disj_branches(set_of_progvar::in, set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::out, list(arm_instmap)::in,
    mode_info::in, mode_info::out) is det.

merge_disj_branches(NonLocals, LargeFlatConstructs, Disjuncts0, Disjuncts,
        ArmInstMaps0, !ModeInfo) :-
    ( if set_of_var.is_empty(LargeFlatConstructs) then
        Disjuncts = Disjuncts0,
        ArmInstMaps = ArmInstMaps0
    else
        % The instmaps will each map every var in LargeFlatConstructs
        % to a very big inst. This means that instmap_merge will take a long
        % time on those variables and add lots of big insts to the merge_inst
        % table. That in turn will cause the later equiv_type_hlds pass
        % to take a long time processing the merge_inst table. All this
        % expense is for nothing, since the chances that the following code
        % wants to know the precise set of possible bindings of variables
        % constructed in what are effectively fact tables is astronomically
        % small.
        %
        % For the variables in LargeFlatConstructs, we know that their
        % final insts do not cause unreachability, do not have uniqueness,
        % do not have higher order inst info, and any information they contain
        % about specific bindings is something we are better off without.
        % We therefore just map all these variables to ground in the instmaps
        % of all the arms before merging them.

        list.map(
            set_large_flat_constructs_to_ground_in_goal(LargeFlatConstructs),
            Disjuncts0, Disjuncts),
        LargeFlatConstructList =
            set_of_var.to_sorted_list(LargeFlatConstructs),
        list.map(
            arm_instmap_set_vars_same(ground(shared, none_or_default_func),
                LargeFlatConstructList),
            ArmInstMaps0, ArmInstMaps)
    ),
    instmap_merge(NonLocals, ArmInstMaps, merge_disj, !ModeInfo).

:- pred arm_instmap_set_vars_same(mer_inst::in, list(prog_var)::in,
    arm_instmap::in, arm_instmap::out) is det.

arm_instmap_set_vars_same(Inst, Vars, ArmInstMap0, ArmInstMap) :-
    ArmInstMap0 = arm_instmap(Context, InstMap0),
    instmap_set_vars_same(Inst, Vars, InstMap0, InstMap),
    ArmInstMap = arm_instmap(Context, InstMap).

%-----------------------------------------------------------------------------%
%
% Modecheck switches.
%

:- pred modecheck_goal_switch(prog_var::in, can_fail::in, list(case)::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_switch(Var, CanFail, Cases0, GoalInfo0, GoalExpr, !ModeInfo) :-
    mode_checkpoint(enter, "switch", !ModeInfo),
    (
        Cases0 = [],
        Cases = [],
        instmap.init_unreachable(InstMap),
        mode_info_set_instmap(InstMap, !ModeInfo)
    ;
        % If you modify this code, you may also need to modify
        % modecheck_clause_switch or the code that calls it.
        Cases0 = [_ | _],
        mode_info_get_pred_var_multimode_error_map(!.ModeInfo,
            MultiModeErrorMap0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        LargeFlatConstructs0 = NonLocals,
        modecheck_case_list(MultiModeErrorMap0, Var, Cases0, Cases1, InstMaps,
            LargeFlatConstructs0, LargeFlatConstructs, !ModeInfo),
        merge_switch_branches(NonLocals, LargeFlatConstructs,
            Cases1, Cases, InstMaps, !ModeInfo)
    ),
    GoalExpr = switch(Var, CanFail, Cases),
    mode_checkpoint(exit, "switch", !ModeInfo).

:- pred modecheck_case_list(pred_var_multimode_error_map::in, prog_var::in,
    list(case)::in, list(case)::out, list(instmap)::out,
    set_of_progvar::in, set_of_progvar::out,
    mode_info::in, mode_info::out) is det.

modecheck_case_list(_, _, [], [], [], !LargeFlatConstructs, !ModeInfo).
modecheck_case_list(MultiModeErrorMap0, Var, [Case0 | Cases0], [Case | Cases],
        [InstMap | InstMaps], !LargeFlatConstructs, !ModeInfo) :-
    mode_info_set_pred_var_multimode_error_map(MultiModeErrorMap0, !ModeInfo),
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    % Record the fact that Var was bound to ConsId in the instmap
    % before processing this case.
    modecheck_functors_test(Var, MainConsId, OtherConsIds, !ModeInfo),

    % Modecheck this case (if it is reachable).
    mode_info_get_instmap(!.ModeInfo, InstMap1),
    ( if instmap_is_reachable(InstMap1) then
        modecheck_goal(Goal0, Goal1, !ModeInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap)
    else
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        Goal1 = true_goal,
        InstMap = InstMap1
    ),

    % Don't lose the information added by the functor test above.
    fixup_instmap_switch_var(Var, InstMap0, InstMap, Goal1, Goal),

    Case = case(MainConsId, OtherConsIds, Goal),
    accumulate_large_flat_constructs(Goal, !LargeFlatConstructs),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_case_list(MultiModeErrorMap0, Var, Cases0, Cases, InstMaps,
        !LargeFlatConstructs, !ModeInfo).

:- pred merge_switch_branches(set_of_progvar::in, set_of_progvar::in,
    list(case)::in, list(case)::out, list(instmap)::in,
    mode_info::in, mode_info::out) is det.

merge_switch_branches(NonLocals, LargeFlatConstructs, Cases0, Cases,
        InstMaps0, !ModeInfo) :-
    ( if set_of_var.is_empty(LargeFlatConstructs) then
        Cases = Cases0,
        InstMaps = InstMaps0
    else
        % The same considerations apply here as in merge_disj_branches.
        list.map(
            set_large_flat_constructs_to_ground_in_case(LargeFlatConstructs),
            Cases0, Cases),
        LargeFlatConstructList =
            set_of_var.to_sorted_list(LargeFlatConstructs),
        list.map(
            instmap_set_vars_same(ground(shared, none_or_default_func),
                LargeFlatConstructList),
            InstMaps0, InstMaps)
    ),
    make_arm_instmaps_for_cases(Cases, InstMaps, ArmInstMaps),
    instmap_merge(NonLocals, ArmInstMaps, merge_disj, !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Utility predicates used to help optimize the modechecking of disjunctions and
% switches.
%

:- pred accumulate_large_flat_constructs(hlds_goal::in,
    set_of_progvar::in, set_of_progvar::out) is det.

accumulate_large_flat_constructs(Goal, !LargeFlatConstructs) :-
    ( if set_of_var.is_empty(!.LargeFlatConstructs) then
        % Calling goal_large_flat_constructs and then set.intersect
        % would be waste of time; !:LargeFlatConstructs will still be empty.
        true
    else
        GoalLargeFlatConstructs = goal_large_flat_constructs(Goal),
        set_of_var.intersect(GoalLargeFlatConstructs, !LargeFlatConstructs)
    ).

:- func goal_large_flat_constructs(hlds_goal) = set_of_progvar.

goal_large_flat_constructs(Goal) = LargeFlatConstructs :-
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = unify(_, _, _, _, _),
        % Unifications not wrapped in from_ground_term_construct scopes
        % are never marked by the modechecker as being constructed statically.
        LargeFlatConstructs = set_of_var.init
    ;
        ( GoalExpr = plain_call(_, _, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        LargeFlatConstructs = set_of_var.init
    ;
        ( GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = shorthand(_)
        ; GoalExpr = conj(parallel_conj, _)
        ),
        LargeFlatConstructs = set_of_var.init
    ;
        GoalExpr = scope(Reason, _),
        (
            Reason = from_ground_term(TermVar, from_ground_term_construct),
            LargeFlatConstructs = set_of_var.make_singleton(TermVar)
        ;
            ( Reason = from_ground_term(_, from_ground_term_initial)
            ; Reason = from_ground_term(_, from_ground_term_deconstruct)
            ; Reason = from_ground_term(_, from_ground_term_other)
            ; Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            LargeFlatConstructs = set_of_var.init
        )
    ;
        GoalExpr = conj(plain_conj, Conjuncts),
        goals_large_flat_constructs(Conjuncts,
            set_of_var.init, LargeFlatConstructs)
    ).

:- pred goals_large_flat_constructs(list(hlds_goal)::in,
    set_of_progvar::in, set_of_progvar::out) is det.

goals_large_flat_constructs([], !LargeFlatConstructs).
goals_large_flat_constructs([Goal | Goals], !LargeFlatConstructs) :-
    GoalLargeFlatConstructs = goal_large_flat_constructs(Goal),
    set_of_var.union(GoalLargeFlatConstructs, !LargeFlatConstructs),
    goals_large_flat_constructs(Goals, !LargeFlatConstructs).

:- pred set_large_flat_constructs_to_ground_in_goal(set_of_progvar::in,
    hlds_goal::in, hlds_goal::out) is det.

set_large_flat_constructs_to_ground_in_goal(LargeFlatConstructs,
        Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = Goal0
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ),
        Goal = Goal0
    ;
        ( GoalExpr0 = disj(_)
        ; GoalExpr0 = switch(_, _, _)
        ; GoalExpr0 = if_then_else(_, _, _, _)
        ; GoalExpr0 = negation(_)
        ; GoalExpr0 = shorthand(_)
        ; GoalExpr0 = conj(parallel_conj, _)
        ),
        Goal = Goal0
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        (
            Reason = from_ground_term(TermVar, from_ground_term_construct),
            ( if set_of_var.member(LargeFlatConstructs, TermVar) then
                InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
                instmap_delta_set_var(TermVar,
                    ground(shared, none_or_default_func),
                    InstMapDelta0, InstMapDelta),
                goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),

                SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo0),
                goal_info_set_instmap_delta(InstMapDelta,
                    SubGoalInfo0, SubGoalInfo),
                % We could also replace the instmap deltas of the conjuncts
                % inside SubGoalExpr0. Doing so would take time but reduce
                % the compiler's memory requirements.
                SubGoal = hlds_goal(SubGoalExpr0, SubGoalInfo),
                GoalExpr = scope(Reason, SubGoal),
                Goal = hlds_goal(GoalExpr, GoalInfo)
            else
                Goal = Goal0
            )
        ;
            ( Reason = from_ground_term(_, from_ground_term_initial)
            ; Reason = from_ground_term(_, from_ground_term_deconstruct)
            ; Reason = from_ground_term(_, from_ground_term_other)
            ; Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = promise_purity(_)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            ; Reason = trace_goal(_, _, _, _, _)
            ; Reason = loop_control(_, _, _)
            ),
            Goal = Goal0
        )
    ;
        GoalExpr0 = conj(plain_conj, Conjuncts0),
        set_large_flat_constructs_to_ground_in_goals(LargeFlatConstructs,
            Conjuncts0, Conjuncts),
        GoalExpr = conj(plain_conj, Conjuncts),

        InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
        instmap_delta_changed_vars(InstMapDelta0, ChangedVars),
        set_of_var.intersect(ChangedVars, LargeFlatConstructs, GroundVars),
        instmap_delta_set_vars_same(ground(shared, none_or_default_func),
            set_of_var.to_sorted_list(GroundVars),
            InstMapDelta0, InstMapDelta),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred set_large_flat_constructs_to_ground_in_goals(set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::out) is det.

set_large_flat_constructs_to_ground_in_goals(_, [], []).
set_large_flat_constructs_to_ground_in_goals(LargeFlatConstructs,
        [Goal0 | Goals0], [Goal | Goals]) :-
    set_large_flat_constructs_to_ground_in_goal(LargeFlatConstructs,
        Goal0, Goal),
    set_large_flat_constructs_to_ground_in_goals(LargeFlatConstructs,
        Goals0, Goals).

:- pred set_large_flat_constructs_to_ground_in_case(set_of_progvar::in,
    case::in, case::out) is det.

set_large_flat_constructs_to_ground_in_case(LargeFlatConstructs,
        Case0, Case) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    set_large_flat_constructs_to_ground_in_goal(LargeFlatConstructs,
        Goal0, Goal),
    Case = case(MainConsId, OtherConsIds, Goal).

%-----------------------------------------------------------------------------%
%
% Modecheck if-then-elses.
%

:- pred modecheck_goal_if_then_else(list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_if_then_else(Vars, Cond0, Then0, Else0, GoalInfo0, GoalExpr,
        !ModeInfo) :-
    mode_checkpoint(enter, "if-then-else", !ModeInfo),
    mode_info_get_pred_var_multimode_error_map(!.ModeInfo, MultiModeErrorMap0),
    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    ThenVars = goal_get_nonlocals(Then0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    % We need to lock the non-local variables, to ensure that the condition
    % of the if-then-else does not bind them.

    mode_info_lock_vars(var_lock_if_then_else, NonLocals, !ModeInfo),
    mode_info_add_live_vars(ThenVars, !ModeInfo),
    modecheck_goal(Cond0, Cond, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMapCond),
    mode_info_remove_live_vars(ThenVars, !ModeInfo),
    mode_info_unlock_vars(var_lock_if_then_else, NonLocals, !ModeInfo),
    ( if instmap_is_reachable(InstMapCond) then
        modecheck_goal(Then0, Then, !ModeInfo),
        mode_info_get_instmap(!.ModeInfo, InstMapThen)
    else
        % We should not mode-analyse the goal, since it is unreachable.
        % Instead we optimize the goal away, so that later passes
        % won't complain about it not having mode information.
        Then = true_goal,
        InstMapThen = InstMapCond
    ),
    mode_info_set_pred_var_multimode_error_map(MultiModeErrorMap0, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_goal(Else0, Else, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMapElse),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    make_arm_instmaps_for_goals([Then, Else], [InstMapThen, InstMapElse],
        ThenElseArgInfos),
    instmap_merge(NonLocals, ThenElseArgInfos, merge_if_then_else, !ModeInfo),
    GoalExpr = if_then_else(Vars, Cond, Then, Else),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    mode_info_get_in_promise_purity_scope(!.ModeInfo, InPromisePurityScope),
    (
        InPromisePurityScope = not_in_promise_purity_scope,
        CondNonLocals0 = goal_get_nonlocals(Cond),
        CondNonLocals =
            set_of_var.to_sorted_list(
                set_of_var.intersect(CondNonLocals0, NonLocals)),
        check_no_inst_any_vars(if_then_else, CondNonLocals,
            InstMap0, InstMap, !ModeInfo)
    ;
        InPromisePurityScope = in_promise_purity_scope
    ),
    mode_checkpoint(exit, "if-then-else", !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Modecheck negations.
%

:- pred modecheck_goal_negation(hlds_goal::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out) is det.

modecheck_goal_negation(SubGoal0, GoalInfo0, GoalExpr, !ModeInfo) :-
    mode_checkpoint(enter, "not", !ModeInfo),
    mode_info_get_pred_var_multimode_error_map(!.ModeInfo, MultiModeErrorMap0),
    NonLocals = goal_info_get_nonlocals(GoalInfo0),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    % When analyzing a negated goal, nothing is forward-live (live on forward
    % execution after that goal), because if the goal succeeds then execution
    % will immediately backtrack. So we need to set the live variables set
    % to empty here. This allows those variables to be backtrackably
    % destructively updated. (If you try to do non-backtrackable destructive
    % update on such a variable, it will be caught later on by unique_modes.m.)
    mode_info_get_live_vars(!.ModeInfo, LiveVars0),
    mode_info_set_live_vars(bag.init, !ModeInfo),

    % We need to lock the non-local variables, to ensure that
    % the negation does not bind them.
    mode_info_lock_vars(var_lock_negation, NonLocals, !ModeInfo),
    modecheck_goal(SubGoal0, SubGoal, !ModeInfo),
    mode_info_set_live_vars(LiveVars0, !ModeInfo),
    mode_info_unlock_vars(var_lock_negation, NonLocals, !ModeInfo),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    mode_info_get_in_promise_purity_scope(!.ModeInfo, InPromisePurityScope),
    (
        InPromisePurityScope = not_in_promise_purity_scope,
        NegNonLocals = goal_info_get_nonlocals(GoalInfo0),
        instmap.init_unreachable(Unreachable),
        check_no_inst_any_vars(negation,
            set_of_var.to_sorted_list(NegNonLocals),
            InstMap0, Unreachable, !ModeInfo)
    ;
        InPromisePurityScope = in_promise_purity_scope
    ),
    GoalExpr = negation(SubGoal),
    mode_info_set_pred_var_multimode_error_map(MultiModeErrorMap0, !ModeInfo),
    mode_checkpoint(exit, "not", !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Modecheck scope goals.
%

:- pred modecheck_goal_scope(scope_reason::in, hlds_goal::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_scope(Reason, SubGoal0, GoalInfo0, GoalExpr, !ModeInfo) :-
    (
        Reason = trace_goal(_, _, _, _, _),
        mode_checkpoint(enter, "trace scope", !ModeInfo),
        mode_info_get_instmap(!.ModeInfo, InstMap0),
        NonLocals = goal_info_get_nonlocals(GoalInfo0),
        % We need to lock the non-local variables, to ensure that
        % the trace goal does not bind them. If it did, then the code
        % would not be valid with the trace goal disabled.
        mode_info_lock_vars(var_lock_trace_goal, NonLocals, !ModeInfo),
        modecheck_goal(SubGoal0, SubGoal, !ModeInfo),
        mode_info_unlock_vars(var_lock_trace_goal, NonLocals, !ModeInfo),
        mode_info_set_instmap(InstMap0, !ModeInfo),
        GoalExpr = scope(Reason, SubGoal),
        mode_checkpoint(exit, "trace scope", !ModeInfo)
    ;
        (
            ( Reason = disable_warnings(_, _)
            ; Reason = exist_quant(_)
            ; Reason = promise_solutions(_, _)
            ; Reason = require_detism(_)
            ; Reason = require_complete_switch(_)
            ; Reason = require_switch_arms_detism(_, _)
            ; Reason = commit(_)
            ; Reason = barrier(_)
            )
        ;
            Reason = loop_control(LCVar, LCSVar, _),
            % We check that the variables for the loop control are ground.
            mode_info_get_instmap(!.ModeInfo, InstMap),
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            expect(var_is_ground_in_instmap(ModuleInfo, InstMap, LCVar),
                $pred, "Loop control variable is not ground"),
            expect(var_is_ground_in_instmap(ModuleInfo, InstMap, LCSVar),
                $pred, "Loop control slot variable is not ground")
        ),
        mode_checkpoint(enter, "scope", !ModeInfo),
        modecheck_goal(SubGoal0, SubGoal, !ModeInfo),
        GoalExpr = scope(Reason, SubGoal),
        mode_checkpoint(exit, "scope", !ModeInfo)
    ;
        Reason = from_ground_term(TermVar, OldKind),
        (
            OldKind = from_ground_term_construct,
            mode_info_var_is_live(!.ModeInfo, TermVar, IsLive),
            (
                IsLive = is_live,
                % We have already modechecked the subgoal. If we had done
                % anything to it that could invalidate its invariants,
                % the part of the compiler that did this should have also
                % updated the scope reason.
                GoalExpr = scope(Reason, SubGoal0),

                InstMapDelta0 = goal_info_get_instmap_delta(GoalInfo0),
                instmap_delta_lookup_var(InstMapDelta0, TermVar, TermVarInst),
                mode_info_get_instmap(!.ModeInfo, InstMap0),
                instmap_set_var(TermVar, TermVarInst, InstMap0, InstMap),
                mode_info_set_instmap(InstMap, !ModeInfo)
            ;
                IsLive = is_dead,
                % We delete construction unifications that construct dead
                % variables; do the same with construct scopes.
                GoalExpr = conj(plain_conj, [])
            )
        ;
            ( OldKind = from_ground_term_initial
            ; OldKind = from_ground_term_deconstruct
            ; OldKind = from_ground_term_other
            ),
            mode_checkpoint(enter, "from_ground_term scope", !ModeInfo),
            modecheck_goal_from_ground_term_scope(TermVar, SubGoal0, GoalInfo0,
                MaybeKind1AndSubGoal1, !ModeInfo),
            mode_checkpoint(exit, "from_ground_term scope", !ModeInfo),
            (
                MaybeKind1AndSubGoal1 = yes(Kind1 - SubGoal1),
                expect(negate(unify(Kind1, from_ground_term_initial)), $pred,
                    "from_ground_term_initial"),
                mode_info_set_had_from_ground_term(had_from_ground_term_scope,
                    !ModeInfo),

                mode_info_get_make_ground_terms_unique(!.ModeInfo,
                    MakeGroundTermsUnique),
                (
                    MakeGroundTermsUnique = do_not_make_ground_terms_unique,
                    UpdatedReason1 = from_ground_term(TermVar, Kind1),
                    GoalExpr = scope(UpdatedReason1, SubGoal1)
                ;
                    MakeGroundTermsUnique = make_ground_terms_unique,
                    (
                        Kind1 = from_ground_term_initial,
                        unexpected($pred, "from_ground_term_initial")
                    ;
                        Kind1 = from_ground_term_construct,
                        modecheck_goal_make_ground_term_unique(TermVar,
                            SubGoal1, GoalInfo0, GoalExpr, !ModeInfo)
                    ;
                        ( Kind1 = from_ground_term_deconstruct
                        ; Kind1 = from_ground_term_other
                        ),
                        UpdatedReason1 = from_ground_term(TermVar, Kind1),
                        GoalExpr = scope(UpdatedReason1, SubGoal1)
                    )
                )
            ;
                MaybeKind1AndSubGoal1 = no,
                GoalExpr = conj(plain_conj, [])
            )
        )
    ;
        Reason = promise_purity(_Purity),
        mode_info_get_in_promise_purity_scope(!.ModeInfo, InPPScope),
        mode_info_set_in_promise_purity_scope(in_promise_purity_scope,
            !ModeInfo),
        mode_checkpoint(enter, "promise_purity scope", !ModeInfo),
        modecheck_goal(SubGoal0, SubGoal, !ModeInfo),
        GoalExpr = scope(Reason, SubGoal),
        mode_checkpoint(exit, "promise_purity scope", !ModeInfo),
        mode_info_set_in_promise_purity_scope(InPPScope, !ModeInfo)
    ).

    % This predicate transforms
    %
    %   scope(TermVar,
    %       conj(plain_conj,
    %           X1 = ...
    %           X2 = ...
    %           ...
    %           TermVar = ...
    %       )
    %   )
    %
    % into
    %
    %   conj(plain_conj,
    %       scope(CloneVar,
    %           conj(plain_conj,
    %               X1 = ...
    %               X2 = ...
    %               ...
    %               CloneVar = ...
    %           )
    %       ),
    %       builtin.copy(CloneVar, TermVar)
    %   )
    %
    % We could transform it instead into a plain conjunction that directly
    % builds a unique term, but that could have a significant detrimental
    % effect on compile time.
    %
    % The performance of the generated code is unlikely to be of too much
    % importance, since we expect programs will rarely need a unique copy
    % of a ground term.
    %
:- pred modecheck_goal_make_ground_term_unique(prog_var::in,
    hlds_goal::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_make_ground_term_unique(TermVar, SubGoal0, GoalInfo0, GoalExpr,
        !ModeInfo) :-
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    mode_info_get_varset(!.ModeInfo, VarSet0),
    varset.new_var(CloneVar, VarSet0, VarSet),
    lookup_var_type(VarTypes0, TermVar, TermVarType),
    add_var_type(CloneVar, TermVarType, VarTypes0, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo),
    Rename = map.singleton(TermVar, CloneVar),
    % By construction, TermVar can appear only in (a) SubGoal0's goal_info,
    % and (b) in the last conjunct in SubGoal0's goal_expr; it cannot appear
    % in any of the other conjuncts. We could make this code more efficient
    % by exploiting this fact, but there is not yet any evidence of any need
    % for this.
    rename_some_vars_in_goal(Rename, SubGoal0, SubGoal),
    rename_vars_in_goal_info(need_not_rename, Rename, GoalInfo0,
        ScopeGoalInfo1),

    % We must put the instmaps into the goal_infos of all the subgoals of the
    % final GoalExpr we return, since modecheck_goal will not get a chance to
    % do so.
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, TermVar, TermVarOldInst),
    ScopeInstMapDelta =
        instmap_delta_from_assoc_list([CloneVar - TermVarOldInst]),
    goal_info_set_instmap_delta(ScopeInstMapDelta,
        ScopeGoalInfo1, ScopeGoalInfo),

    Reason = from_ground_term(CloneVar, from_ground_term_construct),
    ScopeGoalExpr = scope(Reason, SubGoal),
    ScopeGoal = hlds_goal(ScopeGoalExpr, ScopeGoalInfo),

    % We could get a more accurate new inst for TermVar by replacing
    % all the "shared" functors in TermVarOldInst with "unique".
    % However, this should be good enough. XXX wangp, is this right?
    TermVarUniqueInst = ground(unique, none_or_default_func),

    instmap_set_var(CloneVar, TermVarOldInst, InstMap0, InstMap1),
    mode_info_set_instmap(InstMap1, !ModeInfo),

    Context = goal_info_get_context(GoalInfo0),
    modecheck_make_type_info_var_for_type(TermVarType, Context, TypeInfoVar,
        TypeInfoGoals, !ModeInfo),

    InstMapDelta =
        instmap_delta_from_assoc_list([TermVar - TermVarUniqueInst]),
    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    generate_simple_call(ModuleInfo, mercury_public_builtin_module, "copy",
        pf_predicate, mode_no(1), detism_det, purity_pure,
        [TypeInfoVar, CloneVar, TermVar], [], InstMapDelta, Context, CopyGoal),
    mode_info_get_instmap(!.ModeInfo, InstMap2),
    instmap_set_var(TermVar, TermVarUniqueInst, InstMap2, InstMap),
    mode_info_set_instmap(InstMap, !ModeInfo),

    GoalExpr = conj(plain_conj, [ScopeGoal | TypeInfoGoals] ++ [CopyGoal]).

:- pred modecheck_make_type_info_var_for_type(mer_type::in, prog_context::in,
    prog_var::out, list(hlds_goal)::out, mode_info::in, mode_info::out) is det.

modecheck_make_type_info_var_for_type(Type, Context, TypeInfoVar,
        TypeInfoGoals, !ModeInfo) :-
    mode_info_get_module_info(!.ModeInfo, ModuleInfo0),

    % Get the relevant information for the current procedure.
    mode_info_get_pred_id(!.ModeInfo, PredId),
    mode_info_get_proc_id(!.ModeInfo, ProcId),
    module_info_pred_proc_info(ModuleInfo0, PredId, ProcId, PredInfo0,
        ProcInfo0),

    % Create a poly_info for the current procedure. We have to set the varset
    % and vartypes from the mode_info, not the proc_info, because new vars may
    % have been introduced during mode analysis, e.g. when adding
    % unifications to handle implied modes.
    mode_info_get_var_types(!.ModeInfo, VarTypes0),
    mode_info_get_varset(!.ModeInfo, VarSet0),
    proc_info_set_varset(VarSet0, ProcInfo0, ProcInfo1),
    proc_info_set_vartypes(VarTypes0, ProcInfo1, ProcInfo2),

    polymorphism_make_type_info_var_raw(Type, Context,
        TypeInfoVar, TypeInfoGoals, ModuleInfo0, ModuleInfo1,
        PredInfo0, PredInfo, ProcInfo2, ProcInfo),
    module_info_set_pred_proc_info(PredId, ProcId, PredInfo, ProcInfo,
        ModuleInfo1, ModuleInfo),

    % Update the information in the mode_info.
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_vartypes(ProcInfo, VarTypes),
    mode_info_set_varset(VarSet, !ModeInfo),
    mode_info_set_var_types(VarTypes, !ModeInfo),
    mode_info_set_module_info(ModuleInfo, !ModeInfo).

:- pred modecheck_goal_from_ground_term_scope(prog_var::in,
    hlds_goal::in, hlds_goal_info::in,
    maybe(pair(from_ground_term_kind, hlds_goal))::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_from_ground_term_scope(TermVar, SubGoal0, GoalInfo0,
        MaybeKindAndSubGoal, !ModeInfo) :-
    % The original goal does no quantification, so deleting the `scope'
    % would be OK. However, deleting it during mode analysis would mean
    % we don't have it during unique mode analysis and other later compiler
    % passes.
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_lookup_var(InstMap0, TermVar, TermVarInst),
    mode_info_get_varset(!.ModeInfo, VarSet),
    modecheck_specializable_ground_term(SubGoal0, TermVar, TermVarInst,
        MaybeGroundTermMode),
    (
        MaybeGroundTermMode = yes(construct_ground_term(RevConj0)),
        mode_info_var_is_live(!.ModeInfo, TermVar, LiveTermVar),
        (
            LiveTermVar = is_live,
            SubGoal0 = hlds_goal(_, SubGoalInfo0),
            modecheck_ground_term_construct(TermVar, RevConj0,
                SubGoalInfo0, VarSet, SubGoal, !ModeInfo),
            Kind = from_ground_term_construct,
            MaybeKindAndSubGoal = yes(Kind - SubGoal)
        ;
            LiveTermVar = is_dead,
            % The term constructed by the scope is not used anywhere.
            MaybeKindAndSubGoal = no
        )
    ;
        (
            MaybeGroundTermMode = yes(deconstruct_ground_term(_)),
            % We should specialize the handling of these scopes as well as
            % scopes that construct ground terms, but we don't yet have
            % a compelling motivating example.
            SubGoal1 = SubGoal0,
            Kind = from_ground_term_deconstruct
        ;
            MaybeGroundTermMode = no,
            ( if
                TermVarInst = free,
                SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo0),
                SubGoalExpr0 = conj(plain_conj, SubGoalConjuncts0)
            then
                % We reverse the list here for the same reason
                % modecheck_specializable_ground_term does in the
                % corresponding case.
                list.reverse(SubGoalConjuncts0, SubGoalConjuncts1),
                SubGoalExpr1 = conj(plain_conj, SubGoalConjuncts1),
                SubGoal1 = hlds_goal(SubGoalExpr1, SubGoalInfo0)
            else
                SubGoal1 = SubGoal0
            ),
            Kind = from_ground_term_other
        ),
        ( if goal_info_has_feature(GoalInfo0, feature_from_head) then
            attach_features_to_all_goals([feature_from_head],
                attach_in_from_ground_term, SubGoal1, SubGoal2)
        else
            SubGoal2 = SubGoal1
        ),
        mode_checkpoint(enter, "scope", !ModeInfo),
        modecheck_goal(SubGoal2, SubGoal, !ModeInfo),
        mode_checkpoint(exit, "scope", !ModeInfo),
        MaybeKindAndSubGoal = yes(Kind - SubGoal)
    ).

:- type ground_term_mode
    --->    construct_ground_term(list(hlds_goal))
    ;       deconstruct_ground_term(list(hlds_goal)).

:- pred modecheck_specializable_ground_term(hlds_goal::in, prog_var::in,
    mer_inst::in, maybe(ground_term_mode)::out) is det.

modecheck_specializable_ground_term(SubGoal, TermVar, TermVarInst,
        MaybeGroundTermMode) :-
    SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo),
    ( if
        NonLocals = goal_info_get_nonlocals(SubGoalInfo),
        set_of_var.is_singleton(NonLocals, TermVar),
        goal_info_get_purity(SubGoalInfo) = purity_pure,
        SubGoalExpr = conj(plain_conj, [UnifyTermGoal | UnifyArgGoals]),
        % If TermVar is created by an impure unification, which is
        % possible for solver types, it is possible for UnifyTermGoal
        % to contain a unification other than one involving TermVar.
        UnifyTermGoal ^ hg_expr = unify(TermVar, _, _, _, _),
        all_plain_construct_unifies([UnifyTermGoal | UnifyArgGoals])
    then
        ( if TermVarInst = free then
            % UnifyTerGoalm unifies TermVar with the arguments created
            % by UnifyArgGoals. Since TermVar is now free and the
            % argument variables haven't been encountered yet,
            % UnifyTermGoal cannot succeed until *after* the argument
            % variables become ground.
            %
            % Putting UnifyTerGoalm after UnifyArgGoals here is MUCH faster
            % than letting the usual more ordering algorithm delay it
            % repeatedly: it is linear instead of quadratic.

            list.reverse([UnifyTermGoal | UnifyArgGoals], RevConj),
            MaybeGroundTermMode = yes(construct_ground_term(RevConj))
        else if TermVarInst = ground(shared, none_or_default_func) then
            Conj = [UnifyTermGoal | UnifyArgGoals],
            MaybeGroundTermMode = yes(deconstruct_ground_term(Conj))
        else
            MaybeGroundTermMode = no
        )
    else
        MaybeGroundTermMode = no
    ).

:- pred all_plain_construct_unifies(list(hlds_goal)::in) is semidet.

all_plain_construct_unifies([]).
all_plain_construct_unifies([Goal | Goals]) :-
    Goal = hlds_goal(GoalExpr, _),
    GoalExpr = unify(_LHSVar, RHS, _, _, _),
    RHS = rhs_functor(_ConsId, is_not_exist_constr, _RHSVars),
    all_plain_construct_unifies(Goals).

:- pred modecheck_ground_term_construct(prog_var::in, list(hlds_goal)::in,
    hlds_goal_info::in, prog_varset::in, hlds_goal::out,
    mode_info::in, mode_info::out) is det.

modecheck_ground_term_construct(TermVar, ConjGoals0, !.SubGoalInfo, VarSet,
        SubGoal, !ModeInfo) :-
    map.init(LocalVarMap0),
    modecheck_ground_term_construct_goal_loop(VarSet, ConjGoals0, ConjGoals,
        LocalVarMap0, LocalVarMap),
    map.lookup(LocalVarMap, TermVar, TermVarInfo),
    TermVarInfo = construct_var_info(TermVarInst),
    InstMapDelta = instmap_delta_from_assoc_list([TermVar - TermVarInst]),
    goal_info_set_instmap_delta(InstMapDelta, !SubGoalInfo),
    % We present the determinism, so that the determinism analysis pass
    % does not have to traverse the goals inside the scope.
    goal_info_set_determinism(detism_det, !SubGoalInfo),
    SubGoalExpr = conj(plain_conj, ConjGoals),
    SubGoal = hlds_goal(SubGoalExpr, !.SubGoalInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap0),
    instmap_set_var(TermVar, TermVarInst, InstMap0, InstMap),
    mode_info_set_instmap(InstMap, !ModeInfo).

:- type construct_var_info
    --->    construct_var_info(mer_inst).

:- type construct_var_info_map == map(prog_var, construct_var_info).

:- pred modecheck_ground_term_construct_goal_loop(prog_varset::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    construct_var_info_map::in, construct_var_info_map::out) is det.

modecheck_ground_term_construct_goal_loop(_, [], [], !LocalVarMap).
modecheck_ground_term_construct_goal_loop(VarSet,
        [Goal0 | Goals0], [Goal | Goals], !LocalVarMap) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = unify(LHSVar, RHS, _, _, UnifyContext),
        RHS = rhs_functor(ConsId, is_not_exist_constr, RHSVars)
    then
        % We could set TermInst to simply to ground, as opposed to the inst
        % we now use which gives information about LHSVar's shape. This would
        % remove the need for the inst information in !LocalVarMap, and
        % would make HLDS dumps linear in the size of the term instead of
        % quadratic. However, due to structure sharing, the actual memory
        % requirements of these bound insts are only linear in the size of the
        % term.
        modecheck_ground_term_construct_arg_loop(RHSVars, ArgInsts, ArgModes,
            !LocalVarMap),
        BoundInst = bound_functor(ConsId, ArgInsts),
        TermInst = bound(shared, inst_test_results_fgtc, [BoundInst]),
        UnifyMode = unify_modes_li_lf_ri_rf(free, TermInst,
            TermInst, TermInst),
        ConstructHow = construct_statically(born_static),
        Uniqueness = cell_is_shared,
        Unification = construct(LHSVar, ConsId, RHSVars, ArgModes,
            ConstructHow, Uniqueness, no_construct_sub_info),
        GoalExpr = unify(LHSVar, RHS, UnifyMode, Unification, UnifyContext),
        InstMapDelta = instmap_delta_from_assoc_list([LHSVar - TermInst]),
        goal_info_set_instmap_delta(InstMapDelta, GoalInfo0, GoalInfo1),
        % We preset the determinism, so that the determinism analysis pass
        % does not have to traverse the goals inside the scope.
        goal_info_set_determinism(detism_det, GoalInfo1, GoalInfo),
        Goal = hlds_goal(GoalExpr, GoalInfo),

        LHSVarInfo = construct_var_info(TermInst),
        map.det_insert(LHSVar, LHSVarInfo, !LocalVarMap)
    else
        unexpected($pred, "not rhs_functor unify")
    ),
    modecheck_ground_term_construct_goal_loop(VarSet, Goals0, Goals,
        !LocalVarMap).

:- pred modecheck_ground_term_construct_arg_loop(list(prog_var)::in,
    list(mer_inst)::out, list(unify_mode)::out,
    construct_var_info_map::in, construct_var_info_map::out) is det.

modecheck_ground_term_construct_arg_loop([], [], [], !LocalVarMap).
modecheck_ground_term_construct_arg_loop([Var | Vars], [VarInst | VarInsts],
        [ArgMode | ArgModes], !LocalVarMap) :-
    % Each variable introduced by the superhomogeneous transformation
    % for a ground term appears in the from_ground_term scope exactly twice.
    % Once when it is produced (which is handled in the goal loop predicate),
    % and once when it is consumed, which is handled here.
    %
    % Since there will be no more appearances of this variable, we remove it
    % from LocalVarMap. This greatly reduces the size of LocalVarMap.
    map.det_remove(Var, VarInfo, !LocalVarMap),
    VarInfo = construct_var_info(VarInst),
    ArgMode = unify_modes_li_lf_ri_rf(free, VarInst, VarInst, VarInst),
    modecheck_ground_term_construct_arg_loop(Vars, VarInsts, ArgModes,
        !LocalVarMap).

%-----------------------------------------------------------------------------%
%
% Modecheck plain calls. Most of the work is in modecheck_call.m.
%

:- pred modecheck_goal_plain_call(pred_id::in, proc_id::in,
    list(prog_var)::in, maybe(call_unify_context)::in, sym_name::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_plain_call(PredId, ProcId0, Args0, MaybeCallUnifyContext,
        PredName, GoalInfo0, GoalExpr, !ModeInfo) :-
    PredNameString = sym_name_to_string(PredName),
    CallString = "call " ++ PredNameString,
    mode_checkpoint(enter, CallString, !ModeInfo),

    mode_info_set_call_context(call_context_call(mode_call_plain(PredId)),
        !ModeInfo),

    mode_info_get_instmap(!.ModeInfo, InstMap0),
    DeterminismKnown = no,
    modecheck_call_pred(PredId, DeterminismKnown, ProcId0, ProcId,
        Args0, Args, GoalInfo0, ExtraGoals, !ModeInfo),

    mode_info_get_module_info(!.ModeInfo, ModuleInfo),
    mode_info_get_pred_id(!.ModeInfo, CallerPredId),
    Builtin = builtin_state(ModuleInfo, CallerPredId, PredId, ProcId),
    Call = plain_call(PredId, ProcId, Args, Builtin, MaybeCallUnifyContext,
        PredName),
    handle_extra_goals(Call, ExtraGoals, GoalInfo0, Args0, Args,
        InstMap0, GoalExpr, !ModeInfo),

    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, CallString, !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Modecheck generic calls.
%

:- pred modecheck_goal_generic_call(generic_call::in, list(prog_var)::in,
    list(mer_mode)::in, hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_generic_call(GenericCall, Args0, Modes0, GoalInfo0, GoalExpr,
        !ModeInfo) :-
    mode_checkpoint(enter, "generic_call", !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMap0),

    hlds_goal.generic_call_to_id(GenericCall, GenericCallId),
    CallId = mode_call_generic(GenericCallId),
    mode_info_set_call_context(call_context_call(CallId), !ModeInfo),
    (
        GenericCall = higher_order(PredVar, _, PredOrFunc, _),
        modecheck_higher_order_call(PredOrFunc, PredVar,
            Args0, Args, Modes, Det, ExtraGoals, !ModeInfo),
        GoalExpr1 = generic_call(GenericCall, Args, Modes, arg_reg_types_unset,
            Det),
        AllArgs0 = [PredVar | Args0],
        AllArgs = [PredVar | Args],
        handle_extra_goals(GoalExpr1, ExtraGoals, GoalInfo0, AllArgs0, AllArgs,
            InstMap0, GoalExpr, !ModeInfo)
    ;
        % Class method calls are added by polymorphism.m.
        % XXX We should probably fill this in so that
        % rerunning mode analysis works on code with typeclasses.
        GenericCall = class_method(_, _, _, _),
        unexpected($pred, "class_method_call")
    ;
        GenericCall = event_call(EventName),
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        module_info_get_event_set(ModuleInfo, EventSet),
        EventSpecMap = EventSet ^ event_set_spec_map,
        ( if event_arg_modes(EventSpecMap, EventName, ModesPrime) then
            Modes = ModesPrime
        else
            % The typechecker should have caught the unknown event,
            % and not let compilation of this predicate proceed any further.
            unexpected($pred, "unknown event")
        ),
        modecheck_event_call(Modes, Args0, Args, !ModeInfo),
        GoalExpr = generic_call(GenericCall, Args, Modes, arg_reg_types_unset,
            detism_det)
    ;
        GenericCall = cast(CastType),
        ( CastType = unsafe_type_cast
        ; CastType = unsafe_type_inst_cast
        ; CastType = equiv_type_cast
        ; CastType = exists_cast
        ),
        ( if
            goal_info_has_feature(GoalInfo0, feature_keep_constant_binding),
            mode_info_get_instmap(!.ModeInfo, InstMap),
            ( if
                Args0 = [Arg1Prime, _Arg2Prime],
                Modes0 = [Mode1Prime, Mode2Prime]
            then
                Arg1 = Arg1Prime,
                Mode1 = Mode1Prime,
                Mode2 = Mode2Prime
            else
                unexpected($pred, "bad cast")
            ),
            Mode1 = in_mode,
            Mode2 = out_mode,
            instmap_lookup_var(InstMap, Arg1, Inst1),
            Inst1 = bound(Unique, _, [bound_functor(ConsId, [])]),
            mode_info_get_module_info(!.ModeInfo, ModuleInfo),
            get_cons_repn_defn(ModuleInfo, ConsId, ConsRepn),
            ConsRepn ^ cr_tag = shared_local_tag_no_args(_, LocalSectag, _)
        then
            LocalSectag = local_sectag(_, PrimSec, _),
            SectagWholeWord = uint.cast_to_int(PrimSec),
            BoundFunctor = bound_functor(int_const(SectagWholeWord), []),
            BoundInst = bound(Unique, inst_test_results_fgtc, [BoundFunctor]),
            NewMode2 = from_to_mode(free, BoundInst),
            Modes = [Mode1, NewMode2]
        else
            Modes = Modes0
        ),
        modecheck_builtin_cast(Modes, Args0, Args, Det, ExtraGoals, !ModeInfo),
        GoalExpr1 = generic_call(GenericCall, Args, Modes, arg_reg_types_unset,
            Det),
        handle_extra_goals(GoalExpr1, ExtraGoals, GoalInfo0, Args0, Args,
            InstMap0, GoalExpr, !ModeInfo)
    ;
        GenericCall = cast(subtype_coerce),
        modecheck_coerce(Args0, Args, Modes0, Modes, Det, ExtraGoals,
            !ModeInfo),
        GoalExpr1 = generic_call(GenericCall, Args, Modes, arg_reg_types_unset,
            Det),
        handle_extra_goals(GoalExpr1, ExtraGoals, GoalInfo0, Args0, Args,
            InstMap0, GoalExpr, !ModeInfo)
    ),

    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "generic_call", !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Modecheck unifications. Most of the work is in modecheck_unify.m.
%

:- pred modecheck_goal_unify(prog_var::in, unify_rhs::in,
    unification::in, unify_context::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out) is det.

modecheck_goal_unify(LHS0, RHS0, Unification0, UnifyContext, GoalInfo0,
        GoalExpr, !ModeInfo) :-
    mode_checkpoint(enter, "unify", !ModeInfo),
    mode_info_set_call_context(call_context_unify(UnifyContext), !ModeInfo),
    modecheck_unification(LHS0, RHS0, Unification0, UnifyContext, GoalInfo0,
        GoalExpr, !ModeInfo),
    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "unify", !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Modecheck foreign_proc goals.
%

:- pred modecheck_goal_call_foreign_proc(pragma_foreign_proc_attributes::in,
    pred_id::in, proc_id::in, list(foreign_arg)::in, list(foreign_arg)::in,
    maybe(trace_expr(trace_runtime))::in, pragma_foreign_proc_impl::in,
    hlds_goal_info::in, hlds_goal_expr::out,
    mode_info::in, mode_info::out) is det.

modecheck_goal_call_foreign_proc(Attributes, PredId, ProcId0, Args0, ExtraArgs,
        MaybeTraceRuntimeCond, PragmaCode, GoalInfo0, GoalExpr, !ModeInfo) :-
    % To modecheck a foreign_proc, we just modecheck the proc for
    % which it is the goal.

    mode_checkpoint(enter, "pragma_foreign_code", !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    DeterminismKnown = no,
    mode_info_set_call_context(call_context_call(mode_call_plain(PredId)),
        !ModeInfo),
    ArgVars0 = list.map(foreign_arg_var, Args0),
    modecheck_call_pred(PredId, DeterminismKnown, ProcId0, ProcId,
        ArgVars0, ArgVars, GoalInfo0, ExtraGoals, !ModeInfo),

    % zs: The assignment to Pragma looks wrong: instead of Args0,
    % I think we should use Args after the following call:
    % replace_foreign_arg_vars(Args0, ArgVars, Args)
    % or is there some reason why Args0 and Args would be the same?
    Pragma = call_foreign_proc(Attributes, PredId, ProcId, Args0, ExtraArgs,
        MaybeTraceRuntimeCond, PragmaCode),
    handle_extra_goals(Pragma, ExtraGoals, GoalInfo0, ArgVars0, ArgVars,
        InstMap0, GoalExpr, !ModeInfo),

    mode_info_unset_call_context(!ModeInfo),
    mode_checkpoint(exit, "pragma_foreign_code", !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Modecheck shorthand goals.
%

:- pred modecheck_goal_shorthand(shorthand_goal_expr::in, hlds_goal_info::in,
    hlds_goal_expr::out, mode_info::in, mode_info::out) is det.

modecheck_goal_shorthand(ShortHand0, GoalInfo0, GoalExpr, !ModeInfo) :-
    (
        ShortHand0 = atomic_goal(_, Outer, Inner, MaybeOutputVars,
            MainGoal0, OrElseGoals0, OrElseInners),

        % The uniqueness of the Outer and Inner variables are handled by the
        % addition of calls to the fake predicates "stm_inner_to_outer_io" and
        % "stm_outer_to_inner_io" during the construction of the HLDS.
        % These calls are removed when atomic goals are expanded.

        mode_checkpoint(enter, "atomic", !ModeInfo),
        mode_info_get_pred_var_multimode_error_map(!.ModeInfo,
            MultiModeErrorMap0),
        AtomicGoalList0 = [MainGoal0 | OrElseGoals0],
        NonLocals = goal_info_get_nonlocals(GoalInfo0),

        % XXX STM: Locking the outer variables would generate an error message
        % during mode analysis of the sub goal because of the calls to
        % "stm_outer_to_inner_io" and "stm_inner_to_outer_io". I (lmika) don't
        % think this is a problem as the uniqueness states of the outer and
        % inner variables are enforced by these calls anyway.

        % mode_info_lock_vars(var_lock_atomic_goal, OuterVars, !ModeInfo),
        modecheck_orelse_list(MultiModeErrorMap0, AtomicGoalList0,
            AtomicGoalList, InstMapList, !ModeInfo),
        mode_info_get_var_types(!.ModeInfo, VarTypes),
        % mode_info_unlock_vars(var_lock_atomic_goal, OuterVars, !ModeInfo),

        MainGoal = list.det_head(AtomicGoalList),
        OrElseGoals = list.det_tail(AtomicGoalList),

        make_arm_instmaps_for_goals(AtomicGoalList, InstMapList, ArmInstMaps),
        instmap_merge(NonLocals, ArmInstMaps, merge_stm_atomic, !ModeInfo),

        % Here we determine the type of atomic goal this is. It could be argued
        % that this should have been done in the typechecker, but the type of
        % the outer variables could be unknown when the typechecker looks
        % at the atomic goal.
        %
        % To prevent the need to traverse the code again, we will put this
        % check here (also: types of variables must be known at this point).

        Outer = atomic_interface_vars(OuterDI, OuterUO),
        lookup_var_type(VarTypes, OuterDI, OuterDIType),
        lookup_var_type(VarTypes, OuterUO, OuterUOType),
        ( if
            ( OuterDIType = io_state_type
            ; OuterDIType = io_io_type
            )
        then
            GoalType = top_level_atomic_goal
        else if
            OuterDIType = stm_atomic_type
        then
            GoalType = nested_atomic_goal
        else
            unexpected($pred, "atomic_goal: invalid outer var type")
        ),

        % The following are sanity checks.
        expect(unify(OuterDIType, OuterUOType), $pred,
            "atomic_goal: mismatched outer var type"),
        Inner = atomic_interface_vars(InnerDI, InnerUO),
        lookup_var_type(VarTypes, InnerDI, InnerDIType),
        lookup_var_type(VarTypes, InnerUO, InnerUOType),
        expect(unify(InnerDIType, stm_atomic_type), $pred,
            "atomic_goal: invalid inner var type"),
        expect(unify(InnerUOType, stm_atomic_type), $pred,
            "atomic_goal: invalid inner var type"),

        ShortHand = atomic_goal(GoalType, Outer, Inner, MaybeOutputVars,
            MainGoal, OrElseGoals, OrElseInners),
        GoalExpr = shorthand(ShortHand),
        mode_checkpoint(exit, "atomic", !ModeInfo)
    ;
        ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
        mode_checkpoint(enter, "try", !ModeInfo),
        mode_info_get_pred_var_multimode_error_map(!.ModeInfo,
            MultiModeErrorMap0),
         modecheck_goal(SubGoal0, SubGoal, !ModeInfo),
        mode_info_set_pred_var_multimode_error_map(MultiModeErrorMap0,
            !ModeInfo),
        ShortHand = try_goal(MaybeIO, ResultVar, SubGoal),
        GoalExpr = shorthand(ShortHand),
        mode_checkpoint(exit, "try", !ModeInfo)
    ;
        ShortHand0 = bi_implication(_, _),
        % These should have been expanded out by now.
        unexpected($pred, "bi_implication")
    ).

:- pred modecheck_orelse_list(pred_var_multimode_error_map::in,
    list(hlds_goal)::in, list(hlds_goal)::out, list(instmap)::out,
    mode_info::in, mode_info::out) is det.

modecheck_orelse_list(_, [], [], [], !ModeInfo).
modecheck_orelse_list(MultiModeErrorMap0, [Goal0 | Goals0], [Goal | Goals],
        [InstMap | InstMaps], !ModeInfo) :-
    mode_info_set_pred_var_multimode_error_map(MultiModeErrorMap0, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMap0),
    modecheck_goal(Goal0, Goal, !ModeInfo),
    mode_info_get_instmap(!.ModeInfo, InstMap),
    mode_info_set_instmap(InstMap0, !ModeInfo),
    modecheck_orelse_list(MultiModeErrorMap0, Goals0, Goals,
        InstMaps, !ModeInfo).

%-----------------------------------------------------------------------------%
%
% Service predicates dealing with solver variables.
%

    % If the condition of a negation or if-then-else contains any inst any
    % non-locals (a potential referential transparency violation), then
    % we need to check that the programmer has recognised the possibility
    % and placed the if-then-else in a promise_<purity> scope.
    %
:- pred check_no_inst_any_vars(negated_context_desc::in, prog_vars::in,
    instmap::in, instmap::in, mode_info::in, mode_info::out) is det.

check_no_inst_any_vars(_, [], _, _, !ModeInfo).
check_no_inst_any_vars(NegCtxtDesc, [NonLocal | NonLocals], InstMap0, InstMap,
        !ModeInfo) :-
    ( if
        ( instmap_lookup_var(InstMap0, NonLocal, Inst)
        ; instmap_lookup_var(InstMap,  NonLocal, Inst)
        ),
        mode_info_get_module_info(!.ModeInfo, ModuleInfo),
        inst_contains_any(ModuleInfo, Inst)
    then
        WaitingVars = set_of_var.make_singleton(NonLocal),
        ModeError = purity_error_should_be_in_promise_purity_scope(NegCtxtDesc,
            NonLocal),
        mode_info_error(WaitingVars, ModeError, !ModeInfo)
    else
        check_no_inst_any_vars(NegCtxtDesc, NonLocals, InstMap0, InstMap,
            !ModeInfo)
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.modecheck_goal.
%-----------------------------------------------------------------------------%
