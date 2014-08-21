%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: cse_detection.m.
% Main author: zs.
% Much of the code is based on switch_detection.m by fjh.
%
% This module looks for unifications that deconstruct the same variable
% with the SAME function symbol in different arms of a disjunction, and
% hoists those deconstructions out of the disjuncts, thus replacing
%
%   (
%       X = f(A1, B1, C1),
%       ...
%   ;
%       X = f(A2, B2, C2),
%       ...
%   )
%
% with
%
%   X = f(A0, B0, C0),
%   (
%       A1 := A0, B1 := B0, C1 := C0,
%       ...
%   ;
%       A2 := A0, B2 := B0, C2 := C0,
%       ...
%   )
%
% This may (and often does) allow switch detection to recognize that
% the transformed disjunction is in fact a switch, e.g. on A0.
% This in turn often allows determinism analysis to recognize that
% the code is in fact deterministic.
%
% The structure of the code in this module is similar to the structure
% of the code in switch_detection.m. That is because the jobs of
% the two modules are related:
%
% cse_detection.m:
%   looks for unifications that deconstruct the same variable
%   with the SAME function symbol in different arms of a disjunction
%
% switch_detection.m:
%   looks for unifications that deconstruct the same variable
%   with DIFFERENT function symbols in different arms of a disjunction
%
%-----------------------------------------------------------------------------%

:- module check_hlds.cse_detection.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- pred detect_cse_in_module(module_info::in, module_info::out) is det.

:- pred detect_cse_in_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_match.
:- import_module check_hlds.modes.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

detect_cse_in_module(!ModuleInfo) :-
    % Traverse the module structure, calling `detect_cse_in_goal'
    % for each procedure body.
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    detect_cse_in_preds(PredIds, !ModuleInfo).

:- pred detect_cse_in_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

detect_cse_in_preds([], !ModuleInfo).
detect_cse_in_preds([PredId | PredIds], !ModuleInfo) :-
    module_info_get_preds(!.ModuleInfo, PredTable),
    map.lookup(PredTable, PredId, PredInfo),
    detect_cse_in_pred(PredId, PredInfo, !ModuleInfo),
    detect_cse_in_preds(PredIds, !ModuleInfo).

:- pred detect_cse_in_pred(pred_id::in, pred_info::in,
    module_info::in, module_info::out) is det.

detect_cse_in_pred(PredId, PredInfo, !ModuleInfo) :-
    ProcIds = pred_info_non_imported_procids(PredInfo),
    detect_cse_in_procs(PredId, ProcIds, !ModuleInfo).

:- pred detect_cse_in_procs(pred_id::in, list(proc_id)::in,
    module_info::in, module_info::out) is det.

detect_cse_in_procs(_PredId, [], !ModuleInfo).
detect_cse_in_procs(PredId, [ProcId | ProcIds], !ModuleInfo) :-
    detect_cse_in_proc(PredId, ProcId, !ModuleInfo),
    detect_cse_in_procs(PredId, ProcIds, !ModuleInfo).

detect_cse_in_proc(PredId, ProcId, !ModuleInfo) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
    (
        VeryVerbose = yes,
        trace [io(!IO)] (
            io.write_string("% Detecting common deconstructions for ", !IO),
            write_pred_id(!.ModuleInfo, PredId, !IO),
            io.write_string("\n", !IO)
        )
    ;
        VeryVerbose = no
    ),

    % XXX We wouldn't have to keep getting the proc_info out of and back into
    % the module_info if modecheck didn't take a whole module_info.
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),

    detect_cse_in_proc_pass(!.ModuleInfo, Redo, ProcInfo0, ProcInfo1),

    map.det_update(ProcId, ProcInfo1, ProcTable0, ProcTable1),
    pred_info_set_procedures(ProcTable1, PredInfo0, PredInfo1),
    map.det_update(PredId, PredInfo1, PredTable0, PredTable1),
    module_info_set_preds(PredTable1, !ModuleInfo),

    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    trace [io(!IO)] (
        maybe_report_stats(Statistics, !IO)
    ),
    (
        Redo = no
    ;
        Redo = yes,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                io.write_string("% Repeating mode check for ", !IO),
                write_pred_id(!.ModuleInfo, PredId, !IO),
                io.write_string("\n", !IO)
            )
        ;
            VeryVerbose = no
        ),
        modecheck_proc(ProcId, PredId, !ModuleInfo, ModeSpecs, _Changed),
        trace [io(!IO)] (
            maybe_report_stats(Statistics, !IO)
        ),
        ContainsErrors = contains_errors(Globals, ModeSpecs),
        (
            ContainsErrors = yes,
            unexpected($module, $pred, "mode check fails when repeated")
        ;
            ContainsErrors = no
            % There is no point in returning any warnings and/or informational
            % messages to our caller, since any such messages should already
            % have been gathered during the initial mode analysis pass.
        ),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                io.write_string("% Repeating switch detection for ", !IO),
                write_pred_id(!.ModuleInfo, PredId, !IO),
                io.write_string("\n", !IO)
            )
        ;
            VeryVerbose = no
        ),

        module_info_get_preds(!.ModuleInfo, PredTable2),
        map.lookup(PredTable2, PredId, PredInfo2),
        pred_info_get_procedures(PredInfo2, ProcTable2),
        map.lookup(ProcTable2, ProcId, ProcInfo2),

        SwitchDetectInfo = init_switch_detect_info(!.ModuleInfo),
        detect_switches_in_proc(SwitchDetectInfo, ProcInfo2, ProcInfo),

        map.det_update(ProcId, ProcInfo, ProcTable2, ProcTable3),
        pred_info_set_procedures(ProcTable3, PredInfo2, PredInfo3),
        map.det_update(PredId, PredInfo3, PredTable2, PredTable3),
        module_info_set_preds(PredTable3, !ModuleInfo),

        trace [io(!IO)] (
            maybe_report_stats(Statistics, !IO)
        ),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                io.write_string("% Repeating common " ++
                    "deconstruction detection for ", !IO),
                write_pred_id(!.ModuleInfo, PredId, !IO),
                io.write_string("\n", !IO)
            )
        ;
            VeryVerbose = no
        ),
        detect_cse_in_proc(PredId, ProcId, !ModuleInfo)
    ).

:- type cse_info
    --->    cse_info(
                csei_varset         :: prog_varset,
                csei_vartypes       :: vartypes,
                csei_rtti_varmaps   :: rtti_varmaps,
                csei_module_info    :: module_info
            ).

:- pred detect_cse_in_proc_pass(module_info::in, bool::out,
    proc_info::in, proc_info::out) is det.

detect_cse_in_proc_pass(ModuleInfo, Redo, !ProcInfo) :-
    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_cse_in_goal'.

    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_initial_instmap(!.ProcInfo, ModuleInfo, InstMap0),
    proc_info_get_varset(!.ProcInfo, Varset0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    CseInfo0 = cse_info(Varset0, VarTypes0, RttiVarMaps0, ModuleInfo),
    detect_cse_in_goal(Goal0, Goal1, CseInfo0, CseInfo, InstMap0, Redo),

    (
        Redo = no
    ;
        Redo = yes,

        % ModuleInfo should not be changed by detect_cse_in_goal.
        CseInfo = cse_info(VarSet1, VarTypes1, RttiVarMaps1, _),
        proc_info_get_headvars(!.ProcInfo, HeadVars),

        implicitly_quantify_clause_body_general(
            ordinary_nonlocals_maybe_lambda,
            HeadVars, _Warnings,
            Goal1, Goal, VarSet1, VarSet, VarTypes1, VarTypes,
            RttiVarMaps1, RttiVarMaps),

        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_varset(VarSet, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo)
    ).

%-----------------------------------------------------------------------------%

    % Given a goal, and the instmap on entry to that goal,
    % find disjunctions that contain common subexpressions
    % and hoist these out of the disjunction. At the moment
    % we only look for cses that are deconstruction unifications.
    %
:- pred detect_cse_in_goal(hlds_goal::in, hlds_goal::out,
    cse_info::in, cse_info::out, instmap::in, bool::out) is det.

detect_cse_in_goal(Goal0, Goal, !CseInfo, InstMap0, Redo) :-
    detect_cse_in_goal_update_instmap(Goal0, Goal, !CseInfo,
        InstMap0, _InstMap, Redo).

    % This version is the same as the above except that it returns
    % the resulting instmap on exit from the goal, which is computed by
    % applying the instmap delta specified in the goal's goalinfo.
    %
:- pred detect_cse_in_goal_update_instmap(hlds_goal::in, hlds_goal::out,
    cse_info::in, cse_info::out, instmap::in, instmap::out, bool::out) is det.

detect_cse_in_goal_update_instmap(Goal0, Goal, !CseInfo, InstMap0, InstMap,
        Redo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    detect_cse_in_goal_expr(GoalExpr0, GoalExpr, !CseInfo, GoalInfo,
        InstMap0, Redo),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    instmap.apply_instmap_delta(InstMap0, InstMapDelta, InstMap).

    % Here we process each of the different sorts of goals.
    %
:- pred detect_cse_in_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    cse_info::in, cse_info::out, hlds_goal_info::in,
    instmap::in, bool::out) is det.

detect_cse_in_goal_expr(GoalExpr0, GoalExpr, !CseInfo, GoalInfo, InstMap0,
        Redo) :-
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0,
        Redo = no
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Unify,  UnifyContext),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocalVars, Vars, Modes, Det, LambdaGoal0),
            ModuleInfo = !.CseInfo ^ csei_module_info,
            instmap.pre_lambda_update(ModuleInfo, Vars, Modes,
                InstMap0, InstMap1),
            detect_cse_in_goal(LambdaGoal0, LambdaGoal, !CseInfo,
                InstMap1, Redo),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocalVars, Vars, Modes, Det, LambdaGoal)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            RHS = RHS0,
            Redo = no
        ),
        GoalExpr = unify(LHS, RHS, Mode,Unify, UnifyContext)
    ;
        GoalExpr0 = negation(SubGoal0),
        detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0, Redo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        ( Reason0 = from_ground_term(_, from_ground_term_construct) ->
            % There are no deconstructions at all inside these scopes.
            GoalExpr = GoalExpr0,
            Redo = no
        ;
            detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0, Redo),
            (
                Redo = yes,
                Reason0 = from_ground_term(_, from_ground_term_deconstruct)
            ->
                % If we remove a goal from such a scope, what is left
                % may no longer satisfy the invariants we expect it to satisfy.
                SubGoal = hlds_goal(GoalExpr, _)
            ;
                GoalExpr = scope(Reason0, SubGoal)
            )
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        detect_cse_in_conj(Goals0, Goals, !CseInfo, ConjType, InstMap0, Redo),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        (
            Goals0 = [],
            Redo = no,
            GoalExpr = disj([])
        ;
            Goals0 = [_ | _],
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            NonLocalsList = set_of_var.to_sorted_list(NonLocals),
            detect_cse_in_disj(NonLocalsList, Goals0, GoalInfo,
                InstMap0, !CseInfo, Redo, GoalExpr)
        )
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        NonLocalsList = set_of_var.to_sorted_list(NonLocals),
        detect_cse_in_cases(NonLocalsList, Var, CanFail, Cases0, GoalInfo,
            InstMap0, !CseInfo, Redo, GoalExpr)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        NonLocalsList = set_of_var.to_sorted_list(NonLocals),
        detect_cse_in_ite(NonLocalsList, Vars, Cond0, Then0, Else0, GoalInfo,
            InstMap0, !CseInfo, Redo, GoalExpr)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(AtomicGoalType, Outer, Inner,
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            detect_cse_in_goal(MainGoal0, MainGoal, !CseInfo, InstMap0, Redo1),
            detect_cse_in_independent_goals(OrElseGoals0, OrElseGoals,
                !CseInfo, InstMap0, Redo2),
            ShortHand = atomic_goal(AtomicGoalType, Outer, Inner,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners),
            bool.or(Redo1, Redo2, Redo)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($module, $pred, "bi_implication")
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            % XXX not sure about this as SubGoal0 is not in its final form.
            % Also, mightn't the try "Goal" part get hoisted out?
            detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0, Redo),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand)
    ).

%-----------------------------------------------------------------------------%

:- pred detect_cse_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    cse_info::in, cse_info::out, conj_type::in, instmap::in, bool::out) is det.

detect_cse_in_conj([], [], !CseInfo, _ConjType, _InstMap, no).
detect_cse_in_conj([Goal0 | Goals0], Goals, !CseInfo, ConjType, !.InstMap,
        Redo) :-
    detect_cse_in_goal_update_instmap(Goal0, Goal, !CseInfo, !InstMap, Redo1),
    detect_cse_in_conj(Goals0, TailGoals, !CseInfo, ConjType, !.InstMap,
        Redo2),
    % Flatten any non-flat conjunctions we create.
    (
        Goal = hlds_goal(conj(InnerConjType, ConjGoals), _),
        ConjType = InnerConjType
    ->
        Goals = ConjGoals ++ TailGoals
    ;
        Goals = [Goal | TailGoals]
    ),
    bool.or(Redo1, Redo2, Redo).

%-----------------------------------------------------------------------------%

    % These are the interesting bits - we've found a non-empty branched
    % structure, and we've got a list of the non-local variables of that
    % structure. Now for each non-local variable, we check whether each
    % branch matches that variable against the same functor.
    %
:- pred detect_cse_in_disj(list(prog_var)::in, list(hlds_goal)::in,
    hlds_goal_info::in, instmap::in, cse_info::in,
    cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_disj([], Goals0, _, InstMap, !CseInfo, Redo, disj(Goals)) :-
    detect_cse_in_independent_goals(Goals0, Goals, !CseInfo, InstMap, Redo).
detect_cse_in_disj([Var | Vars], Goals0, GoalInfo0, InstMap0,
        !CseInfo, Redo, GoalExpr) :-
    (
        instmap_lookup_var(InstMap0, Var, VarInst0),
        ModuleInfo = !.CseInfo ^ csei_module_info,
        % XXX We only need inst_is_bound, but leave this as it is until
        % mode analysis can handle aliasing between free variables.
        inst_is_ground_or_any(ModuleInfo, VarInst0),
        common_deconstruct(Goals0, Var, !CseInfo, Unify,
            FirstOldNew, LaterOldNew, Goals)
    ->
        maybe_update_existential_data_structures(Unify,
            FirstOldNew, LaterOldNew, !CseInfo),
        GoalExpr = conj(plain_conj,
            [Unify, hlds_goal(disj(Goals), GoalInfo0)]),
        Redo = yes
    ;
        detect_cse_in_disj(Vars, Goals0, GoalInfo0, InstMap0,
            !CseInfo, Redo, GoalExpr)
    ).

:- pred detect_cse_in_independent_goals(
    list(hlds_goal)::in, list(hlds_goal)::out,
    cse_info::in, cse_info::out, instmap::in, bool::out) is det.

detect_cse_in_independent_goals([], [], !CseInfo, _, no).
detect_cse_in_independent_goals([Goal0 | Goals0], [Goal | Goals], !CseInfo,
        InstMap0, Redo) :-
    detect_cse_in_goal(Goal0, Goal, !CseInfo, InstMap0, Redo1),
    detect_cse_in_independent_goals(Goals0, Goals, !CseInfo, InstMap0, Redo2),
    bool.or(Redo1, Redo2, Redo).

:- pred detect_cse_in_cases(list(prog_var)::in, prog_var::in, can_fail::in,
    list(case)::in, hlds_goal_info::in, instmap::in,
    cse_info::in, cse_info::out, bool::out, hlds_goal_expr::out) is det.

detect_cse_in_cases([], SwitchVar, CanFail, Cases0, _GoalInfo, InstMap0,
        !CseInfo, Redo, switch(SwitchVar, CanFail, Cases)) :-
    detect_cse_in_cases_arms(Cases0, Cases, !CseInfo, InstMap0, Redo).
detect_cse_in_cases([Var | Vars], SwitchVar, CanFail, Cases0, GoalInfo,
        InstMap0, !CseInfo, Redo, GoalExpr) :-
    (
        Var \= SwitchVar,
        instmap_lookup_var(InstMap0, Var, VarInst0),
        ModuleInfo = !.CseInfo ^ csei_module_info,
        % XXX We only need inst_is_bound, but leave this as it is until
        % mode analysis can handle aliasing between free variables.
        inst_is_ground_or_any(ModuleInfo, VarInst0),
        common_deconstruct_cases(Cases0, Var, !CseInfo,
            Unify, FirstOldNew, LaterOldNew, Cases)
    ->
        maybe_update_existential_data_structures(Unify,
            FirstOldNew, LaterOldNew, !CseInfo),
        GoalExpr = conj(plain_conj,
            [Unify, hlds_goal(switch(SwitchVar, CanFail, Cases), GoalInfo)]),
        Redo = yes
    ;
        detect_cse_in_cases(Vars, SwitchVar, CanFail, Cases0, GoalInfo,
            InstMap0, !CseInfo, Redo, GoalExpr)
    ).

:- pred detect_cse_in_cases_arms(list(case)::in, list(case)::out,
    cse_info::in, cse_info::out, instmap::in, bool::out) is det.

detect_cse_in_cases_arms([], [], !CseInfo, _, no).
detect_cse_in_cases_arms([Case0 | Cases0], [Case | Cases], !CseInfo, InstMap0,
        Redo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    detect_cse_in_goal(Goal0, Goal, !CseInfo, InstMap0, Redo1),
    Case = case(MainConsId, OtherConsIds, Goal),
    detect_cse_in_cases_arms(Cases0, Cases, !CseInfo, InstMap0, Redo2),
    bool.or(Redo1, Redo2, Redo).

:- pred detect_cse_in_ite(list(prog_var)::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_info::in,
    instmap::in, cse_info::in, cse_info::out, bool::out,
    hlds_goal_expr::out) is det.

detect_cse_in_ite([], IfVars, Cond0, Then0, Else0, _, InstMap, !CseInfo,
        Redo, if_then_else(IfVars, Cond, Then, Else)) :-
    detect_cse_in_ite_arms(Cond0, Cond, Then0, Then, Else0, Else, !CseInfo,
        InstMap, Redo).
detect_cse_in_ite([Var | Vars], IfVars, Cond0, Then0, Else0, GoalInfo,
        InstMap, !CseInfo, Redo, GoalExpr) :-
    (
        ModuleInfo = !.CseInfo ^ csei_module_info,
        instmap_lookup_var(InstMap, Var, VarInst0),
        % XXX We only need inst_is_bound, but leave this as it is until
        % mode analysis can handle aliasing between free variables.
        inst_is_ground_or_any(ModuleInfo, VarInst0),
        common_deconstruct([Then0, Else0], Var, !CseInfo,
            Unify, FirstOldNew, LaterOldNew, Goals),
        Goals = [Then, Else]
    ->
        maybe_update_existential_data_structures(Unify,
            FirstOldNew, LaterOldNew, !CseInfo),
        IfGoal = hlds_goal(if_then_else(IfVars, Cond0, Then, Else), GoalInfo),
        GoalExpr = conj(plain_conj, [Unify, IfGoal]),
        Redo = yes
    ;
        detect_cse_in_ite(Vars, IfVars, Cond0, Then0, Else0, GoalInfo,
            InstMap, !CseInfo, Redo, GoalExpr)
    ).

:- pred detect_cse_in_ite_arms(hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    cse_info::in, cse_info::out, instmap::in, bool::out) is det.

detect_cse_in_ite_arms(Cond0, Cond, Then0, Then, Else0, Else, !CseInfo,
        InstMap0, Redo) :-
    detect_cse_in_goal_update_instmap(Cond0, Cond, !CseInfo,
        InstMap0, InstMap1, Redo1),
    detect_cse_in_goal(Then0, Then, !CseInfo, InstMap1, Redo2),
    detect_cse_in_goal(Else0, Else, !CseInfo, InstMap0, Redo3),
    bool.or(Redo1, Redo2, Redo12),
    bool.or(Redo12, Redo3, Redo).

%-----------------------------------------------------------------------------%

    % common_deconstruct(Goals0, Var, !CseInfo, Unify, Goals):
    % input vars:
    %   Goals0 is a list of parallel goals in a branched structure
    %   (disjunction, if-then-else, or switch).
    %   Var is the variable we are looking for a common deconstruction on.
    %   !.CseInfo contains the original varset and type map.
    % output vars:
    %   !:CseInfo has a varset and a type map reflecting the new variables
    %   we have introduced.
    %   Goals is the modified version of Goals0 after the common deconstruction
    %   has been hoisted out, with the new variables as the functor arguments.
    %   Unify is the unification that was hoisted out.
    %
:- pred common_deconstruct(list(hlds_goal)::in, prog_var::in, cse_info::in,
    cse_info::out, hlds_goal::out, assoc_list(prog_var)::out,
    list(assoc_list(prog_var))::out, list(hlds_goal)::out) is semidet.

common_deconstruct(Goals0, Var, !CseInfo, Unify, FirstOldNew, LaterOldNew,
        Goals) :-
    common_deconstruct_2(Goals0, Var, before_candidate,
        have_candidate(Unify, FirstOldNew, LaterOldNew), !CseInfo, Goals),
    LaterOldNew = [_ | _].

:- pred common_deconstruct_2(list(hlds_goal)::in, prog_var::in,
    cse_state::in, cse_state::out, cse_info::in, cse_info::out,
    list(hlds_goal)::out) is semidet.

common_deconstruct_2([], _Var, !CseState, !CseInfo, []).
common_deconstruct_2([Goal0 | Goals0], Var, !CseState, !CseInfo,
        [Goal | Goals]) :-
    find_bind_var(Var, find_bind_var_for_cse_in_deconstruct, Goal0, Goal,
        !CseState, !CseInfo, did_find_deconstruct),
    !.CseState = have_candidate(_, _, _),
    common_deconstruct_2(Goals0, Var, !CseState, !CseInfo, Goals).

%-----------------------------------------------------------------------------%

:- pred common_deconstruct_cases(list(case)::in, prog_var::in,
    cse_info::in, cse_info::out, hlds_goal::out, assoc_list(prog_var)::out,
    list(assoc_list(prog_var))::out, list(case)::out) is semidet.

common_deconstruct_cases(Cases0, Var, !CseInfo, Unify,
        FirstOldNew, LaterOldNew, Cases) :-
    common_deconstruct_cases_2(Cases0, Var, before_candidate,
        have_candidate(Unify, FirstOldNew, LaterOldNew), !CseInfo, Cases),
    LaterOldNew = [_ | _].

:- pred common_deconstruct_cases_2(list(case)::in, prog_var::in,
    cse_state::in, cse_state::out, cse_info::in, cse_info::out,
    list(case)::out) is semidet.

common_deconstruct_cases_2([], _Var, !CseState, !CseInfo, []).
common_deconstruct_cases_2([Case0 | Cases0], Var, !CseState, !CseInfo,
        [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    find_bind_var(Var, find_bind_var_for_cse_in_deconstruct, Goal0, Goal,
        !CseState, !CseInfo, did_find_deconstruct),
    Case = case(MainConsId, OtherConsIds, Goal),
    !.CseState = have_candidate(_, _, _),
    common_deconstruct_cases_2(Cases0, Var, !CseState, !CseInfo, Cases).

%-----------------------------------------------------------------------------%

    % This data structure represents the state of the search for
    % deconstructions in all the branches of a branched control structure
    % that deconstruct a given variable with the same functor.
    % Initially, we don't know what unification we will hoist out, so the
    % state is before_candidate. When we find a unification we want to
    % hoist out, this fixes the functor, and the state is have_candidate.
    % If we find that some branches unify that variable with some other
    % functor, we have multiple_candidates, which means that we don't hoist
    % out any of them. (Although our caller may try again with another
    % variable.)
    %
    % The goal field contains the unification we are proposing to put
    % before the branched control structure. The first_old_new field
    % gives the mapping from argument variables in the old unification
    % in the first branch to the freshly created variables in the goal
    % being hoisted before the branched control structure. The
    % later_old_new field contains the same information for the second
    % and later branches.
:- type cse_state
    --->    before_candidate
    ;       have_candidate(
                goal            ::  hlds_goal,
                first_old_new   ::  assoc_list(prog_var),
                later_old_new   ::  list(assoc_list(prog_var))
            )
    ;       multiple_candidates.

:- pred find_bind_var_for_cse_in_deconstruct(prog_var::in, hlds_goal::in,
    list(hlds_goal)::out, cse_state::in, cse_state::out,
    cse_info::in, cse_info::out) is det.

find_bind_var_for_cse_in_deconstruct(Var, Goal0, Goals,
        !CseState, !CseInfo) :-
    (
        !.CseState = before_candidate,
        construct_common_unify(Var, Goal0, !CseInfo, OldNewVars,
            HoistedGoal, Goals),
        !:CseState = have_candidate(HoistedGoal, OldNewVars, [])
    ;
        !.CseState = have_candidate(HoistedGoal,
            FirstOldNewVars, LaterOldNewVars0),
        Goal0 = hlds_goal(_, GoalInfo),
        Context = goal_info_get_context(GoalInfo),
        (
            find_similar_deconstruct(HoistedGoal,
                Goal0, Context, OldNewVars, Goals0)
        ->
            Goals = Goals0,
            LaterOldNewVars = [OldNewVars | LaterOldNewVars0],
            !:CseState = have_candidate(HoistedGoal,
                FirstOldNewVars, LaterOldNewVars)
        ;
            Goals = [Goal0],
            !:CseState = multiple_candidates
        )
    ;
        !.CseState = multiple_candidates,
        Goals = [Goal0],
        !:CseState = multiple_candidates
    ).

:- pred construct_common_unify(prog_var::in, hlds_goal::in,
    cse_info::in, cse_info::out, assoc_list(prog_var)::out,
    hlds_goal::out, list(hlds_goal)::out) is det.

construct_common_unify(Var, hlds_goal(GoalExpr0, GoalInfo), !CseInfo,
        OldNewVars, HoistedGoal, Replacements) :-
    (
        GoalExpr0 = unify(_, RHS, Umode, Unif0, Ucontext),
        Unif0 = deconstruct(_, Consid, Args, Submodes, CanFail, CanCGC)
    ->
        Unif = deconstruct(Var, Consid, Args, Submodes, CanFail, CanCGC),
        (
            RHS = rhs_functor(_, _, _),
            GoalExpr1 = unify(Var, RHS, Umode, Unif, Ucontext)
        ;
            ( RHS = rhs_var(_)
            ; RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _, _)
            ),
            unexpected($module, $pred, "non-functor unify")
        ),
        Context = goal_info_get_context(GoalInfo),
        create_parallel_subterms(Args, Context, Ucontext, !CseInfo,
            OldNewVars, Replacements),
        map.from_assoc_list(OldNewVars, Sub),
        rename_some_vars_in_goal(Sub, hlds_goal(GoalExpr1, GoalInfo),
            HoistedGoal)
    ;
        unexpected($module, $pred, "non-unify goal")
    ).

:- pred create_parallel_subterms(list(prog_var)::in, prog_context::in,
    unify_context::in, cse_info::in, cse_info::out,
    assoc_list(prog_var)::out, list(hlds_goal)::out) is det.

create_parallel_subterms([], _, _, !CseInfo, [], []).
create_parallel_subterms([OFV | OFV0], Context, UnifyContext, !CseInfo,
        OldNewVars, Replacements) :-
    create_parallel_subterms(OFV0, Context, UnifyContext, !CseInfo,
        OldNewVars1, Replacements1),
    create_parallel_subterm(OFV, Context, UnifyContext, !CseInfo,
        OldNewVars1, OldNewVars, Goal),
    Replacements = [Goal | Replacements1].

:- pred create_parallel_subterm(prog_var::in, prog_context::in,
    unify_context::in, cse_info::in, cse_info::out,
    assoc_list(prog_var)::in, assoc_list(prog_var)::out,
    hlds_goal::out) is det.

create_parallel_subterm(OFV, Context, UnifyContext, !CseInfo, !OldNewVar,
        Goal) :-
    VarSet0 = !.CseInfo ^ csei_varset,
    VarTypes0 = !.CseInfo ^ csei_vartypes,
    varset.new_var(NFV, VarSet0, VarSet),
    lookup_var_type(VarTypes0, OFV, Type),
    add_var_type(NFV, Type, VarTypes0, VarTypes),
    !:OldNewVar = [OFV - NFV | !.OldNewVar],
    UnifyContext = unify_context(MainCtxt, SubCtxt),
    % It is ok to create complicated unifications here, because we rerun
    % mode analysis on the resulting goal. It would be nicer to generate
    % the right assignment unification directly, but that would require keeping
    % track of the inst of OFV.
    create_pure_atomic_complicated_unification(OFV, rhs_var(NFV),
        Context, MainCtxt, SubCtxt, Goal),
    !CseInfo ^ csei_varset := VarSet,
    !CseInfo ^ csei_vartypes := VarTypes.

%-----------------------------------------------------------------------------%

:- pred find_similar_deconstruct(hlds_goal::in, hlds_goal::in,
    prog_context::in, assoc_list(prog_var)::out, list(hlds_goal)::out)
    is semidet.

find_similar_deconstruct(HoistedUnifyGoal, OldUnifyGoal, Context,
        OldHoistedVars, Replacements) :-
    (
        HoistedUnifyGoal = hlds_goal(unify(_, _, _, HoistedUnifyInfo, OC), _),
        HoistedUnifyInfo = deconstruct(_, HoistedFunctor,
            HoistedVars, _, _, _),
        OldUnifyGoal = hlds_goal(unify(_, _, _, OldUnifyInfo, _NC), _),
        OldUnifyInfo = deconstruct(_, OldFunctor, OldVars, _, _, _)
    ->
        HoistedFunctor = OldFunctor,
        list.length(HoistedVars, HoistedVarsCount),
        list.length(OldVars, OldVarsCount),
        HoistedVarsCount = OldVarsCount,
        assoc_list.from_corresponding_lists(OldVars, HoistedVars,
            OldHoistedVars),
        pair_subterms(OldHoistedVars, Context, OC, Replacements)
    ;
        unexpected($module, $pred, "non-deconstruct unify")
    ).

:- pred pair_subterms(assoc_list(prog_var)::in, prog_context::in,
    unify_context::in, list(hlds_goal)::out) is det.

pair_subterms([], _Context, _UnifyContext, []).
pair_subterms([OldVar - HoistedVar | OldHoistedVars], Context, UnifyContext,
        Replacements) :-
    pair_subterms(OldHoistedVars, Context, UnifyContext, Replacements1),
    ( OldVar = HoistedVar ->
        Replacements = Replacements1
    ;
        UnifyContext = unify_context(MainCtxt, SubCtxt),
        % It is ok to create complicated unifications here, because we rerun
        % mode analysis on the resulting goal. It would be nicer to generate
        % the right assignment unification directly, but that would require
        % keeping track of the inst of OldVar.
        create_pure_atomic_complicated_unification(HoistedVar, rhs_var(OldVar),
            Context, MainCtxt, SubCtxt, Goal),
        Replacements = [Goal | Replacements1]
    ).

%-----------------------------------------------------------------------------%

% This section handles the case where the functor involved in the
% common subexpression contains existentially typed arguments,
% whether or not they are constrained to belong to a typeclass.
% In such cases, what the compiler used to consider several distinct
% types (the types of say the first the existentially typed argument
% in the deconstructions in the different branches) become one (in this
% case, the type of the first existentially typed argument in the
% hoisted out deconstruction). The prog_vars describing the types
% of the existentially typed arguments (i.e. containing their
% typeinfos) change as well, from being some of the variables in
% in the original deconstructions to being the corresponding variables
% in the hoisted out deconstruction.
%
% As an example, consider a disjunction such as
%
%   (
%       HeadVar.g2_2 = x:u(TypeClassInfo_for_v_8, V_4),
%       ...
%   ;
%       HeadVar.g2_2 = x:u(TypeClassInfo_for_v_14, V_6)
%       ...
%   )
%
% The main part of cse_detection will replace this with
%
%   HeadVar.g2_2 = x:u(V_17, V_16)
%   (
%       TypeClassInfo_for_v_8 = V_17,
%       V_4 = V_16,
%       ...
%   ;
%       TypeClassInfo_for_v_14 = V_17,
%       V_6 = V_16,
%       ...
%   )
%
% However, this is not enough. Since TypeClassInfo_for_v_8 and
% TypeClassInfo_for_v_14 may (and probably will) be eliminated later,
% it is imperative that the data structures in the proc_info that refer
% to them be updated to eliminate references to those variables.
% Those data structures may originally contain something like this:
%
% type_info varmap:
% T_1 (number 1) -> typeclass_info(TypeClassInfo_for_v_8, 1)
% T_3 (number 3) -> typeclass_info(TypeClassInfo_for_v_14, 1)
% typeclass_info varmap:
% x:v(T_1) -> TypeClassInfo_for_v_8
% x:v(T_3) -> TypeClassInfo_for_v_14
% variable types map:
% V_4 (number 4) :: T_1
% V_6 (number 6) :: T_3
%
% They must be updated like this:
%
% type_info varmap:
% T_1 (number 1) -> typeclass_info(V_17, 1)
% typeclass_info varmap:
% x:v(T_1) -> V_17
% variable types map:
% V_4 (number 4) :: T_1
% V_6 (number 6) :: T_1

:- pred maybe_update_existential_data_structures(hlds_goal::in,
    assoc_list(prog_var)::in, list(assoc_list(prog_var))::in,
    cse_info::in, cse_info::out) is det.

maybe_update_existential_data_structures(Unify, FirstOldNew, LaterOldNew,
        !CseInfo) :-
    (
        Unify = hlds_goal(unify(_, _, _, UnifyInfo, _), _),
        UnifyInfo = deconstruct(Var, ConsId, _, _, _, _),
        ModuleInfo = !.CseInfo ^ csei_module_info,
        VarTypes = !.CseInfo ^ csei_vartypes,
        lookup_var_type(VarTypes, Var, Type),
        cons_id_is_existq_cons(ModuleInfo, Type, ConsId)
    ->
        update_existential_data_structures(FirstOldNew, LaterOldNew, !CseInfo)
    ;
        true
    ).

:- pred update_existential_data_structures(
    assoc_list(prog_var)::in, list(assoc_list(prog_var))::in,
    cse_info::in, cse_info::out) is det.

update_existential_data_structures(FirstOldNew, LaterOldNews, !CseInfo) :-
    list.condense(LaterOldNews, LaterOldNew),
    map.from_assoc_list(FirstOldNew, FirstOldNewMap),
    map.from_assoc_list(LaterOldNew, LaterOldNewMap),

    RttiVarMaps0 = !.CseInfo ^ csei_rtti_varmaps,
    VarTypes0 = !.CseInfo ^ csei_vartypes,

    % Build a map for all locations in the rtti_varmaps that are changed
    % by the application of FirstOldNewMap. The keys of this map are the
    % new locations, and the values are the tvars (from the first branch)
    % that have had their locations moved.
    rtti_varmaps_tvars(RttiVarMaps0, TvarsList),
    list.foldl(find_type_info_locn_tvar_map(RttiVarMaps0, FirstOldNewMap),
        TvarsList, map.init, NewTvarMap),

    % Traverse TVarsList again, this time looking for locations in later
    % branches that merge with locations in the first branch. When we find one,
    % add a type substitution which represents the type variables that were
    % merged.
    list.foldl(find_merged_tvars(RttiVarMaps0, LaterOldNewMap, NewTvarMap),
        TvarsList, map.init, Renaming),

    % Apply the full old->new map and the type substitution to the
    % rtti_varmaps, and apply the type substitution to the vartypes.
    list.append(FirstOldNew, LaterOldNew, OldNew),
    map.from_assoc_list(OldNew, OldNewMap),
    apply_substitutions_to_rtti_varmaps(Renaming, map.init, OldNewMap,
        RttiVarMaps0, RttiVarMaps),
    transform_var_types(apply_variable_renaming_to_type(Renaming),
        VarTypes0, VarTypes),

    !CseInfo ^ csei_rtti_varmaps := RttiVarMaps,
    !CseInfo ^ csei_vartypes := VarTypes.

:- pred find_type_info_locn_tvar_map(rtti_varmaps::in,
    map(prog_var, prog_var)::in, tvar::in,
    map(type_info_locn, tvar)::in, map(type_info_locn, tvar)::out) is det.

find_type_info_locn_tvar_map(RttiVarMaps, FirstOldNewMap, Tvar, !NewTvarMap) :-
    rtti_lookup_type_info_locn(RttiVarMaps, Tvar, TypeInfoLocn0),
    type_info_locn_var(TypeInfoLocn0, Old),
    ( map.search(FirstOldNewMap, Old, New) ->
        type_info_locn_set_var(New, TypeInfoLocn0, TypeInfoLocn),
        map.det_insert(TypeInfoLocn, Tvar, !NewTvarMap)
    ;
        true
    ).

:- pred find_merged_tvars(rtti_varmaps::in, map(prog_var, prog_var)::in,
    map(type_info_locn, tvar)::in, tvar::in,
    tvar_renaming::in, tvar_renaming::out) is det.

find_merged_tvars(RttiVarMaps, LaterOldNewMap, NewTvarMap, Tvar, !Renaming) :-
    rtti_lookup_type_info_locn(RttiVarMaps, Tvar, TypeInfoLocn0),
    type_info_locn_var(TypeInfoLocn0, Old),
    ( map.search(LaterOldNewMap, Old, New) ->
        type_info_locn_set_var(New, TypeInfoLocn0, TypeInfoLocn),
        map.lookup(NewTvarMap, TypeInfoLocn, NewTvar),
        ( NewTvar = Tvar ->
            true
        ;
            map.det_insert(Tvar, NewTvar, !Renaming)
        )
    ;
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module check_hlds.cse_detection.
%-----------------------------------------------------------------------------%
