%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 1995-2012 The University of Melbourne.
% Copyright (C) 2015 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
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
%       <arm 1>
%   ;
%       X = f(A2, B2, C2),
%       <arm 2>
%   )
%
% with
%
%   X = f(A0, B0, C0),
%   (
%       A1 := A0, B1 := B0, C1 := C0,
%       <arm 1>
%   ;
%       A2 := A0, B2 := B0, C2 := C0,
%       <arm 2>
%   )
%
% This may (and often does) allow switch detection to recognize that
% the transformed disjunction is in fact a switch, e.g. on A0.
% This in turn often allows determinism analysis to recognize that
% the code is in fact deterministic.
%
% In theory, we could use goal renames instead of adding unifications.
% This means that we instead of the above, we could generate this:
%
%   X = f(A0, B0, C0),
%   (
%       <arm 1> with the substitution [A1 -> A0, B1 -> B0, C1 -> C0]
%   ;
%       <arm 2> with the substitution [A2 -> A0, B2 -> B0, C2 -> C0]
%   )
%
% This would reduce the size of the goal instead of increasing it,
% which would help speed up later passes of the compiler. Unfortunately,
% it also has the potential to make error messages about code in the
% various <arm i> confusing: if the user wrote "p(A2, ...)", he/she will
% be surprised to see an error message mention "A0" in the call to p.
% That is why we add unifications instead of doing renaming. Once
% the compiler's semantic checks are done and any error messages
% have been generated, the simplification pass will try to remove
% all "excess" unifications, including the ones we insert here, 
%
%---------------------------------------------------------------------------%
%
% Note that the structure of the code in this module is similar to the
% structure of the code in switch_detection.m. That is because the jobs of
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
%---------------------------------------------------------------------------%

:- module check_hlds.cse_detection.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.

:- pred detect_cse_in_module(module_info::in, module_info::out) is det.

:- pred detect_cse_in_proc(pred_id::in, proc_id::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.inst_test.
:- import_module check_hlds.modes.
:- import_module check_hlds.switch_detection.
:- import_module check_hlds.type_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.

:- import_module assoc_list.
:- import_module bool.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%

detect_cse_in_module(!ModuleInfo) :-
    % Traverse the module structure, calling `detect_cse_in_goal'
    % for each procedure body.
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    detect_cse_in_preds(PredIds, !ModuleInfo).

:- pred detect_cse_in_preds(list(pred_id)::in,
    module_info::in, module_info::out) is det.

detect_cse_in_preds([], !ModuleInfo).
detect_cse_in_preds([PredId | PredIds], !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    detect_cse_in_pred(PredId, PredInfo, !ModuleInfo),
    detect_cse_in_preds(PredIds, !ModuleInfo).

:- pred detect_cse_in_pred(pred_id::in, pred_info::in,
    module_info::in, module_info::out) is det.

detect_cse_in_pred(PredId, PredInfo, !ModuleInfo) :-
    ProcIds = pred_info_valid_non_imported_procids(PredInfo),
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
            get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
            io.format(ProgressStream,
                "%% Detecting common deconstructions for %s\n",
                [s(pred_id_to_user_string(!.ModuleInfo, PredId))], !IO)
        )
    ;
        VeryVerbose = no
    ),

    % XXX We wouldn't have to keep getting the proc_info out of and back into
    % the module_info if modecheck didn't take a whole module_info.
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),

    detect_cse_in_proc_pass(!.ModuleInfo, Redo, ProcInfo0, ProcInfo1),

    pred_info_set_proc_info(ProcId, ProcInfo1, PredInfo0, PredInfo1),
    module_info_set_pred_info(PredId, PredInfo1, !ModuleInfo),

    globals.lookup_bool_option(Globals, detailed_statistics, Statistics),
    trace [io(!IO)] (
        get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
        maybe_report_stats(ProgressStream, Statistics, !IO)
    ),
    (
        Redo = no
    ;
        Redo = yes,
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
                io.format(ProgressStream,
                    "%% Repeating mode check for %s\n",
                    [s(pred_id_to_user_string(!.ModuleInfo, PredId))], !IO)
            )
        ;
            VeryVerbose = no
        ),
        modecheck_proc(PredId, ProcId, !ModuleInfo, _Changed, ModeSpecs),
        trace [io(!IO)] (
            get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
            maybe_report_stats(ProgressStream, Statistics, !IO)
        ),
        ContainsErrors = contains_errors(Globals, ModeSpecs),
        (
            ContainsErrors = yes,
            trace [io(!IO)] (
                maybe_dump_hlds(!.ModuleInfo, 46, "cse_repeat_modecheck",
                    no_prev_dump, _DumpInfo, !IO),
                get_error_output_stream(!.ModuleInfo, ErrorStream, !IO),
                write_error_specs(ErrorStream, Globals, ModeSpecs, !IO)
            ),
            unexpected($pred, "mode check fails when repeated")
        ;
            ContainsErrors = no
            % There is no point in returning any warnings and/or informational
            % messages to our caller, since any such messages should already
            % have been gathered during the initial mode analysis pass.
        ),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
                io.format(ProgressStream,
                    "%% Repeating switch detection for %s\n",
                    [s(pred_id_to_user_string(!.ModuleInfo, PredId))], !IO)
            )
        ;
            VeryVerbose = no
        ),

        module_info_pred_info(!.ModuleInfo, PredId, PredInfo2),
        pred_info_proc_info(PredInfo2, ProcId, ProcInfo2),

        SwitchDetectInfo = init_switch_detect_info(!.ModuleInfo),
        detect_switches_in_proc(SwitchDetectInfo, ProcInfo2, ProcInfo),

        pred_info_set_proc_info(ProcId, ProcInfo, PredInfo2, PredInfo3),
        module_info_set_pred_info(PredId, PredInfo3, !ModuleInfo),

        trace [io(!IO)] (
            get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
            maybe_report_stats(ProgressStream, Statistics, !IO)
        ),
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                get_progress_output_stream(!.ModuleInfo, ProgressStream, !IO),
                io.format(ProgressStream,
                    "%% Repeating common deconstruction detection for %s\n",
                    [s(pred_id_to_user_string(!.ModuleInfo, PredId))], !IO)
            )
        ;
            VeryVerbose = no
        ),
        disable_warning [suspicious_recursion] (
            detect_cse_in_proc(PredId, ProcId, !ModuleInfo)
        )
    ).

:- type cse_info
    --->    cse_info(
                csei_module_info        :: module_info,
                csei_var_table          :: var_table,
                csei_rtti_varmaps       :: rtti_varmaps,
                csei_redo               :: bool,
                csei_nopull_contexts    :: list(prog_context)
            ).

:- pred detect_cse_in_proc_pass(module_info::in, bool::out,
    proc_info::in, proc_info::out) is det.

detect_cse_in_proc_pass(ModuleInfo, Redo, !ProcInfo) :-
    % To process each ProcInfo, we get the goal, initialize the instmap
    % based on the modes of the head vars, and pass these to
    % `detect_cse_in_goal'.
    proc_info_get_goal(!.ProcInfo, Goal0),
    proc_info_get_initial_instmap(ModuleInfo, !.ProcInfo, InstMap0),
    proc_info_get_var_table(!.ProcInfo, VarTable0),
    proc_info_get_rtti_varmaps(!.ProcInfo, RttiVarMaps0),
    Redo0 = no,
    CseInfo0 = cse_info(ModuleInfo, VarTable0, RttiVarMaps0, Redo0, []),
    detect_cse_in_goal(Goal0, Goal1, CseInfo0, CseInfo, InstMap0),
    CseInfo = cse_info(_, _, _, Redo, CseNoPullContexts),
    proc_info_get_cse_nopull_contexts(!.ProcInfo, NoPullContexts0),
    NoPullContexts = CseNoPullContexts ++ NoPullContexts0,
    proc_info_set_cse_nopull_contexts(NoPullContexts, !ProcInfo),
    (
        Redo = no
    ;
        Redo = yes,
        % ModuleInfo should not be changed by detect_cse_in_goal.
        CseInfo = cse_info(_, VarTable1, RttiVarMaps1, _, _),

        proc_info_get_headvars(!.ProcInfo, HeadVars),
        implicitly_quantify_clause_body_general_vt(
            ordinary_nonlocals_maybe_lambda, HeadVars, _Warnings, Goal1, Goal,
            VarTable1, VarTable, RttiVarMaps1, RttiVarMaps),

        proc_info_set_goal(Goal, !ProcInfo),
        proc_info_set_var_table(VarTable, !ProcInfo),
        proc_info_set_rtti_varmaps(RttiVarMaps, !ProcInfo)
    ).

%---------------------------------------------------------------------------%

    % Given a goal, and the instmap on entry to that goal,
    % find disjunctions that contain common subexpressions
    % and hoist these out of the disjunction. At the moment
    % we only look for cses that are deconstruction unifications.
    %
:- pred detect_cse_in_goal(hlds_goal::in, hlds_goal::out,
    cse_info::in, cse_info::out, instmap::in) is det.

detect_cse_in_goal(Goal0, Goal, !CseInfo, InstMap0) :-
    detect_cse_in_goal_update_instmap(Goal0, Goal, !CseInfo,
        InstMap0, _InstMap).

    % This version is the same as the above except that it returns
    % the resulting instmap on exit from the goal, which is computed by
    % applying the instmap delta specified in the goal's goalinfo.
    %
:- pred detect_cse_in_goal_update_instmap(hlds_goal::in, hlds_goal::out,
    cse_info::in, cse_info::out, instmap::in, instmap::out) is det.

detect_cse_in_goal_update_instmap(Goal0, Goal, !CseInfo, InstMap0, InstMap) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = plain_call(_, _, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(LHS, RHS0, Mode, Unify,  UnifyContext),
        (
            RHS0 = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocalVars, VarsModes, Det, LambdaGoal0),
            ModuleInfo = !.CseInfo ^ csei_module_info,
            instmap.pre_lambda_update(ModuleInfo, VarsModes,
                InstMap0, InstMap1),
            detect_cse_in_goal(LambdaGoal0, LambdaGoal, !CseInfo, InstMap1),
            RHS = rhs_lambda_goal(Purity, Groundness, PredOrFunc, EvalMethod,
                NonLocalVars, VarsModes, Det, LambdaGoal)
        ;
            ( RHS0 = rhs_var(_)
            ; RHS0 = rhs_functor(_, _, _)
            ),
            RHS = RHS0
        ),
        GoalExpr = unify(LHS, RHS, Mode,Unify, UnifyContext)
    ;
        GoalExpr0 = negation(SubGoal0),
        detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        (
            Reason0 = from_ground_term(_, FGTReason),
            (
                FGTReason = from_ground_term_construct,
                % There are no deconstructions at all inside these scopes.
                GoalExpr = GoalExpr0
            ;
                FGTReason = from_ground_term_deconstruct,
                % We want to know whether the redo flag is set during the
                % processing of SubGoal0.
                OldRedo = !.CseInfo ^ csei_redo,
                !CseInfo ^ csei_redo := no,
                detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0),
                SubGoalRedo = !.CseInfo ^ csei_redo,
                !CseInfo ^ csei_redo := bool.or(OldRedo, SubGoalRedo),
                (
                    SubGoalRedo = no,
                    GoalExpr = scope(Reason0, SubGoal)
                ;
                    SubGoalRedo = yes,
                    % If we remove a goal from such a scope, what is left
                    % may no longer satisfy the invariants we expect it
                    % to satisfy.
                    SubGoal = hlds_goal(GoalExpr, _)
                )
            ;
                FGTReason = from_ground_term_other,
                detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0),
                GoalExpr = scope(Reason0, SubGoal)
            ;
                FGTReason = from_ground_term_initial,
                % Mode analysis should have replaced this kind of fgt scope
                % with one of the other kinds.
                unexpected($pred, "from_ground_term_initial")
            )
        ;
            Reason0 = require_switch_arms_detism(_, _),
            SubGoal0 = hlds_goal(SubGoalExpr0, SubGoalInfo0),
            ( if SubGoalExpr0 = switch(SwitchVar, CanFail, Cases0) then
                % If we find some common subexpressions in Cases0 and
                % pull them out of the switch, then the updated subgoal
                % of the scope will be a *conjunction* of the pulled-out
                % subexpressions and the modified switch. Simply checking that
                % each arm of the modified switch has the required determinism
                % is not enough, because it is possible for the determinism
                % of an arm to differ between the original and the modified
                % switch. For example, an original arm could consist of a
                % semidet unification and some det code; pulling the semidet
                % unification out of the arm would transform a switch arm
                % from one for which we want to generate an error and do,
                % to one for which we want to generate an error but don't.
                %
                % We could in theory fix this by modifying the code that
                % checks require_switch_arms_detism scopes to take into account
                % the possibility that we modified the switch, but it is
                % simpler not to modify such the arms of such switches at all.
                % Since require_switch_arms_detism scopes are rare, the impact
                % should be negligible in terms of both code size and speed.
                detect_cse_in_cases_arms(Cases0, Cases, !CseInfo, InstMap0),
                SubGoalExpr = switch(SwitchVar, CanFail, Cases),
                SubGoal = hlds_goal(SubGoalExpr, SubGoalInfo0),
                GoalExpr = scope(Reason0, SubGoal)
            else
                detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0),
                GoalExpr = scope(Reason0, SubGoal)
            )
        ;
            ( Reason0 = exist_quant(_)
            ; Reason0 = disable_warnings(_, _)
            ; Reason0 = barrier(_)
            ; Reason0 = commit(_)
            ; Reason0 = loop_control(_, _, _)
            ; Reason0 = promise_purity(_)
            ; Reason0 = promise_solutions(_, _)
            ; Reason0 = require_complete_switch(_)
            ; Reason0 = require_detism(_)
            ; Reason0 = trace_goal(_, _, _, _, _)
            ),
            detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0),
            GoalExpr = scope(Reason0, SubGoal)
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        detect_cse_in_conj(Goals0, Goals, !CseInfo, ConjType, InstMap0),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        (
            Goals0 = [],
            GoalExpr = disj([])
        ;
            Goals0 = [_ | _],
            NonLocals = goal_info_get_nonlocals(GoalInfo),
            NonLocalsList = set_of_var.to_sorted_list(NonLocals),
            detect_cse_in_disj(NonLocalsList, Goals0, GoalInfo,
                InstMap0, !CseInfo, GoalExpr)
        )
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        NonLocalsList = set_of_var.to_sorted_list(NonLocals),
        detect_cse_in_cases(NonLocalsList, Var, CanFail, Cases0, GoalInfo,
            InstMap0, !CseInfo, GoalExpr)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        NonLocals = goal_info_get_nonlocals(GoalInfo),
        NonLocalsList = set_of_var.to_sorted_list(NonLocals),
        detect_cse_in_ite(NonLocalsList, Vars, Cond0, Then0, Else0, GoalInfo,
            InstMap0, !CseInfo, GoalExpr)
    ;
        GoalExpr0 = shorthand(ShortHand0),
        (
            ShortHand0 = atomic_goal(AtomicGoalType, Outer, Inner,
                MaybeOutputVars, MainGoal0, OrElseGoals0, OrElseInners),
            detect_cse_in_goal(MainGoal0, MainGoal, !CseInfo, InstMap0),
            detect_cse_in_independent_goals(OrElseGoals0, OrElseGoals,
                !CseInfo, InstMap0),
            ShortHand = atomic_goal(AtomicGoalType, Outer, Inner,
                MaybeOutputVars, MainGoal, OrElseGoals, OrElseInners)
        ;
            ShortHand0 = bi_implication(_, _),
            % These should have been expanded out by now.
            unexpected($pred, "bi_implication")
        ;
            ShortHand0 = try_goal(MaybeIO, ResultVar, SubGoal0),
            % XXX not sure about this as SubGoal0 is not in its final form.
            % Also, mightn't the try "Goal" part get hoisted out?
            detect_cse_in_goal(SubGoal0, SubGoal, !CseInfo, InstMap0),
            ShortHand = try_goal(MaybeIO, ResultVar, SubGoal)
        ),
        GoalExpr = shorthand(ShortHand)
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    InstMapDelta = goal_info_get_instmap_delta(GoalInfo),
    apply_instmap_delta(InstMapDelta, InstMap0, InstMap).

%---------------------------------------------------------------------------%

:- pred detect_cse_in_conj(list(hlds_goal)::in, list(hlds_goal)::out,
    cse_info::in, cse_info::out, conj_type::in, instmap::in) is det.

detect_cse_in_conj([], [], !CseInfo, _ConjType, _InstMap).
detect_cse_in_conj([Goal0 | Goals0], Goals, !CseInfo, ConjType, !.InstMap) :-
    detect_cse_in_goal_update_instmap(Goal0, Goal, !CseInfo, !InstMap),
    detect_cse_in_conj(Goals0, TailGoals, !CseInfo, ConjType, !.InstMap),
    % Flatten any non-flat conjunctions we create.
    ( if
        Goal = hlds_goal(conj(InnerConjType, ConjGoals), _),
        ConjType = InnerConjType
    then
        Goals = ConjGoals ++ TailGoals
    else
        Goals = [Goal | TailGoals]
    ).

%---------------------------------------------------------------------------%
%
% We have found a non-empty branched structure, and we have a list
% of the nonlocal variables of that structure. For each nonlocal variable,
% we check whether each branch matches that variable against
% the same functor.
%

:- pred detect_cse_in_disj(list(prog_var)::in, list(hlds_goal)::in,
    hlds_goal_info::in, instmap::in, cse_info::in, cse_info::out,
    hlds_goal_expr::out) is det.

detect_cse_in_disj([], Goals0, _GoalInfo0, InstMap0, !CseInfo, GoalExpr) :-
    % We get here only if we couldn't pull any common unifications
    % out of two or more of the disjuncts represented by Goals0.
    % In that case, we look for transformation opportunities inside
    % *each* disjunct.
    detect_cse_in_independent_goals(Goals0, Goals, !CseInfo, InstMap0),
    GoalExpr = disj(Goals).
detect_cse_in_disj([Var | Vars], Goals0, GoalInfo0, InstMap0,
        !CseInfo, GoalExpr) :-
    CseInfo0 = !.CseInfo,
    ( if
        common_deconstruct(Goals0, Var, !CseInfo, UnifyGoal, ConsId,
            FirstOldNew, LaterOldNew, Goals)
    then
        instmap_lookup_var(InstMap0, Var, VarInst0),
        ( if may_pull_lhs_inst_cons_id(!.CseInfo, VarInst0, ConsId) then
            maybe_update_existential_data_structures(UnifyGoal,
                FirstOldNew, LaterOldNew, !CseInfo),
            GoalExpr = conj(plain_conj,
                [UnifyGoal, hlds_goal(disj(Goals), GoalInfo0)]),
            !CseInfo ^ csei_redo := yes
        else
            % Throw away any changes made by common_deconstruct above.
            !:CseInfo = CseInfo0,
            % Record the fact that we *could* have pulled a deconstruction
            % out of two or more arms *if* uniqueness in the inst of the
            % variable concerned didn't stop us.
            record_pull_decline(UnifyGoal, !CseInfo),
            detect_cse_in_disj(Vars, Goals0, GoalInfo0, InstMap0,
                !CseInfo, GoalExpr)
        )
    else
        detect_cse_in_disj(Vars, Goals0, GoalInfo0, InstMap0,
            !CseInfo, GoalExpr)
    ).

:- pred detect_cse_in_independent_goals(
    list(hlds_goal)::in, list(hlds_goal)::out,
    cse_info::in, cse_info::out, instmap::in) is det.

detect_cse_in_independent_goals([], [], !CseInfo, _).
detect_cse_in_independent_goals([Goal0 | Goals0], [Goal | Goals], !CseInfo,
        InstMap0) :-
    detect_cse_in_goal(Goal0, Goal, !CseInfo, InstMap0),
    detect_cse_in_independent_goals(Goals0, Goals, !CseInfo, InstMap0).

:- pred detect_cse_in_cases(list(prog_var)::in, prog_var::in, can_fail::in,
    list(case)::in, hlds_goal_info::in, instmap::in,
    cse_info::in, cse_info::out, hlds_goal_expr::out) is det.

detect_cse_in_cases([], SwitchVar, CanFail, Cases0, _GoalInfo,
        InstMap0, !CseInfo, GoalExpr) :-
    % We get here only if we couldn't pull any common unifications
    % out of two or more of the switch arms represented by Cases0.
    % In that case, we look for transformation opportunities inside
    % *each* switch arm.
    detect_cse_in_cases_arms(Cases0, Cases, !CseInfo, InstMap0),
    GoalExpr = switch(SwitchVar, CanFail, Cases).
detect_cse_in_cases([Var | Vars], SwitchVar, CanFail, Cases0, GoalInfo,
        InstMap0, !CseInfo, GoalExpr) :-
    CseInfo0 = !.CseInfo,
    ( if
        Var \= SwitchVar,
        common_deconstruct_cases(Cases0, Var, !CseInfo, UnifyGoal, ConsId,
            FirstOldNew, LaterOldNew, Cases)
    then
        instmap_lookup_var(InstMap0, Var, VarInst0),
        ( if may_pull_lhs_inst_cons_id(!.CseInfo, VarInst0, ConsId) then
            maybe_update_existential_data_structures(UnifyGoal,
                FirstOldNew, LaterOldNew, !CseInfo),
            SwitchGoalExpr = switch(SwitchVar, CanFail, Cases),
            SwitchGoal = hlds_goal(SwitchGoalExpr, GoalInfo),
            GoalExpr = conj(plain_conj, [UnifyGoal, SwitchGoal]),
            !CseInfo ^ csei_redo := yes
        else
            % Throw away any changes made by common_deconstruct above.
            !:CseInfo = CseInfo0,
            % Record the fact that we *could* have pulled a deconstruction
            % out of two or more arms *if* uniqueness in the inst of the
            % variable concerned didn't stop us.
            record_pull_decline(UnifyGoal, !CseInfo),
            detect_cse_in_cases(Vars, SwitchVar, CanFail, Cases0, GoalInfo,
                InstMap0, !CseInfo, GoalExpr)
        )
    else
        detect_cse_in_cases(Vars, SwitchVar, CanFail, Cases0, GoalInfo,
            InstMap0, !CseInfo, GoalExpr)
    ).

:- pred detect_cse_in_cases_arms(list(case)::in, list(case)::out,
    cse_info::in, cse_info::out, instmap::in) is det.

detect_cse_in_cases_arms([], [], !CseInfo, _).
detect_cse_in_cases_arms([Case0 | Cases0], [Case | Cases], !CseInfo,
        InstMap0) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    detect_cse_in_goal(Goal0, Goal, !CseInfo, InstMap0),
    Case = case(MainConsId, OtherConsIds, Goal),
    detect_cse_in_cases_arms(Cases0, Cases, !CseInfo, InstMap0).

:- pred detect_cse_in_ite(list(prog_var)::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_info::in,
    instmap::in, cse_info::in, cse_info::out, hlds_goal_expr::out) is det.

detect_cse_in_ite([], IfVars, Cond0, Then0, Else0, _, InstMap0, !CseInfo,
        GoalExpr) :-
    % We get here only if we couldn't pull any common unifications
    % out of both arms of the if-then-else. In that case, we look for
    % transformation opportunities inside *each* arm.
    detect_cse_in_ite_arms(Cond0, Cond, Then0, Then, Else0, Else, !CseInfo,
        InstMap0),
    GoalExpr = if_then_else(IfVars, Cond, Then, Else).
detect_cse_in_ite([Var | Vars], IfVars, Cond0, Then0, Else0, GoalInfo,
        InstMap0, !CseInfo, GoalExpr) :-
    CseInfo0 = !.CseInfo,
    ( if
        common_deconstruct([Then0, Else0], Var, !CseInfo, UnifyGoal, ConsId,
            FirstOldNew, LaterOldNew, Goals)
    then
        ( if Goals = [Then1, Else1] then
            Then = Then1,
            Else = Else1
        else
            unexpected($pred, "common_deconstruct changes number of goals")
        ),
        instmap_lookup_var(InstMap0, Var, VarInst0),
        ( if may_pull_lhs_inst_cons_id(!.CseInfo, VarInst0, ConsId) then
            maybe_update_existential_data_structures(UnifyGoal,
                FirstOldNew, LaterOldNew, !CseInfo),
            IfGoalExpr = if_then_else(IfVars, Cond0, Then, Else),
            IfGoal = hlds_goal(IfGoalExpr, GoalInfo),
            GoalExpr = conj(plain_conj, [UnifyGoal, IfGoal]),
            !CseInfo ^ csei_redo := yes
        else
            % Throw away any changes made by common_deconstruct above.
            !:CseInfo = CseInfo0,
            % Record the fact that we *could* have pulled a deconstruction
            % out of two or more arms *if* uniqueness in the inst of the
            % variable concerned didn't stop us.
            record_pull_decline(UnifyGoal, !CseInfo),
            detect_cse_in_ite(Vars, IfVars, Cond0, Then0, Else0, GoalInfo,
                InstMap0, !CseInfo, GoalExpr)
        )
    else
        detect_cse_in_ite(Vars, IfVars, Cond0, Then0, Else0, GoalInfo,
            InstMap0, !CseInfo, GoalExpr)
    ).

:- pred detect_cse_in_ite_arms(hlds_goal::in, hlds_goal::out,
    hlds_goal::in, hlds_goal::out, hlds_goal::in, hlds_goal::out,
    cse_info::in, cse_info::out, instmap::in) is det.

detect_cse_in_ite_arms(Cond0, Cond, Then0, Then, Else0, Else, !CseInfo,
        InstMap0) :-
    detect_cse_in_goal_update_instmap(Cond0, Cond, !CseInfo,
        InstMap0, InstMap1),
    detect_cse_in_goal(Then0, Then, !CseInfo, InstMap1),
    detect_cse_in_goal(Else0, Else, !CseInfo, InstMap0).

%---------------------------------------------------------------------------%

:- pred record_pull_decline(hlds_goal::in, cse_info::in, cse_info::out) is det.

record_pull_decline(UnifyGoal, !CseInfo) :-
    UnifyGoal = hlds_goal(_, UnifyGoalInfo),
    Context = goal_info_get_context(UnifyGoalInfo),
    NoPullContexts0 = !.CseInfo ^ csei_nopull_contexts,
    NoPullContexts = [Context | NoPullContexts0],
    !CseInfo ^ csei_nopull_contexts := NoPullContexts.

%---------------------------------------------------------------------------%

    % common_deconstruct(Goals0, Var, !CseInfo, Unify, ConsId,
    %   FirstOldNew, LaterOldNew, Goals):
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
    %   Unify is the unification goal that was hoisted out; it is guaranteed
    %   to be a deconstruction unification with the right hand side being an
    %   rhs_functor naming ConsId as the function symbol.
    %   FirstOldNew and LaterOldNew give the mapping from argument variables
    %   in the old unification in the first and later branches respectively
    %   to the freshly created argument variables in Unify.
    %
:- pred common_deconstruct(list(hlds_goal)::in, prog_var::in,
    cse_info::in, cse_info::out, hlds_goal::out, cons_id::out,
    assoc_list(prog_var)::out, list(assoc_list(prog_var))::out,
    list(hlds_goal)::out) is semidet.

common_deconstruct(Goals0, Var, !CseInfo, Unify, ConsId,
        FirstOldNew, LaterOldNew, Goals) :-
    CseState0 = before_candidate,
    common_deconstruct_2(Goals0, Var, CseState0, CseState,
        !CseInfo, Goals),
    CseState = have_candidate(Unify, ConsId, FirstOldNew, LaterOldNew),
    LaterOldNew = [_ | _].

:- pred common_deconstruct_2(list(hlds_goal)::in, prog_var::in,
    cse_state::in, cse_state::out, cse_info::in, cse_info::out,
    list(hlds_goal)::out) is semidet.

common_deconstruct_2([], _Var, !CseState, !CseInfo, []).
common_deconstruct_2([Goal0 | Goals0], Var, !CseState, !CseInfo,
        [Goal | Goals]) :-
    find_bind_var(Var, find_bind_var_for_cse_in_deconstruct,
        Goal0, Goal, !CseState, !CseInfo, did_find_deconstruct),
    !.CseState = have_candidate(_, _, _, _),
    common_deconstruct_2(Goals0, Var, !CseState, !CseInfo, Goals).

%---------------------------------------------------------------------------%

:- pred common_deconstruct_cases(list(case)::in, prog_var::in,
    cse_info::in, cse_info::out, hlds_goal::out, cons_id::out,
    assoc_list(prog_var)::out, list(assoc_list(prog_var))::out,
    list(case)::out) is semidet.

common_deconstruct_cases(Cases0, Var, !CseInfo, Unify, ConsId,
        FirstOldNew, LaterOldNew, Cases) :-
    CseState0 = before_candidate,
    common_deconstruct_cases_2(Cases0, Var, CseState0, CseState,
        !CseInfo, Cases),
    CseState = have_candidate(Unify, ConsId, FirstOldNew, LaterOldNew),
    LaterOldNew = [_ | _].

:- pred common_deconstruct_cases_2(list(case)::in, prog_var::in,
    cse_state::in, cse_state::out, cse_info::in, cse_info::out,
    list(case)::out) is semidet.

common_deconstruct_cases_2([], _Var, !CseState, !CseInfo, []).
common_deconstruct_cases_2([Case0 | Cases0], Var, !CseState, !CseInfo,
        [Case | Cases]) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    find_bind_var(Var, find_bind_var_for_cse_in_deconstruct,
        Goal0, Goal, !CseState, !CseInfo, did_find_deconstruct),
    Case = case(MainConsId, OtherConsIds, Goal),
    !.CseState = have_candidate(_, _, _, _),
    common_deconstruct_cases_2(Cases0, Var, !CseState, !CseInfo, Cases).

%---------------------------------------------------------------------------%

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
    % being hoisted before the branched control structure. The later_old_new
    % field contains the same information for the second and later branches.
:- type cse_state
    --->    before_candidate
    ;       have_candidate(
                hc_goal             ::  hlds_goal,
                hc_cons_id          ::  cons_id,
                hc_first_old_new    ::  assoc_list(prog_var),
                hc_later_old_new    ::  list(assoc_list(prog_var))
            )
    ;       multiple_candidates.

:- pred find_bind_var_for_cse_in_deconstruct(prog_var::in, hlds_goal::in,
    list(hlds_goal)::out, cse_state::in, cse_state::out,
    cse_info::in, cse_info::out) is det.

find_bind_var_for_cse_in_deconstruct(Var, Goal0, Goals,
        !CseState, !CseInfo) :-
    (
        !.CseState = before_candidate,
        construct_common_unify(Var, Goal0, !CseInfo, ConsId, OldNewVars,
            HoistedGoal, Goals),
        !:CseState = have_candidate(HoistedGoal, ConsId, OldNewVars, [])
    ;
        !.CseState = have_candidate(HoistedGoal, ConsId,
            FirstOldNewVars, LaterOldNewVars0),
        Goal0 = hlds_goal(_, GoalInfo),
        Context = goal_info_get_context(GoalInfo),
        ( if
            find_similar_deconstruct(HoistedGoal,
                Goal0, Context, OldNewVars, Goals0)
        then
            Goals = Goals0,
            LaterOldNewVars = [OldNewVars | LaterOldNewVars0],
            !:CseState = have_candidate(HoistedGoal, ConsId,
                FirstOldNewVars, LaterOldNewVars)
        else
            Goals = [Goal0],
            !:CseState = multiple_candidates
        )
    ;
        !.CseState = multiple_candidates,
        Goals = [Goal0],
        !:CseState = multiple_candidates
    ).

:- pred construct_common_unify(prog_var::in, hlds_goal::in,
    cse_info::in, cse_info::out, cons_id::out, assoc_list(prog_var)::out,
    hlds_goal::out, list(hlds_goal)::out) is det.

construct_common_unify(Var, Goal0, !CseInfo, ConsId, OldNewVars, HoistedGoal,
        ReplacementGoals) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ( if
        GoalExpr0 = unify(_, RHS, Umode, Unif0, Ucontext),
        Unif0 = deconstruct(_, ConsId0, ArgVars, Submodes, CanFail, CanCGC)
    then
        ConsId = ConsId0,
        Unif = deconstruct(Var, ConsId, ArgVars, Submodes, CanFail, CanCGC),
        (
            RHS = rhs_functor(_, _, _),
            GoalExpr1 = unify(Var, RHS, Umode, Unif, Ucontext)
        ;
            ( RHS = rhs_var(_)
            ; RHS = rhs_lambda_goal(_, _, _, _, _, _, _, _)
            ),
            unexpected($pred, "non-functor unify")
        ),
        goal_info_add_feature(feature_lifted_by_cse, GoalInfo0, GoalInfo1),
        Context = goal_info_get_context(GoalInfo1),
        create_new_arg_vars(ArgVars, Context, Ucontext, !CseInfo,
            OldNewVars, ReplacementGoals),
        map.from_assoc_list(OldNewVars, Subn),
        rename_some_vars_in_goal(Subn, hlds_goal(GoalExpr1, GoalInfo1),
            HoistedGoal)
    else
        unexpected($pred, "non-unify goal")
    ).

:- pred create_new_arg_vars(list(prog_var)::in, prog_context::in,
    unify_context::in, cse_info::in, cse_info::out,
    assoc_list(prog_var)::out, list(hlds_goal)::out) is det.

create_new_arg_vars([], _, _, !CseInfo, [], []).
create_new_arg_vars([HeadOldArgVar | TailOldArgVars], Context,
        UnifyContext, !CseInfo, !:OldNewVars, ReplacementGoals) :-
    create_new_arg_vars(TailOldArgVars, Context, UnifyContext, !CseInfo,
        !:OldNewVars, TailReplacementGoals),
    create_new_arg_var(HeadOldArgVar, Context, UnifyContext, !CseInfo,
        !OldNewVars, HeadReplacementGoal),
    ReplacementGoals = [HeadReplacementGoal | TailReplacementGoals].

:- pred create_new_arg_var(prog_var::in, prog_context::in,
    unify_context::in, cse_info::in, cse_info::out,
    assoc_list(prog_var)::in, assoc_list(prog_var)::out,
    hlds_goal::out) is det.

create_new_arg_var(OldArgVar, Context, UnifyContext, !CseInfo, !OldNewVars,
        Goal) :-
    % If OldArgVar was a type_info, typeclass_info or a component of either,
    % and it was named, then we need to preserve the fact that it is named.
    % The reason is documented by the comment before delay_death_proc_body
    % in liveness.m.
    %
    % The reason why we don't preserve the name of all fields that have names
    % is that taking the name of an argument variable from one branch
    % and using it as the name of the argument variable in the hoisted-out
    % goal makes that name visible in the other branches as well, and in
    % THOSE branches, it can be misleading.
    %
    % This is illustrated by the merge predicate in tests/debugger/dice.m:
    %
    % merge([], [], []).
    % merge([S | Ss], [], [S | Ss]).
    % merge([], [S | Ss], [S | Ss]).
    % merge([A | As], [B | Bs], [C | Cs]) :-
    %   ( if A =< B then
    %       dice.merge(As, [B | Bs], Cs),
    %       C = A
    %   else
    %       dice.merge(As, [B | Bs], Cs), % BUG
    %       C = B
    %   ).
    %
    % When printing the variables live at some point in the if-then-else,
    % programmers shouldn't be surprised by the debugger telling them
    % about a live variable named "S" or "Ss", when the names of those fields
    % at that point in the code are actually "A" and "As".

    VarTable0 = !.CseInfo ^ csei_var_table,
    lookup_var_entry(VarTable0, OldArgVar, OldArgVarEntry),
    OldArgVarEntry = vte(OldArgVarName, OldArgVarType, OldArgVarIsDummy),
    ModuleInfo = !.CseInfo ^ csei_module_info,
    TypeCat = classify_type(ModuleInfo, OldArgVarType),
    ( if
        TypeCat = ctor_cat_system(_),
        OldArgVarName \= ""
    then
        NewArgVarName = OldArgVarName
    else
        NewArgVarName = ""
    ),
    NewArgVarEntry = vte(NewArgVarName, OldArgVarType, OldArgVarIsDummy),
    add_var_entry(NewArgVarEntry, NewArgVar, VarTable0, VarTable),
    !:OldNewVars = [OldArgVar - NewArgVar | !.OldNewVars],
    UnifyContext = unify_context(MainCtxt, SubCtxt),
    % It is ok to create complicated unifications here, because we rerun
    % mode analysis on the resulting goal. It would be nicer to generate
    % the right assignment unification directly, but that would require keeping
    % track of the inst of OldArgVar.
    create_pure_atomic_complicated_unification(OldArgVar, rhs_var(NewArgVar),
        Context, MainCtxt, SubCtxt, Goal),
    !CseInfo ^ csei_var_table := VarTable.

%---------------------------------------------------------------------------%

:- pred find_similar_deconstruct(hlds_goal::in, hlds_goal::in,
    prog_context::in, assoc_list(prog_var)::out, list(hlds_goal)::out)
    is semidet.

find_similar_deconstruct(HoistedUnifyGoal, OldUnifyGoal, Context,
        OldHoistedVars, Replacements) :-
    ( if
        HoistedUnifyGoal = hlds_goal(unify(_, _, _, HoistedUnifyInfo, OC), _),
        HoistedUnifyInfo = deconstruct(_, HoistedFunctor,
            HoistedVars, _, _, _),
        OldUnifyGoal = hlds_goal(unify(_, _, _, OldUnifyInfo, _NC), _),
        OldUnifyInfo = deconstruct(_, OldFunctor, OldVars, _, _, _)
    then
        HoistedFunctor = OldFunctor,
        list.length(HoistedVars, HoistedVarsCount),
        list.length(OldVars, OldVarsCount),
        HoistedVarsCount = OldVarsCount,
        assoc_list.from_corresponding_lists(OldVars, HoistedVars,
            OldHoistedVars),
        pair_subterms(OldHoistedVars, Context, OC, Replacements)
    else
        unexpected($pred, "non-deconstruct unify")
    ).

:- pred pair_subterms(assoc_list(prog_var)::in, prog_context::in,
    unify_context::in, list(hlds_goal)::out) is det.

pair_subterms([], _Context, _UnifyContext, []).
pair_subterms([OldVar - HoistedVar | OldHoistedVars], Context, UnifyContext,
        Replacements) :-
    pair_subterms(OldHoistedVars, Context, UnifyContext, Replacements1),
    ( if OldVar = HoistedVar then
        Replacements = Replacements1
    else
        UnifyContext = unify_context(MainCtxt, SubCtxt),
        % It is ok to create complicated unifications here, because we rerun
        % mode analysis on the resulting goal. It would be nicer to generate
        % the right assignment unification directly, but that would require
        % keeping track of the inst of OldVar.
        create_pure_atomic_complicated_unification(HoistedVar, rhs_var(OldVar),
            Context, MainCtxt, SubCtxt, Goal),
        Replacements = [Goal | Replacements1]
    ).

%---------------------------------------------------------------------------%
%
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
%       HeadVar.g2_2 = x.u(TypeClassInfo_for_v_8, V_4),
%       ...
%   ;
%       HeadVar.g2_2 = x.u(TypeClassInfo_for_v_14, V_6)
%       ...
%   )
%
% The main part of cse_detection will replace this with
%
%   HeadVar.g2_2 = x.u(V_17, V_16)
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
%

:- pred maybe_update_existential_data_structures(hlds_goal::in,
    assoc_list(prog_var)::in, list(assoc_list(prog_var))::in,
    cse_info::in, cse_info::out) is det.

maybe_update_existential_data_structures(UnifyGoal, FirstOldNew, LaterOldNew,
        !CseInfo) :-
    ( if
        UnifyGoal = hlds_goal(unify(_, _, _, UnifyInfo, _), _),
        UnifyInfo = deconstruct(Var, ConsId, _, _, _, _),
        ModuleInfo = !.CseInfo ^ csei_module_info,
        VarTable = !.CseInfo ^ csei_var_table,
        lookup_var_type(VarTable, Var, Type),
        cons_id_is_existq_cons(ModuleInfo, Type, ConsId)
    then
        update_existential_data_structures(FirstOldNew, LaterOldNew, !CseInfo)
    else
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
    VarTable0 = !.CseInfo ^ csei_var_table,

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
    apply_variable_renaming_to_var_table(Renaming, VarTable0, VarTable),

    !CseInfo ^ csei_rtti_varmaps := RttiVarMaps,
    !CseInfo ^ csei_var_table := VarTable.

:- pred find_type_info_locn_tvar_map(rtti_varmaps::in,
    map(prog_var, prog_var)::in, tvar::in,
    map(type_info_locn, tvar)::in, map(type_info_locn, tvar)::out) is det.

find_type_info_locn_tvar_map(RttiVarMaps, FirstOldNewMap, Tvar, !NewTvarMap) :-
    rtti_lookup_type_info_locn(RttiVarMaps, Tvar, TypeInfoLocn0),
    type_info_locn_var(TypeInfoLocn0, Old),
    ( if map.search(FirstOldNewMap, Old, New) then
        type_info_locn_set_var(New, TypeInfoLocn0, TypeInfoLocn),
        map.det_insert(TypeInfoLocn, Tvar, !NewTvarMap)
    else
        true
    ).

:- pred find_merged_tvars(rtti_varmaps::in, map(prog_var, prog_var)::in,
    map(type_info_locn, tvar)::in, tvar::in,
    tvar_renaming::in, tvar_renaming::out) is det.

find_merged_tvars(RttiVarMaps, LaterOldNewMap, NewTvarMap, Tvar, !Renaming) :-
    rtti_lookup_type_info_locn(RttiVarMaps, Tvar, TypeInfoLocn0),
    type_info_locn_var(TypeInfoLocn0, Old),
    ( if map.search(LaterOldNewMap, Old, New) then
        type_info_locn_set_var(New, TypeInfoLocn0, TypeInfoLocn),
        map.lookup(NewTvarMap, TypeInfoLocn, NewTvar),
        ( if NewTvar = Tvar then
            true
        else
            map.det_insert(Tvar, NewTvar, !Renaming)
        )
    else
        true
    ).

%---------------------------------------------------------------------------%

    % May we pull a deconstruction unification whose left hand side
    % variable has this inst out of two or more arms, to put before
    % the disjunction, switch or if-then-else?
    %
:- pred may_pull_lhs_inst_cons_id(cse_info::in, mer_inst::in, cons_id::in)
    is semidet.

may_pull_lhs_inst_cons_id(CseInfo, VarInst, ConsId) :-
    ModuleInfo = CseInfo ^ csei_module_info,
    % XXX We only need inst_is_bound, but leave this as it is until
    % mode analysis can handle aliasing between free variables.
    inst_is_ground_or_any(ModuleInfo, VarInst),

    % We need to test for the absence of uniqueness until we can track
    % uniqueness through the extra unifications we introduce when we pull
    % a deconstruction out of an arm of a disjunction, switch or if-then-else.
    %
    % We only need the insts of the *arguments* to be free of uniqueness.
    % However, the vast majority of the time, the whole inst is free
    % of uniqueness, so for efficiency in the common case, we test that first.
    ( if inst_is_not_partly_unique(ModuleInfo, VarInst) then
        true
    else
        inst_is_bound_to_functors(ModuleInfo, VarInst, FunctorBoundInsts),
        compute_may_pull_cons_id(ModuleInfo, FunctorBoundInsts, ConsId,
            MayPullConsId),
        MayPullConsId = may_pull_cons_id
    ).

:- type may_pull_cons_id
    --->    may_pull_cons_id
    ;       may_not_pull_cons_id.

:- pred compute_may_pull_cons_id(module_info::in, list(bound_inst)::in,
    cons_id::in, may_pull_cons_id::out) is det.

compute_may_pull_cons_id(_ModuleInfo, [], _ConsId, may_not_pull_cons_id).
compute_may_pull_cons_id(ModuleInfo, [BoundInst | BoundInsts],
        ConsId, MayPullConsId) :-
    BoundInst = bound_functor(InstConsId, ArgInsts),
    ( if
        (
            ConsId = InstConsId
        ;
            % While the type constructor in ConsId is filled in by the type
            % checker, the type constructor in InstConsId need not be.
            % This code tests whether the two cons_ids are the same
            % modulo this irrelevant possible difference.
            ConsId = cons(SymName, Arity, _),
            InstConsId = cons(InstSymName, InstArity, _),
            SymName = InstSymName,
            Arity = InstArity
        )
    then
        ( if
            list.all_true(inst_is_not_partly_unique(ModuleInfo), ArgInsts)
        then
            MayPullConsId = may_pull_cons_id
        else
            MayPullConsId = may_not_pull_cons_id
        )
    else
        compute_may_pull_cons_id(ModuleInfo, BoundInsts, ConsId, MayPullConsId)
    ).

%---------------------------------------------------------------------------%
:- end_module check_hlds.cse_detection.
%---------------------------------------------------------------------------%
