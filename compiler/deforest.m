%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 1999-2012 University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: deforest.m.
% Main author: stayl.
%
% Deforestation attempts to remove multiple traversals over data structures,
% and construction followed by immediate deconstruction of data structures.
% It does this by combining the bodies of pairs of called procedures in a
% conjunction where the top-level functor of one of the argument variables of
% the first called procedure is known at the end of some of the branches of
% the body of that procedure, and the second called procedure switches on that
% variable.
%
% The deforestation pass also inlines calls for which the top-level goal in
% the called procedure is a switch and the functor of the switched-on variable
% is known. This allows simplify.m to prune away the failing branches.
%
% The constraint propagation pass, which is called from the deforestation
% pass, transforms the code so that goals which could fail are executed as
% early as possible.
%
% For a more detailed description, see Simon Taylor's Honours thesis,
% available from the papers page of mercurylang.org.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.deforest.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred deforest_module(io.text_output_stream::in,
    module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.det_analysis.
:- import_module check_hlds.det_report.
:- import_module check_hlds.inst_test.
:- import_module check_hlds.modecheck_util.
:- import_module check_hlds.recompute_instmap_deltas.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module check_hlds.type_util.
:- import_module hlds.goal_form.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.optimization_options.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.error_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_detism.
:- import_module parse_tree.prog_type_subst.
:- import_module parse_tree.set_of_var.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.inlining.
:- import_module transform_hlds.pd_cost.
:- import_module transform_hlds.pd_debug.
:- import_module transform_hlds.pd_info.
:- import_module transform_hlds.pd_term.
:- import_module transform_hlds.pd_util.

:- import_module assoc_list.
:- import_module bool.
:- import_module getopt.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module univ.
:- import_module varset.

%-----------------------------------------------------------------------------%

deforest_module(ProgressStream, !ModuleInfo) :-
    proc_arg_info_init(ProcArgInfo0),
    type_to_univ(ProcArgInfo0, UnivProcArgInfo0),

    % Find out which arguments of each procedure are switched on at the top
    % level or are constructed in a way which is possibly deforestable.
    Task0 = update_module_cookie(get_branch_vars_proc_univ, UnivProcArgInfo0),
    process_valid_nonimported_procs_update(Task0, Task, !ModuleInfo),
    ( if
        Task = update_module_cookie(_, UnivProcArgInfo),
        univ_to_type(UnivProcArgInfo, ProcArgInfo1)
    then
        ProcArgInfo = ProcArgInfo1
    else
        unexpected($pred, "passes_aux stuffed up")
    ),

    % We process the module bottom-up to make estimation of the
    % cost improvement of new versions a little more accurate and
    % also to avoid redoing optimizations.
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    DepList = dependency_info_get_condensed_bottom_up_sccs(DepInfo),

    pd_info_init(ProgressStream, !.ModuleInfo, ProcArgInfo, PDInfo0),
    list.foldl(deforest_proc, DepList, PDInfo0, PDInfo),
    pd_info_get_module_info(PDInfo, !:ModuleInfo),
    module_info_clobber_dependency_info(!ModuleInfo),
    pd_info_get_versions(PDInfo, VersionIndex),

    map.keys(VersionIndex, Versions),

    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    PropConstraints = OptTuple ^ ot_prop_constraints,
    ( if
        PropConstraints = prop_constraints,
        Versions = [_ | _]
    then
        % We can sometimes improve efficiency by rerunning determinism
        % inference on the specialized versions after constraint propagation,
        % because some nondet predicates will have become semidet.
        list.foldl(reset_inferred_proc_determinism, Versions, !ModuleInfo),

        disable_det_warnings(_OptionsToRestore, Globals, NoWarnGlobals),
        module_info_set_globals(NoWarnGlobals, !ModuleInfo),
        determinism_pass(!ModuleInfo, Specs),
        module_info_set_globals(Globals, !ModuleInfo),

        FoundErrors = contains_errors(Globals, Specs),
        expect(unify(FoundErrors, no), $pred,
            "determinism errors after deforestation")
    else
        true
    ).

:- pred reset_inferred_proc_determinism(pred_proc_id::in,
    module_info::in, module_info::out) is det.

reset_inferred_proc_determinism(PredProcId, !ModuleInfo) :-
    module_info_pred_proc_info(!.ModuleInfo, PredProcId, PredInfo, ProcInfo0),
    proc_info_get_inferred_determinism(ProcInfo0, Detism0),
    determinism_components(Detism0, _, MaxSolns),
    (
        MaxSolns = at_most_many_cc
        % `cc_multi' or `cc_nondet' determinisms are never inferred,
        % so resetting the determinism would cause determinism errors.
    ;
        ( MaxSolns = at_most_zero
        ; MaxSolns = at_most_one
        ; MaxSolns = at_most_many
        ),
        proc_info_set_inferred_determinism(detism_erroneous,
            ProcInfo0, ProcInfo),
        module_info_set_pred_proc_info(PredProcId, PredInfo, ProcInfo,
            !ModuleInfo)
    ).

:- pred proc_arg_info_init(map(pred_proc_id, pd_proc_arg_info)::out) is det.

proc_arg_info_init(ProcArgInfo0) :-
    map.init(ProcArgInfo0).

:- pred get_branch_vars_proc_univ(pred_proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out,
    univ::in, univ::out) is det.

get_branch_vars_proc_univ(PredProcId, ProcInfo, ProcInfo,
        !ModuleInfo, UnivProcArgInfo0, UnivProcArgInfo) :-
    det_univ_to_type(UnivProcArgInfo0, ProcArgInfo0),
    pd_util.get_branch_vars_proc(PredProcId, ProcInfo, !ModuleInfo,
        ProcArgInfo0, ProcArgInfo),
    type_to_univ(ProcArgInfo, UnivProcArgInfo).

:- pred deforest_proc(pred_proc_id::in, pd_info::in, pd_info::out) is det.

deforest_proc(PredProcId, !PDInfo) :-
    deforest_proc_deltas(PredProcId, _, _, !PDInfo).

:- pred deforest_proc_deltas(pred_proc_id::in, int::out, int::out,
    pd_info::in, pd_info::out) is det.

deforest_proc_deltas(PredProcId, CostDelta, SizeDelta, !PDInfo) :-
    some [!ModuleInfo, !PredInfo, !ProcInfo, !Goal] (
        pd_info_get_progress_stream(!.PDInfo, ProgressStream),
        pd_info_get_module_info(!.PDInfo, !:ModuleInfo),
        trace [io(!IO)] (
            maybe_write_proc_progress_message(ProgressStream, !.ModuleInfo,
                "Deforesting", PredProcId, !IO)
        ),
        module_info_pred_proc_info(!.ModuleInfo, PredProcId,
            !:PredInfo, !:ProcInfo),
        pd_info_init_unfold_info(PredProcId, !.PredInfo, !.ProcInfo, !PDInfo),
        proc_info_get_goal(!.ProcInfo, !:Goal),

        % Inlining may have created some opportunities for simplification.
        module_info_get_globals(!.ModuleInfo, Globals),
        find_simplify_tasks(Globals, do_not_generate_warnings, SimplifyTasks),
        pd_util.pd_simplify_goal(SimplifyTasks, !Goal, !PDInfo),
        pd_util.propagate_constraints(!Goal, !PDInfo),
        trace [io(!IO)] (
            pd_debug_output_goal(!.PDInfo, "after constraints\n", !.Goal, !IO)
        ),
        deforest_goal(!Goal, !PDInfo),
        pd_info_get_proc_info(!.PDInfo, !:ProcInfo),
        proc_info_set_goal(!.Goal, !ProcInfo),
        pd_info_get_changed(!.PDInfo, Changed),

        (
            Changed = yes,
            pd_info_get_module_info(!.PDInfo, !:ModuleInfo),
            requantify_proc_general(ord_nl_no_lambda, !ProcInfo),
            proc_info_get_goal(!.ProcInfo, !:Goal),
            proc_info_get_initial_instmap(!.ModuleInfo, !.ProcInfo, InstMap0),
            proc_info_get_var_table(!.ProcInfo, VarTable),
            proc_info_get_inst_varset(!.ProcInfo, InstVarSet),
            recompute_instmap_delta(recomp_atomics, VarTable, InstVarSet,
                InstMap0, !Goal, !ModuleInfo),
            pd_info_set_module_info(!.ModuleInfo, !PDInfo),
            pd_info_get_pred_info(!.PDInfo, !:PredInfo),
            proc_info_set_goal(!.Goal, !ProcInfo),
            module_info_set_pred_proc_info(PredProcId,
                !.PredInfo, !.ProcInfo, !ModuleInfo),
            pd_info_get_rerun_det(!.PDInfo, RerunDet),

            (
                RerunDet = yes,
                PredProcId = proc(PredId, ProcId),
                % If the determinism of some sub-goals has changed,
                % then we re-run determinism analysis. As with inlining.m,
                % this avoids problems with inlining erroneous procedures.
                det_infer_proc_ignore_msgs(PredId, ProcId, !ModuleInfo)
            ;
                RerunDet = no
            ),

            % Recompute the branch_info for the procedure.
            pd_info_get_proc_arg_info(!.PDInfo, ProcArgInfo0),
            pd_util.get_branch_vars_proc(PredProcId, !.ProcInfo,
                !ModuleInfo, ProcArgInfo0, ProcArgInfo),
            pd_info_set_proc_arg_info(ProcArgInfo, !PDInfo),
            pd_info_set_module_info(!.ModuleInfo, !PDInfo)
        ;
            Changed = no,
            pd_info_get_module_info(!.PDInfo, !:ModuleInfo),
            pd_info_get_pred_info(!.PDInfo, !:PredInfo),
            module_info_set_pred_proc_info(PredProcId, !.PredInfo, !.ProcInfo,
                !ModuleInfo),
            pd_info_set_module_info(!.ModuleInfo, !PDInfo)
        ),

        pd_info_get_module_info(!.PDInfo, !:ModuleInfo),
        trace [io(!IO)] (
            maybe_write_proc_progress_message(ProgressStream, !.ModuleInfo,
                "Finished deforesting", PredProcId, !IO)
        ),
        pd_info_get_cost_delta(!.PDInfo, CostDelta),
        pd_info_get_size_delta(!.PDInfo, SizeDelta),
        pd_info_unset_unfold_info(!PDInfo)
    ).

:- pred deforest_goal(hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

deforest_goal(Goal0, Goal, !PDInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    deforest_goal_expr(GoalExpr0, GoalExpr, GoalInfo0, GoalInfo, !PDInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred deforest_goal_expr(hlds_goal_expr::in, hlds_goal_expr::out,
    hlds_goal_info::in, hlds_goal_info::out, pd_info::in, pd_info::out) is det.

deforest_goal_expr(GoalExpr0, GoalExpr, !GoalInfo, !PDInfo) :-
    (
        GoalExpr0 = conj(ConjType, Goals0),
        some [!Goals] (
            !:Goals = Goals0,
            (
                ConjType = plain_conj,
                pd_info_get_instmap(!.PDInfo, InstMap0),
                partially_evaluate_conj_goals(!.Goals, [], !:Goals, !PDInfo),
                pd_info_set_instmap(InstMap0, !PDInfo),
                NonLocals = goal_info_get_nonlocals(!.GoalInfo),
                pd_info_get_module_info(!.PDInfo, ModuleInfo),
                module_info_get_globals(ModuleInfo, Globals),
                globals.get_opt_tuple(Globals, OptTuple),
                Deforest = OptTuple ^ ot_deforest,
                (
                    Deforest = deforest,
                    compute_goal_infos(!Goals, !PDInfo),
                    pd_info_set_instmap(InstMap0, !PDInfo),
                    deforest_conj(!.Goals, NonLocals, [], !:Goals, !PDInfo)
                ;
                    Deforest = do_not_deforest
                ),
                PropConstraints = OptTuple ^ ot_prop_constraints,
                pd_info_set_instmap(InstMap0, !PDInfo),
                (
                    PropConstraints = prop_constraints,
                    propagate_conj_constraints(!.Goals, NonLocals, [], !:Goals,
                        !PDInfo)
                ;
                    PropConstraints = do_not_prop_constraints
                ),
                pd_info_set_instmap(InstMap0, !PDInfo)
            ;
                ConjType = parallel_conj
                % XXX cannot deforest across parallel_conjunctions!
            ),
            Goals = !.Goals
        ),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        deforest_disj(Goals0, Goals, !PDInfo),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        pd_info_get_instmap(!.PDInfo, InstMap0),
        deforest_goal(Cond0, Cond, !PDInfo),
        pd_info_update_goal(Cond, !PDInfo),
        deforest_goal(Then0, Then, !PDInfo),
        pd_info_set_instmap(InstMap0, !PDInfo),
        deforest_goal(Else0, Else, !PDInfo),
        pd_info_set_instmap(InstMap0, !PDInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        deforest_cases(Var, Cases0, Cases, !PDInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = negation(SubGoal0),
        deforest_goal(SubGoal0, SubGoal, !PDInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            SubGoal = SubGoal0
        else
            deforest_goal(SubGoal0, SubGoal, !PDInfo)
        ),
        GoalExpr = scope(Reason, SubGoal)
    ;
        GoalExpr0 = plain_call(PredId, ProcId, Args, BuiltinState, _, Name),
        deforest_call(PredId, ProcId, Args, Name, BuiltinState,
            hlds_goal(GoalExpr0, !.GoalInfo), hlds_goal(GoalExpr, !:GoalInfo),
            !PDInfo)
    ;
        ( GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr = unify(_, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred deforest_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

deforest_disj([], [], !PDInfo).
deforest_disj([Goal0 | Goals0], [Goal | Goals], !PDInfo) :-
    pd_info_get_instmap(!.PDInfo, InstMap0),
    deforest_goal(Goal0, Goal, !PDInfo),
    pd_info_set_instmap(InstMap0, !PDInfo),
    deforest_disj(Goals0, Goals, !PDInfo).

%-----------------------------------------------------------------------------%

:- pred deforest_cases(prog_var::in, list(case)::in, list(case)::out,
    pd_info::in, pd_info::out) is det.

deforest_cases(_, [], [], !PDInfo).
deforest_cases(Var, [Case0 | Cases0], [Case | Cases], !PDInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    % Bind Var to MainConsId or one of the OtherConsIds in the instmap
    % before processing this case.
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_bind_var_to_functors(Var, MainConsId, OtherConsIds, !PDInfo),
    deforest_goal(Goal0, Goal, !PDInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    pd_info_set_instmap(InstMap0, !PDInfo),
    deforest_cases(Var, Cases0, Cases, !PDInfo).

%-----------------------------------------------------------------------------%

    % Perform partial evaluation on the goals of a conjunction.
    %
:- pred partially_evaluate_conj_goals(list(hlds_goal)::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

partially_evaluate_conj_goals([], RevGoals, Goals, !PDInfo) :-
    list.reverse(RevGoals, Goals).
partially_evaluate_conj_goals([Goal0 | Goals0], RevGoals0, Goals, !PDInfo) :-
    deforest_goal(Goal0, Goal1, !PDInfo),
    pd_info_update_goal(Goal1, !PDInfo),
    ( if Goal1 = hlds_goal(conj(plain_conj, Goals1), _) then
        list.reverse(Goals1, RevGoals1),
        list.append(RevGoals1, RevGoals0, RevGoals2)
    else
        RevGoals2 = [Goal1 | RevGoals0]
    ),
    partially_evaluate_conj_goals(Goals0, RevGoals2, Goals, !PDInfo).

%-----------------------------------------------------------------------------%

    % Compute the branch info for each goal in a conjunction.
    %
:- pred compute_goal_infos(list(hlds_goal)::in, annotated_conj::out,
    pd_info::in, pd_info::out) is det.

compute_goal_infos([], [], !PDInfo).
compute_goal_infos([Goal | Goals0], [Goal - MaybeBranchInfo | Goals],
        !PDInfo) :-
    deforest_get_branch_vars_goal(Goal, MaybeBranchInfo, !PDInfo),
    pd_info_update_goal(Goal, !PDInfo),
    compute_goal_infos(Goals0, Goals, !PDInfo).

:- pred deforest_get_branch_vars_goal(hlds_goal::in,
    maybe(pd_branch_info(prog_var))::out, pd_info::in, pd_info::out) is det.

deforest_get_branch_vars_goal(Goal, MaybeBranchInfo, !PDInfo) :-
    Goal = hlds_goal(GoalExpr, _),
    (
        ( GoalExpr = disj(_)
        ; GoalExpr = switch(_, _, _)
        ; GoalExpr = if_then_else(_, _, _, _)
        ),
        pd_util.get_branch_vars_goal(Goal, MaybeBranchInfo, !PDInfo)
    ;
        ( GoalExpr = unify(_, _, _, _, _)
        ; GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr = conj(_, _)
        ; GoalExpr = negation(_)
        ; GoalExpr = scope(_, _)
        ),
        MaybeBranchInfo = no
    ;
        GoalExpr = plain_call(PredId, ProcId, Args, _, _, _),
        pd_info_get_proc_arg_info(!.PDInfo, ProcBranchInfos),
        ( if
            map.search(ProcBranchInfos, proc(PredId, ProcId), BranchInfo0)
        then
            % Rename the branch_info for the called procedure
            % onto the argument variables.
            pd_util.convert_branch_info(BranchInfo0, Args, BranchInfo),
            MaybeBranchInfo = yes(BranchInfo)
        else
            MaybeBranchInfo = no
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

%-----------------------------------------------------------------------------%

:- pred propagate_conj_constraints(list(hlds_goal)::in,
    set_of_progvar::in, list(hlds_goal)::in, list(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

propagate_conj_constraints([], _, RevGoals, Goals, !PDInfo) :-
    list.reverse(RevGoals, Goals).
propagate_conj_constraints([Goal0 | Goals0], NonLocals, RevGoals0, Goals,
        !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    ( if
        % constraint.m ensures that only constraints relevant
        % to this goal are placed adjacent to it.
        Goal0 = hlds_goal(GoalExpr0, _GoalInfo0),
        GoalExpr0 = plain_call(PredId, _ProcId, _Args, _, _, SymName),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        not pred_info_is_imported(PredInfo),
        list.take_while(
            ( pred(CnstrGoal::in) is semidet :-
                CnstrGoal = hlds_goal(_, CnstrGoalInfo),
                goal_info_has_feature(CnstrGoalInfo, feature_constraint)
            ), Goals0, Constraints, Goals1),
        Constraints = [_ | _]
    then
        SymNameString = sym_name_to_string(SymName),
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "propagating constraints into call to %s\n",
                [s(SymNameString)], !IO)
        ),

        do_get_sub_conj_nonlocals(NonLocals, RevGoals0, [],
            Goal0, Constraints, no, [], Goals1, ConjNonLocals),
        call_call(ConjNonLocals, Goal0, Constraints, no, MaybeGoal, !PDInfo),
        (
            MaybeGoal = yes(Goal),
            pd_info_set_rerun_det(yes, !PDInfo),
            pd_info_update_goal(Goal, !PDInfo),
            propagate_conj_constraints(Goals1, NonLocals,
                [Goal | RevGoals0], Goals, !PDInfo)
        ;
            MaybeGoal = no,
            pd_info_update_goal(Goal0, !PDInfo),
            propagate_conj_constraints(Goals0, NonLocals,
                [Goal0 | RevGoals0], Goals, !PDInfo)
        )
    else
        pd_info_update_goal(Goal0, !PDInfo),
        propagate_conj_constraints(Goals0, NonLocals,
            [Goal0 | RevGoals0], Goals, !PDInfo)
    ).

%-----------------------------------------------------------------------------%

:- type annotated_conj ==
    assoc_list(hlds_goal, maybe(pd_branch_info(prog_var))).

:- pred deforest_conj(annotated_conj::in, set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

deforest_conj([], _, RevGoals, Goals, !PDInfo) :-
    list.reverse(RevGoals, Goals).
deforest_conj([Goal0 - MaybeBranchInfo | Goals0], NonLocals,
        RevGoals0, RevGoals, !PDInfo) :-
    ( if
        % Look for a goal later in the conjunction to deforest with.
        MaybeBranchInfo = yes(GoalBranchInfo),
        detect_deforestation(Goal0, GoalBranchInfo, Goals0, Goals1,
            DeforestInfo)
    then
        handle_deforestation(NonLocals, DeforestInfo,
            RevGoals0, RevGoals1, Goals1, Goals2, Optimized, !PDInfo),
        (
            Optimized = yes,
            deforest_conj(Goals2, NonLocals, RevGoals1, RevGoals, !PDInfo)
        ;
            Optimized = no,
            pd_info_update_goal(Goal0, !PDInfo),
            RevGoals2 = [Goal0 | RevGoals0],
            deforest_conj(Goals0, NonLocals, RevGoals2, RevGoals, !PDInfo)
        )
    else
        pd_info_update_goal(Goal0, !PDInfo),
        RevGoals1 = [Goal0 | RevGoals0],
        deforest_conj(Goals0, NonLocals, RevGoals1, RevGoals, !PDInfo)
    ).

%-----------------------------------------------------------------------------%

:- type deforest_info
    --->    deforest_info(
                hlds_goal,                  % earlier goal in conjunction
                pd_branch_info(prog_var),
                                            % branch_info for earlier goal
                list(hlds_goal),            % goals in between
                hlds_goal,                  % later goal in conjunction
                pd_branch_info(prog_var),
                                            % branch_info for later goal
                set(int)                    % branches for which there is
                                            % extra information about the
                                            % second goal, numbering starts
                                            % at 1.
            ).

    % Search backwards through the conjunction for the last goal which contains
    % extra information about the variable being switched on.
    %
:- pred detect_deforestation(hlds_goal::in,
    pd_branch_info(prog_var)::in, annotated_conj::in,
    annotated_conj::out, deforest_info::out) is semidet.

detect_deforestation(EarlierGoal, BranchInfo, !Goals, DeforestInfo) :-
    search_for_deforest_goal(EarlierGoal, BranchInfo, [], !Goals,
        DeforestInfo).

:- pred search_for_deforest_goal(hlds_goal::in,
    pd_branch_info(prog_var)::in, annotated_conj::in,
    annotated_conj::in, annotated_conj::out,
    deforest_info::out) is semidet.

search_for_deforest_goal(EarlierGoal, EarlierBranchInfo, RevBetweenGoals0,
        [Goal | Goals0], Goals, DeforestInfo) :-
    ( if
        Goal = LaterGoal - yes(LaterBranchInfo),
        potential_deforestation(EarlierBranchInfo,
            LaterBranchInfo, DeforestBranches)
    then
        list.reverse(RevBetweenGoals0, BetweenGoals1),
        assoc_list.keys(BetweenGoals1, BetweenGoals),
        Goals = Goals0,
        DeforestInfo = deforest_info(EarlierGoal, EarlierBranchInfo,
            BetweenGoals, LaterGoal, LaterBranchInfo, DeforestBranches)
    else
        search_for_deforest_goal(EarlierGoal, EarlierBranchInfo,
            [Goal | RevBetweenGoals0], Goals0, Goals, DeforestInfo)
    ).

    % Look for a variable in the second branch_info for which we have more
    % information in the first than in the instmap. Get the branches in the
    % first goal which contain this extra information.
    %
:- pred potential_deforestation(pd_branch_info(prog_var)::in,
    pd_branch_info(prog_var)::in, set(int)::out) is semidet.

potential_deforestation(Info1, Info2, DeforestBranches) :-
    Info1 = pd_branch_info(VarMap1, _, _),
    Info2 = pd_branch_info(_, LeftVars2, _),

    map.select(VarMap1, LeftVars2, VarMap),
    map.to_assoc_list(VarMap, VarAssoc),
    not map.is_empty(VarMap),

    % Work out which branches of the first goal should contain
    % unfolded versions of the second goal.
    GetBranches =
        ( pred(VarInfo::in, Branches0::in, Branches::out) is det :-
            VarInfo = _ - Branches1,
            set.union(Branches0, Branches1, Branches)
    ),
    set.init(DeforestBranches0),
    list.foldl(GetBranches, VarAssoc, DeforestBranches0, DeforestBranches).

%-----------------------------------------------------------------------------%

    % Take the part of a conjunction found to have potential
    % for deforestation and attempt the optimization.
    %
:- pred handle_deforestation(set_of_progvar::in, deforest_info::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    annotated_conj::in, annotated_conj::out, bool::out,
    pd_info::in, pd_info::out) is det.

handle_deforestation(NonLocals, DeforestInfo0, !RevBeforeGoals, !AfterGoals,
        Optimized, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_get_created_versions(!.PDInfo, CreatedVersions0),

    pd_info_get_depth(!.PDInfo, Depth0),
    trace [io(!IO)] (
        pd_debug_message(!.PDInfo,
            "checking for deforestation at depth %i\n", [i(Depth0)], !IO)
    ),

    reorder_conj(DeforestInfo0, DeforestInfo,
        BeforeIrrelevant, AfterIrrelevant, !.PDInfo),

    get_sub_conj_nonlocals(NonLocals, DeforestInfo, !.RevBeforeGoals,
        BeforeIrrelevant, AfterIrrelevant, !.AfterGoals, ConjNonLocals),

    % Update the instmap.
    list.foldl(pd_info_update_goal, BeforeIrrelevant, !PDInfo),

    pd_info_get_pred_proc_id(!.PDInfo, CurrPredProcId),
    pd_info_get_parents(!.PDInfo, Parents0),
    pd_info_get_cost_delta(!.PDInfo, CostDelta0),
    pd_info_get_size_delta(!.PDInfo, SizeDelta0),

    DeforestInfo = deforest_info(EarlierGoal, _, BetweenGoals,
        LaterGoal, _, DeforestBranches),

    should_try_deforestation(DeforestInfo, ShouldOptimize, !PDInfo),
    (
        ShouldOptimize = no,
        Optimized0 = no,
        Goals = []
    ;
        ShouldOptimize = yes,
        ( if
            EarlierGoal = hlds_goal(plain_call(PredId1, _, _, _, _, _), _),
            LaterGoal = hlds_goal(plain_call(PredId2, _, _, _, _, _), _)
        then
            % If both goals are calls create a new predicate for the
            % conjunction to be deforested and process it.
            PredName1 = predicate_name(ModuleInfo, PredId1),
            PredName2 = predicate_name(ModuleInfo, PredId2),
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo, "deforesting calls to %s and %s\n",
                    [s(PredName1), s(PredName2)], !IO)
            ),
            call_call(ConjNonLocals, EarlierGoal, BetweenGoals,
                yes(LaterGoal), MaybeGoal, !PDInfo),
            (
                MaybeGoal = yes(Goal),
                Optimized0 = yes,
                Goals = [Goal]
            ;
                MaybeGoal = no,
                Optimized0 = no,
                Goals = []
            )
        else if
            % If the first goal is branched and the second goal is a call,
            % attempt to push the call into the branches. Don't push a
            % recursive call or a call to a predicate we have already pushed
            % into a switch, since it is difficult to stop the process.
            EarlierGoal = hlds_goal(EarlierGoalExpr, _),
            goal_util.goal_is_branched(EarlierGoalExpr),
            LaterGoal = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _),
            PredProcId = proc(PredId, ProcId),
            PredProcId \= CurrPredProcId,
            not set.member(PredProcId, Parents0)
        then
            CurrPredName = predicate_name(ModuleInfo, PredId),
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo, "Pushing call to %s into goal\n",
                    [s(CurrPredName)], !IO)
            ),
            set.insert(proc(PredId, ProcId), Parents0, Parents),
            pd_info_set_parents(Parents, !PDInfo),
            push_goal_into_goal(ConjNonLocals, DeforestBranches,
                EarlierGoal, BetweenGoals, LaterGoal, Goal, !PDInfo),
            Goals = [Goal],
            Optimized0 = yes
        else if
            % If both goals are branched, push the second into the branches
            % of the first.
            EarlierGoal = hlds_goal(EarlierGoalExpr, _),
            LaterGoal = hlds_goal(LaterGoalExpr, _),
            goal_util.goal_is_branched(EarlierGoalExpr),
            goal_util.goal_is_branched(LaterGoalExpr)
        then
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo, "Pushing goal into goal\n", [], !IO)
            ),
            push_goal_into_goal(ConjNonLocals, DeforestBranches,
                EarlierGoal, BetweenGoals, LaterGoal, Goal, !PDInfo),
            Goals = [Goal],
            goals_size([EarlierGoal | BetweenGoals], ConjSize1),
            goal_size(LaterGoal, ConjSize2),
            goal_size(Goal, NewSize),
            SizeDiff = NewSize - ConjSize1 - ConjSize2,
            pd_info_incr_size_delta(SizeDiff, !PDInfo),
            Optimized0 = yes
        else
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo, "not optimizing\n", [], !IO)
            ),
            Goals = [],
            Optimized0 = no
        )
    ),
    Optimized = is_improvement_worth_while(!.PDInfo, Optimized0,
        CostDelta0, SizeDelta0),

    % Clean up.
    pd_info_set_depth(Depth0, !PDInfo),
    pd_info_set_instmap(InstMap0, !PDInfo),
    (
        Optimized = no,

        % XXX Currently this only attempts to deforest the first goal
        % with the first matching goal later in the conjunction. If the
        % deforestation failed, other later goals should be tried.
        %
        % Return everything to the state it was in before the attempted
        % optimization.
        pd_info_set_cost_delta(CostDelta0, !PDInfo),
        pd_info_set_size_delta(SizeDelta0, !PDInfo),

        % Remove any versions which were created.
        pd_info_get_created_versions(!.PDInfo, CreatedVersions),
        set.difference(CreatedVersions, CreatedVersions0, NewVersions0),
        set.to_sorted_list(NewVersions0, NewVersions),
        list.foldl(pd_info_remove_version, NewVersions, !PDInfo)

        % AfterGoals will be restored properly in conj.
    ;
        Optimized = yes,
        % We want to reprocess the deforested goal to see if it can be
        % deforested with other goals later in the conjunction.
        list.condense([BeforeIrrelevant, Goals, AfterIrrelevant],
            GoalsToProcess),
        compute_goal_infos(GoalsToProcess, GoalsAndInfo, !PDInfo),
        list.append(GoalsAndInfo, !AfterGoals),
        pd_info_set_instmap(InstMap0, !PDInfo),
        pd_info_set_changed(yes, !PDInfo),
        pd_info_set_rerun_det(yes, !PDInfo)
    ),
    trace [io(!IO)] (
        pd_debug_message(!.PDInfo,
            "finished deforestation at depth %i\n", [i(Depth0)], !IO)
    ),
    pd_info_set_parents(Parents0, !PDInfo).

    % Check whether deforestation is legal and worthwhile.
    %
:- pred should_try_deforestation(deforest_info::in, bool::out,
    pd_info::in, pd_info::out) is det.

should_try_deforestation(DeforestInfo, ShouldTry, !PDInfo) :-
    DeforestInfo = deforest_info(EarlierGoal, EarlierBranchInfo,
        BetweenGoals, LaterGoal, _, _),
    pd_info_get_useless_versions(!.PDInfo, UselessVersions),
    ( if
        EarlierGoal = hlds_goal(plain_call(PredId1, ProcId1, _, _, _, _), _),
        LaterGoal = hlds_goal(plain_call(PredId2, ProcId2, _, _, _, _), _),
        set.member(proc(PredId1, ProcId1) - proc(PredId2, ProcId2),
            UselessVersions)
    then
        trace [compile_time(flag("debug_deforest")), io(!IO)] (
            pd_debug_message(!.PDInfo,
                "version tried before, not worthwhile\n", [], !IO)
        ),
        ShouldTry = no
    else if
        % If some later goal depends on a variable such as an io.state
        % for which the construction cannot be reversed, recursive
        % folding will be impossible, so give up on the optimization.
        EarlierBranchInfo = pd_branch_info(_, _, OpaqueVars),
        ( list.member(OpaqueGoal, BetweenGoals)
        ; OpaqueGoal = LaterGoal
        ),
        OpaqueGoal = hlds_goal(_, OpaqueGoalInfo),
        OpaqueNonLocals = goal_info_get_nonlocals(OpaqueGoalInfo),
        OpaqueVarsSet = set_of_var.set_to_bitset(OpaqueVars),
        set_of_var.intersect(OpaqueNonLocals, OpaqueVarsSet, UsedOpaqueVars),
        set_of_var.is_non_empty(UsedOpaqueVars)
    then
        trace [compile_time(flag("debug_deforest")), io(!IO)] (
            pd_debug_message(!.PDInfo,
                "later goals depend on opaque vars\n", [], !IO)
        ),
        ShouldTry = no
    else if
        % A disjunction can be semidet only if it binds no variables.
        % If we push a goal which binds a variable into a semidet disjunction,
        % it won't be semidet anymore.
        %
        % We assume that LaterGoal can bind variables, because we don't know
        % whether it can or not. Its instmap_delta doesn't tell us, because
        % it tells us only the NEW inst of the variables whose insts it
        % changes. Without knowing their old insts as well, we don't know
        % whether the inst change reflects the variable being bound, or just
        % the gathering of knowledge about what its initial value could have
        % been.
        EarlierGoal = hlds_goal(disj(_), EarlierGoalInfo),
        EarlierGoalDetism = goal_info_get_determinism(EarlierGoalInfo),
        determinism_components(EarlierGoalDetism, _, EarlierGoalMaxSolns),
        EarlierGoalMaxSolns \= at_most_many
    then
        ShouldTry = no
    else
        ShouldTry = yes
    ).

:- pred can_optimize_conj(hlds_goal::in, list(hlds_goal)::in,
    maybe(hlds_goal)::in, bool::out, pd_info::in, pd_info::out) is det.

can_optimize_conj(EarlierGoal, BetweenGoals, MaybeLaterGoal, ShouldTry,
        !PDInfo) :-
    pd_info_get_depth(!.PDInfo, Depth0),
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    MaxDepth = OptTuple ^ ot_deforestation_depth_limit,
    Depth = Depth0 + 1,
    pd_info_set_depth(Depth, !PDInfo),
    SizeLimit = OptTuple ^ ot_deforestation_size_threshold,
    globals.lookup_option(Globals, fully_strict, FullyStrictOp),
    ( if
        MaxDepth \= -1,     % no depth limit set
        % XXX The *default* value of the depth_limit is 4,
        % so -1 does NOT mean that there is no depth limit set.
        Depth0 >= MaxDepth
    then
        % The depth limit was exceeded. This should not occur too often in
        % practice - the depth limit is just a safety net.
        trace [compile_time(flag("debug_deforest")), io(!IO)] (
            pd_debug_message(!.PDInfo,
                "\n\n*****Depth limit exceeded*****\n\n", [], !IO)
        ),
        ShouldTry = no
    else if
        % Check whether either of the goals to be deforested is too large.
        % XXX This is probably a bit too crude, especially for LaterGoal,
        % which should be reduced in size in the specialized version
        % (the specialized version will only include one branch of the
        % top-level switch).
        SizeLimit \= -1,
        (
            EarlierGoal = hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _)
        ;
            MaybeLaterGoal = yes(
                hlds_goal(plain_call(PredId, ProcId, _, _, _, _), _))
        ),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            _, CalledProcInfo),
        proc_info_get_goal(CalledProcInfo, CalledGoal),
        goal_size(CalledGoal, CalledGoalSize),
        SizeLimit \= -1,
        CalledGoalSize > SizeLimit
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "goal too large\n", [], !IO)
        ),
        ShouldTry = no
    else if
        % Check whether either of the goals to be deforested can't be inlined.
        (
            EarlierGoal = hlds_goal(EarlierGoalExpr, _),
            EarlierGoalExpr = plain_call(PredId, ProcId, _, BuiltinState, _, _)
        ;
            MaybeLaterGoal = yes(hlds_goal(LaterGoalExpr, _)),
            LaterGoalExpr = plain_call(PredId, ProcId, _, BuiltinState, _, _)
        ),

        % We don't attempt to deforest predicates which have purity promises
        % because the extra impurity propagated through the goal when such
        % predicates are inlined will defeat any attempt at deforestation.
        % XXX We should probably allow deforestation of semipure goals.
        not inlining.can_inline_proc(ModuleInfo, PredId, ProcId, BuiltinState,
            may_not_inline_purity_promised_pred)
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "non-inlineable calls\n", [], !IO)
        ),
        ShouldTry = no
    else if
        %
        % Don't optimize if that would require duplicating
        % branched goal structures.
        %
        not is_simple_goal_list(BetweenGoals)
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "between goals not simple enough\n", [], !IO)
        ),
        ShouldTry = no
    else if
        % Give up if there are any impure goals involved.
        % XXX We should probably allow deforestation of semipure goals.
        ( list.member(ImpureGoal, BetweenGoals)
        ; ImpureGoal = EarlierGoal
        ; MaybeLaterGoal = yes(ImpureGoal)
        ),
        ImpureGoal = hlds_goal(_, ImpureGoalInfo),
        not goal_info_get_purity(ImpureGoalInfo) = purity_pure
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "goal list contains impure goal(s)\n", [], !IO)
        ),
        ShouldTry = no
    else if
        % Check whether interleaving the execution of the goals could alter
        % the termination behaviour in a way which is illegal according to the
        % semantics options.
        %
        FullyStrictOp = bool(FullyStrict),
        (
            list.member(OtherGoal, BetweenGoals)
        ;
            MaybeLaterGoal = yes(LaterGoal),
            OtherGoal = LaterGoal
        ),
        not goal_util.reordering_maintains_termination_old(ModuleInfo,
            FullyStrict, EarlierGoal, OtherGoal)
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "interleaving execution could change termination behaviour\n",
                [], !IO)
        ),
        ShouldTry = no
    else
        ShouldTry = yes
    ).

    % Check that the code size increase is justified by the estimated
    % performance increase. This should err towards allowing optimization
    % - without any check at all the code size of the library only increases
    % ~10%.
    %
:- func is_improvement_worth_while(pd_info, bool, int, int) = bool.

is_improvement_worth_while(PDInfo, Optimized0, CostDelta0, SizeDelta0)
        = Optimized :-
    pd_info_get_cost_delta(PDInfo, CostDelta),
    pd_info_get_size_delta(PDInfo, SizeDelta),
    Improvement = CostDelta - CostDelta0,
    SizeDifference = SizeDelta - SizeDelta0,

    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    Factor = OptTuple ^ ot_deforestation_cost_factor,
    ( if
        Optimized0 = yes,
        check_deforestation_improvement(Factor, Improvement, SizeDifference)
    then
        Optimized = yes,
        trace [io(!IO)] (
            pd_debug_message(PDInfo,
                "Enough improvement: cost(%i) size(%i)\n",
                [i(Improvement), i(SizeDifference)], !IO)
        )
    else
        Optimized = no,
        trace [io(!IO)] (
            pd_debug_message(PDInfo,
                "Not enough improvement: cost(%i) size(%i)\n",
                [i(Improvement), i(SizeDifference)], !IO)
        )
    ).

%-----------------------------------------------------------------------------%

    % Attempt deforestation on a pair of calls.
    %
:- pred call_call(set_of_progvar::in, hlds_goal::in,
    list(hlds_goal)::in, maybe(hlds_goal)::in, maybe(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

call_call(ConjNonLocals, EarlierGoal, BetweenGoals, MaybeLaterGoal, MaybeGoal,
        !PDInfo) :-
    can_optimize_conj(EarlierGoal, BetweenGoals, MaybeLaterGoal, ShouldTry,
        !PDInfo),
    (
        ShouldTry = yes,
        disable_warning [suspicious_recursion] (
            call_call(ConjNonLocals, EarlierGoal, BetweenGoals,
                MaybeLaterGoal, MaybeGoal, !PDInfo)
        )
    ;
        ShouldTry = no,
        MaybeGoal = no
    ).

    % Attempt deforestation on a pair of calls.
    %
:- pred call_call_2(set_of_progvar::in, hlds_goal::in,
    list(hlds_goal)::in, maybe(hlds_goal)::in, maybe(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.
:- pragma consider_used(pred(call_call_2/7)).

call_call_2(ConjNonLocals, EarlierGoal, BetweenGoals, MaybeLaterGoal,
        MaybeGoal, !PDInfo) :-
    create_conj(EarlierGoal, BetweenGoals, MaybeLaterGoal, ConjNonLocals,
        FoldGoal),

    pd_info_search_version(!.PDInfo, FoldGoal, MaybeVersion),
    pd_info_get_parent_versions(!.PDInfo, Parents),

    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, debug_pd, DebugPD),
    ( if
        MaybeVersion = version(_, VersionPredProcId,
            VersionInfo, Renaming, TypeRenaming)
    then
        % If we see an opportunity to fold, take it.
        VersionPredProcId = proc(VersionPredId, _),
        pd_info_get_module_info(!.PDInfo, ModuleInfo0),
        FoldPredName = predicate_name(ModuleInfo0, VersionPredId),
        trace [io(!IO)] (
            (
                DebugPD = no
            ;
                DebugPD = yes,
                module_info_get_name(ModuleInfo, ModuleName),
                get_debug_output_stream(Globals, ModuleName, Stream, !IO),
                io.format(Stream, "Folded with %s\n", [s(FoldPredName)], !IO)
            )
        ),
        ( if set.member(VersionPredProcId, Parents) then
            FoldCostDelta = cost_of_recursive_fold
        else
            FoldCostDelta = cost_of_fold
        ),
        pd_info_incr_cost_delta(FoldCostDelta, !PDInfo),
        goals_size([EarlierGoal | BetweenGoals], NegSizeDelta),
        SizeDelta = - NegSizeDelta,
        pd_info_incr_size_delta(SizeDelta, !PDInfo),
        create_call_goal(VersionPredProcId, VersionInfo, Renaming,
            TypeRenaming, Goal, !PDInfo),
        MaybeGoal = yes(Goal)
    else
        pd_info_get_global_term_info(!.PDInfo, TermInfo0),
        pd_info_get_parent_versions(!.PDInfo, ParentVersions0),

        trace [io(!IO)] (
            (
                DebugPD = no
            ;
                DebugPD = yes,
                module_info_get_name(ModuleInfo, ModuleName),
                get_debug_output_stream(Globals, ModuleName, Stream, !IO),
                io.write_string(Stream, "Parents: ", !IO),
                io.write_line(Stream, ParentVersions0, !IO),
                io.flush_output(Stream, !IO)
            )
        ),

        pd_info_get_versions(!.PDInfo, Versions),
        pd_info_get_instmap(!.PDInfo, InstMap),
        pd_term.global_check(ModuleInfo, EarlierGoal, BetweenGoals,
            MaybeLaterGoal, InstMap, Versions, TermInfo0, TermInfo,
            CheckResult),
        (
            CheckResult = ok(ProcPair, Size),
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "global termination check succeeded - " ++
                    "creating new version\n",
                    [], !IO)
            ),
            pd_info_set_global_term_info(TermInfo, !PDInfo),
            RunModes = no,
            MaybeGeneralised = no,
            create_deforest_goal(EarlierGoal, BetweenGoals,
                MaybeLaterGoal, FoldGoal, ConjNonLocals, RunModes, ProcPair,
                Size, MaybeGeneralised, MaybeGoal, !PDInfo)
        ;
            CheckResult = possible_loop(ProcPair, Size,
                CoveringPredProcId),
            % The termination check found the same pair of end-points with the
            % same length goal. If the goal matches the goal for the "covering"
            % predicate, perform a most specific generalisation on the insts
            % then keep on going.
            try_generalisation(EarlierGoal, BetweenGoals,
                MaybeLaterGoal, FoldGoal, ConjNonLocals, ProcPair, Size,
                CoveringPredProcId, MaybeGoal, !PDInfo)
        ;
            CheckResult = loop,
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "global termination check failed\n", [], !IO)
            ),
            MaybeGoal = no
        ),
        pd_info_set_global_term_info(TermInfo0, !PDInfo)
    ).

%-----------------------------------------------------------------------------%

    % Create a new procedure for a conjunction to be deforested, then
    % recursively process that procedure.
    %
:- pred create_deforest_goal(hlds_goal::in, hlds_goals::in,
    maybe(hlds_goal)::in, hlds_goal::in, set_of_progvar::in, bool::in,
    proc_pair::in, int::in, maybe(pred_proc_id)::in, maybe(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

create_deforest_goal(EarlierGoal, BetweenGoals, MaybeLaterGoal,
        FoldGoal0, NonLocals, RunModes, ProcPair, Size,
        MaybeGeneralised, MaybeCallGoal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo0),
    module_info_get_globals(ModuleInfo0, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    VarsThreshold = OptTuple ^ ot_deforestation_vars_threshold,
    ( if
        EarlierGoal = hlds_goal(EarlierGoalExpr, _),
        EarlierGoalExpr = plain_call(PredId1, ProcId1, Args1, _, _, _),
        (
            % No threshold set.
            VarsThreshold = -1
            % XXX The *default* value of the vars threshold is 200,
            % so -1 does NOT mean that there is no threshold set.
        ;
            % Check that we are not creating a procedure with a massive number
            % of variables. We assume that all the variables in the first
            % called goal are present in the final version. If the number
            % of variables in the first called goal plus the number of
            % variables in BetweenGoals is less than
            % --deforestation-vars-threshold, go ahead and optimize.

            module_info_pred_proc_info(ModuleInfo0, PredId1, ProcId1, _,
                CalledProcInfo1),
            proc_info_get_goal(CalledProcInfo1, CalledGoal1),
            goal_util.goal_vars(CalledGoal1, GoalVars1),
            set_of_var.to_sorted_list(GoalVars1, GoalVarsList1),
            goal_util.goals_goal_vars(BetweenGoals, GoalVars2),
            set_of_var.to_sorted_list(GoalVars2, GoalVarsList2),

            list.length(GoalVarsList1, NumVars1),
            list.length(GoalVarsList2, NumVars2),
            NumVars = NumVars1 + NumVars2,
            NumVars < VarsThreshold
        )
    then
        % Create the goal for the new predicate, unfolding the first call.

        pd_info_get_instmap(!.PDInfo, InstMap0),
        pd_info_get_proc_info(!.PDInfo, ProcInfo0),
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "unfolding first call\n", [], !IO)
        ),

        unfold_call(no, no, PredId1, ProcId1, Args1, EarlierGoal, UnfoldedCall,
            DidUnfold, !PDInfo),
        create_conj(UnfoldedCall, BetweenGoals, MaybeLaterGoal, NonLocals,
            DeforestGoal0),
        set_of_var.to_sorted_list(NonLocals, NonLocalsList),

        ( if
            DidUnfold = yes,
            RunModes = yes
        then
            % If we did a generalisation step when creating this version,
            % we need to modecheck to propagate through the new insts.
            % If this causes mode errors, don't create the new version.
            % This can happen if a procedure expected an input to be bound
            % to a particular functor but the extra information was
            % generalised away.

            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "running modes on deforest goal\n", [], !IO)
            ),
            pd_util.unique_modecheck_goal(DeforestGoal0, DeforestGoal,
                Errors1, !PDInfo),
            pd_util.unique_modecheck_goal(FoldGoal0, FoldGoal,
                Errors2, !PDInfo),
            Errors = Errors1 ++ Errors2
        else
            DeforestGoal = DeforestGoal0,
            FoldGoal = FoldGoal0,
            Errors = []
        ),

        % We must have been able to unfold the first call to proceed
        % with the optimization, otherwise we will introduce an
        % infinite loop in the generated code.
        ( if
            DidUnfold = yes,
            Errors = []
        then
            % Create the new version.

            pd_info_define_new_pred(DeforestGoal, PredProcId, CallGoal,
                !PDInfo),
            PredProcId = proc(PredId, _),

            pd_info_get_module_info(!.PDInfo, ModuleInfo),

            PredName = predicate_name(ModuleInfo, PredId),
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "\nCreated predicate %s\n", [s(PredName)], !IO)
            ),
            ( if
                MaybeLaterGoal = yes(hlds_goal(LaterGoalExpr, _)),
                LaterGoalExpr = plain_call(PredId2, ProcId2, _, _, _, _)
            then
                CalledPreds = [proc(PredId1, ProcId1), proc(PredId2, ProcId2)]
            else
                CalledPreds = [proc(PredId1, ProcId1)]
            ),
            pd_info_get_parent_versions(!.PDInfo, Parents0),

            pd_info_get_proc_info(!.PDInfo, ProcInfo1),
            proc_info_get_var_table(ProcInfo1, VarTable),
            lookup_var_types(VarTable, NonLocalsList, ArgTypes),
            VersionInfo = version_info(FoldGoal, CalledPreds, NonLocalsList,
                ArgTypes, InstMap0, 0, 0, Parents0, MaybeGeneralised),
            pd_info_get_global_term_info(!.PDInfo, TermInfo0),
            pd_term.update_global_term_info(ProcPair, PredProcId,
                Size, TermInfo0, TermInfo),
            pd_info_set_global_term_info(TermInfo, !PDInfo),
            set.insert_list([PredProcId | CalledPreds], Parents0, Parents),
            pd_info_set_parent_versions(Parents, !PDInfo),
            pd_info_register_version(PredProcId, VersionInfo, !PDInfo),

            % Run deforestation on the new predicate to do the folding.
            pd_info_get_unfold_info(!.PDInfo, UnfoldInfo),
            deforest_proc_deltas(PredProcId, CostDelta, SizeDelta, !PDInfo),
            pd_info_set_unfold_info(UnfoldInfo, !PDInfo),
            pd_info_incr_cost_delta(CostDelta, !PDInfo),
            pd_info_incr_size_delta(SizeDelta, !PDInfo),
            pd_info_set_parent_versions(Parents0, !PDInfo),

            pd_info_get_progress_stream(!.PDInfo, ProgressStream),
            pd_info_get_pred_proc_id(!.PDInfo, CurPredProcId),
            trace [io(!IO)] (
                maybe_write_proc_progress_message(ProgressStream, ModuleInfo,
                    "Back in", CurPredProcId, !IO)
            ),
            MaybeCallGoal = yes(CallGoal)
        else
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "Generalisation produced mode errors\n", [], !IO)
            ),
            MaybeCallGoal = no
        ),

        % The var_table field was updated when we unfolded the first call,
        % but all the new variables are only used in the new version,
        % so it is safe to reset the proc_info.
        pd_info_set_proc_info(ProcInfo0, !PDInfo),
        pd_info_set_instmap(InstMap0, !PDInfo)
    else
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "vars threshold exceeded\n", [], !IO)
        ),
        MaybeCallGoal = no
    ).

%-----------------------------------------------------------------------------%

    % Create a goal to call a newly created version.
    %
:- pred create_call_goal(pred_proc_id::in, version_info::in,
    map(prog_var, prog_var)::in, tsubst::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

create_call_goal(proc(PredId, ProcId), VersionInfo, Renaming, TypeSubn, Goal,
        !PDInfo) :-
    OldArgs = VersionInfo ^ version_arg_vars,
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
        CalledPredInfo, CalledProcInfo),
    pred_info_get_arg_types(CalledPredInfo, CalledTVarSet, _CalledExistQVars,
        ArgTypes0),

    % Rename the arguments in the version.
    pd_info_get_proc_info(!.PDInfo, ProcInfo0),
    pd_info_get_pred_info(!.PDInfo, PredInfo0),

    proc_info_get_var_table(ProcInfo0, VarTable0),
    pred_info_get_typevarset(PredInfo0, TVarSet0),

    % Rename the argument types using the current pred's tvarset.
    tvarset_merge_renaming(TVarSet0, CalledTVarSet, TVarSet, TypeRenaming),
    pred_info_set_typevarset(TVarSet, PredInfo0, PredInfo),
    pd_info_set_pred_info(PredInfo, !PDInfo),
    apply_variable_renaming_to_type_list(TypeRenaming, ArgTypes0, ArgTypes1),

    create_deforest_call_args(ModuleInfo, Renaming, TypeSubn,
        OldArgs, ArgTypes1, Args, VarTable0, VarTable),
    proc_info_set_var_table(VarTable, ProcInfo0, ProcInfo),
    pd_info_set_proc_info(ProcInfo, !PDInfo),

    % Compute a goal_info.
    proc_info_get_argmodes(CalledProcInfo, ArgModes),
    instmap_delta_from_mode_list(ModuleInfo, Args, ArgModes, InstMapDelta),
    proc_info_interface_determinism(ProcInfo, Detism),
    set_of_var.list_to_set(Args, NonLocals),
    pred_info_get_purity(CalledPredInfo, Purity),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, GoalInfo),

    PredModule = pred_info_module(CalledPredInfo),
    PredName = pred_info_name(CalledPredInfo),
    GoalExpr = plain_call(PredId, ProcId, Args, not_builtin, no,
        qualified(PredModule, PredName)),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred create_deforest_call_args(module_info::in,
    map(prog_var, prog_var)::in, tsubst::in,
    list(prog_var)::in, list(mer_type)::in, list(prog_var)::out,
    var_table::in, var_table::out) is det.

create_deforest_call_args(_, _, _, [], [], [], !VarTable).
create_deforest_call_args(_, _, _, [], [_ | _], _, !VarTable) :-
    unexpected($pred, "length mismatch").
create_deforest_call_args(_, _, _, [_ | _], [], _, !VarTable) :-
    unexpected($pred, "length mismatch").
create_deforest_call_args(ModuleInfo, Renaming, TypeSubn,
        [OldArg | OldArgs], [ArgType | ArgTypes], [Arg | Args], !VarTable) :-
    ( if map.search(Renaming, OldArg, ArgPrime) then
        Arg = ArgPrime
    else
        % The variable is local to the call. Create a fresh variable.
        apply_subst_to_type(TypeSubn, ArgType, SubnArgType),
        IsDummy = is_type_a_dummy(ModuleInfo, SubnArgType),
        ArgEntry = vte("", SubnArgType, IsDummy),
        add_var_entry(ArgEntry, Arg, !VarTable)
    ),
    create_deforest_call_args(ModuleInfo, Renaming, TypeSubn,
        OldArgs, ArgTypes, Args, !VarTable).

%-----------------------------------------------------------------------------%

    % Combine the two goals to be deforested and the goals in between
    % into a conjunction.
    %
:- pred create_conj(hlds_goal::in, list(hlds_goal)::in,
    maybe(hlds_goal)::in, set_of_progvar::in, hlds_goal::out) is det.

create_conj(EarlierGoal, BetweenGoals, MaybeLaterGoal, NonLocals, FoldGoal) :-
    (
        MaybeLaterGoal = yes(LaterGoal),
        list.append([EarlierGoal | BetweenGoals], [LaterGoal], DeforestConj)
    ;
        MaybeLaterGoal = no,
        DeforestConj = [EarlierGoal | BetweenGoals]
    ),
    goal_list_determinism(DeforestConj, Detism),
    goal_list_instmap_delta(DeforestConj, InstMapDelta0),
    instmap_delta_restrict(NonLocals, InstMapDelta0, InstMapDelta),
    goal_list_purity(DeforestConj, Purity),
    goal_info_init(NonLocals, InstMapDelta, Detism, Purity, ConjInfo0),

    % Give the conjunction a context so that the generated predicate
    % name points to the location of the first goal.
    EarlierGoal = hlds_goal(_, EarlierGoalInfo),
    EarlierContext = goal_info_get_context(EarlierGoalInfo),
    goal_info_set_context(EarlierContext, ConjInfo0, ConjInfo),
    FoldGoal = hlds_goal(conj(plain_conj, DeforestConj), ConjInfo).

%-----------------------------------------------------------------------------%

    % "Round-off" some of the extra information that caused the termination
    % check to fail and/or the insts of the versions not to match in an attempt
    % to achieve folding.
    %
:- pred try_generalisation(hlds_goal::in, list(hlds_goal)::in,
    maybe(hlds_goal)::in, hlds_goal::in, set_of_progvar::in,
    proc_pair::in, int::in, pred_proc_id::in, maybe(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

try_generalisation(EarlierGoal, BetweenGoals, MaybeLaterGoal,
        FoldGoal, ConjNonLocals, ProcPair, Size,
        CoveringPredProcId, MaybeGoal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    trace [io(!IO)] (
        pd_debug_message(!.PDInfo, "trying generalisation\n", [], !IO)
    ),
    pd_info_get_versions(!.PDInfo, VersionIndex),
    map.lookup(VersionIndex, CoveringPredProcId, Version),
    Version = version_info(VersionGoal, _, VersionArgVars,
        VersionArgTypes, VersionInstMap, _, _, _, _),
    pd_info_get_versions(!.PDInfo, Versions),
    pd_info_get_proc_info(!.PDInfo, ProcInfo),
    proc_info_get_var_table(ProcInfo, VarTable),
    ( if
        pd_util.goals_match(ModuleInfo, VersionGoal, VersionArgVars,
            VersionArgTypes, FoldGoal, VarTable, Renaming, _)
    then
        do_generalisation(VersionArgVars, Renaming, VersionInstMap,
            EarlierGoal, BetweenGoals, MaybeLaterGoal, FoldGoal, ConjNonLocals,
            ProcPair, Size, CoveringPredProcId, MaybeGoal, !PDInfo)
    else if
        % If the earlier goal is a generalisation of another version, try
        % matching against that. This happens when attempting two
        % deforestations in a row and the first deforestation required
        % generalisation.
        match_generalised_version(ModuleInfo, VersionGoal,
            VersionArgVars, VersionArgTypes, EarlierGoal, BetweenGoals,
            MaybeLaterGoal, ConjNonLocals, VarTable, Versions,
            Renaming)
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "matched with generalised version\n", [], !IO)
        ),
        do_generalisation(VersionArgVars, Renaming, VersionInstMap,
            EarlierGoal, BetweenGoals, MaybeLaterGoal, FoldGoal, ConjNonLocals,
            ProcPair, Size, CoveringPredProcId, MaybeGoal, !PDInfo)
    else
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "goals don't match\n", [], !IO)
        ),
        MaybeGoal = no
    ).

:- pred do_generalisation(list(prog_var)::in,
    map(prog_var, prog_var)::in, instmap::in, hlds_goal::in,
    list(hlds_goal)::in, maybe(hlds_goal)::in, hlds_goal::in,
    set_of_progvar::in, proc_pair::in, int::in,
    pred_proc_id::in, maybe(hlds_goal)::out,
    pd_info::in, pd_info::out) is det.

do_generalisation(VersionArgVars, Renaming, VersionInstMap, EarlierGoal,
        BetweenGoals, MaybeLaterGoal, FoldGoal, ConjNonLocals,
        ProcPair, Size, Generalised, MaybeGoal, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    trace [io(!IO)] (
        pd_debug_message(!.PDInfo, "goals match, trying MSG\n", [], !IO)
    ),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    instmap_lookup_vars(VersionInstMap, VersionArgVars, VersionInsts),
    pd_util.inst_list_size(ModuleInfo, VersionInsts, VersionInstSizes),
    set_of_var.to_sorted_list(ConjNonLocals, ConjNonLocalsList),
    ( if
        % Check whether we can do a most specific generalisation of insts
        % of the non-locals.
        try_MSG(ModuleInfo, VersionInstMap, Renaming, VersionArgVars,
            InstMap0, InstMap),
        instmap_lookup_vars(InstMap, ConjNonLocalsList, ArgInsts),
        pd_util.inst_list_size(ModuleInfo, ArgInsts, NewInstSizes),
        NewInstSizes < VersionInstSizes
    then
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "MSG succeeded", [], !IO)
        ),
        pd_info_set_instmap(InstMap, !PDInfo),
        create_deforest_goal(EarlierGoal, BetweenGoals,
            MaybeLaterGoal, FoldGoal, ConjNonLocals, yes, ProcPair,
            Size, yes(Generalised), MaybeGoal, !PDInfo)
    else
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "MSG failed\n", [], !IO)
        ),
        MaybeGoal = no
    ),
    pd_info_set_instmap(InstMap0, !PDInfo).

:- pred try_MSG(module_info::in, instmap::in, map(prog_var, prog_var)::in,
    list(prog_var)::in, instmap::in, instmap::out) is semidet.

try_MSG(_, _, _, [], !InstMap).
try_MSG(ModuleInfo, VersionInstMap, Renaming, [VersionArgVar | VersionArgVars],
        !InstMap) :-
    instmap_lookup_var(VersionInstMap, VersionArgVar, VersionInst),
    ( if
        map.search(Renaming, VersionArgVar, ArgVar),
        instmap_lookup_var(!.InstMap, ArgVar, VarInst),
        inst_MSG(ModuleInfo, VersionInst, VarInst, Inst)
    then
        instmap_set_var(ArgVar, Inst, !InstMap)
    else
        true
    ),
    try_MSG(ModuleInfo, VersionInstMap, Renaming, VersionArgVars, !InstMap).

%-----------------------------------------------------------------------------%

    % If the global termination check and generalisation failed and
    % the first goal in the conjunction to be specialised is a generalisation
    % of another version, try matching and generalising using that
    % (non-generalised) version.
    %
    % This predicate maps the call to the generalised predicate back
    % onto the non-generalised version. This makes the goal match
    % with the previous conjunction, so the generalisation can be
    % reapplied to the entire conjunction.
    %
    % XXX This only undoes one level of generalisation.
    %
:- pred match_generalised_version(module_info::in,
    hlds_goal::in, list(prog_var)::in, list(mer_type)::in,
    hlds_goal::in, list(hlds_goal)::in, maybe(hlds_goal)::in,
    set_of_progvar::in, var_table::in,
    version_index::in, map(prog_var, prog_var)::out) is semidet.

match_generalised_version(ModuleInfo, VersionGoal, VersionArgVars,
        VersionArgTypes, FirstGoal, BetweenGoals, MaybeLastGoal,
        ConjNonLocals, !.VarTable, Versions, Renaming) :-
    FirstGoal = hlds_goal(FirstGoalExpr, _),
    FirstGoalExpr =
        plain_call(FirstPredId, FirstProcId, FirstArgVars, _, _, _),

    % Look up the version which the first goal calls.
    map.search(Versions, proc(FirstPredId, FirstProcId), FirstVersionInfo),
    FirstVersionInfo = version_info(FirstVersionGoal, _, FirstVersionArgVars,
        _, _, _, _, _, MaybeNonGeneralisedVersion),
    MaybeNonGeneralisedVersion = yes(NonGeneralisedPredProcId),
    map.from_corresponding_lists(FirstVersionArgVars, FirstArgVars,
        FirstRenaming0),

    goal_util.goal_vars(FirstVersionGoal, FirstVersionVars0),
    set_of_var.to_sorted_list(FirstVersionVars0, FirstVersionVars),

    module_info_pred_proc_info(ModuleInfo, FirstPredId, FirstProcId,
        _, FirstProcInfo),
    proc_info_get_var_table(FirstProcInfo, FirstVersionVarTable),

    clone_variables(FirstVersionVars, FirstVersionVarTable,
        !VarTable, FirstRenaming0, FirstRenaming),
    must_rename_vars_in_goal(FirstRenaming,
        FirstVersionGoal, RenamedFirstVersionGoal),

    % Look up the version which was generalised to create the version
    % which the first goal calls.
    NonGeneralisedPredProcId = proc(NonGeneralisedPredId,
        NonGeneralisedProcId),
    goal_to_conj_list(VersionGoal, VersionGoalList),
    VersionGoalList = [hlds_goal(
        plain_call(NonGeneralisedPredId, NonGeneralisedProcId, _, _, _, _), _)
        | _],

    % Find a renaming from the argument variables of the generalised
    % version to the version which was generalised.
    map.search(Versions, NonGeneralisedPredProcId,
        NonGeneralisedVersion),
    NonGeneralisedVersion = version_info(NonGeneralisedGoal, _,
        NonGeneralisedArgVars, NonGeneralisedArgTypes,_,_,_,_,_),
    pd_util.goals_match(ModuleInfo, NonGeneralisedGoal,
        NonGeneralisedArgVars, NonGeneralisedArgTypes,
        RenamedFirstVersionGoal, !.VarTable,
        GeneralRenaming, TypeRenaming),

    module_info_pred_info(ModuleInfo, NonGeneralisedPredId,
        NonGeneralisedPredInfo),
    pred_info_get_arg_types(NonGeneralisedPredInfo, NonGeneralisedArgTypes),
    create_deforest_call_args(ModuleInfo, GeneralRenaming, TypeRenaming,
        NonGeneralisedArgVars, NonGeneralisedArgTypes, NewArgVars,
        !.VarTable, _),

    % Only fill in as much as pd_util.goals_match actually looks at.
    goal_info_init(GoalInfo),
    NonGeneralFirstGoalExpr = plain_call(NonGeneralisedPredId,
        NonGeneralisedProcId, NewArgVars, not_builtin, no, unqualified("")),
    NonGeneralFirstGoal = hlds_goal(NonGeneralFirstGoalExpr, GoalInfo),
    create_conj(NonGeneralFirstGoal, BetweenGoals, MaybeLastGoal,
        ConjNonLocals, GoalToMatch),

    % Check whether the entire conjunction matches.
    pd_util.goals_match(ModuleInfo, VersionGoal, VersionArgVars,
        VersionArgTypes, GoalToMatch, !.VarTable, Renaming, _).

%-----------------------------------------------------------------------------%

    % Work out the nonlocals of a sub-conjunction from the non-locals of the
    % entire conjunction and the goals before and after the sub-conjunction.
    % This is needed to ensure that the temporary list in double_append is
    % found to be local to the conjunction and can be removed.
    %
:- pred get_sub_conj_nonlocals(set_of_progvar::in, deforest_info::in,
    list(hlds_goal)::in, list(hlds_goal)::in, list(hlds_goal)::in,
    annotated_conj::in, set_of_progvar::out) is det.

get_sub_conj_nonlocals(NonLocals0, DeforestInfo,
        RevBeforeGoals, BeforeIrrelevant, AfterIrrelevant,
        AfterGoals0, SubConjNonLocals) :-
    DeforestInfo = deforest_info(EarlierGoal, _, BetweenGoals, LaterGoal,
        _, _),
    assoc_list.keys(AfterGoals0, AfterGoals),
    do_get_sub_conj_nonlocals(NonLocals0, RevBeforeGoals,
        BeforeIrrelevant, EarlierGoal, BetweenGoals, yes(LaterGoal),
        AfterIrrelevant, AfterGoals, SubConjNonLocals).

:- pred do_get_sub_conj_nonlocals(set_of_progvar::in,
    list(hlds_goal)::in, list(hlds_goal)::in, hlds_goal::in,
    list(hlds_goal)::in, maybe(hlds_goal)::in, list(hlds_goal)::in,
    list(hlds_goal)::in, set_of_progvar::out) is det.

do_get_sub_conj_nonlocals(!.NonLocals, RevBeforeGoals, BeforeIrrelevant,
        EarlierGoal, BetweenGoals, MaybeLaterGoal,
        AfterIrrelevant, AfterGoals, !:SubConjNonLocals) :-
    AddGoalNonLocals =
        ( pred(Goal::in, Vars0::in, Vars::out) is det :-
            Goal = hlds_goal(_, GoalInfo),
            GoalNonLocals = goal_info_get_nonlocals(GoalInfo),
            set_of_var.union(GoalNonLocals, Vars0, Vars)
        ),
    list.foldl(AddGoalNonLocals, RevBeforeGoals, !NonLocals),
    list.foldl(AddGoalNonLocals, BeforeIrrelevant, !NonLocals),
    list.foldl(AddGoalNonLocals, AfterIrrelevant, !NonLocals),
    list.foldl(AddGoalNonLocals, AfterGoals, !NonLocals),

    list.foldl(AddGoalNonLocals, [EarlierGoal | BetweenGoals],
        set_of_var.init, !:SubConjNonLocals),
    (
        MaybeLaterGoal = yes(LaterGoal),
        call(AddGoalNonLocals, LaterGoal, !SubConjNonLocals)
    ;
        MaybeLaterGoal = no
    ),
    set_of_var.intersect(!.NonLocals, !SubConjNonLocals).

%-----------------------------------------------------------------------------%

    % Attempt to move irrelevant goals out of the conjunction. This does a safe
    % re-ordering that is guaranteed not to require rescheduling of the
    % conjunction, since it does not reorder goals that depend on each other.
    % We favor moving goals backward to avoid removing tail recursion.
    %
:- pred reorder_conj(deforest_info::in, deforest_info::out,
    list(hlds_goal)::out, list(hlds_goal)::out, pd_info::in) is det.

reorder_conj(DeforestInfo0, DeforestInfo,
        BeforeIrrelevant, AfterIrrelevant, PDInfo) :-
    pd_info_get_module_info(PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    trace [io(!IO)] (
        pd_debug_message(PDInfo, "Reordering conjunction\n", [], !IO)
    ),
    DeforestInfo0 = deforest_info(EarlierGoal, EarlierBranchInfo,
        BetweenGoals0, LaterGoal, LaterBranchInfo, DeforestBranches),

    globals.lookup_bool_option(Globals, fully_strict, FullyStrict),
    move_goals(deforest.can_move_goal_backward, ModuleInfo,
        FullyStrict, BetweenGoals0, [], RevBetweenGoals1, EarlierGoal,
        [], RevBeforeIrrelevant),
    move_goals(deforest.can_move_goal_forward,
        ModuleInfo, FullyStrict, RevBetweenGoals1,
        [], BetweenGoals, LaterGoal, [], AfterIrrelevant),

    list.reverse(RevBeforeIrrelevant, BeforeIrrelevant),
    DeforestInfo = deforest_info(EarlierGoal, EarlierBranchInfo,
        BetweenGoals, LaterGoal, LaterBranchInfo, DeforestBranches).

:- pred move_goals(can_move::can_move, module_info::in, bool::in,
    hlds_goals::in, hlds_goals::in, hlds_goals::out,
    hlds_goal::in, hlds_goals::in, hlds_goals::out) is det.

move_goals(_, _, _, [], !BetweenGoals, _, !MovedGoal).
move_goals(CanMove, ModuleInfo, FullyStrict, [BetweenGoal | RevBetweenGoals0],
        !BetweenGoals, EndGoal, !MovedGoals) :-
    ( if
        call(CanMove, ModuleInfo, FullyStrict, BetweenGoal,
            [EndGoal | !.BetweenGoals])
    then
        !:MovedGoals = [BetweenGoal | !.MovedGoals]
    else
        !:BetweenGoals = [BetweenGoal | !.BetweenGoals]
    ),
    move_goals(CanMove, ModuleInfo, FullyStrict,
        RevBetweenGoals0, !BetweenGoals, EndGoal, !MovedGoals).

:- type can_move == pred(module_info, bool, hlds_goal, hlds_goals).
:- mode can_move == (pred(in, in, in, in) is semidet).

    % Check all goals occurring later in the conjunction to see if they depend
    % on the current goal. A goal depends on the current goal if any of the
    % non-locals of the later goal have their instantiatedness changed
    % by the current goal.
    %
:- pred can_move_goal_forward(module_info::in, bool::in,
    hlds_goal::in, list(hlds_goal)::in) is semidet.

can_move_goal_forward(ModuleInfo, FullyStrict, ThisGoal, Goals) :-
    (
        list.member(LaterGoal, Goals)
    =>
        pd_can_reorder_goals(ModuleInfo, FullyStrict, ThisGoal, LaterGoal)
    ).

    % Check all goals occurring earlier in the conjunction to see
    % if the current goal depends on them.
    %
:- pred can_move_goal_backward(module_info::in, bool::in,
    hlds_goal::in, list(hlds_goal)::in) is semidet.

can_move_goal_backward(ModuleInfo, FullyStrict, ThisGoal, Goals) :-
    (
        list.member(EarlierGoal, Goals)
    =>
        pd_can_reorder_goals(ModuleInfo, FullyStrict,
            EarlierGoal, ThisGoal)
    ).

%-----------------------------------------------------------------------------%

    % Tack the second goal and the goals in between onto the end of each branch
    % of the first goal, unfolding the second goal in the branches which have
    % extra information about the arguments.
    %
:- pred push_goal_into_goal(set_of_progvar::in, set(int)::in,
    hlds_goal::in, hlds_goals::in, hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

push_goal_into_goal(NonLocals, DeforestInfo, EarlierGoal,
        BetweenGoals, LaterGoal, Goal, !PDInfo) :-
    pd_info_get_instmap(!.PDInfo, InstMap0),
    EarlierGoal = hlds_goal(EarlierGoalExpr, _),
    (
        EarlierGoalExpr = switch(Var1, CanFail1, Cases1),
        set_of_var.insert(Var1, NonLocals, CaseNonLocals),
        append_goal_to_cases(Var1, BetweenGoals, LaterGoal,
            CaseNonLocals, 1, DeforestInfo, Cases1, Cases, !PDInfo),
        GoalExpr = switch(Var1, CanFail1, Cases)
    ;
        EarlierGoalExpr = if_then_else(Vars, Cond, Then0, Else0),
        pd_info_update_goal(Cond, !PDInfo),
        Cond = hlds_goal(_, CondInfo),
        CondNonLocals = goal_info_get_nonlocals(CondInfo),
        set_of_var.union(CondNonLocals, NonLocals, ThenNonLocals),
        append_goal(Then0, BetweenGoals, LaterGoal,
            ThenNonLocals, 1, DeforestInfo, Then, !PDInfo),
        pd_info_set_instmap(InstMap0, !PDInfo),
        append_goal(Else0, BetweenGoals, LaterGoal,
            NonLocals, 2, DeforestInfo, Else, !PDInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        EarlierGoalExpr = disj(Disjuncts0),
        append_goal_to_disjuncts(BetweenGoals, LaterGoal,
            NonLocals, 1, DeforestInfo, Disjuncts0, Disjuncts, !PDInfo),
        GoalExpr = disj(Disjuncts)
    ;
        ( EarlierGoalExpr = unify(_, _, _, _, _)
        ; EarlierGoalExpr = plain_call(_, _, _, _, _, _)
        ; EarlierGoalExpr = generic_call(_, _, _, _, _)
        ; EarlierGoalExpr = call_foreign_proc(_, _, _, _, _, _, _)
        ; EarlierGoalExpr = conj(_, _)
        ; EarlierGoalExpr = negation(_)
        ; EarlierGoalExpr = scope(_, _)
        ; EarlierGoalExpr = shorthand(_)
        ),
        unexpected($pred, "unexpected goal type")
    ),
    pd_info_set_instmap(InstMap0, !PDInfo),
    goal_list_instmap_delta([EarlierGoal | BetweenGoals], Delta0),
    LaterGoal = hlds_goal(_, LaterInfo),
    Delta1 = goal_info_get_instmap_delta(LaterInfo),
    instmap_delta_apply_instmap_delta(Delta0, Delta1, test_size, Delta2),
    instmap_delta_restrict(NonLocals, Delta2, Delta),
    goal_list_determinism([EarlierGoal | BetweenGoals], Detism0),
    Detism1 = goal_info_get_determinism(LaterInfo),
    det_conjunction_detism(Detism0, Detism1, Detism),
    goal_list_purity([EarlierGoal | BetweenGoals], Purity0),
    Purity1 = goal_info_get_purity(LaterInfo),
    worst_purity(Purity0, Purity1) = Purity,
    goal_info_init(NonLocals, Delta, Detism, Purity, GoalInfo),
    Goal2 = hlds_goal(GoalExpr, GoalInfo),

    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    find_simplify_tasks(Globals, do_not_generate_warnings, SimplifyTasks0),
    SimpList0 = simplify_tasks_to_list(SimplifyTasks0),
    % Be a bit more aggressive with common structure elimination.
    % This helps achieve folding in some cases.
    SimpList = [simptask_extra_common_structs | SimpList0],
    SimplifyTasks = list_to_simplify_tasks(Globals, SimpList),
    pd_util.pd_simplify_goal(SimplifyTasks, Goal2, Goal3, !PDInfo),
    pd_info_set_instmap(InstMap0, !PDInfo),

    % Perform any folding which may now be possible.
    deforest_goal(Goal3, Goal, !PDInfo),
    pd_info_set_instmap(InstMap0, !PDInfo).

:- pred append_goal_to_disjuncts(hlds_goals::in, hlds_goal::in,
    set_of_progvar::in, int::in, set(int)::in, hlds_goals::in, hlds_goals::out,
    pd_info::in, pd_info::out) is det.

append_goal_to_disjuncts(_, _, _, _, _, [], [], !PDInfo).
append_goal_to_disjuncts(BetweenGoals, GoalToAppend, NonLocals,
        CurrBranch, Branches, [Goal0 | Goals0], [Goal | Goals], !PDInfo) :-
    pd_info_get_instmap(!.PDInfo, InstMap0),
    append_goal(Goal0, BetweenGoals, GoalToAppend,
        NonLocals, CurrBranch, Branches, Goal, !PDInfo),
    NextBranch = CurrBranch + 1,
    pd_info_set_instmap(InstMap0, !PDInfo),
    append_goal_to_disjuncts(BetweenGoals, GoalToAppend,
        NonLocals, NextBranch, Branches, Goals0, Goals, !PDInfo).

:- pred append_goal_to_cases(prog_var::in, hlds_goals::in,
    hlds_goal::in, set_of_progvar::in, int::in, set(int)::in,
    list(case)::in,list(case)::out, pd_info::in, pd_info::out) is det.

append_goal_to_cases(_, _, _, _, _, _, [], [], !PDInfo).
append_goal_to_cases(Var, BetweenGoals, GoalToAppend, NonLocals,
        CurrCase, Branches, [Case0 | Cases0], [Case | Cases], !PDInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    pd_info_get_instmap(!.PDInfo, InstMap0),
    pd_info_bind_var_to_functors(Var, MainConsId, OtherConsIds, !PDInfo),
    append_goal(Goal0, BetweenGoals, GoalToAppend, NonLocals,
        CurrCase, Branches, Goal, !PDInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    NextCase = CurrCase + 1,
    pd_info_set_instmap(InstMap0, !PDInfo),
    append_goal_to_cases(Var, BetweenGoals, GoalToAppend,
        NonLocals, NextCase, Branches, Cases0, Cases, !PDInfo).

:- pred append_goal(hlds_goal::in, hlds_goals::in,
    hlds_goal::in, set_of_progvar::in, int::in, set(int)::in,
    hlds_goal::out, pd_info::in, pd_info::out) is det.

append_goal(Goal0, BetweenGoals, GoalToAppend0, NonLocals0,
        CurrBranch, Branches, Goal, !PDInfo) :-
    ( if set.member(CurrBranch, Branches) then
        % Unfold the call.
        pd_info_get_instmap(!.PDInfo, InstMap0),
        list.foldl(pd_info_update_goal, [Goal0 | BetweenGoals], !PDInfo),
        deforest_goal(GoalToAppend0, GoalToAppend, !PDInfo),
        pd_info_set_instmap(InstMap0, !PDInfo)
    else
        GoalToAppend = GoalToAppend0
    ),
    goal_to_conj_list(Goal0, GoalList0),
    goal_to_conj_list(GoalToAppend, GoalListToAppend),
    list.condense([GoalList0, BetweenGoals, GoalListToAppend], Goals),

    goal_list_nonlocals(Goals, SubNonLocals),
    set_of_var.intersect(NonLocals0, SubNonLocals, NonLocals),
    goal_list_instmap_delta(Goals, Delta0),
    instmap_delta_restrict(NonLocals, Delta0, Delta),
    goal_list_determinism(Goals, Detism),
    goal_list_purity(Goals, Purity),
    goal_info_init(NonLocals, Delta, Detism, Purity, GoalInfo),
    Goal = hlds_goal(conj(plain_conj, Goals), GoalInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred deforest_call(pred_id::in, proc_id::in, list(prog_var)::in,
    sym_name::in, builtin_state::in, hlds_goal::in, hlds_goal::out,
    pd_info::in, pd_info::out) is det.

deforest_call(PredId, ProcId, Args, SymName, BuiltinState, Goal0, Goal,
        !PDInfo) :-
    pd_info_get_proc_arg_info(!.PDInfo, ProcArgInfos),
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    pd_info_get_instmap(!.PDInfo, InstMap),
    Name = unqualify_name(SymName),
    list.length(Args, Arity),
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Context = goal_info_get_context(GoalInfo0),

    pd_info_get_local_term_info(!.PDInfo, LocalTermInfo0),

    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    SizeThreshold = OptTuple ^ ot_deforestation_size_threshold,
    ( if
        % Check for extra information to the call.
        map.search(ProcArgInfos, proc(PredId, ProcId), ProcArgInfo),
        ProcArgInfo = pd_branch_info(_, LeftArgs, _),
        set.member(LeftArg, LeftArgs),
        list.det_index1(Args, LeftArg, Arg),
        instmap_lookup_var(InstMap, Arg, ArgInst),
        inst_is_bound_to_functors(ModuleInfo, ArgInst, [_]),

        % We don't attempt to deforest predicates which have purity promises
        % because the extra impurity propagated through the goal when such
        % predicates are inlined will defeat any attempt at deforestation.
        % XXX We should probably allow deforestation of semipure goals.

        inlining.can_inline_proc(ModuleInfo, PredId, ProcId, BuiltinState,
            may_not_inline_purity_promised_pred),

        % Check the goal size.
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId, _,
            CalledProcInfo),
        proc_info_get_goal(CalledProcInfo, CalledGoal),
        goal_size(CalledGoal, CalledGoalSize),
        ( SizeThreshold = -1
        ; CalledGoalSize < SizeThreshold
        )
    then
        trace [io(!IO)] (
            pd_debug_message_context(!.PDInfo, Context,
                "Found extra information for call to %s/%i\n",
                [s(Name), i(Arity)], !IO)
        ),
        ( if
            pd_term.local_check(ModuleInfo, Goal0, InstMap,
                LocalTermInfo0, LocalTermInfo)
        then
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "Local termination check succeeded\n", [], !IO)
            ),
            pd_info_set_local_term_info(LocalTermInfo, !PDInfo),
            unfold_call(yes, yes, PredId, ProcId,
                Args, Goal0, Goal1, Optimized, !PDInfo),
            (
                Optimized = yes,
                deforest_goal(Goal1, Goal, !PDInfo)
            ;
                Optimized = no,
                Goal = Goal1
            ),
            pd_info_set_local_term_info(LocalTermInfo0, !PDInfo)
        else
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "Local termination check failed\n", [], !IO)
            ),
            Goal = hlds_goal(GoalExpr0, GoalInfo0)
        )
    else
        trace [io(!IO)] (
            pd_debug_message_context(!.PDInfo, Context,
                "No extra information for call to %s/%i\n",
                [s(Name), i(Arity)], !IO)
        ),
        Goal = Goal0
    ).

:- pred unfold_call(bool::in, bool::in, pred_id::in, proc_id::in,
    list(prog_var)::in, hlds_goal::in, hlds_goal::out, bool::out,
    pd_info::in, pd_info::out) is det.

unfold_call(CheckImprovement, CheckVars, PredId, ProcId, Args,
        Goal0, Goal, Optimized, !PDInfo) :-
    pd_info_get_module_info(!.PDInfo, ModuleInfo),
    module_info_get_globals(ModuleInfo, Globals),
    globals.get_opt_tuple(Globals, OptTuple),
    VarsThreshold = OptTuple ^ ot_deforestation_vars_threshold,
    pd_info_get_proc_info(!.PDInfo, ProcInfo0),
    proc_info_get_var_table(ProcInfo0, VarTable0),
    var_table_count(VarTable0, NumVars),
    ( if
        % Check that we haven't already got too many variables.
        (
            CheckVars = no
        ;
            VarsThreshold = -1
            % XXX The *default* value of the vars threshold is 200,
            % so -1 does NOT mean that there is no threshold set.
        ;
            NumVars < VarsThreshold
        )
    then
        pd_info_get_pred_info(!.PDInfo, PredInfo0),
        module_info_pred_proc_info(ModuleInfo, PredId, ProcId,
            CalledPredInfo, CalledProcInfo),
        pred_info_get_typevarset(PredInfo0, TypeVarSet0),
        pred_info_get_univ_quant_tvars(PredInfo0, UnivQVars),
        proc_info_get_rtti_varmaps(ProcInfo0, RttiVarMaps0),
        inlining.do_inline_call(ModuleInfo, UnivQVars, Args,
            CalledPredInfo, CalledProcInfo, TypeVarSet0, TypeVarSet,
            VarTable0, VarTable, RttiVarMaps0, RttiVarMaps, Goal1),
        pred_info_set_typevarset(TypeVarSet, PredInfo0, PredInfo),
        proc_info_get_has_parallel_conj(CalledProcInfo, CalledHasParallelConj),

        proc_info_set_var_table(VarTable, ProcInfo0, ProcInfo1),
        proc_info_set_rtti_varmaps(RttiVarMaps, ProcInfo1, ProcInfo2),
        (
            CalledHasParallelConj = has_parallel_conj,
            proc_info_set_has_parallel_conj(has_parallel_conj,
                ProcInfo2, ProcInfo)
        ;
            CalledHasParallelConj = has_no_parallel_conj,
            % Leave the has_parallel_conj field of the proc_info as it is.
            ProcInfo = ProcInfo2
        ),

        pd_info_set_pred_info(PredInfo, !PDInfo),
        pd_info_set_proc_info(ProcInfo, !PDInfo),

        goal_cost(Goal1, OriginalCost),
        pd_info_get_cost_delta(!.PDInfo, CostDelta0),
        pd_info_get_size_delta(!.PDInfo, SizeDelta0),
        pd_info_get_changed(!.PDInfo, Changed0),

        % Update the quantification if not all the output arguments are used.
        Goal1 = hlds_goal(_, GoalInfo1),
        NonLocals1 = goal_info_get_nonlocals(GoalInfo1),
        set_of_var.list_to_set(Args, NonLocals),
        ( if set_of_var.equal(NonLocals1, NonLocals) then
            Goal2 = Goal1
        else
            pd_requantify_goal(NonLocals, Goal1, Goal2, !PDInfo)
        ),

        % Push the extra information from the call through the goal.
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "Running unique modes\n", [], !IO)
        ),
        proc_info_arglives(ModuleInfo, CalledProcInfo, ArgLives),
        get_live_vars(Args, ArgLives, LiveVars0),
        set_of_var.list_to_set(LiveVars0, LiveVars1),
        set_of_var.intersect(NonLocals, LiveVars1, LiveVars),
        pd_util.unique_modecheck_goal_live_vars(LiveVars, Goal2, Goal3, Errors,
            !PDInfo),

        (
            Errors = [],
            Optimized0 = yes
        ;
            Errors = [_ | _],
            % This can happen because common.m does not maintain unique mode
            % correctness. This should eventually be fixed.
            Optimized0 = no
        ),

        trace [io(!IO)] (
            pd_debug_message(!.PDInfo, "Running simplify\n", [], !IO)
        ),
        find_simplify_tasks(Globals, do_not_generate_warnings, SimplifyTasks),
        pd_util.pd_simplify_goal(SimplifyTasks, Goal3, Goal4, !PDInfo),

        pd_info_get_cost_delta(!.PDInfo, CostDelta1),
        CostDelta = CostDelta1 - CostDelta0,
        goal_size(Goal4, GoalSize),
        SizeDelta = GoalSize - cost_of_call,
        Factor = OptTuple ^ ot_deforestation_cost_factor,
        ( if
            Optimized0 = yes,
            (
                CheckImprovement = no
            ;
                CheckImprovement = yes,
                % XXX Should this test Goal4? zs
                ( if is_simple_goal(Goal3) then
                    true
                else
                    check_improvement(Factor, GoalSize, OriginalCost,
                        CostDelta)
                )
            )
        then
            Goal = Goal4,
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "inlined: cost(%i) size(%i)\n",
                    [i(CostDelta), i(SizeDelta)], !IO)
            ),
            pd_info_incr_size_delta(SizeDelta, !PDInfo),
            pd_info_set_changed(yes, !PDInfo),
            Goal0 = hlds_goal(_, GoalInfo0),
            Det0 = goal_info_get_determinism(GoalInfo0),
            Goal = hlds_goal(_, GoalInfo),
            Det = goal_info_get_determinism(GoalInfo),

            % Rerun determinism analysis later if the determinism of any of
            % the sub-goals changes - this avoids problems with inlining
            % erroneous predicates.
            ( if Det = Det0 then
                true
            else
                pd_info_set_rerun_det(yes, !PDInfo)
            ),

            Optimized = yes
        else
            trace [io(!IO)] (
                pd_debug_message(!.PDInfo,
                    "not enough improvement - " ++
                    "not inlining: cost(%i) size(%i)\n",
                    [i(CostDelta), i(SizeDelta)], !IO)
            ),
            pd_info_set_pred_info(PredInfo0, !PDInfo),
            pd_info_set_proc_info(ProcInfo0, !PDInfo),
            pd_info_set_size_delta(SizeDelta0, !PDInfo),
            pd_info_set_cost_delta(CostDelta0, !PDInfo),
            pd_info_set_changed(Changed0, !PDInfo),
            Goal = Goal0,
            Optimized = no
        )
    else
        trace [io(!IO)] (
            pd_debug_message(!.PDInfo,
                "too many variables - not inlining\n", [], !IO)
        ),
        Goal = Goal0,
        Optimized = no
    ).

%-----------------------------------------------------------------------------%

:- pred is_simple_goal_list(list(hlds_goal)::in) is semidet.

is_simple_goal_list([]).
is_simple_goal_list([Goal | Goals]) :-
    is_simple_goal(Goal),
    is_simple_goal_list(Goals).

:- pred is_simple_goal(hlds_goal::in) is semidet.

is_simple_goal(hlds_goal(GoalExpr, _)) :-
    (
        goal_expr_has_subgoals(GoalExpr) = does_not_have_subgoals
    ;
        GoalExpr = negation(Goal1),
        % Handle a call or builtin + tests on the output.
        goal_to_conj_list(Goal1, GoalList1),
        is_simple_goal_list(GoalList1)
    ).

%-----------------------------------------------------------------------------%

    % Very rough heuristics for checking improvement. This should lean
    % towards allowing optimizations.
    %
:- pred check_improvement(int::in, int::in, int::in, int::in) is semidet.

check_improvement(_Factor, Size, OriginalCost, CostDelta) :-
    ( if Size =< 5 then
        % For small increases in size, accept any amount of optimization.
        CostDelta > 0
    else
        PercentChange = CostDelta * 100 // OriginalCost,
        PercentChange >= 5
    ).

:- pred check_deforestation_improvement(int::in, int::in, int::in)
    is semidet.

check_deforestation_improvement(Factor, CostDelta, SizeChange) :-
    ( if SizeChange =< 5 then
        % For small increases in size, accept any amount of optimization.
        CostDelta > 0
    else
        % Accept the optimization if we save the equivalent of a heap increment
        % per 3 extra atomic goals. Note that folding is heavily rewarded by
        % pd_cost.m, so this isn't very restrictive if a fold occurs.
        ExpectedCostDelta = 1000 * cost_of_heap_incr * SizeChange // 3,
        FudgedCostDelta = CostDelta * Factor,
        FudgedCostDelta >= ExpectedCostDelta
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.deforest.
%-----------------------------------------------------------------------------%
