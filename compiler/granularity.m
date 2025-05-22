%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% Copyright (C) 2013-2020, 2022, 2025 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: granularity.m.
% Author: zs.
%
% Find every parallel conjunction G1 & G2 & G3 & ... & Gn in the given module,
% and replace it with a goal that tests at runtime whether there are enough
% free CPUs to make parallel execution worthwhile, and if not, executes
% the conjunction sequentially instead.
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.granularity.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.

%-----------------------------------------------------------------------------%

:- pred control_granularity(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_refs.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.

:- import_module bool.
:- import_module list.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.

%-----------------------------------------------------------------------------%

control_granularity(!ModuleInfo) :-
    module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl(runtime_granularity_test_in_scc, SCCs, !ModuleInfo).

:- pred runtime_granularity_test_in_scc(set(pred_proc_id)::in,
    module_info::in, module_info::out) is det.

runtime_granularity_test_in_scc(SCC, !ModuleInfo) :-
    set.foldl(runtime_granularity_test_in_proc(SCC), SCC, !ModuleInfo).

:- pred runtime_granularity_test_in_proc(scc::in, pred_proc_id::in,
    module_info::in, module_info::out) is det.

runtime_granularity_test_in_proc(SCC, proc(PredId, ProcId), !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_proc_info(PredInfo0, ProcId, ProcInfo0),
    proc_info_get_has_parallel_conj(ProcInfo0, HasParallelConj),
    (
        HasParallelConj = has_parallel_conj,
        proc_info_get_goal(ProcInfo0, Goal0),
        runtime_granularity_test_in_goal(Goal0, Goal, no, Changed,
            SCC, !.ModuleInfo),
        (
            Changed = no
        ;
            Changed = yes,
            proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
            requantify_proc_general(ord_nl_no_lambda, ProcInfo1, ProcInfo),
            pred_info_set_proc_info(ProcId, ProcInfo, PredInfo0, PredInfo),
            module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
        )
    ;
        HasParallelConj = has_no_parallel_conj
        % There is no parallelism in this procedure, so there is no granularity
        % to control.
    ).

:- pred runtime_granularity_test_in_goal(hlds_goal::in, hlds_goal::out,
    bool::in, bool::out, scc::in, module_info::in) is det.

runtime_granularity_test_in_goal(Goal0, Goal, !Changed, SCC, ModuleInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo),
    (
        GoalExpr0 = conj(parallel_conj, Goals0),
        runtime_granularity_test_in_goals(Goals0, Goals, !Changed, SCC,
            ModuleInfo),
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_target(Globals, Target),
        (
            Target = target_c,
            ModuleName = mercury_par_builtin_module,
            CalledSCCPredProcIds = goal_list_calls_proc_in_set(Goals, SCC),
            ( if set.is_empty(CalledSCCPredProcIds) then
                GoalExpr = conj(parallel_conj, Goals)
            else
                ProcName = "evaluate_parallelism_condition",
                Args = [],
                ExtraArgs = [],
                MaybeRuntimeCond = no,
                Features = [],
                Context = goal_info_get_context(GoalInfo),
                some [!Attributes] (
                    !:Attributes = default_attributes(lang_c),
                    set_thread_safe(proc_thread_safe, !Attributes),
                    set_purity(purity_impure, !Attributes),
                    set_may_call_mercury(proc_will_not_call_mercury,
                        !Attributes),
                    set_terminates(proc_terminates, !Attributes),
                    set_may_throw_exception(proc_will_not_throw_exception,
                        !Attributes),
                    set_may_call_mm_tabled(proc_will_not_call_mm_tabled,
                        !Attributes),
                    set_may_modify_trail(proc_will_not_modify_trail,
                        !Attributes),
                    Attributes = !.Attributes
                ),
                generate_call_foreign_proc(ModuleInfo, pf_predicate,
                    ModuleName, ProcName,
                    [], Args, ExtraArgs, instmap_delta_bind_no_var, only_mode,
                    detism_semi, purity_impure, Features, Attributes,
                    MaybeRuntimeCond, runtime_test_code, Context, Cond),

                Then = hlds_goal(conj(parallel_conj, Goals), GoalInfo),
                Else = hlds_goal(conj(plain_conj, Goals), GoalInfo),
                IfThenElse = hlds_goal(if_then_else([], Cond, Then, Else),
                    GoalInfo),
                Reason = promise_purity(purity_pure),
                GoalExpr = scope(Reason, IfThenElse),
                !:Changed = yes
            )
        ;
            ( Target = target_csharp
            ; Target = target_java
            ),
            % This should have caught by mercury_compile.m.
            unexpected($pred, "unsupported target language")
        )
    ;
        GoalExpr0 = conj(plain_conj, Goals0),
        runtime_granularity_test_in_goals(Goals0, Goals, !Changed, SCC,
            ModuleInfo),
        GoalExpr = conj(plain_conj, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        runtime_granularity_test_in_goals(Goals0, Goals, !Changed, SCC,
            ModuleInfo),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        runtime_granularity_test_in_cases(Cases0, Cases, !Changed, SCC,
            ModuleInfo),
        GoalExpr = switch(Var, CanFail, Cases)
    ;
        GoalExpr0 = if_then_else(Vars, Cond0, Then0, Else0),
        runtime_granularity_test_in_goal(Cond0, Cond, !Changed, SCC,
            ModuleInfo),
        runtime_granularity_test_in_goal(Then0, Then, !Changed, SCC,
            ModuleInfo),
        runtime_granularity_test_in_goal(Else0, Else, !Changed, SCC,
            ModuleInfo),
        GoalExpr = if_then_else(Vars, Cond, Then, Else)
    ;
        GoalExpr0 = negation(SubGoal0),
        runtime_granularity_test_in_goal(SubGoal0, SubGoal, !Changed, SCC,
            ModuleInfo),
        GoalExpr = negation(SubGoal)
    ;
        GoalExpr0 = scope(Reason, SubGoal0),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            GoalExpr = GoalExpr0
        else
            runtime_granularity_test_in_goal(SubGoal0, SubGoal, !Changed, SCC,
                ModuleInfo),
            GoalExpr = scope(Reason, SubGoal)
        )
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred runtime_granularity_test_in_goals(
    list(hlds_goal)::in, list(hlds_goal)::out, bool::in, bool::out,
    scc::in, module_info::in) is det.

runtime_granularity_test_in_goals([], [], !Changed, _, _).
runtime_granularity_test_in_goals([Goal0 | Goals0], [Goal | Goals], !Changed,
        SCC, ModuleInfo) :-
    runtime_granularity_test_in_goal(Goal0, Goal, !Changed, SCC, ModuleInfo),
    runtime_granularity_test_in_goals(Goals0, Goals, !Changed, SCC,
        ModuleInfo).

:- pred runtime_granularity_test_in_cases( list(case)::in, list(case)::out,
    bool::in, bool::out, scc::in, module_info::in) is det.

runtime_granularity_test_in_cases([], [], !Changed, _, _).
runtime_granularity_test_in_cases([Case0 | Cases0], [Case | Cases], !Changed,
        SCC, ModuleInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    runtime_granularity_test_in_goal(Goal0, Goal, !Changed, SCC, ModuleInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    runtime_granularity_test_in_cases(Cases0, Cases, !Changed, SCC,
        ModuleInfo).

%-----------------------------------------------------------------------------%

:- func runtime_test_code = string.

runtime_test_code =
    "SUCCESS_INDICATOR = MR_par_cond_local_wsdeque_length;\n" ++
    "#ifdef MR_DEBUG_RUNTIME_GRANULARITY_CONTROL\n" ++
    "MR_record_conditional_parallelism_decision(SUCCESS_INDICATOR);\n" ++
    "#endif\n".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.granularity.
%-----------------------------------------------------------------------------%
