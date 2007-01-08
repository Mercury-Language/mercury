%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2007 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
% 
% File: granularity.m.
% Author: zs.
% 
%-----------------------------------------------------------------------------%

:- module transform_hlds.granularity.
:- interface.

:- import_module hlds.hlds_module.

%-----------------------------------------------------------------------------%

:- pred control_granularity(module_info::in, module_info::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_goal.
:- import_module hlds.pred_table.
:- import_module hlds.quantification.
:- import_module libs.compiler_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module transform_hlds.dependency_graph.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module parse_tree.prog_data.

:- import_module bool.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module string.

%-----------------------------------------------------------------------------%

control_granularity(!ModuleInfo) :-
    module_info_rebuild_dependency_info(!ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl(runtime_granularity_test_in_scc, SCCs, !ModuleInfo).

:- pred runtime_granularity_test_in_scc(list(pred_proc_id)::in,
    module_info::in, module_info::out) is det.

runtime_granularity_test_in_scc(SCC, !ModuleInfo) :-
    list.foldl(runtime_granularity_test_in_proc(SCC), SCC, !ModuleInfo).

:- pred runtime_granularity_test_in_proc(list(pred_proc_id)::in,
    pred_proc_id::in, module_info::in, module_info::out) is det.

runtime_granularity_test_in_proc(SCC, proc(PredId, ProcId), !ModuleInfo) :-
    module_info_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    proc_info_get_has_parallel_conj(ProcInfo0, HasParallelConj),
    (
        HasParallelConj = yes,
        proc_info_get_goal(ProcInfo0, Goal0),
        runtime_granularity_test_in_goal(Goal0, Goal, no, Changed,
            SCC, !.ModuleInfo),
        (
            Changed = no
        ;
            Changed = yes,
            proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
            requantify_proc(ProcInfo1, ProcInfo),
            map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable),
            pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
            map.det_update(PredTable0, PredId, PredInfo, PredTable),
            module_info_set_preds(PredTable, !ModuleInfo)
        )
    ;
        HasParallelConj = no
        % There is no parallelism in this procedure, so there is no granularity
        % to control.
    ).

:- pred runtime_granularity_test_in_goal(hlds_goal::in, hlds_goal::out,
    bool::in, bool::out, list(pred_proc_id)::in, module_info::in) is det.

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
            CalledSCCPredProcIds = goal_list_calls_proc_in_list(Goals, SCC),
            (
                CalledSCCPredProcIds = [],
                GoalExpr = conj(parallel_conj, Goals)
            ;
                CalledSCCPredProcIds = [_ | _],
                ProcName = "evaluate_parallelism_condition",
                globals.lookup_int_option(Globals, parallelism_target,
                    NumCPUs),
                NumCPUsStr = string.int_to_string(NumCPUs),
                Code = "SUCCESS_INDICATOR = " ++
                    "MR_choose_parallel_over_sequential_cond(" ++
                    NumCPUsStr ++ ");",
                Args = [],
                ExtraArgs = [],
                MaybeRuntimeCond = no,
                Features = [],
                InstMapDeltaSrc = [],
                goal_info_get_context(GoalInfo, Context),
                some [!Attributes] (
                    !:Attributes = default_attributes(lang_c),
                    set_purity(purity_impure, !Attributes),
                    set_may_call_mercury(proc_will_not_call_mercury,
                        !Attributes),
                    set_terminates(proc_terminates, !Attributes),
                    set_may_throw_exception(proc_will_not_throw_exception,
                        !Attributes),
                    set_may_call_mm_tabled(will_not_call_mm_tabled,
                        !Attributes),
                    set_may_modify_trail(proc_will_not_modify_trail,
                        !Attributes),
                    Attributes = !.Attributes
                ),
                generate_foreign_proc(ModuleName, ProcName, predicate,
                    only_mode, detism_semi, purity_impure, Attributes,
                    Args, ExtraArgs, MaybeRuntimeCond, Code, Features,
                    InstMapDeltaSrc, ModuleInfo, Context, Cond),

                Then = hlds_goal(conj(parallel_conj, Goals), GoalInfo),
                Else = hlds_goal(conj(plain_conj, Goals), GoalInfo),
                IfThenElse = hlds_goal(if_then_else([], Cond, Then, Else),
                    GoalInfo),
                Reason = promise_purity(dont_make_implicit_promises,
                    purity_pure),
                GoalExpr = scope(Reason, IfThenElse),
                !:Changed = yes
            )
        ;
            ( Target = target_il
            ; Target = target_java
            ; Target = target_asm
            ; Target = target_x86_64
            ),
            % This should have caught by mercury_compile.m.
            unexpected(this_file, "runtime_granularity_test_in_goal: " ++
                "unsupported target language")
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
        runtime_granularity_test_in_goal(SubGoal0, SubGoal, !Changed, SCC,
            ModuleInfo),
        GoalExpr = scope(Reason, SubGoal)
    ;
        ( GoalExpr0 = plain_call(_, _, _, _, _, _)
        ; GoalExpr0 = generic_call(_, _, _, _)
        ; GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
        ; GoalExpr0 = unify(_, _, _, _, _)
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "runtime_granularity_test_in_goal: shorthand")
    ),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred runtime_granularity_test_in_goals(
    list(hlds_goal)::in, list(hlds_goal)::out, bool::in, bool::out,
    list(pred_proc_id)::in, module_info::in) is det.

runtime_granularity_test_in_goals([], [], !Changed, _, _).
runtime_granularity_test_in_goals([Goal0 | Goals0], [Goal | Goals], !Changed,
        SCC, ModuleInfo) :-
    runtime_granularity_test_in_goal(Goal0, Goal, !Changed, SCC, ModuleInfo),
    runtime_granularity_test_in_goals(Goals0, Goals, !Changed, SCC,
        ModuleInfo).

:- pred runtime_granularity_test_in_cases( list(case)::in, list(case)::out,
    bool::in, bool::out, list(pred_proc_id)::in, module_info::in) is det.

runtime_granularity_test_in_cases([], [], !Changed, _, _).
runtime_granularity_test_in_cases([Case0 | Cases0], [Case | Cases], !Changed,
        SCC, ModuleInfo) :-
    Case0 = case(ConsId, Goal0),
    runtime_granularity_test_in_goal(Goal0, Goal, !Changed, SCC, ModuleInfo),
    Case = case(ConsId, Goal),
    runtime_granularity_test_in_cases(Cases0, Cases, !Changed, SCC,
        ModuleInfo).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "granularity.m".

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.granularity.
%-----------------------------------------------------------------------------%
