%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2012 The University of Melbourne.
% Copyright (C) 2015, 2017 The Mercury team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: deep_profiling.m.
% Main author: conway.
%
% This module applies the deep profiling transformation described in the paper
% ``Engineering a profiler for a logic programming language'' by Thomas Conway
% and Zoltan Somogyi.
%
%-----------------------------------------------------------------------------%

:- module ll_backend.deep_profiling.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module hlds.vartypes.
:- import_module parse_tree.
:- import_module parse_tree.prog_data.

:- import_module list.
:- import_module maybe.

:- pred apply_deep_profiling_transform(module_info::in, module_info::out)
    is det.

:- pred generate_deep_call(module_info::in, string::in, int::in,
    list(prog_var)::in, maybe(list(prog_var))::in, determinism::in,
    hlds_goal::out) is det.

:- pred generate_deep_const_unify(cons_id::in, prog_var::in, hlds_goal::out)
    is det.

    % Create a variable with the given name and type, adding it to the
    % prog_var_set_types structure.
    %
:- pred generate_var(string::in, mer_type::in, prog_var::out,
    prog_var_set_types::in, prog_var_set_types::out) is det.

:- pred get_deep_profile_builtin_ppid(module_info::in, string::in, int::in,
    pred_id::out, proc_id::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_top_functor.
:- import_module hlds.code_model.
:- import_module hlds.goal_path.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.make_goal.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ll_backend.coverage_profiling.
:- import_module mdbcomp.
:- import_module mdbcomp.builtin_modules.
:- import_module mdbcomp.goal_path.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.builtin_lib_types.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.
:- import_module transform_hlds.dead_proc_elim.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module map.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

apply_deep_profiling_transform(!ModuleInfo) :-
    % XXX The dead proc elimination pass changes the status of opt_imported
    % predicates, which changes what labels they get generated. The
    % call_site_static structures we generate must match the labels created
    % during code generation.
    dead_proc_elim(elim_opt_imported, !ModuleInfo),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, deep_profile_tail_recursion,
        TailRecursion),
    (
        TailRecursion = yes,
        apply_deep_prof_tail_rec_transform(!ModuleInfo)
    ;
        TailRecursion = no
    ),
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    module_info_get_pred_id_table(!.ModuleInfo, PredIdTable0),
    list.foldl(deep_prof_transform_pred(!.ModuleInfo), PredIds,
        PredIdTable0, PredIdTable),
    module_info_set_pred_id_table(PredIdTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred apply_deep_prof_tail_rec_transform(module_info::in, module_info::out)
    is det.

apply_deep_prof_tail_rec_transform(!ModuleInfo) :-
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl(apply_deep_prof_tail_rec_transform_to_scc, SCCs, !ModuleInfo).

:- pred apply_deep_prof_tail_rec_transform_to_scc(scc::in,
    module_info::in, module_info::out) is det.

apply_deep_prof_tail_rec_transform_to_scc(SCC, !ModuleInfo) :-
    % For the time being, we only look for self-tail-recursive calls.
    % If the SCC contains more than one procedure, a self-tail-recursive
    % call in Proc A could end up calling the other procedure Proc B
    % in the SCC, which could then call back to Proc A. This would screw up
    % our bookkeeping.
    ( if set.is_singleton(SCC, PredProcId) then
        apply_deep_prof_tail_rec_transform_to_proc(PredProcId, !ModuleInfo)
    else
        true
    ).

:- pred apply_deep_prof_tail_rec_transform_to_proc(pred_proc_id::in,
    module_info::in, module_info::out) is det.

apply_deep_prof_tail_rec_transform_to_proc(PredProcId, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    pred_info_get_arg_types(PredInfo0, Types),
    pred_info_get_origin(PredInfo0, Origin),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),
    proc_info_interface_determinism(ProcInfo0, Detism),
    ( if
        determinism_components(Detism, _CanFail, SolnCount),
        SolnCount \= at_most_many,
        proc_info_get_headvars(ProcInfo0, HeadVars),
        proc_info_get_argmodes(ProcInfo0, Modes),
        find_list_of_output_args(HeadVars, Modes, Types, !.ModuleInfo,
            Outputs),
        clone_proc_id(ProcTable0, ProcId, CloneProcId),
        ClonePredProcId = proc(PredId, CloneProcId),
        TailRecInfo = deep_prof_tail_rec_info(!.ModuleInfo,
            [PredProcId - ClonePredProcId], Detism, Outputs),
        apply_deep_prof_tail_rec_to_goal(Goal0, Goal, TailRecInfo,
            no, FoundTailCall, _Continue),
        FoundTailCall = yes,
        % The unification or comparison procedure for a type can be called
        % from builtin.unify or builtin.compare respectively, through the
        % type_ctor_info of an argument type. This means that we cannot
        % guarantee that a unification or comparison procedure is alone
        % in its SCC unless it cannot call builtin.unify and builtin.compare.
        (
            Origin = origin_special_pred(_, _)
        =>
            goal_contains_builtin_unify_or_compare(Goal) = no
        )
    then
        proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
        figure_out_rec_call_numbers(Goal, 0, _Num, [], TailCallSites),
        OrigDeepRecInfo = yes(deep_recursion_info(
            deep_prof_outer_proc(ClonePredProcId),
            [visible_scc_data(PredProcId, ClonePredProcId, TailCallSites)])),
        make_deep_original_body(!.ModuleInfo, ProcInfo0, DeepOriginalBody),
        OrigDeepProfileInfo = deep_profile_proc_info(OrigDeepRecInfo, no,
            DeepOriginalBody),
        CloneDeepRecInfo = yes(deep_recursion_info(
            deep_prof_inner_proc(PredProcId),
            [visible_scc_data(PredProcId, ClonePredProcId, TailCallSites)])),
        CloneDeepProfileInfo = deep_profile_proc_info(CloneDeepRecInfo, no,
            DeepOriginalBody),
        proc_info_set_maybe_deep_profile_info(yes(OrigDeepProfileInfo),
            ProcInfo1, ProcInfo),
        proc_info_set_maybe_deep_profile_info(yes(CloneDeepProfileInfo),
            ProcInfo1, CloneProcInfo),
        map.det_update(ProcId, ProcInfo, ProcTable0, ProcTable1),
        map.det_insert(CloneProcId, CloneProcInfo, ProcTable1, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    else
        true
    ).

:- pred find_list_of_output_args(list(prog_var)::in, list(mer_mode)::in,
    list(mer_type)::in, module_info::in, list(prog_var)::out) is det.

find_list_of_output_args(Vars, Modes, Types, ModuleInfo, !:Outputs) :-
    ( if
        find_list_of_output_args_2(Vars, Modes, Types, ModuleInfo, !:Outputs)
    then
        true
    else
        unexpected($pred, "list length mismatch")
    ).

:- pred find_list_of_output_args_2(list(prog_var)::in, list(mer_mode)::in,
    list(mer_type)::in, module_info::in, list(prog_var)::out) is semidet.

find_list_of_output_args_2([], [], [], _, []).
find_list_of_output_args_2([Var | Vars], [Mode | Modes], [Type | Types],
        ModuleInfo, Outputs) :-
    find_list_of_output_args_2(Vars, Modes, Types, ModuleInfo, LaterOutputs),
    mode_to_top_functor_mode(ModuleInfo, Mode, Type, TopFunctorMode),
    (
        TopFunctorMode = top_in,
        Outputs = LaterOutputs
    ;
        ( TopFunctorMode = top_out
        ; TopFunctorMode = top_unused
        ),
        Outputs = [Var | LaterOutputs]
    ).

:- func goal_contains_builtin_unify_or_compare(hlds_goal) = bool.

goal_contains_builtin_unify_or_compare(Goal) = Contains :-
    Goal = hlds_goal(GoalExpr, _GoalInfo),
    (
        GoalExpr = unify(_, _, _, _, _),
        Contains = no
    ;
        ( GoalExpr = generic_call(_, _, _, _, _)
        ; GoalExpr = plain_call(_, _, _, _, _, _)
        ),
        % Unfortunately, even if the procedure we are calling is neither
        % builtin.unify nor builtin.compare, we cannot know whether it
        % can call those predicates directly or indirectly.
        Contains = yes
    ;
        GoalExpr = call_foreign_proc(Attributes, _, _, _, _, _, _),
        MayCallMercury = get_may_call_mercury(Attributes),
        (
            MayCallMercury = proc_will_not_call_mercury,
            Contains = no
        ;
            MayCallMercury = proc_may_call_mercury,
            % The Mercury code may call builtin.unify or builtin.compare.
            Contains = yes
        )
    ;
        ( GoalExpr = conj(_, Goals)
        ; GoalExpr = disj(Goals)
        ),
        Contains = goals_contain_builtin_unify_or_compare(Goals)
    ;
        GoalExpr = switch(_, _, Cases),
        Contains = cases_contain_builtin_unify_or_compare(Cases)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        ( if
            goal_contains_builtin_unify_or_compare(Cond) = no,
            goal_contains_builtin_unify_or_compare(Then) = no,
            goal_contains_builtin_unify_or_compare(Else) = no
        then
            Contains = no
        else
            Contains = yes
        )
    ;
        ( GoalExpr = negation(SubGoal)
        ; GoalExpr = scope(_, SubGoal)
        ),
        Contains = goal_contains_builtin_unify_or_compare(SubGoal)
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- func goals_contain_builtin_unify_or_compare(list(hlds_goal)) = bool.

goals_contain_builtin_unify_or_compare([]) = no.
goals_contain_builtin_unify_or_compare([Goal | Goals]) = Contains :-
    ( if goal_contains_builtin_unify_or_compare(Goal) = yes then
        Contains = yes
    else
        Contains = goals_contain_builtin_unify_or_compare(Goals)
    ).

:- func cases_contain_builtin_unify_or_compare(list(case)) = bool.

cases_contain_builtin_unify_or_compare([]) = no.
cases_contain_builtin_unify_or_compare([Case | Cases]) = Contains :-
    Case = case(_, _, Goal),
    ( if goal_contains_builtin_unify_or_compare(Goal) = yes then
        Contains = yes
    else
        Contains = cases_contain_builtin_unify_or_compare(Cases)
    ).

%-----------------------------------------------------------------------------%

:- type deep_prof_tail_rec_info
    --->    deep_prof_tail_rec_info(
                dptri_moduleinfo    :: module_info,
                dptri_scc_ppids     :: assoc_list(pred_proc_id),
                dptri_detism        :: determinism,
                dptri_outputs       :: list(prog_var)
            ).

:- pred apply_deep_prof_tail_rec_to_goal(hlds_goal::in, hlds_goal::out,
    deep_prof_tail_rec_info::in, bool::in, bool::out,
    maybe(list(prog_var))::out) is det.

apply_deep_prof_tail_rec_to_goal(Goal0, Goal, TailRecInfo, !FoundTailCall,
        Continue) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        Goal = Goal0,
        Continue = no
    ;
        GoalExpr0 = plain_call(PredId, ProcId, Args, Builtin, UnifyContext,
            SymName),
        ( if
            PredProcId = proc(PredId, ProcId),
            assoc_list.search(TailRecInfo ^ dptri_scc_ppids, PredProcId,
                ClonePredProcId),
            module_info_pred_proc_info(TailRecInfo ^ dptri_moduleinfo,
                PredId, ProcId, PredInfo, ProcInfo),
            proc_info_interface_determinism(ProcInfo, CallDetism),
            CallDetism = TailRecInfo ^ dptri_detism,
            pred_info_get_arg_types(PredInfo, Types),
            proc_info_get_argmodes(ProcInfo, Modes),
            find_list_of_output_args(Args, Modes, Types,
                TailRecInfo ^ dptri_moduleinfo, CallOutputs),
            CallOutputs = TailRecInfo ^ dptri_outputs,
            Builtin = not_builtin
        then
            ClonePredProcId = proc(ClonePredId, CloneProcId),
            GoalExpr = plain_call(ClonePredId, CloneProcId, Args,
                Builtin, UnifyContext, SymName),
            goal_info_add_feature(feature_deep_self_tail_rec_call,
                GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr, GoalInfo),
            !:FoundTailCall = yes
        else
            Goal = Goal0
        ),
        Continue = no
    ;
        GoalExpr0 = generic_call(_, _, _, _, _),
        Goal = Goal0,
        Continue = no
    ;
        GoalExpr0 = unify(_, _, _, Unify0, _),
        Goal = Goal0,
        (
            Unify0 = assign(ToVar, FromVar),
            apply_deep_prof_tail_rec_to_assign(TailRecInfo ^ dptri_outputs,
                ToVar, FromVar, Outputs),
            Continue = yes(Outputs)
        ;
            ( Unify0 = construct(_, _, _, _, _, __, _)
            ; Unify0 = deconstruct(_, _, _, _, __, _)
            ; Unify0 = simple_test(_, _)
            ; Unify0 = complicated_unify(_, _, _)
            ),
            Continue = no
        )
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        (
            ConjType = plain_conj,
            apply_deep_prof_tail_rec_to_conj(Goals0, Goals, TailRecInfo,
                !FoundTailCall, Continue),
            GoalExpr = conj(ConjType, Goals),
            Goal = hlds_goal(GoalExpr, GoalInfo0)
        ;
            ConjType = parallel_conj,
            Goal = Goal0,
            Continue = no
        )
    ;
        GoalExpr0 = disj(Goals0),
        apply_deep_prof_tail_rec_to_disj(Goals0, Goals, TailRecInfo,
            !FoundTailCall),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        Continue = no
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        apply_deep_prof_tail_rec_to_cases(Cases0, Cases, TailRecInfo,
            !FoundTailCall),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        Continue = no
    ;
        GoalExpr0 = if_then_else(Vars, Cond, Then0, Else0),
        apply_deep_prof_tail_rec_to_goal(Then0, Then, TailRecInfo,
            !FoundTailCall, _),
        apply_deep_prof_tail_rec_to_goal(Else0, Else, TailRecInfo,
            !FoundTailCall, _),
        GoalExpr = if_then_else(Vars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo0),
        Continue = no
    ;
        GoalExpr0 = scope(_, _),
        Goal = Goal0,
        Continue = no
    ;
        GoalExpr0 = negation(_),
        Goal = Goal0,
        Continue = no
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred apply_deep_prof_tail_rec_to_assign(list(prog_var)::in,
    prog_var::in, prog_var::in, list(prog_var)::out) is det.

apply_deep_prof_tail_rec_to_assign([], _, _, []).
apply_deep_prof_tail_rec_to_assign([Output0 | Outputs0], ToVar, FromVar,
        [Output | Outputs]) :-
    ( if ToVar = Output0 then
        Output = FromVar
    else
        Output = Output0
    ),
    apply_deep_prof_tail_rec_to_assign(Outputs0, ToVar, FromVar, Outputs).

:- pred apply_deep_prof_tail_rec_to_conj(list(hlds_goal)::in,
    list(hlds_goal)::out, deep_prof_tail_rec_info::in,
    bool::in, bool::out, maybe(list(prog_var))::out) is det.

apply_deep_prof_tail_rec_to_conj([], [], TailRecInfo,
        !FoundTailCall, yes(TailRecInfo ^ dptri_outputs)).
apply_deep_prof_tail_rec_to_conj([Goal0 | Goals0], [Goal | Goals], TailRecInfo,
        !FoundTailCall, Continue) :-
    apply_deep_prof_tail_rec_to_conj(Goals0, Goals, TailRecInfo,
        !FoundTailCall, TailContinue),
    (
        TailContinue = yes(Outputs),
        HeadTailRecInfo = TailRecInfo ^ dptri_outputs := Outputs,
        apply_deep_prof_tail_rec_to_goal(Goal0, Goal, HeadTailRecInfo,
            !FoundTailCall, Continue)
    ;
        TailContinue = no,
        Goal = Goal0,
        Continue = no
    ).

:- pred apply_deep_prof_tail_rec_to_disj(list(hlds_goal)::in,
    list(hlds_goal)::out, deep_prof_tail_rec_info::in, bool::in, bool::out)
    is det.

apply_deep_prof_tail_rec_to_disj([], [], _, !FoundTailCall).
apply_deep_prof_tail_rec_to_disj([Goal0], [Goal], TailRecInfo,
        !FoundTailCall) :-
    apply_deep_prof_tail_rec_to_goal(Goal0, Goal, TailRecInfo, !FoundTailCall,
        _).
apply_deep_prof_tail_rec_to_disj([Goal0 | Goals0], [Goal0 | Goals],
        TailRecInfo, !FoundTailCall) :-
    Goals0 = [_ | _],
    apply_deep_prof_tail_rec_to_disj(Goals0, Goals, TailRecInfo,
        !FoundTailCall).

:- pred apply_deep_prof_tail_rec_to_cases(list(case)::in, list(case)::out,
    deep_prof_tail_rec_info::in, bool::in, bool::out) is det.

apply_deep_prof_tail_rec_to_cases([], [], _, !FoundTailCall).
apply_deep_prof_tail_rec_to_cases([Case0 | Cases0], [Case | Cases],
        TailRecInfo, !FoundTailCall) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    apply_deep_prof_tail_rec_to_goal(Goal0, Goal, TailRecInfo,
        !FoundTailCall, _),
    Case = case(MainConsId, OtherConsIds, Goal),
    apply_deep_prof_tail_rec_to_cases(Cases0, Cases, TailRecInfo,
        !FoundTailCall).

%-----------------------------------------------------------------------------%

:- pred figure_out_rec_call_numbers(hlds_goal::in, int::in, int::out,
    list(int)::in, list(int)::out) is det.

figure_out_rec_call_numbers(Goal, !N, !TailCallSites) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = call_foreign_proc(Attrs, _, _, _, _, _, _),
        MayCallMercury = get_may_call_mercury(Attrs),
        (
            MayCallMercury = proc_may_call_mercury,
            !:N = !.N + 1
        ;
            MayCallMercury = proc_will_not_call_mercury
        )
    ;
        GoalExpr = plain_call(_, _, _, BuiltinState, _, _),
        Features = goal_info_get_features(GoalInfo),
        ( if set.member(feature_deep_self_tail_rec_call, Features) then
            !:TailCallSites = [!.N | !.TailCallSites]
        else
            true
        ),
        (
            BuiltinState = not_builtin,
            !:N = !.N + 1
        ;
            BuiltinState = inline_builtin
        )
    ;
        GoalExpr = generic_call(_, _, _, _, _),
        !:N = !.N + 1
    ;
        GoalExpr = unify(_, _, _, _, _)
    ;
        GoalExpr = conj(_ConjType, Goals),
        figure_out_rec_call_numbers_in_goal_list(Goals, !N, !TailCallSites)
    ;
        GoalExpr = disj(Goals),
        figure_out_rec_call_numbers_in_goal_list(Goals, !N, !TailCallSites)
    ;
        GoalExpr = switch(_, _, Cases),
        figure_out_rec_call_numbers_in_case_list(Cases, !N, !TailCallSites)
    ;
        GoalExpr = if_then_else(_, Cond, Then, Else),
        figure_out_rec_call_numbers(Cond, !N, !TailCallSites),
        figure_out_rec_call_numbers(Then, !N, !TailCallSites),
        figure_out_rec_call_numbers(Else, !N, !TailCallSites)
    ;
        GoalExpr = negation(SubGoal),
        figure_out_rec_call_numbers(SubGoal, !N, !TailCallSites)
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            true
        else
            figure_out_rec_call_numbers(SubGoal, !N, !TailCallSites)
        )
    ;
        GoalExpr = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred figure_out_rec_call_numbers_in_goal_list(list(hlds_goal)::in,
    int::in, int::out, list(int)::in, list(int)::out) is det.

figure_out_rec_call_numbers_in_goal_list([], !N, !TailCallSites).
figure_out_rec_call_numbers_in_goal_list([Goal|Goals], !N, !TailCallSites) :-
    figure_out_rec_call_numbers(Goal, !N, !TailCallSites),
    figure_out_rec_call_numbers_in_goal_list(Goals, !N, !TailCallSites).

:- pred figure_out_rec_call_numbers_in_case_list(list(case)::in,
    int::in, int::out, list(int)::in, list(int)::out) is det.

figure_out_rec_call_numbers_in_case_list([], !N, !TailCallSites).
figure_out_rec_call_numbers_in_case_list([Case|Cases], !N, !TailCallSites) :-
    Case = case(_, _, Goal),
    figure_out_rec_call_numbers(Goal, !N, !TailCallSites),
    figure_out_rec_call_numbers_in_case_list(Cases, !N, !TailCallSites).

%-----------------------------------------------------------------------------%

:- pred deep_prof_transform_pred(module_info::in, pred_id::in,
    pred_id_table::in, pred_id_table::out) is det.

deep_prof_transform_pred(ModuleInfo, PredId, !PredMap) :-
    map.lookup(!.PredMap, PredId, PredInfo0),
    ProcIds = pred_info_valid_non_imported_procids(PredInfo0),
    pred_info_get_proc_table(PredInfo0, ProcTable0),
    list.foldl(deep_prof_maybe_transform_proc(ModuleInfo, PredId),
        ProcIds, ProcTable0, ProcTable),
    pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredId, PredInfo, !PredMap).

:- pred deep_prof_maybe_transform_proc(module_info::in,
    pred_id::in, proc_id::in, proc_table::in, proc_table::out) is det.

deep_prof_maybe_transform_proc(ModuleInfo, PredId, ProcId, !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    PredModuleName = predicate_module(ModuleInfo, PredId),
    ( if
        % We don't want to transform the procedures for managing the deep
        % profiling call graph, or we'd get infinite recursion.
        PredModuleName = mercury_profiling_builtin_module
    then
        true
    else
        trace [io(!IO)] (
            module_info_get_globals(ModuleInfo, Globals),
            globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
            ProcName = pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId),
            get_progress_output_stream(ModuleInfo, ProgressStream, !IO),
            maybe_write_string(ProgressStream, VeryVerbose,
                string.format("%% Deep profiling: %s\n", [s(ProcName)]), !IO)
        ),
        deep_prof_transform_proc(ModuleInfo, proc(PredId, ProcId),
            ProcInfo0, ProcInfo),
        map.det_update(ProcId, ProcInfo, !ProcTable)
    ).

:- pred deep_prof_transform_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

deep_prof_transform_proc(ModuleInfo, PredProcId, !ProcInfo) :-
    proc_info_get_maybe_deep_profile_info(!.ProcInfo, MaybeDeepInfo),
    (
        MaybeDeepInfo = yes(DeepInfo0),
        DeepInfo0 = deep_profile_proc_info(MaybeDeepRecInfo, _, OrigBody),
        ( if
            MaybeDeepRecInfo = yes(RecInfo),
            RecInfo ^ dri_role = deep_prof_inner_proc(_)
        then
            deep_prof_transform_inner_proc(ModuleInfo, PredProcId, !ProcInfo),
            MaybeDeepLayoutInfo = no
        else
            deep_prof_transform_normal_proc(ModuleInfo, PredProcId, !ProcInfo,
                DeepLayoutInfo),
            MaybeDeepLayoutInfo = yes(DeepLayoutInfo)
        )
    ;
        MaybeDeepInfo = no,
        make_deep_original_body(ModuleInfo, !.ProcInfo, OrigBody),
        deep_prof_transform_normal_proc(ModuleInfo, PredProcId, !ProcInfo,
            DeepLayoutInfo),
        MaybeDeepLayoutInfo = yes(DeepLayoutInfo),
        MaybeDeepRecInfo = no
    ),
    DeepInfo = deep_profile_proc_info(MaybeDeepRecInfo, MaybeDeepLayoutInfo,
        OrigBody),
    proc_info_set_maybe_deep_profile_info(yes(DeepInfo), !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred make_deep_original_body(module_info::in, proc_info::in,
    deep_original_body::out) is det.

make_deep_original_body(ModuleInfo, ProcInfo, DeepOriginalBody) :-
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_initial_instmap(ModuleInfo, ProcInfo, Instmap),
    proc_info_get_vartypes(ProcInfo, Vartypes),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    (
        MaybeDetism = yes(Detism)
    ;
        MaybeDetism = no,
        proc_info_get_inferred_determinism(ProcInfo, Detism)
    ),
    proc_info_get_varset(ProcInfo, Varset),
    DeepOriginalBody = deep_original_body(Body, HeadVars, Instmap, Vartypes,
        Detism, Varset).

%-----------------------------------------------------------------------------%

    % This structure contains stateful information used throughout the deep
    % profiling transformation of a procedure.
    %
:- type deep_info
    --->    deep_info(
                deep_module_info        :: module_info,
                deep_pred_proc_id       :: pred_proc_id,
                deep_containing_goal_map:: containing_goal_map,
                deep_current_csd        :: prog_var,
                deep_site_num_counter   :: counter,
                deep_call_sites         :: cord(call_site_static_data),
                deep_varinfo            :: prog_var_set_types,
                deep_proc_filename      :: string,
                deep_maybe_rec_info     :: maybe(deep_recursion_info)
            ).

    % Transfrom a procedure.
    %
:- pred deep_prof_transform_normal_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out, hlds_deep_layout::out) is det.

deep_prof_transform_normal_proc(ModuleInfo, PredProcId, !ProcInfo,
        DeepLayoutInfo) :-
    fill_goal_id_slots_in_proc(ModuleInfo, ContainingGoalMap, !ProcInfo),

    module_info_get_globals(ModuleInfo, Globals),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    some [!VarInfo, !DeepInfo, !Goal] (
        proc_info_get_goal(!.ProcInfo, !:Goal),
        !.Goal = hlds_goal(_, GoalInfo0),

        !:VarInfo = prog_var_set_types(VarSet0, VarTypes0),
        generate_var("TopCSD", c_pointer_type, TopCSD, !VarInfo),
        generate_var("MiddleCSD", c_pointer_type, MiddleCSD, !VarInfo),
        generate_var("ProcStaticLayout", c_pointer_type, ProcStaticVar,
            !VarInfo),

        proc_info_get_context(!.ProcInfo, Context),
        FileName = term.context_file(Context),
        LineNumber = term.context_line(Context),

        proc_info_get_maybe_deep_profile_info(!.ProcInfo, MaybeDeepProfInfo),
        extract_deep_rec_info(MaybeDeepProfInfo, MaybeRecInfo),
        !:DeepInfo = deep_info(ModuleInfo, PredProcId, ContainingGoalMap,
            MiddleCSD, counter.init(0), cord.empty, !.VarInfo, FileName,
            MaybeRecInfo),

        % This call transforms the goals of the procedure.
        deep_prof_transform_goal(!Goal, _, !DeepInfo),
        !:VarInfo = !.DeepInfo ^ deep_varinfo,
        CallSites = cord.list(!.DeepInfo ^ deep_call_sites),

        % Do coverage profiling if requested.
        globals.lookup_bool_option(Globals, coverage_profiling,
            DoCoverageProfiling),
        (
            DoCoverageProfiling = yes,
            coverage_prof_transform_proc_body(ModuleInfo, PredProcId,
                ContainingGoalMap, MaybeRecInfo, !Goal, !VarInfo,
                CoveragePoints)
        ;
            DoCoverageProfiling = no,
            CoveragePoints = []
        ),

        ( if
            MaybeRecInfo = yes(RecInfo),
            RecInfo ^ dri_role = deep_prof_inner_proc(OuterPredProcId)
        then
            OuterPredProcId = proc(PredId, ProcId)
        else
            PredProcId = proc(PredId, ProcId)
        ),

        globals.lookup_bool_option(Globals, use_activation_counts,
            UseActivationCounts),

        IsInInterface = is_proc_in_interface(ModuleInfo, PredId, ProcId),
        ProcStatic = hlds_proc_static(FileName, LineNumber, IsInInterface,
            CallSites, CoveragePoints),
        ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
        ProcStaticConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        generate_deep_const_unify(ProcStaticConsId, ProcStaticVar,
            BindProcStaticVarGoal),

        % Wrap the procedure body inside more goals that invoke the port codes
        % when necessary. When they are necessary depends on the code model
        % of the procedure.
        CodeModel = proc_info_interface_code_model(!.ProcInfo),
        (
            CodeModel = model_det,
            maybe_generate_activation_ptr(UseActivationCounts, TopCSD,
                MiddleCSD, MaybeActivationPtr, ExcpVars, !VarInfo),
            build_det_proc_body(ModuleInfo, TopCSD, MiddleCSD, ProcStaticVar,
                MaybeActivationPtr, GoalInfo0, BindProcStaticVarGoal, !Goal)
        ;
            CodeModel = model_semi,
            maybe_generate_activation_ptr(UseActivationCounts, TopCSD,
                MiddleCSD, MaybeActivationPtr, ExcpVars, !VarInfo),
            build_semi_proc_body(ModuleInfo, TopCSD, MiddleCSD, ProcStaticVar,
                MaybeActivationPtr, GoalInfo0, BindProcStaticVarGoal, !Goal)
        ;
            CodeModel = model_non,
            generate_outermost_proc_dyns(UseActivationCounts, TopCSD,
                MiddleCSD, MaybeOldActivationPtr, NewOutermostProcDyn,
                ExcpVars, !VarInfo),
            build_non_proc_body(ModuleInfo, TopCSD, MiddleCSD,
                ProcStaticVar, MaybeOldActivationPtr, NewOutermostProcDyn,
                GoalInfo0, BindProcStaticVarGoal, !Goal)
        ),

        !.VarInfo = prog_var_set_types(Vars, VarTypes),
        proc_info_set_varset(Vars, !ProcInfo),
        proc_info_set_vartypes(VarTypes, !ProcInfo),
        proc_info_set_goal(!.Goal, !ProcInfo),
        DeepLayoutInfo = hlds_deep_layout(ProcStatic, ExcpVars)
    ).

    % Transform an inner procedure for deep profiling. Inner procedures are
    % created by the tail recursion preservation pass above.
    %
    % XXX: Inner procedures have no coverage profiling transformation done to
    % them yet. This is because they are currently broken, and hence disabled.
    %
:- pred deep_prof_transform_inner_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

deep_prof_transform_inner_proc(ModuleInfo, PredProcId, !ProcInfo) :-
    fill_goal_id_slots_in_proc(ModuleInfo, ContainingGoalMap, !ProcInfo),

    proc_info_get_goal(!.ProcInfo, Goal0),
    Goal0 = hlds_goal(_, GoalInfo0),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    VarInfo0 = prog_var_set_types(VarSet0, VarTypes0),
    generate_var("MiddleCSD", c_pointer_type, MiddleCSD, VarInfo0, VarInfo1),

    Context = goal_info_get_context(GoalInfo0),
    FileName = term.context_file(Context),

    proc_info_get_maybe_deep_profile_info(!.ProcInfo, MaybeDeepProfInfo),
    extract_deep_rec_info(MaybeDeepProfInfo, MaybeRecInfo),
    DeepInfo0 = deep_info(ModuleInfo, PredProcId, ContainingGoalMap, MiddleCSD,
        counter.init(0), cord.empty, VarInfo1, FileName, MaybeRecInfo),

    deep_prof_transform_goal(Goal0, Goal, _, DeepInfo0, DeepInfo),

    VarInfo = DeepInfo ^ deep_varinfo,
    VarInfo = prog_var_set_types(VarSet, VarTypes),

    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo).

:- func is_proc_in_interface(module_info, pred_id, proc_id) = bool.

is_proc_in_interface(ModuleInfo, PredId, _ProcId) = IsInInterface :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( if
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_pseudo_exported(PredInfo)
        )
    then
        IsInInterface = yes
    else
        IsInInterface = no
    ).

%-----------------------------------------------------------------------------%

    % Wrap the procedure body in the deep profiling port goals.
    %
    % When modifing this transformation be sure to modify original_root/3 in
    % deep_profiler/program_representation_utils.m which must be able to undo
    % this transformation.
    %
:- pred build_det_proc_body(module_info::in, prog_var::in, prog_var::in,
    prog_var::in, maybe(prog_var)::in, hlds_goal_info::in, hlds_goal::in,
    hlds_goal::in, hlds_goal::out) is det.

build_det_proc_body(ModuleInfo, TopCSD, MiddleCSD, ProcStaticVar,
        MaybeActivationPtr, GoalInfo0, BindProcStaticVarGoal, Goal0, Goal) :-
    (
        MaybeActivationPtr = yes(ActivationPtr1),
        generate_deep_det_call(ModuleInfo, "det_call_port_code_sr", 4,
            [ProcStaticVar, TopCSD, MiddleCSD, ActivationPtr1],
            [TopCSD, MiddleCSD, ActivationPtr1], CallPortCode0),
        goal_add_feature(feature_save_deep_excp_vars,
            CallPortCode0, CallPortCode),
        generate_deep_det_call(ModuleInfo, "det_exit_port_code_sr", 3,
            [TopCSD, MiddleCSD, ActivationPtr1], [], ExitPortCode)
    ;
        MaybeActivationPtr = no,
        generate_deep_det_call(ModuleInfo, "det_call_port_code_ac", 3,
            [ProcStaticVar, TopCSD, MiddleCSD],
            [TopCSD, MiddleCSD], CallPortCode0),
        goal_add_feature(feature_save_deep_excp_vars,
            CallPortCode0, CallPortCode),
        generate_deep_det_call(ModuleInfo, "det_exit_port_code_ac", 2,
            [TopCSD, MiddleCSD], [], ExitPortCode)
    ),

    make_impure(GoalInfo0, GoalInfo),
    GoalExpr = conj(plain_conj, [
        BindProcStaticVarGoal,
        CallPortCode,
        Goal0,
        ExitPortCode
    ]),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Wrap the goal for a semidet procedure.
    %
    % If changing this transformation be sure to change original_root/3 in
    % deep_profiler/program_represenentation_utils.m.
    %
:- pred build_semi_proc_body(module_info::in, prog_var::in, prog_var::in,
    prog_var::in, maybe(prog_var)::in, hlds_goal_info::in, hlds_goal::in,
    hlds_goal::in, hlds_goal::out) is det.

build_semi_proc_body(ModuleInfo, TopCSD, MiddleCSD, ProcStaticVar,
        MaybeActivationPtr, GoalInfo0, BindProcStaticVarGoal, Goal0, Goal) :-
    (
        MaybeActivationPtr = yes(ActivationPtr1),
        generate_deep_det_call(ModuleInfo, "semi_call_port_code_sr", 4,
            [ProcStaticVar, TopCSD, MiddleCSD, ActivationPtr1],
            [TopCSD, MiddleCSD, ActivationPtr1], CallPortCode0),
        goal_add_feature(feature_save_deep_excp_vars,
            CallPortCode0, CallPortCode),
        generate_deep_det_call(ModuleInfo, "semi_exit_port_code_sr", 3,
            [TopCSD, MiddleCSD, ActivationPtr1], [], ExitPortCode),
        generate_deep_call(ModuleInfo, "semi_fail_port_code_sr", 3,
            [TopCSD, MiddleCSD, ActivationPtr1], maybe.no, detism_failure,
            FailPortCode),
        NewNonlocals =
            set_of_var.list_to_set([TopCSD, MiddleCSD, ActivationPtr1])
    ;
        MaybeActivationPtr = no,
        generate_deep_det_call(ModuleInfo, "semi_call_port_code_ac", 3,
            [ProcStaticVar, TopCSD, MiddleCSD],
            [TopCSD, MiddleCSD], CallPortCode0),
        goal_add_feature(feature_save_deep_excp_vars,
            CallPortCode0, CallPortCode),
        generate_deep_det_call(ModuleInfo, "semi_exit_port_code_ac", 2,
            [TopCSD, MiddleCSD], [], ExitPortCode),
        generate_deep_call(ModuleInfo, "semi_fail_port_code_ac", 2,
            [TopCSD, MiddleCSD], maybe.no, detism_failure, FailPortCode),
        NewNonlocals = set_of_var.list_to_set([TopCSD, MiddleCSD])
    ),

    ExitConjGoalInfo = goal_info_add_nonlocals_make_impure(GoalInfo0,
        NewNonlocals),

    make_impure(GoalInfo0, GoalInfo),
    GoalExpr = conj(plain_conj, [
        BindProcStaticVarGoal,
        CallPortCode,
        hlds_goal(
            disj([
                hlds_goal(conj(plain_conj, [Goal0, ExitPortCode]),
                    ExitConjGoalInfo),
                FailPortCode
            ]),
            ExitConjGoalInfo)
    ]),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred build_non_proc_body(module_info::in, prog_var::in, prog_var::in,
    prog_var::in, maybe(prog_var)::in, prog_var::in, hlds_goal_info::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::out) is det.

build_non_proc_body(ModuleInfo, TopCSD, MiddleCSD, ProcStaticVar,
        MaybeOldActivationPtr, NewOutermostProcDyn, GoalInfo0,
        BindProcStaticVarGoal, Goal0, Goal) :-
    (
        MaybeOldActivationPtr = yes(OldOutermostProcDyn2),
        generate_deep_det_call(ModuleInfo, "non_call_port_code_sr", 5,
            [ProcStaticVar, TopCSD, MiddleCSD,
            OldOutermostProcDyn2, NewOutermostProcDyn],
            [TopCSD, MiddleCSD, OldOutermostProcDyn2, NewOutermostProcDyn],
            CallPortCode0),
        goal_add_feature(feature_save_deep_excp_vars,
            CallPortCode0, CallPortCode),
        generate_deep_det_call(ModuleInfo, "non_exit_port_code_sr", 3,
            [TopCSD, MiddleCSD, OldOutermostProcDyn2], [],
            ExitPortCode),
        generate_deep_call(ModuleInfo, "non_fail_port_code_sr", 3,
            [TopCSD, MiddleCSD, OldOutermostProcDyn2], maybe.no,
            detism_failure, FailPortCode),
        generate_deep_call(ModuleInfo, "non_redo_port_code_sr", 2,
            [MiddleCSD, NewOutermostProcDyn], maybe.no,
            detism_failure, RedoPortCode0),
        NewNonlocals =
            set_of_var.list_to_set([TopCSD, MiddleCSD, OldOutermostProcDyn2])
    ;
        MaybeOldActivationPtr = no,
        generate_deep_det_call(ModuleInfo, "non_call_port_code_ac", 4,
            [ProcStaticVar, TopCSD, MiddleCSD, NewOutermostProcDyn],
            [TopCSD, MiddleCSD, NewOutermostProcDyn],
            CallPortCode0),
        goal_add_feature(feature_save_deep_excp_vars,
            CallPortCode0, CallPortCode),
        generate_deep_det_call(ModuleInfo, "non_exit_port_code_ac", 2,
            [TopCSD, MiddleCSD], [], ExitPortCode),
        generate_deep_call(ModuleInfo, "non_fail_port_code_ac", 2,
            [TopCSD, MiddleCSD], maybe.no, detism_failure, FailPortCode),
        generate_deep_call(ModuleInfo, "non_redo_port_code_ac", 2,
            [MiddleCSD, NewOutermostProcDyn], maybe.no,
            detism_failure, RedoPortCode0),
        NewNonlocals = set_of_var.list_to_set([TopCSD, MiddleCSD])
    ),

    RedoPortCode0 = hlds_goal(RedoPortExpr, RedoPortGoalInfo0),
    goal_info_add_feature(feature_preserve_backtrack_into,
        RedoPortGoalInfo0, RedoPortGoalInfo),
    RedoPortCode = hlds_goal(RedoPortExpr, RedoPortGoalInfo),

    % Even though the procedure has a model_non interface determinism, the
    % actual determinism of its original body goal may have been at_most once.
    % However, the exit/redo disjunction we insert into the procedure body
    % means that the procedure body does actually leave a nondet stack frame
    % when it succeeds, and its determinism must be adjusted accordingly.
    %
    Detism0 = goal_info_get_determinism(GoalInfo0),
    determinism_components(Detism0, CanFail, _),
    determinism_components(Detism, CanFail, at_most_many),
    goal_info_set_determinism(Detism, GoalInfo0, GoalInfo1),

    set_of_var.insert(NewOutermostProcDyn, NewNonlocals, ExitRedoNonLocals),
    ExitRedoGoalInfo = impure_reachable_init_goal_info(ExitRedoNonLocals,
        detism_multi),

    CallExitRedoGoalInfo = goal_info_add_nonlocals_make_impure(GoalInfo1,
        ExitRedoNonLocals),

    make_impure(GoalInfo1, GoalInfo),
    GoalExpr = conj(plain_conj, [
        BindProcStaticVarGoal,
        CallPortCode,
        hlds_goal(
            disj([
                hlds_goal(
                    conj(plain_conj, [
                        Goal0,
                        hlds_goal(
                            disj([
                                ExitPortCode,
                                RedoPortCode
                            ]),
                            ExitRedoGoalInfo)
                    ]),
                    CallExitRedoGoalInfo),
                FailPortCode
            ]),
            CallExitRedoGoalInfo)
    ]),
    Goal = hlds_goal(GoalExpr, GoalInfo).

%-----------------------------------------------------------------------------%

:- pred deep_prof_transform_goal(hlds_goal::in, hlds_goal::out,
    bool::out, deep_info::in, deep_info::out) is det.

deep_prof_transform_goal(Goal0, Goal, AddedImpurity, !DeepInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_mdprof_inst(goal_is_not_mdprof_inst, GoalInfo0, GoalInfo1),
    Goal1 = hlds_goal(GoalExpr0, GoalInfo1),
    (
        GoalExpr0 = plain_call(_, _, _, BuiltinState, _, _),
        (
            BuiltinState = not_builtin,
            deep_prof_wrap_call(Goal1, Goal, !DeepInfo),
            AddedImpurity = yes
        ;
            BuiltinState = inline_builtin,
            Goal = Goal1,
            AddedImpurity = no
        )
    ;
        GoalExpr0 = generic_call(GenericCall, _, _, _, _),
        (
            ( GenericCall = higher_order(_, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ),
            deep_prof_wrap_call(Goal1, Goal, !DeepInfo),
            AddedImpurity = yes
        ;
            ( GenericCall = event_call(_)
            ; GenericCall = cast(_)
            ),
            Goal = Goal1,
            AddedImpurity = no
        )
    ;
        GoalExpr0 = call_foreign_proc(Attrs, _, _, _, _, _, _),
        MayCallMercury = get_may_call_mercury(Attrs),
        (
            MayCallMercury = proc_may_call_mercury,
            deep_prof_wrap_foreign_code(Goal1, Goal, !DeepInfo),
            AddedImpurity = yes
        ;
            MayCallMercury = proc_will_not_call_mercury,
            Goal = Goal1,
            AddedImpurity = no
        )
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = Goal1,
        AddedImpurity = no
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        deep_prof_transform_conj(ConjType, Goals0, Goals, AddedImpurity,
            !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo1, GoalInfo),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Goals0),
        deep_prof_transform_disj(Goals0, Goals, AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo1, GoalInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        deep_prof_transform_switch(Cases0, Cases, AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo1, GoalInfo),
        GoalExpr = switch(Var, CanFail, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        deep_prof_transform_goal(SubGoal0, SubGoal, AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo1, GoalInfo),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(IVars, Cond0, Then0, Else0),
        deep_prof_transform_goal(Cond0, Cond, AddedImpurityC, !DeepInfo),
        deep_prof_transform_goal(Then0, Then, AddedImpurityT, !DeepInfo),
        deep_prof_transform_goal(Else0, Else, AddedImpurityE, !DeepInfo),
        ( if
            ( AddedImpurityC = yes
            ; AddedImpurityT = yes
            ; AddedImpurityE = yes
            )
        then
            AddedImpurity = yes
        else
            AddedImpurity = no
        ),
        add_impurity_if_needed(AddedImpurity, GoalInfo1, GoalInfo),
        GoalExpr = if_then_else(IVars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        SubGoal0 = hlds_goal(_, InnerInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo1),
        InnerDetism = goal_info_get_determinism(InnerInfo),
        ( if InnerDetism = OuterDetism then
            Reason = Reason0,
            AddForceCommit = no
        else
            % Given a subgoal containing both at_most_many code and impure
            % code, determinism analysis will remove the `scope' wrapped
            % around that subgoal if it is allowed to. If we get here, then
            % the subgoal inside the `scope' contains at_most_many code
            % (which means that removing the scope will change its determinism)
            % and the deep profiling transformation will make it impure
            % as well.

            ( if Reason0 = commit(_) then
                Reason = commit(force_pruning),
                AddForceCommit = no
            else
                Reason = Reason0,
                AddForceCommit = yes
            )
        ),
        ( if
            Reason = from_ground_term(_, FGT),
            ( FGT = from_ground_term_construct
            ; FGT = from_ground_term_deconstruct
            )
        then
            % We must annotate the scope goal and its children with a default
            % deep profiling information structure, this is required by the
            % coverage profiling transformation.
            transform_all_goals(deep_prof_mark_goal_as_not_mdprof_inst,
                SubGoal0, SubGoal),
            AddedImpurity = no
        else
            deep_prof_transform_goal(SubGoal0, SubGoal, AddedImpurity,
                !DeepInfo)
        ),
        add_impurity_if_needed(AddedImpurity, GoalInfo1, GoalInfo),
        (
            AddForceCommit = no,
            Goal = hlds_goal(scope(Reason, SubGoal), GoalInfo)
        ;
            AddForceCommit = yes,
            goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo,
                InnerGoalInfo),
            InnerGoal = hlds_goal(scope(Reason, SubGoal), InnerGoalInfo),
            Goal = hlds_goal(scope(commit(force_pruning), InnerGoal), GoalInfo)
        )
    ;
        GoalExpr0 = shorthand(_),
        unexpected($pred, "shorthand")
    ).

:- pred deep_prof_mark_goal_as_not_mdprof_inst(hlds_goal::in, hlds_goal::out)
    is det.

deep_prof_mark_goal_as_not_mdprof_inst(Goal0, Goal) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_mdprof_inst(goal_is_not_mdprof_inst, GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr0, GoalInfo).

:- pred deep_prof_transform_conj(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::out,
    deep_info::in, deep_info::out) is det.

deep_prof_transform_conj(_, [], [], no, !DeepInfo).
deep_prof_transform_conj(ConjType, [Goal0 | Goals0], Goals,
        AddedImpurity, !DeepInfo) :-
    deep_prof_transform_goal(Goal0, Goal, AddedImpurityFirst, !DeepInfo),
    deep_prof_transform_conj(ConjType, Goals0, TailGoals,
        AddedImpurityLater, !DeepInfo),
    Goal = hlds_goal(GoalExpr, _),
    ( if
        GoalExpr = conj(plain_conj, Conjuncts),
        ConjType = plain_conj
    then
        Goals = Conjuncts ++ TailGoals
    else
        Goals = [Goal | TailGoals]
    ),
    bool.or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity).

:- pred deep_prof_transform_disj(list(hlds_goal)::in, list(hlds_goal)::out,
    bool::out, deep_info::in, deep_info::out) is det.

deep_prof_transform_disj([], [], no, !DeepInfo).
deep_prof_transform_disj([Goal0 | Goals0], [Goal | Goals],
        AddedImpurity, !DeepInfo) :-
    deep_prof_transform_goal(Goal0, Goal, AddedImpurityFirst, !DeepInfo),
    deep_prof_transform_disj(Goals0, Goals, AddedImpurityLater,
        !DeepInfo),
    bool.or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity).

:- pred deep_prof_transform_switch(list(case)::in, list(case)::out, bool::out,
    deep_info::in, deep_info::out) is det.

deep_prof_transform_switch([], [], no, !DeepInfo).
deep_prof_transform_switch([Case0 | Cases0], [Case | Cases], AddedImpurity,
        !DeepInfo) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    deep_prof_transform_goal(Goal0, Goal, AddedImpurityFirst, !DeepInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    deep_prof_transform_switch(Cases0, Cases,
        AddedImpurityLater, !DeepInfo),
    bool.or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity).

:- pred deep_prof_wrap_call(hlds_goal::in, hlds_goal::out,
    deep_info::in, deep_info::out) is det.

deep_prof_wrap_call(Goal0, Goal, !DeepInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    GoalId = goal_info_get_goal_id(GoalInfo0),
    ContainingGoalMap = !.DeepInfo ^ deep_containing_goal_map,
    GoalPath = goal_id_to_forward_path(ContainingGoalMap, GoalId),

    ModuleInfo = !.DeepInfo ^ deep_module_info,
    GoalFeatures = goal_info_get_features(GoalInfo0),
    goal_info_remove_feature(feature_deep_self_tail_rec_call,
        GoalInfo0, GoalInfo1),
    make_impure(GoalInfo1, GoalInfo2),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo2,
        MdprofInstGoalInfo),

    % We need to make the call itself impure. If we didn't do so,
    % then simplify could eliminate the goal (e.g. if it was a duplicate call).
    % The result would be a prepare_for_{...}_call whose execution
    % is not followed by the execution of the call port code of the callee.
    % This would leave the MR_csd_callee_ptr field NULL, which violates
    % invariants of the deep profiling tree (which allows this field to be
    % NULL only temporarily, between the prepare_for_{...}_call and the
    % call port code).
    Goal1 = hlds_goal(GoalExpr0, GoalInfo2),

    SiteNumCounter0 = !.DeepInfo ^ deep_site_num_counter,
    counter.allocate(SiteNum, SiteNumCounter0, SiteNumCounter),
    VarInfo0 = !.DeepInfo ^ deep_varinfo,
    generate_var("SiteNum", int_type, SiteNumVar, VarInfo0, VarInfo1),
    generate_deep_const_unify(some_int_const(int_const(SiteNum)),
        SiteNumVar, SiteNumVarGoal),
    !DeepInfo ^ deep_varinfo := VarInfo1,
    !DeepInfo ^ deep_site_num_counter := SiteNumCounter,

    Context = goal_info_get_context(GoalInfo0),
    FileName0 = term.context_file(Context),
    LineNumber = term.context_line(Context),
    compress_filename(!.DeepInfo, FileName0, FileName),
    CallKind = classify_call(ModuleInfo, GoalExpr0),
    (
        CallKind = call_class_normal(PredProcId),
        ( if set.member(feature_deep_self_tail_rec_call, GoalFeatures) then
            generate_deep_det_call(ModuleInfo, "prepare_for_tail_call", 1,
                [SiteNumVar], [], PrepareGoal)
        else
            generate_deep_det_call(ModuleInfo, "prepare_for_normal_call", 1,
                [SiteNumVar], [], PrepareGoal)
        ),
        PredProcId = proc(PredId, ProcId),
        TypeSubst = compute_type_subst(GoalExpr0, !.DeepInfo),
        MaybeRecInfo = !.DeepInfo ^ deep_maybe_rec_info,
        ( if
            MaybeRecInfo = yes(RecInfo1),
            RecInfo1 ^ dri_role = deep_prof_inner_proc(OuterPredProcId),
            PredProcId = !.DeepInfo ^ deep_pred_proc_id
        then
            OuterPredProcId = proc(OuterPredId, OuterProcId),
            RttiProcLabel = make_rtti_proc_label(ModuleInfo,
                OuterPredId, OuterProcId)
        else if
            MaybeRecInfo = yes(RecInfo2),
            RecInfo2 ^ dri_role = deep_prof_outer_proc(InnerPredProcId),
            PredProcId = InnerPredProcId
        then
            OuterPredProcId = !.DeepInfo ^ deep_pred_proc_id,
            OuterPredProcId = proc(OuterPredId, OuterProcId),
            RttiProcLabel = make_rtti_proc_label(ModuleInfo,
                OuterPredId, OuterProcId)
        else
            RttiProcLabel = make_rtti_proc_label(ModuleInfo, PredId, ProcId)
        ),
        CallSite = normal_call(RttiProcLabel, TypeSubst,
            FileName, LineNumber, GoalPath),
        Goal2 = Goal1
    ;
        CallKind = call_class_special(_PredProcId, TypeInfoVar),
        generate_deep_det_call(ModuleInfo, "prepare_for_special_call", 2,
            [SiteNumVar, TypeInfoVar], [], PrepareGoal),
        CallSite = special_call(FileName, LineNumber, GoalPath),
        Goal2 = Goal1
    ;
        CallKind = call_class_generic(Generic),
        (
            Generic = higher_order(ClosureVar, _, _, _),
            generate_deep_det_call(ModuleInfo, "prepare_for_ho_call", 2,
                [SiteNumVar, ClosureVar], [], PrepareGoal),
            CallSite = higher_order_call(FileName, LineNumber, GoalPath)
        ;
            Generic = class_method(TypeClassInfoVar, MethodNum, _, _),
            VarInfo2 = !.DeepInfo ^ deep_varinfo,
            generate_var("MethodNum", int_type, MethodNumVar, VarInfo2,
                VarInfo3),
            !DeepInfo ^ deep_varinfo := VarInfo3,
            generate_deep_const_unify(some_int_const(int_const(MethodNum)),
                MethodNumVar, MethodNumVarGoal),
            generate_deep_det_call(ModuleInfo, "prepare_for_method_call", 3,
                [SiteNumVar, TypeClassInfoVar, MethodNumVar],
                [], PrepareCallGoal),
            PrepareCallGoal = hlds_goal(_, PrepareCallGoalInfo),
            PrepareGoalExpr = conj(plain_conj,
                [MethodNumVarGoal, PrepareCallGoal]),
            PrepareGoal = hlds_goal(PrepareGoalExpr, PrepareCallGoalInfo),
            CallSite = method_call(FileName, LineNumber, GoalPath)
        ;
            Generic = event_call(_),
            unexpected($pred, "event_call")
        ;
            Generic = cast(_),
            unexpected($pred, "cast")
        ),
        GoalCodeModel = goal_info_get_code_model(GoalInfo0),
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, use_zeroing_for_ho_cycles,
            UseZeroing),
        (
            UseZeroing = yes,
            deep_prof_transform_higher_order_call(Globals, GoalCodeModel,
                Goal1, Goal2, !DeepInfo)
        ;
            UseZeroing = no,
            Goal2 = Goal1
        )
    ),

    !DeepInfo ^ deep_call_sites :=
        cord.snoc(!.DeepInfo ^ deep_call_sites, CallSite),
    ( if
        set.member(feature_deep_self_tail_rec_call, GoalFeatures),
        !.DeepInfo ^ deep_maybe_rec_info = yes(RecInfo),
        RecInfo ^ dri_role = deep_prof_outer_proc(_)
    then
        VisSCC = RecInfo ^ dri_visible_scc,
        MiddleCSD = !.DeepInfo ^ deep_current_csd,
        (
            VisSCC = [],
            CallGoals = [],
            ExitGoals = [],
            FailGoals = [],
            SaveRestoreVars = []
        ;
            VisSCC = [SCCmember],
            generate_recursion_counter_saves_and_restores(
                SCCmember ^ rec_call_sites, MiddleCSD,
                CallGoals, ExitGoals, FailGoals, SaveRestoreVars, !DeepInfo)
        ;
            VisSCC = [_, _ | _],
            unexpected($pred, "multi-procedure SCCs not yet implemented")
        ),

        CodeModel = goal_info_get_code_model(GoalInfo0),
        (
            CodeModel = model_det,
            Goals =
                CallGoals ++ [SiteNumVarGoal, PrepareGoal, Goal2] ++ ExitGoals,
            GoalExpr = conj(plain_conj, Goals)
        ;
            ( CodeModel = model_semi
            ; CodeModel = model_non
            ),
            ExtraVars = set_of_var.list_to_set([MiddleCSD | SaveRestoreVars]),
            WrappedGoalGoalInfo0 =
                goal_info_add_nonlocals_make_impure(MdprofInstGoalInfo,
                    ExtraVars),
            goal_info_set_mdprof_inst(goal_is_mdprof_inst,
                WrappedGoalGoalInfo0, WrappedGoalGoalInfo),

            ReturnFailsGoalInfo0 =
                impure_unreachable_init_goal_info(ExtraVars, detism_failure),
            goal_info_set_mdprof_inst(goal_is_mdprof_inst,
                ReturnFailsGoalInfo0, ReturnFailsGoalInfo),

            FailGoalInfo0 = fail_goal_info,
            goal_info_set_mdprof_inst(goal_is_mdprof_inst,
                FailGoalInfo0, FailGoalInfo),
            FailGoal = hlds_goal(disj([]), FailGoalInfo),

            FailGoalsAndFail = FailGoals ++ [FailGoal],

            DisjGoalExpr = disj([
                hlds_goal(
                    conj(plain_conj,
                        [SiteNumVarGoal, PrepareGoal, Goal2 | ExitGoals]),
                    WrappedGoalGoalInfo),
                hlds_goal(
                    conj(plain_conj, FailGoalsAndFail),
                    ReturnFailsGoalInfo)
            ]),
            DisjGoal = hlds_goal(DisjGoalExpr, WrappedGoalGoalInfo),
            Goals = CallGoals ++ [DisjGoal],
            GoalExpr = conj(plain_conj, Goals)
        )
    else
        GoalExpr = conj(plain_conj, [SiteNumVarGoal, PrepareGoal, Goal2])
    ),
    Goal = hlds_goal(GoalExpr, MdprofInstGoalInfo).

:- pred deep_prof_transform_higher_order_call(globals::in, code_model::in,
    hlds_goal::in, hlds_goal::out, deep_info::in, deep_info::out) is det.

deep_prof_transform_higher_order_call(Globals, CodeModel, Goal0, Goal,
        !DeepInfo) :-
    some [!VarInfo] (
        !:VarInfo = !.DeepInfo ^ deep_varinfo,

        generate_var("SavedPtr", c_pointer_type, SavedPtrVar, !VarInfo),

        globals.lookup_bool_option(Globals, use_activation_counts,
            UseActivationCounts),
        (
            UseActivationCounts = yes,

            generate_var("SavedCounter", int_type, SavedCountVar, !VarInfo),
            ExtraNonLocals =
                set_of_var.list_to_set([SavedCountVar, SavedPtrVar]),

            generate_deep_det_call(!.DeepInfo ^ deep_module_info,
                "save_and_zero_activation_info_ac", 2,
                [SavedCountVar, SavedPtrVar],
                [SavedCountVar, SavedPtrVar], SaveStuff),
            generate_deep_det_call(!.DeepInfo ^ deep_module_info,
                "reset_activation_info_ac", 2,
                [SavedCountVar, SavedPtrVar], [], RestoreStuff),
            generate_deep_det_call(!.DeepInfo ^ deep_module_info,
                "rezero_activation_info_ac", 0,
                [], [], ReZeroStuff)
        ;
            UseActivationCounts = no,

            ExtraNonLocals = set_of_var.make_singleton(SavedPtrVar),

            generate_deep_det_call(!.DeepInfo ^ deep_module_info,
                "save_and_zero_activation_info_sr", 1,
                [SavedPtrVar], [SavedPtrVar], SaveStuff),
            generate_deep_det_call(!.DeepInfo ^ deep_module_info,
                "reset_activation_info_sr", 1,
                [SavedPtrVar], [], RestoreStuff),
            generate_deep_det_call(!.DeepInfo ^ deep_module_info,
                "rezero_activation_info_sr", 0,
                [], [], ReZeroStuff)
        ),

        !DeepInfo ^ deep_varinfo := !.VarInfo
    ),

    Goal0 = hlds_goal(_, GoalInfo0),
    ExtGoalInfo0 = goal_info_add_nonlocals_make_impure(GoalInfo0,
        ExtraNonLocals),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, ExtGoalInfo0, ExtGoalInfo),

    % XXX We should build up NoBindExtGoalInfo from scratch.
    instmap_delta_init_reachable(EmptyDelta),
    goal_info_set_instmap_delta(EmptyDelta, ExtGoalInfo, NoBindExtGoalInfo),

    FailGoalInfo0 = fail_goal_info,
    goal_info_set_mdprof_inst(goal_is_mdprof_inst,
        FailGoalInfo0, FailGoalInfo),
    FailGoal = hlds_goal(disj([]), FailGoalInfo),

    RestoreFailGoalInfo0 = impure_unreachable_init_goal_info(ExtraNonLocals,
        detism_failure),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst,
        RestoreFailGoalInfo0, RestoreFailGoalInfo),

    RezeroFailGoalInfo0 = impure_unreachable_init_goal_info(set_of_var.init,
        detism_failure),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst,
        RezeroFailGoalInfo0, RezeroFailGoalInfo),

    make_impure(GoalInfo0, GoalInfo),
    (
        CodeModel = model_det,
        GoalExpr = conj(plain_conj, [SaveStuff, Goal0, RestoreStuff]),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        CodeModel = model_semi,
        GoalExpr = conj(plain_conj, [
            SaveStuff,
            hlds_goal(
                disj([
                    hlds_goal(
                        conj(plain_conj, [Goal0, RestoreStuff]),
                        ExtGoalInfo),
                    hlds_goal(
                        conj(plain_conj, [RestoreStuff, FailGoal]),
                        RestoreFailGoalInfo)
                ]),
                ExtGoalInfo)
        ]),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        CodeModel = model_non,
        GoalExpr = conj(plain_conj, [
            SaveStuff,
            hlds_goal(
                disj([
                    hlds_goal(
                        conj(plain_conj, [
                            Goal0,
                            hlds_goal(
                                disj([
                                    RestoreStuff,
                                    hlds_goal(
                                        conj(plain_conj,
                                            [ReZeroStuff, FailGoal]),
                                        RezeroFailGoalInfo)
                                ]),
                                NoBindExtGoalInfo)
                        ]),
                        ExtGoalInfo),
                    hlds_goal(
                        conj(plain_conj, [RestoreStuff, FailGoal]),
                        RestoreFailGoalInfo)
                ]),
                ExtGoalInfo)
        ]),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ).

:- pred deep_prof_wrap_foreign_code(hlds_goal::in, hlds_goal::out,
    deep_info::in, deep_info::out) is det.

deep_prof_wrap_foreign_code(Goal0, Goal, !DeepInfo) :-
    Goal0 = hlds_goal(_GoalExpr0, GoalInfo0),
    GoalId = goal_info_get_goal_id(GoalInfo0),
    ContainingGoalMap = !.DeepInfo ^ deep_containing_goal_map,
    GoalPath = goal_id_to_forward_path(ContainingGoalMap, GoalId),

    SiteNumCounter0 = !.DeepInfo ^ deep_site_num_counter,
    counter.allocate(SiteNum, SiteNumCounter0, SiteNumCounter),
    generate_var("SiteNum", int_type, SiteNumVar, !.DeepInfo ^ deep_varinfo,
        VarInfo),
    generate_deep_const_unify(some_int_const(int_const(SiteNum)),
        SiteNumVar, SiteNumVarGoal),

    ModuleInfo = !.DeepInfo ^ deep_module_info,
    generate_deep_det_call(ModuleInfo, "prepare_for_callback", 1,
        [SiteNumVar], [], PrepareGoal),

    Context = goal_info_get_context(GoalInfo0),
    LineNumber = term.context_line(Context),
    FileName0 = term.context_file(Context),
    compress_filename(!.DeepInfo, FileName0, FileName),
    CallSite = callback(FileName, LineNumber, GoalPath),

    make_impure(GoalInfo0, GoalInfo1),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo1, GoalInfo),
    GoalExpr = conj(plain_conj, [SiteNumVarGoal, PrepareGoal, Goal0]),
    Goal = hlds_goal(GoalExpr, GoalInfo),
    !DeepInfo ^ deep_site_num_counter := SiteNumCounter,
    !DeepInfo ^ deep_varinfo := VarInfo,
    !DeepInfo ^ deep_call_sites :=
        cord.snoc(!.DeepInfo ^ deep_call_sites, CallSite).

:- pred compress_filename(deep_info::in, string::in, string::out) is det.

compress_filename(Deep, FileName0, FileName) :-
    ( if FileName0 = Deep ^ deep_proc_filename then
        FileName = ""
    else
        FileName = FileName0
    ).

:- type call_class
    --->    call_class_normal(pred_proc_id)
            % For normal first order calls

    ;       call_class_special(pred_proc_id, prog_var)
            % For calls to unify/2, compare/3 and
            % compare_representation/3

    ;       call_class_generic(generic_call).
            % For higher order and typeclass method calls

:- func classify_call(module_info, hlds_goal_expr) = call_class.

classify_call(ModuleInfo, Expr) = Class :-
    (
        Expr = plain_call(PredId, ProcId, Args, _, _, _),
        ( if
            lookup_builtin_pred_proc_id(ModuleInfo,
                mercury_public_builtin_module, "unify",
                pf_predicate, user_arity(2), mode_no(0), PredId, _),
            Args = [TypeInfoVar | _]
        then
            Class = call_class_special(proc(PredId, ProcId), TypeInfoVar)
        else if
            lookup_builtin_pred_proc_id(ModuleInfo,
                mercury_public_builtin_module, "compare",
                pf_predicate, user_arity(3), mode_no(0), PredId, _),
            Args = [TypeInfoVar | _]
        then
            Class = call_class_special(proc(PredId, ProcId), TypeInfoVar)
        else if
            lookup_builtin_pred_proc_id(ModuleInfo,
                mercury_public_builtin_module,
                "compare_representation", pf_predicate, user_arity(3),
                mode_no(0), PredId, _),
            Args = [TypeInfoVar | _]
        then
            Class = call_class_special(proc(PredId, ProcId), TypeInfoVar)
        else
            Class = call_class_normal(proc(PredId, ProcId))
        )
    ;
        Expr = generic_call(Generic, _, _, _, _),
        Class = call_class_generic(Generic)
    ;
        ( Expr = call_foreign_proc(_, _, _, _, _, _, _)
        ; Expr = unify(_, _, _, _, _)
        ; Expr = conj(_, _)
        ; Expr = disj(_)
        ; Expr = switch(_, _, _)
        ; Expr = if_then_else(_, _, _, _)
        ; Expr = negation(_)
        ; Expr = scope(_, _)
        ; Expr = shorthand(_)
        ),
        unexpected($pred, "unexpected goal type")
    ).

:- func compute_type_subst(hlds_goal_expr, deep_info) = string.

% XXX we don't compute type substitution strings yet.
compute_type_subst(_, _) = "".

    % The maximum value of N for which save_recursion_depth_N,
    % restore_recursion_depth_exit_N and restore_recursion_depth_fail_N
    % exist in library/profiling_builtin.m.
    %
:- func max_save_restore_vector_size = int.

max_save_restore_vector_size = 9.

:- pred generate_recursion_counter_saves_and_restores(list(int)::in,
    prog_var::in, list(hlds_goal)::out, list(hlds_goal)::out,
    list(hlds_goal)::out, list(prog_var)::out,
    deep_info::in, deep_info::out) is det.

generate_recursion_counter_saves_and_restores(CSNs, CSDVar, CallGoals,
        ExitGoals, FailGoals, ExtraVars, !DeepInfo) :-
    list.chunk(CSNs, max_save_restore_vector_size, CSNChunks),
    generate_recursion_counter_saves_and_restores_2(CSNChunks, CSDVar,
        CallGoals, ExitGoals, FailGoals, ExtraVars, !DeepInfo).

:- pred generate_recursion_counter_saves_and_restores_2(list(list(int))::in,
    prog_var::in, list(hlds_goal)::out, list(hlds_goal)::out,
    list(hlds_goal)::out, list(prog_var)::out,
    deep_info::in, deep_info::out) is det.

generate_recursion_counter_saves_and_restores_2([], _, [], [], [], [],
        !DeepInfo).
generate_recursion_counter_saves_and_restores_2([Chunk | Chunks], CSDVar,
        CallGoals, ExitGoals, FailGoals, ExtraVars, !DeepInfo) :-
    list.map_foldl(generate_depth_var, Chunk, DepthVars, !DeepInfo),

    % We generate three separate variables to hold the constant CSN vector.
    % If we used only one, the code generator would have to save its value
    % on the stack when we enter the disjunction that wraps the goal.
    list.length(Chunk, Length),
    generate_csn_vector(Length, Chunk, CSNCallVars, CSNCallGoals, CallCellVar,
        !DeepInfo),
    generate_csn_vector(Length, Chunk, CSNExitVars, CSNExitGoals, ExitCellVar,
        !DeepInfo),
    generate_csn_vector(Length, Chunk, CSNFailVars, CSNFailGoals, FailCellVar,
        !DeepInfo),
    list.condense([CSNCallVars, CSNExitVars, CSNFailVars], CSNExtraVars),

    CallPredName = string.format("save_recursion_depth_%d", [i(Length)]),
    ExitPredName = string.format("restore_recursion_depth_exit_%d",
        [i(Length)]),
    FailPredName = string.format("restore_recursion_depth_fail_%d",
        [i(Length)]),
    ModuleInfo = !.DeepInfo ^ deep_module_info,
    generate_deep_det_call(ModuleInfo, CallPredName, Length + 2,
        [CSDVar, CallCellVar | DepthVars], DepthVars, CallCellGoal),
    generate_deep_det_call(ModuleInfo, ExitPredName, Length + 2,
        [CSDVar, ExitCellVar | DepthVars], [], ExitCellGoal),
    generate_deep_det_call(ModuleInfo, FailPredName, Length + 2,
        [CSDVar, FailCellVar | DepthVars], [], FailCellGoal),

    generate_recursion_counter_saves_and_restores_2(Chunks, CSDVar,
        TailCallGoals, TailExitGoals, TailFailGoals, TailExtraVars, !DeepInfo),

    CallGoals = CSNCallGoals ++ [CallCellGoal | TailCallGoals],
    ExitGoals = CSNExitGoals ++ [ExitCellGoal | TailExitGoals],
    FailGoals = CSNFailGoals ++ [FailCellGoal | TailFailGoals],
    ExtraVars = CSNExtraVars ++ TailExtraVars.

:- pred generate_depth_var(int::in, prog_var::out,
    deep_info::in, deep_info::out) is det.

generate_depth_var(CSN, DepthVar, !DeepInfo) :-
    VarInfo0 = !.DeepInfo ^ deep_varinfo,
    VarName = string.format("Depth%d", [i(CSN)]),
    generate_var(VarName, int_type, DepthVar, VarInfo0, VarInfo),
    !DeepInfo ^ deep_varinfo:= VarInfo.

:- pred generate_csn_vector(int::in, list(int)::in, list(prog_var)::out,
    list(hlds_goal)::out, prog_var::out,
    deep_info::in, deep_info::out) is det.

generate_csn_vector(Length, CSNs, CSNVars, UnifyGoals, CellVar, !DeepInfo) :-
    ( if CSNs = [CSN] then
        generate_single_csn_unify(CSN, CSNVar - UnifyGoal, !DeepInfo),
        CSNVars = [CSNVar],
        UnifyGoals = [UnifyGoal],
        CellVar = CSNVar
    else
        expect(Length =< max_save_restore_vector_size, $pred, "too long"),
        list.map_foldl(generate_single_csn_unify, CSNs, CSNVarsGoals,
            !DeepInfo),
        InnerVars = assoc_list.keys(CSNVarsGoals),
        InnerGoals = assoc_list.values(CSNVarsGoals),
        generate_csn_vector_cell(Length, InnerVars, CellVar, CellGoal,
            !DeepInfo),
        CSNVars = [CellVar | InnerVars],
        UnifyGoals = InnerGoals ++ [CellGoal]
    ).

:- pred generate_csn_vector_cell(int::in, list(prog_var)::in,
    prog_var::out, hlds_goal::out, deep_info::in, deep_info::out) is det.

generate_csn_vector_cell(Length, CSNVars, CellVar, CellGoal, !DeepInfo) :-
    VarInfo0 = !.DeepInfo ^ deep_varinfo,
    ProfilingBuiltin = mercury_profiling_builtin_module,
    CellTypeName = string.format("call_site_nums_%d", [i(Length)]),
    CellTypeCtor = type_ctor(qualified(ProfilingBuiltin, CellTypeName), 0),
    construct_type(CellTypeCtor, [], CellType),
    generate_var("CSNCell", CellType, CellVar, VarInfo0, VarInfo),
    !DeepInfo ^ deep_varinfo := VarInfo,
    ConsId = cons(qualified(ProfilingBuiltin, CellTypeName), Length,
        CellTypeCtor),
    generate_deep_cell_unify(Length, ConsId, CSNVars, CellVar, CellGoal).

:- pred generate_single_csn_unify(int::in,
    pair(prog_var, hlds_goal)::out, deep_info::in, deep_info::out) is det.

generate_single_csn_unify(CSN, CSNVar - UnifyGoal, !DeepInfo) :-
    VarInfo0 = !.DeepInfo ^ deep_varinfo,
    VarName = string.format("CSN%d", [i(CSN)]),
    generate_var(VarName, int_type, CSNVar, VarInfo0, VarInfo),
    !DeepInfo ^ deep_varinfo := VarInfo,
    generate_deep_const_unify(some_int_const(int_const(CSN)),
        CSNVar, UnifyGoal).

:- pred generate_deep_det_call(module_info::in, string::in, int::in,
    list(prog_var)::in, list(prog_var)::in, hlds_goal::out) is det.

generate_deep_det_call(ModuleInfo, Name, Arity, ArgVars, OutputVars, Goal) :-
    generate_deep_call(ModuleInfo, Name, Arity, ArgVars, yes(OutputVars),
        detism_det, Goal).

generate_deep_call(ModuleInfo, Name, Arity, ArgVars, MaybeOutputVars, Detism,
        Goal) :-
    get_deep_profile_builtin_ppid(ModuleInfo, Name, Arity, PredId, ProcId),
    NonLocals = set_of_var.list_to_set(ArgVars),
    (
        MaybeOutputVars = yes(OutputVars),
        InstMapDelta = instmap_delta_bind_vars(OutputVars)
    ;
        MaybeOutputVars = no,
        instmap_delta_init_unreachable(InstMapDelta)
    ),
    SymName = unqualified(Name),
    GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
    GoalInfo1 = impure_init_goal_info(NonLocals, InstMapDelta, Detism),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_deep_const_unify(ConsId, Var, Goal) :-
    Ground = ground(shared, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    Unification = construct(Var, ConsId, [], [],
        construct_statically(born_static), cell_is_shared,
        no_construct_sub_info),
    GoalExpr = unify(Var, rhs_functor(ConsId, is_not_exist_constr, []),
        UnifyMode, Unification, unify_context(umc_explicit, [])),
    NonLocals = set_of_var.make_singleton(Var),
    InstMapDelta = instmap_delta_bind_var(Var),
    goal_info_init(NonLocals, InstMapDelta, detism_det, purity_pure,
        GoalInfo1),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred generate_deep_cell_unify(int::in, cons_id::in, list(prog_var)::in,
    prog_var::in, hlds_goal::out) is det.

generate_deep_cell_unify(Length, ConsId, Args, Var, Goal) :-
    Ground = ground(shared, none_or_default_func),
    UnifyMode = unify_modes_li_lf_ri_rf(free, Ground, Ground, Ground),
    list.duplicate(Length, UnifyMode, ArgModes),
    Unification = construct(Var, ConsId, Args, ArgModes,
        construct_statically(born_static), cell_is_shared,
        no_construct_sub_info),
    GoalExpr = unify(Var, rhs_functor(ConsId, is_not_exist_constr, Args),
        UnifyMode, Unification, unify_context(umc_explicit, [])),
    NonLocals = set_of_var.list_to_set([Var | Args]),
    InstMapDelta = instmap_delta_bind_var(Var),
    Determinism = detism_det,
    goal_info_init(NonLocals, InstMapDelta, Determinism, purity_pure,
        GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

generate_var(Name, Type, Var, !VarInfo) :-
    some [!VarSet, !VarTypes]
    (
        !.VarInfo = prog_var_set_types(!:VarSet, !:VarTypes),
        generate_var_2(Name, Type, Var, !VarSet, !VarTypes),
        !:VarInfo = prog_var_set_types(!.VarSet, !.VarTypes)
    ).

    % Create a variable with the given name and type, adding it to the
    % separate prog_varset and vartypes structures.
    %
:- pred generate_var_2(string::in, mer_type::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

generate_var_2(Name, Type, Var, !VarSet, !VarTypes) :-
    varset.new_named_var(Name, Var, !VarSet),
    add_var_type(Var, Type, !VarTypes).

:- pred maybe_generate_activation_ptr(bool::in, prog_var::in, prog_var::in,
    maybe(prog_var)::out, hlds_deep_excp_vars::out,
    prog_var_set_types::in, prog_var_set_types::out) is det.

maybe_generate_activation_ptr(UseActivationCounts, TopCSD, MiddleCSD,
    MaybeActivationPtr, ExcpVars, !VarInfo) :-
    (
        UseActivationCounts = no,
        generate_var("ActivationPtr", c_pointer_type, ActivationPtr0,
            !VarInfo),
        MaybeActivationPtr = yes(ActivationPtr0)
    ;
        UseActivationCounts = yes,
        MaybeActivationPtr = no
    ),
    ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD, MaybeActivationPtr).

:- pred generate_outermost_proc_dyns(bool::in, prog_var::in, prog_var::in,
    maybe(prog_var)::out, prog_var::out, hlds_deep_excp_vars::out,
    prog_var_set_types::in, prog_var_set_types::out) is det.

generate_outermost_proc_dyns(UseActivationCounts, TopCSD, MiddleCSD,
        MaybeOldActivationPtr, NewOutermostProcDyn, ExcpVars, !VarInfo) :-
    (
        UseActivationCounts = no,
        generate_var("OldOutermost", c_pointer_type, OldOutermostProcDyn0,
            !VarInfo),
        MaybeOldActivationPtr = yes(OldOutermostProcDyn0)
    ;
        UseActivationCounts = yes,
        MaybeOldActivationPtr = no
    ),
    ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD, MaybeOldActivationPtr),
    generate_var("NewOutermost", c_pointer_type, NewOutermostProcDyn,
        !VarInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

get_deep_profile_builtin_ppid(ModuleInfo, Name, Arity, PredId, ProcId) :-
    ModuleName = mercury_profiling_builtin_module,
    module_info_get_predicate_table(ModuleInfo, PredTable),
    predicate_table_lookup_pred_m_n_a(PredTable,
        is_fully_qualified, ModuleName, Name, user_arity(Arity), PredIds),
    (
        PredIds = [],
        unexpected($pred, "no pred_id")
    ;
        PredIds = [PredId],
        predicate_table_get_pred_id_table(PredTable, PredIdTable),
        map.lookup(PredIdTable, PredId, PredInfo),
        ProcIds = pred_info_all_procids(PredInfo),
        (
            ProcIds = [],
            unexpected($pred, "no proc_id")
        ;
            ProcIds = [ProcId]
        ;
            ProcIds = [_, _ | _],
            unexpected($pred, "proc_id not unique")
        )
    ;
        PredIds = [_, _ | _],
        unexpected($pred, "pred_id not unique")
    ).

%-----------------------------------------------------------------------------%

:- pred extract_deep_rec_info(maybe(deep_profile_proc_info)::in,
    maybe(deep_recursion_info)::out) is det.

extract_deep_rec_info(MaybeDeepProfInfo, MaybeRecInfo) :-
    (
        MaybeDeepProfInfo = yes(DeepProfInfo),
        DeepProfInfo = deep_profile_proc_info(MaybeRecInfo, _, _)
    ;
        MaybeDeepProfInfo = no,
        MaybeRecInfo = no
    ).

%-----------------------------------------------------------------------------%
:- end_module ll_backend.deep_profiling.
%-----------------------------------------------------------------------------%
