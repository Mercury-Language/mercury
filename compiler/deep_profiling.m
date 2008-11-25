%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2001-2008 The University of Melbourne.
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

:- import_module hlds.hlds_module.

:- import_module io.

%-----------------------------------------------------------------------------%

:- pred apply_deep_profiling_transformation(module_info::in, module_info::out,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module backend_libs.rtti.
:- import_module check_hlds.mode_util.
:- import_module check_hlds.type_util.
:- import_module hlds.code_model.
:- import_module hlds.goal_util.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_pred.
:- import_module hlds.hlds_rtti.
:- import_module hlds.instmap.
:- import_module hlds.pred_table.
:- import_module libs.compiler_util.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.program_representation.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_type.
:- import_module transform_hlds.
:- import_module transform_hlds.dead_proc_elim.
:- import_module transform_hlds.dependency_graph.

:- import_module assoc_list.
:- import_module bool.
:- import_module cord.
:- import_module counter.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module svvarset.
:- import_module term.
:- import_module varset.

%-----------------------------------------------------------------------------%

apply_deep_profiling_transformation(!ModuleInfo, !IO) :-
    % XXX The dead proc elimination pass changes the status of opt_imported
    % predicates, which changes what labels they get generated. The
    % call_site_static structures we generate must match the labels created
    % during code generation.
    dead_proc_elim(elim_opt_imported, !ModuleInfo, _ElimSpecs),
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, deep_profile_tail_recursion,
        TailRecursion),
    (
        TailRecursion = yes,
        apply_deep_prof_tail_rec_transform(!ModuleInfo)
    ;
        TailRecursion = no
    ),
    module_info_predids(PredIds, !ModuleInfo),
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    predicate_table_get_preds(PredTable0, PredMap0),
    list.foldl2(transform_predicate(!.ModuleInfo), PredIds, PredMap0, PredMap,
        !IO),
    predicate_table_set_preds(PredMap, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo).

%-----------------------------------------------------------------------------%

:- pred apply_deep_prof_tail_rec_transform(module_info::in, module_info::out)
    is det.

apply_deep_prof_tail_rec_transform(!ModuleInfo) :-
    module_info_ensure_dependency_info(!ModuleInfo),
    module_info_dependency_info(!.ModuleInfo, DepInfo),
    hlds_dependency_info_get_dependency_ordering(DepInfo, SCCs),
    list.foldl(apply_deep_prof_tail_rec_transform_to_scc, SCCs, !ModuleInfo).

:- pred apply_deep_prof_tail_rec_transform_to_scc(list(pred_proc_id)::in,
    module_info::in, module_info::out) is det.

apply_deep_prof_tail_rec_transform_to_scc(SCC, !ModuleInfo) :-
    % For the time being, we only look for self-tail-recursive calls.
    list.foldl(apply_deep_prof_tail_rec_transform_to_proc, SCC, !ModuleInfo).

:- pred apply_deep_prof_tail_rec_transform_to_proc(pred_proc_id::in,
    module_info::in, module_info::out) is det.

apply_deep_prof_tail_rec_transform_to_proc(PredProcId, !ModuleInfo) :-
    PredProcId = proc(PredId, ProcId),
    module_info_preds(!.ModuleInfo, PredTable0),
    map.lookup(PredTable0, PredId, PredInfo0),
    pred_info_get_arg_types(PredInfo0, Types),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    map.lookup(ProcTable0, ProcId, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),
    proc_info_interface_determinism(ProcInfo0, Detism),
    (
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
        FoundTailCall = yes
    ->
        proc_info_set_goal(Goal, ProcInfo0, ProcInfo1),
        figure_out_rec_call_numbers(Goal, 0, _N, [], TailCallSites),
        OrigDeepRecInfo = yes(deep_recursion_info(
            outer_proc(ClonePredProcId),
            [visible_scc_data(PredProcId, ClonePredProcId, TailCallSites)])),
        make_deep_original_body(ProcInfo0, !.ModuleInfo, DeepOriginalBody),
        OrigDeepProfileInfo = deep_profile_proc_info(OrigDeepRecInfo, no,
            DeepOriginalBody),
        CloneDeepRecInfo = yes(deep_recursion_info(inner_proc(PredProcId),
            [visible_scc_data(PredProcId, ClonePredProcId, TailCallSites)])),
        CloneDeepProfileInfo = deep_profile_proc_info(CloneDeepRecInfo, no,
            DeepOriginalBody),
        proc_info_set_maybe_deep_profile_info(yes(OrigDeepProfileInfo),
            ProcInfo1, ProcInfo),
        proc_info_set_maybe_deep_profile_info(
            yes(CloneDeepProfileInfo), ProcInfo1, CloneProcInfo),
        map.det_update(ProcTable0, ProcId, ProcInfo, ProcTable1),
        map.det_insert(ProcTable1, CloneProcId, CloneProcInfo, ProcTable),
        pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
        map.det_update(PredTable0, PredId, PredInfo, PredTable),
        module_info_set_preds(PredTable, !ModuleInfo)
    ;
        true
    ).

:- pred find_list_of_output_args(list(prog_var)::in, list(mer_mode)::in,
    list(mer_type)::in, module_info::in, list(prog_var)::out) is det.

find_list_of_output_args(Vars, Modes, Types, ModuleInfo, !:Outputs) :-
    ( find_list_of_output_args_2(Vars, Modes, Types, ModuleInfo, !:Outputs) ->
        true
    ;
        unexpected(this_file, "find_list_of_output_args: list length mismatch")
    ).

:- pred find_list_of_output_args_2(list(prog_var)::in, list(mer_mode)::in,
    list(mer_type)::in, module_info::in, list(prog_var)::out) is semidet.

find_list_of_output_args_2([], [], [], _, []).
find_list_of_output_args_2([Var | Vars], [Mode | Modes], [Type | Types],
        ModuleInfo, Outputs) :-
    find_list_of_output_args_2(Vars, Modes, Types, ModuleInfo, Outputs1),
    mode_to_arg_mode(ModuleInfo, Mode, Type, ArgMode),
    (
        ArgMode = top_in,
        Outputs = Outputs1
    ;
        ( ArgMode = top_out
        ; ArgMode = top_unused
        ),
        Outputs = [Var | Outputs1]
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
        (
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
        ->
            ClonePredProcId = proc(ClonePredId, CloneProcId),
            GoalExpr = plain_call(ClonePredId, CloneProcId, Args,
                Builtin, UnifyContext, SymName),
            goal_info_add_feature(feature_deep_tail_rec_call,
                GoalInfo0, GoalInfo),
            Goal = hlds_goal(GoalExpr, GoalInfo),
            !:FoundTailCall = yes
        ;
            Goal = Goal0
        ),
        Continue = no
    ;
        GoalExpr0 = generic_call(_, _, _, _),
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
        unexpected(this_file, "shorthand in apply_deep_prof_tail_rec_to_goal")
    ).

:- pred apply_deep_prof_tail_rec_to_assign(list(prog_var)::in,
    prog_var::in, prog_var::in, list(prog_var)::out) is det.

apply_deep_prof_tail_rec_to_assign([], _, _, []).
apply_deep_prof_tail_rec_to_assign([Output0 | Outputs0], ToVar, FromVar,
        [Output | Outputs]) :-
    ( ToVar = Output0 ->
        Output = FromVar
    ;
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
        ( get_may_call_mercury(Attrs) = proc_may_call_mercury ->
            !:N = !.N + 1
        ;
            true
        )
    ;
        GoalExpr = plain_call(_, _, _, BuiltinState, _, _),
        Features = goal_info_get_features(GoalInfo),
        ( set.member(feature_deep_tail_rec_call, Features) ->
            !:TailCallSites = [!.N | !.TailCallSites]
        ;
            true
        ),
        (
            ( BuiltinState = out_of_line_builtin
            ; BuiltinState = not_builtin
            ),
            !:N = !.N + 1
        ;
            BuiltinState = inline_builtin
        )
    ;
        GoalExpr = generic_call(_, _, _, _),
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
        GoalExpr = scope(_, Goal1),
        figure_out_rec_call_numbers(Goal1, !N, !TailCallSites)
    ;
        GoalExpr = negation(Goal1),
        figure_out_rec_call_numbers(Goal1, !N, !TailCallSites)
    ;
        GoalExpr = shorthand(_),
        unexpected(this_file, "shorthand in figure_out_rec_call_numbers")
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

:- pred transform_predicate(module_info::in, pred_id::in,
    pred_table::in, pred_table::out, io::di, io::uo) is det.

transform_predicate(ModuleInfo, PredId, PredMap0, PredMap, !IO) :-
    map.lookup(PredMap0, PredId, PredInfo0),
    ProcIds = pred_info_non_imported_procids(PredInfo0),
    pred_info_get_procedures(PredInfo0, ProcTable0),
    list.foldl2(maybe_transform_procedure(ModuleInfo, PredId),
        ProcIds, ProcTable0, ProcTable, !IO),
    pred_info_set_procedures(ProcTable, PredInfo0, PredInfo),
    map.det_update(PredMap0, PredId, PredInfo, PredMap).

:- pred maybe_transform_procedure(module_info::in, pred_id::in, proc_id::in,
    proc_table::in, proc_table::out, io::di, io::uo) is det.

maybe_transform_procedure(ModuleInfo, PredId, ProcId, !ProcTable, !IO) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    proc_info_get_goal(ProcInfo0, Goal0),
    PredModuleName = predicate_module(ModuleInfo, PredId),
    (
        % XXX We need to eliminate nondet C code...
        Goal0 = hlds_goal(call_foreign_proc(_, _, _, _, _, _, Impl), _),
        Impl = fc_impl_model_non(_, _, _, _, _, _, _, _, _)
    ->
        unexpected(this_file,
            "deep profiling is incompatible with nondet foreign code")
    ;
        % We don't want to transform the procedures for managing the deep
        % profiling call graph, or we'd get infinite recursion.
        PredModuleName = mercury_profiling_builtin_module
    ->
        true
    ;
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),
        ProcName = pred_proc_id_pair_to_string(ModuleInfo, PredId, ProcId),
        maybe_write_string(VeryVerbose,
            string.format("%% Deep profiling: %s\n", [s(ProcName)]),
            !IO),
        deep_prof_transform_proc(ModuleInfo, proc(PredId, ProcId),
            ProcInfo0, ProcInfo),
        map.det_update(!.ProcTable, ProcId, ProcInfo, !:ProcTable)
    ).

:- pred deep_prof_transform_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

deep_prof_transform_proc(ModuleInfo, PredProcId, !ProcInfo) :-
    proc_info_get_maybe_deep_profile_info(!.ProcInfo, MaybeDeepInfo),
    (
        MaybeDeepInfo = yes(DeepInfo0),
        DeepInfo0 = deep_profile_proc_info(MaybeDeepRecInfo, _, OrigBody),
        (
            MaybeDeepRecInfo = yes(RecInfo),
            RecInfo ^ role = inner_proc(_)
        ->
            transform_inner_proc(ModuleInfo, PredProcId, !ProcInfo),
            MaybeDeepLayoutInfo = no
        ;
            transform_normal_proc(ModuleInfo, PredProcId, !ProcInfo,
                DeepLayoutInfo),
            MaybeDeepLayoutInfo = yes(DeepLayoutInfo)
        )
    ;
        MaybeDeepInfo = no,
        make_deep_original_body(!.ProcInfo, ModuleInfo, OrigBody),
        transform_normal_proc(ModuleInfo, PredProcId, !ProcInfo,
            DeepLayoutInfo),
        MaybeDeepLayoutInfo = yes(DeepLayoutInfo),
        MaybeDeepRecInfo = no
    ),
    DeepInfo = deep_profile_proc_info(MaybeDeepRecInfo, MaybeDeepLayoutInfo,
        OrigBody),
    proc_info_set_maybe_deep_profile_info(yes(DeepInfo), !ProcInfo).

%-----------------------------------------------------------------------------%

:- pred make_deep_original_body(proc_info::in, module_info::in,
    deep_original_body::out) is det.

make_deep_original_body(ProcInfo, ModuleInfo, DeepOriginalBody) :-
    proc_info_get_goal(ProcInfo, Body),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_initial_instmap(ProcInfo, ModuleInfo, Instmap),
    proc_info_get_vartypes(ProcInfo, Vartypes),
    proc_info_get_declared_determinism(ProcInfo, MaybeDetism),
    (
        MaybeDetism = yes(Detism)
    ;
        MaybeDetism = no,
        proc_info_get_inferred_determinism(ProcInfo, Detism)
    ),
    DeepOriginalBody = deep_original_body(Body, HeadVars, Instmap, Vartypes,
        Detism).

%-----------------------------------------------------------------------------%

    % Information relating to variables.  When a variable is added to a
    % procedure both these values should be updated, and can be using
    % generate_var/5 below.
    %
:- type var_info
    --->    var_info(
                varinfo_varset      :: prog_varset,
                varinfo_vartypes    :: vartypes
            ).

    % This structure contains stateful information used throughout the deep
    % profiling transformation of a procedure.
    %
:- type deep_info
    --->    deep_info(
                deep_module_info        :: module_info,
                deep_pred_proc_id       :: pred_proc_id,
                deep_current_csd        :: prog_var,
                deep_site_num_counter   :: counter,
                deep_call_sites         :: list(call_site_static_data),
                deep_varinfos           :: var_info,
                deep_proc_filename      :: string,
                deep_maybe_rec_info     :: maybe(deep_recursion_info)
            ).

    % Transfrom a procedure.
    %
:- pred transform_normal_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out, hlds_deep_layout::out) is det.

transform_normal_proc(ModuleInfo, PredProcId, !ProcInfo, DeepLayoutInfo) :-
    module_info_get_globals(ModuleInfo, Globals),
    proc_info_get_goal(!.ProcInfo, Goal0),
    Goal0 = hlds_goal(_, GoalInfo0),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    some [!VarInfo] (
        !:VarInfo = var_info(VarSet0, VarTypes0),
        generate_var("TopCSD", c_pointer_type, TopCSD, !VarInfo),
        generate_var("MiddleCSD", c_pointer_type, MiddleCSD, !VarInfo),
        generate_var("ProcStaticLayout", c_pointer_type, ProcStaticVar,
            !VarInfo),

        proc_info_get_context(!.ProcInfo, Context),
        FileName = term.context_file(Context),
        LineNumber = term.context_line(Context),

        proc_info_get_maybe_deep_profile_info(!.ProcInfo, MaybeDeepProfInfo),
        extract_deep_rec_info(MaybeDeepProfInfo, MaybeRecInfo),
        DeepInfo0 = deep_info(ModuleInfo, PredProcId, MiddleCSD,
            counter.init(0), [], !.VarInfo, FileName, MaybeRecInfo),

        % This call transforms the goals of the procedure.
        deep_prof_transform_goal(empty_goal_path, Goal0, Goal1, _,
            DeepInfo0, DeepInfo),
        !:VarInfo = DeepInfo ^ deep_varinfos,
        CallSites = DeepInfo ^ deep_call_sites,

        % Do coverage profiling if requested.
        globals.lookup_bool_option(Globals, coverage_profiling,
            DoCoverageProfiling),
        (
            DoCoverageProfiling = yes,
            coverage_prof_transform_goal(ModuleInfo, PredProcId,
                MaybeRecInfo, Goal1, TransformedGoal, !VarInfo,
                CoveragePoints)
        ;
            DoCoverageProfiling = no,
            CoveragePoints = [],
            TransformedGoal = Goal1
        ),

        (
            MaybeRecInfo = yes(RecInfo),
            RecInfo ^ role = inner_proc(OuterPredProcId)
        ->
            OuterPredProcId = proc(PredId, ProcId)
        ;
            PredProcId = proc(PredId, ProcId)
        ),

        globals.lookup_bool_option(Globals, use_activation_counts,
            UseActivationCounts),

        IsInInterface = is_proc_in_interface(ModuleInfo, PredId, ProcId),
        ProcStatic = hlds_proc_static(FileName, LineNumber, IsInInterface,
            CallSites, CoveragePoints),
        ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
        ProcStaticConsId = deep_profiling_proc_layout(ShroudedPredProcId),
        generate_unify(ProcStaticConsId, ProcStaticVar,
            BindProcStaticVarGoal),

        % Wrap the procedure body inside more goals that run the port code
        % as necessary.  This depends on the code model of the procedure
        % being considered.
        CodeModel = proc_info_interface_code_model(!.ProcInfo),
        (
            (
                CodeModel = model_det,
                BuildProcBody = build_det_proc_body
            ;
                CodeModel = model_semi,
                BuildProcBody = build_semi_proc_body
            ),
            maybe_generate_activation_ptr(UseActivationCounts, TopCSD,
                MiddleCSD, MaybeActivationPtr, ExcpVars, !VarInfo),
            BuildProcBody(ModuleInfo, TopCSD, MiddleCSD, ProcStaticVar,
                MaybeActivationPtr, GoalInfo0, BindProcStaticVarGoal,
                TransformedGoal, Goal)
        ;
            CodeModel = model_non,
            generate_outermost_proc_dyns(UseActivationCounts, TopCSD,
                MiddleCSD, MaybeOldActivationPtr, NewOutermostProcDyn,
                ExcpVars, !VarInfo),
            build_non_proc_body(ModuleInfo, TopCSD, MiddleCSD,
                ProcStaticVar, MaybeOldActivationPtr, NewOutermostProcDyn,
                GoalInfo0, BindProcStaticVarGoal, TransformedGoal, Goal)
        ),

        !.VarInfo = var_info(Vars, VarTypes)
    ),
    proc_info_set_varset(Vars, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(Goal, !ProcInfo),
    DeepLayoutInfo = hlds_deep_layout(ProcStatic, ExcpVars).


    % Wrap the procedure body in the deep profiling port goals.
    %
    % When modifing this transformation be sure to modify original_root/3 in
    % deep_profiler/program_represetntation_utils.m which must be able to undo
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

    % Wrap the goal for a semidet procedure,
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
            [TopCSD, MiddleCSD, ActivationPtr1], no, detism_failure,
            FailPortCode),
        NewNonlocals = list_to_set([TopCSD, MiddleCSD, ActivationPtr1])
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
            [TopCSD, MiddleCSD], no, detism_failure, FailPortCode),
        NewNonlocals = list_to_set([TopCSD, MiddleCSD])
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
            [TopCSD, MiddleCSD, OldOutermostProcDyn2], no,
            detism_failure, FailPortCode),
        generate_deep_call(ModuleInfo, "non_redo_port_code_sr", 2,
            [MiddleCSD, NewOutermostProcDyn], no,
            detism_failure, RedoPortCode0),
        NewNonlocals = list_to_set([TopCSD, MiddleCSD, OldOutermostProcDyn2])
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
            [TopCSD, MiddleCSD], no, detism_failure, FailPortCode),
        generate_deep_call(ModuleInfo, "non_redo_port_code_ac", 2,
            [MiddleCSD, NewOutermostProcDyn], no,
            detism_failure, RedoPortCode0),
        NewNonlocals = list_to_set([TopCSD, MiddleCSD])
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

    ExitRedoNonLocals = set.union(NewNonlocals,
        list_to_set([NewOutermostProcDyn])),
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

    % Transform an inner procedure for deep profiling.  Inner procedures are
    % created by the tail recursion preservation pass above.
    %
    % XXX: Inner procedures have no coverage profiling transformation done to
    % them yet.  This is because they are currently broken, and hence disabled.
    %
:- pred transform_inner_proc(module_info::in, pred_proc_id::in,
    proc_info::in, proc_info::out) is det.

transform_inner_proc(ModuleInfo, PredProcId, !ProcInfo) :-
    proc_info_get_goal(!.ProcInfo, Goal0),
    Goal0 = hlds_goal(_, GoalInfo0),
    proc_info_get_varset(!.ProcInfo, VarSet0),
    proc_info_get_vartypes(!.ProcInfo, VarTypes0),
    VarInfo0 = var_info(VarSet0, VarTypes0),
    generate_var("MiddleCSD", c_pointer_type, MiddleCSD, VarInfo0,
        VarInfo1),

    Context = goal_info_get_context(GoalInfo0),
    FileName = term.context_file(Context),

    proc_info_get_maybe_deep_profile_info(!.ProcInfo, MaybeDeepProfInfo),
    extract_deep_rec_info(MaybeDeepProfInfo, MaybeRecInfo),
    DeepInfo0 = deep_info(ModuleInfo, PredProcId, MiddleCSD,
        counter.init(0), [], VarInfo1,
        FileName, MaybeRecInfo),

    deep_prof_transform_goal(empty_goal_path, Goal0, TransformedGoal, _,
        DeepInfo0, DeepInfo),

    VarInfo = DeepInfo ^ deep_varinfos,
    VarInfo = var_info(VarSet, VarTypes),

    proc_info_set_varset(VarSet, !ProcInfo),
    proc_info_set_vartypes(VarTypes, !ProcInfo),
    proc_info_set_goal(TransformedGoal, !ProcInfo).

%-----------------------------------------------------------------------------%

:- func is_proc_in_interface(module_info, pred_id, proc_id) = bool.

is_proc_in_interface(ModuleInfo, PredId, _ProcId) = IsInInterface :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    (
        ( pred_info_is_exported(PredInfo)
        ; pred_info_is_pseudo_exported(PredInfo)
        )
    ->
        IsInInterface = yes
    ;
        IsInInterface = no
    ).

%-----------------------------------------------------------------------------%

:- pred deep_prof_transform_goal(goal_path::in, hlds_goal::in, hlds_goal::out,
    bool::out, deep_info::in, deep_info::out) is det.

deep_prof_transform_goal(Path, Goal0, Goal, AddedImpurity, !DeepInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    goal_info_set_goal_path(Path, GoalInfo0, GoalInfo1),
    goal_info_set_mdprof_inst(goal_is_not_mdprof_inst, GoalInfo1, GoalInfo2),
    Goal1 = hlds_goal(GoalExpr0, GoalInfo2),
    (
        GoalExpr0 = plain_call(_, _, _, BuiltinState, _, _),
        (
            ( BuiltinState = out_of_line_builtin
            ; BuiltinState = not_builtin
            ),
            deep_prof_wrap_call(Path, Goal1, Goal, !DeepInfo),
            AddedImpurity = yes
        ;
            BuiltinState = inline_builtin,
            Goal = Goal1,
            AddedImpurity = no
        )
    ;
        GoalExpr0 = generic_call(GenericCall, _, _, _),
        (
            ( GenericCall = higher_order(_, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ),
            deep_prof_wrap_call(Path, Goal1, Goal, !DeepInfo),
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
        ( get_may_call_mercury(Attrs) = proc_may_call_mercury ->
            deep_prof_wrap_foreign_code(Path, Goal1, Goal, !DeepInfo),
            AddedImpurity = yes
        ;
            Goal = Goal1,
            AddedImpurity = no
        )
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        Goal = Goal1,
        AddedImpurity = no
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        deep_prof_transform_conj(0, ConjType, Path, Goals0, Goals,
            AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo2, GoalInfo),
        GoalExpr = conj(ConjType, Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = disj(Goals0),
        deep_prof_transform_disj(0, Path, Goals0, Goals, AddedImpurity,
            !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo2, GoalInfo),
        GoalExpr = disj(Goals),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = switch(Var, CF, Cases0),
        % XXX: This computation seems broken, it's been disabled so I can
        % relyably implement coverage profiling.  Test it on
        % erlang_rtti_implementation.deconstruct_2/9-2 whose switch's type has
        % 25 constructors yet this computes 27.  Are constructors different to
        % functors?
        % zs: this computation is NOT broken.
        % ModuleInfo = !.DeepInfo ^ deep_module_info,
        % VarTypes = !.DeepInfo ^ deep_varinfos ^ varinfo_vartypes,
        % map.lookup(VarTypes, Var, Type),
        % ( switch_type_num_functors(ModuleInfo, Type, NumFunctors) ->
        %     MaybeNumFunctors = yes(NumFunctors)
        % ;
        %     MaybeNumFunctors = no
        % ),
        MaybeNumFunctors = no,
        deep_prof_transform_switch(MaybeNumFunctors, 0, Path, Cases0, Cases,
            AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo2, GoalInfo),
        GoalExpr = switch(Var, CF, Cases),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = negation(SubGoal0),
        deep_prof_transform_goal(goal_path_add_at_end(Path, step_neg),
            SubGoal0, SubGoal, AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo2, GoalInfo),
        GoalExpr = negation(SubGoal),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = if_then_else(IVars, Cond0, Then0, Else0),
        deep_prof_transform_goal(goal_path_add_at_end(Path, step_ite_cond),
            Cond0, Cond, AddedImpurityC, !DeepInfo),
        deep_prof_transform_goal(goal_path_add_at_end(Path, step_ite_then),
            Then0, Then, AddedImpurityT, !DeepInfo),
        deep_prof_transform_goal(goal_path_add_at_end(Path, step_ite_else),
            Else0, Else, AddedImpurityE, !DeepInfo),
        (
            ( AddedImpurityC = yes
            ; AddedImpurityT = yes
            ; AddedImpurityE = yes
            )
        ->
            AddedImpurity = yes
        ;
            AddedImpurity = no
        ),
        add_impurity_if_needed(AddedImpurity, GoalInfo2, GoalInfo),
        GoalExpr = if_then_else(IVars, Cond, Then, Else),
        Goal = hlds_goal(GoalExpr, GoalInfo)
    ;
        GoalExpr0 = scope(Reason0, SubGoal0),
        SubGoal0 = hlds_goal(_, InnerInfo),
        OuterDetism = goal_info_get_determinism(GoalInfo2),
        InnerDetism = goal_info_get_determinism(InnerInfo),
        ( InnerDetism = OuterDetism ->
            MaybeCut = scope_is_no_cut,
            Reason = Reason0,
            AddForceCommit = no
        ;
            % Given a subgoal containing both at_most_many code and impure
            % code, determinism analysis will remove the `scope' wrapped
            % around that subgoal if it is allowed to. If we get here, then
            % the subgoal inside the `scope' contains at_most_many code
            % (which means that removing the scope will change its determinism)
            % and the deep profiling transformation will make it impure
            % as well.

            MaybeCut = scope_is_cut,
            ( Reason0 = commit(_) ->
                Reason = commit(force_pruning),
                AddForceCommit = no
            ;
                Reason = Reason0,
                AddForceCommit = yes
            )
        ),
        ScopedGoalPath = goal_path_add_at_end(Path, step_scope(MaybeCut)),
        deep_prof_transform_goal(ScopedGoalPath, SubGoal0, SubGoal,
            AddedImpurity, !DeepInfo),
        add_impurity_if_needed(AddedImpurity, GoalInfo2, GoalInfo),
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
        unexpected(this_file,
            "deep_prof_transform_goal: shorthand should have gone by now")
    ).

:- pred deep_prof_transform_conj(int::in,
    conj_type::in, goal_path::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::out,
    deep_info::in, deep_info::out) is det.

deep_prof_transform_conj(_, _, _, [], [], no, !DeepInfo).
deep_prof_transform_conj(N, ConjType, Path, [Goal0 | Goals0], Goals,
        AddedImpurity, !DeepInfo) :-
    N1 = N + 1,
    deep_prof_transform_goal(goal_path_add_at_end(Path, step_conj(N1)),
        Goal0, Goal, AddedImpurityFirst, !DeepInfo),
    deep_prof_transform_conj(N1, ConjType, Path, Goals0,
        TailGoals, AddedImpurityLater, !DeepInfo),
    Goal = hlds_goal(GoalExpr, _),
    (
        GoalExpr = conj(plain_conj, Conjuncts),
        ConjType = plain_conj
    ->
        Goals = Conjuncts ++ TailGoals
    ;
        Goals = [Goal | TailGoals]
    ),
    bool.or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity).

:- pred deep_prof_transform_disj(int::in, goal_path::in,
    list(hlds_goal)::in, list(hlds_goal)::out, bool::out,
    deep_info::in, deep_info::out) is det.

deep_prof_transform_disj(_, _, [], [], no, !DeepInfo).
deep_prof_transform_disj(N, Path, [Goal0 | Goals0], [Goal | Goals],
        AddedImpurity, !DeepInfo) :-
    N1 = N + 1,
    deep_prof_transform_goal(goal_path_add_at_end(Path, step_disj(N1)),
        Goal0, Goal, AddedImpurityFirst, !DeepInfo),
    deep_prof_transform_disj(N1, Path, Goals0, Goals, AddedImpurityLater,
        !DeepInfo),
    bool.or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity).

:- pred deep_prof_transform_switch(maybe(int)::in, int::in, goal_path::in,
    list(case)::in, list(case)::out, bool::out,
    deep_info::in, deep_info::out) is det.

deep_prof_transform_switch(_, _, _, [], [], no, !DeepInfo).
deep_prof_transform_switch(MaybeNumCases, N, Path,
        [Case0 | Cases0], [Case | Cases], AddedImpurity, !DeepInfo) :-
    N1 = N + 1,
    Case0 = case(MainConsId, OtherConsIds, Goal0),
    deep_prof_transform_goal(
        goal_path_add_at_end(Path, step_switch(N1, MaybeNumCases)),
        Goal0, Goal, AddedImpurityFirst, !DeepInfo),
    Case = case(MainConsId, OtherConsIds, Goal),
    deep_prof_transform_switch(MaybeNumCases, N1, Path, Cases0, Cases,
        AddedImpurityLater, !DeepInfo),
    bool.or(AddedImpurityFirst, AddedImpurityLater, AddedImpurity).

:- pred deep_prof_wrap_call(goal_path::in, hlds_goal::in, hlds_goal::out,
    deep_info::in, deep_info::out) is det.

deep_prof_wrap_call(GoalPath, Goal0, Goal, !DeepInfo) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    ModuleInfo = !.DeepInfo ^ deep_module_info,
    GoalFeatures = goal_info_get_features(GoalInfo0),
    goal_info_remove_feature(feature_deep_tail_rec_call, GoalInfo0, GoalInfo1),
    make_impure(GoalInfo1, GoalInfo2),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo2,
        MdprofInstGoalInfo),

    % We need to make the call itself impure. If we didn't do so,
    % then simplify could eliminate the goal (e.g. if it was a duplicate
    % call). The result would be a prepare_for_{...}_call whose execution
    % is not followed by the execution of the call port code of the callee.
    % This would leave the MR_csd_callee_ptr field NULL, which violates
    % invariants of the deep profiling tree (which allows this field to be
    % NULL only temporarily, between the prepare_for_{...}_call and the
    % call port code).
    Goal1 = hlds_goal(GoalExpr0, GoalInfo2),

    SiteNumCounter0 = !.DeepInfo ^ deep_site_num_counter,
    counter.allocate(SiteNum, SiteNumCounter0, SiteNumCounter),
    VarInfo0 = !.DeepInfo ^ deep_varinfos,
    generate_var("SiteNum", int_type, SiteNumVar, VarInfo0, VarInfo1),
    generate_unify(int_const(SiteNum), SiteNumVar, SiteNumVarGoal),
    !:DeepInfo = !.DeepInfo ^ deep_varinfos := VarInfo1,
    !:DeepInfo = !.DeepInfo ^ deep_site_num_counter := SiteNumCounter,

    Context = goal_info_get_context(GoalInfo0),
    FileName0 = term.context_file(Context),
    LineNumber = term.context_line(Context),
    compress_filename(!.DeepInfo, FileName0, FileName),
    CallKind = classify_call(ModuleInfo, GoalExpr0),
    (
        CallKind = call_class_normal(PredProcId),
        ( set.member(feature_deep_tail_rec_call, GoalFeatures) ->
            generate_deep_det_call(ModuleInfo, "prepare_for_tail_call", 1,
                [SiteNumVar], [], PrepareGoal)
        ;
            generate_deep_det_call(ModuleInfo, "prepare_for_normal_call", 1,
                [SiteNumVar], [], PrepareGoal)
        ),
        PredProcId = proc(PredId, ProcId),
        TypeSubst = compute_type_subst(GoalExpr0, !.DeepInfo),
        MaybeRecInfo = !.DeepInfo ^ deep_maybe_rec_info,
        (
            MaybeRecInfo = yes(RecInfo1),
            RecInfo1 ^ role = inner_proc(OuterPredProcId),
            PredProcId = !.DeepInfo ^ deep_pred_proc_id
        ->
            OuterPredProcId = proc(OuterPredId, OuterProcId),
            RttiProcLabel = make_rtti_proc_label(ModuleInfo,
                OuterPredId, OuterProcId)
        ;
            MaybeRecInfo = yes(RecInfo2),
            RecInfo2 ^ role = outer_proc(InnerPredProcId),
            PredProcId = InnerPredProcId
        ->
            OuterPredProcId = !.DeepInfo ^ deep_pred_proc_id,
            OuterPredProcId = proc(OuterPredId, OuterProcId),
            RttiProcLabel = make_rtti_proc_label(ModuleInfo,
                OuterPredId, OuterProcId)
        ;
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
            VarInfo2 = !.DeepInfo ^ deep_varinfos,
            generate_var("MethodNum", int_type, MethodNumVar, VarInfo2,
                VarInfo3),
            !:DeepInfo = !.DeepInfo ^ deep_varinfos := VarInfo3,
            generate_unify(int_const(MethodNum), MethodNumVar,
                MethodNumVarGoal),
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
            unexpected(this_file, "deep_profiling.wrap_call: event_call")
        ;
            Generic = cast(_),
            unexpected(this_file, "deep_profiling.wrap_call: cast")
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

    !:DeepInfo = !.DeepInfo ^ deep_call_sites :=
        (!.DeepInfo ^ deep_call_sites ++ [CallSite]),
    (
        set.member(feature_deep_tail_rec_call, GoalFeatures),
        !.DeepInfo ^ deep_maybe_rec_info = yes(RecInfo),
        RecInfo ^ role = outer_proc(_)
    ->
        VisSCC = RecInfo ^ visible_scc,
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
            unexpected(this_file,
                "wrap_call: multi-procedure SCCs not yet implemented")
        ),

        CodeModel = goal_info_get_code_model(GoalInfo0),
        (
            CodeModel = model_det,
            list.condense([
                CallGoals,
                [SiteNumVarGoal, PrepareGoal, Goal2],
                ExitGoals
            ], Goals),
            GoalExpr = conj(plain_conj, Goals)
        ;
            ( CodeModel = model_semi
            ; CodeModel = model_non
            ),
            ExtraVars = list_to_set([MiddleCSD | SaveRestoreVars]),
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
                    conj(plain_conj, [
                        SiteNumVarGoal,
                        PrepareGoal,
                        Goal2 |
                        ExitGoals
                    ]),
                    WrappedGoalGoalInfo),
                hlds_goal(
                    conj(plain_conj, FailGoalsAndFail),
                    ReturnFailsGoalInfo)
            ]),
            DisjGoal = hlds_goal(DisjGoalExpr, WrappedGoalGoalInfo),
            Goals = CallGoals ++ [DisjGoal],
            GoalExpr = conj(plain_conj, Goals)
        )
    ;
        GoalExpr = conj(plain_conj, [SiteNumVarGoal, PrepareGoal, Goal2])
    ),
    Goal = hlds_goal(GoalExpr, MdprofInstGoalInfo).

:- pred deep_prof_transform_higher_order_call(globals::in, code_model::in,
    hlds_goal::in, hlds_goal::out, deep_info::in, deep_info::out) is det.

deep_prof_transform_higher_order_call(Globals, CodeModel, Goal0, Goal,
        !DeepInfo) :-
    some [!VarInfo] (
        !:VarInfo = !.DeepInfo ^ deep_varinfos,

        generate_var("SavedPtr", c_pointer_type, SavedPtrVar, !VarInfo),

        globals.lookup_bool_option(Globals, use_activation_counts,
            UseActivationCounts),
        (
            UseActivationCounts = yes,

            generate_var("SavedCounter", int_type, SavedCountVar, !VarInfo),
            ExtraNonLocals = set.list_to_set([SavedCountVar, SavedPtrVar]),

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

            ExtraNonLocals = set.list_to_set([SavedPtrVar]),

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

        !:DeepInfo = !.DeepInfo ^ deep_varinfos := !.VarInfo
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

    RezeroFailGoalInfo0 = impure_unreachable_init_goal_info(set.init,
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

:- pred deep_prof_wrap_foreign_code(goal_path::in,
    hlds_goal::in, hlds_goal::out, deep_info::in, deep_info::out) is det.

deep_prof_wrap_foreign_code(GoalPath, Goal0, Goal, !DeepInfo) :-
    Goal0 = hlds_goal(_, GoalInfo0),
    ModuleInfo = !.DeepInfo ^ deep_module_info,

    SiteNumCounter0 = !.DeepInfo ^ deep_site_num_counter,
    counter.allocate(SiteNum, SiteNumCounter0, SiteNumCounter),
    generate_var("SiteNum", int_type, SiteNumVar, !.DeepInfo ^
        deep_varinfos, VarInfo),
    generate_unify(int_const(SiteNum), SiteNumVar, SiteNumVarGoal),

    generate_deep_det_call(ModuleInfo, "prepare_for_callback", 1,
        [SiteNumVar], [], PrepareGoal),

    Context = goal_info_get_context(GoalInfo0),
    LineNumber = term.context_line(Context),
    FileName0 = term.context_file(Context),
    compress_filename(!.DeepInfo, FileName0, FileName),
    CallSite = callback(FileName, LineNumber, GoalPath),

    make_impure(GoalInfo0, GoalInfo1),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo1, GoalInfo),
    Goal = hlds_goal(conj(plain_conj, [SiteNumVarGoal, PrepareGoal, Goal0]),
        GoalInfo),
    !:DeepInfo = !.DeepInfo ^ deep_site_num_counter := SiteNumCounter,
    !:DeepInfo = !.DeepInfo ^ deep_varinfos := VarInfo,
    !:DeepInfo = !.DeepInfo ^ deep_call_sites :=
        !.DeepInfo ^ deep_call_sites ++ [CallSite].

:- pred compress_filename(deep_info::in, string::in, string::out) is det.

compress_filename(Deep, FileName0, FileName) :-
    ( FileName0 = Deep ^ deep_proc_filename ->
        FileName = ""
    ;
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
        (
            lookup_builtin_pred_proc_id(ModuleInfo,
                mercury_public_builtin_module, "unify",
                pf_predicate, 2, mode_no(0), PredId, _),
            Args = [TypeInfoVar | _]
        ->
            Class = call_class_special(proc(PredId, ProcId), TypeInfoVar)
        ;
            lookup_builtin_pred_proc_id(ModuleInfo,
                mercury_public_builtin_module, "compare",
                pf_predicate, 3, mode_no(0), PredId, _),
            Args = [TypeInfoVar | _]
        ->
            Class = call_class_special(proc(PredId, ProcId), TypeInfoVar)
        ;
            lookup_builtin_pred_proc_id(ModuleInfo,
                mercury_public_builtin_module,
                "compare_representation", pf_predicate, 3,
                mode_no(0), PredId, _),
            Args = [TypeInfoVar | _]
        ->
            Class = call_class_special(proc(PredId, ProcId), TypeInfoVar)
        ;
            Class = call_class_normal(proc(PredId, ProcId))
        )
    ;
        Expr = generic_call(Generic, _, _, _),
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
        unexpected(this_file, "unexpected goal type in classify_call/2")
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
    VarInfo0 = !.DeepInfo ^ deep_varinfos,
    VarName = string.format("Depth%d", [i(CSN)]),
    generate_var(VarName, int_type, DepthVar, VarInfo0, VarInfo),
    !:DeepInfo = !.DeepInfo ^ deep_varinfos := VarInfo.

:- pred generate_csn_vector(int::in, list(int)::in, list(prog_var)::out,
    list(hlds_goal)::out, prog_var::out,
    deep_info::in, deep_info::out) is det.

generate_csn_vector(Length, CSNs, CSNVars, UnifyGoals, CellVar, !DeepInfo) :-
    ( CSNs = [CSN] ->
        generate_single_csn_unify(CSN, CSNVar - UnifyGoal, !DeepInfo),
        CSNVars = [CSNVar],
        UnifyGoals = [UnifyGoal],
        CellVar = CSNVar
    ;
        expect(Length =< max_save_restore_vector_size, this_file,
            "generate_csn_vector_unifies: too long"),
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
    VarInfo0 = !.DeepInfo ^ deep_varinfos,
    ProfilingBuiltin = mercury_profiling_builtin_module,
    CellTypeName = string.format("call_site_nums_%d", [i(Length)]),
    CellTypeId = type_ctor(qualified(ProfilingBuiltin, CellTypeName), Length),
    construct_type(CellTypeId, [], CellType),
    generate_var("CSNCell", CellType, CellVar, VarInfo0, VarInfo),
    !:DeepInfo = !.DeepInfo ^ deep_varinfos := VarInfo,
    ConsId = cons(qualified(ProfilingBuiltin, CellTypeName), Length),
    generate_cell_unify(Length, ConsId, CSNVars, CellVar, CellGoal).

:- pred generate_single_csn_unify(int::in,
    pair(prog_var, hlds_goal)::out, deep_info::in, deep_info::out) is det.

generate_single_csn_unify(CSN, CSNVar - UnifyGoal, !DeepInfo) :-
    VarInfo0 = !.DeepInfo ^ deep_varinfos,
    VarName = string.format("CSN%d", [i(CSN)]),
    generate_var(VarName, int_type, CSNVar, VarInfo0, VarInfo),
    !:DeepInfo = !.DeepInfo ^ deep_varinfos := VarInfo,
    generate_unify(int_const(CSN), CSNVar, UnifyGoal).

:- pred generate_deep_det_call(module_info::in, string::in, int::in,
    list(prog_var)::in, list(prog_var)::in, hlds_goal::out) is det.

generate_deep_det_call(ModuleInfo, Name, Arity, ArgVars, OutputVars, Goal) :-
    generate_deep_call(ModuleInfo, Name, Arity, ArgVars, yes(OutputVars),
        detism_det, Goal).

:- pred generate_deep_call(module_info::in, string::in, int::in,
    list(prog_var)::in, maybe(list(prog_var))::in, determinism::in,
    hlds_goal::out) is det.

generate_deep_call(ModuleInfo, Name, Arity, ArgVars, MaybeOutputVars, Detism,
        Goal) :-
    get_deep_profile_builtin_ppid(ModuleInfo, Name, Arity, PredId, ProcId),
    NonLocals = list_to_set(ArgVars),
    Ground = ground(shared, none),
    (
        MaybeOutputVars = yes(OutputVars),
        map((pred(V::in, P::out) is det :-
            P = V - Ground
        ), OutputVars, OutputInsts),
        instmap_delta_from_assoc_list(OutputInsts, InstMapDelta)
    ;
        MaybeOutputVars = no,
        instmap_delta_init_unreachable(InstMapDelta)
    ),
    SymName = unqualified(Name),
    GoalExpr = plain_call(PredId, ProcId, ArgVars, not_builtin, no, SymName),
    GoalInfo1 = impure_init_goal_info(NonLocals, InstMapDelta, Detism),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo1, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred generate_unify(cons_id::in, prog_var::in, hlds_goal::out) is det.

generate_unify(ConsId, Var, Goal) :-
    Ground = ground(shared, none),
    NonLocals = set.make_singleton_set(Var),
    instmap_delta_from_assoc_list([Var - ground(shared, none)], InstMapDelta),
    Determinism = detism_det,
    goal_info_init(NonLocals, InstMapDelta, Determinism, purity_pure,
        GoalInfo1),
    goal_info_set_mdprof_inst(goal_is_mdprof_inst, GoalInfo1, GoalInfo),
    GoalExpr = unify(Var, rhs_functor(ConsId, no, []),
        (free -> Ground) - (Ground -> Ground),
        construct(Var, ConsId, [], [], construct_statically([]),
            cell_is_shared, no_construct_sub_info),
        unify_context(umc_explicit, [])),
    Goal = hlds_goal(GoalExpr, GoalInfo).

:- pred generate_cell_unify(int::in, cons_id::in, list(prog_var)::in,
    prog_var::in, hlds_goal::out) is det.

generate_cell_unify(Length, ConsId, Args, Var, Goal) :-
    Ground = ground(shared, none),
    NonLocals = set.list_to_set([Var | Args]),
    instmap_delta_from_assoc_list([Var - Ground], InstMapDelta),
    Determinism = detism_det,
    goal_info_init(NonLocals, InstMapDelta, Determinism, purity_pure,
        GoalInfo),
    ArgMode = ((free - Ground) -> (Ground - Ground)),
    list.duplicate(Length, ArgMode, ArgModes),
    GoalExpr = unify(Var, rhs_functor(ConsId, no, Args),
        (free -> Ground) - (Ground -> Ground),
        construct(Var, ConsId, Args, ArgModes,
            construct_statically([]), cell_is_shared, no_construct_sub_info),
        unify_context(umc_explicit, [])),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Create a variable with the given name and type, adding it to the
    % var_info structure.
    %
:- pred generate_var(string::in, mer_type::in, prog_var::out, var_info::in,
    var_info::out) is det.

generate_var(Name, Type, Var, !VarInfo) :-
    some [!VarSet, !VarTypes]
    (
        !.VarInfo = var_info(!:VarSet, !:VarTypes),
        generate_var_2(Name, Type, Var, !VarSet, !VarTypes),
        !:VarInfo = var_info(!.VarSet, !.VarTypes)
    ).

    % Create a variable with the given name and type, adding it to the
    % separate prog_varset and vartypes structures.
    %
:- pred generate_var_2(string::in, mer_type::in, prog_var::out,
    prog_varset::in, prog_varset::out, vartypes::in, vartypes::out) is det.

generate_var_2(Name, Type, Var, !VarSet, !VarTypes) :-
    svvarset.new_named_var(Name, Var, !VarSet),
    svmap.set(Var, Type, !VarTypes).

:- pred maybe_generate_activation_ptr(bool::in, prog_var::in, prog_var::in,
        maybe(prog_var)::out, hlds_deep_excp_vars::out, var_info::in,
        var_info::out) is det.

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
        var_info::in, var_info::out) is det.

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
    ExcpVars = hlds_deep_excp_vars(TopCSD, MiddleCSD,
        MaybeOldActivationPtr),
    generate_var("NewOutermost", c_pointer_type, NewOutermostProcDyn,
        !VarInfo).

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- pred get_deep_profile_builtin_ppid(module_info::in, string::in, int::in,
    pred_id::out, proc_id::out) is det.

get_deep_profile_builtin_ppid(ModuleInfo, Name, Arity, PredId, ProcId) :-
    ModuleName = mercury_profiling_builtin_module,
    module_info_get_predicate_table(ModuleInfo, PredTable),
    (
        predicate_table_search_pred_m_n_a(PredTable,
            is_fully_qualified, ModuleName, Name, Arity, PredIds)
    ->
        (
            PredIds = [],
            unexpected(this_file, "get_deep_profile_builtin_ppid: no pred_id")
        ;
            PredIds = [PredId],
            predicate_table_get_preds(PredTable, Preds),
            lookup(Preds, PredId, PredInfo),
            ProcIds = pred_info_procids(PredInfo),
            (
                ProcIds = [],
                unexpected(this_file,
                    "get_deep_profile_builtin_ppid: no proc_id")
            ;
                ProcIds = [ProcId]
            ;
                ProcIds = [_, _ | _],
                unexpected(this_file,
                    "get_deep_profile_builtin_ppid: proc_id not unique")
            )
        ;
            PredIds = [_, _ | _],
            unexpected(this_file,
                "get_deep_profile_builtin_ppid: pred_id not unique")
        )
    ;
        format("couldn't find pred_id for `%s'/%d", [s(Name), i(Arity)], Msg),
        unexpected(this_file, Msg)
    ).

%-----------------------------------------------------------------------------%

:- func impure_init_goal_info(set(prog_var), instmap_delta, determinism)
    = hlds_goal_info.

impure_init_goal_info(NonLocals, InstMapDelta, Determinism) = GoalInfo :-
    goal_info_init(NonLocals, InstMapDelta, Determinism, purity_impure,
        GoalInfo0),
    goal_info_add_feature(feature_not_impure_for_determinism,
        GoalInfo0, GoalInfo).

:- func impure_reachable_init_goal_info(set(prog_var), determinism)
    = hlds_goal_info.

impure_reachable_init_goal_info(NonLocals, Determinism) = GoalInfo :-
    instmap_delta_init_reachable(InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, Determinism, purity_impure,
        GoalInfo).

:- func impure_unreachable_init_goal_info(set(prog_var), determinism)
    = hlds_goal_info.

impure_unreachable_init_goal_info(NonLocals, Determinism) = GoalInfo :-
    instmap_delta_init_unreachable(InstMapDelta),
    goal_info_init(NonLocals, InstMapDelta, Determinism, purity_impure,
        GoalInfo0),
    goal_info_add_feature(feature_not_impure_for_determinism,
        GoalInfo0, GoalInfo).

:- func goal_info_add_nonlocals_make_impure(hlds_goal_info, set(prog_var))
    = hlds_goal_info.

goal_info_add_nonlocals_make_impure(!.GoalInfo, NewNonLocals) = !:GoalInfo :-
    NonLocals0 = goal_info_get_nonlocals(!.GoalInfo),
    NonLocals = set.union(NonLocals0, NewNonLocals),
    goal_info_set_nonlocals(NonLocals, !GoalInfo),
    make_impure(!GoalInfo).

:- func fail_goal_info = hlds_goal_info.

fail_goal_info = GoalInfo :-
    instmap_delta_init_unreachable(InstMapDelta),
    goal_info_init(set.init, InstMapDelta, detism_failure, purity_pure,
        GoalInfo).

:- pred make_impure(hlds_goal_info::in, hlds_goal_info::out) is det.

make_impure(!GoalInfo) :-
    ( goal_info_get_purity(!.GoalInfo) = purity_impure ->
        % We don't add not_impure_for_determinism, since we want to
        % keep the existing determinism.
        true
    ;
        goal_info_set_purity(purity_impure, !GoalInfo),
        goal_info_add_feature(feature_not_impure_for_determinism, !GoalInfo)
    ).

:- pred add_impurity_if_needed(bool::in, hlds_goal_info::in,
    hlds_goal_info::out) is det.

add_impurity_if_needed(AddedImpurity, !GoalInfo) :-
    (
        AddedImpurity = no
    ;
        AddedImpurity = yes,
        make_impure(!GoalInfo)
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
% Coverage Profiling.
%-----------------------------------------------------------------------------%

    % Information used by the coverage profiling transformation.
    %
:- type proc_coverage_info
    --->    proc_coverage_info(
                % A map from coverage point indexes to coverage point
                % information. Updated as coverage points are inserted.
                ci_coverage_points          :: map(int, coverage_point_info),

                % Used to track the next coverage point index to be allocated.
                ci_cp_index_counter         :: counter,

                % Information about variables, this is updated as variables are
                % introduced.
                ci_var_info                 :: var_info,

                % The following fields are static; they are not modified
                % after initialization.
                ci_module_info              :: module_info,
                ci_pred_proc_id             :: pred_proc_id,
                ci_maybe_rec_info           :: maybe(deep_recursion_info),
                ci_coverage_profiling_opts  :: coverage_profiling_options
            ).

    % Store what coverage profiling options have been selected.
    %
:- type coverage_profiling_options
    --->    coverage_profiling_options(
                % These fields correspond to coverage profiling options that
                % may be specified on the command line.
                cpo_use_calls               :: bool,
                cpo_coverage_after_goal     :: bool,
                cpo_branch_ite              :: bool,
                cpo_branch_switch           :: bool,
                cpo_branch_disj             :: bool,
                cpo_use_portcounts          :: bool,
                cpo_use_trivial             :: bool,

                % cpo_run_first_pass is true if some information needs to be
                % collected in an initial pass.
                cpo_run_first_pass          :: bool
            ).

    % Initialize the coverage_profiling_options structure.
    %
:- pred coverage_profiling_options(module_info::in,
    coverage_profiling_options::out) is det.

coverage_profiling_options(ModuleInfo, CoveragePointOptions) :-
    module_info_get_globals(ModuleInfo, Globals),

    % Options controlling what instrumentation code we generate.
    globals.lookup_bool_option(Globals, coverage_profiling_via_calls,
        UseCalls),

    % Coverage point types.
    globals.lookup_bool_option(Globals, profile_deep_coverage_after_goal,
        CoverageAfterGoal),
    globals.lookup_bool_option(Globals, profile_deep_coverage_branch_ite,
        BranchIf),
    globals.lookup_bool_option(Globals, profile_deep_coverage_branch_switch,
        BranchSwitch),
    globals.lookup_bool_option(Globals, profile_deep_coverage_branch_disj,
        BranchDisj),

    % Options for tuning the coverage profiling pass.
    globals.lookup_bool_option(Globals, profile_deep_coverage_use_portcounts,
        UsePortCounts),
    globals.lookup_bool_option(Globals, profile_deep_coverage_use_trivial,
        UseTrivial),
    bool.or(UsePortCounts, UseTrivial, RunFirstPass),

    CoveragePointOptions = coverage_profiling_options(UseCalls,
        CoverageAfterGoal, BranchIf, BranchSwitch, BranchDisj,
        UsePortCounts, UseTrivial, RunFirstPass).

    % Perform the coverage profiling transformation on the given goal,
    % and return a list of the coverage points created.
    %
:- pred coverage_prof_transform_goal(module_info::in, pred_proc_id::in,
    maybe(deep_recursion_info)::in, hlds_goal::in, hlds_goal::out,
    var_info::in, var_info::out, list(coverage_point_info)::out) is det.

coverage_prof_transform_goal(ModuleInfo, PredProcId, MaybeRecInfo, !Goal,
        !VarInfo, CoveragePoints) :-
    coverage_profiling_options(ModuleInfo, CoverageProfilingOptions),
    CoverageInfo0 = init_proc_coverage_info(!.VarInfo, ModuleInfo,
        PredProcId, MaybeRecInfo, CoverageProfilingOptions),
    RunFirstPass = CoverageProfilingOptions ^ cpo_run_first_pass,
    (
        RunFirstPass = yes,
        coverage_prof_first_pass(CoverageProfilingOptions, !Goal,
            port_counts_give_coverage_after, _)
    ;
        RunFirstPass = no
    ),
    coverage_prof_second_pass_goal(!Goal, coverage_before_known, _,
        CoverageInfo0, CoverageInfo, _),
    CoverageInfo ^ ci_coverage_points = CoveragePointsMap,
    CoverageInfo ^ ci_var_info = !:VarInfo,
    coverage_points_map_list(CoveragePointsMap, CoveragePoints).

    % Transform a goal for coverage profiling. This is the second pass of
    % the coverage profiling transformation, and it consists of several steps.
    %
    % Step 1: Apply transformation recursively.
    %
    % Step 2: Decide whether to insert a coverage point after this goal
    % to measure how many times it succeeds.
    %
    % Step 3: Insert the coverage point if we decided to do so.
    %
:- pred coverage_prof_second_pass_goal(hlds_goal::in, hlds_goal::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_goal(Goal0, Goal,
        CoverageBeforeKnown, NextCoverageBeforeKnown, !Info, AddedImpurity) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    Detism = GoalInfo0 ^ goal_info_get_determinism,
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    GoalPath = goal_info_get_goal_path(GoalInfo0),

    (
        IsMDProfInst = goal_is_not_mdprof_inst,
        CoverageBeforeKnown = coverage_before_unknown
    ->
        UnknownMsg = string.format(
            "coverage_prof_second_pass_goal: Coverage information is unknown\n"
            ++ "\tGoalPath: %s",
            [s(goal_path_to_string(GoalPath))]),
        unexpected(this_file, UnknownMsg)
    ;
        true
    ),

    % Currently the first pass is unsupported, we don't make use of the
    % information it provides.
    DPInfo = goal_info_get_dp_info(GoalInfo0),
    DPInfo = dp_goal_info(IsMDProfInst, _MaybeDPCoverageInfo),

    % Step 1.
    %
    % Apply transformation recursively.
    (
        (
            GoalExpr0 = unify(_, _, _, _, _)
        ;
            GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _)
            % Even though the deep profiler creates a call site when these may
            % call Mercury, the coverage propagation code cannot make use of
            % the call site's port counts, since they measure how often
            % Mercury is re-entered, not how often this call is made.
        ),
        coverage_known_after_goal_with_detism(Detism,
            CoverageBeforeKnown, NextCoverageBeforeKnown0),
        AddedImpurityInner = no,
        GoalExpr1 = GoalExpr0
    ;
        (
            GoalExpr0 = plain_call(_, _, _, BuiltinState, _, _),
            (
                ( BuiltinState = out_of_line_builtin
                ; BuiltinState = not_builtin
                ),
                GathersCoverageAfter = yes
            ;
                BuiltinState = inline_builtin,
                GathersCoverageAfter = no
            )
        ;
            GoalExpr0 = generic_call(GenericCall, _, _, _),
            (
                ( GenericCall = higher_order(_, _, _, _)
                ; GenericCall = class_method(_, _, _, _)
                ),
                GathersCoverageAfter = yes
            ;
                ( GenericCall = cast(_)
                ; GenericCall = event_call(_)
                ),
                GathersCoverageAfter = no
            )
        ),
        (
            GathersCoverageAfter = yes,
            NextCoverageBeforeKnown0 = coverage_before_known
        ;
            GathersCoverageAfter = no,
            coverage_known_after_goal_with_detism(Detism,
                CoverageBeforeKnown, NextCoverageBeforeKnown0)
        ),
        AddedImpurityInner = no,
        GoalExpr1 = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        coverage_prof_second_pass_conj(ConjType, Goals0, Goals,
            CoverageBeforeKnown, NextCoverageBeforeKnown0, !Info,
            AddedImpurityInner),
        GoalExpr1 = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        coverage_prof_second_pass_disj(DPInfo,
            CoverageBeforeKnown, NextCoverageBeforeKnown0,
            Goals0, Goals, !Info, AddedImpurityInner),
        GoalExpr1 = disj(Goals)
    ;
        GoalExpr0 = switch(Var, SwitchCanFail, Cases0),
        coverage_prof_second_pass_switchcase(DPInfo, SwitchCanFail,
            Cases0, Cases, CoverageBeforeKnown, NextCoverageBeforeKnown0,
            !Info, AddedImpurityInner),
        GoalExpr1 = switch(Var, SwitchCanFail, Cases)
    ;
        GoalExpr0 = negation(NegGoal0),
        coverage_prof_second_pass_goal(NegGoal0, NegGoal,
            CoverageBeforeKnown, _, !Info, AddedImpurityInner),
        % The coverage after a negated goal cannot be inferred from its inner
        % goals.
        NextCoverageBeforeKnown0 = coverage_before_unknown,
        GoalExpr1 = negation(NegGoal)
    ;
        GoalExpr0 = scope(Reason, ScopeGoal0),
        coverage_prof_second_pass_goal(ScopeGoal0, ScopeGoal,
            CoverageBeforeKnown, CoverageAfterScopedGoalKnown, !Info,
            AddedImpurityInner),
        % A scope may cut away solutions, if it does we don't know the number
        % of solutions of the scoped goal.
        ScopedGoalDetism =
            ScopeGoal0 ^ hlds_goal_info ^ goal_info_get_determinism,
        ( ScopedGoalDetism = Detism ->
            NextCoverageBeforeKnown0 = CoverageAfterScopedGoalKnown
        ;
            NextCoverageBeforeKnown0 = coverage_before_unknown
        ),
        GoalExpr1 = scope(Reason, ScopeGoal)
    ;
        GoalExpr0 = if_then_else(ITEExistVars, Cond, Then, Else),
        coverage_prof_second_pass_ite(DPInfo, ITEExistVars, Cond, Then, Else,
            GoalExpr1, CoverageBeforeKnown, NextCoverageBeforeKnown0, !Info,
            AddedImpurityInner)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "coverage_prof_second_pass_goal: shorthand")
    ),

    % Step 2.
    %
    % Decide whether we need to insert a coverage point after this goal
    % to measure how many times execution reaches there.
    (
        (
            % Never insert coverage points on goals that are part of the deep
            % profiling instrumentation.
            IsMDProfInst = goal_is_mdprof_inst
        ;
            % We already have execution counts for the program point after this
            % goal; adding a counter would be redundant.
            NextCoverageBeforeKnown0 = coverage_before_known
        )
    ->
        MaybeAddCP = no,
        NextCoverageBeforeKnown = NextCoverageBeforeKnown0
    ;
        CoverageAfterGoals = CPOptions ^ cpo_coverage_after_goal,
        (
            CoverageAfterGoals  = yes,
            MaybeAddCP = yes(cp_type_coverage_after),
            NextCoverageBeforeKnown = coverage_before_known
        ;
            CoverageAfterGoals = no,
            MaybeAddCP = no,
            NextCoverageBeforeKnown = NextCoverageBeforeKnown0
        )
    ),

    % Step 3.
    %
    % Insert the coverage point if we decided to.
    add_impurity_if_needed(AddedImpurityInner, GoalInfo0, GoalInfo1),
    Goal1 = hlds_goal(GoalExpr1, GoalInfo1),
    (
        MaybeAddCP = yes(CPType),
        CPInfo = coverage_point_info(GoalPath, CPType),

        make_coverage_point(CPOptions, CPInfo, CPGoals, !Info),
        create_conj_from_list([Goal1 | CPGoals], plain_conj, Goal),

        AddedImpurity = yes
    ;
        MaybeAddCP = no,
        Goal = Goal1,
        AddedImpurity = AddedImpurityInner
    ).

:- pred coverage_known_after_goal_with_detism(determinism::in,
    coverage_before_known::in, coverage_before_known::out) is det.

coverage_known_after_goal_with_detism(Detism, !CoverageKnown) :-
    (
        ( Detism = detism_semi
        ; Detism = detism_multi
        ; Detism = detism_non
        ; Detism = detism_cc_non
        ; Detism = detism_erroneous
        ; Detism = detism_failure
        ),
        !:CoverageKnown = coverage_before_unknown
    ;
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        )
    ).

    % Perform the coverage profiling transformation for conjuncts.
    %
    % The goal list represents the tail of a conjunction.  Pos is the position
    % of this list within the entire conjunction, if this is the entire
    % conjunction then Pos should be 1.
    %
:- pred coverage_prof_second_pass_conj(conj_type::in,
    list(hlds_goal)::in, list(hlds_goal)::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_conj(_, [], [], !CoverageBeforeKnown, !Info, no).
coverage_prof_second_pass_conj(ConjType, [HeadGoal0 | TailGoals0], Goals,
        CoverageBeforeKnown, NextCoverageBeforeKnown, !Info, AddedImpurity) :-
    coverage_prof_second_pass_goal(HeadGoal0, HeadGoal,
        CoverageBeforeKnown, CoverageBeforeTailKnown, !Info,
        AddedImpurityHead),
    coverage_prof_second_pass_conj(ConjType, TailGoals0, TailGoals,
        CoverageBeforeTailKnown, NextCoverageBeforeKnown, !Info,
        AddedImpurityTail),
    % Flatten the conjunction. We need to do this if we replaced the head
    % with a goal that is itself a conjunction.
    (
        HeadGoal = hlds_goal(conj(plain_conj, HeadConjGoals), _),
        ConjType = plain_conj
    ->
        Goals = HeadConjGoals ++ TailGoals
    ;
        Goals = [HeadGoal | TailGoals]
    ),
    bool.or(AddedImpurityHead, AddedImpurityTail, AddedImpurity).

    % Perform the coverage profiling transformation over goals within a
    % disjunction.
    %
:- pred coverage_prof_second_pass_disj(dp_goal_info::in,
    coverage_before_known::in, coverage_before_known::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_disj(DPInfo, CoverageBeforeKnown,
        NextCoverageBeforeKnown, Disjuncts0, Disjuncts, !Info,
        AddedImpurity) :-
    % If the disjunction was introduced by the deep profiling pass, it has two
    % disjuncts and its second disjunct has 'failure' determinism, then
    % perform the coverage profiling pass on the first disjunct as if this
    % is the only goal.
    (
        DPInfo = dp_goal_info(goal_is_mdprof_inst, _),
        Disjuncts0 = [FirstDisjunct0, SecondDisjunct],
        goal_info_get_determinism(SecondDisjunct ^ hlds_goal_info) =
            detism_failure
        % XXX: zs: Would this be a better test here?
        % goal_has_feature(SecondDisjunct, feature_preserve_backtrack_into)
        % pbone: I don't think so, the deep profiler doesn't seem to add this
        % feature to disjuncts that end in failure, it is probably a good idea
        % to add this annotation to prevent later compiler passes from breaking
        % the deep profiler. 
    ->
        coverage_prof_second_pass_goal(FirstDisjunct0, FirstDisjunct,
            CoverageBeforeKnown, NextCoverageBeforeKnown, !Info,
            AddedImpurity),
        Disjuncts = [FirstDisjunct, SecondDisjunct]
    ;
        coverage_prof_second_pass_disj_2(DPInfo, CoverageBeforeKnown,
            coverage_before_known, NextCoverageBeforeKnown,
            Disjuncts0, Disjuncts, !Info, AddedImpurity)
    ).

:- pred coverage_prof_second_pass_disj_2(dp_goal_info::in,
    coverage_before_known::in,
    coverage_before_known::in, coverage_before_known::out,
    list(hlds_goal)::in, list(hlds_goal)::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_disj_2(_, _, !CoverageKnownAfter, [], [], !Info, no).
coverage_prof_second_pass_disj_2(DPInfo, CoverageBeforeKnown0, !CoverageAfterKnown,
        [HeadDisjunct0 | TailDisjuncts0], [HeadDisjunct | TailDisjuncts],
        !Info, AddedImpurity) :-
    % Decide whether we want to insert a branch coverage point at the beginning
    % of the head disjunct.
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    CPOBranchDisj = CPOptions ^ cpo_branch_disj,
    DPInfo = dp_goal_info(IsMDProfInst, _),
    (
        CPOBranchDisj = yes,
        CoverageBeforeKnown0 = coverage_before_unknown,
        IsMDProfInst = goal_is_not_mdprof_inst
    ->
        InsertCP = yes,
        CoverageBeforeKnown = coverage_before_known
    ;
        InsertCP = no,
        CoverageBeforeKnown = CoverageBeforeKnown0
    ),

    coverage_prof_second_pass_goal(HeadDisjunct0, HeadDisjunct1,
        CoverageBeforeKnown, CoverageAfterDisjKnown, !Info,
        AddedImpurityHead),
    !:CoverageAfterKnown = 
        coverage_before_known_and(!.CoverageAfterKnown, CoverageAfterDisjKnown),
    coverage_prof_second_pass_disj_2(DPInfo, coverage_before_unknown,
        !CoverageAfterKnown, TailDisjuncts0, TailDisjuncts, !Info,
        AddedImpurityTail),

    % Insert the coverage point if we decided to above.
    (
        InsertCP = yes,
        DisjPath = goal_info_get_goal_path(HeadDisjunct0 ^ hlds_goal_info),
        HeadCoveragePoint = coverage_point_info(DisjPath, cp_type_branch_arm),
        insert_coverage_point_before(CPOptions, HeadCoveragePoint,
            HeadDisjunct1, HeadDisjunct, !Info),
        AddedImpurity = yes
    ;
        InsertCP = no,
        HeadDisjunct = HeadDisjunct1,
        AddedImpurity = bool.or(AddedImpurityHead, AddedImpurityTail)
    ).

    % coverage_prof_second_pass_switchcase(DPInfo, SwitchCanFial, !Cases,
    %   CoverageBeforeSwitch, !Info, AddedImpurity).
    %
    % Preform coverage profiling transformation on switch cases.
    %
:- pred coverage_prof_second_pass_switchcase(dp_goal_info::in, can_fail::in,
    list(case)::in, list(case)::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_switchcase(DPInfo, CanFail, Cases0, Cases,
        CoverageBeforeSwitchKnown, CoverageAfterSwitchKnown, !Info,
        AddedImpurity) :-
    % If the switch can fail then the coverage after it will be unknown.
    (
        CanFail = can_fail,
        CoverageAfterSwitchKnown0 = coverage_before_unknown
    ;
        CanFail = cannot_fail,
        CoverageAfterSwitchKnown0 = coverage_before_known
    ),
    CoverageBeforeEveryCaseKnown = coverage_before_known,
    coverage_prof_second_pass_switchcase_2(DPInfo, CanFail, Cases0, Cases,
        CoverageBeforeSwitchKnown, CoverageBeforeEveryCaseKnown,
        CoverageAfterSwitchKnown0, CoverageAfterSwitchKnown, !Info,
        AddedImpurity).

:- pred coverage_prof_second_pass_switchcase_2(dp_goal_info::in, can_fail::in,
    list(case)::in, list(case)::out, coverage_before_known::in,
    coverage_before_known::in,
    coverage_before_known::in,  coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_switchcase_2(_, _, [], [], _, _,
        !CoverageAfterSwitchKnown, !Info, no).
coverage_prof_second_pass_switchcase_2(DPInfo, SwitchCanFail,
        [Case0 | Cases0], [Case | Cases], CoverageBeforeSwitchKnown,
        CoverageBeforeEveryCaseKnown, !CoverageAfterSwitchKnown, !Info,
        AddedImpurity) :-
    Case0 = case(MainConsId, OtherConsIds, Goal0),

    % If the switch cannot fail and this is the last case, then the coverage
    % at the beginning of this case can be computed from the coverage before
    % the entire switch and coverage information from each of the other
    % branches of the switch.
    (
        Cases0 = [],
        (
            SwitchCanFail = cannot_fail,
            CoverageBeforeCaseKnown0 = coverage_before_known_and(
                CoverageBeforeSwitchKnown, CoverageBeforeEveryCaseKnown)
        ;
            SwitchCanFail = can_fail,
            CoverageBeforeCaseKnown0 = coverage_before_unknown
        )
    ;
        Cases0 = [_ | _],
        CoverageBeforeCaseKnown0 = coverage_before_unknown
    ),

    % Decide whether to insert a coverage point here.
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    CPOBranchSwitch = CPOptions ^ cpo_branch_switch,
    DPInfo = dp_goal_info(IsMDProfInst, _),
    (
        CPOBranchSwitch = yes,
        CoverageBeforeCaseKnown0 = coverage_before_unknown,
        IsMDProfInst = goal_is_not_mdprof_inst
    ->
        InsertCP = yes,
        CoverageBeforeCaseKnown = coverage_before_known
    ;
        InsertCP = no,
        CoverageBeforeCaseKnown = CoverageBeforeCaseKnown0
    ),

    coverage_prof_second_pass_goal(Goal0, Goal1,
        CoverageBeforeCaseKnown, CoverageAfterCaseKnown, !Info,
        AddedImpurityHead0),
    !:CoverageAfterSwitchKnown = coverage_before_known_and(
        CoverageAfterCaseKnown, !.CoverageAfterSwitchKnown),

    % Possibly insert coverage point at the start of the case.
    (
        InsertCP = yes,
        CasePath = goal_info_get_goal_path(Goal0 ^ hlds_goal_info),
        CoveragePoint = coverage_point_info(CasePath, cp_type_branch_arm),
        insert_coverage_point_before(CPOptions, CoveragePoint, Goal1, Goal,
            !Info),
        AddedImpurityHead = yes
    ;
        InsertCP = no,
        Goal = Goal1,
        AddedImpurityHead = AddedImpurityHead0
    ),

    % Handle recursive case and prepare output variables.
    % We cannot optimize away the coverage point at the start of the last case
    % if one of the previous cases does not have coverage information at its
    % start.
    NextCoverageBeforeEveryCaseKnown = coverage_before_known_and(
        CoverageBeforeEveryCaseKnown, CoverageBeforeCaseKnown),
    coverage_prof_second_pass_switchcase_2(DPInfo, SwitchCanFail,
        Cases0, Cases,
        CoverageBeforeSwitchKnown, NextCoverageBeforeEveryCaseKnown,
        !CoverageAfterSwitchKnown, !Info, AddedImpurityTail),
    Case = case(MainConsId, OtherConsIds, Goal),
    bool.or(AddedImpurityHead, AddedImpurityTail, AddedImpurity).

    % Determine if branch coverage points should be inserted in either or
    % both of the then and else branches, insert them and transform the
    % subgoals.
    %
    % This is performed by first transforming the condition, then making
    % decisions about coverage points and inserting them, then transforming
    % the then and else branches and constructing the new ITE goal_expr.
    %
:- pred coverage_prof_second_pass_ite(dp_goal_info::in, list(prog_var)::in,
    hlds_goal::in, hlds_goal::in, hlds_goal::in, hlds_goal_expr::out,
    coverage_before_known::in, coverage_before_known::out,
    proc_coverage_info::in, proc_coverage_info::out, bool::out) is det.

coverage_prof_second_pass_ite(DPInfo, ITEExistVars, Cond0, Then0, Else0,
        GoalExpr, CoverageBeforeITEKnown, NextCoverageBeforeKnown,
        !Info, AddedImpurity) :-
    % Transform the condition.
    coverage_prof_second_pass_goal(Cond0, Cond,
        CoverageBeforeITEKnown, CoverageKnownAfterCond, !Info,
        AddedImpurityCond),

    CoverageKnownBeforeThen0 = CoverageKnownAfterCond,
    CoverageKnownBeforeElse0 = coverage_before_unknown,

    % Gather information and decide what coverage points to insert.
    %
    % Notice that it doesn't matter if any of the goals are trivial or not,
    % we want to know what branch is taken regardless of how inexpensive it
    % may be as different variables may be used in different branches.
    %
    % Whatever we do we will ensure that the coverage will be known at the
    % beginning of each branch.
    CPOptions = !.Info ^ ci_coverage_profiling_opts,
    CPOBranchIf = CPOptions ^ cpo_branch_ite,
    DPInfo = dp_goal_info(IsMDProfInst, _),
    (
        CPOBranchIf = yes,
        IsMDProfInst = goal_is_not_mdprof_inst
    ->
        (
            CoverageKnownBeforeThen0 = coverage_before_unknown,
            ThenPath = goal_info_get_goal_path(Then0 ^ hlds_goal_info),
            InsertCPThen = yes(coverage_point_info(ThenPath,
                cp_type_branch_arm))
        ;
            CoverageKnownBeforeThen0 = coverage_before_known,
            InsertCPThen = no
        ),
        % Always insert a coverage point for the else branch.
        ElsePath = goal_info_get_goal_path(Else0 ^ hlds_goal_info),
        InsertCPElse = yes(coverage_point_info(ElsePath, cp_type_branch_arm)),
        CoverageKnownBeforeThen = coverage_before_known,
        CoverageKnownBeforeElse = coverage_before_known
    ;
        % Don't insert any coverage points.
        InsertCPThen = no,
        InsertCPElse = no,
        CoverageKnownBeforeThen = CoverageKnownBeforeThen0,
        CoverageKnownBeforeElse = CoverageKnownBeforeElse0
    ),

    % Transform Then and Else branches,
    coverage_prof_second_pass_goal(Then0, Then1,
        CoverageKnownBeforeThen, NextCoverageKnownThen, !Info,
        AddedImpurityThenGoal),
    coverage_prof_second_pass_goal(Else0, Else1,
        CoverageKnownBeforeElse, NextCoverageKnownElse, !Info,
        AddedImpurityElseGoal),

    % Insert any coverage points.
    (
        InsertCPThen = yes(CPInfoThen),
        insert_coverage_point_before(CPOptions, CPInfoThen, Then1, Then,
            !Info),
        AddedImpurityThen = yes
    ;
        InsertCPThen = no,
        Then = Then1,
        AddedImpurityThen = AddedImpurityThenGoal
    ),
    (
        InsertCPElse = yes(CPInfoElse),
        insert_coverage_point_before(CPOptions, CPInfoElse, Else1, Else,
            !Info),
        AddedImpurityElse = yes
    ;
        InsertCPElse = no,
        Else = Else1,
        AddedImpurityElse = AddedImpurityElseGoal
    ),

    % Build goal expression and tidy up.
    AddedImpurity = bool.or(AddedImpurityCond,
        bool.or(AddedImpurityThen, AddedImpurityElse)),
    GoalExpr = if_then_else(ITEExistVars, Cond, Then, Else),
    NextCoverageBeforeKnown = coverage_before_known_and(
        NextCoverageKnownThen, NextCoverageKnownElse).

%-----------------------------------------------------------------------------%

    % Create a coverage info struture, initializing some values to sensible
    % defaults.
    %
:- func init_proc_coverage_info(var_info, module_info, pred_proc_id,
    maybe(deep_recursion_info), coverage_profiling_options) =
    proc_coverage_info.

init_proc_coverage_info(VarInfo, ModuleInfo, PredProcId, MaybeRecInfo,
        CoverageProfilingOptions) = CoverageInfo :-
    CoverageInfo = proc_coverage_info(map.init, counter.init(0), VarInfo,
        ModuleInfo, PredProcId, MaybeRecInfo, CoverageProfilingOptions).

    % Used to describe if coverage information is known at a partiular point
    % within a procedure.
    %
:- type coverage_before_known
    --->    coverage_before_known
    ;       coverage_before_unknown.

    % The logical 'and' of coverage_before_known values.
:- func coverage_before_known_and(coverage_before_known, coverage_before_known)
    = coverage_before_known.

coverage_before_known_and(coverage_before_known, coverage_before_known) =
    coverage_before_known.
coverage_before_known_and(coverage_before_known, coverage_before_unknown) =
    coverage_before_unknown.
coverage_before_known_and(coverage_before_unknown, _) =
    coverage_before_unknown.

    % Boolean AND for the goal_trivial data type.
    %
:- pred goal_trivial_and(goal_trivial::in, goal_trivial::in,
    goal_trivial::out) is det.

goal_trivial_and(A, B, Trivial) :-
    (
        A = goal_is_trivial,
        B = goal_is_trivial
    ->
        Trivial = goal_is_trivial
    ;
        Trivial = goal_is_nontrivial
    ).

:- pred port_counts_give_coverage_after_and(port_counts_give_coverage_after::in,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

port_counts_give_coverage_after_and(A, B, PortCountsCoverageAfter) :-
    (
        A = port_counts_give_coverage_after,
        B = port_counts_give_coverage_after
    ->
        PortCountsCoverageAfter = port_counts_give_coverage_after
    ;
        PortCountsCoverageAfter = no_port_counts_give_coverage_after
    ).

    % Given a goal, whether it has its own port counts and whether port counts
    % are available immediately before it, determine if either set of port
    % counts allows us to determine how often execution reaches the point
    % immediately after the goal.
    %
:- pred has_port_counts_after(hlds_goal::in,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::in,
    port_counts_give_coverage_after::out) is det.

has_port_counts_after(Goal, PCDirect, PCBefore, PC) :-
    (
        % The trivial case. If port counts are directly available,
        % then they can be used to determine coverage immediately after it.

        PCDirect = port_counts_give_coverage_after,
        PC = port_counts_give_coverage_after
    ;
        PCDirect = no_port_counts_give_coverage_after,

        % If port counts aren't directly available but are before this goal
        % and this goal behaves deterministically (it cannot fail or redo),
        % then they can be used to determine how often execution reaches the
        % point after this goal.

        Detism = goal_info_get_determinism(Goal ^ hlds_goal_info),
        has_port_counts_if_det(Detism, PCBefore, PC)
    ).

    % Given the current goal's determinism and whether the next earliest goal
    % has port counts does this goal have port counts
    %
:- pred has_port_counts_if_det(determinism::in,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

has_port_counts_if_det(Detism, PortCountsCoverageAfter0,
        PortCountsCoverageAfter) :-
    (
        ( Detism = detism_det
        ; Detism = detism_cc_multi
        )
    ->
        PortCountsCoverageAfter = PortCountsCoverageAfter0
    ;
        PortCountsCoverageAfter = no_port_counts_give_coverage_after
    ).

    % Used to gather some information about goals before the coverage
    % transformation.
    %
    % This pass gathers the information in the dp_coverage_goal_info structure,
    % namely
    %
    % - whether the goal is trivial (a goal is trivial if neither it
    %   nor any of its subgoals are calls), and
    % - whether a port count is available from the deep profiler from which
    %   the coverage _after_ this goal can be computed.
    %
    % XXX: Currently the first pass is unsupported. The second pass does not
    % use the information it generates.
    %
:- pred coverage_prof_first_pass(coverage_profiling_options::in, hlds_goal::in,
    hlds_goal::out, port_counts_give_coverage_after::in,
    dp_coverage_goal_info::out) is det.

coverage_prof_first_pass(CPOptions, Goal0, Goal, PortCountsCoverageAfterBefore,
        Info) :-
    Goal0 = hlds_goal(GoalExpr0, GoalInfo0),
    (
        % XXX: Not all call goals have associated call sites, therefore not all
        % of these will have port counts.  For example inline foreign code does
        % not get instrumented by the deep profiler.  See above in the
        % deep_profiling transformation.
        %
        % This doesn't matter for the near future, since we're using a single
        % pass coverage profiling algorithm.  This will need to be fixed when
        % the two-pass coverage profiling is enabled.  Or if a naive assumption
        % in the second pass is corrected, (See the XXX comment at the
        % beginning of coverage_prof_second_pass_goal regarding the defaults
        % that are assumed if the information from the first pass is not
        % available.).
        %
        GoalExpr0 = plain_call(_, _, _, BuiltinState, _, _),
        (
            ( BuiltinState = out_of_line_builtin
            ; BuiltinState = not_builtin
            ),
            Trivial0 = goal_is_nontrivial,
            PortCountsCoverageAfterDirect = port_counts_give_coverage_after
        ;
            BuiltinState = inline_builtin,
            Trivial0 = goal_is_trivial,
            PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = generic_call(GenericCall, _, _, _),
        (
            ( GenericCall = higher_order(_, _, _, _)
            ; GenericCall = class_method(_, _, _, _)
            ),
            Trivial0 = goal_is_nontrivial,
            PortCountsCoverageAfterDirect = port_counts_give_coverage_after
        ;
            ( GenericCall = cast(_)
            ; GenericCall = event_call(_)
            ),
            Trivial0 = goal_is_trivial,
            PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after
        ),
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = call_foreign_proc(_, _, _, _, _, _, _),
        % Some foreign proc goals may be trivial., but there is no clear
        % criteria by which we can conclude that here.
        Trivial0 = goal_is_nontrivial,
        PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after,
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = unify(_, _, _, _, _),
        Trivial0 = goal_is_trivial,
        PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after,
        GoalExpr = GoalExpr0
    ;
        GoalExpr0 = conj(ConjType, Goals0),
        map_foldl2(coverage_prof_first_pass_conj(CPOptions), Goals0, Goals,
            goal_is_trivial, Trivial0,
            PortCountsCoverageAfterBefore, PortCountsCoverageAfterDirect),
        GoalExpr = conj(ConjType, Goals)
    ;
        GoalExpr0 = disj(Goals0),
        coverage_prof_first_pass_disj(CPOptions, Goals0, Goals, Trivial0,
            PortCountsCoverageAfterBefore, PortCountsCoverageAfterDirect),
        GoalExpr = disj(Goals)
    ;
        GoalExpr0 = switch(Var, CanFail, Cases0),
        coverage_prof_first_pass_switchcase(CPOptions, Cases0, Cases, Trivial0,
            PortCountsCoverageAfterCases),
        GoalExpr = switch(Var, CanFail, Cases),
        (
            CanFail = can_fail,
            PortCountsCoverageAfterDirect = no_port_counts_give_coverage_after
        ;
            CanFail = cannot_fail,
            PortCountsCoverageAfterDirect = PortCountsCoverageAfterCases
        )
    ;
        GoalExpr0 = negation(InnerGoal0),
        coverage_prof_first_pass(CPOptions, InnerGoal0, InnerGoal,
            PortCountsCoverageAfterBefore,
            dp_coverage_goal_info(Trivial0, PortCountsCoverageAfterDirect)),
        GoalExpr = negation(InnerGoal)
    ;
        GoalExpr0 = scope(Reason, InnerGoal0),
        coverage_prof_first_pass(CPOptions, InnerGoal0, InnerGoal,
            PortCountsCoverageAfterBefore,
            dp_coverage_goal_info(Trivial0, PortCountsCoverageAfterDirect)),
        GoalExpr = scope(Reason, InnerGoal)
    ;
        GoalExpr0 = if_then_else(Vars, CondGoal0, ThenGoal0, ElseGoal0),

        % The then and else parts of a ITE goal will be able to use the
        % port counts provided by the cond goal if it has them.

        coverage_prof_first_pass(CPOptions, CondGoal0, CondGoal,
            PortCountsCoverageAfterBefore,
            dp_coverage_goal_info(TrivialCond, PortCountsCoverageAfterCond)),

        coverage_prof_first_pass(CPOptions, ThenGoal0, ThenGoal,
            PortCountsCoverageAfterCond,
            dp_coverage_goal_info(TrivialThen, PortCountsCoverageAfterThen)),
        coverage_prof_first_pass(CPOptions, ElseGoal0, ElseGoal,
            PortCountsCoverageAfterCond,
            dp_coverage_goal_info(TrivialElse, PortCountsCoverageAfterElse)),

        GoalExpr = if_then_else(Vars, CondGoal, ThenGoal, ElseGoal),

        % An ITE is trivial iff all of its inner goals are trivial,

        goal_trivial_and(TrivialCond, TrivialThen, TrivialCondThen),
        goal_trivial_and(TrivialCondThen, TrivialElse, Trivial0),

        % And it has port counts iff it will end in a goal with a port count
        % regardless of which of the then and the else branch is taken.

        port_counts_give_coverage_after_and(PortCountsCoverageAfterThen,
            PortCountsCoverageAfterElse, PortCountsCoverageAfterDirect)
    ;
        GoalExpr0 = shorthand(_),
        unexpected(this_file, "coverage_prof_first_pass: shorthand")
    ),

    (
        CPOptions ^ cpo_use_portcounts = yes,
        has_port_counts_after(Goal0, PortCountsCoverageAfterDirect,
            PortCountsCoverageAfterBefore, PortCountsCoverageAfter)
    ;
        CPOptions ^ cpo_use_portcounts = no,
        PortCountsCoverageAfter = no_port_counts_give_coverage_after
    ),

    (
        CPOptions ^ cpo_use_trivial = yes,
        Trivial = Trivial0
    ;
        CPOptions ^ cpo_use_trivial = no,
        Trivial = goal_is_nontrivial
    ),

    % Annotate the goal with this new information.
    Info = dp_coverage_goal_info(Trivial, PortCountsCoverageAfter),
    goal_info_get_maybe_dp_info(GoalInfo0) = MaybeDPInfo0,
    (
        MaybeDPInfo0 = yes(dp_goal_info(IsProfilingInstrumentation, _)),
        DPInfo = dp_goal_info(IsProfilingInstrumentation, yes(Info))
    ;
        MaybeDPInfo0 = no,
        error("coverage_prof_first_pass: goal_dp_info not present")
    ),
    goal_info_set_maybe_dp_info(yes(DPInfo), GoalInfo0, GoalInfo),
    Goal = hlds_goal(GoalExpr, GoalInfo).

    % Combine information about goals within a conjunction
    %
:- pred coverage_prof_first_pass_conj(coverage_profiling_options::in,
    hlds_goal::in, hlds_goal::out, goal_trivial::in, goal_trivial::out,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

coverage_prof_first_pass_conj(CPOptions, Goal0, Goal, TrivialAcc, Trivial,
        PortCountsCoverageAfterAcc, PortCountsCoverageAfter) :-
    coverage_prof_first_pass(CPOptions, Goal0, Goal, PortCountsCoverageAfterAcc,
        dp_coverage_goal_info(TrivialGoal, PortCountsCoverageAfter)),
    goal_trivial_and(TrivialAcc, TrivialGoal, Trivial).

    % Combine information about goals within a disjunction.
    %
    % A portcount may be available to the goal executed when first entering a
    % disjunction. However it is impractical to determine if any disjuncts
    % other than the first are ever tried. So port counts at the beginning of
    % them are unknown.
    %
:- pred coverage_prof_first_pass_disj(coverage_profiling_options::in,
    list(hlds_goal)::in, list(hlds_goal)::out, goal_trivial::out,
    port_counts_give_coverage_after::in, port_counts_give_coverage_after::out)
    is det.

coverage_prof_first_pass_disj(_, [], [], goal_is_trivial,
        !PortCountsCoverageAfter).
coverage_prof_first_pass_disj(CPOptions, [Goal0 | Goals0], [Goal | Goals],
        Trivial, PortCountsCoverageBeforeDisjunct, PortCountsCoverageAfter) :-
    coverage_prof_first_pass(CPOptions, Goal0, Goal,
        PortCountsCoverageBeforeDisjunct,
        dp_coverage_goal_info(TrivialGoal, PortCountsCoverageAfterGoal)),
    coverage_prof_first_pass_disj(CPOptions, Goals0, Goals, TrivialDisj,
        no_port_counts_give_coverage_after, PortCountsCoverageAfterDisj),
    goal_trivial_and(TrivialGoal, TrivialDisj, Trivial),
    port_counts_give_coverage_after_and(PortCountsCoverageAfterGoal,
        PortCountsCoverageAfterDisj, PortCountsCoverageAfter).

    % A switch is a special type of disjunction.  The important difference here
    % is that the coverage of the first case cannot be inferred from the
    % coverage before the switch.
    %
:- pred coverage_prof_first_pass_switchcase(coverage_profiling_options::in,
    list(case)::in, list(case)::out, goal_trivial::out,
    port_counts_give_coverage_after::out) is det.

coverage_prof_first_pass_switchcase(_, [], [],
        goal_is_trivial, port_counts_give_coverage_after).
coverage_prof_first_pass_switchcase(CPOptions,
        [Case0 | Cases0], [Case | Cases], Trivial, PortCountsCoverageAfter) :-
    Case0 = case(FirstFunctor, LaterFunctors, Goal0),

    coverage_prof_first_pass(CPOptions, Goal0, Goal,
        no_port_counts_give_coverage_after,
        dp_coverage_goal_info(TrivialGoal, PortCountsCoverageAfterGoal)),
    coverage_prof_first_pass_switchcase(CPOptions, Cases0, Cases,
        TrivialSwitchcase, PortCountsCoverageAfterSwitchcase),
    goal_trivial_and(TrivialGoal, TrivialSwitchcase, Trivial),
    port_counts_give_coverage_after_and(PortCountsCoverageAfterGoal,
        PortCountsCoverageAfterSwitchcase, PortCountsCoverageAfter),

    Case = case(FirstFunctor, LaterFunctors, Goal).

%-----------------------------------------------------------------------------%

    % Insert a coverage point before the given goal. This returns a flat
    % conjunction consisting of a coverage point followed by the goal.
    %
:- pred insert_coverage_point_before(coverage_profiling_options::in,
    coverage_point_info::in, hlds_goal::in, hlds_goal::out,
    proc_coverage_info::in, proc_coverage_info::out) is det.

insert_coverage_point_before(CPOptions, CPInfo, !Goal, !Info) :-
    make_coverage_point(CPOptions, CPInfo, CPGoals, !Info),
    ( !.Goal = hlds_goal(conj(plain_conj, InnerGoals), _) ->
        Goals = CPGoals ++ InnerGoals
    ;
        Goals = CPGoals ++ [!.Goal]
    ),
    create_conj_from_list(Goals, plain_conj, !:Goal).

    % Builds a list of goals (that will form part of a conjunction)
    % for a coverage point.
    %
:- pred make_coverage_point(coverage_profiling_options::in,
    coverage_point_info::in, list(hlds_goal)::out,
    proc_coverage_info::in, proc_coverage_info::out) is det.

make_coverage_point(CPOptions, CoveragePointInfo, Goals, !CoverageInfo) :-
    CoveragePointInfos0 = !.CoverageInfo ^ ci_coverage_points,
    CPIndexCounter0 = !.CoverageInfo ^ ci_cp_index_counter,

    counter.allocate(CPIndex, CPIndexCounter0, CPIndexCounter),
    map.det_insert(CoveragePointInfos0, CPIndex, CoveragePointInfo,
        CoveragePointInfos),
    !:CoverageInfo = !.CoverageInfo ^ ci_coverage_points := CoveragePointInfos,
    !:CoverageInfo = !.CoverageInfo ^ ci_cp_index_counter := CPIndexCounter,

    % Build unifications for the coverage point index and the proc static.

    some [!VarInfo] (
        !:VarInfo = !.CoverageInfo ^ ci_var_info,

        generate_var("CPIndex", int_type, CPIndexVar, !VarInfo),
        generate_unify(int_const(CPIndex), CPIndexVar, GoalUnifyIndex),
        generate_var("ProcLayout", c_pointer_type, ProcLayoutVar, !VarInfo),
        proc_static_cons_id(!.CoverageInfo, ProcStaticConsId),
        generate_unify(ProcStaticConsId, ProcLayoutVar, GoalUnifyProcLayout),

        !CoverageInfo ^ ci_var_info := !.VarInfo
    ),

    % Build a call to the instrumentation code.

    UseCalls = CPOptions ^ cpo_use_calls,
    ModuleInfo = !.CoverageInfo ^ ci_module_info,
    PredName = "increment_coverage_point_count",
    PredArity = 2,
    ArgVars = [ProcLayoutVar, CPIndexVar],
    % Note: The body of increment_coverage_point_count includes several
    % assertions. If these are enabled, then bodily including the C code
    % at EVERY coverage point will cause significant code bloat. Generating
    % a call to a predicate with the same code in library/profiling_builtin.m
    % should then yield smaller code, and due to cache effects, it will
    % probably yield faster code as well.
    (
        UseCalls = no,
        get_deep_profile_builtin_ppid(ModuleInfo, PredName, PredArity,
            PredId, ProcId),
        Ground = ground(shared, none),
        make_foreign_args([ProcLayoutVar, CPIndexVar],
            [(yes("ProcLayout" - (Ground -> Ground)) - native_if_possible),
            (yes("CPIndex" - (Ground -> Ground)) - native_if_possible)],
            [c_pointer_type, int_type], ForeignArgVars),
        coverage_point_ll_code(ForeignCallAttrs, ForeignCode),
        CallGoalExpr = call_foreign_proc(ForeignCallAttrs, PredId, ProcId,
            ForeignArgVars, [], no, ForeignCode),
        NonLocals = list_to_set(ArgVars),
        instmap_delta_from_assoc_list([], InstMapDelta),
        CallGoalInfo = impure_init_goal_info(NonLocals, InstMapDelta,
            detism_det),
        CallGoal = hlds_goal(CallGoalExpr, CallGoalInfo)
    ;
        UseCalls = yes,
        generate_deep_call(ModuleInfo, PredName, PredArity, ArgVars,
            yes([]), detism_det, CallGoal)
    ),

    % Construct complete goal list.
    Goals = [GoalUnifyIndex, GoalUnifyProcLayout, CallGoal].

    % Turn a map of coverage points and their indexes into a list in sorted
    % order.
    %
:- pred coverage_points_map_list(map(int, coverage_point_info)::in,
    list(coverage_point_info)::out) is det.

coverage_points_map_list(Map, List) :-
    map.to_sorted_assoc_list(Map, AssocList),
    assoc_list.values(AssocList, List).

    % Retrieve the pred and proc ids from either the deep_maybe_rec_info or
    % deep_pred_proc_id fields of a deep_info structure.
    %
:- pred pred_proc_id(proc_coverage_info::in, pred_id::out, proc_id::out)
    is det.

pred_proc_id(CoverageInfo, PredId, ProcId) :-
    MaybeRecInfo = CoverageInfo ^ ci_maybe_rec_info,
    PredProcId = CoverageInfo ^ ci_pred_proc_id,
    (
        MaybeRecInfo = yes(RecInfo),
        RecInfo ^ role = inner_proc(OuterPredProcId)
    ->
        OuterPredProcId = proc(PredId, ProcId)
    ;
        PredProcId = proc(PredId, ProcId)
    ).

    % Create a proc static cons_id from the deep recursion info.
    %
:- pred proc_static_cons_id(proc_coverage_info::in, cons_id::out) is det.

proc_static_cons_id(CoverageInfo, ProcStaticConsId) :-
    pred_proc_id(CoverageInfo, PredId, ProcId),
    ShroudedPredProcId = shroud_pred_proc_id(proc(PredId, ProcId)),
    ProcStaticConsId = deep_profiling_proc_layout(ShroudedPredProcId).

    % Returns a string containing the Low Level C code for a coverage point.
    %
:- pred coverage_point_ll_code(pragma_foreign_proc_attributes::out,
    pragma_foreign_code_impl::out) is det.

coverage_point_ll_code(ForeignProcAttrs, ForeignCodeImpl) :-
    some [!ForeignProcAttrs] (
        % XXX When running this code in a parallel grade the contention for the
        % foreign code mutex may be very expensive.  TO improve this we should
        % add a mechanism that, in par grades, allows us to replace the general
        % foreign code mutex with one that guards only the data coverage data
        % structure we are updating, since that would yield a LOT less
        % contention.
        !:ForeignProcAttrs = default_attributes(lang_c),
        set_thread_safe(proc_not_thread_safe, !ForeignProcAttrs),
        set_may_call_mercury(proc_will_not_call_mercury, !ForeignProcAttrs),
        set_purity(purity_impure, !ForeignProcAttrs),
        set_terminates(proc_terminates, !ForeignProcAttrs),
        set_may_throw_exception(proc_will_not_throw_exception,
            !ForeignProcAttrs),
        ForeignProcAttrs = !.ForeignProcAttrs
    ),
    ForeignCodeImpl = fc_impl_ordinary(Code, no),
    Code =
    % The code of this predicate is duplicated bodily in profiling_builtin.m
    % in the library directory, so any changes here should also be made there.
"
#ifdef MR_DEEP_PROFILING
    const MR_ProcLayout *pl;
    MR_ProcStatic       *ps;

    MR_enter_instrumentation();

  #ifdef MR_DEEP_PROFILING_LOWLEVEL_DEBUG
    if (MR_calldebug && MR_lld_print_enabled) {
        MR_print_deep_prof_vars(stdout, ""increment_coverage_point_count"");
        printf("", ProcLayout: 0x%x, CPIndex: %d\\n"", ProcLayout, CPIndex);
    }
  #endif

    pl = (const MR_ProcLayout *) ProcLayout;

    MR_deep_assert(NULL, NULL, NULL, pl != NULL);
    ps = pl->MR_sle_proc_static;
    MR_deep_assert(NULL, pl, NULL, ps != NULL);

    MR_deep_assert(NULL, pl, ps, CPIndex >= ps->MR_ps_num_coverage_points);
    MR_deep_assert(NULL, pl, ps, ps->MR_ps_coverage_points != NULL);

    ps->MR_ps_coverage_points[CPIndex]++;

    MR_leave_instrumentation();
#else
    MR_fatal_error(
        ""increment_coverage_point_count: deep profiling not enabled"");
#endif /* MR_DEEP_PROFILING */
".

%-----------------------------------------------------------------------------%

:- func goal_info_get_dp_info(hlds_goal_info) = dp_goal_info.

goal_info_get_dp_info(GoalInfo) = DPInfo :-
    MaybeDPInfo = goal_info_get_maybe_dp_info(GoalInfo),
    (
        MaybeDPInfo = yes(DPInfo)
    ;
        MaybeDPInfo = no,
        error("goal_info_get_dp_info: MaybeDPInfo = no")
    ).

:- func goal_info_get_maybe_dp_coverage_info(hlds_goal_info) =
    maybe(dp_coverage_goal_info).

goal_info_get_maybe_dp_coverage_info(GoalInfo) = MaybeCoverageInfo :-
    MaybeDPInfo = goal_info_get_maybe_dp_info(GoalInfo),
    (
        MaybeDPInfo = yes(DPInfo),
        DPInfo = dp_goal_info(_, MaybeCoverageInfo)
    ;
        MaybeDPInfo = no,
        MaybeCoverageInfo = no
    ).

:- func goal_get_maybe_dp_port_counts_coverage(hlds_goal) =
    port_counts_give_coverage_after.

goal_get_maybe_dp_port_counts_coverage(Goal) = PortCountsGiveCoverageAfter :-
    Goal = hlds_goal(_, GoalInfo),
    MaybeCoverageInfo = goal_info_get_maybe_dp_coverage_info(GoalInfo),
    (
        MaybeCoverageInfo =
            yes(dp_coverage_goal_info(_, PortCountsGiveCoverageAfter))
    ;
        MaybeCoverageInfo = no,
        PortCountsGiveCoverageAfter = no_port_counts_give_coverage_after
    ).

    % Set the 'goal_is_mdprof_inst' field in the goal_dp_info structure
    % in the given goal info structure.
    %
:- pred goal_info_set_mdprof_inst(goal_is_mdprof_inst::in,
    hlds_goal_info::in, hlds_goal_info::out) is det.

goal_info_set_mdprof_inst(IsMDProfInst, !GoalInfo) :-
    goal_info_get_maybe_dp_info(!.GoalInfo) = MaybeDPInfo0,
    (
        MaybeDPInfo0 = yes(dp_goal_info(_, DPCoverageInfo)),
        MaybeDPInfo = yes(dp_goal_info(IsMDProfInst, DPCoverageInfo))
    ;
        MaybeDPInfo0 = no,
        MaybeDPInfo = yes(dp_goal_info(IsMDProfInst, no))
    ),
    goal_info_set_maybe_dp_info(MaybeDPInfo, !GoalInfo).

%-----------------------------------------------------------------------------%

:- func this_file = string.

this_file = "deep_profiling.m".

%-----------------------------------------------------------------------------%
:- end_module deep_profiling.
%-----------------------------------------------------------------------------%
