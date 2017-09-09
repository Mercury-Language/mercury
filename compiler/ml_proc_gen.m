%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2009-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: ml_proc_gen.m.
% Main author: fjh.
%

:- module ml_backend.ml_proc_gen.
:- interface.

:- import_module hlds.
:- import_module hlds.hlds_module.
:- import_module ml_backend.ml_gen_info.
:- import_module ml_backend.ml_global_data.
:- import_module ml_backend.mlds.
:- import_module parse_tree.
:- import_module parse_tree.error_util.

:- import_module list.

%---------------------------------------------------------------------------%

    % Generate MLDS definitions for all the non-imported predicates
    % (and functions) in the HLDS.
    %
:- pred ml_gen_preds(mlds_target_lang::in,
    ml_const_struct_map::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    module_info::in, module_info::out,
    list(error_spec)::in, list(error_spec)::out) is det.

%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.mode_util.
:- import_module hlds.code_model.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_desc.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_pred.
:- import_module hlds.mark_tail_calls.
:- import_module hlds.passes_aux.
:- import_module hlds.quantification.
:- import_module hlds.status.
:- import_module hlds.vartypes.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module ml_backend.ml_args_util.
:- import_module ml_backend.ml_code_gen.
:- import_module ml_backend.ml_code_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module int.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

ml_gen_preds(Target, ConstStructMap, FuncDefns,
        !GlobalData, !ModuleInfo, !Specs) :-
    module_info_get_preds(!.ModuleInfo, PredTable0),
    map.to_sorted_assoc_list(PredTable0, PredIdInfos0),
    ml_find_procs_for_code_gen(PredIdInfos0, PredIdInfos, [], PredProcIds),
    map.from_sorted_assoc_list(PredIdInfos, PredTable),
    module_info_set_preds(PredTable, !ModuleInfo),

    list.sort(PredProcIds, SortedPredProcIds),
    set.sorted_list_to_set(SortedPredProcIds, CodeGenPredProcIds),

    DepInfo = build_proc_dependency_graph(!.ModuleInfo, CodeGenPredProcIds,
        only_all_calls),
    get_bottom_up_sccs_with_entry_points(!.ModuleInfo, DepInfo,
        BottomUpSCCsWithEntryPoints),

    % Optimize tail calls only if asked.
    module_info_get_globals(!.ModuleInfo, Globals),
    ( if
        globals.lookup_bool_option(Globals, optimize_tailcalls, yes),
        globals.lookup_bool_option(Globals, optimize_tailcalls_codegen, yes)
    then
        globals.lookup_bool_option(Globals, optimize_tailcalls_codegen_mutual,
            TailCallsMutual),
        (
            TailCallsMutual = no,
            OptTailCallsMutual = no_tail_call_opt_mutual
        ;
            TailCallsMutual = yes,
            OptTailCallsMutual = tail_call_opt_mutual
        ),
        OptTailCalls = tail_call_opt_in_code_gen(OptTailCallsMutual)
    else
        OptTailCalls = no_tail_call_opt_in_code_gen
    ),
    ml_gen_sccs(!.ModuleInfo, OptTailCalls, Target, ConstStructMap,
        BottomUpSCCsWithEntryPoints, [], FuncDefns, !GlobalData, !Specs).

:- pred ml_find_procs_for_code_gen(
    assoc_list(pred_id, pred_info)::in,
    assoc_list(pred_id, pred_info)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

ml_find_procs_for_code_gen([], [], !CodeGenPredProcIds).
ml_find_procs_for_code_gen([PredIdInfo0 | PredIdInfos0],
        [PredIdInfo | PredIdInfos], !CodeGenPredProcIds) :-
    PredIdInfo0 = PredId - PredInfo0,
    pred_info_get_status(PredInfo0, PredStatus),
    ( if
        (
            PredStatus = pred_status(status_imported(_))
        ;
            % We generate incorrect and unnecessary code for the external
            % special preds which are pseudo_imported, so just ignore them.
            is_unify_or_compare_pred(PredInfo0),
            PredStatus =
                pred_status(status_external(status_pseudo_imported))
        )
    then
        PredIdInfo = PredIdInfo0
    else
        % Generate MLDS definitions for all the non-imported procedures
        % of a given predicate (or function).
        %
        % If a type is imported, the compiler will generate the (in,in) mode
        % of its unify and compare predicates in its home module, but if this
        % module needs one of these predicates in a more specialize mode
        % (e.g. one in which an input argument is known to be bound to one
        % of a small subset of the possible function symbols), it has to create
        % code for it itself. Such procedures are pseudo imported, which means
        % that their procedure 0 (the procedure implementing the (in,in) mode)
        % is imported, but any other procedures are not.

        ( if PredStatus = pred_status(status_external(_)) then
            ProcIds = pred_info_procids(PredInfo0)
        else
            ProcIds = pred_info_non_imported_procids(PredInfo0)
        ),
        pred_info_get_proc_table(PredInfo0, ProcTable0),
        list.foldl(requantify_codegen_proc, ProcIds, ProcTable0, ProcTable),
        pred_info_set_proc_table(ProcTable, PredInfo0, PredInfo),
        PredIdInfo = PredId - PredInfo,

        PredProcIds = list.map((func(ProcId) = proc(PredId, ProcId)), ProcIds),
        !:CodeGenPredProcIds = PredProcIds ++ !.CodeGenPredProcIds
    ),
    ml_find_procs_for_code_gen(PredIdInfos0, PredIdInfos, !CodeGenPredProcIds).

    % The specification of the HLDS allows goal_infos to overestimate
    % the set of non-locals. Such overestimates are bad for us for two reasons:
    %
    % - If the non-locals of the top-level goal contained any variables other
    %   than head vars, those variables would not be declared.
    %
    % - The code of goal_expr_find_subgoal_nonlocals depends on the nonlocals
    %   sets of goals being exactly correct, since this is the only way it can
    %   avoid traversing the entirety of the goals themselves. Such traversals
    %   can be very expensive on large goals, since it would have to be done
    %   repeatedly, once for each containing goal. Quantification does just one
    %   traversal.
    %
:- pred requantify_codegen_proc(proc_id::in, proc_table::in, proc_table::out)
    is det.

requantify_codegen_proc(ProcId, !ProcTable) :-
    map.lookup(!.ProcTable, ProcId, ProcInfo0),
    requantify_proc_general(ordinary_nonlocals_no_lambda, ProcInfo0, ProcInfo),
    map.det_update(ProcId, ProcInfo, !ProcTable).

:- type maybe_tail_call_opt_mutual
    --->    no_tail_call_opt_mutual
    ;       tail_call_opt_mutual.

:- type maybe_tail_call_opt_in_code_gen
    --->    no_tail_call_opt_in_code_gen
    ;       tail_call_opt_in_code_gen(maybe_tail_call_opt_mutual).

:- pred ml_gen_sccs(module_info::in, maybe_tail_call_opt_in_code_gen::in,
    mlds_target_lang::in, ml_const_struct_map::in,
    list(scc_with_entry_points)::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_sccs(_, _, _, _, [], !FuncDefns, !GlobalData, !Specs).
ml_gen_sccs(ModuleInfo, OptTailCalls, Target, ConstStructMap, [SCCE | SCCEs],
        !FuncDefns, !GlobalData, !Specs) :-
    ml_gen_scc(ModuleInfo, OptTailCalls, Target, ConstStructMap, SCCE,
        !FuncDefns, !GlobalData, !Specs),
    ml_gen_sccs(ModuleInfo, OptTailCalls, Target, ConstStructMap, SCCEs,
        !FuncDefns, !GlobalData, !Specs).

%---------------------------------------------------------------------------%

:- pred ml_gen_scc(module_info::in, maybe_tail_call_opt_in_code_gen::in,
    mlds_target_lang::in, ml_const_struct_map::in, scc_with_entry_points::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_scc(ModuleInfo, OptTailCalls, Target, ConstStructMap, SCCE,
        !FuncDefns, !GlobalData, !Specs) :-
    SCCE = scc_with_entry_points(PredProcIds, CalledFromHigherSCCs,
        ExportedProcs),
    set.union(CalledFromHigherSCCs, ExportedProcs, SCCEntryProcs),
    (
        OptTailCalls = no_tail_call_opt_in_code_gen,
        set.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                no_tail_rec), PredProcIds, !FuncDefns, !GlobalData, !Specs)
    ;
        OptTailCalls = tail_call_opt_in_code_gen(OptTailCallsMutual),
        partition_scc_procs(ModuleInfo, set.to_sorted_list(PredProcIds),
            NonePredProcIdInfos, SelfPredProcIdInfos0,
            MutualDetPredProcIdInfos0, MutualSemiPredProcIdInfos0),

        % The predicates called by ml_gen_tscc have not (yet) been taught
        % how to handle copy out of output arguments, and (for now) they
        % always generate gc_no_stmt as the gc annotation on MLDS function
        % parameters. Until those limitations are fixed, don't give
        % any work to ml_gen_tscc in circumstances where they would bite,
        % or when the user has requested that it not be given any work.
        module_info_get_globals(ModuleInfo, Globals),
        globals.lookup_bool_option(Globals, det_copy_out, DetCopyOut),
        globals.get_gc_method(Globals, GC),
        ( if
            DetCopyOut = no,
            GC \= gc_accurate,
            OptTailCallsMutual = tail_call_opt_mutual
        then
            SelfPredProcIdInfos = SelfPredProcIdInfos0,
            MutualDetPredProcIdInfos = MutualDetPredProcIdInfos0,
            MutualSemiPredProcIdInfos = MutualSemiPredProcIdInfos0
        else
            SelfPredProcIdInfos = SelfPredProcIdInfos0 ++
                MutualDetPredProcIdInfos0 ++ MutualSemiPredProcIdInfos0,
            MutualDetPredProcIdInfos = [],
            MutualSemiPredProcIdInfos = []
        ),

        % Translate the procedures we cannot apply tail call optimization to.
        list.foldl3(
            ml_gen_proc(ModuleInfo, Target, ConstStructMap,
                no_tail_rec),
            NonePredProcIdInfos, !FuncDefns, !GlobalData, !Specs),

        % Translate the procedures to which we can apply only self-tail-call
        % optimization.
        list.foldl3(
            ml_gen_proc(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            SelfPredProcIdInfos, !FuncDefns, !GlobalData, !Specs),

        % Translate the procedures to which we can apply mutual-tail-call
        % optimization as well.
        DetTSCCDepInfo = build_proc_dependency_graph(ModuleInfo,
            set.list_to_set(
                list.map(project_pred_proc_id_info_id,
                    MutualDetPredProcIdInfos)),
            only_tail_calls),
        SemiTSCCDepInfo = build_proc_dependency_graph(ModuleInfo,
            set.list_to_set(
                list.map(project_pred_proc_id_info_id,
                    MutualSemiPredProcIdInfos)),
            only_tail_calls),
        get_bottom_up_sccs_with_entry_points(ModuleInfo, DetTSCCDepInfo,
            DetTSCCEntries),
        get_bottom_up_sccs_with_entry_points(ModuleInfo, SemiTSCCDepInfo,
            SemiTSCCEntries),
        partition_tsccs(DetTSCCEntries,
            DetLonePredProcIds, DetNonTrivialTSCCEntries),
        partition_tsccs(SemiTSCCEntries,
            SemiLonePredProcIds, SemiNonTrivialTSCCEntries),
        list.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            DetLonePredProcIds, !FuncDefns, !GlobalData, !Specs),
        list.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            SemiLonePredProcIds, !FuncDefns, !GlobalData, !Specs),
        list.foldl3(
            ml_gen_tscc(ModuleInfo, Target, ConstStructMap, SCCEntryProcs,
                tscc_det),
            DetNonTrivialTSCCEntries, !FuncDefns, !GlobalData, !Specs),
        list.foldl3(
            ml_gen_tscc(ModuleInfo, Target, ConstStructMap, SCCEntryProcs,
                tscc_semi),
            SemiNonTrivialTSCCEntries, !FuncDefns, !GlobalData, !Specs)
    ).

:- type pred_proc_id_info
    --->    pred_proc_id_info(
                pred_proc_id,
                pred_info,
                proc_info,
                prog_context
            ).

:- func project_pred_proc_id_info_id(pred_proc_id_info) = pred_proc_id.

project_pred_proc_id_info_id(PredProcIdInfo) = PredProcId :-
    PredProcIdInfo = pred_proc_id_info(PredProcId, _, _, _).

    % Partition the procedures in an SCC into the following four categories.
    %
    % - Those that don't contain any tail calls (NoneIdInfos).
    % - Those that in which we can only optimize self recursive tail calls,
    %   either because they don't contain any mutually tail recursive calls,
    %   or because we can't (yet) optimize the mutually tail recursive calls
    %   they do contain (SelfIdInfos).
    % - Those model_det procedures in which we can optimize mutually tail
    %   recursive calls (MutualDetIdInfos).
    % - Those model_semi procedures in which we can optimize mutually tail
    %   recursive calls (MutualSemiIdInfos).
    %
:- pred partition_scc_procs(module_info::in, list(pred_proc_id)::in,
    list(pred_proc_id_info)::out, list(pred_proc_id_info)::out,
    list(pred_proc_id_info)::out, list(pred_proc_id_info)::out) is det.

partition_scc_procs(_ModuleInfo, [], [], [], [], []).
partition_scc_procs(ModuleInfo, [PredProcId | PredProcIds],
        !:NoneIdInfos, !:SelfIdInfos,
        !:MutualDetIdInfos, !:MutualSemiIdInfos) :-
    partition_scc_procs(ModuleInfo, PredProcIds,
        !:NoneIdInfos, !:SelfIdInfos, !:MutualDetIdInfos, !:MutualSemiIdInfos),
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    ProcContext = goal_info_get_context(GoalInfo),
    IdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo, ProcContext),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    (
        % Tail recursion optimization does not apply to model_non procedures.
        % XXX Actually, ml_tailcall.m can and does find *some* opportunities
        % for tail calls in model_non procedures. It would be nice to teach
        % this module as well how to exploit such opportunities, but applying
        % mutual tail recursion optimization in only det and semidet procedures
        % does capture *almost all* of the available benefit.
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        proc_info_interface_determinism(ProcInfo, Detism),
        determinism_components(Detism, _CanFail, SolnCount),
        proc_info_get_has_tail_rec_call(ProcInfo, HasTailRecCall),
        HasTailRecCall = has_tail_rec_call(HasSelfTailRecCall,
            HasMutualTailRecCall),
        ( if
            HasMutualTailRecCall = has_mutual_tail_rec_call,
            % XXX This limitation exists for now because handling return values
            % need to be handled separately from other output arguments,
            % and the initial implementation of mutual tail recursion
            % is simpler without this extra complication.
            not proc_is_output_det_function(ModuleInfo, PredInfo, ProcInfo, _),
            % To prevent control just falling through to the next procedure
            % body once it reaches the end of the previous procedure body
            % in the TSCC, we put a return statement at the end of each
            % procedure body. This will generate an error from some target
            % language compilers (e.g. Java) if its knows that this return
            % statement is not reachable.
            SolnCount \= at_most_zero
        then
            (
                CodeModel = model_det,
                !:MutualDetIdInfos = [IdInfo | !.MutualDetIdInfos]
            ;
                CodeModel = model_semi,
                !:MutualSemiIdInfos = [IdInfo | !.MutualSemiIdInfos]
            )
        else
            (
                HasSelfTailRecCall = has_self_tail_rec_call,
                !:SelfIdInfos = [IdInfo | !.SelfIdInfos]
            ;
                HasSelfTailRecCall = has_no_self_tail_rec_call,
                !:NoneIdInfos = [IdInfo | !.NoneIdInfos]
            )
        )
    ;
        CodeModel = model_non,
        !:NoneIdInfos = [IdInfo | !.NoneIdInfos]
    ).

:- pred partition_tsccs(list(scc_with_entry_points)::in,
    list(pred_proc_id)::out, list(scc_with_entry_points)::out) is det.

partition_tsccs([], [], []).
partition_tsccs([TSCC | TSCCs], !:LonePredProcIds, !:NonTrivialTSCCS) :-
    partition_tsccs(TSCCs, !:LonePredProcIds, !:NonTrivialTSCCS),
    TSCC = scc_with_entry_points(TSCCPredProcIdsSet, _, _),
    set.to_sorted_list(TSCCPredProcIdsSet, TSCCPredProcIds),
    (
        TSCCPredProcIds = [],
        unexpected($pred, "empty TSCC")
    ;
        TSCCPredProcIds = [TSCCPredProcId],
        !:LonePredProcIds = [TSCCPredProcId | !.LonePredProcIds]
    ;
        TSCCPredProcIds = [_, _ | _],
        !:NonTrivialTSCCS = [TSCC | !.NonTrivialTSCCS]
    ).

%---------------------------------------------------------------------------%
%
% Code for handling individual procedures.
%

:- pred ml_gen_proc_lookup(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, none_or_self_tail_rec::in, pred_proc_id::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap, NoneOrSelf,
        PredProcId, !FuncDefns, !GlobalData, !Specs) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    ProcContext = goal_info_get_context(GoalInfo),
    PredProcIdInfo =
        pred_proc_id_info(PredProcId, PredInfo, ProcInfo, ProcContext),
    ml_gen_proc(ModuleInfo, Target, ConstStructMap,
        NoneOrSelf, PredProcIdInfo, !FuncDefns, !GlobalData, !Specs).

%---------------------%

:- type none_or_self_tail_rec
    --->    no_tail_rec
    ;       self_tail_rec.

:- pred ml_gen_proc(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, none_or_self_tail_rec::in, pred_proc_id_info::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_proc(ModuleInfo, Target, ConstStructMap, NoneOrSelf,
        PredProcIdInfo, !FuncDefns, !GlobalData, !Specs) :-
    PredProcIdInfo =
        pred_proc_id_info(PredProcId, PredInfo, ProcInfo, ProcContext),
    trace [io(!IO)] (
        write_proc_progress_message("% Generating MLDS code for ",
            PredProcId, ModuleInfo, !IO)
    ),

    some [!Info] (
        compute_initial_tail_rec_map_for_none_or_self(ModuleInfo, NoneOrSelf,
            PredProcId, TailRecMap0),
        TsccInfo0 = init_ml_gen_tscc_info(ModuleInfo, TailRecMap0,
            tscc_self_rec_only),
        !:Info = ml_gen_info_init(ModuleInfo, Target, ConstStructMap,
            PredProcId, ProcInfo, !.GlobalData, TsccInfo0),

        pred_info_get_status(PredInfo, PredStatus),
        ( if PredStatus = pred_status(status_external(_)) then
            % For Mercury procedures declared `:- pragma external_{pred/func}',
            % we generate an MLDS definition with no function body.
            % The MLDS -> target code pass can treat this accordingly.
            % For example, for C it outputs a function declaration with no
            % corresponding definition, making sure that the function is
            % declared as `extern' rather than `static'.
            ml_gen_info_proc_params(PredProcId, FuncParams,
                _ByRefOutputVars, _CopiedOutputVars, !.Info, _Info),
            FuncBody = body_external,
            set.init(EnvVarNames),
            ClosureWrapperFuncDefns = []
        else
            % Set up the initial success continuation, if any.
            % Also figure out which output variables are returned by value
            % (rather than being passed by reference) and remove them from
            % the byref_output_vars field in the ml_gen_info.
            CodeModel = proc_info_interface_code_model(ProcInfo),
            ml_gen_info_proc_params(PredProcId, FuncParams,
                ByRefOutputVars, CopiedOutputVars, !.Info, _Info),
            ml_gen_info_set_byref_output_vars(ByRefOutputVars, !Info),
            (
                ( CodeModel = model_det
                ; CodeModel = model_semi
                )
            ;
                CodeModel = model_non,
                ml_gen_var_list(!.Info, CopiedOutputVars, OutputVarLvals),
                ml_variable_types(!.Info, CopiedOutputVars, OutputVarTypes),
                ml_initial_cont(!.Info, OutputVarLvals, OutputVarTypes,
                    InitialCont),
                ml_gen_info_push_success_cont(InitialCont, !Info)
            ),

            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_argmodes(ProcInfo, HeadModes),
            pred_info_get_arg_types(PredInfo, HeadTypes),
            modes_to_top_functor_modes(ModuleInfo, HeadModes, HeadTypes,
                TopFunctorModes),
            proc_info_get_goal(ProcInfo, Goal),
            ml_gen_proc_body(CodeModel, sot_solo, HeadVars, HeadTypes,
                TopFunctorModes, CopiedOutputVars, Goal,
                LocalVarDefns0, FuncDefns, GoalStmts, !Info),
            ml_gen_post_process_locals(ProcInfo, ProcContext,
                HeadVars, HeadTypes, CopiedOutputVars,
                LocalVarDefns0, LocalVarDefns, !Info),

            ml_gen_info_final(!.Info, EnvVarNames,
                ClosureWrapperFuncDefns, !:GlobalData, TsccInfo),
            TailRecInfo = TsccInfo ^ mgti_tail_rec_info,
            construct_func_body_maybe_wrap_in_loop(PredProcId, ProcContext,
                LocalVarDefns, FuncDefns, GoalStmts, TailRecInfo, FuncBody,
                !Specs)
        )
    ),

    construct_func_defn(ModuleInfo, PredProcIdInfo, FuncParams, FuncBody,
        EnvVarNames, FuncDefn),
    !:FuncDefns = ClosureWrapperFuncDefns ++ [FuncDefn | !.FuncDefns].

:- pred compute_initial_tail_rec_map_for_none_or_self(module_info::in,
    none_or_self_tail_rec::in, pred_proc_id::in, tail_rec_target_map::out)
    is det.

compute_initial_tail_rec_map_for_none_or_self(ModuleInfo, NoneOrSelf,
        PredProcId, TailRecMap0) :-
    (
        NoneOrSelf = no_tail_rec,
        map.init(TailRecMap0)
    ;
        NoneOrSelf = self_tail_rec,
        InputParams =
            ml_gen_proc_params_inputs_only_no_gc_stmts(ModuleInfo, PredProcId),
        TailRecTargetInfo0 = tail_rec_target_info(proc_id_in_tscc(1),
            InputParams, have_not_done_self_tail_rec,
            have_not_done_mutual_tail_rec, have_not_done_nontail_rec),
        TailRecMap0 = map.singleton(PredProcId, TailRecTargetInfo0)
    ).

:- pred construct_func_body_maybe_wrap_in_loop(pred_proc_id::in,
    prog_context::in, list(mlds_local_var_defn)::in,
    list(mlds_function_defn)::in, list(mlds_stmt)::in, tail_rec_info::in,
    mlds_function_body::out,
    list(error_spec)::in, list(error_spec)::out) is det.

construct_func_body_maybe_wrap_in_loop(PredProcId, Context,
        LocalVarDefns, FuncDefns, GoalStmts, TailRecInfo, FuncBody, !Specs) :-
    TailRecInfo = tail_rec_info(TargetMap, TsccKind, LoopKind,
        _WarnDefaultParams, _WarnProcParams, TailRecSpecs),
    expect(unify(TsccKind, tscc_self_rec_only), $pred,
        "TsccKind != tscc_self_rec_only"),
    !:Specs = TailRecSpecs ++ !.Specs,
    ( if
        map.search(TargetMap, PredProcId, TailRecTargetInfo),
        TailRecTargetInfo = tail_rec_target_info(IdInTscc, _InputParams,
            HaveDoneSelfTailRec, _HaveDoneMutualTailRec, _HaveDoneNonTailRec),
        HaveDoneSelfTailRec = have_done_self_tail_rec
    then
        Comment = comment("setup for tailcalls optimized into a loop"),
        CommentStmt = ml_stmt_atomic(Comment, Context),
        (
            LoopKind = tail_rec_loop_while_continue,
            BreakStmt = ml_stmt_goto(goto_break, Context),
            LoopBodyStmt = ml_stmt_block(LocalVarDefns, FuncDefns,
                [CommentStmt] ++ GoalStmts ++ [BreakStmt], Context),
            FuncBodyStmt = ml_stmt_while(may_loop_zero_times,
                ml_const(mlconst_true), LoopBodyStmt, Context)
        ;
            LoopKind = tail_rec_loop_label_goto,
            StartLabel = generate_tail_rec_start_label(TsccKind, IdInTscc),
            LoopTopLabelStmt = ml_stmt_label(StartLabel, Context),
            FuncBodyStmt = ml_stmt_block(LocalVarDefns, FuncDefns,
                [CommentStmt, LoopTopLabelStmt] ++ GoalStmts, Context)
        )
    else
        FuncBodyStmt = ml_gen_block(LocalVarDefns, FuncDefns,
            GoalStmts, Context)
    ),
    FuncBody = body_defined_here(FuncBodyStmt).

:- pred construct_func_defn(module_info::in, pred_proc_id_info::in,
    mlds_func_params::in, mlds_function_body::in, set(string)::in,
    mlds_function_defn::out) is det.

construct_func_defn(ModuleInfo, PredProcIdInfo, FuncParams, FuncBody,
        EnvVarNames, FuncDefn) :-
    PredProcIdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo,
        _ProcContext),
    PredProcId = proc(PredId, ProcId),
    ml_gen_proc_label(ModuleInfo, PredProcId, _ModuleName, PlainFuncName),
    proc_info_get_context(ProcInfo, ProcContext),
    DeclFlags = ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId),
    MaybePredProcId = yes(PredProcId),
    pred_info_get_attributes(PredInfo, Attributes),
    attributes_to_attribute_list(Attributes, AttributeList),
    MLDS_Attributes =
        list.map(attribute_to_mlds_attribute(ModuleInfo), AttributeList),
    MaybeRequireTailrecInfoFD = no,
    FuncDefn = mlds_function_defn(mlds_function_name(PlainFuncName),
        ProcContext, DeclFlags, MaybePredProcId, FuncParams, FuncBody,
        MLDS_Attributes, EnvVarNames, MaybeRequireTailrecInfoFD).

%---------------------------------------------------------------------------%
%
% Code for handling TSCCs (shorthand for via-tail-call SCCs, i.e. SCCs
% computed by taking only *tail* calls into account).
%
% The code we generate follows the pattern shown by the following (simplified)
% example, which is for a TSCC containing two det procedures,
% a(AIn1, AIn2, AOut1) and b(BIn1, BIn2, BIn3, BOut1):
% 
% a_3(
%   MR_Word tscc_proc_1_input_1,
%   MR_Word tscc_proc_1_input_2,
%   MR_Word * tscc_output_1)
% {
%   MR_Word tscc_proc_2_input_1;
%   MR_Word tscc_proc_2_input_2;
%   MR_Word tscc_proc_2_input_3;
% 
%   goto top_of_proc_1;
% top_of_proc_1:
%   {
%     MR_Word HeadVar1_AIn1 = tscc_proc_1_input_1;
%     MR_Word HeadVar2_AIn2 = tscc_proc_1_input_2;
%     MR_Word * HeadVar2_AOut1 = tscc_output_1;
%     ...
%     ... the code of a/3, in which tail recursive calls to a/3 are done as
%     ... assignments to tscc_proc_1_input_{1,2} and "goto top_of_proc_1",
%     ... while tail recursive calls to b/4 are done as
%     ... assignments to tscc_proc_2_input_{1,2,3} and "goto top_of_proc_2".
%     ...
%     return;
%   }
% top_of_proc_2:
%   {
%     MR_Word HeadVar1_BIn1 = tscc_proc_1_input_1;
%     MR_Word HeadVar2_BIn2 = tscc_proc_1_input_2;
%     MR_Word HeadVar2_BIn3 = tscc_proc_1_input_3;
%     MR_Word * HeadVar2_BOut1 = tscc_output_1;
%     ...
%     ... the code of b/4, in which tail recursive calls are done the same way.
%     ...
%     return;
%   }
% }
% 
% b_4(
%   MR_Word tscc_proc_2_input_1,
%   MR_Word tscc_proc_2_input_2,
%   MR_Word tscc_proc_2_input_3,
%   MR_Word * tscc_output_1)
% {
%   MR_Word tscc_proc_1_input_1;
%   MR_Word tscc_proc_1_input_2;
% 
%   goto top_of_proc_2;
%   ... the rest is the same as in a_3().
% }
%
% In general, if a TSCC has N procedures, and M of those N are called
% in any way other than tail calls from the TSCC (i.e. they are called
% from higher TSCCs/SCCs, or are called via non-tail calls in the TSCC),
% then we will generate M functions, each of which will contain the bodies
% of all N procedures in the TSCC (modulo the three caveats in the code below).
%
% If the target language does not support gotos, we can replace the use
% of labels and gotos with an infinite while loop whose body is a switch,
% with one arm for each procedure in the TSCC, in which a goto to a procedure
% is done by setting the selector variable (that the switch switches on)
% and executing "continue". (This relies on the fact that the code we generate
% for calls cannot be inside any loop *except* the infinite while loop
% added specifically for this purpose.)
%
% Our code generation strategy currently assumes that all outputs are
% returned by reference, which means we cannot yet handle mutually recursive
% det functions (since the return value is *not* returned by reference).
% Lifting this restriction is future work.
%
% Note that every argument of every procedure in the TSCC has two names:
% a "tscc" name, and an "own" name. The tscc name will be tscc_proc_x_input_y
% for input args and tscc_output_y for output args, so each procedure has
% its own separate set of input tscc args, but they all share the *same*
% output tscc args. The tscc names of all procedures in the TSCC are visible
% from all the procedure bodies included in the function, which is needed
% to allow all those procedure bodies to pass the parameters of tail
% recursive calls by assigning the actual input parameters to them.
%
% On the other hand, the *own* MLDS variables, which are generated from
% the corresponding HLDS variables in the usual fashion, are only ever visible
% from the block that contains the body of the relevant procedure; they are
% *never* visible from the MLDS code that implements the body of any *other*
% procedure in the TSCC. This is why we don't need to do any renaming apart
% of either HLDS or MLDS variables.

:- type tscc_code_model
    --->    tscc_det
    ;       tscc_semi.

:- pred ml_gen_tscc(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, set(pred_proc_id)::in, tscc_code_model::in,
    scc_with_entry_points::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_tscc(ModuleInfo, Target, ConstStructMap, _SCCEntryPredProcIds,
        TsccCodeModel, TSCCE, !FuncDefns, !GlobalData, !Specs) :-
    TSCCE = scc_with_entry_points(PredProcIds, _CalledFromHigherTSCCs,
        _ExportedTSCCPredProcIds),
    PredProcIdList = set.to_sorted_list(PredProcIds),
    (
        PredProcIdList = [],
        unexpected($pred, "empty TSCC")
    ;
        PredProcIdList = [SinglePredProcId],
        % For a TSCC containing just one procedure, we neither need nor want
        % the extra overhead required for managing *mutual* tail recursion.
        ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap, self_tail_rec,
            SinglePredProcId, !FuncDefns, !GlobalData, !Specs)
    ;
        PredProcIdList = [_, _ | _],
        % Try to compile each procedure in the TSCC into the MLDS code
        % fragment that will go under "top_of_proc_i" for each i.
        %
        % Caveat 1:
        % By definition, every procedure in the TSCC has at least one
        % mutually-tail-recursive call to it from another procedure
        % in the TSCC. However, the TSCC is computed from the HLDS.
        % Some calls that look like tail calls in the HLDS turn out
        % *not* to be tail calls in the MLDS, because they pass as input
        % arguments the addresses of local variables in the caller's stack
        % frame, addresses that would become dangling references if the
        % call were made a tail call. If it turns out that *none* of the
        % mutually-tail-recursive calls in the HLDS to a procedure
        % turn out to be mutually-tail-recursive calls in the MLDS,
        % then including that procedure in the scheme we use above
        % would be a waste; it would just incur overhead for no gain
        % in either stack usage or speed. ml_gen_tscc_trial will return
        % the identities of such procedures in NoMutualPredProcIds.
        % All the procedures in MutualPredProcIds will have mutually-tail-
        % recursive calls to them in the *M*LDS. (Therefore MutualPredProcIds
        % may have 0 elements, or 2, or 3, or any other number except 1.)
        %
        % Caveat 2:
        % If the MLDS code we generated for any of MutualPredProcIds contained
        % any function definitions nested inside of them, then we have
        % a problem. The scheme shown above would create M copies of each
        % such nested function definition, with M > 1. In the usual case of
        % us generating non-nested code, ml_elim_nested.m would hoist out
        % such nested definitions M times, leading to the M copies of the
        % hoisted-out definitions. Therefore, until we teach ml_elim_nested.m
        % not to do that, we abandon the optimization of mutually recursive
        % tail calls in the presence of such nested function definitions.
        ml_gen_tscc_trial(ModuleInfo, Target, ConstStructMap,
            TsccCodeModel, PredProcIds, _NonTailEntryPredProcIds,
            NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
            MutualContainsNestedFuncs, MutualEnvVarNames,
            MutualClosureWrapperFuncDefns, MutualTailRecSpecs,
            LoopKind, !.GlobalData, TrialGlobalData),
        (
            MutualContainsNestedFuncs = contains_nested_funcs,
            OutsideTsccPredProcIds = PredProcIds
        ;
            MutualContainsNestedFuncs = does_not_contain_nested_funcs,
            OutsideTsccPredProcIds = NoMutualPredProcIds,
            !:Specs = MutualTailRecSpecs ++ !.Specs,
            !:GlobalData = TrialGlobalData,

            % Caveat 3:
            % XXX We SHOULD add to TSCCEntryPredProcIds the procedures in
            % PredProcIds that are called from the procedures in SCC that are
            % NOT in this TSCC. We do that for procedures that are in the
            % SAME SET of TSCCs, but that leaves out the procedures that are
            % in the OTHER set of TSCCs (one model_det, one model_semi),
            % and the procedures that are not candidates for mutual tail
            % recursion optimization. Until we fix that, we can't intersect
            % MutualPredProcIds with MutualEntryPredProcIds before passing
            % its list form to list.map below.
            %
            % This means that the commented-out code below calculating
            % the various kinds of entry procedures can't (yet) be used.
%           TSCCEntryPredProcIds =
%               set.union(CalledFromHigherTSCCs, ExportedTSCCPredProcIds),
%           SCCEntryPredProcIdsInTSCC =
%               set.intersect(PredProcIds, SCCEntryPredProcIds),
%           ExternalEntryPredProcIds =
%               set.union(SCCEntryPredProcIdsInTSCC, TSCCEntryPredProcIds),
%    
%           MutualEntryPredProcIds =
%               set.difference(
%                   set.union(ExternalEntryPredProcIds,
%                       NonTailEntryPredProcIds),
%                   NoMutualPredProcIds),
%           some [!StartCommentStmts] (
%               !:StartCommentStmts = [],
%               describe_pred_proc_ids(ModuleInfo,
%                   "TSCC PredProcIds", PredProcIds,
%                   !StartCommentStmts),
%               describe_pred_proc_ids(ModuleInfo,
%                   "TSCCEntryPredProcIds", TSCCEntryPredProcIds,
%                   !StartCommentStmts),
%               describe_pred_proc_ids(ModuleInfo,
%                   "ExternalEntryPredProcIds", ExternalEntryPredProcIds,
%                   !StartCommentStmts),
%               describe_pred_proc_ids(ModuleInfo,
%                   "NonTailEntryPredProcIds", NonTailEntryPredProcIds,
%                   !StartCommentStmts),
%               describe_pred_proc_ids(ModuleInfo,
%                   "MutualEntryPredProcIds", MutualEntryPredProcIds,
%                   !StartCommentStmts),
%               BlankStmt = ml_stmt_atomic(comment(""), term.context_init),
%               !:StartCommentStmts = !.StartCommentStmts ++ [BlankStmt],
%               StartCommentStmts = !.StartCommentStmts
%           ),
            StartCommentStmts = [],

            list.map(
                construct_tscc_entry_proc(ModuleInfo, LoopKind, TsccCodeModel,
                    MutualPredProcCodes, MutualEnvVarNames, StartCommentStmts),
                set.to_sorted_list(MutualPredProcIds), TSCCFuncDefns),
            !:FuncDefns = MutualClosureWrapperFuncDefns ++ TSCCFuncDefns ++
                !.FuncDefns
        ),

        list.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            set.to_sorted_list(OutsideTsccPredProcIds),
            !FuncDefns, !GlobalData, !Specs)
    ).

    % Generate a representation of the codes to go under each "top_of_proc_i"
    % label in the scheme above, but also return the information our caller
    % needs to prepare for handling caveats 1 and 2.
    %
:- pred ml_gen_tscc_trial(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, tscc_code_model::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::out, set(pred_proc_id)::out, list(pred_proc_code)::out,
    maybe_contains_nested_funcs::out, set(string)::out,
    list(mlds_function_defn)::out, list(error_spec)::out,
    tail_rec_loop_kind::out, ml_global_data::in, ml_global_data::out) is det.

ml_gen_tscc_trial(ModuleInfo, Target, ConstStructMap, TsccCodeModel,
        PredProcIds, NonTailEntryPredProcIds,
        NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
        MutualContainsNestedFuncs, MutualEnvVarNames,
        MutualClosureWrapperFuncDefns, MutualTailRecSpecs,
        LoopKind, !GlobalData) :-
    % Compute the information we need for generating tail calls
    % to any of the procedures in the TSCC.
    list.map_foldl4(compute_initial_tail_rec_map_for_mutual(ModuleInfo),
        set.to_sorted_list(PredProcIds), PredProcIdArgsInfos,
        1, _, no, _MaybeNumOutputs,
        map.init, _OutArgNames, map.init, TailRecMap0),

    % Translate each procedure in the TSCC into a representation of the
    % code that will go under "top_of_proc_i".
    TsccInfo0 = init_ml_gen_tscc_info(ModuleInfo, TailRecMap0,
        tscc_self_and_mutual_rec),
    list.map_foldl2(
        ml_gen_tscc_proc_code(ModuleInfo, Target, ConstStructMap,
            TsccCodeModel),
        PredProcIdArgsInfos, PredProcCodes,
        !GlobalData, TsccInfo0, TsccInfo),

    TailRecInfo = TsccInfo ^ mgti_tail_rec_info,
    TargetMap = TailRecInfo ^ tri_target_map,
    LoopKind = TailRecInfo ^ tri_loop_kind,
    map.foldl2(accumulate_entry_procs, TargetMap,
        set.init, NonTailEntryPredProcIds, set.init, NoMutualPredProcIds0),
    % Prepare for caveats 1 and 2.
    separate_mutually_recursive_procs(NoMutualPredProcIds0, PredProcCodes,
        NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
        MutualContainsNestedFuncs, MutualClosureWrapperFuncDefns,
        MutualEnvVarNames, MutualTailRecSpecs).

:- pred accumulate_entry_procs(pred_proc_id::in, tail_rec_target_info::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

accumulate_entry_procs(PredProcId, TargetInfo,
        !NonTailEntryPredProcIds, !NoMutualTailRecPredProcIds) :-
    HaveDoneNonTailRec = TargetInfo ^ trti_done_nontail_rec,
    (
        HaveDoneNonTailRec = have_not_done_nontail_rec
    ;
        HaveDoneNonTailRec = have_done_nontail_rec,
        set.insert(PredProcId, !NonTailEntryPredProcIds)
    ),
    HaveDoneMutualTailRec = TargetInfo ^ trti_done_mutual_tail_rec,
    (
        HaveDoneMutualTailRec = have_not_done_mutual_tail_rec,
        set.insert(PredProcId, !NoMutualTailRecPredProcIds)
    ;
        HaveDoneMutualTailRec = have_done_mutual_tail_rec
    ).

    % separate_mutually_recursive_procs(NoMutualTailRecProcs, PredProcCodes,
    %     NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
    %     MutualContainsNestedFuncs, MutualClosureWrapperFuncDefns,
    %     MutualEnvVarNames, MutualTailRecSpecs):
    %
    % Given a list of procedures we have generated code for (PredProcCodes),
    % divide the procedures in it into two partitions: those whose ids
    % occur in NoMutualTailRecProcs, whose ids are returned in
    % NoMutualPredProcIds, and those whose ids do not occur there
    % whose ids are returned in MutualPredProcIds, and whose other info
    % is returned in the other output arguments whose names start with Mutual.
    %
:- pred separate_mutually_recursive_procs(set(pred_proc_id)::in,
    list(pred_proc_code)::in,
    set(pred_proc_id)::out, set(pred_proc_id)::out, list(pred_proc_code)::out,
    maybe_contains_nested_funcs::out, list(mlds_function_defn)::out,
    set(string)::out, list(error_spec)::out) is det.

separate_mutually_recursive_procs(_NoMutualTailRecProcs, [],
        set.init, set.init, [], does_not_contain_nested_funcs, [],
        set.init, []).
separate_mutually_recursive_procs(NoMutualTailRecProcs,
        [PredProcCode | PredProcCodes],
        !:NoMutualPredProcIds, !:MutualPredProcIds, !:MutualPredProcCodes,
        !:MutualContainsNestedFuncs, !:MutualClosureWrapperFuncDefns,
        !:MutualEnvVarNames, !:MutualTailRecSpecs) :-
    separate_mutually_recursive_procs(NoMutualTailRecProcs, PredProcCodes,
        !:NoMutualPredProcIds, !:MutualPredProcIds, !:MutualPredProcCodes,
        !:MutualContainsNestedFuncs, !:MutualClosureWrapperFuncDefns,
        !:MutualEnvVarNames, !:MutualTailRecSpecs),
    PredProcCode = pred_proc_code(PredProcIdArgsInfo,
        _FuncParams, _LocalVarDefns, FuncDefns, DescCommentStmt, GoalStmts,
        ProcClosureWrapperFuncDefns, ProcEnvVarNames, ProcTailRecSpecs),
    PredProcId = PredProcIdArgsInfo ^ ppiai_pred_proc_id,
    ( if set.member(PredProcId, NoMutualTailRecProcs) then
        set.insert(PredProcId, !NoMutualPredProcIds)
    else
        ( if
            (
                FuncDefns = [_ | _]
            ;
                list.foldl(does_stmt_contain_nested_func_defn,
                    [DescCommentStmt | GoalStmts],
                    does_not_contain_nested_funcs, ContainsNestedFuncs),
                ContainsNestedFuncs = contains_nested_funcs
            )
        then
            !:MutualContainsNestedFuncs = contains_nested_funcs
        else
            true
        ),
        set.insert(PredProcId, !MutualPredProcIds),
        !:MutualPredProcCodes = [PredProcCode | !.MutualPredProcCodes],
        !:MutualClosureWrapperFuncDefns =
            ProcClosureWrapperFuncDefns ++ !.MutualClosureWrapperFuncDefns,
        set.union(ProcEnvVarNames, !MutualEnvVarNames),
        !:MutualTailRecSpecs = ProcTailRecSpecs ++ !.MutualTailRecSpecs
    ).

    % The pred_proc_id_args_info structure contains the information we generate
    % about a procedure's arguments. We set this up *before* we generate code
    % for any procedure in the TSCC, because any procedure may contain tail
    % recursive calls to any other procedure in the TSCC, and to generate
    % the right code for such tail recursive calls, we need to know the
    % sequence of the *input* arguments of the callee.
    %
    % That sequence is not stored here; since it is needed *only* for
    % generating code for tail calls, it is stored in the data structure
    % used for that purpose (the tail_rec_info slot of the ml_gen_info).
    % However, when we compute this sequence, it is convenient to compute,
    % at the same time, all the *other* information we will need later
    % about the procedure's arguments, and that is what is stored here.
:- type pred_proc_id_args_info
    --->    pred_proc_id_args_info(
                % Various aspects of the identity of this procedure.
                ppiai_pred_proc_id              :: pred_proc_id,
                ppiai_pred_info                 :: pred_info,
                ppiai_proc_info                 :: proc_info,
                ppiai_proc_context              :: prog_context,
                ppiai_id_in_tscc                :: proc_id_in_tscc,

                % The variable names of the procedure's arguments,
                % their types and modes.
                ppiai_head_vars                 :: list(prog_var),
                ppiai_head_var_types            :: list(mer_type),
                ppiai_head_var_modes            :: list(top_functor_mode),

                % Local variable definitions of the tscc variables
                % for the input arguments of the procedure only.
                ppiai_tscc_in_local_var_defns   :: list(mlds_local_var_defn),

                % Argument definitions for the TSCC variables for all
                % the arguments of the procedure, both input and output,
                % in their original order.
                ppiai_tscc_args                 :: list(mlds_argument),

                % The tscc variables for the input arguments of procedure i
                % in the TSCC will be defined
                %
                % - in the argument list of the MLDS function,
                %   if the MLDS function is for procedure j with i = j; and
                %
                % - at the top of the body of the MLDS function,
                %   if the MLDS function is for procedure j with i != j.
                %
                % The tscc variables for the output arguments will always
                % be defined in the argument list of the MLDS function.

                % Local variable definitions of the own variables
                % for all the arguments of the procedure. These go at the
                % top of the block that implements the body this procedure.
                ppiai_own_local_var_defns       :: list(mlds_local_var_defn),

                % Statements that, for each argument of the procedure,
                % both input and output, assign the tscc variable for
                % that argument to the two variable for that argument.
                % These go at the top of the block that implements
                % the body this procedure. (The big example above merges
                % these assignments statements into the local variable
                % definitions as initializations, but this merging is
                % actually done later, by ml_optimize.m.)
                ppiai_tscc_to_own_copy_stmts    :: list(mlds_stmt)
            ).

    % Compute the map that tells the code generator how to translate
    % tail recursive calls to any procedure in the TSCC, and return
    % the other information about the argument lists of the procedures in the
    % TSCC that is convenient to compute at the same time.
    %
:- pred compute_initial_tail_rec_map_for_mutual(module_info::in,
    pred_proc_id::in, pred_proc_id_args_info::out,
    int::in, int::out, maybe(int)::in, maybe(int)::out,
    map(int, string)::in, map(int, string)::out,
    tail_rec_target_map::in, tail_rec_target_map::out) is det.

compute_initial_tail_rec_map_for_mutual(ModuleInfo,
        PredProcId, PredProcIdArgsInfo,
        !ProcNum, !MaybeNumOutputs, !OutArgNames, !TailRecMap0) :-
    ThisProcNum = !.ProcNum,
    IdInTscc = proc_id_in_tscc(ThisProcNum),
    !:ProcNum = !.ProcNum + 1,

    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    pred_info_get_arg_types(PredInfo, HeadTypes),
    proc_info_get_argmodes(ProcInfo, HeadModes),
    modes_to_top_functor_modes(ModuleInfo, HeadModes, HeadTypes,
        TopFunctorModes),
    proc_info_get_varset(ProcInfo, VarSet),
    HeadVarNames = ml_gen_local_var_names(VarSet, HeadVars),

    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    ProcContext = goal_info_get_context(GoalInfo),

    ml_gen_tscc_arg_decls(ModuleInfo, HeadVarNames, HeadTypes, TopFunctorModes,
        VarSet, ProcContext, IdInTscc, 1, 1, NumOutputs, !OutArgNames,
        TsccInArgs, TsccInLocalVarDefns, TsccArgs, OwnLocalVarDefns,
        CopyTsccToOwnStmts),
    (
        !.MaybeNumOutputs = no,
        !:MaybeNumOutputs = yes(NumOutputs)
    ;
        !.MaybeNumOutputs = yes(OldNumOutputs),
        expect(unify(OldNumOutputs, NumOutputs), $pred,
            "different procedures in TSCC have different number of outputs")
    ),
    PredProcIdArgsInfo = pred_proc_id_args_info(PredProcId, PredInfo, ProcInfo,
        ProcContext, IdInTscc, HeadVars, HeadTypes, TopFunctorModes,
        TsccInLocalVarDefns, TsccArgs, OwnLocalVarDefns, CopyTsccToOwnStmts),
    TailRecTargetInfo0 = tail_rec_target_info(IdInTscc, TsccInArgs,
        have_not_done_self_tail_rec, have_not_done_mutual_tail_rec,
        have_not_done_nontail_rec),
    map.det_insert(PredProcId, TailRecTargetInfo0, !TailRecMap0).

    % Each value of this type records the results of invoking the code
    % generator on the body of procedure in a TSCC.
    %
:- type pred_proc_code
    --->    pred_proc_code(
                ppc_id_args_info            :: pred_proc_id_args_info,
                ppc_func_params             :: mlds_func_params,
                ppc_local_var_defns         :: list(mlds_local_var_defn),
                ppc_local_func_defns        :: list(mlds_function_defn),
                ppc_desc_comment_stmt       :: mlds_stmt,
                ppc_goal_stmts              :: list(mlds_stmt),
                ppc_closure_wrapper_funcs   :: list(mlds_function_defn),
                ppc_env_var_names           :: set(string),
                ppc_tail_rec_specs          :: list(error_spec)
            ).

    % Translate the body of the given procedure to MLDS, and return the
    % results in a form that our caller can join together with the MLDS code
    % we get for the *other* procedures in the TSCC.
    %
:- pred ml_gen_tscc_proc_code(module_info::in,
    mlds_target_lang::in, ml_const_struct_map::in, tscc_code_model::in,
    pred_proc_id_args_info::in, pred_proc_code::out,
    ml_global_data::in, ml_global_data::out,
    ml_gen_tscc_info::in, ml_gen_tscc_info::out) is det.

ml_gen_tscc_proc_code(ModuleInfo, Target, ConstStructMap, TsccCodeModel,
        PredProcIdArgsInfo, PredProcCode, !GlobalData, !TsccInfo) :-
    PredProcIdArgsInfo = pred_proc_id_args_info(PredProcId, PredInfo, ProcInfo,
        ProcContext, ProcIdInTscc, HeadVars, HeadTypes, TopFunctorModes,
        _TsscInLocalVarDefns, _TsccArgs, _OwnLocalVarDefns,
        _CopyTsccToOwnStmts),

    trace [io(!IO)] (
        write_proc_progress_message("% Generating in-TSCC MLDS code for ",
            PredProcId, ModuleInfo, !IO)
    ),

    some [!Info] (
        % We initialize the tri_msgs field to be the empty before starting
        % to generate code for a procedure, and have finished generating that
        % code and we have picked the list of accumulated messages, we reset
        % the field to empty again. Therefore the field should always be empty
        % *between* procedures.
        TailRecInfo0 = !.TsccInfo ^ mgti_tail_rec_info,
        TailRecSpecs0 = TailRecInfo0 ^ tri_msgs,
        expect(unify(TailRecSpecs0, []), $pred, "TailRecSpecs0 != []"),

        !:Info = ml_gen_info_init(ModuleInfo, Target, ConstStructMap,
            PredProcId, ProcInfo, !.GlobalData, !.TsccInfo),

        pred_info_get_status(PredInfo, PredStatus),
        ( if PredStatus = pred_status(status_external(_)) then
            % External predicates don't have bodies, so they can't contain
            % tail calls (or any other kind of calls), so they cannot be
            % in a non-singleton TSCC.
            unexpected($pred, "status_external")
        else
            true
        ),

        PredProcId = proc(_PredId, ProcId),
        ProcDesc = describe_proc(PredInfo, ProcId),
        ProcIdInTscc = proc_id_in_tscc(ProcNumInTscc),
        ProcDescComment = string.format("proc %d in TSCC: %s",
            [i(ProcNumInTscc), s(ProcDesc)]),
        CommentStmt = ml_stmt_atomic(comment(ProcDescComment), ProcContext),

        ml_gen_info_proc_params(PredProcId, FuncParams,
            ByRefOutputVars, CopiedOutputVars, !Info),
        ml_gen_info_set_byref_output_vars(ByRefOutputVars, !Info),
        % For now, the code we generate returns all output variables
        % by reference. Our ancestors should ensure we get here only
        % the target platform does not use copy-out parameter passing,
        % and that no procedure in the TSCC has a return value.
        % XXX We should generalize this code so that we *can* handle
        % copy-out parameter passing.
        expect(unify(CopiedOutputVars, []), $pred, "CopiedOutputVars != []"),

        ( TsccCodeModel = tscc_det, CodeModel = model_det
        ; TsccCodeModel = tscc_semi, CodeModel = model_semi
        ),
        proc_info_get_goal(ProcInfo, Goal),
        ml_gen_proc_body(CodeModel, sot_tscc, HeadVars, HeadTypes,
            TopFunctorModes, CopiedOutputVars, Goal,
            LocalVarDefns0, FuncDefns, GoalStmts, !Info),
        ml_gen_post_process_locals(ProcInfo, ProcContext,
            HeadVars, HeadTypes,
            CopiedOutputVars, LocalVarDefns0, LocalVarDefns, !Info),

        ml_gen_info_final(!.Info, EnvVarNames,
            ClosureWrapperFuncDefns, !:GlobalData, !:TsccInfo),

        TailRecInfo1 = !.TsccInfo ^ mgti_tail_rec_info,
        TailRecSpecs = TailRecInfo1 ^ tri_msgs,
        TailRecInfo = TailRecInfo1 ^ tri_msgs := [],
        !TsccInfo ^ mgti_tail_rec_info := TailRecInfo,

        PredProcCode = pred_proc_code(PredProcIdArgsInfo, FuncParams,
            LocalVarDefns, FuncDefns, CommentStmt, GoalStmts,
            ClosureWrapperFuncDefns, EnvVarNames, TailRecSpecs)
    ).

    % Given the results of translating each procedure in a TSCC into MLDS code,
    % wrap them up in an MLDS function that implements EntryProc.
    %
:- pred construct_tscc_entry_proc(module_info::in, tail_rec_loop_kind::in,
    tscc_code_model::in, list(pred_proc_code)::in, set(string)::in,
    list(mlds_stmt)::in, pred_proc_id::in, mlds_function_defn::out) is det.

construct_tscc_entry_proc(ModuleInfo, LoopKind, TsccCodeModel, PredProcCodes,
        EnvVarNames, EntryProcDescComments, EntryProc, FuncDefn) :-
    trace [io(!IO)] (
        write_proc_progress_message("% Generating MLDS code for ",
            EntryProc, ModuleInfo, !IO)
    ),

    list.map_foldl2(construct_func_body_for_tscc(EntryProc), PredProcCodes,
        ProcStmtInfos, no, MaybeEntryProcInfo, [], TsccInLocalDefns),
    (
        MaybeEntryProcInfo = no,
        unexpected($pred, "MaybeEntryProcInfo = no")
    ;
        MaybeEntryProcInfo = yes(EntryProcInfo),
        EntryProcInfo =
            entry_proc_info(EntryIdInTscc, EntryPredProcIdInfo, EntryProcArgs)
    ),
    (
        TsccCodeModel = tscc_det,
        ReturnArgTypes = []
    ;
        TsccCodeModel = tscc_semi,
        ReturnArgTypes = [mlds_native_bool_type]
    ),
    EntryProcParams = mlds_func_params(EntryProcArgs, ReturnArgTypes),
    EntryPredProcIdInfo = pred_proc_id_info(_EntryPredProcId,
        _EntryPredInfo, _EntryProcInfo, EntryProcContext),
    wrap_proc_stmts(LoopKind, EntryIdInTscc, EntryProcContext,
        ProcStmtInfos, SelectorVarDefns, Stmts),
    LocalVarDefns = SelectorVarDefns ++ TsccInLocalDefns,

    EntryIdInTscc = proc_id_in_tscc(EntryIdInTsccNum),
    EntryProcDesc = describe_proc_from_id(ModuleInfo, EntryProc),
    Comment0 = string.format("The code for TSCC PROC %d: %s",
        [i(EntryIdInTsccNum), s(EntryProcDesc)]),
    CommentStmt0 = ml_stmt_atomic(comment(Comment0), EntryProcContext),
    Comment1 = "Setup for mutual tailcalls optimized into a loop.",
    CommentStmt1 = ml_stmt_atomic(comment(Comment1), EntryProcContext),
    Comment2 = "The mutually recursive procedures are:",
    CommentStmt2 = ml_stmt_atomic(comment(Comment2), EntryProcContext),
    EmptyComment = comment(""),
    EmptyCommentStmt = ml_stmt_atomic(EmptyComment, EntryProcContext),
    ProcDescCommentStmts =
        list.map(func(PPC) = PPC ^ ppc_desc_comment_stmt, PredProcCodes),

    FuncBodyStmts =
        [CommentStmt0, CommentStmt1, CommentStmt2, EmptyCommentStmt
            | ProcDescCommentStmts]
        ++ [EmptyCommentStmt | EntryProcDescComments] ++ Stmts,

    FuncBodyStmt = ml_stmt_block(LocalVarDefns, [],
        FuncBodyStmts, EntryProcContext),
    FuncBody = body_defined_here(FuncBodyStmt),
    construct_func_defn(ModuleInfo, EntryPredProcIdInfo, EntryProcParams,
        FuncBody, EnvVarNames, FuncDefn).

:- type entry_proc_info
    --->    entry_proc_info(
                proc_id_in_tscc,
                pred_proc_id_info,
                list(mlds_argument)
            ).

:- type proc_stmt_info
    --->    proc_stmt_info(
                proc_id_in_tscc,
                mlds_stmt,
                prog_context
            ).

    % Given PredProcCode, the results of translating procedure PredProcId
    % in a TSCC into MLDS code, stitch these results together in a way
    % that is appropriate to go under the "top_of_proc_i" for PredProcId
    % in the MLDS function that implements EntryProc.
    %
    % Some aspects of the appropriate form depend on whether PredProcId =
    % EntryProc, so we cannot put the same code under "top_of_proc_i"
    % in *all* the MLDS functions we generate for a TSCC.
    %
:- pred construct_func_body_for_tscc(pred_proc_id::in, pred_proc_code::in,
    proc_stmt_info::out,
    maybe(entry_proc_info)::in, maybe(entry_proc_info)::out,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out) is det.

construct_func_body_for_tscc(EntryProc, PredProcCode, ProcStmtInfo,
        !MaybeEntryProcInfo, !LocalVarDefns) :-
    PredProcCode = pred_proc_code(PredProcIdArgsInfo, _FuncParams,
        GoalLocalVarDefns, GoalFuncDefns, _DescCommentStmt, GoalStmts,
        _ClosureWrapperFuncDefns, _EnvVarNames, _TailRecSpecs),
    PredProcIdArgsInfo = pred_proc_id_args_info(PredProcId,
        PredInfo, ProcInfo, ProcContext, IdInTscc,
        _HeadVars, _HeadTypes, _TopFunctorModes,
        TsccInLocalVarDefns, TsccArgs, OwnLocalVarDefns, CopyTsccToOwnStmts),
    ( if PredProcId = EntryProc then
        expect(unify(!.MaybeEntryProcInfo, no), $pred,
            "!.MaybeEntryProcInfo != no"),
        PredProcIdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo,
            ProcContext),
        EntryProcInfo = entry_proc_info(IdInTscc, PredProcIdInfo, TsccArgs),
        !:MaybeEntryProcInfo = yes(EntryProcInfo)
    else
        !:LocalVarDefns = !.LocalVarDefns ++ TsccInLocalVarDefns
    ),
    ProcStmt = ml_stmt_block(OwnLocalVarDefns ++ GoalLocalVarDefns,
        GoalFuncDefns, CopyTsccToOwnStmts ++ GoalStmts, ProcContext),
    ProcStmtInfo = proc_stmt_info(IdInTscc, ProcStmt, ProcContext).

%---------------------%

    % Take the codes that go under each "top_of_proc_i", and join them
    % together to yield the body of the MLDS function for EntryProc.
    % Use whiles and continues instead of labels and gotos if requested.
    %
:- pred wrap_proc_stmts(tail_rec_loop_kind::in,
    proc_id_in_tscc::in, prog_context::in, list(proc_stmt_info)::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out) is det.

wrap_proc_stmts(LoopKind, EntryProc, EntryProcContext,
        ProcStmtInfos, SelectorVarDefns, Stmts) :-
    (
        LoopKind = tail_rec_loop_label_goto,
        wrap_proc_stmts_label_goto(EntryProc, EntryProcContext,
            ProcStmtInfos, Stmts),
        SelectorVarDefns = []
    ;
        LoopKind = tail_rec_loop_while_continue,
        wrap_proc_stmts_while_continue(EntryProc, EntryProcContext,
            ProcStmtInfos, SelectorVarDefn, Stmts),
        SelectorVarDefns = [SelectorVarDefn]
    ).

%---------------------%

    % We wrap the statements we generate for each TSCC procedure like this:
    %
    % goto top_of_proc_<entry_proc>;
    %
    % top_of_proc_1:
    %   <copy tscc args 1 to own args 1>
    %   <body of TSCC proc 1>
    %   return;
    % ...
    % top_of_proc_N:
    %   <copy tscc args N to own args N>
    %   <body of TSCC proc N>
    %   return;
    % end_of_tscc:
    %
    % A tail call to TSCC proc i can just
    %
    % - assign the actual parameters to tscc args i, and
    % - goto `top_of_proc_i'.
    %
:- pred wrap_proc_stmts_label_goto(proc_id_in_tscc::in, prog_context::in,
    list(proc_stmt_info)::in, list(mlds_stmt)::out) is det.

wrap_proc_stmts_label_goto(EntryProc, EntryProcContext, ProcStmtInfos,
        WrappedStmts) :-
    list.map(prefix_proc_stmt_with_start_label, ProcStmtInfos,
        ProcWrappedStmtLists),
    list.condense(ProcWrappedStmtLists, ProcWrappedStmts),
    EntryStartLabel =
        generate_tail_rec_start_label(tscc_self_and_mutual_rec, EntryProc),
    GotoEntryStmt =
        ml_stmt_goto(goto_label(EntryStartLabel), EntryProcContext),
    WrappedStmts = [GotoEntryStmt | ProcWrappedStmts].

:- pred prefix_proc_stmt_with_start_label(proc_stmt_info::in,
    list(mlds_stmt)::out) is det.

prefix_proc_stmt_with_start_label(ProcStmtInfo, LabelProcStmts) :-
    ProcStmtInfo = proc_stmt_info(IdInTscc, ProcStmt, ProcContext),
    StartLabel =
        generate_tail_rec_start_label(tscc_self_and_mutual_rec, IdInTscc),
    StartLabelStmt = ml_stmt_label(StartLabel, ProcContext),
    LabelProcStmts = [StartLabelStmt, ProcStmt].

%---------------------%

    % We wrap the statements we generate for each TSCC procedure like this:
    %
    % proc_selector = <entry_proc>;;
    % while (true) {
    %   switch (proc_selector) {
    %       case 1:
    %           <copy tscc args 1 to own args 1>
    %           <body of TSCC proc 1>
    %           return;
    %       ...
    %       case N:
    %           <copy tscc args N to own args N>
    %           <body of TSCC proc N>
    %           return;
    %   }
    % }
    %
    % A tail call to TSCC proc i can just
    %
    % - assign the actual parameters to tscc args i,
    % - set proc_selector to i, and
    % - execute `continue'.
    %
:- pred wrap_proc_stmts_while_continue(proc_id_in_tscc::in, prog_context::in,
    list(proc_stmt_info)::in,
    mlds_local_var_defn::out, list(mlds_stmt)::out) is det.

wrap_proc_stmts_while_continue(EntryProc, ProcContext, ProcStmtInfos,
        SelectorVarDefn, WrappedStmts) :-
    list.map_foldl(prefix_proc_stmt_with_switch_cond,
        ProcStmtInfos, SwitchCases, set.init, PossibleSwitchValues),

    SelectorVar = lvn_comp_var(lvnc_tscc_proc_selector),
    SelectorType = mlds_native_int_type,
    SelectorVarDefn = mlds_local_var_defn(SelectorVar, ProcContext,
        SelectorType, no_initializer, gc_no_stmt),

    EntryProc = proc_id_in_tscc(EntryProcNum),
    SelectorVarLval = ml_local_var(SelectorVar, SelectorType),
    SetSelectorStmt = ml_stmt_atomic(
        assign(SelectorVarLval, ml_const(mlconst_int(EntryProcNum))),
        ProcContext),

    set.to_sorted_list(PossibleSwitchValues, PossibleSwitchValuesList),
    SwitchMin = list.det_head(PossibleSwitchValuesList),
    SwitchMax = list.det_last(PossibleSwitchValuesList),
    SwitchRange = mlds_switch_range(SwitchMin, SwitchMax),
    Default = default_is_unreachable,
    SwitchStmt = ml_stmt_switch(SelectorType, ml_lval(SelectorVarLval),
        SwitchRange, SwitchCases, Default, ProcContext),
    LoopStmt = ml_stmt_while(may_loop_zero_times,
        ml_const(mlconst_true), SwitchStmt, ProcContext),
    WrappedStmts = [SetSelectorStmt, LoopStmt].

:- pred prefix_proc_stmt_with_switch_cond(proc_stmt_info::in,
    mlds_switch_case::out, set(int)::in, set(int)::out) is det.

prefix_proc_stmt_with_switch_cond(ProcStmtInfo, SwitchCase,
        !PossibleSwitchValues) :-
    ProcStmtInfo = proc_stmt_info(IdInTscc, ProcStmt, _ProcContext),
    IdInTscc = proc_id_in_tscc(IdInTsccNum),
    MatchCond = match_value(ml_const(mlconst_int(IdInTsccNum))),
    SwitchCase = mlds_switch_case(MatchCond, [], ProcStmt),
    set.insert(IdInTsccNum, !PossibleSwitchValues).

%---------------------------------------------------------------------------%

:- func attribute_to_mlds_attribute(module_info, pred_attribute)
    = mlds_attribute.

attribute_to_mlds_attribute(ModuleInfo, custom(Type)) =
    custom(mercury_type_to_mlds_type(ModuleInfo, Type)).

    % Return the declaration flags appropriate for a procedure definition.
    %
:- func ml_gen_proc_decl_flags(module_info, pred_id, proc_id)
    = mlds_function_decl_flags.

ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId) = DeclFlags :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( if procedure_is_exported(ModuleInfo, PredInfo, ProcId) then
        Access = acc_public
    else
        Access = acc_private
    ),
    PerInstance = one_copy,
    DeclFlags = init_function_decl_flags(Access, PerInstance).

    % Generate the code for a procedure body.
    %
:- pred ml_gen_proc_body(code_model::in, solo_or_tscc::in, list(prog_var)::in,
    list(mer_type)::in, list(top_functor_mode)::in, list(prog_var)::in,
    hlds_goal::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_proc_body(CodeModel, SoloOrTscc, HeadVars, ArgTypes,
        TopFunctorModes, CopiedOutputVars, Goal, LocalVarDefns, FuncDefns,
        Stmts, !Info) :-
    Goal = hlds_goal(_, GoalInfo),
    Context = goal_info_get_context(GoalInfo),

    % First just generate the code for the procedure's goal.

    % In certain cases -- for example existentially typed procedures,
    % or unification/compare procedures for equivalence types --
    % the parameters types may not match the types of the head variables.
    % In such cases, we need to box/unbox/cast them to the right type.
    % We also grab the original (uncast) lvals for the copied output
    % variables (if any) here, since for the return statement that
    % we append below, we want the original vars, not their cast versions.

    ml_gen_var_list(!.Info, CopiedOutputVars, CopiedOutputVarOriginalLvals),
    ml_gen_convert_headvars(HeadVars, ArgTypes, TopFunctorModes,
        CopiedOutputVars, Context, ConvLocalVarDefns,
        ConvInputStmts, ConvOutputStmts, !Info),
    ( if
        ConvLocalVarDefns = [],
        ConvInputStmts = [],
        ConvOutputStmts = []
    then
        % No boxing/unboxing/casting required.
        ml_gen_goal(CodeModel, Goal, LocalVarDefns, FuncDefns, Stmts1, !Info)
    else
        DoGenGoal = ml_gen_goal(CodeModel, Goal),

        % Boxing/unboxing/casting required. We need to convert the input
        % arguments, generate the goal, convert the output arguments,
        % and then succeeed.
        DoConvOutputs =
            ( pred(NewLocalVarDefns::out, NewFuncDefns::out,
                    NewStmts::out, Info0::in, Info::out) is det :-
                ml_gen_success(CodeModel, Context, SuccStmts, Info0, Info),
                NewLocalVarDefns = [],
                NewFuncDefns = [],
                NewStmts = ConvOutputStmts ++ SuccStmts
            ),
        ml_combine_conj(CodeModel, Context, DoGenGoal, DoConvOutputs,
            LocalVarDefns0, FuncDefns0, Stmts0, !Info),
        Stmts1 = ConvInputStmts ++ Stmts0,
        LocalVarDefns = ConvLocalVarDefns ++ LocalVarDefns0,
        FuncDefns = FuncDefns0
    ),

    % Finally append an appropriate `return' statement, if needed.
    ml_append_return_statement(CodeModel, SoloOrTscc,
        CopiedOutputVarOriginalLvals, Context, Stmts1, Stmts, !Info).

    % In certain cases -- for example existentially typed procedures,
    % or unification/compare procedures for equivalence types --
    % the parameter types may not match the types of the head variables.
    % In such cases, we need to box/unbox/cast them to the right type.
    % This procedure handles that.
    %
:- pred ml_gen_convert_headvars(list(prog_var)::in, list(mer_type)::in,
    list(top_functor_mode)::in, list(prog_var)::in, prog_context::in,
    list(mlds_local_var_defn)::out,
    list(mlds_stmt)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_convert_headvars(Vars, HeadTypes, TopFunctorModes, CopiedOutputVars,
        Context, LocalVarDefns, InputStmts, OutputStmts, !Info) :-
    ( if
        Vars = [],
        HeadTypes = [],
        TopFunctorModes = []
    then
        LocalVarDefns = [],
        InputStmts = [],
        OutputStmts = []
    else if
        Vars = [Var | VarsTail],
        HeadTypes = [HeadType | HeadTypesTail],
        TopFunctorModes = [TopFunctorMode | TopFunctorModesTail]
    then
        ml_variable_type(!.Info, Var, BodyType),
        ( if
            % An argument doesn't need any conversion if ...
            (
                % ... its type is the same in the head as in the body
                % (modulo contexts), or ...
                map.init(Subst0),
                type_unify(HeadType, BodyType, [], Subst0, Subst),
                map.is_empty(Subst)
            ;
                % ... if it is unused.
                TopFunctorMode = top_unused
            )
        then
            ml_gen_convert_headvars(VarsTail, HeadTypesTail,
                TopFunctorModesTail, CopiedOutputVars, Context,
                LocalVarDefns, InputStmts, OutputStmts, !Info)
        else
            % Generate the lval for the head variable.
            ml_gen_var_with_type(!.Info, Var, HeadType, HeadVarLval),

            % Generate code to box or unbox that head variable,
            % to convert its type from HeadType to BodyType.
            ml_gen_info_get_varset(!.Info, VarSet),
            VarName = ml_gen_local_var_name(VarSet, Var),
            ml_gen_box_or_unbox_lval(HeadType, BodyType, bp_native_if_possible,
                HeadVarLval, VarName, Context, no, 0, BodyLval,
                ConvLocalVarDefns, ConvInputStmts, ConvOutputStmts, !Info),

            % Ensure that for any uses of this variable in the procedure body,
            % we use the BodyLval (which has type BodyType) rather than the
            % HeadVarLval (which has type HeadType).
            ml_gen_info_set_var_lval(Var, BodyLval, !Info),

            ml_gen_convert_headvars(VarsTail, HeadTypesTail,
                TopFunctorModesTail, CopiedOutputVars, Context,
                LocalVarDefnsTail, InputStmtsTail, OutputStmtsTail, !Info),

            % Add the code to convert this input or output.
            ml_gen_info_get_byref_output_vars(!.Info, ByRefOutputVars),
            ( if
                ( list.member(Var, ByRefOutputVars)
                ; list.member(Var, CopiedOutputVars)
                )
            then
                InputStmts = InputStmtsTail,
                OutputStmts = OutputStmtsTail ++ ConvOutputStmts
            else
                InputStmts = ConvInputStmts ++ InputStmtsTail,
                OutputStmts = OutputStmtsTail
            ),
            LocalVarDefns = ConvLocalVarDefns ++ LocalVarDefnsTail
        )
    else
        unexpected($pred, "length mismatch")
    ).

:- pred ml_gen_post_process_locals(proc_info::in, prog_context::in,
    list(prog_var)::in, list(mer_type)::in, list(prog_var)::in,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_post_process_locals(ProcInfo, Context, HeadVars, HeadTypes,
        CopiedOutputVars, LocalVarDefns0, LocalVarDefns, !Info) :-
    % This would generate all the local variables at the top of
    % the function:
    %   ml_gen_all_local_var_decls(Goal,
    %       VarSet, VarTypes, HeadVars, MLDS_LocalVars, !Info)
    % But instead we now generate them locally for each goal.
    % We just declare the `succeeded' var here, plus locals
    % for any output arguments that are returned by value
    % (e.g. if --nondet-copy-out is enabled, or for det function
    % return values).
    (
        CopiedOutputVars = [],
        % Optimize common case.
        OutputVarLocalDefns = []
    ;
        CopiedOutputVars = [_ | _],
        proc_info_get_varset(ProcInfo, VarSet),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        % Note that for headvars we must use the types from
        % the procedure interface, not from the procedure body.
        vartypes_overlay_corresponding_lists(HeadVars, HeadTypes,
            VarTypes, UpdatedVarTypes),
        ml_gen_local_var_decls(VarSet, UpdatedVarTypes,
            Context, CopiedOutputVars, OutputVarLocalDefns, !Info)
    ),
    ml_gen_info_get_used_succeeded_var(!.Info, UsedSucceededVar),
    (
        UsedSucceededVar = no,
        ProcLocalVarDefns = OutputVarLocalDefns
    ;
        UsedSucceededVar = yes,
        ProcLocalVarDefns = [ml_gen_succeeded_var_decl(Context) |
            OutputVarLocalDefns]
    ),
    LocalVarDefns = ProcLocalVarDefns ++ LocalVarDefns0.

%---------------------------------------------------------------------------%

:- type maybe_contains_nested_funcs
    --->    does_not_contain_nested_funcs
    ;       contains_nested_funcs.

:- pred does_stmt_contain_nested_func_defn(mlds_stmt::in,
    maybe_contains_nested_funcs::in, maybe_contains_nested_funcs::out) is det.

does_stmt_contain_nested_func_defn(Stmt, !ContainsNestedFuncs) :-
    (
        Stmt = ml_stmt_block(_LocalVarDefns, FuncDefns, SubStmts, _Context),
        (
            FuncDefns = [],
            list.foldl(does_stmt_contain_nested_func_defn, SubStmts,
                !ContainsNestedFuncs)
        ;
            FuncDefns = [_ | _],
            !:ContainsNestedFuncs = contains_nested_funcs
        )
    ;
        Stmt = ml_stmt_while(_LoopKind, _CondRval, SubStmt, _Context),
        does_stmt_contain_nested_func_defn(SubStmt, !ContainsNestedFuncs)
    ;
        Stmt = ml_stmt_if_then_else(_CondRval, ThenStmt, MaybeElseStmt,
            _Context),
        does_stmt_contain_nested_func_defn(ThenStmt, !ContainsNestedFuncs),
        (
            MaybeElseStmt = no
        ;
            MaybeElseStmt = yes(ElseStmt),
            does_stmt_contain_nested_func_defn(ElseStmt, !ContainsNestedFuncs)
        )
    ;
        Stmt = ml_stmt_switch(_Type, _Rval, _Range, Cases, Default, _Context),
        list.foldl(does_case_contain_nested_func_defn, Cases,
            !ContainsNestedFuncs),
        (
            Default = default_is_unreachable
        ;
            Default = default_do_nothing
        ;
            Default = default_case(DefaultStmt),
            does_stmt_contain_nested_func_defn(DefaultStmt,
                !ContainsNestedFuncs)
        )
    ;
        Stmt = ml_stmt_try_commit(_Lval, GoalStmt, HandlerStmt, _Context),
        does_stmt_contain_nested_func_defn(GoalStmt, !ContainsNestedFuncs),
        does_stmt_contain_nested_func_defn(HandlerStmt, !ContainsNestedFuncs)
    ;
        ( Stmt = ml_stmt_label(_Label, _Context)
        ; Stmt = ml_stmt_goto(_Target, _Context)
        ; Stmt = ml_stmt_computed_goto(_Rval, _Targets, _Context)
        ; Stmt = ml_stmt_call(_Sig, _Callee, _Args, _Ret, _Kind, _M, _Context)
        ; Stmt = ml_stmt_return(_RetVals, _Context)
        ; Stmt = ml_stmt_do_commit(_Rval, _Context)
        ; Stmt = ml_stmt_atomic(_Atomic, _Context)
        )
    ).

:- pred does_case_contain_nested_func_defn(mlds_switch_case::in,
    maybe_contains_nested_funcs::in, maybe_contains_nested_funcs::out) is det.

does_case_contain_nested_func_defn(Case, !ContainsNestedFuncs) :-
    Case = mlds_switch_case(_FirstCond, _LaterConds, Stmt),
    does_stmt_contain_nested_func_defn(Stmt, !ContainsNestedFuncs).

%---------------------------------------------------------------------------%

:- pred describe_pred_proc_ids(module_info::in, string::in,
    set(pred_proc_id)::in, list(mlds_stmt)::in, list(mlds_stmt)::out) is det.

describe_pred_proc_ids(ModuleInfo, Msg, PredProcIds, !StartCommentStmts) :-
    MsgStmt = ml_stmt_atomic(comment(Msg), term.context_init),
    DescStmts = list.map(pred_proc_id_desc(ModuleInfo),
        set.to_sorted_list(PredProcIds)),
    !:StartCommentStmts = !.StartCommentStmts ++ [MsgStmt | DescStmts].

:- func pred_proc_id_desc(module_info, pred_proc_id) = mlds_stmt.

pred_proc_id_desc(ModuleInfo, PredProcId) = DescStmt :-
    Comment = "  " ++ describe_proc_from_id(ModuleInfo, PredProcId),
    DescStmt = ml_stmt_atomic(comment(Comment), term.context_init).

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_proc_gen.
%---------------------------------------------------------------------------%
