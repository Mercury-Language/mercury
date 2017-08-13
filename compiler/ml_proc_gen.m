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
:- import_module ml_backend.ml_target_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.

:- import_module assoc_list.
:- import_module bool.
:- import_module map.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module set.

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
        OptTailCalls = tail_call_opt_in_code_gen
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

:- type maybe_tail_call_opt_in_code_gen
    --->    no_tail_call_opt_in_code_gen
    ;       tail_call_opt_in_code_gen.

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
    mlds_target_lang::in, ml_const_struct_map::in,
    scc_with_entry_points::in,
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
                no_tail_rec),
            PredProcIds, !FuncDefns, !GlobalData, !Specs)
    ;
        OptTailCalls = tail_call_opt_in_code_gen,
        partition_scc_procs(ModuleInfo, set.to_sorted_list(PredProcIds),
            NonePredProcIdInfos, SelfPredProcIdInfos, MutualPredProcIdInfos),

        % Translate the procedures cannot apply tail call optimization to.
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
        TSCCDepInfo = build_proc_dependency_graph(ModuleInfo,
            set.list_to_set(
                list.map(project_pred_proc_id_info_id, MutualPredProcIdInfos)),
            only_tail_calls),
        get_bottom_up_sccs_with_entry_points(ModuleInfo, TSCCDepInfo,
            TSCCEntries),
        partition_tsccs(TSCCEntries, LonePredProcIds, NonTrivialTSCCEntries),
        list.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            LonePredProcIds, !FuncDefns, !GlobalData, !Specs),
        list.foldl3(
            ml_gen_tscc(ModuleInfo, Target, ConstStructMap, SCCEntryProcs),
            NonTrivialTSCCEntries, !FuncDefns, !GlobalData, !Specs)
    ).

:- type pred_proc_id_info
    --->    pred_proc_id_info(
                pred_proc_id,
                pred_info,
                proc_info
            ).

:- func project_pred_proc_id_info_id(pred_proc_id_info) = pred_proc_id.

project_pred_proc_id_info_id(pred_proc_id_info(PredProcId, _, _)) = PredProcId.

:- pred partition_scc_procs(module_info::in, list(pred_proc_id)::in,
    list(pred_proc_id_info)::out, list(pred_proc_id_info)::out,
    list(pred_proc_id_info)::out) is det.

partition_scc_procs(_ModuleInfo, [], [], [], []).
partition_scc_procs(ModuleInfo, [PredProcId | PredProcIds],
        !:NoneIdInfos, !:SelfIdInfos, !:MutualIdInfos) :-
    partition_scc_procs(ModuleInfo, PredProcIds,
        !:NoneIdInfos, !:SelfIdInfos, !:MutualIdInfos),
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    IdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    (
        % Tail recursion optimization does not apply to model_non
        % procedures.
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        proc_info_get_has_tail_rec_call(ProcInfo, HasTailRecCall),
        HasTailRecCall = has_tail_rec_call(HasSelfTailRecCall,
            HasMutualTailRecCall),
        (
            HasMutualTailRecCall = has_mutual_tail_rec_call,
            !:MutualIdInfos = [IdInfo | !.MutualIdInfos]
        ;
            HasMutualTailRecCall = has_no_mutual_tail_rec_call,
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
    PredProcIdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo),
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
    PredProcIdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo),
    trace [io(!IO)] (
        write_proc_progress_message("% Generating MLDS code for ",
            PredProcId, ModuleInfo, !IO)
    ),

    pred_info_get_status(PredInfo, PredStatus),
    pred_info_get_arg_types(PredInfo, ArgTypes),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),
    proc_info_get_argmodes(ProcInfo, Modes),
    proc_info_get_goal(ProcInfo, Goal),
    MaybeRequireTailrecInfoFD = no,
    PredProcId = proc(PredId, ProcId),

    Goal = hlds_goal(_GoalExpr, GoalInfo),
    Context = goal_info_get_context(GoalInfo),

    some [!Info] (
        module_info_get_globals(ModuleInfo, Globals),
        SupportsBreakContinue =
            globals_target_supports_break_and_continue(Globals),
        (
            NoneOrSelf = no_tail_rec,
            map.init(TailRecMap0)
        ;
            NoneOrSelf = self_tail_rec,
            InputParams =
                ml_gen_proc_params_inputs_only(ModuleInfo, PredId, ProcId),
            (
                SupportsBreakContinue = yes,
                % If we find a tail call, we will wrap the function body inside
                % `while (true) { ... break; }', and so the tail call can just
                % do a `continue', which will continue the next iteration
                % of the loop.
                TailRecMechanism0 = tail_rec_via_while_loop
            ;
                SupportsBreakContinue = no,
                % If we find a tail call, we will insert a label at the start
                % of the function, and so the tail call can just goto
                % to that label.
                TailRecMechanism0 = tail_rec_via_start_label("proc_top")
            ),
            TailRecTargetInfo0 = tail_rec_target_info(TailRecMechanism0,
                InputParams, have_not_done_tail_rec),
            TailRecMap0 = map.singleton(PredProcId, TailRecTargetInfo0)
        ),
        !:Info = ml_gen_info_init(ModuleInfo, Target, ConstStructMap,
            PredId, ProcId, ProcInfo, TailRecMap0, !.GlobalData),

        ( if PredStatus = pred_status(status_external(_)) then
            % For Mercury procedures declared `:- pragma external_{pred/func}',
            % we generate an MLDS definition with no function body.
            % The MLDS -> target code pass can treat this accordingly.
            % For example, for C it outputs a function declaration with no
            % corresponding definition, making sure that the function is
            % declared as `extern' rather than `static'.
            FuncBody = body_external,
            ClosureWrapperFuncDefns = [],
            ml_gen_info_proc_params(PredId, ProcId, MLDS_Params,
                !.Info, _Info),
            set.init(EnvVarNames)
        else
            % Set up the initial success continuation, if any.
            % Also figure out which output variables are returned by value
            % (rather than being passed by reference) and remove them from
            % the byref_output_vars field in the ml_gen_info.
            (
                ( CodeModel = model_det
                ; CodeModel = model_semi
                ),
                ml_det_copy_out_vars(ModuleInfo, CopiedOutputVars, !Info)
            ;
                CodeModel = model_non,
                ml_set_up_initial_succ_cont(ModuleInfo, CopiedOutputVars,
                    !Info)
            ),

            modes_to_top_functor_modes(ModuleInfo, Modes, ArgTypes,
                TopFunctorModes),
            ml_gen_proc_body(CodeModel, HeadVars, ArgTypes, TopFunctorModes,
                CopiedOutputVars, Goal, LocalVarDefns0, FuncDefns, GoalStmts,
                !Info),

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
                vartypes_overlay_corresponding_lists(HeadVars, ArgTypes,
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
            ml_gen_info_proc_params(PredId, ProcId, MLDS_Params, !Info),
            ml_gen_info_get_closure_wrapper_defns(!.Info,
                ClosureWrapperFuncDefns),
            ml_gen_info_get_global_data(!.Info, !:GlobalData),
            LocalVarDefns = ProcLocalVarDefns ++ LocalVarDefns0,
            ml_gen_info_get_tail_rec_info(!.Info, TailRecInfo),
            TailRecInfo = tail_rec_info(TargetMap, _WarnParams, TailRecSpecs),
            !:Specs = TailRecSpecs ++ !.Specs,
            ( if
                map.search(TargetMap, PredProcId, TailRecTargetInfo),
                TailRecTargetInfo = tail_rec_target_info(TailRecMechanism,
                    _InputParams, HaveDoneTailRec),
                HaveDoneTailRec = have_done_tail_rec
            then
                % XXX Use better comment, e.g. "setup for optimized tail calls"
                CommentStmt = ml_stmt_atomic(
                    comment("setup for tailcalls optimized into a loop"),
                    Context),
                % XXX We *should* generate the following code, but ...
                (
                    TailRecMechanism = tail_rec_via_while_loop,
                    BreakStmt = ml_stmt_goto(goto_break, Context),
                    LoopBodyStmt = ml_stmt_block(LocalVarDefns, FuncDefns,
                        [CommentStmt] ++ GoalStmts ++ [BreakStmt], Context),
                    FuncBodyStmt = ml_stmt_while(may_loop_zero_times,
                        ml_const(mlconst_true), LoopBodyStmt, Context)
                ;
                    TailRecMechanism = tail_rec_via_start_label(StartLabel),
                    LoopTopLabelStmt = ml_stmt_label(StartLabel, Context),
                    FuncBodyStmt = ml_stmt_block(LocalVarDefns, FuncDefns,
                        [CommentStmt, LoopTopLabelStmt] ++ GoalStmts, Context)
                )
            else
                FuncBodyStmt = ml_gen_block(LocalVarDefns, FuncDefns,
                    GoalStmts, Context)
            ),
            FuncBody = body_defined_here(FuncBodyStmt),
            ml_gen_info_get_env_var_names(!.Info, EnvVarNames)
        )
    ),

    proc_info_get_context(ProcInfo, ProcContext),
    ml_gen_proc_label(ModuleInfo, PredId, ProcId,
        _ModuleName, PlainFuncName),
    DeclFlags = ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId),
    MaybePredProcId = yes(PredProcId),
    pred_info_get_attributes(PredInfo, Attributes),
    attributes_to_attribute_list(Attributes, AttributeList),
    MLDS_Attributes =
        list.map(attribute_to_mlds_attribute(ModuleInfo), AttributeList),
    FuncDefn = mlds_function_defn(mlds_function_name(PlainFuncName),
        ProcContext, DeclFlags, MaybePredProcId, MLDS_Params,
        FuncBody, MLDS_Attributes, EnvVarNames, MaybeRequireTailrecInfoFD),
    !:FuncDefns = ClosureWrapperFuncDefns ++ [FuncDefn | !.FuncDefns].

%---------------------------------------------------------------------------%
%
% Code for handling via-tail-call SCCs.
%

:- pred ml_gen_tscc(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, set(pred_proc_id)::in, scc_with_entry_points::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_tscc(ModuleInfo, Target, ConstStructMap, SCCEntryProcs,
        TSCCE, !FuncDefns, !GlobalData, !Specs) :-
    TSCCE = scc_with_entry_points(PredProcIds, CalledFromHigherSCCs,
        ExportedProcs),
    set.union(CalledFromHigherSCCs, ExportedProcs, TSCCEntryProcs),
    % The following code is temporary.
    set.union(SCCEntryProcs, TSCCEntryProcs, _EntryProcs),
    set.foldl3(
        ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
            self_tail_rec),
        PredProcIds, !FuncDefns, !GlobalData, !Specs).

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

    % For model_det and model_semi procedures, figure out which output
    % variables are returned by value (rather than being passed by reference)
    % and remove them from the byref_output_vars field in the ml_gen_info.
    %
:- pred ml_det_copy_out_vars(module_info::in, list(prog_var)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_det_copy_out_vars(ModuleInfo, CopiedOutputVars, !Info) :-
    ml_gen_info_get_byref_output_vars(!.Info, OutputVars),
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, det_copy_out, DetCopyOut),
    (
        % If --det-copy-out is enabled, all non-dummy output variables are
        % returned by value, rather than passing them by reference.
        DetCopyOut = yes,
        ByRefOutputVars = [],
        ml_gen_info_get_var_types(!.Info, VarTypes),
        list.filter(var_is_of_dummy_type(ModuleInfo, VarTypes), OutputVars,
            _, CopiedOutputVars)
    ;
        DetCopyOut = no,
        ( if
            % For det functions, the function result variable is returned by
            % value, and any remaining output variables are passed by
            % reference.
            ml_gen_info_get_pred_id(!.Info, PredId),
            ml_gen_info_get_proc_id(!.Info, ProcId),
            ml_is_output_det_function(ModuleInfo, PredId, ProcId, ResultVar)
        then
            CopiedOutputVars = [ResultVar],
            list.delete_all(OutputVars, ResultVar, ByRefOutputVars)
        else
            % Otherwise, all output vars are passed by reference.
            CopiedOutputVars = [],
            ByRefOutputVars = OutputVars
        )
    ),
    ml_gen_info_set_byref_output_vars(ByRefOutputVars, !Info).

    % For model_non procedures, figure out which output variables are returned
    % by value (rather than being passed by reference) and remove them from
    % the byref_output_vars field in the ml_gen_info, and construct the
    % initial success continuation.
    %
:- pred ml_set_up_initial_succ_cont(module_info::in, list(prog_var)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_set_up_initial_succ_cont(ModuleInfo, NondetCopiedOutputVars, !Info) :-
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, nondet_copy_out, NondetCopyOut),
    (
        NondetCopyOut = yes,
        % For --nondet-copy-out, we generate local variables for the output
        % variables and then pass them to the continuation, rather than
        % passing them by reference.
        ml_gen_info_get_byref_output_vars(!.Info, NondetCopiedOutputVars),
        ml_gen_info_set_byref_output_vars([], !Info)
    ;
        NondetCopyOut = no,
        NondetCopiedOutputVars = []
    ),
    ml_gen_var_list(!.Info, NondetCopiedOutputVars, OutputVarLvals),
    ml_variable_types(!.Info, NondetCopiedOutputVars, OutputVarTypes),
    ml_initial_cont(!.Info, OutputVarLvals, OutputVarTypes, InitialCont),
    ml_gen_info_push_success_cont(InitialCont, !Info).

    % Generate the code for a procedure body.
    %
:- pred ml_gen_proc_body(code_model::in, list(prog_var)::in,
    list(mer_type)::in, list(top_functor_mode)::in, list(prog_var)::in,
    hlds_goal::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_proc_body(CodeModel, HeadVars, ArgTypes, TopFunctorModes,
        CopiedOutputVars, Goal, LocalVarDefns, FuncDefns, Stmts, !Info) :-
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
    ml_append_return_statement(CodeModel, CopiedOutputVarOriginalLvals,
        Context, Stmts1, Stmts, !Info).

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

%---------------------------------------------------------------------------%
:- end_module ml_backend.ml_proc_gen.
%---------------------------------------------------------------------------%
