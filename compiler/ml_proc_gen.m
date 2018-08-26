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
:- import_module ml_backend.ml_unused_assign.
:- import_module ml_backend.ml_util.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_foreign.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.

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
    globals.lookup_bool_option(Globals, optimize_tailcalls, TailCalls),
    (
        TailCalls = yes,
        OptTailCalls = tail_call_opt_in_code_gen
    ;
        TailCalls = no,
        OptTailCalls = no_tail_call_opt_in_code_gen
    ),
    get_default_warn_parms(Globals, DefaultWarnParams),
    ml_gen_sccs(!.ModuleInfo, OptTailCalls, DefaultWarnParams, Target,
        ConstStructMap, BottomUpSCCsWithEntryPoints,
        [], FuncDefns, !GlobalData, !Specs).

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
    warn_non_tail_rec_params::in, mlds_target_lang::in,
    ml_const_struct_map::in, list(scc_with_entry_points)::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_sccs(_, _, _, _, _, [], !FuncDefns, !GlobalData, !Specs).
ml_gen_sccs(ModuleInfo, OptTailCalls, DefaultWarnParams, Target,
        ConstStructMap, [SCCE | SCCEs], !FuncDefns, !GlobalData, !Specs) :-
    ml_gen_scc(ModuleInfo, OptTailCalls, DefaultWarnParams, Target,
        ConstStructMap, SCCE, !FuncDefns, !GlobalData, !Specs),
    ml_gen_sccs(ModuleInfo, OptTailCalls, DefaultWarnParams, Target,
        ConstStructMap, SCCEs, !FuncDefns, !GlobalData, !Specs).

%---------------------------------------------------------------------------%

:- pred ml_gen_scc(module_info::in, maybe_tail_call_opt_in_code_gen::in,
    warn_non_tail_rec_params::in, mlds_target_lang::in,
    ml_const_struct_map::in, scc_with_entry_points::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    list(error_spec)::in, list(error_spec)::out) is det.

ml_gen_scc(ModuleInfo, OptTailCalls, DefaultWarnParams, Target,
        ConstStructMap, SCCE, !FuncDefns, !GlobalData, !Specs) :-
    ml_gen_scc_code(ModuleInfo, OptTailCalls, Target, ConstStructMap, SCCE,
        InSccMap, !FuncDefns, !GlobalData),
    map.foldl_values(gather_nontail_rec_calls, InSccMap, [], NonTailRecCalls),
    ( if
        % If we were trying to implement recursive calls as tail calls, ...
        OptTailCalls = tail_call_opt_in_code_gen,
        % ... but some recursive calls turned out NOT to be implementable
        % as tail calls, ...
        NonTailRecCalls = [_ | _]
    then
        % ... then generate messages for them, if the appropriate settings
        % call for such messages.
        %
        % Having a list of all the non-tail recursive calls in the SCC
        % in one place should allow a future diff to report, in cases
        % where the caller and callee are in different TSCCs, exactly
        % which recursive calls being non-tail calls prevent them from
        % being in the same TSCC.
        list.foldl(report_nontail_rec_call(ModuleInfo, DefaultWarnParams), 
            NonTailRecCalls, !Specs)
    else
        true
    ).

:- pred gather_nontail_rec_calls(in_scc_info::in,
    list(nontail_rec_call)::in, list(nontail_rec_call)::out) is det.

gather_nontail_rec_calls(InSccInfo, !NonTailRecCalls) :-
    !:NonTailRecCalls =
        InSccInfo ^ isi_is_target_of_non_tail_rec ++ !.NonTailRecCalls.

:- pred report_nontail_rec_call(module_info::in,
    warn_non_tail_rec_params::in, nontail_rec_call::in,
    list(error_spec)::in, list(error_spec)::out) is det.

report_nontail_rec_call(ModuleInfo, DefaultWarnParams, NonTailRecCall,
        !Specs) :-
    NonTailRecCall = nontail_rec_call(Caller, Callee, Context,
        Reason, Obviousness, Status),
    (
        Status = nontail_rec_call_warn_disabled
    ;
        Status = nontail_rec_call_warn_enabled,
        module_info_pred_proc_info(ModuleInfo, Caller, _PredInfo, ProcInfo),
        maybe_override_warn_params_for_proc(ProcInfo,
            DefaultWarnParams, ProcWarnParams),
        maybe_report_nontail_recursive_call(ModuleInfo, Caller, Callee,
            Context, Reason, Obviousness, ProcWarnParams, !Specs)
    ).

%---------------------------------------------------------------------------%

:- pred ml_gen_scc_code(module_info::in, maybe_tail_call_opt_in_code_gen::in,
    mlds_target_lang::in, ml_const_struct_map::in, scc_with_entry_points::in,
    in_scc_map::out,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_scc_code(ModuleInfo, OptTailCalls, Target, ConstStructMap, SCCE,
        !:InSccMap, !FuncDefns, !GlobalData) :-
    SCCE = scc_with_entry_points(PredProcIds, CalledFromHigherSCCs,
        ExportedProcs),
    set.union(CalledFromHigherSCCs, ExportedProcs, SCCEntryProcs),

    set.fold(add_to_in_scc_map, PredProcIds, map.init, !:InSccMap),
    (
        OptTailCalls = no_tail_call_opt_in_code_gen,
        set.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                no_tail_rec),
            PredProcIds, !FuncDefns, !GlobalData, !InSccMap)
    ;
        OptTailCalls = tail_call_opt_in_code_gen,
        partition_scc_procs(ModuleInfo, set.to_sorted_list(PredProcIds),
            NonePredProcIdInfos, SelfPredProcIdInfos0,
            MutualDetPredProcIdInfos0, MutualSemiPredProcIdInfos0),

        % The predicates called by ml_gen_tscc always generate gc_no_stmt
        % as the gc annotation on MLDS function parameters. Until this
        % limitation is fixed, don't give any work to ml_gen_tscc in
        % circumstances where it would bite.
        module_info_get_globals(ModuleInfo, Globals),
        globals.get_gc_method(Globals, GC),
        ( if GC = gc_accurate then
            SelfPredProcIdInfos = SelfPredProcIdInfos0 ++
                MutualDetPredProcIdInfos0 ++ MutualSemiPredProcIdInfos0,
            MutualDetPredProcIdInfos = [],
            MutualSemiPredProcIdInfos = []
        else
            SelfPredProcIdInfos = SelfPredProcIdInfos0,
            MutualDetPredProcIdInfos = MutualDetPredProcIdInfos0,
            MutualSemiPredProcIdInfos = MutualSemiPredProcIdInfos0
        ),

        % Translate the procedures we cannot apply tail call optimization to.
        list.foldl3(
            ml_gen_proc(ModuleInfo, Target, ConstStructMap, no_tail_rec),
            NonePredProcIdInfos, !FuncDefns, !GlobalData, !InSccMap),

        % Translate the procedures to which we can apply only self-tail-call
        % optimization.
        list.foldl3(
            ml_gen_proc(ModuleInfo, Target, ConstStructMap, self_tail_rec),
            SelfPredProcIdInfos, !FuncDefns, !GlobalData, !InSccMap),

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
            DetLonePredProcIds, !FuncDefns, !GlobalData, !InSccMap),
        list.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            SemiLonePredProcIds, !FuncDefns, !GlobalData, !InSccMap),
        list.foldl3(
            ml_gen_tscc(ModuleInfo, Target, ConstStructMap, SCCEntryProcs,
                tscc_det),
            DetNonTrivialTSCCEntries, !FuncDefns, !GlobalData, !InSccMap),
        list.foldl3(
            ml_gen_tscc(ModuleInfo, Target, ConstStructMap, SCCEntryProcs,
                tscc_semi),
            SemiNonTrivialTSCCEntries, !FuncDefns, !GlobalData, !InSccMap)
    ).

%---------------------%

:- pred add_to_in_scc_map(pred_proc_id::in, in_scc_map::in, in_scc_map::out)
    is det.

add_to_in_scc_map(PredProcId, !InSccMap) :-
    InSccInfo = in_scc_info(not_in_tscc,
        is_not_target_of_self_trcall, is_not_target_of_mutual_trcall, []),
    map.det_insert(PredProcId, InSccInfo, !InSccMap).

:- pred reset_in_scc_map(in_scc_map::in, in_scc_map::out) is det.

reset_in_scc_map(!InSccMap) :-
    map.map_values_only(reset_scc_info, !InSccMap).

:- pred reset_scc_info(in_scc_info::in, in_scc_info::out) is det.

reset_scc_info(!InSccInfo) :-
    !InSccInfo ^ isi_maybe_in_tscc := not_in_tscc.

%---------------------%

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

%---------------------%

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
    proc_info_get_has_tail_rec_call(ProcInfo, HasTailRecCall),
    HasTailRecCall =
        has_tail_rec_call(HasSelfTailRecCall, HasMutualTailRecCall),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    (
        ( CodeModel = model_det
        ; CodeModel = model_semi
        ),
        proc_info_interface_determinism(ProcInfo, Detism),
        determinism_components(Detism, _CanFail, SolnCount),
        ( if
            HasMutualTailRecCall = has_mutual_tail_rec_call,
            % To prevent control just falling through to the next procedure
            % body once it reaches the end of the previous procedure body
            % in the TSCC, we put a goto or break statement at the end of each
            % procedure body. This will generate an error from some target
            % language compilers (e.g. Java) if its knows that this statement
            % is not reachable.
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
        % Mutual tail recursion optimization does not apply to model_non
        % procedures (at least not yet). It would be nice to teach this module
        % how to exploit such opportunities, but applying mutual tail recursion
        % optimization to only det and semidet procedures does capture
        % *almost all* of the available benefit.
        (
            HasSelfTailRecCall = has_self_tail_rec_call,
            !:SelfIdInfos = [IdInfo | !.SelfIdInfos]
        ;
            HasSelfTailRecCall = has_no_self_tail_rec_call,
            !:NoneIdInfos = [IdInfo | !.NoneIdInfos]
        )
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
    in_scc_map::in, in_scc_map::out) is det.

ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap, NoneOrSelf,
        PredProcId, !FuncDefns, !GlobalData, !InSccMap) :-
    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    ProcContext = goal_info_get_context(GoalInfo),
    PredProcIdInfo =
        pred_proc_id_info(PredProcId, PredInfo, ProcInfo, ProcContext),
    ml_gen_proc(ModuleInfo, Target, ConstStructMap,
        NoneOrSelf, PredProcIdInfo, !FuncDefns, !GlobalData, !InSccMap).

%---------------------%

:- type none_or_self_tail_rec
    --->    no_tail_rec
    ;       self_tail_rec.

:- pred ml_gen_proc(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, none_or_self_tail_rec::in, pred_proc_id_info::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    in_scc_map::in, in_scc_map::out) is det.

ml_gen_proc(ModuleInfo, Target, ConstStructMap, NoneOrSelf,
        PredProcIdInfo, !FuncDefns, !GlobalData, !InSccMap) :-
    PredProcIdInfo =
        pred_proc_id_info(PredProcId, PredInfo, ProcInfo, ProcContext),
    trace [io(!IO)] (
        write_proc_progress_message("% Generating MLDS code for ",
            PredProcId, ModuleInfo, !IO)
    ),

    some [!Info] (
        reset_in_scc_map(!InSccMap),
        compute_initial_tail_rec_map_for_none_or_self(ModuleInfo, NoneOrSelf,
            PredProcId, !InSccMap),
        init_ml_gen_tscc_info(ModuleInfo, !.InSccMap, tscc_self_rec_only,
            TsccInfo0),
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
            ml_gen_info_proc_params(PredProcId, _Tuples, FuncParams,
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
            ml_gen_info_proc_params(PredProcId, ArgTuples, FuncParams,
                ByRefOutputVars, CopiedOutputVars, !.Info, _Info),
            set_of_var.list_to_set(ByRefOutputVars, ByRefOutputVarsSet),
            ml_gen_info_set_byref_output_vars(ByRefOutputVarsSet, !Info),
            (
                ( CodeModel = model_det
                ; CodeModel = model_semi
                )
            ;
                CodeModel = model_non,
                list.map(get_var_mlds_lval_and_type(!.Info),
                    CopiedOutputVars, OutputVarLvalTypes),
                ml_initial_cont(!.Info, OutputVarLvalTypes, InitialCont),
                ml_gen_info_push_success_cont(InitialCont, !Info)
            ),

            proc_info_get_goal(ProcInfo, Goal),
            ml_gen_info_get_tail_rec_info(!.Info, TailRecInfo1),
            TailRecInfo1 = tail_rec_info(InSccMap1, LoopKind1, TsccKind1),
            map.lookup(InSccMap1, PredProcId, InSccInfo1),
            MaybeInScc1 = InSccInfo1 ^ isi_maybe_in_tscc,
            (
                MaybeInScc1 = not_in_tscc,
                map.init(SeenAtLabelMap)
            ;
                MaybeInScc1 = in_tscc(IdInTscc, Args),
                (
                    LoopKind1 = tail_rec_loop_while_continue,
                    (
                        TsccKind1 = tscc_self_rec_only,
                        map.init(SeenAtLabelMap)
                    ;
                        TsccKind1 = tscc_self_and_mutual_rec,
                        unexpected($pred, "tscc_self_and_mutual_rec")
                    )
                ;
                    LoopKind1 = tail_rec_loop_label_goto,
                    StartLabel = generate_tail_rec_start_label(TsccKind1,
                        IdInTscc),
                    LocalVars = list.map(project_mlds_argument_name, Args),
                    SeenAtLabelMap =
                        map.singleton(StartLabel, set.list_to_set(LocalVars))
                )
            ),
            ml_gen_proc_body(CodeModel, ArgTuples, CopiedOutputVars, Goal,
                SeenAtLabelMap, LocalVarDefns0, FuncDefns, GoalStmts0, !Info),
            list.map(get_var_rval(!.Info),
                CopiedOutputVars, CopiedOutputVarRvals),
            ml_append_return_statement(CodeModel, ProcContext,
                CopiedOutputVarRvals, GoalStmts0, GoalStmts),
            ml_gen_local_var_defns_for_copied_output_vars(ProcInfo,
                ProcContext, ArgTuples, CopiedOutputVars, OutputVarLocalDefns,
                !Info),
            ml_gen_maybe_local_var_defn_for_succeeded(!.Info, ProcContext,
                SucceededVarDefns),
            LocalVarDefns = SucceededVarDefns ++ OutputVarLocalDefns ++
                LocalVarDefns0,

            ml_gen_info_final(!.Info, EnvVarNames,
                ClosureWrapperFuncDefns, !:GlobalData, TsccInfo),
            TailRecInfo = TsccInfo ^ mgti_tail_rec_info,
            !:InSccMap = TailRecInfo ^ tri_in_scc_map,
            construct_func_body_maybe_wrap_in_loop(PredProcId, CodeModel,
                ProcContext, LocalVarDefns, FuncDefns, GoalStmts,
                TailRecInfo, FuncBody)
        )
    ),

    construct_func_defn(ModuleInfo, PredProcIdInfo, FuncParams, FuncBody,
        EnvVarNames, FuncDefn),
    !:FuncDefns = ClosureWrapperFuncDefns ++ [FuncDefn | !.FuncDefns].

:- pred get_var_mlds_lval_and_type(ml_gen_info::in, prog_var::in,
    pair(mlds_lval, mer_type)::out) is det.

get_var_mlds_lval_and_type(Info, Var, VarLval - Type) :-
    ml_gen_var(Info, Var, VarLval),
    ml_variable_type(Info, Var, Type).

:- pred get_var_rval(ml_gen_info::in, prog_var::in, mlds_rval::out) is det.

get_var_rval(Info, Var, VarRval) :-
    ml_gen_var(Info, Var, VarLval),
    VarRval = ml_lval(VarLval).

:- pred compute_initial_tail_rec_map_for_none_or_self(module_info::in,
    none_or_self_tail_rec::in, pred_proc_id::in,
    in_scc_map::in, in_scc_map::out) is det.

compute_initial_tail_rec_map_for_none_or_self(ModuleInfo, NoneOrSelf,
        PredProcId, !InSccMap) :-
    (
        NoneOrSelf = no_tail_rec
        % Nothing to do.
    ;
        NoneOrSelf = self_tail_rec,
        InputParams =
            ml_gen_proc_params_inputs_only_no_gc_stmts(ModuleInfo, PredProcId),
        map.lookup(!.InSccMap, PredProcId, InSccInfo0),
        InSccInfo = InSccInfo0 ^ isi_maybe_in_tscc :=
            in_tscc(proc_id_in_tscc(1), InputParams),
        map.det_update(PredProcId, InSccInfo, !InSccMap)
    ).

:- pred construct_func_body_maybe_wrap_in_loop(pred_proc_id::in,
    code_model::in, prog_context::in, list(mlds_local_var_defn)::in,
    list(mlds_function_defn)::in, list(mlds_stmt)::in, tail_rec_info::in,
    mlds_function_body::out) is det.

construct_func_body_maybe_wrap_in_loop(PredProcId, CodeModel, Context,
        LocalVarDefns, FuncDefns, GoalStmts, TailRecInfo, FuncBody) :-
    TailRecInfo = tail_rec_info(InSccMap, LoopKind, TsccKind),
    expect(unify(TsccKind, tscc_self_rec_only), $pred,
        "TsccKind != tscc_self_rec_only"),
    map.lookup(InSccMap, PredProcId, InSccInfo),
    InSccInfo = in_scc_info(MaybeInTscc,
        IsTargetOfSelfTRCall, _IsTargetOfMutualTRCall, _IsTargetOfNonTailRec),
    ( if
        IsTargetOfSelfTRCall = is_target_of_self_trcall,
        % We cannot have done self-tail-recursion if MaybeInTscc = not_in_tscc,
        % though the compiler doesn't know that.
        MaybeInTscc = in_tscc(IdInTscc, TsccInArgs)
    then
        ( CodeModel = model_det,  CodeModelStr = "model_det"
        ; CodeModel = model_semi, CodeModelStr = "model_semi"
        ; CodeModel = model_non,  CodeModelStr = "model_non"
        ),
        string.format("setup for %s tailcalls optimized into a loop",
            [s(CodeModelStr)], CommentStr),
        Comment = comment(CommentStr),
        CommentStmt = ml_stmt_atomic(Comment, Context),
        (
            LoopKind = tail_rec_loop_while_continue,
            BreakStmt = ml_stmt_goto(goto_break_loop, Context),
            LoopBodyStmt = ml_stmt_block(LocalVarDefns, FuncDefns,
                [CommentStmt] ++ GoalStmts ++ [BreakStmt], Context),
            % Since TsccKind = tscc_self_rec_only, we don't need to include
            % the procedure selector variable in the loop local vars.
            InputArgLocalVars =
                list.map(project_mlds_argument_name, TsccInArgs),
            FuncBodyStmt = ml_stmt_while(may_loop_zero_times,
                ml_const(mlconst_true), LoopBodyStmt,
                InputArgLocalVars, Context)
        ;
            LoopKind = tail_rec_loop_label_goto,
            StartLabel = generate_tail_rec_start_label(TsccKind, IdInTscc),
            LoopTopLabelStmt = ml_stmt_label(StartLabel, Context),
            FuncBodyStmt = ml_stmt_block(LocalVarDefns, FuncDefns,
                [CommentStmt, LoopTopLabelStmt] ++ GoalStmts, Context)
        )
    else
        FuncBodyStmt =
            ml_gen_block(LocalVarDefns, FuncDefns, GoalStmts, Context)
    ),
    FuncBody = body_defined_here(FuncBodyStmt).

:- pred construct_func_defn(module_info::in, pred_proc_id_info::in,
    mlds_func_params::in, mlds_function_body::in, set(string)::in,
    mlds_function_defn::out) is det.

construct_func_defn(ModuleInfo, PredProcIdInfo, FuncParams, FuncBody,
        EnvVarNames, FuncDefn) :-
    PredProcIdInfo = pred_proc_id_info(PredProcId, _PredInfo, ProcInfo,
        _ProcContext),
    PredProcId = proc(PredId, ProcId),
    ml_gen_proc_label(ModuleInfo, PredProcId, _ModuleName, PlainFuncName),
    proc_info_get_context(ProcInfo, ProcContext),
    DeclFlags = ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId),
    MaybePredProcId = yes(PredProcId),
    MaybeRequireTailrecInfoFD = no,
    FuncDefn = mlds_function_defn(mlds_function_name(PlainFuncName),
        ProcContext, DeclFlags, MaybePredProcId, FuncParams, FuncBody,
        EnvVarNames, MaybeRequireTailrecInfoFD).

%---------------------------------------------------------------------------%
%
% Code for handling TSCCs (shorthand for via-tail-call SCCs, i.e. SCCs
% computed by taking only *tail* calls into account).
%
% The scheme followed by ml_gen_tscc and its subcontractors is documented
% in notes/mlds_tail_recursion.html.
%

:- type tscc_code_model
    --->    tscc_det
    ;       tscc_semi.

:- pred ml_gen_tscc(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, set(pred_proc_id)::in, tscc_code_model::in,
    scc_with_entry_points::in,
    list(mlds_function_defn)::in, list(mlds_function_defn)::out,
    ml_global_data::in, ml_global_data::out,
    in_scc_map::in, in_scc_map::out) is det.

ml_gen_tscc(ModuleInfo, Target, ConstStructMap, _SCCEntryPredProcIds,
        TsccCodeModel, TSCCE, !FuncDefns, !GlobalData, !InSccMap) :-
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
            SinglePredProcId, !FuncDefns, !GlobalData, !InSccMap)
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
        %
        % Caveat 3.
        % The code in mark_tail_calls.m looks for tail calls by looking
        % at the argument lists of procedures. These argument lists treat
        % a variable representing the result of functions the same as
        % a variable representing the last argument of a predicate.
        % On the other hand, on the C backend, the MLDS uses two *different*
        % mechanisms to return these arguments; it returns function results
        % via return statements, while it uses pass-by-reference for all
        % other output arguments. Currently, ml_gen_tscc_trial handles this
        % by returning CanGenerateTscc = can_not_generate_code_for_tscc
        % if the procedures in the TSCC it is given don't all have the same
        % vector of return values.
        % XXX When a TSCC contains both predicates and functions, the two
        % must present different signatures to their callers *outside* the
        % TSCC, because those callers expect those different signatures.
        % However, *inside* the TSCC, we could generate code that ignores
        % the distinction, the obvious way of doing that being to use
        % the tail call version of the calling convention intended for
        % predicates for all intra-TSCC tail calls. However, using different
        % conventions for intra- and inter-TSCC parameter passing would
        % complicate the code both here and in ml_args_util.m, and for now
        % at least, I (zs) don't see that the potential benefits justify
        % the costs.
        %
        ml_gen_tscc_trial(ModuleInfo, Target, ConstStructMap,
            TsccCodeModel, PredProcIds, _NonTailEntryPredProcIds,
            NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
            CanGenerateTscc, MutualEnvVarNames, MutualClosureWrapperFuncDefns,
            LoopKind, !.InSccMap, TrialInSccMap,
            !.GlobalData, TrialGlobalData),
        (
            CanGenerateTscc = can_not_generate_code_for_tscc,
            OutsideTsccPredProcIds = PredProcIds
        ;
            CanGenerateTscc = can_generate_code_for_tscc,
            OutsideTsccPredProcIds = NoMutualPredProcIds,
            !:InSccMap = TrialInSccMap,
            !:GlobalData = TrialGlobalData,

            % Caveat 4:
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
                construct_tscc_entry_proc(ModuleInfo, LoopKind,
                    MutualPredProcCodes, MutualEnvVarNames, StartCommentStmts),
                set.to_sorted_list(MutualPredProcIds), TSCCFuncDefns),
            !:FuncDefns = MutualClosureWrapperFuncDefns ++ TSCCFuncDefns ++
                !.FuncDefns
        ),

        list.foldl3(
            ml_gen_proc_lookup(ModuleInfo, Target, ConstStructMap,
                self_tail_rec),
            set.to_sorted_list(OutsideTsccPredProcIds),
            !FuncDefns, !GlobalData, !InSccMap)
    ).

:- type can_we_generate_code_for_tscc
    --->    can_not_generate_code_for_tscc
    ;       can_generate_code_for_tscc.

    % Generate a representation of the codes to go under each "top_of_proc_i"
    % label in the scheme above, but also return the information our caller
    % needs to prepare for handling caveats 1 and 2.
    %
:- pred ml_gen_tscc_trial(module_info::in, mlds_target_lang::in,
    ml_const_struct_map::in, tscc_code_model::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::out, set(pred_proc_id)::out, list(pred_proc_code)::out,
    can_we_generate_code_for_tscc::out, set(string)::out,
    list(mlds_function_defn)::out, tail_rec_loop_kind::out,
    in_scc_map::in, in_scc_map::out,
    ml_global_data::in, ml_global_data::out) is det.

ml_gen_tscc_trial(ModuleInfo, Target, ConstStructMap, TsccCodeModel,
        PredProcIds, NonTailEntryPredProcIds,
        NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
        CanGenerateTscc, MutualEnvVarNames,
        MutualClosureWrapperFuncDefns, LoopKind, !InSccMap, !GlobalData) :-
    % Compute the information we need for generating tail calls
    % to any of the procedures in the TSCC.
    reset_in_scc_map(!InSccMap),
    list.map_foldl6(compute_initial_tail_rec_map_for_mutual(ModuleInfo),
        set.to_sorted_list(PredProcIds), PredProcIdArgsInfos,
        1, _, maybe.no, _, can_generate_code_for_tscc, CanGenerateTscc0,
        map.init, _OutArgNames, !InSccMap, map.init, SeenAtLabelMap),

    % Translate each procedure in the TSCC into a representation of the
    % code that will go under "top_of_proc_i".
    init_ml_gen_tscc_info(ModuleInfo, !.InSccMap, tscc_self_and_mutual_rec,
        TsccInfo0),
    list.map_foldl2(
        ml_gen_tscc_proc_code(ModuleInfo, Target, ConstStructMap,
            TsccCodeModel, SeenAtLabelMap),
        PredProcIdArgsInfos, PredProcCodes,
        !GlobalData, TsccInfo0, TsccInfo),

    TailRecInfo = TsccInfo ^ mgti_tail_rec_info,
    !:InSccMap = TailRecInfo ^ tri_in_scc_map,
    LoopKind = TailRecInfo ^ tri_loop_kind,
    map.foldl2(accumulate_entry_procs, !.InSccMap,
        set.init, NonTailEntryPredProcIds, set.init, NoMutualPredProcIds0),
    % Returning NoMutualPredProcIds separately from MutualPredProcIds
    % handles caveat 1.
    separate_mutually_recursive_procs(NoMutualPredProcIds0, PredProcCodes,
        NoMutualPredProcIds, MutualPredProcIds, MutualPredProcCodes,
        MutualContainsNestedFuncs, MutualClosureWrapperFuncDefns,
        MutualEnvVarNames),

    ( if
        % Handle caveat 2.
        MutualContainsNestedFuncs = does_not_contain_nested_funcs,
        % Handle caveat 3.
        CanGenerateTscc0 = can_generate_code_for_tscc
    then
        CanGenerateTscc = can_generate_code_for_tscc
    else
        CanGenerateTscc = can_not_generate_code_for_tscc
    ).

:- pred accumulate_entry_procs(pred_proc_id::in, in_scc_info::in,
    set(pred_proc_id)::in, set(pred_proc_id)::out,
    set(pred_proc_id)::in, set(pred_proc_id)::out) is det.

accumulate_entry_procs(PredProcId, InSccInfo,
        !NonTailEntryPredProcIds, !NoMutualTailRecPredProcIds) :-
    IsTargetOfNonTRCalls = InSccInfo ^ isi_is_target_of_non_tail_rec,
    (
        IsTargetOfNonTRCalls = []
    ;
        IsTargetOfNonTRCalls = [_ | _],
        set.insert(PredProcId, !NonTailEntryPredProcIds)
    ),
    IsTargetOfMutualTRCall = InSccInfo ^ isi_is_target_of_mutual_tr,
    (
        IsTargetOfMutualTRCall = is_not_target_of_mutual_trcall,
        set.insert(PredProcId, !NoMutualTailRecPredProcIds)
    ;
        IsTargetOfMutualTRCall = is_target_of_mutual_trcall
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
    set(string)::out) is det.

separate_mutually_recursive_procs(_NoMutualTailRecProcs, [],
        set.init, set.init, [], does_not_contain_nested_funcs, [], set.init).
separate_mutually_recursive_procs(NoMutualTailRecProcs,
        [PredProcCode | PredProcCodes],
        !:NoMutualPredProcIds, !:MutualPredProcIds, !:MutualPredProcCodes,
        !:MutualContainsNestedFuncs, !:MutualClosureWrapperFuncDefns,
        !:MutualEnvVarNames) :-
    separate_mutually_recursive_procs(NoMutualTailRecProcs, PredProcCodes,
        !:NoMutualPredProcIds, !:MutualPredProcIds, !:MutualPredProcCodes,
        !:MutualContainsNestedFuncs, !:MutualClosureWrapperFuncDefns,
        !:MutualEnvVarNames),
    PredProcCode = pred_proc_code(PredProcIdArgsInfo,
        _FuncParams, _LocalVarDefns, FuncDefns, DescCommentStmt, GoalStmts,
        ProcClosureWrapperFuncDefns, ProcEnvVarNames),
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
        set.union(ProcEnvVarNames, !MutualEnvVarNames)
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
                ppiai_arg_tuples                :: list(var_mvar_type_mode),

                % Argument definitions for the TSCC variables for all
                % the arguments of the procedure, both input and output,
                % in their original order, and the types of the returned
                % arguments.
                ppiai_tscc_func_params          :: mlds_func_params,

                % The vector of rvals to return in the procedure's return
                % statement, together with their types.
                ppiai_return_rvals_types        :: assoc_list(mlds_rval,
                                                    mlds_type),

                % The lvn_tscc_proc_input_var variables for the input arguments
                % of procedure i in the TSCC will be defined
                %
                % - in the argument list of the MLDS function,
                %   if the MLDS function is for procedure j with i = j; and
                %
                % - at the top of the body of the MLDS function,
                %   if the MLDS function is for procedure j with i != j.
                %
                % The lvn_tscc_output_var variables for the byref output
                % arguments will always be defined in the argument list
                % of the MLDS function. The tscc variables for the copied-out
                % output arguments will always be defined at the top of
                % the body of the MLDS function.

                % Local variable definitions of the own variables for
                % all the arguments of the procedure. These go at the top
                % of the wrapped procedure.
                ppiai_own_local_var_defns       :: list(mlds_local_var_defn),

                % Local variable definitions of the tscc variables
                % for the input arguments of the procedure only.
                % These go at the top of the container function
                % for every procedure *other* than this one.
                % (In the container function for this procedure,
                % the variables that these would define are defined
                % in the function's signature instead.)
                ppiai_tscc_in_local_var_defns   :: list(mlds_local_var_defn),

                % The names of the variables defined in the previous field.
                % Needed in some cases for transmission to ml_unused_assign.m.
                ppiai_tscc_in_local_vars        :: list(mlds_local_var_name),

                % Local variable definitions of the tscc out variables
                % for the output arguments of the procedure only.
                % These go at the top of this procedure's container function.
                % (The container procedure of every other function
                % will have its own identical list of definitions.)
                ppiai_tscc_out_value_local_var_defns
                                                :: list(mlds_local_var_defn),

                % Statements that, for each input argument of the procedure,
                % assign the tscc in variable for that argument to the own
                % variable for that argument. These statements go at the
                % top of the wrapped procedure.
                ppiai_copy_tscc_in_to_own_copy  :: list(mlds_stmt),

                % Statements that, for each output argument of the procedure
                % (including the implicit argument for success indication
                % in model semi procedures), assign the own variable for that
                % argument to the tscc out variable for that argument.
                % These statements go at the end of the wrapped procedure.
                ppiai_copy_own_to_tscc_out_copy :: list(mlds_stmt),

                % Statements that, for each output argument of the procedure
                % that is returned by reference, store the value computed
                % for the argument (in the argument's tscc out variable)
                % in the memory location that the argument's tscc out ptr
                % variable indicates is where that argument should be put.
                % These statements go at the end of the container procedure.
                ppiai_copy_out_through_ptr      :: list(mlds_stmt)
            ).

    % Compute the map that tells the code generator how to translate
    % tail recursive calls to any procedure in the TSCC, and return
    % the other information about the argument lists of the procedures in the
    % TSCC that is convenient to compute at the same time.
    %
:- pred compute_initial_tail_rec_map_for_mutual(module_info::in,
    pred_proc_id::in, pred_proc_id_args_info::out, int::in, int::out,
    maybe(assoc_list(mlds_local_var_name, mlds_type))::in,
    maybe(assoc_list(mlds_local_var_name, mlds_type))::out,
    can_we_generate_code_for_tscc::in, can_we_generate_code_for_tscc::out,
    map(int, string)::in, map(int, string)::out,
    in_scc_map::in, in_scc_map::out,
    seen_at_label_map::in, seen_at_label_map::out) is det.

compute_initial_tail_rec_map_for_mutual(ModuleInfo,
        PredProcId, PredProcIdArgsInfo, !ProcNum, !MaybeOutVarsTypes,
        !CanGenerateTscc, !OutArgNames, !InSccMap, !SeenAtLabelMap) :-
    ThisProcNum = !.ProcNum,
    IdInTscc = proc_id_in_tscc(ThisProcNum),
    !:ProcNum = !.ProcNum + 1,

    module_info_pred_proc_info(ModuleInfo, PredProcId, PredInfo, ProcInfo),
    pred_info_get_is_pred_or_func(PredInfo, PredOrFunc),
    CodeModel = proc_info_interface_code_model(ProcInfo),
    proc_info_get_varset(ProcInfo, VarSet),
    proc_info_get_headvars(ProcInfo, HeadVars),
    pred_info_get_arg_types(PredInfo, HeadTypes),
    proc_info_get_argmodes(ProcInfo, HeadModes),

    proc_info_get_goal(ProcInfo, Goal),
    Goal = hlds_goal(_GoalExpr, GoalInfo),
    ProcContext = goal_info_get_context(GoalInfo),

    ml_gen_tscc_arg_params(ModuleInfo, PredOrFunc, CodeModel,
        ProcContext, IdInTscc, VarSet, HeadVars, HeadTypes, HeadModes,
        ArgTuples, !OutArgNames,
        TsccInArgs, FuncParams, ReturnRvalsTypes, OutVarsTypes,
        OwnLocalVarDefns, TsccInLocalVarDefns, TsccValueLocalVarDefns,
        CopyTsccToOwnStmts, CopyOwnToTsccStmts, CopyOutValThroughPtrStmts),
    TsccInLocalVars = list.map(
        (func(LocalVarDefn) = LocalVarDefn ^ mlvd_name), TsccInLocalVarDefns),
    (
        !.MaybeOutVarsTypes = no,
        !:MaybeOutVarsTypes = yes(OutVarsTypes)
    ;
        !.MaybeOutVarsTypes = yes(OldOutVarsTypes),
        ( if OutVarsTypes = OldOutVarsTypes then
            true
        else
            % The different procedures in the TSCC have different vectors of
            % output arguments.
            !:CanGenerateTscc = can_not_generate_code_for_tscc
        )
    ),
    PredProcIdArgsInfo = pred_proc_id_args_info(PredProcId, PredInfo, ProcInfo,
        ProcContext, IdInTscc, ArgTuples, FuncParams, ReturnRvalsTypes,
        OwnLocalVarDefns, TsccInLocalVarDefns, TsccInLocalVars,
        TsccValueLocalVarDefns, CopyTsccToOwnStmts, CopyOwnToTsccStmts,
        CopyOutValThroughPtrStmts),
    map.lookup(!.InSccMap, PredProcId, InSccInfo0),
    InSccInfo = InSccInfo0 ^ isi_maybe_in_tscc :=
        in_tscc(IdInTscc, TsccInArgs),
    map.det_update(PredProcId, InSccInfo, !InSccMap),
    TsccKind = tscc_self_and_mutual_rec,
    StartLabel = generate_tail_rec_start_label(TsccKind, IdInTscc),
    map.det_insert(StartLabel, set.list_to_set(TsccInLocalVars),
        !SeenAtLabelMap).

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
                ppc_env_var_names           :: set(string)
            ).

    % Translate the body of the given procedure to MLDS, and return the
    % results in a form that our caller can join together with the MLDS code
    % we get for the *other* procedures in the TSCC.
    %
:- pred ml_gen_tscc_proc_code(module_info::in,
    mlds_target_lang::in, ml_const_struct_map::in, tscc_code_model::in,
    seen_at_label_map::in, pred_proc_id_args_info::in, pred_proc_code::out,
    ml_global_data::in, ml_global_data::out,
    ml_gen_tscc_info::in, ml_gen_tscc_info::out) is det.

ml_gen_tscc_proc_code(ModuleInfo, Target, ConstStructMap, TsccCodeModel,
        SeenAtLabelMap, PredProcIdArgsInfo, PredProcCode,
        !GlobalData, !TsccInfo) :-
    PredProcIdArgsInfo = pred_proc_id_args_info(PredProcId, PredInfo, ProcInfo,
        ProcContext, ProcIdInTscc, ArgTuples, _FuncParams, _ReturnRvalsTypes,
        _OwnLocalVarDefns, _TsscInLocalVarDefns, _TsscInLocalVars,
        _TsccOutLocalVarDefns, _CopyTsccInToOwnStmts, _CopyOwnToTsccOutStmts,
        _CopyOutValThroughPtrStmts),

    trace [io(!IO)] (
        write_proc_progress_message("% Generating in-TSCC MLDS code for ",
            PredProcId, ModuleInfo, !IO)
    ),

    some [!Info] (
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

        ml_gen_info_proc_params(PredProcId, ProcArgTuples, FuncParams,
            ByRefOutputVars, CopiedOutputVars0, !Info),
        expect(unify(ArgTuples, ProcArgTuples), $pred,
            "ArgTuples != ProcArgTuples"),
        % The container procedure will take care of any output arguments
        % returned by reference. The wrapped procedures return the values
        % of all output arguments to the container functions by value,
        % so set up the code generator state accordingly.
        CopiedOutputVars = ByRefOutputVars ++ CopiedOutputVars0,
        set_of_var.init(ByRefOutputVarsSet),
        ml_gen_info_set_byref_output_vars(ByRefOutputVarsSet, !Info),
        (
            TsccCodeModel = tscc_det,
            CodeModel = model_det,
            InitSucceededStmts = []
        ;
            TsccCodeModel = tscc_semi,
            CodeModel = model_semi,
            % In some model_semi predicates, the only goal in their body
            % that can fail is a tail recursive call. The InitSucceededStmt
            % ensures that the success indicator variable (a) gets declared,
            % and (b) contains a meaningful and correct value on execution
            % paths that don't include the tail recursive call.
            ml_gen_set_success(ml_const(mlconst_true), ProcContext,
                InitSucceededStmt, !Info),
            InitSucceededStmts = [InitSucceededStmt]
        ),
        proc_info_get_goal(ProcInfo, Goal),
        ml_gen_proc_body(CodeModel, ArgTuples, CopiedOutputVars, Goal,
            SeenAtLabelMap, LocalVarDefns0, FuncDefns, GoalStmts0, !Info),
        GoalStmts = InitSucceededStmts ++ GoalStmts0,
        ml_gen_maybe_local_var_defn_for_succeeded(!.Info, ProcContext,
            SucceededVarDefns),
        LocalVarDefns = SucceededVarDefns ++ LocalVarDefns0,

        ml_gen_info_final(!.Info, EnvVarNames,
            ClosureWrapperFuncDefns, !:GlobalData, !:TsccInfo),

        PredProcCode = pred_proc_code(PredProcIdArgsInfo, FuncParams,
            LocalVarDefns, FuncDefns, CommentStmt, GoalStmts,
            ClosureWrapperFuncDefns, EnvVarNames)
    ).

    % Given the results of translating each procedure in a TSCC into MLDS code,
    % wrap them up in an MLDS function that implements EntryProc.
    %
:- pred construct_tscc_entry_proc(module_info::in, tail_rec_loop_kind::in,
    list(pred_proc_code)::in, set(string)::in,
    list(mlds_stmt)::in, pred_proc_id::in, mlds_function_defn::out) is det.

construct_tscc_entry_proc(ModuleInfo, LoopKind, PredProcCodes,
        EnvVarNames, EntryProcDescComments, EntryProc, FuncDefn) :-
    trace [io(!IO)] (
        write_proc_progress_message("% Generating MLDS code for ",
            EntryProc, ModuleInfo, !IO)
    ),

    list.map_foldl2(construct_func_body_for_tscc(EntryProc),
        PredProcCodes, ProcStmtInfos,
        no, MaybeEntryProcInfo, [], NonEntryTsccInLocalVarDefns),
    (
        MaybeEntryProcInfo = no,
        unexpected($pred, "MaybeEntryProcInfo = no")
    ;
        MaybeEntryProcInfo = yes(EntryProcInfo),
        EntryProcInfo = entry_proc_info(EntryIdInTscc, EntryPredProcIdInfo,
            EntryProcParams, EntryReturnRvalsTypes,
            EntryTsccOutLocalVarDefns, EntryCopyOutValThroughPtrStmts),
        assoc_list.keys(EntryReturnRvalsTypes, EntryReturnRvals)
    ),
    EntryPredProcIdInfo = pred_proc_id_info(_EntryPredProcId,
        _EntryPredInfo, _EntryProcInfo, EntryProcContext),
    make_container_proc(LoopKind, EntryCopyOutValThroughPtrStmts,
        EntryReturnRvals, EntryIdInTscc, EntryProcContext, ProcStmtInfos,
        ContainerVarDefns, Stmts),
    FuncBodyLocalVarDefns = ContainerVarDefns ++
        NonEntryTsccInLocalVarDefns ++ EntryTsccOutLocalVarDefns,

    EntryIdInTscc = proc_id_in_tscc(EntryIdInTsccNum),
    EntryProcDesc = describe_proc_from_id(ModuleInfo, EntryProc),
    Comment0 = string.format("The code for TSCC PROC %d: %s.",
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

    FuncBodyStmt = ml_stmt_block(FuncBodyLocalVarDefns, [],
        FuncBodyStmts, EntryProcContext),
    FuncBody = body_defined_here(FuncBodyStmt),
    construct_func_defn(ModuleInfo, EntryPredProcIdInfo, EntryProcParams,
        FuncBody, EnvVarNames, FuncDefn).

:- type entry_proc_info
    --->    entry_proc_info(
                proc_id_in_tscc,
                pred_proc_id_info,
                mlds_func_params,
                assoc_list(mlds_rval, mlds_type),
                list(mlds_local_var_defn),
                list(mlds_stmt)
            ).

:- type proc_stmt_info
    --->    proc_stmt_info(
                proc_id_in_tscc,
                list(mlds_local_var_name),  % The TSCC input arguments.
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
:- pred construct_func_body_for_tscc(pred_proc_id::in,
    pred_proc_code::in, proc_stmt_info::out,
    maybe(entry_proc_info)::in, maybe(entry_proc_info)::out,
    list(mlds_local_var_defn)::in, list(mlds_local_var_defn)::out) is det.

construct_func_body_for_tscc(EntryProc, PredProcCode,
        ProcStmtInfo, !MaybeEntryProcInfo, !NonEntryTsccInLocalVarDefns) :-
    PredProcCode = pred_proc_code(PredProcIdArgsInfo, _FuncParams,
        GoalLocalVarDefns, GoalFuncDefns, _DescCommentStmt, GoalStmts,
        _ClosureWrapperFuncDefns, _EnvVarNames),
    PredProcIdArgsInfo = pred_proc_id_args_info(PredProcId,
        PredInfo, ProcInfo, ProcContext, IdInTscc, _ArgTuples,
        FuncParams, ReturnRvalsTypes, OwnLocalVarDefns,
        TsccInLocalVarDefns, TsccInLocalVars, TsccOutLocalVarDefns,
        CopyTsccInToOwnStmts, CopyOwnToTsccOutStmts,
        CopyOutValThroughPtrStmts),
    ( if PredProcId = EntryProc then
        expect(unify(!.MaybeEntryProcInfo, no), $pred,
            "!.MaybeEntryProcInfo != no"),
        PredProcIdInfo = pred_proc_id_info(PredProcId, PredInfo, ProcInfo,
            ProcContext),
        EntryProcInfo = entry_proc_info(IdInTscc, PredProcIdInfo,
            FuncParams, ReturnRvalsTypes,
            TsccOutLocalVarDefns, CopyOutValThroughPtrStmts),
        !:MaybeEntryProcInfo = yes(EntryProcInfo)
    else
        !:NonEntryTsccInLocalVarDefns = !.NonEntryTsccInLocalVarDefns ++
            TsccInLocalVarDefns
    ),
    AllLocalVarDefns = OwnLocalVarDefns ++ GoalLocalVarDefns,
    AllStmts = CopyTsccInToOwnStmts ++ GoalStmts ++ CopyOwnToTsccOutStmts,
    ProcStmt = ml_stmt_block(AllLocalVarDefns, GoalFuncDefns, AllStmts,
        ProcContext),
    ProcStmtInfo = proc_stmt_info(IdInTscc, TsccInLocalVars, ProcStmt,
        ProcContext).

%---------------------%

    % Take the codes that go under each "top_of_proc_i", and join them
    % together to yield the body of the MLDS function for EntryProc.
    % Use whiles and continues instead of labels and gotos if requested.
    %
:- pred make_container_proc(tail_rec_loop_kind::in, list(mlds_stmt)::in,
    list(mlds_rval)::in, proc_id_in_tscc::in, prog_context::in,
    list(proc_stmt_info)::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out) is det.

make_container_proc(LoopKind, CopyOutValThroughPtrStmts, ReturnRvals,
        EntryProc, EntryProcContext, ProcStmtInfos, ContainerVarDefns,
        Stmts) :-
    ReturnStmt = ml_stmt_return(ReturnRvals, EntryProcContext),
    (
        LoopKind = tail_rec_loop_label_goto,
        make_container_proc_with_label_goto(CopyOutValThroughPtrStmts,
            ReturnStmt, EntryProc, EntryProcContext, ProcStmtInfos, Stmts),
        ContainerVarDefns = []
    ;
        LoopKind = tail_rec_loop_while_continue,
        make_container_proc_with_while_continue(CopyOutValThroughPtrStmts,
            ReturnStmt, EntryProc, EntryProcContext, ProcStmtInfos,
            ContainerVarDefns, Stmts)
    ).

%---------------------%

    % We wrap the statements we generate for each TSCC procedure like this:
    %
    % goto top_of_proc_<entry_proc>;
    %
    % top_of_proc_1:
    %   <copy tscc args 1 to own args 1>
    %   <body of TSCC proc 1>
    %   <copy own output args 1 to tscc output args>
    %   goto EndLabel;
    % ...
    % top_of_proc_N:
    %   <copy tscc args N to own args N>
    %   <body of TSCC proc N>
    %   <copy own output args N to tscc output args>
    %   goto EndLabel;
    % EndLabel:
    %   <for tscc byref output args, copy value to *ptr>
    %   return <tscc copied output args>;
    %
    % A tail call to TSCC proc i can just
    %
    % - assign the actual parameters to tscc args i, and
    % - goto `top_of_proc_i'.
    %
:- pred make_container_proc_with_label_goto(list(mlds_stmt)::in, mlds_stmt::in,
    proc_id_in_tscc::in, prog_context::in, list(proc_stmt_info)::in,
    list(mlds_stmt)::out) is det.

make_container_proc_with_label_goto(CopyOutValThroughPtrStmts, ReturnStmt,
        EntryProc, EntryProcContext, ProcStmtInfos, WrappedStmts) :-
    EndLabel = "tscc_end",
    EndLabelStmt = ml_stmt_label(EndLabel, EntryProcContext),
    GotoEndStmt = ml_stmt_goto(goto_label(EndLabel), EntryProcContext),
    list.map(make_wrapped_proc_with_label_goto(GotoEndStmt), ProcStmtInfos,
        ProcWrappedStmtLists),
    list.condense(ProcWrappedStmtLists, ProcWrappedStmts),
    EntryStartLabel =
        generate_tail_rec_start_label(tscc_self_and_mutual_rec, EntryProc),
    GotoEntryStmt =
        ml_stmt_goto(goto_label(EntryStartLabel), EntryProcContext),
    WrappedStmts = [GotoEntryStmt | ProcWrappedStmts] ++
        [EndLabelStmt | CopyOutValThroughPtrStmts] ++ [ReturnStmt].

:- pred make_wrapped_proc_with_label_goto(mlds_stmt::in, proc_stmt_info::in,
    list(mlds_stmt)::out) is det.

make_wrapped_proc_with_label_goto(GotoEndStmt, ProcStmtInfo, LabelProcStmts) :-
    ProcStmtInfo = proc_stmt_info(IdInTscc, _LoopLocalVars, ProcStmt,
        ProcContext),
    StartLabel =
        generate_tail_rec_start_label(tscc_self_and_mutual_rec, IdInTscc),
    StartLabelStmt = ml_stmt_label(StartLabel, ProcContext),
    LabelProcStmts =
        [StartLabelStmt | append_to_stmt(ProcStmt, [GotoEndStmt])].

%---------------------%

    % We wrap the statements we generate for each TSCC procedure like this:
    %
    % proc_selector = <entry_proc>;
    % while (TRUE) {
    %   switch (proc_selector) {
    %       case 1:
    %           <copy tscc input args 1 to own input args 1>
    %           <body of TSCC proc 1>
    %           <copy own output args 1 to tscc output args>
    %           break;
    %       ...
    %       case N:
    %           <copy tscc args N to own args N>
    %           <body of TSCC proc N>
    %           <copy own output args N to tscc output args>
    %           break;
    %   }
    %   break;
    % }
    % <for tscc byref output args, copy value to *ptr>
    % return <tscc copied output args>;
    %
    % A tail call to TSCC proc i can just
    %
    % - assign the actual parameters to tscc args i,
    % - set proc_selector to i, and
    % - execute `continue'.
    %
:- pred make_container_proc_with_while_continue(list(mlds_stmt)::in,
    mlds_stmt::in, proc_id_in_tscc::in, prog_context::in,
    list(proc_stmt_info)::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out) is det.

make_container_proc_with_while_continue(CopyOutValThroughPtrStmts, ReturnStmt,
        EntryProc, EntryProcContext, ProcStmtInfos,
        ContainerVarDefns, WrappedStmts) :-
    GotoEndStmts = [ml_stmt_goto(goto_break_switch, EntryProcContext)],
    list.map_foldl2(make_wrapped_proc_with_while_continue(GotoEndStmts),
        ProcStmtInfos, SwitchCases,
        set.init, PossibleSwitchValues, [], AllTsccInLocalVars),

    SelectorVar = lvn_comp_var(lvnc_tscc_proc_selector),
    SelectorType = mlds_native_int_type,
    SelectorVarDefn = mlds_local_var_defn(SelectorVar, EntryProcContext,
        SelectorType, no_initializer, gc_no_stmt),
    ContainerVarDefns = [SelectorVarDefn],

    EntryProc = proc_id_in_tscc(EntryProcNum),
    SelectorVarLval = ml_local_var(SelectorVar, SelectorType),
    SetSelectorStmt = ml_stmt_atomic(
        assign(SelectorVarLval, ml_const(mlconst_int(EntryProcNum))),
        EntryProcContext),

    set.to_sorted_list(PossibleSwitchValues, PossibleSwitchValuesList),
    SwitchMin = list.det_head(PossibleSwitchValuesList),
    SwitchMax = list.det_last(PossibleSwitchValuesList),
    SwitchRange = mlds_switch_range(SwitchMin, SwitchMax),
    Default = default_is_unreachable,
    SwitchStmt = ml_stmt_switch(SelectorType, ml_lval(SelectorVarLval),
        SwitchRange, SwitchCases, Default, EntryProcContext),
    BreakStmt = ml_stmt_goto(goto_break_loop, EntryProcContext),
    SwitchBreakStmt = ml_stmt_block([], [], [SwitchStmt, BreakStmt],
        EntryProcContext),
    LoopLocalVars = [SelectorVar | AllTsccInLocalVars],
    LoopStmt = ml_stmt_while(may_loop_zero_times, ml_const(mlconst_true),
        SwitchBreakStmt, LoopLocalVars, EntryProcContext),
    WrappedStmts = [SetSelectorStmt, LoopStmt] ++
        CopyOutValThroughPtrStmts ++ [ReturnStmt].

:- pred make_wrapped_proc_with_while_continue(list(mlds_stmt)::in,
    proc_stmt_info::in, mlds_switch_case::out, set(int)::in, set(int)::out,
    list(mlds_local_var_name)::in, list(mlds_local_var_name)::out) is det.

make_wrapped_proc_with_while_continue(GotoEndStmts, ProcStmtInfo, SwitchCase,
        !PossibleSwitchValues, !AllLoopLocalVars) :-
    ProcStmtInfo = proc_stmt_info(IdInTscc, LoopLocalVars, ProcStmt,
        ProcContext),
    IdInTscc = proc_id_in_tscc(IdInTsccNum),
    MatchCond = match_value(ml_const(mlconst_int(IdInTsccNum))),
    SwitchStmt = ml_gen_block([], [], append_to_stmt(ProcStmt, GotoEndStmts),
        ProcContext),
    SwitchCase = mlds_switch_case(MatchCond, [], SwitchStmt),
    set.insert(IdInTsccNum, !PossibleSwitchValues),
    !:AllLoopLocalVars = LoopLocalVars ++ !.AllLoopLocalVars.

%---------------------%

:- func append_to_stmt(mlds_stmt, list(mlds_stmt)) = list(mlds_stmt).

append_to_stmt(BaseStmt, EndStmts) = Stmts :-
    ( if
        BaseStmt = ml_stmt_block(LocalVarDefns, FuncDefns, BaseStmts, Context)
    then
        Stmts = [ml_stmt_block(LocalVarDefns, FuncDefns,
            BaseStmts ++ EndStmts, Context)]
    else
        Stmts = [BaseStmt | EndStmts]
    ).

%---------------------------------------------------------------------------%

    % Return the declaration flags appropriate for a procedure definition.
    %
:- func ml_gen_proc_decl_flags(module_info, pred_id, proc_id)
    = mlds_function_decl_flags.

ml_gen_proc_decl_flags(ModuleInfo, PredId, ProcId) = DeclFlags :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ( if procedure_is_exported(ModuleInfo, PredInfo, ProcId) then
        Access = func_public
    else
        Access = func_private
    ),
    DeclFlags = mlds_function_decl_flags(Access, one_copy).

    % Generate the code for a procedure body.
    %
:- pred ml_gen_proc_body(code_model::in, list(var_mvar_type_mode)::in,
    list(prog_var)::in, hlds_goal::in, seen_at_label_map::in,
    list(mlds_local_var_defn)::out, list(mlds_function_defn)::out,
    list(mlds_stmt)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_proc_body(CodeModel, ArgTuples, CopiedOutputVars, Goal, SeenAtLabelMap,
        LocalVarDefns, FuncDefns, Stmts, !Info) :-
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

    ml_gen_convert_headvars(ArgTuples, CopiedOutputVars, Context,
        ConvLocalVarDefns, ConvInputStmts, ConvOutputStmts, !Info),
    ( if
        ConvLocalVarDefns = [],
        ConvInputStmts = [],
        ConvOutputStmts = []
    then
        % No boxing/unboxing/casting required.
        ml_gen_goal(CodeModel, Goal, LocalVarDefns1, FuncDefns0, Stmts1, !Info)
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
        LocalVarDefns1 = ConvLocalVarDefns ++ LocalVarDefns0
    ),
    ml_gen_info_get_globals(!.Info, Globals),
    globals.lookup_bool_option(Globals, eliminate_unused_mlds_assigns,
        EliminateUnusedAssigns),
    (
        EliminateUnusedAssigns = no,
        LocalVarDefns = LocalVarDefns1,
        FuncDefns = FuncDefns0,
        Stmts = Stmts1
    ;
        EliminateUnusedAssigns = yes,
        list.map((func(var_mvar_type_mode(_, LocalVar, _, _)) = LocalVar),
            ArgTuples) = ArgLocalVars,
        (
            ( CodeModel = model_det
            ; CodeModel = model_non
            ),
            OutsideVars = ArgLocalVars
        ;
            CodeModel = model_semi,
            OutsideVars = [lvn_comp_var(lvnc_succeeded) | ArgLocalVars]
        ),
        optimize_away_unused_assigns_in_proc_body(OutsideVars,
            SeenAtLabelMap, LocalVarDefns1, LocalVarDefns,
            FuncDefns0, FuncDefns, Stmts1, Stmts)
    ).

    % In certain cases -- for example existentially typed procedures,
    % or unification/compare procedures for equivalence types --
    % the parameter types may not match the types of the head variables.
    % In such cases, we need to box/unbox/cast them to the right type.
    % This procedure handles that.
    %
:- pred ml_gen_convert_headvars(list(var_mvar_type_mode)::in,
    list(prog_var)::in, prog_context::in,
    list(mlds_local_var_defn)::out, list(mlds_stmt)::out, list(mlds_stmt)::out,
    ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_convert_headvars([], _CopiedOutputVars, _Context, [], [], [], !Info).
ml_gen_convert_headvars([ArgTuple | ArgTuples], CopiedOutputVars, Context,
        LocalVarDefns, InputStmts, OutputStmts, !Info) :-
    ArgTuple = var_mvar_type_mode(Var, MLDSVarName, HeadType, TopFunctorMode),
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
        ml_gen_convert_headvars(ArgTuples, CopiedOutputVars, Context,
            LocalVarDefns, InputStmts, OutputStmts, !Info)
    else
        % Generate the lval for the head variable.
        ml_gen_var_with_type(!.Info, Var, HeadType, HeadVarLval),

        % Generate code to box or unbox that head variable,
        % to convert its type from HeadType to BodyType.
        ml_gen_box_or_unbox_lval(HeadType, BodyType, bp_native_if_possible,
            HeadVarLval, MLDSVarName, Context, no, 0, BodyLval,
            ConvLocalVarDefns, ConvInputStmts, ConvOutputStmts, !Info),

        % Ensure that for any uses of this variable in the procedure body,
        % we use the BodyLval (which has type BodyType) rather than the
        % HeadVarLval (which has type HeadType).
        ml_gen_info_set_var_lval(Var, BodyLval, !Info),

        ml_gen_convert_headvars(ArgTuples, CopiedOutputVars, Context,
            LocalVarDefnsTail, InputStmtsTail, OutputStmtsTail, !Info),

        % Add the code to convert this input or output.
        ml_gen_info_get_byref_output_vars(!.Info, ByRefOutputVars),
        ( if
            ( set_of_var.member(ByRefOutputVars, Var)
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
    ).

:- pred ml_gen_local_var_defns_for_copied_output_vars(proc_info::in,
    prog_context::in, list(var_mvar_type_mode)::in, list(prog_var)::in,
    list(mlds_local_var_defn)::out, ml_gen_info::in, ml_gen_info::out) is det.

ml_gen_local_var_defns_for_copied_output_vars(ProcInfo, Context, ArgTuples,
        CopiedOutputVars, OutputVarLocalDefns, !Info) :-
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
        HeadVars = list.map((func(var_mvar_type_mode(HV, _, _, _)) = HV),
            ArgTuples),
        HeadTypes = list.map((func(var_mvar_type_mode(_, _, HT, _)) = HT),
            ArgTuples),
        vartypes_overlay_corresponding_lists(HeadVars, HeadTypes,
            VarTypes, UpdatedVarTypes),
        ml_gen_local_var_decls(VarSet, UpdatedVarTypes,
            Context, CopiedOutputVars, OutputVarLocalDefns, !Info)
    ).

:- pred ml_gen_maybe_local_var_defn_for_succeeded(ml_gen_info::in,
    prog_context::in, list(mlds_local_var_defn)::out) is det.

ml_gen_maybe_local_var_defn_for_succeeded(Info, Context, SucceededVarDefns) :-
    ml_gen_info_get_used_succeeded_var(Info, UsedSucceededVar),
    (
        UsedSucceededVar = no,
        SucceededVarDefns = []
    ;
        UsedSucceededVar = yes,
        SucceededVarDefns = [ml_gen_succeeded_var_decl(Context)]
    ).

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
        Stmt = ml_stmt_while(_LoopKind, _CondRval, SubStmt, _LoopLocalVars,
            _Context),
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
        ; Stmt = ml_stmt_call(_Sig, _Callee, _Args, _Ret, _Kind, _Context)
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
:- pragma consider_used(describe_pred_proc_ids/5).

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
