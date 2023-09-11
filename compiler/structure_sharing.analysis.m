%---------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%---------------------------------------------------------------------------%
% Copyright (C) 2005-2012 The University of Melbourne.
% Copyright (C) 2017 The Mercury Team.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%---------------------------------------------------------------------------%
%
% File: structure_sharing.analysis.m.
% Main authors: nancy.
%
% Implementation of the structure sharing analysis needed for compile-time
% garbage collection (CTGC).
%
%---------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_sharing.analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.
:- import_module hlds.hlds_module.

:- import_module io.

%---------------------------------------------------------------------------%

    % Perform structure sharing analysis on the procedures defined in the
    % current module.
    %
:- pred perform_structure_sharing_analysis(io.text_output_stream::in,
    module_info::in, module_info::out) is det.

%---------------------------------------------------------------------------%

:- type structure_sharing_call.
:- type structure_sharing_answer.
:- type structure_sharing_func_info.

    % Only answer patterns actually require structure_sharing_func_info for
    % comparisons.
    %
:- instance analysis(structure_sharing_func_info,
    structure_sharing_call, structure_sharing_answer).

:- instance call_pattern(structure_sharing_func_info, structure_sharing_call).
:- instance partial_order(structure_sharing_func_info,
    structure_sharing_call).
:- instance to_term(structure_sharing_call).

:- instance answer_pattern(structure_sharing_func_info,
    structure_sharing_answer).
:- instance partial_order(structure_sharing_func_info,
    structure_sharing_answer).
:- instance to_term(structure_sharing_answer).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

:- implementation.

:- import_module check_hlds.
:- import_module check_hlds.simplify.
:- import_module check_hlds.simplify.simplify_proc.
:- import_module check_hlds.simplify.simplify_tasks.
:- import_module hlds.arg_info.
:- import_module hlds.hlds_dependency_graph.
:- import_module hlds.hlds_goal.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.hlds_pred.
:- import_module hlds.passes_aux.
:- import_module hlds.status.
:- import_module libs.
:- import_module libs.dependency_graph.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.op_mode.
:- import_module libs.options.
:- import_module ll_backend.
:- import_module ll_backend.liveness.
:- import_module mdbcomp.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.
:- import_module parse_tree.parse_tree_out_misc.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_data_pragma.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.var_table.
:- import_module transform_hlds.ctgc.fixpoint_table.
:- import_module transform_hlds.ctgc.structure_sharing.domain.
:- import_module transform_hlds.ctgc.util.
:- import_module transform_hlds.intermod_analysis.
:- import_module transform_hlds.mmc_analysis.

:- import_module bool.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.
:- import_module term_context.
:- import_module term_conversion.

%---------------------------------------------------------------------------%

    % During analysis we accumulate a list of imported procedures whose
    % answers this module depends on. This doesn't include `opt_imported'
    % procedures nor procedures that we can just predict the results for.
    %
:- type dep_procs == list(pred_proc_id).

%---------------------------------------------------------------------------%

perform_structure_sharing_analysis(ProgressStream, !ModuleInfo) :-
    % Process all the imported sharing information.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        process_intermod_analysis_imported_sharing(!ModuleInfo)
    ;
        IntermodAnalysis = no,
        process_imported_sharing(!ModuleInfo)
    ),

    % Annotate the HLDS with liveness information. The liveness analysis
    % requires argument passing information.
    generate_arg_info(!ModuleInfo),
    annotate_liveness(!ModuleInfo),

    % Load all structure sharing information present in the HLDS.
    LoadedSharingTable = load_structure_sharing_table(!.ModuleInfo),

    % Analyse structure sharing for the module.
    sharing_analysis(ProgressStream, LoadedSharingTable, !ModuleInfo),

    module_info_get_proc_analysis_kinds(!.ModuleInfo, ProcAnalysisKinds0),
    set.insert(pak_structure_sharing, ProcAnalysisKinds0, ProcAnalysisKinds),
    module_info_set_proc_analysis_kinds(ProcAnalysisKinds, !ModuleInfo).

%---------------------------------------------------------------------------%
%
% Preliminary steps.
%

    % Process the imported sharing information from .opt files
    %
:- pred process_imported_sharing(module_info::in, module_info::out) is det.

process_imported_sharing(!ModuleInfo):-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(process_imported_sharing_in_pred, PredIds, !ModuleInfo).

:- pred process_imported_sharing_in_pred(pred_id::in, module_info::in,
    module_info::out) is det.

process_imported_sharing_in_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    process_imported_sharing_in_procs(PredInfo0, PredInfo),
    module_info_set_pred_info(PredId, PredInfo, !ModuleInfo).

:- pred process_imported_sharing_in_procs(pred_info::in,
    pred_info::out) is det.

process_imported_sharing_in_procs(!PredInfo) :-
    some [!ProcTable] (
        pred_info_get_proc_table(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_all_procids(!.PredInfo),
        list.foldl(process_imported_sharing_in_proc(!.PredInfo),
            ProcIds, !ProcTable),
        pred_info_set_proc_table(!.ProcTable, !PredInfo)
    ).

:- pred process_imported_sharing_in_proc(pred_info::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

process_imported_sharing_in_proc(PredInfo, ProcId, !ProcTable) :-
    some [!ProcInfo] (
        map.lookup(!.ProcTable, ProcId, !:ProcInfo),
        ( if
            proc_info_get_imported_structure_sharing(!.ProcInfo,
                ImpHeadVars, ImpTypes, ImpSharing)
        then
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            pred_info_get_arg_types(PredInfo, HeadVarTypes),
            map.from_corresponding_lists(ImpHeadVars, HeadVars, VarRenaming),
            some [!TypeSubst] (
                !:TypeSubst = map.init,
                ( if
                    type_unify_list(ImpTypes, HeadVarTypes, [], !.TypeSubst,
                        TypeSubstNew)
                then
                    !:TypeSubst = TypeSubstNew
                else
                    true
                ),
                rename_structure_sharing_domain(VarRenaming, !.TypeSubst,
                    ImpSharing, Sharing)
            ),
            % Optimality does not apply to `--intermodule-optimisation'
            % system, only `--intermodule-analysis'.
            proc_info_set_structure_sharing(
                structure_sharing_domain_and_status(Sharing, optimal),
                !ProcInfo),
            proc_info_reset_imported_structure_sharing(!ProcInfo),
            map.det_update(ProcId, !.ProcInfo, !ProcTable)
        else
            true
        )
    ).

%---------------------------------------------------------------------------%

    % Process the intermodule imported sharing information from the analysis
    % framework
    %
:- pred process_intermod_analysis_imported_sharing(module_info::in,
    module_info::out) is det.

process_intermod_analysis_imported_sharing(!ModuleInfo):-
    module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
    list.foldl(process_intermod_analysis_imported_sharing_in_pred, PredIds,
        !ModuleInfo).

:- pred process_intermod_analysis_imported_sharing_in_pred(pred_id::in,
    module_info::in, module_info::out) is det.

process_intermod_analysis_imported_sharing_in_pred(PredId, !ModuleInfo) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo0),
    ( if pred_info_is_imported_not_external(PredInfo0) then
        module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo),
        process_intermod_analysis_imported_sharing_in_procs(!.ModuleInfo,
            AnalysisInfo, PredId, PredInfo0, PredInfo),
        module_info_set_pred_info(PredId, PredInfo, !ModuleInfo)
    else
        true
    ).

:- pred process_intermod_analysis_imported_sharing_in_procs(module_info::in,
    analysis_info::in, pred_id::in, pred_info::in, pred_info::out) is det.

process_intermod_analysis_imported_sharing_in_procs(ModuleInfo, AnalysisInfo,
        PredId, !PredInfo) :-
    some [!ProcTable] (
        pred_info_get_proc_table(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_all_procids(!.PredInfo),
        list.foldl(
            process_intermod_analysis_imported_sharing_in_proc(ModuleInfo,
                AnalysisInfo, PredId, !.PredInfo),
            ProcIds, !ProcTable),
        pred_info_set_proc_table(!.ProcTable, !PredInfo)
    ).

:- pred process_intermod_analysis_imported_sharing_in_proc(module_info::in,
    analysis_info::in, pred_id::in, pred_info::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

process_intermod_analysis_imported_sharing_in_proc(ModuleInfo, AnalysisInfo,
        PredId, PredInfo, ProcId, !ProcTable) :-
    PPId = proc(PredId, ProcId),
    some [!ProcInfo] (
        map.lookup(!.ProcTable, ProcId, !:ProcInfo),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        FuncInfo = structure_sharing_func_info(ModuleInfo, !.ProcInfo),
        lookup_best_result(AnalysisInfo, ModuleName, FuncId, FuncInfo,
            structure_sharing_call, MaybeBestResult),
        (
            MaybeBestResult = yes(analysis_result(_Call, Answer,
                ResultStatus)),
            pred_info_get_arg_types(PredInfo, HeadVarTypes),
            structure_sharing_answer_to_domain(yes(PPId), HeadVarTypes,
                !.ProcInfo, Answer, Sharing),
            proc_info_set_structure_sharing(
                structure_sharing_domain_and_status(Sharing, ResultStatus),
                !ProcInfo),
            map.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            MaybeBestResult = no
        )
    ).

:- pred structure_sharing_answer_to_domain(maybe(pred_proc_id)::in,
    list(mer_type)::in, proc_info::in, structure_sharing_answer::in,
    structure_sharing_domain::out) is det.

structure_sharing_answer_to_domain(MaybePPId, HeadVarTypes, ProcInfo, Answer,
        Sharing) :-
    (
        Answer = structure_sharing_answer_bottom,
        Sharing = structure_sharing_bottom
    ;
        Answer = structure_sharing_answer_top,
        (
            MaybePPId = yes(PPId),
            TopReason = set.make_singleton_set(
                top_from_lookup(shroud_pred_proc_id(PPId)))
        ;
            MaybePPId = no,
            TopReason = set.init
        ),
        Sharing = structure_sharing_top(TopReason)
    ;
        Answer = structure_sharing_answer_real(ImpHeadVars, ImpTypes,
            ImpSharingPairs),
        proc_info_get_headvars(ProcInfo, HeadVars),
        map.from_corresponding_lists(ImpHeadVars, HeadVars, VarRenaming),
        ( if
            type_unify_list(ImpTypes, HeadVarTypes, [], map.init, TypeSubst)
        then
            rename_structure_sharing(VarRenaming, TypeSubst, ImpSharingPairs,
                SharingPairs),
            Sharing = structure_sharing_real(SharingPairs)
        else
            unexpected($pred, "type_unify_list failed")
        )
    ).

%---------------------------------------------------------------------------%
%---------------------------------------------------------------------------%

    % Annotate the HLDS with pre-birth and post-death information, as
    % used by the liveness pass (liveness.m). This information is used to
    % eliminate useless sharing pairs during sharing analysis.
    %
:- pred annotate_liveness(module_info::in, module_info::out) is det.

annotate_liveness(!ModuleInfo) :-
    process_valid_nonimported_procs(
        update_module(simplify_and_detect_liveness_proc), !ModuleInfo).

:- pred simplify_and_detect_liveness_proc(pred_proc_id::in,
    proc_info::in, proc_info::out, module_info::in, module_info::out) is det.

simplify_and_detect_liveness_proc(PredProcId, !ProcInfo, !ModuleInfo) :-
    % Liveness annotation expects the procedure to have been simplified.
    % For example, an if-then-else with an `erroneous' condition will cause
    % an assertion failure if it is not simplified away.
    module_info_get_globals(!.ModuleInfo, Globals),
    SimplifyTasks = list_to_simplify_tasks(Globals, []),
    PredProcId = proc(PredId, ProcId),
    MaybeProgressStream = maybe.no,
    simplify_proc(MaybeProgressStream, SimplifyTasks, PredId, ProcId,
        !ModuleInfo, !ProcInfo),
    detect_liveness_proc(!.ModuleInfo, PredProcId, !ProcInfo).

%---------------------------------------------------------------------------%

:- pred sharing_analysis(io.text_output_stream::in, sharing_as_table::in,
    module_info::in, module_info::out) is det.

sharing_analysis(ProgressStream, !.SharingTable, !ModuleInfo) :-
    % Perform a bottom-up traversal of the SCCs in the program,
    % analysing structure sharing in each one as we go.
    module_info_ensure_dependency_info(!ModuleInfo, DepInfo),
    SCCs = dependency_info_get_bottom_up_sccs(DepInfo),
    list.foldl2(analyse_scc(ProgressStream, !.ModuleInfo),
        SCCs, !SharingTable, [], DepProcs),

    % Record the sharing results in the HLDS.
    map.foldl(save_sharing_in_module_info, !.SharingTable, !ModuleInfo),

    % If making a `.analysis' file, record structure sharing results, analysis
    % dependencies, assumed answers and requests in the analysis framework.
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.get_op_mode(Globals, OpMode),
    ( if
        OpMode = opm_top_args(opma_augment(opmau_make_analysis_registry), _)
    then
        some [!AnalysisInfo] (
            module_info_get_analysis_info(!.ModuleInfo, !:AnalysisInfo),
            module_info_get_valid_pred_ids(!.ModuleInfo, PredIds),
            list.foldl(maybe_record_sharing_analysis_result(!.ModuleInfo,
                !.SharingTable), PredIds, !AnalysisInfo),
            list.foldl(handle_dep_procs(!.ModuleInfo), DepProcs,
                !AnalysisInfo),
            module_info_set_analysis_info(!.AnalysisInfo, !ModuleInfo)
        )
    else
        true
    ).

:- pred save_sharing_in_module_info(pred_proc_id::in,
    sharing_as_and_status::in, module_info::in, module_info::out) is det.

save_sharing_in_module_info(PPId, SharingAs_Status, !ModuleInfo) :-
    SharingAs_Status = sharing_as_and_status(SharingAs, Status),
    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    SharingDomain = to_structure_sharing_domain(SharingAs),
    proc_info_set_structure_sharing(
        structure_sharing_domain_and_status(SharingDomain, Status),
        ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

:- pred analyse_scc(io.text_output_stream::in, module_info::in, scc::in,
    sharing_as_table::in, sharing_as_table::out,
    dep_procs::in, dep_procs::out) is det.

analyse_scc(ProgressStream, ModuleInfo, SCC, !SharingTable, !DepProcs) :-
    set.to_sorted_list(SCC, SCCProcs),
    ( if some_preds_require_no_analysis(ModuleInfo, SCC) then
        % At least one procedure in the SCC requires that we don't analyse it.
        % We update the sharing table otherwise procedures which call it will
        % not be able to find a result, and therefore conclude that the
        % analysis is suboptimal.
        ProcsStrs = list.map(pred_proc_id_to_dev_string(ModuleInfo), SCCProcs),
        ProcsStr = string.join_list(", ", ProcsStrs),
        Msg = "SCC cannot be analysed: " ++ ProcsStr,
        SharingAs = sharing_as_top_sharing(top_cannot_improve(Msg)),
        SharingAndStatus = sharing_as_and_status(SharingAs, optimal),
        set.foldl(
            ( pred(PPId::in, ST0::in, ST::out) is det :-
                sharing_as_table_set(PPId, SharingAndStatus, ST0, ST)
            ), SCC, !SharingTable)
    else
        FixpointTable0 = ss_fixpoint_table_init(SCCProcs),
        analyse_scc_until_fixpoint(ProgressStream, ModuleInfo, SCCProcs,
            !.SharingTable, FixpointTable0, FixpointTable, !DepProcs),
        set.foldl(update_sharing_in_table(FixpointTable), SCC, !SharingTable)
    ).

:- pred analyse_scc_until_fixpoint(io.text_output_stream::in, module_info::in,
    list(pred_proc_id)::in, sharing_as_table::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

analyse_scc_until_fixpoint(ProgressStream, ModuleInfo, SCC,
        SharingTable, !FixpointTable, !DepProcs) :-
    % Abort if the analysis is taking too long. It is probably a bug.
    Run = ss_fixpoint_table_which_run(!.FixpointTable),
    ( if Run > max_runs then
        unexpected($pred, "fixpoint not reached after "
            ++ string.from_int(max_runs) ++ " runs")
    else
        true
    ),

    list.foldl2(analyse_pred_proc(ProgressStream, ModuleInfo, SharingTable),
        SCC, !FixpointTable, !DepProcs),
    ( if ss_fixpoint_table_stable(!.FixpointTable) then
        true
    else
        ss_fixpoint_table_new_run(!FixpointTable),
        analyse_scc_until_fixpoint(ProgressStream, ModuleInfo, SCC,
            SharingTable, !FixpointTable, !DepProcs)
    ).

:- func max_runs = int.

max_runs = 100.

%---------------------------------------------------------------------------%
%
% Perform structure sharing analysis on a procedure.
%

:- pred analyse_pred_proc(io.text_output_stream::in, module_info::in,
    sharing_as_table::in, pred_proc_id::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    dep_procs::in, dep_procs::out) is det.

analyse_pred_proc(ProgressStream, ModuleInfo, SharingTable, PPId,
        !FixpointTable, !DepProcs) :-
    % Collect relevant compiler options.
    module_info_get_globals(ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, Verbose),
    globals.lookup_int_option(Globals, structure_sharing_widening,
        WideningLimit),

    % Collect relevant procedure information.
    module_info_pred_proc_info(ModuleInfo, PPId, PredInfo, ProcInfo),
    proc_info_get_headvars(ProcInfo, HeadVars),

    % Write progress message for the start of analysing current procedure.
    Run = ss_fixpoint_table_which_run(!.FixpointTable),
    TabledAsDescr = ss_fixpoint_table_get_short_description(PPId,
        !.FixpointTable),
    trace [io(!IO)] (
        maybe_write_proc_progress_message(ProgressStream, ModuleInfo,
            "Sharing analysis (run " ++ string.int_to_string(Run) ++ ")",
            PPId, !IO)
    ),

    % In some cases the sharing can be predicted to be bottom, in which
    % case a full sharing analysis is not needed.
    %
    some [!Sharing] (
        !:Sharing = sharing_as_init,
        ( if
            bottom_sharing_is_safe_approximation(ModuleInfo, PredInfo,
                ProcInfo)
        then
            trace [io(!IO)] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                maybe_write_string(DebugStream, Verbose,
                    "\t\t: bottom predicted", !IO)
            ),
            Status = optimal
        else
            % Start analysis.
            proc_info_get_goal(ProcInfo, Goal),
            analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable,
                Verbose, Goal, !FixpointTable, !DepProcs, !Sharing,
                optimal, Status),
            FullAsDescr = sharing_as_short_description(!.Sharing),

            sharing_as_project(HeadVars, !Sharing),
            ProjAsDescr = sharing_as_short_description(!.Sharing),

            proc_info_get_var_table(ProcInfo, VarTable),
            domain.apply_widening(ModuleInfo, VarTable, WideningLimit,
                WideningDone, !Sharing),
            (
                WideningDone = yes,
                WidenAsDescr = sharing_as_short_description(!.Sharing)
            ;
                WideningDone = no,
                WidenAsDescr = "-"
            ),

            trace [io(!IO)] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                maybe_write_string(DebugStream, Verbose,
                    "\n\t\t: " ++ TabledAsDescr ++ "->" ++
                    FullAsDescr ++ "/" ++ ProjAsDescr ++ "/" ++ WidenAsDescr,
                    !IO)
            )
        ),
        SharingAs_Status = sharing_as_and_status(!.Sharing, Status),
        ss_fixpoint_table_new_as(ModuleInfo, ProcInfo, PPId, SharingAs_Status,
            !FixpointTable)
    ),
    Desc = ss_fixpoint_table_description(!.FixpointTable),
    trace [io(!IO)] (
        get_debug_output_stream(ModuleInfo, DebugStream, !IO),
        maybe_write_string(DebugStream, Verbose,
            "\t\t (ft = " ++ Desc ++ ")\n", !IO)
    ).

%---------------------------------------------------------------------------%
%
% Structure sharing analysis of goals.
%

:- pred analyse_goal(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, bool::in, hlds_goal::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    dep_procs::in, dep_procs::out, sharing_as::in, sharing_as::out,
    analysis_status::in, analysis_status::out) is det.

analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose, Goal,
        !FixpointTable, !DepProcs, !SharingAs, !Status) :-
    Goal = hlds_goal(GoalExpr, GoalInfo),
    (
        GoalExpr = conj(ConjType, Goals),
        (
            ConjType = plain_conj,
            list.foldl4(analyse_goal_with_progress(ModuleInfo, PredInfo,
                ProcInfo, SharingTable, Verbose), Goals,
                !FixpointTable, !DepProcs, !SharingAs, !Status)
        ;
            ConjType = parallel_conj,
            Context = goal_info_get_context(GoalInfo),
            context_to_string(Context, ContextString),
            !:SharingAs = sharing_as_top_sharing_accumulate(
                top_cannot_improve("par_conj (" ++ ContextString ++ ")"),
                !.SharingAs)
        )
    ;
        GoalExpr = plain_call(CalleePredId, CalleeProcId, CallArgs,_, _, _),
        CalleePPId = proc(CalleePredId, CalleeProcId),
        lookup_sharing(ModuleInfo, SharingTable, CalleePPId,
            !FixpointTable, CalleeSharing, CalleeStatus, IsPredicted),

        % If the called procedure was imported (not opt_imported) and its
        % result is not predictable, then remember that this module depends on
        % the results for that procedure.
        ( if
            IsPredicted = no,
            pred_info_get_status(PredInfo, PredStatus),
            pred_status_defined_in_this_module(PredStatus) = yes,
            module_info_pred_info(ModuleInfo, CalleePredId, CalleePredInfo),
            pred_info_is_imported_not_external(CalleePredInfo),
            not is_unify_index_or_compare_pred(CalleePredInfo)
        then
            !:DepProcs = [CalleePPId | !.DepProcs]
        else
            true
        ),

        % Rename
        proc_info_get_var_table(ProcInfo, CallerVarTable),
        lookup_var_types(CallerVarTable, CallArgs, ActualTypes),
        pred_info_get_typevarset(PredInfo, CallerTypeVarSet),
        pred_info_get_univ_quant_tvars(PredInfo, CallerHeadParams),
        sharing_as_rename_using_module_info(ModuleInfo, CalleePPId, CallArgs,
            ActualTypes, CallerTypeVarSet, CallerHeadParams,
            CalleeSharing, RenamedSharing),

        % Combine
        !:SharingAs = sharing_as_comb(ModuleInfo, ProcInfo,
            RenamedSharing, !.SharingAs),
        !:Status = lub(CalleeStatus, !.Status)
    ;
        GoalExpr = generic_call(GenDetails, CallArgs, Modes, _MaybeArgRegs,
            _Detism),
        analyse_generic_call(ModuleInfo, ProcInfo, GenDetails, CallArgs,
            Modes, GoalInfo, !SharingAs)
    ;
        GoalExpr = unify(_, _, _, Unification, _),
        !:SharingAs = add_unify_sharing(ModuleInfo, ProcInfo, Unification,
            GoalInfo, !.SharingAs)
    ;
        GoalExpr = disj(Goals),
        list.foldl4(
            analyse_disj(ModuleInfo, PredInfo, ProcInfo,
                SharingTable, !.SharingAs, Verbose),
            Goals, !FixpointTable, !DepProcs,
            sharing_as_init, !:SharingAs, !Status)
    ;
        GoalExpr = switch(_, _, Cases),
        list.foldl4(
            analyse_case(ModuleInfo, PredInfo, ProcInfo,
                SharingTable, !.SharingAs, Verbose),
            Cases, !FixpointTable, !DepProcs,
            sharing_as_init, !:SharingAs, !Status)
    ;
        GoalExpr = negation(_Goal)
        % XXX Check theory, but a negated goal can not create bindings,
        % hence it also can not create additional sharing.
    ;
        GoalExpr = scope(Reason, SubGoal),
        ( if Reason = from_ground_term(_, from_ground_term_construct) then
            % Ground terms cannot introduce sharing.
            true
        else
            % XXX Check theory.
            analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose,
                SubGoal, !FixpointTable, !DepProcs, !SharingAs, !Status)
        )
    ;
        GoalExpr = if_then_else(_, IfGoal, ThenGoal, ElseGoal),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose,
            IfGoal, !FixpointTable, !DepProcs,
            !.SharingAs, IfSharingAs, !Status),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose,
            ThenGoal, !FixpointTable, !DepProcs,
            IfSharingAs, ThenSharingAs, !Status),
        analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose,
            ElseGoal, !FixpointTable, !DepProcs,
            !.SharingAs, ElseSharingAs, !Status),
        !:SharingAs = sharing_as_least_upper_bound(ModuleInfo, ProcInfo,
            ThenSharingAs, ElseSharingAs)
    ;
        GoalExpr = call_foreign_proc(Attributes, ForeignPredId, ForeignProcId,
            Args, _ExtraArgs, _MaybeTraceRuntimeCond, _Impl),
        Context = goal_info_get_context(GoalInfo),
        ForeignPPId = proc(ForeignPredId, ForeignProcId),
        add_foreign_proc_sharing(ModuleInfo, PredInfo, ProcInfo, ForeignPPId,
            Attributes, Args, Context, !SharingAs)
    ;
        GoalExpr = shorthand(_),
        % These should have been expanded out by now.
        unexpected($pred, "shorthand")
    ).

:- pred analyse_goal_with_progress(module_info::in, pred_info::in,
    proc_info::in, sharing_as_table::in, bool::in, hlds_goal::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    dep_procs::in, dep_procs::out, sharing_as::in, sharing_as::out,
    analysis_status::in, analysis_status::out) is det.

analyse_goal_with_progress(ModuleInfo, PredInfo, ProcInfo, SharingTable,
        Verbose, Goal, !FixpointTable, !DepProcs, !SharingAs, !Status) :-
    (
        Verbose = yes,
        trace [io(!IO)] (
            get_debug_output_stream(ModuleInfo, DebugStream, !IO),
            io.write_char(DebugStream, '.', !IO),
            io.flush_output(DebugStream, !IO)
        )
    ;
        Verbose = no
    ),
    analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose, Goal,
        !FixpointTable, !DepProcs, !SharingAs, !Status).

%---------------------------------------------------------------------------%
%
% Additional code for analysing disjuctions.
%

:- pred analyse_disj(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, sharing_as::in, bool::in, hlds_goal::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    dep_procs::in, dep_procs::out, sharing_as::in, sharing_as::out,
    analysis_status::in, analysis_status::out) is det.

analyse_disj(ModuleInfo, PredInfo, ProcInfo, SharingTable, SharingBeforeDisj,
        Verbose, Goal, !FixpointTable, !DepProcs, !Sharing, !Status) :-
    analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose, Goal,
        !FixpointTable, !DepProcs, SharingBeforeDisj, GoalSharing,
        !Status),
    !:Sharing = sharing_as_least_upper_bound(ModuleInfo, ProcInfo, !.Sharing,
        GoalSharing).

%---------------------------------------------------------------------------%
%
% Additional code for analysing switches.
%

:- pred analyse_case(module_info::in, pred_info::in, proc_info::in,
    sharing_as_table::in, sharing_as::in, bool::in, case::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out,
    dep_procs::in, dep_procs::out, sharing_as::in, sharing_as::out,
    analysis_status::in, analysis_status::out) is det.

analyse_case(ModuleInfo, PredInfo, ProcInfo, SharingTable, Sharing0,
        Verbose, Case, !FixpointTable, !DepProcs, !Sharing, !Status) :-
    Case = case(_, _, Goal),
    analyse_goal(ModuleInfo, PredInfo, ProcInfo, SharingTable, Verbose, Goal,
        !FixpointTable, !DepProcs, Sharing0, CaseSharing, !Status),
    !:Sharing = sharing_as_least_upper_bound(ModuleInfo, ProcInfo, !.Sharing,
        CaseSharing).

%---------------------------------------------------------------------------%
%
% Code for handling calls.
%

    % Lookup the sharing information of a procedure identified by its
    % pred_proc_id.
    %
:- pred lookup_sharing(module_info::in, sharing_as_table::in, pred_proc_id::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out, sharing_as::out,
    analysis_status::out, bool::out) is det.

lookup_sharing(ModuleInfo, SharingTable, PPId, !FixpointTable, SharingAs,
        Status, IsPredicted) :-
    ( if
        % Check fixpoint table.
        ss_fixpoint_table_get_as(PPId, SharingAs_Status, !FixpointTable)
    then
        SharingAs_Status = sharing_as_and_status(SharingAs, Status),
        IsPredicted = no
    else
        lookup_sharing_or_predict(ModuleInfo, SharingTable, PPId, SharingAs,
            Status, IsPredicted)
    ).

:- pred analyse_generic_call(module_info::in, proc_info::in, generic_call::in,
    prog_vars::in, list(mer_mode)::in, hlds_goal_info::in, sharing_as::in,
    sharing_as::out) is det.

analyse_generic_call(ModuleInfo, ProcInfo, GenDetails, CallArgs, Modes,
        GoalInfo, !SharingAs) :-
    (
        ( GenDetails = higher_order(_, _, _, _)
        ; GenDetails = class_method(_, _, _, _)
        ),
        proc_info_get_var_table(ProcInfo, CallerVarTable),
        lookup_var_types(CallerVarTable, CallArgs, ActualTypes),
        ( if
            bottom_sharing_is_safe_approximation_by_args(ModuleInfo, Modes,
                ActualTypes)
        then
            SetToTop = no
        else
            SetToTop = yes
        )
    ;
        ( GenDetails = event_call(_) % XXX too conservative
        ; GenDetails = cast(_)
        ),
        SetToTop = yes
    ),
    (
        SetToTop = yes,
        Context = goal_info_get_context(GoalInfo),
        context_to_string(Context, ContextString),
        !:SharingAs = sharing_as_top_sharing_accumulate(
            top_cannot_improve("generic call (" ++ ContextString ++ ")"),
            !.SharingAs)
    ;
        SetToTop = no
    ).

%---------------------------------------------------------------------------%

:- pred update_sharing_in_table(ss_fixpoint_table::in, pred_proc_id::in,
    sharing_as_table::in, sharing_as_table::out) is det.

update_sharing_in_table(FixpointTable, PPId, !SharingTable) :-
    ss_fixpoint_table_get_final_as(PPId, FixpointTable, SharingAs_Status),
    sharing_as_table_set(PPId, SharingAs_Status, !SharingTable).

%---------------------------------------------------------------------------%
%
% Structure sharing fixpoint table.
%

:- type ss_fixpoint_table ==
    fixpoint_table(pred_proc_id, sharing_as_and_status).

    % Initialise the fixpoint table for the given set of pred_proc_id's.
    %
:- func ss_fixpoint_table_init(list(pred_proc_id)) = ss_fixpoint_table.

ss_fixpoint_table_init(Keys) = init_fixpoint_table(wrapped_init, Keys).

    % Add the results of a new analysis pass to the already existing
    % fixpoint table.
    %
:- pred ss_fixpoint_table_new_run(ss_fixpoint_table::in,
    ss_fixpoint_table::out) is det.

ss_fixpoint_table_new_run(!Table) :-
    fixpoint_table.new_run(!Table).

    % The fixpoint table keeps track of the number of analysis passes. This
    % predicate returns this number.
    %
:- func ss_fixpoint_table_which_run(ss_fixpoint_table) = int.

ss_fixpoint_table_which_run(Tin) = fixpoint_table.which_run(Tin).

    % A fixpoint is reached if all entries in the table are stable,
    % i.e. haven't been modified by the last analysis pass.
    %
:- pred ss_fixpoint_table_stable(ss_fixpoint_table::in) is semidet.

ss_fixpoint_table_stable(Table) :-
    fixpoint_table.fixpoint_reached(Table).

    % Give a string description of the state of the fixpoint table.
    %
:- func ss_fixpoint_table_description(ss_fixpoint_table) = string.

ss_fixpoint_table_description(Table) = fixpoint_table.description(Table).

    % Enter the newly computed structure sharing description for a given
    % procedure. If the description is different from the one that was
    % already stored for that procedure, the stability of the fixpoint
    % table is set to "unstable".
    % Software error if the procedure is not in the fixpoint table.
    %
:- pred ss_fixpoint_table_new_as(module_info::in, proc_info::in,
    pred_proc_id::in, sharing_as_and_status::in,
    ss_fixpoint_table::in, ss_fixpoint_table::out) is det.

ss_fixpoint_table_new_as(ModuleInfo, ProcInfo, Id, SharingAs, !Table) :-
    add_to_fixpoint_table(
        sharing_as_and_status_is_subsumed_by(ModuleInfo, ProcInfo),
        Id, SharingAs, !Table).

    % Retrieve the structure sharing description for a given pred_proc_id.
    %
    % If the id is part of the fixpoint table, but does not yet record any
    % sharing information about that pred_proc_id, then this means that the
    % set of pred_proc_id's to which the fixpoint table relates is mutually
    % recursive, hence the table is characterised as recursive.
    %
    % If the id is not part of the fixpoint table: fail.
    %
:- pred ss_fixpoint_table_get_as(pred_proc_id::in, sharing_as_and_status::out,
    ss_fixpoint_table::in, ss_fixpoint_table::out) is semidet.

ss_fixpoint_table_get_as(PPId, SharingAs, !Table) :-
    get_from_fixpoint_table(PPId, SharingAs, !Table).

:- func ss_fixpoint_table_get_short_description(pred_proc_id,
    ss_fixpoint_table) = string.

ss_fixpoint_table_get_short_description(PPId, Table) = Descr :-
    ( if
        ss_fixpoint_table_get_final_as_semidet(PPId, Table, SharingAs_Status)
    then
        SharingAs_Status = sharing_as_and_status(As, _Status),
        Descr = sharing_as_short_description(As)
    else
        Descr = "-"
    ).

    % Retrieve the structure sharing information without changing the table.
    % To be used after fixpoint has been reached.
    % Software error if the procedure is not in the table.
    %
:- pred ss_fixpoint_table_get_final_as(pred_proc_id::in,
    ss_fixpoint_table::in, sharing_as_and_status::out) is det.

ss_fixpoint_table_get_final_as(PPId, T, SharingAs_Status) :-
    SharingAs_Status = get_from_fixpoint_table_final(PPId, T).

    % Same as ss_fixpoint_table_get_final_as, but fails instead of aborting
    % if the procedure is not in the table.
    %
:- pred ss_fixpoint_table_get_final_as_semidet(pred_proc_id::in,
    ss_fixpoint_table::in, sharing_as_and_status::out) is semidet.

ss_fixpoint_table_get_final_as_semidet(PPId, T, SharingAs_Status) :-
    get_from_fixpoint_table_final_semidet(PPId, T, SharingAs_Status).

%---------------------------------------------------------------------------%

:- func wrapped_init(pred_proc_id) = sharing_as_and_status.

wrapped_init(_Id) = sharing_as_and_status(sharing_as_init, optimal).

%---------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework.
%

:- type structure_sharing_call
    --->    structure_sharing_call.

:- type structure_sharing_answer
    --->    structure_sharing_answer_bottom
    ;       structure_sharing_answer_top
    ;       structure_sharing_answer_real(
                % XXX The next two fields should be a list of pairs,
                % not a pair of lists.
                ssar_vars       :: list(prog_var),
                ssar_types      :: list(mer_type),
                ssar_sharing    :: structure_sharing
                % We cannot keep this as a sharing_as. When the analysis
                % answers are loaded, we don't have enough information to
                % rename the variables in the .analysis answer to the correct
                % variables for the proc_info that the sharing_as will be used
                % with.
            ).

:- type structure_sharing_func_info
    --->    structure_sharing_func_info(
                ssfi_module     :: module_info,
                ssfi_proc       :: proc_info
            ).

:- func analysis_name = string.

analysis_name = "structure_sharing".

:- instance analysis(structure_sharing_func_info, structure_sharing_call,
    structure_sharing_answer) where
[
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 2,
    preferred_fixpoint_type(_, _) = greatest_fixpoint,
    bottom(_, _) = structure_sharing_answer_bottom,
    top(_, _) = structure_sharing_answer_top,

    ( get_func_info(ModuleInfo, ModuleName, FuncId, _, _, FuncInfo) :-
        func_id_to_ppid(ModuleInfo, ModuleName, FuncId, PPId),
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        FuncInfo = structure_sharing_func_info(ModuleInfo, ProcInfo)
    )
].

:- instance call_pattern(structure_sharing_func_info,
    structure_sharing_call) where [].

:- instance partial_order(structure_sharing_func_info,
        structure_sharing_call) where [
    (more_precise_than(_, _, _) :-
        semidet_fail
    ),
    equivalent(_, Call, Call)
].

:- instance to_term(structure_sharing_call) where [
    ( to_term(structure_sharing_call) = Term :-
        Term = term.functor(atom("any"), [], dummy_context)
    ),
    ( from_term(Term, structure_sharing_call) :-
        Term = term.functor(atom("any"), [], _)
    )
].

:- instance answer_pattern(structure_sharing_func_info,
    structure_sharing_answer) where [].

:- instance partial_order(structure_sharing_func_info,
        structure_sharing_answer) where [
    (more_precise_than(FuncInfo, Answer1, Answer2) :-
        % Fast path (maybe).
        Answer1 \= Answer2,

        FuncInfo = structure_sharing_func_info(ModuleInfo, ProcInfo),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_var_table(ProcInfo, VarTable),
        lookup_var_types(VarTable, HeadVars, HeadVarTypes),
        structure_sharing_answer_to_domain(no, HeadVarTypes, ProcInfo,
            Answer1, Sharing1),
        structure_sharing_answer_to_domain(no, HeadVarTypes, ProcInfo,
            Answer2, Sharing2),
        SharingAs1 = from_structure_sharing_domain(Sharing1),
        SharingAs2 = from_structure_sharing_domain(Sharing2),
        sharing_as_is_subsumed_by(ModuleInfo, ProcInfo,
            SharingAs1, SharingAs2),
        not sharing_as_is_subsumed_by(ModuleInfo, ProcInfo,
            SharingAs2, SharingAs1)
    ),

    (equivalent(FuncInfo, Answer1, Answer2) :-
        (
            % Fast path (maybe).
            Answer1 = Answer2
        ;
            FuncInfo = structure_sharing_func_info(ModuleInfo, ProcInfo),
            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_var_table(ProcInfo, VarTable),
            lookup_var_types(VarTable, HeadVars, HeadVarTypes),
            structure_sharing_answer_to_domain(no, HeadVarTypes, ProcInfo,
                Answer1, Sharing1),
            structure_sharing_answer_to_domain(no, HeadVarTypes, ProcInfo,
                Answer2, Sharing2),
            SharingAs1 = from_structure_sharing_domain(Sharing1),
            SharingAs2 = from_structure_sharing_domain(Sharing2),
            sharing_as_is_subsumed_by(ModuleInfo, ProcInfo,
                SharingAs2, SharingAs1),
            sharing_as_is_subsumed_by(ModuleInfo, ProcInfo,
                SharingAs1, SharingAs2)
        )
    )
].

:- instance to_term(structure_sharing_answer) where [
    func(to_term/1) is sharing_answer_to_term,
    pred(from_term/2) is sharing_answer_from_term
].

:- func sharing_answer_to_term(structure_sharing_answer) = term.

sharing_answer_to_term(Answer) = Term :-
    (
        Answer = structure_sharing_answer_bottom,
        Term = term.functor(atom("b"), [], dummy_context)
    ;
        Answer = structure_sharing_answer_top,
        Term = term.functor(atom("t"), [], dummy_context)
    ;
        Answer = structure_sharing_answer_real(HeadVars, Types, SharingPairs),
        type_to_term(HeadVars, HeadVarsTerm),
        type_to_term(Types, TypesTerm),
        type_to_term(SharingPairs, SharingPairsTerm),
        Term = term.functor(atom("sharing"),
            [HeadVarsTerm, TypesTerm, SharingPairsTerm], dummy_context)
    ).

:- pred sharing_answer_from_term(term::in, structure_sharing_answer::out)
    is semidet.

sharing_answer_from_term(Term, Answer) :-
    (
        Term = term.functor(atom("b"), [], _),
        Answer = structure_sharing_answer_bottom
    ;
        Term = term.functor(atom("t"), [], _),
        Answer = structure_sharing_answer_top
    ;
        Term = term.functor(atom("sharing"),
            [HeadVarsTerm, TypesTerm, SharingPairsTerm], _),
        term_to_type(HeadVarsTerm, HeadVars),
        term_to_type(TypesTerm, Types),
        term_to_type(SharingPairsTerm, SharingPairs),
        Answer = structure_sharing_answer_real(HeadVars, Types, SharingPairs)
    ).

%---------------------------------------------------------------------------%
%
% Additional predicates used for intermodule analysis.
%

:- pred maybe_record_sharing_analysis_result(module_info::in,
    sharing_as_table::in, pred_id::in, analysis_info::in, analysis_info::out)
    is det.

maybe_record_sharing_analysis_result(ModuleInfo, SharingAsTable, PredId,
        !AnalysisInfo) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    ProcIds = pred_info_all_procids(PredInfo),
    list.foldl(
        maybe_record_sharing_analysis_result_2(ModuleInfo, SharingAsTable,
            PredId, PredInfo),
        ProcIds, !AnalysisInfo).

:- pred maybe_record_sharing_analysis_result_2(module_info::in,
    sharing_as_table::in, pred_id::in, pred_info::in, proc_id::in,
    analysis_info::in, analysis_info::out) is det.

maybe_record_sharing_analysis_result_2(ModuleInfo, SharingAsTable, PredId,
        PredInfo, ProcId, !AnalysisInfo) :-
    should_write_sharing_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = should_write,
        pred_info_proc_info(PredInfo, ProcId, ProcInfo),
        PPId = proc(PredId, ProcId),
        ( if
            sharing_as_table_search(PPId, SharingAsTable,
                sharing_as_and_status(SharingAsPrime, StatusPrime))
        then
            Sharing = to_structure_sharing_domain(SharingAsPrime),
            Status0 = StatusPrime
        else if
            % Probably an exported `:- pragma external_{pred/func}' procedure.
            bottom_sharing_is_safe_approximation(ModuleInfo, PredInfo,
                ProcInfo)
        then
            Sharing = structure_sharing_bottom,
            Status0 = optimal
        else
            Sharing = structure_sharing_top(set.init),
            Status0 = optimal
        ),
        (
            Sharing = structure_sharing_bottom,
            Answer = structure_sharing_answer_bottom,
            Status = optimal
        ;
            Sharing = structure_sharing_top(Reasons),
            Answer = structure_sharing_answer_top,
            % If the procedure contains a generic or foreign foreign call, or
            % it calls a procedure in a non-local module for which we have no
            % results, we won't be able to do better upon reanalysis.
            ( if
                set.member(Reason, Reasons),
                reason_implies_optimal(ModuleInfo, !.AnalysisInfo, Reason)
            then
                Status = optimal
            else
                Status = Status0
            ),
            trace [io(!IO),
                compile_time(flag("structure_sharing")),
                run_time(env("TOP_REASONS"))
            ] (
                get_debug_output_stream(ModuleInfo, DebugStream, !IO),
                ReasonsList = set.to_sorted_list(Reasons),
                io.write_string(DebugStream, ":\n", !IO),
                list.foldl(write_top_feedback(DebugStream, ModuleInfo),
                    ReasonsList, !IO),
                io.write_string(DebugStream, "\t", !IO),
                io.write_line(DebugStream, Status, !IO)
            )
        ;
            Sharing = structure_sharing_real(SharingPairs),
            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_var_table(ProcInfo, VarTable),
            lookup_var_types(VarTable, HeadVars, HeadVarTypes),
            Answer = structure_sharing_answer_real(HeadVars, HeadVarTypes,
                SharingPairs),
            Status = Status0
        ),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, structure_sharing_call, Answer,
            Status, !AnalysisInfo)
    ;
        ShouldWrite = should_not_write
    ).

:- pred reason_implies_optimal(module_info::in, analysis_info::in,
    top_feedback::in) is semidet.

reason_implies_optimal(ModuleInfo, AnalysisInfo, Reason) :-
    (
        Reason = top_cannot_improve(_)
    ;
        Reason = top_failed_lookup(ShroudedPPId),
        proc(PredId, _) = unshroud_pred_proc_id(ShroudedPPId),
        module_info_pred_info(ModuleInfo, PredId, PredInfo),
        PredModule = pred_info_module(PredInfo),
        module_is_local(AnalysisInfo, PredModule, no)
    ).

:- pred handle_dep_procs(module_info::in, pred_proc_id::in,
    analysis_info::in, analysis_info::out) is det.

handle_dep_procs(ModuleInfo, DepPPId, !AnalysisInfo) :-
    % Record that we depend on the result for the called procedure.
    module_name_func_id(ModuleInfo, DepPPId, DepModuleName, DepFuncId),
    Call = structure_sharing_call,
    Answer = _ : structure_sharing_answer,
    get_func_info(ModuleInfo, DepModuleName, DepFuncId, Call, Answer,
        FuncInfo),
    record_dependency(DepModuleName, DepFuncId, FuncInfo, Call, Answer,
        !AnalysisInfo).

%---------------------------------------------------------------------------%

:- pred write_top_feedback(io.text_output_stream::in, module_info::in,
    top_feedback::in, io::di, io::uo) is det.

write_top_feedback(Stream, ModuleInfo, Reason, !IO) :-
    io.write_string(Stream, "\t", !IO),
    (
        Reason = top_failed_lookup(ShroudedPPId),
        PPId = unshroud_pred_proc_id(ShroudedPPId),
        PPIdStr = pred_proc_id_to_dev_string(ModuleInfo, PPId),
        io.format(Stream, "failed_lookup: %s\n", [s(PPIdStr)], !IO)
    ;
        Reason = top_from_lookup(ShroudedPPId),
        PPId = unshroud_pred_proc_id(ShroudedPPId),
        PPIdStr = pred_proc_id_to_dev_string(ModuleInfo, PPId),
        io.format(Stream, "from_lookup: %s\n", [s(PPIdStr)], !IO)
    ;
        Reason = top_cannot_improve(String),
        io.format(Stream, "cannot_improve: %s\n", [s(String)], !IO)
    ).

%---------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_sharing.analysis.
%---------------------------------------------------------------------------%
