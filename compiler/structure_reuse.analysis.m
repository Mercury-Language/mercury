%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2006-2012 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: structure_reuse.analysis.m.
% Main authors: nancy, wangp.
%
% Implementation of the structure reuse analysis (compile-time garbage
% collection system): each procedure is analysed to see whether some
% of the terms it manipulates become garbage thus making it possible
% to reuse that garbage straight away for creating new terms.
%
% Structure reuse is broken up into three phases:
%   * the direct reuse analysis (structure_reuse.direct.m)
%   * the indirect analysis (structure_reuse.indirect.m)
%   * and the generation of the optimised procedures.
%
% The following example shows instances of direct and indirect reuse:
%
% list.append(H1, H2, H3) :-
%   (
%       H1 => [],
%       H3 := H2
%   ;
%           % Cell H1 dies provided some condition about the
%           % structure sharing of H1 is true.  A deconstruction
%           % generating a dead cell, followed by a
%           % construction reusing that cell, is called a direct
%           % reuse.
%       H1 => [X | Xs],
%
%           % If the condition about the structure sharing of H1
%           % is true then we can call the version of list.append
%           % which does reuse. Calling the optimised version here leads
%           % to a new condition to be met by the headvars of any
%           % call to the resulting optimised version of append.
%           % This is an indirect reuse.
%       list.append(Xs, H2, Zs),
%
%           % Reuse the dead cell H1.  This is a direct reuse.
%       H3 <= [X | Zs]
%   ).
%
%-----------------------------------------------------------------------------%

:- module transform_hlds.ctgc.structure_reuse.analysis.
:- interface.

:- import_module analysis.
:- import_module hlds.hlds_module.
:- import_module hlds.hlds_pred.
:- import_module transform_hlds.ctgc.structure_reuse.domain.

:- import_module bool.
:- import_module io.

%-----------------------------------------------------------------------------%

    % Perform structure reuse analysis on the procedures defined in the
    % current module.
    %
:- pred perform_structure_reuse_analysis(module_info::in, module_info::out,
    io::di, io::uo) is det.

    % Write all the reuse information concerning the specified predicate as
    % reuse pragmas.
    %
:- pred write_pred_reuse_info(module_info::in, pred_id::in,
    io::di, io::uo) is det.

%-----------------------------------------------------------------------------%

:- type structure_reuse_call.
:- type structure_reuse_answer.
:- type structure_reuse_func_info.

:- instance analysis(structure_reuse_func_info, structure_reuse_call,
    structure_reuse_answer).

:- instance call_pattern(structure_reuse_func_info, structure_reuse_call).
:- instance partial_order(structure_reuse_func_info, structure_reuse_call).
:- instance to_term(structure_reuse_call).

:- instance answer_pattern(structure_reuse_func_info, structure_reuse_answer).
:- instance partial_order(structure_reuse_func_info, structure_reuse_answer).
:- instance to_term(structure_reuse_answer).

:- pred structure_reuse_answer_harsher_than_in_analysis_registry(
    module_info::in, reuse_as_table::in, pred_proc_id::in, bool::out) is det.

%-----------------------------------------------------------------------------%
%-----------------------------------------------------------------------------%

:- implementation.

:- import_module hlds.goal_path.
:- import_module hlds.hlds_out.
:- import_module hlds.hlds_out.hlds_out_util.
:- import_module hlds.passes_aux.
:- import_module hlds.pred_table.
:- import_module libs.file_util.
:- import_module libs.globals.
:- import_module libs.options.
:- import_module mdbcomp.
:- import_module mdbcomp.prim_data.
:- import_module mdbcomp.sym_name.
:- import_module parse_tree.error_util.
:- import_module parse_tree.file_names.
:- import_module parse_tree.mercury_to_mercury.
:- import_module parse_tree.prog_ctgc.
:- import_module parse_tree.prog_data.
:- import_module parse_tree.prog_item.
:- import_module parse_tree.prog_type.
:- import_module parse_tree.set_of_var.
:- import_module transform_hlds.ctgc.selector.
:- import_module transform_hlds.ctgc.structure_reuse.direct.
:- import_module transform_hlds.ctgc.structure_reuse.domain.
:- import_module transform_hlds.ctgc.structure_reuse.indirect.
:- import_module transform_hlds.ctgc.structure_reuse.lbu.
:- import_module transform_hlds.ctgc.structure_reuse.lfu.
:- import_module transform_hlds.ctgc.structure_reuse.versions.
:- import_module transform_hlds.ctgc.structure_sharing.domain.
:- import_module transform_hlds.mmc_analysis.

:- import_module bimap.
:- import_module bool.
:- import_module int.
:- import_module io.
:- import_module list.
:- import_module map.
:- import_module maybe.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module term.

%-----------------------------------------------------------------------------%

perform_structure_reuse_analysis(!ModuleInfo, !IO):-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),

    % Load all available structure sharing information into a sharing table.
    SharingTable = load_structure_sharing_table(!.ModuleInfo),

    % Process all imported reuse information.
    globals.lookup_bool_option(Globals, intermodule_analysis,
        IntermodAnalysis),
    (
        IntermodAnalysis = yes,
        % Load structure reuse answers from the analysis registry into a reuse
        % table.  Add procedures to the module as necessary.  Look up the
        % requests made for procedures in this module by other modules.
        process_intermod_analysis_reuse(!ModuleInfo, ReuseTable0,
            ExternalRequests, MustHaveReuseVersions)
    ;
        IntermodAnalysis = no,
        % Convert imported structure reuse information into structure reuse
        % information, then load the available reuse information into a reuse
        % table.
        %
        % There is no way to request specific reuse versions of procedures
        % across module boundaries using the old intermodule optimisation
        % system.
        process_imported_reuse(!ModuleInfo),
        ReuseTable0 = load_structure_reuse_table(!.ModuleInfo),
        ExternalRequests = [],
        MustHaveReuseVersions = []
    ),

    some [!ReuseTable] (
        !:ReuseTable = ReuseTable0,

        % Pre-annotate each of the goals with "Local Forward Use" and
        % "Local Backward Use" information, and fill in all the goal_id slots
        % as well.
        trace [io(!TIO)] (
            maybe_write_string(VeryVerbose,
                "% Annotating in use information...", !TIO)
        ),
        process_all_nonimported_procs(update_proc(annotate_in_use_information),
            !ModuleInfo),
        trace [io(!TIO)] (
            maybe_write_string(VeryVerbose, "done.\n", !TIO),
            maybe_write_string(VeryVerbose,
                "% Reuse table before intermediate reuse:\n", !TIO),
            reuse_as_table_maybe_dump(VeryVerbose, !.ModuleInfo, !.ReuseTable,
                !TIO)
        ),


        % Create copies of externally requested procedures.  This must be done
        % after the in-use annotations have been added to the procedures being
        % copied.
        list.map_foldl2(make_intermediate_reuse_proc, ExternalRequests,
            _NewPPIds, !ReuseTable, !ModuleInfo),

        % Determine information about possible direct reuses.
        trace [io(!TIO)] (
            maybe_write_string(VeryVerbose,
                "% Reuse table after intermediate reuse:\n", !TIO),
            reuse_as_table_maybe_dump(VeryVerbose, !.ModuleInfo, !.ReuseTable,
                !TIO),
            maybe_write_string(VeryVerbose, "% Direct reuse...\n", !TIO)
        ),
        direct_reuse_pass(SharingTable, !ModuleInfo, !ReuseTable),
        trace [io(!TIO)] (
            maybe_write_string(VeryVerbose, "% Direct reuse: done.\n", !TIO),
            reuse_as_table_maybe_dump(VeryVerbose, !.ModuleInfo, !.ReuseTable,
                !TIO)
        ),

        % Determine information about possible indirect reuses.
        trace [io(!TIO)] (
            maybe_write_string(VeryVerbose, "% Indirect reuse...\n", !TIO)
        ),
        indirect_reuse_pass(SharingTable, !ModuleInfo, !ReuseTable, DepProcs0,
            InternalRequests, IntermodRequests0),
        trace [io(!TIO)] (
            maybe_write_string(VeryVerbose, "% Indirect reuse: done.\n", !TIO),
            reuse_as_table_maybe_dump(VeryVerbose, !.ModuleInfo, !.ReuseTable,
                !TIO)
        ),

        % Handle requests for "intermediate" reuse versions of procedures
        % and repeat the analyses.
        globals.lookup_int_option(Globals, structure_reuse_repeat, Repeats),
        handle_structure_reuse_requests(Repeats, SharingTable,
            InternalRequests, !ReuseTable, !ModuleInfo, DepProcs0, DepProcs,
            IntermodRequests0, IntermodRequests),

        % Create reuse versions of procedures.  Update goals to reuse cells
        % and call reuse versions of procedures.
        create_reuse_procedures(!ReuseTable, !ModuleInfo),

        ReuseTable = !.ReuseTable
    ),

    (
        IntermodAnalysis = no,
        % Create forwarding procedures for procedures which we thought had
        % conditional reuse when making the `.opt' file, but with further
        % information (say, from `.trans_opt' files) we decide has no reuse
        % opportunities. Otherwise other modules may contain references to
        % reuse versions of procedures which we never produce.
        maybe_create_forwarding_procedures_intermod_opt(ReuseTable0,
            ReuseTable, !ModuleInfo)
    ;
        IntermodAnalysis = yes,
        % We may need to create forwarding procedures for procedures which had
        % conditional reuse in the `.analysis' file, but which have no reuse
        % or unconditional reuse now.  We should only need to do this for
        % procedures with NoClobbers = [].
        list.foldl(
            maybe_create_forwarding_procedures_intermod_analysis(ReuseTable),
            MustHaveReuseVersions, !ModuleInfo)
    ),

    ReuseTable = reuse_as_table(ReuseInfoMap, ReuseVersionMap),

    % Record the results of the reuse table into the HLDS.
    % This is mainly to show the reuse information in HLDS dumps as no later
    % passes need the information.
    map.foldl(save_reuse_in_module_info, ReuseInfoMap, !ModuleInfo),

    % Only write structure reuse pragmas to `.opt' files for
    % `--intermodule-optimization' not `--intermodule-analysis'.
    globals.lookup_bool_option(Globals, make_optimization_interface,
        MakeOptInt),
    (
        MakeOptInt = yes,
        IntermodAnalysis = no
    ->
        make_opt_int(!ModuleInfo, !IO)
    ;
        true
    ),

    % If making a `.analysis' file, record structure reuse results, analysis
    % dependencies, assumed answers and requests in the analysis framework.
    globals.lookup_bool_option(Globals, make_analysis_registry,
        MakeAnalysisRegistry),
    (
        MakeAnalysisRegistry = yes,
        some [!AnalysisInfo] (
            module_info_get_analysis_info(!.ModuleInfo, !:AnalysisInfo),
            map.foldl(
                record_structure_reuse_results(!.ModuleInfo, ReuseVersionMap),
                ReuseInfoMap, !AnalysisInfo),
            set.fold(handle_structure_reuse_dependency(!.ModuleInfo),
                DepProcs, !AnalysisInfo),
            set.fold(record_intermod_requests(!.ModuleInfo),
                IntermodRequests, !AnalysisInfo),
            module_info_set_analysis_info(!.AnalysisInfo, !ModuleInfo)
        )
    ;
        MakeAnalysisRegistry = no
    ),

    % Delete the reuse versions of procedures which turn out to have no reuse.
    % Nothing should be calling them but dead procedure elimination won't
    % remove them if they were created from exported procedures (so would be
    % exported themselves).
    module_info_get_predicate_table(!.ModuleInfo, PredTable0),
    bimap.foldl(
        remove_useless_reuse_proc(!.ModuleInfo, VeryVerbose, ReuseInfoMap),
        ReuseVersionMap, PredTable0, PredTable),
    module_info_set_predicate_table(PredTable, !ModuleInfo),

    selector.reset_tables(!IO).

%-----------------------------------------------------------------------------%

    % Create intermediate reuse versions of procedures according to the
    % requests from indirect reuse analysis.  We perform direct reuse
    % analyses on the newly created procedures, then repeat indirect reuse
    % analysis on all procedures in the module so that calls to the new
    % procedures can be made.  This may create new requests.
    %
    % XXX this is temporary only; we shouldn't be redoing so much work.
    %
:- pred handle_structure_reuse_requests(int::in, sharing_as_table::in,
    set(sr_request)::in, reuse_as_table::in, reuse_as_table::out,
    module_info::in, module_info::out,
    set(ppid_no_clobbers)::in, set(ppid_no_clobbers)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

handle_structure_reuse_requests(Repeats, SharingTable, Requests,
        !ReuseTable, !ModuleInfo, !DepProcs, !IntermodRequests) :-
    ( Repeats > 0 ->
        handle_structure_reuse_requests_2(Repeats, SharingTable, Requests,
            !ReuseTable, !ModuleInfo, !DepProcs, !IntermodRequests)
    ;
        true
    ).

:- pred handle_structure_reuse_requests_2(int::in, sharing_as_table::in,
    set(sr_request)::in, reuse_as_table::in, reuse_as_table::out,
    module_info::in, module_info::out,
    set(ppid_no_clobbers)::in, set(ppid_no_clobbers)::out,
    set(sr_request)::in, set(sr_request)::out) is det.

handle_structure_reuse_requests_2(Repeats, SharingTable, Requests,
        !ReuseTable, !ModuleInfo, !DepProcs, !IntermodRequests) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    globals.lookup_bool_option(Globals, very_verbose, VeryVerbose),

    % Create copies of the requested procedures.
    RequestList = set.to_sorted_list(Requests),
    list.map_foldl2(make_intermediate_reuse_proc, RequestList, NewPPIds,
        !ReuseTable, !ModuleInfo),

    % Perform direct reuse analysis on the new procedures.
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose, "% Repeating direct reuse...\n", !IO)
    ),
    direct_reuse_process_specific_procs(SharingTable, NewPPIds,
        !ModuleInfo, !ReuseTable),
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose, "% done.\n", !IO)
    ),

    % Rerun indirect reuse analysis on all procedures.
    %
    % XXX goals which already have reuse annotations don't need to be
    % reanalysed.  For old procedures (not the ones just created) we actually
    % only need to check that calls which previously had no reuse opportunity
    % might be able to call the new procedures.
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose, "% Repeating indirect reuse...\n", !IO)
    ),
    indirect_reuse_rerun(SharingTable, !ModuleInfo, !ReuseTable,
        NewDepProcs, NewRequests, !IntermodRequests),
    !:DepProcs = set.union(NewDepProcs, !.DepProcs),
    trace [io(!IO)] (
        maybe_write_string(VeryVerbose, "% done.\n", !IO)
    ),

    ( set.is_empty(NewRequests) ->
        trace [io(!IO)] (
            maybe_write_string(VeryVerbose,
                "% No more structure reuse requests.\n", !IO)
        )
    ;
        trace [io(!IO)] (
            maybe_write_string(VeryVerbose,
                "% Outstanding structure reuse requests exist.\n", !IO)
        ),
        handle_structure_reuse_requests(Repeats - 1, SharingTable, NewRequests,
            !ReuseTable, !ModuleInfo, !DepProcs, !IntermodRequests)
    ).

    % Create a new copy of a procedure to satisfy an intermediate reuse
    % request, i.e. some of its arguments are prevented from being reused.
    %
    % The goal of the original procedure must already be annotated with in-use
    % sets.  For the new procedure, we simply add the head variables at the
    % no-clobber argument positions to the forward-use set of each goal.
    % We also remove any existing reuse annotations on the goals.
    %
:- pred make_intermediate_reuse_proc(sr_request::in, pred_proc_id::out,
    reuse_as_table::in, reuse_as_table::out, module_info::in, module_info::out)
    is det.

make_intermediate_reuse_proc(sr_request(PPId, NoClobbers), NewPPId,
        !ReuseTable, !ModuleInfo) :-
    create_fresh_pred_proc_info_copy(PPId, NoClobbers, NewPPId, !ModuleInfo),

    module_info_pred_proc_info(!.ModuleInfo, NewPPId, PredInfo, ProcInfo0),
    proc_info_get_headvars(ProcInfo0, HeadVars),
    get_numbered_args(1, NoClobbers, HeadVars, NoClobberVars),
    add_vars_to_lfu(set_of_var.list_to_set(NoClobberVars),
        ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(NewPPId, PredInfo, ProcInfo, !ModuleInfo),

    reuse_as_table_insert_reuse_version_proc(PPId, NoClobbers, NewPPId,
        !ReuseTable).

:- pred get_numbered_args(int::in, list(int)::in,
    list(prog_var)::in, list(prog_var)::out) is det.

get_numbered_args(_, [], _, []).
get_numbered_args(_, [_ | _], [], _) :-
    unexpected($module, $pred, "argument list too short").
get_numbered_args(I, [N | Ns], [Var | Vars], Selected) :-
    ( I = N ->
        get_numbered_args(I + 1, Ns, Vars, Selected0),
        Selected = [Var | Selected0]
    ;
        get_numbered_args(I + 1, [N | Ns], Vars, Selected)
    ).

%-----------------------------------------------------------------------------%

:- pred maybe_create_forwarding_procedures_intermod_opt(reuse_as_table::in,
    reuse_as_table::in, module_info::in, module_info::out) is det.

maybe_create_forwarding_procedures_intermod_opt(
        InitialReuseTable, FinalReuseTable, !ModuleInfo) :-
    map.foldl(
        maybe_create_forwarding_procedures_intermod_opt_2(FinalReuseTable),
        InitialReuseTable ^ reuse_info_map, !ModuleInfo).

:- pred maybe_create_forwarding_procedures_intermod_opt_2(reuse_as_table::in,
    pred_proc_id::in, reuse_as_and_status::in,
    module_info::in, module_info::out) is det.

maybe_create_forwarding_procedures_intermod_opt_2(FinalReuseTable, PPId,
        reuse_as_and_status(InitialReuseAs, _), !ModuleInfo) :-
    PPId = proc(PredId, _),
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, ImportStatus),
    (
        reuse_as_conditional_reuses(InitialReuseAs),
        status_defined_in_this_module(ImportStatus) = yes,
        reuse_as_table_search(FinalReuseTable, PPId, FinalReuseAs_Status),
        FinalReuseAs_Status = reuse_as_and_status(FinalReuseAs, _),
        reuse_as_no_reuses(FinalReuseAs)
    ->
        NoClobbers = [],
        create_fake_reuse_procedure(PPId, NoClobbers, !ModuleInfo)
    ;
        true
    ).

:- pred maybe_create_forwarding_procedures_intermod_analysis(
    reuse_as_table::in, pred_proc_id::in, module_info::in, module_info::out)
    is det.

maybe_create_forwarding_procedures_intermod_analysis(ReuseTable, PredProcId,
        !ModuleInfo) :-
    % The procedure PredProcId would have been listed as having conditional
    % reuse for call pattern NoClobbers = [] in the analysis registry.  If our
    % analysis of the procedure didn't create a conditional reuse version,
    % then we need to produce a forwarding procedure to avoid linking
    % problems.
    (
        reuse_as_table_search(ReuseTable, PredProcId, ReuseAs_Status),
        ReuseAs_Status = reuse_as_and_status(ReuseAs, _),
        reuse_as_conditional_reuses(ReuseAs)
    ->
        true
    ;
        NoClobbers = [],
        create_fake_reuse_procedure(PredProcId, NoClobbers, !ModuleInfo)
    ).

%-----------------------------------------------------------------------------%

    % Process the imported reuse annotations from .opt files.
    %
:- pred process_imported_reuse(module_info::in, module_info::out) is det.

process_imported_reuse(!ModuleInfo):-
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    list.foldl(process_imported_reuse_in_pred, PredIds, !ModuleInfo).

:- pred process_imported_reuse_in_pred(pred_id::in, module_info::in,
    module_info::out) is det.

process_imported_reuse_in_pred(PredId, !ModuleInfo) :-
    some [!PredTable] (
        module_info_get_preds(!.ModuleInfo, !:PredTable),
        map.lookup(!.PredTable, PredId, PredInfo0),
        process_imported_reuse_in_procs(PredInfo0, PredInfo),
        map.det_update(PredId, PredInfo, !PredTable),
        module_info_set_preds(!.PredTable, !ModuleInfo)
    ).

:- pred process_imported_reuse_in_procs(pred_info::in,
    pred_info::out) is det.

process_imported_reuse_in_procs(!PredInfo) :-
    some [!ProcTable] (
        pred_info_get_procedures(!.PredInfo, !:ProcTable),
        ProcIds = pred_info_procids(!.PredInfo),
        list.foldl(process_imported_reuse_in_proc(!.PredInfo),
            ProcIds, !ProcTable),
        pred_info_set_procedures(!.ProcTable, !PredInfo)
    ).

:- pred process_imported_reuse_in_proc(pred_info::in, proc_id::in,
    proc_table::in, proc_table::out) is det.

process_imported_reuse_in_proc(PredInfo, ProcId, !ProcTable) :-
    some [!ProcInfo] (
        !:ProcInfo = !.ProcTable ^ det_elem(ProcId),
        (
            proc_info_get_imported_structure_reuse(!.ProcInfo,
                ImpHeadVars, ImpTypes, ImpReuse)
        ->
            proc_info_get_headvars(!.ProcInfo, HeadVars),
            pred_info_get_arg_types(PredInfo, HeadVarTypes),
            map.from_corresponding_lists(ImpHeadVars, HeadVars, VarRenaming),
            some [!TypeSubst] (
                !:TypeSubst = map.init,
                (
                    type_unify_list(ImpTypes, HeadVarTypes, [], !.TypeSubst,
                        TypeSubstNew)
                ->
                    !:TypeSubst = TypeSubstNew
                ;
                    true
                ),
                rename_structure_reuse_domain(VarRenaming, !.TypeSubst,
                    ImpReuse, Reuse)
            ),
            % Optimality does not apply to `--intermodule-optimisation'
            % system, only `--intermodule-analysis'.
            proc_info_set_structure_reuse(
                structure_reuse_domain_and_status(Reuse, optimal), !ProcInfo),
            proc_info_reset_imported_structure_reuse(!ProcInfo),
            map.det_update(ProcId, !.ProcInfo, !ProcTable)
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%

    % Process the intermodule imported reuse information from the analysis
    % framework.
    %
:- pred process_intermod_analysis_reuse(module_info::in, module_info::out,
    reuse_as_table::out, list(sr_request)::out, list(pred_proc_id)::out)
    is det.

process_intermod_analysis_reuse(!ModuleInfo, ReuseTable, ExternalRequests,
        MustHaveReuseVersions) :-
    module_info_get_valid_predids(PredIds, !ModuleInfo),
    list.foldl4(process_intermod_analysis_reuse_pred, PredIds,
        !ModuleInfo, reuse_as_table_init, ReuseTable, [], ExternalRequests0,
        [], MustHaveReuseVersions),
    list.sort_and_remove_dups(ExternalRequests0, ExternalRequests).

:- pred process_intermod_analysis_reuse_pred(pred_id::in,
    module_info::in, module_info::out, reuse_as_table::in, reuse_as_table::out,
    list(sr_request)::in, list(sr_request)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

process_intermod_analysis_reuse_pred(PredId, !ModuleInfo, !ReuseTable,
        !ExternalRequests, !MustHaveReuseVersions) :-
    module_info_pred_info(!.ModuleInfo, PredId, PredInfo),
    pred_info_get_import_status(PredInfo, ImportStatus),
    ProcIds = pred_info_procids(PredInfo),
    (
        ImportStatus = status_imported(_)
    ->
        % Read in answers for imported procedures.
        list.foldl2(process_intermod_analysis_reuse_proc(PredId, PredInfo),
            ProcIds, !ModuleInfo, !ReuseTable)
    ;
        status_defined_in_this_module(ImportStatus) = yes
    ->
        % For procedures defined in this module we need to read in the answers
        % from previous passes to know which versions of procedures other
        % modules will be expecting.  We also need to read in new requests.
        list.foldl2(
            process_intermod_analysis_defined_proc(!.ModuleInfo, PredId),
            ProcIds, !ExternalRequests, !MustHaveReuseVersions)
    ;
        true
    ).

:- pred process_intermod_analysis_reuse_proc(pred_id::in,
    pred_info::in, proc_id::in, module_info::in, module_info::out,
    reuse_as_table::in, reuse_as_table::out) is det.

process_intermod_analysis_reuse_proc(PredId, PredInfo, ProcId,
        !ModuleInfo, !ReuseTable) :-
    PPId = proc(PredId, ProcId),
    module_info_get_analysis_info(!.ModuleInfo, AnalysisInfo),
    module_name_func_id(!.ModuleInfo, PPId, ModuleName, FuncId),
    pred_info_proc_info(PredInfo, ProcId, ProcInfo),
    lookup_results(AnalysisInfo, ModuleName, FuncId, ImportedResults),
    list.foldl2(
        process_intermod_analysis_imported_reuse_answer(PPId, PredInfo,
            ProcInfo),
        ImportedResults, !ModuleInfo, !ReuseTable).

:- pred process_intermod_analysis_imported_reuse_answer(pred_proc_id::in,
    pred_info::in, proc_info::in,
    analysis_result(structure_reuse_call, structure_reuse_answer)::in,
    module_info::in, module_info::out, reuse_as_table::in, reuse_as_table::out)
    is det.

process_intermod_analysis_imported_reuse_answer(PPId, PredInfo, ProcInfo,
        ImportedResult, !ModuleInfo, !ReuseTable) :-
    ImportedResult = analysis_result(Call, Answer, ResultStatus),
    Call = structure_reuse_call(NoClobbers),
    pred_info_get_arg_types(PredInfo, HeadVarTypes),
    structure_reuse_answer_to_domain(HeadVarTypes, ProcInfo, Answer, Domain),
    ReuseAs = from_structure_reuse_domain(Domain),
    ReuseAs_Status = reuse_as_and_status(ReuseAs, ResultStatus),
    (
        NoClobbers = [],
        % When the no-clobber list is empty we store the information with the
        % original pred_proc_id.
        reuse_as_table_set(PPId, ReuseAs_Status, !ReuseTable)
    ;
        NoClobbers = [_ | _],
        % When the no-clobber list is non-empty we need to create a new
        % procedure stub and add a mapping to from the original pred_proc_id to
        % the stub.
        create_fresh_pred_proc_info_copy(PPId, NoClobbers, NewPPId,
            !ModuleInfo),
        reuse_as_table_set(NewPPId, ReuseAs_Status, !ReuseTable),
        reuse_as_table_insert_reuse_version_proc(PPId, NoClobbers, NewPPId,
            !ReuseTable)
    ).

:- pred structure_reuse_answer_to_domain(list(mer_type)::in,
    proc_info::in, structure_reuse_answer::in, structure_reuse_domain::out)
    is det.

structure_reuse_answer_to_domain(HeadVarTypes, ProcInfo, Answer, Reuse) :-
    (
        Answer = structure_reuse_answer_no_reuse,
        Reuse = has_no_reuse
    ;
        Answer = structure_reuse_answer_unconditional,
        Reuse = has_only_unconditional_reuse
    ;
        Answer = structure_reuse_answer_conditional(ImpHeadVars, ImpTypes,
            ImpReuseConditions),
        proc_info_get_headvars(ProcInfo, HeadVars),
        map.from_corresponding_lists(ImpHeadVars, HeadVars, VarRenaming),
        ( type_unify_list(ImpTypes, HeadVarTypes, [], map.init, TypeSubst) ->
            rename_structure_reuse_domain(VarRenaming, TypeSubst,
                has_conditional_reuse(ImpReuseConditions), Reuse)
        ;
            unexpected($module, $pred, "type_unify_list failed")
        )
    ).

:- pred process_intermod_analysis_defined_proc(module_info::in, pred_id::in,
    proc_id::in, list(sr_request)::in, list(sr_request)::out,
    list(pred_proc_id)::in, list(pred_proc_id)::out) is det.

process_intermod_analysis_defined_proc(ModuleInfo, PredId, ProcId,
        !ExternalRequests, !MustHaveReuseVersions) :-
    PPId = proc(PredId, ProcId),
    module_info_get_analysis_info(ModuleInfo, AnalysisInfo),
    module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),

    % Only add requests for procedures that *really* belong to this module.
    module_info_get_name(ModuleInfo, ThisModule),
    ( ThisModule = ModuleName ->
        % Add requests corresponding to the call patterns of existing answers.
        lookup_existing_call_patterns(AnalysisInfo, analysis_name, ModuleName,
            FuncId, OldCalls),
        list.foldl(add_reuse_request(PPId), OldCalls, !ExternalRequests),

        % Add new requests from other modules.
        lookup_requests(AnalysisInfo, analysis_name, ModuleName, FuncId,
            NewCalls),
        list.foldl(add_reuse_request(PPId), NewCalls, !ExternalRequests),

        % A procedure listed as having conditional reuse *must* have a reuse
        % version procedure, even if in this analysis we don't find
        % conditional reuse.
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
        Call = structure_reuse_call([]),
        lookup_best_result(AnalysisInfo, ModuleName, FuncId, FuncInfo, Call,
            MaybeBestResult),
        (
            MaybeBestResult = yes(analysis_result(_, Answer, _)),
            (
                Answer = structure_reuse_answer_no_reuse
            ;
                Answer = structure_reuse_answer_unconditional
            ;
                Answer = structure_reuse_answer_conditional(_, _, _),
                !:MustHaveReuseVersions = [PPId | !.MustHaveReuseVersions]
            )
        ;
            MaybeBestResult = no
        )
    ;
        true
    ).

:- pred add_reuse_request(pred_proc_id::in, structure_reuse_call::in,
    list(sr_request)::in, list(sr_request)::out) is det.

add_reuse_request(PPId, structure_reuse_call(NoClobbers), !Requests) :-
    (
        NoClobbers = []
        % We don't need to add these as explicit requests, and in fact it's
        % better if we don't.  The analysis is already designed to analyse for
        % this case by default and create the reuse procedures if necessary.
    ;
        NoClobbers = [_ | _],
        !:Requests = [sr_request(PPId, NoClobbers) | !.Requests]
    ).

%-----------------------------------------------------------------------------%

:- pred save_reuse_in_module_info(pred_proc_id::in, reuse_as_and_status::in,
    module_info::in, module_info::out) is det.

save_reuse_in_module_info(PPId, ReuseAs_Status, !ModuleInfo) :-
    ReuseAs_Status = reuse_as_and_status(ReuseAs, Status),
    ReuseDomain = to_structure_reuse_domain(ReuseAs),
    Domain_Status = structure_reuse_domain_and_status(ReuseDomain, Status),

    module_info_pred_proc_info(!.ModuleInfo, PPId, PredInfo, ProcInfo0),
    proc_info_set_structure_reuse(Domain_Status, ProcInfo0, ProcInfo),
    module_info_set_pred_proc_info(PPId, PredInfo, ProcInfo, !ModuleInfo).

:- pred annotate_in_use_information(module_info::in,
    proc_info::in, proc_info::out) is det.

annotate_in_use_information(ModuleInfo, !ProcInfo) :-
    forward_use_information(!ProcInfo),
    backward_use_information(ModuleInfo, !ProcInfo),
    fill_goal_path_slots_in_proc(ModuleInfo, !ProcInfo).

%-----------------------------------------------------------------------------%
%
% Code for writing out optimization interfaces
%

:- pred make_opt_int(module_info::in, module_info::out, io::di, io::uo) is det.

make_opt_int(!ModuleInfo, !IO) :-
    module_info_get_globals(!.ModuleInfo, Globals),
    module_info_get_name(!.ModuleInfo, ModuleName),
    module_name_to_file_name(Globals, ModuleName, ".opt.tmp",
        do_not_create_dirs, OptFileName, !IO),
    globals.lookup_bool_option(Globals, verbose, Verbose),
    maybe_write_string(Verbose, "% Appending structure_reuse pragmas to ",
        !IO),
    maybe_write_string(Verbose, add_quotes(OptFileName), !IO),
    maybe_write_string(Verbose, "...", !IO),
    maybe_flush_output(Verbose, !IO),
    io.open_append(OptFileName, OptFileRes, !IO),
    (
        OptFileRes = ok(OptFile),
        io.set_output_stream(OptFile, OldStream, !IO),
        module_info_get_valid_predids(PredIds, !ModuleInfo),
        list.foldl(write_pred_reuse_info(!.ModuleInfo), PredIds, !IO),
        io.set_output_stream(OldStream, _, !IO),
        io.close_output(OptFile, !IO),
        maybe_write_string(Verbose, " done.\n", !IO)
    ;
        OptFileRes = error(IOError),
        maybe_write_string(Verbose, " failed!\n", !IO),
        io.error_message(IOError, IOErrorMessage),
        io.write_strings(["Error opening file `",
            OptFileName, "' for output: ", IOErrorMessage], !IO),
        io.set_exit_status(1, !IO)
    ).

%-----------------------------------------------------------------------------%
%
% Code for writing out structure_reuse pragmas.
%

write_pred_reuse_info(ModuleInfo, PredId, !IO) :-
    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    PredName = pred_info_name(PredInfo),
    ProcIds = pred_info_procids(PredInfo),
    PredOrFunc = pred_info_is_pred_or_func(PredInfo),
    ModuleName = pred_info_module(PredInfo),
    pred_info_get_procedures(PredInfo, ProcTable),
    pred_info_get_context(PredInfo, Context),
    SymName = qualified(ModuleName, PredName),
    pred_info_get_typevarset(PredInfo, TypeVarSet),
    list.foldl(write_proc_reuse_info(ModuleInfo, PredId, PredInfo, ProcTable,
        PredOrFunc, SymName, Context, TypeVarSet), ProcIds, !IO).

:- pred write_proc_reuse_info(module_info::in, pred_id::in, pred_info::in,
    proc_table::in, pred_or_func::in, sym_name::in, prog_context::in,
    tvarset::in, proc_id::in, io::di, io::uo) is det.

write_proc_reuse_info(ModuleInfo, PredId, PredInfo, ProcTable, PredOrFunc,
        SymName, Context, TypeVarSet, ProcId, !IO) :-
    should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_pragma, ShouldWrite),
    (
        ShouldWrite = yes,
        map.lookup(ProcTable, ProcId, ProcInfo),
        proc_info_get_structure_reuse(ProcInfo, MaybeStructureReuseDomain),
        (
            MaybeStructureReuseDomain = yes(
                structure_reuse_domain_and_status(Reuse, _Status)),
            proc_info_declared_argmodes(ProcInfo, Modes),
            proc_info_get_varset(ProcInfo, VarSet),
            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_vartypes(ProcInfo, VarTypes),
            lookup_var_types(VarTypes, HeadVars, HeadVarTypes),
            MaybeReuse = yes(Reuse),
            PredNameModesPF = pred_name_modes_pf(SymName, Modes, PredOrFunc),
            ReuseInfo = pragma_info_structure_reuse(PredNameModesPF,
                HeadVars, HeadVarTypes, MaybeReuse),
            write_pragma_structure_reuse_info(ReuseInfo,
                yes(VarSet), yes(TypeVarSet), Context, !IO)
        ;
            MaybeStructureReuseDomain = no
        )
    ;
        ShouldWrite = no
    ).

%-----------------------------------------------------------------------------%
%
% Types and instances for the intermodule analysis framework.
%

:- type structure_reuse_call
    --->    structure_reuse_call(no_clobber_args).

:- type structure_reuse_answer
    --->    structure_reuse_answer_no_reuse
    ;       structure_reuse_answer_unconditional
    ;       structure_reuse_answer_conditional(
                srac_vars   :: prog_vars,
                srac_types  :: list(mer_type),
                srac_conds  :: structure_reuse_conditions
                % We cannot keep this as a reuse_as.  When the analysis answers
                % are loaded, we don't have enough information to rename the
                % variables in the .analysis answer to the correct variables
                % for the proc_info that the reuse_as will be used with.
            ).

:- type structure_reuse_func_info
    --->    structure_reuse_func_info(
                srfi_module :: module_info,
                srfi_proc   :: proc_info
            ).

:- func analysis_name = string.

analysis_name = "structure_reuse".

:- instance analysis(structure_reuse_func_info, structure_reuse_call,
    structure_reuse_answer) where
[
    analysis_name(_, _) = analysis_name,
    analysis_version_number(_, _) = 3,
    preferred_fixpoint_type(_, _) = greatest_fixpoint,
    bottom(_, _) = structure_reuse_answer_no_reuse,
    ( top(_, _) = _ :-
        % We have no representation for "all possible conditions".
        unexpected($module, $pred, "top/2 called")
    ),
    ( get_func_info(ModuleInfo, ModuleName, FuncId, _, _, FuncInfo) :-
        func_id_to_ppid(ModuleInfo, ModuleName, FuncId, PPId),
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo)
    )
].

:- instance call_pattern(structure_reuse_func_info, structure_reuse_call)
    where [].

:- instance partial_order(structure_reuse_func_info, structure_reuse_call)
        where [
    (more_precise_than(_, Call1, Call2) :-
        Call1 = structure_reuse_call(Args1),
        Call2 = structure_reuse_call(Args2),
        set.subset(sorted_list_to_set(Args2), sorted_list_to_set(Args1))
    ),
    equivalent(_, Call, Call)
].

:- instance to_term(structure_reuse_call) where [
    ( to_term(Call) = Term :-
        Call = structure_reuse_call(NoClobbers),
        type_to_term(NoClobbers, Term)
    ),
    ( from_term(Term, Call) :-
        term_to_type(Term, NoClobbers),
        Call = structure_reuse_call(NoClobbers)
    )
].

:- instance answer_pattern(structure_reuse_func_info, structure_reuse_answer)
    where [].

:- instance partial_order(structure_reuse_func_info, structure_reuse_answer)
        where [

    % We deliberately have `conditional' reuse incomparable with
    % `unconditional' reuse.  If they were comparable, a caller using an
    % `conditional' answer would would only be marked `suboptimal' if that
    % answer changes to `unconditional'.  Since we don't honour the old
    % `conditional' answer by generating that version of the procedure, there
    % would be a linking error if the caller is not updated to call the
    % unconditional version.

    (more_precise_than(FuncInfo, Answer1, Answer2) :-
        (
            Answer1 = structure_reuse_answer_conditional(_, _, _),
            Answer2 = structure_reuse_answer_no_reuse
        ;
            Answer1 = structure_reuse_answer_unconditional,
            Answer2 = structure_reuse_answer_no_reuse
        ;
            Answer1 = structure_reuse_answer_conditional(_, _, _),
            Answer2 = structure_reuse_answer_conditional(_, _, _),
            FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_vartypes(ProcInfo, VarTypes),
            lookup_var_types(VarTypes, HeadVars, HeadVarTypes),
            structure_reuse_answer_to_domain(HeadVarTypes, ProcInfo, Answer1,
                Reuse1),
            structure_reuse_answer_to_domain(HeadVarTypes, ProcInfo, Answer2,
                Reuse2),
            ReuseAs1 = from_structure_reuse_domain(Reuse1),
            ReuseAs2 = from_structure_reuse_domain(Reuse2),
            reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs1, ReuseAs2),
            not reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs2, ReuseAs1)
        )
    ),

    (equivalent(FuncInfo, Answer1, Answer2) :-
        (
            Answer1 = Answer2
        ;
            Answer1 = structure_reuse_answer_conditional(_, _, _),
            Answer2 = structure_reuse_answer_conditional(_, _, _),
            FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
            proc_info_get_headvars(ProcInfo, HeadVars),
            proc_info_get_vartypes(ProcInfo, VarTypes),
            lookup_var_types(VarTypes, HeadVars, HeadVarTypes),
            structure_reuse_answer_to_domain(HeadVarTypes, ProcInfo, Answer1,
                Reuse1),
            structure_reuse_answer_to_domain(HeadVarTypes, ProcInfo, Answer2,
                Reuse2),
            ReuseAs1 = from_structure_reuse_domain(Reuse1),
            ReuseAs2 = from_structure_reuse_domain(Reuse2),
            reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs2, ReuseAs1),
            reuse_as_subsumed_by(ModuleInfo, ProcInfo, ReuseAs1, ReuseAs2)
        )
    )
].

:- instance to_term(structure_reuse_answer) where [
    func(to_term/1) is reuse_answer_to_term,
    pred(from_term/2) is reuse_answer_from_term
].

:- func reuse_answer_to_term(structure_reuse_answer) = term.

reuse_answer_to_term(Answer) = Term :-
    (
        Answer = structure_reuse_answer_no_reuse,
        Term = term.functor(atom("no_reuse"), [], term.context_init)
    ;
        Answer = structure_reuse_answer_unconditional,
        Term = term.functor(atom("uncond"), [], term.context_init)
    ;
        Answer = structure_reuse_answer_conditional(HeadVars, Types,
            Conditions),
        type_to_term(HeadVars, HeadVarsTerm),
        type_to_term(Types, TypesTerm),
        type_to_term(Conditions, ConditionsTerm),
        Term = term.functor(atom("cond"),
            [HeadVarsTerm, TypesTerm, ConditionsTerm], term.context_init)
    ).

:- pred reuse_answer_from_term(term::in, structure_reuse_answer::out)
    is semidet.

reuse_answer_from_term(Term, Answer) :-
    (
        Term = functor(atom("no_reuse"), [], _),
        Answer = structure_reuse_answer_no_reuse
    ;
        Term = functor(atom("uncond"), [], _),
        Answer = structure_reuse_answer_unconditional
    ;
        Term = functor(atom("cond"),
            [HeadVarsTerm, TypesTerm, ConditionsTerm], _),
        term_to_type(HeadVarsTerm, HeadVars),
        term_to_type(TypesTerm, Types),
        term_to_type(ConditionsTerm, Conditions),
        Answer = structure_reuse_answer_conditional(HeadVars, Types,
            Conditions)
    ).

%-----------------------------------------------------------------------------%
%
% Additional predicates used for intermodule analysis
%

:- pred record_structure_reuse_results(module_info::in,
    bimap(ppid_no_clobbers, pred_proc_id)::in, pred_proc_id::in,
    reuse_as_and_status::in, analysis_info::in, analysis_info::out) is det.

record_structure_reuse_results(ModuleInfo, CondReuseMap, PPId, ReuseAs_Status,
        !AnalysisInfo) :-
    ( bimap.reverse_search(CondReuseMap, Key, PPId) ->
        % PPId is a conditional reuse procedure created from another procedure.
        % We need to record the result using the name of the original
        % procedure.
        Key = ppid_no_clobbers(RecordPPId, NoClobbers)
    ;
        RecordPPId = PPId,
        NoClobbers = []
    ),
    record_structure_reuse_results_2(ModuleInfo, RecordPPId, NoClobbers,
        ReuseAs_Status, !AnalysisInfo).

:- pred record_structure_reuse_results_2(module_info::in, pred_proc_id::in,
    no_clobber_args::in, reuse_as_and_status::in,
    analysis_info::in, analysis_info::out) is det.

record_structure_reuse_results_2(ModuleInfo, PPId, NoClobbers, ReuseAs_Status,
        !AnalysisInfo) :-
    PPId = proc(PredId, ProcId),
    ReuseAs_Status = reuse_as_and_status(ReuseAs, Status),

    module_info_pred_info(ModuleInfo, PredId, PredInfo),
    should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo,
        for_analysis_framework, ShouldWrite),
    (
        ShouldWrite = yes,
        reuse_as_to_structure_reuse_answer(ModuleInfo, PPId, ReuseAs, Answer),
        module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
        record_result(ModuleName, FuncId, structure_reuse_call(NoClobbers),
            Answer, Status, !AnalysisInfo)
    ;
        ShouldWrite = no
    ).

:- pred reuse_as_to_structure_reuse_answer(module_info::in, pred_proc_id::in,
    reuse_as::in, structure_reuse_answer::out) is det.

reuse_as_to_structure_reuse_answer(ModuleInfo, PPId, ReuseAs, Answer) :-
    Reuse = to_structure_reuse_domain(ReuseAs),
    (
        Reuse = has_no_reuse,
        Answer = structure_reuse_answer_no_reuse
    ;
        Reuse = has_only_unconditional_reuse,
        Answer = structure_reuse_answer_unconditional
    ;
        Reuse = has_conditional_reuse(Conditions),
        module_info_proc_info(ModuleInfo, PPId, ProcInfo),
        proc_info_get_headvars(ProcInfo, HeadVars),
        proc_info_get_vartypes(ProcInfo, VarTypes),
        lookup_var_types(VarTypes, HeadVars, HeadVarTypes),
        Answer = structure_reuse_answer_conditional(HeadVars, HeadVarTypes,
            Conditions)
    ).

:- pred handle_structure_reuse_dependency(module_info::in,
    ppid_no_clobbers::in, analysis_info::in, analysis_info::out) is det.

handle_structure_reuse_dependency(ModuleInfo,
        ppid_no_clobbers(DepPPId, NoClobbers), !AnalysisInfo) :-
    % Record that we depend on the result for the called procedure.
    module_name_func_id(ModuleInfo, DepPPId, DepModuleName, DepFuncId),
    Call = structure_reuse_call(NoClobbers),
    Answer = _ : structure_reuse_answer,
    get_func_info(ModuleInfo, DepModuleName, DepFuncId, Call, Answer,
        FuncInfo),
    record_dependency(DepModuleName, DepFuncId, FuncInfo, Call, Answer,
        !AnalysisInfo).

:- pred record_intermod_requests(module_info::in, sr_request::in,
    analysis_info::in, analysis_info::out) is det.

record_intermod_requests(ModuleInfo, sr_request(PPId, NoClobbers),
        !AnalysisInfo) :-
    module_name_func_id(ModuleInfo, PPId, ModuleName, FuncId),
    record_request(analysis_name, ModuleName, FuncId,
        structure_reuse_call(NoClobbers), !AnalysisInfo).

%-----------------------------------------------------------------------------%

:- type should_write_for
    --->    for_analysis_framework
    ;       for_pragma.

:- pred should_write_reuse_info(module_info::in, pred_id::in, proc_id::in,
    pred_info::in, should_write_for::in, bool::out) is det.

should_write_reuse_info(ModuleInfo, PredId, ProcId, PredInfo, WhatFor,
        ShouldWrite) :-
    (
        procedure_is_exported(ModuleInfo, PredInfo, ProcId),
        \+ is_unify_or_compare_pred(PredInfo),

        % Don't write out info for reuse versions of procedures.
        pred_info_get_origin(PredInfo, PredOrigin),
        PredOrigin \= origin_transformed(transform_structure_reuse, _, _),

        (
            WhatFor = for_analysis_framework
        ;
            WhatFor = for_pragma,
            % XXX These should be allowed, but the predicate declaration for
            % the specialized predicate is not produced before the structure
            % reuse pragmas are read in, resulting in an undefined predicate
            % error.
            module_info_get_type_spec_info(ModuleInfo, TypeSpecInfo),
            TypeSpecInfo = type_spec_info(_, TypeSpecForcePreds, _, _),
            \+ set.member(PredId, TypeSpecForcePreds)
        )
    ->
        ShouldWrite = yes
    ;
        ShouldWrite = no
    ).

%-----------------------------------------------------------------------------%
%
% for structure_reuse.versions
%

structure_reuse_answer_harsher_than_in_analysis_registry(ModuleInfo,
        ReuseTable, ReusePPId, Harsher) :-
    module_info_get_analysis_info(ModuleInfo, AnalysisInfo),

    % Find the original pred_proc_id and no-clobber list that this reuse
    % procedure was made for.
    reuse_as_table_reverse_search_reuse_version_proc(ReuseTable, ReusePPId,
        OrigPPId, NoClobbers),

    % Look up the old result.
    module_name_func_id(ModuleInfo, OrigPPId, ModuleName, FuncId),
    module_info_proc_info(ModuleInfo, OrigPPId, ProcInfo),
    FuncInfo = structure_reuse_func_info(ModuleInfo, ProcInfo),
    Call = structure_reuse_call(NoClobbers),
    analysis.lookup_best_result(AnalysisInfo, ModuleName, FuncId, FuncInfo,
        Call, MaybeOldResult),
    (
        MaybeOldResult = yes(analysis_result(OldCall, OldAnswer, _)),
        equivalent(FuncInfo, Call, OldCall)
    ->
        % Compare with the new result.
        lookup_new_structure_reuse_answer(ModuleInfo, ReuseTable, ReusePPId,
            NewAnswer),
        ( more_precise_than(FuncInfo, NewAnswer, OldAnswer) ->
            Harsher = yes,
            trace [
                compile_time(flag("harsher_answer_check")),
                runtime(env("HARSHER_ANSWER_CHECK")),
                io(!IO)
            ] (
                io.write_string("Structure reuse answer for ", !IO),
                write_pred_proc_id(ModuleInfo, ReusePPId, !IO),
                io.write_string(" has harsher conditions than listed " ++
                    "in analysis file.\n", !IO),
                io.write_string("was: ", !IO),
                io.write(OldAnswer, !IO),
                io.nl(!IO),
                io.write_string("now: ", !IO),
                io.write(NewAnswer, !IO),
                io.nl(!IO)
            )
        ;
            Harsher = no
        )
    ;
        Harsher = no
    ).

:- pred lookup_new_structure_reuse_answer(module_info::in, reuse_as_table::in,
    pred_proc_id::in, structure_reuse_answer::out) is det.

lookup_new_structure_reuse_answer(ModuleInfo, ReuseTable, ReusePPId,
        NewAnswer) :-
    ( reuse_as_table_search(ReuseTable, ReusePPId, ReuseAs_Status) ->
        ReuseAs_Status = reuse_as_and_status(NewReuseAs, _)
    ;
        unexpected($module, $pred, "search failed")
    ),
    reuse_as_to_structure_reuse_answer(ModuleInfo, ReusePPId, NewReuseAs,
        NewAnswer).

%-----------------------------------------------------------------------------%

:- pred remove_useless_reuse_proc(module_info::in, bool::in,
    map(pred_proc_id, reuse_as_and_status)::in,
    ppid_no_clobbers::in, pred_proc_id::in,
    predicate_table::in, predicate_table::out) is det.

remove_useless_reuse_proc(ModuleInfo, VeryVerbose, ReuseAsMap, _, PPId,
        !PredTable) :-
    map.lookup(ReuseAsMap, PPId, ReuseAs_Status),
    ReuseAs_Status = reuse_as_and_status(ReuseAs, _),
    % XXX perhaps we can also remove reuse procedures with only unconditional
    % reuse?  Such a procedure should be the same as the "non-reuse" procedure
    % (which also implements any unconditional reuse).
    ( reuse_as_no_reuses(ReuseAs) ->
        (
            VeryVerbose = yes,
            trace [io(!IO)] (
                io.write_string("% Removing useless reuse ", !IO),
                write_pred_proc_id(ModuleInfo, PPId, !IO),
                io.nl(!IO)
            )
        ;
            VeryVerbose = no
        ),

        PPId = proc(PredId, _),
        % We can remove the whole predicate because we never generate
        % multi-moded reuse versions of predicates.
        predicate_table_remove_predicate(PredId, !PredTable)
    ;
        true
    ).

%-----------------------------------------------------------------------------%
:- end_module transform_hlds.ctgc.structure_reuse.analysis.
%-----------------------------------------------------------------------------%
