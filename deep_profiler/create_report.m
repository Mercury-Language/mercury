%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008 The University of Melbourne.
% This file may only be copied under the terms of the GNU General
% Public License - see the file COPYING in the Mercury distribution.
%-----------------------------------------------------------------------------%
%
% File: create_report.m.
% Author: pbone.
%
% This module creates a report from a deep data structure and a query.
%
%-----------------------------------------------------------------------------%

:- module create_report.
:- interface.

:- import_module maybe.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module profile.
:- import_module query.
:- import_module report.

%-----------------------------------------------------------------------------%

:- pred create_report(cmd::in, deep::in, maybe_error(prog_rep)::in, 
    deep_report::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module apply_exclusion.
:- import_module measurement_units.
:- import_module measurements.
:- import_module program_representation_utils.
:- import_module top_procs.

:- import_module array.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module list.
:- import_module map.
:- import_module math.
:- import_module maybe.
:- import_module pair.
:- import_module require.
:- import_module string.
:- import_module svmap.
:- import_module univ.

%-----------------------------------------------------------------------------%

create_report(Cmd, Deep, MaybeProgRep, Report) :-
    (
        Cmd = deep_cmd_quit,
        Msg = string.format("Shutting down deep profile server for %s.",
            [s(Deep ^ data_file_name)]),
        MessageReport = message_report(Msg),
        Report = report_message(MessageReport)
    ;
        Cmd = deep_cmd_timeout(Timeout),
        Msg = string.format("Timeout set to %d minutes.", [i(Timeout)]),
        MessageReport = message_report(Msg),
        Report = report_message(MessageReport)
    ;
        Cmd = deep_cmd_menu,
        Deep ^ profile_stats = profile_stats(ProgramName,
            NumCSD, NumCSS, NumPD, NumPS,
            QuantaPerSec, InstrumentationQuanta, UserQuanta, NumCallseqs,
            _, _),
        NumCliques = array.max(Deep ^ clique_members),
        MenuReport = menu_report(ProgramName, QuantaPerSec,
            UserQuanta, InstrumentationQuanta,
            NumCallseqs, NumCSD, NumCSS, NumPD, NumPS, NumCliques),
        Report = report_menu(ok(MenuReport))
    ;
        Cmd = deep_cmd_program_modules,
        create_program_modules_report(Deep, MaybeProgramModulesReport),
        Report = report_program_modules(MaybeProgramModulesReport)
    ;
        Cmd = deep_cmd_module(ModuleName),
        create_module_report(Deep, ModuleName, MaybeModuleReport),
        Report = report_module(MaybeModuleReport)
    ;
        Cmd = deep_cmd_top_procs(Limit, CostKind, InclDesc, Scope),
        create_top_procs_report(Deep, Limit, CostKind, InclDesc, Scope,
            MaybeTopProcsReport),
        Report = report_top_procs(MaybeTopProcsReport)
    ;
        Cmd = deep_cmd_procrep_coverage(PSPtr),
        (   
            MaybeProgRep = ok(ProgRep),
            generate_procrep_coverage_dump_report(Deep, ProgRep, PSPtr,
                MaybeProcrepCoverageReport)
        ;
            MaybeProgRep = error(Error),
            MaybeProcrepCoverageReport = 
                error("No procedure representation information: " ++ Error) 
        ),
        Report = report_procrep_coverage_dump(MaybeProcrepCoverageReport)
    ;
        Cmd = deep_cmd_proc(PSPtr),
        create_proc_report(Deep, PSPtr, MaybeProcReport),
        Report = report_proc(MaybeProcReport)
    ;
        Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum, Contour),
        create_proc_callers_report(Deep, PSPtr, CallerGroups, BunchNum,
            Contour, MaybeProcCallersReport),
        Report = report_proc_callers(MaybeProcCallersReport)
    ;
        Cmd = deep_cmd_dump_proc_static(PSPtr),
        create_proc_static_dump_report(Deep, PSPtr, MaybeProcStaticDump),
        Report = report_proc_static_dump(MaybeProcStaticDump)
    ;
        Cmd = deep_cmd_dump_proc_dynamic(PDPtr),
        create_proc_dynamic_dump_report(Deep, PDPtr, MaybeProcDynamicDump),
        Report = report_proc_dynamic_dump(MaybeProcDynamicDump)
    ;
        Cmd = deep_cmd_dump_call_site_static(CSSPtr),
        create_call_site_static_dump_report(Deep, CSSPtr,
            MaybeCallSiteStaticDump),
        Report = report_call_site_static_dump(MaybeCallSiteStaticDump)
    ;
        Cmd = deep_cmd_dump_call_site_dynamic(CSDPtr),
        create_call_site_dynamic_dump_report(Deep, CSDPtr,
            MaybeCallSiteStaticDump),
        Report = report_call_site_dynamic_dump(MaybeCallSiteStaticDump)
    ;
        Cmd = deep_cmd_restart,
        error("create_report/3", "unexpected restart command")
    ;
        ( Cmd = deep_cmd_root(_)
        ; Cmd = deep_cmd_clique(_)
        ; Cmd = deep_cmd_dump_clique(_)
        ),
        error("create_report/3", "Command not supported: " ++ string(Cmd))
    ).

%-----------------------------------------------------------------------------%
%
% Code to build a program_modules report.
%

    % Create a modules report, from the given data with the specified
    % parameters.
    %
:- pred create_program_modules_report(deep::in,
    maybe_error(program_modules_report)::out) is det.

create_program_modules_report(Deep, MaybeProgramModulesReport) :-
    map.to_assoc_list(Deep ^ module_data, ModulePairs0),
    list.filter(not_mercury_runtime, ModulePairs0, ModulePairs),
    ModuleRowDatas = list.map(module_pair_to_row_data(Deep), ModulePairs),
    ProgramModulesReport = program_modules_report(ModuleRowDatas),
    MaybeProgramModulesReport = ok(ProgramModulesReport).

:- pred not_mercury_runtime(pair(string, module_data)::in) is semidet.

not_mercury_runtime(ModuleName - _) :-
    ModuleName \= "Mercury runtime".

:- func module_pair_to_row_data(deep, pair(string, module_data))
    = perf_row_data(module_active).

module_pair_to_row_data(Deep, ModuleName - ModuleData) = ModuleRowData :-
    Own = ModuleData ^ module_own,
    IsActive = compute_is_active(Own),
    (
        IsActive = is_active,
        ModuleIsActive = module_is_active
    ;
        IsActive = is_not_active,
        ModuleIsActive = module_is_not_active
    ),
    ModuleActive = module_active(ModuleName, ModuleIsActive),
    own_and_maybe_inherit_to_perf_row_data(Deep, ModuleActive, Own, no,
        ModuleRowData).

%-----------------------------------------------------------------------------%
%
% Code to build a module report.
%

    % Create a module report, from the given data with the specified
    % parameters.
    %
:- pred create_module_report(deep::in, string::in,
    maybe_error(module_report)::out) is det.

create_module_report(Deep, ModuleName, MaybeModuleReport) :-
    ( map.search(Deep ^ module_data, ModuleName, ModuleData) ->
        PSPtrs = ModuleData ^ module_procs, 
        ProcRowDatas = list.map(proc_to_active_row_data(Deep), PSPtrs),
        ModuleReport = module_report(ModuleName, ProcRowDatas),
        MaybeModuleReport = ok(ModuleReport)
    ;
        Msg = string.format("There is no module named `%s'.\n",
            [s(ModuleName)]),
        MaybeModuleReport = error(Msg)
    ).

:- func proc_to_active_row_data(deep, proc_static_ptr)
    = perf_row_data(proc_active).

proc_to_active_row_data(Deep, PSPtr) = ProcRowData :-
    deep_lookup_ps_own(Deep, PSPtr, Own),
    deep_lookup_ps_desc(Deep, PSPtr, Desc),
    IsActive = compute_is_active(Own),
    (
        IsActive = is_active,
        ProcIsActive = proc_is_active
    ;
        IsActive = is_not_active,
        ProcIsActive = proc_is_not_active
    ),
    ProcDesc = describe_proc(Deep, PSPtr),
    ProcActive = proc_active(ProcDesc, ProcIsActive),
    own_and_inherit_to_perf_row_data(Deep, ProcActive, Own, Desc, ProcRowData).

%-----------------------------------------------------------------------------%
%
% Code to build a top_procs report.
%

    % Create a top procs report, from the given data with the specified
    % parameters.
    %
:- pred create_top_procs_report(deep::in, display_limit::in, cost_kind::in,
    include_descendants::in, measurement_scope::in,
    maybe_error(top_procs_report)::out) is det.

create_top_procs_report(Deep, Limit, CostKind, InclDesc0, Scope0,
        MaybeTopProcsReport) :-
    (
        CostKind = cost_calls,
        % Counting calls is incompatible both with self_and_desc
        % and per_call.
        InclDesc = self,
        Scope = overall
    ;
        ( CostKind = cost_redos
        ; CostKind = cost_time
        ; CostKind = cost_callseqs
        ; CostKind = cost_allocs
        ; CostKind = cost_words
        ),
        InclDesc = InclDesc0,
        Scope = Scope0
    ),
    MaybeTopPSIs = find_top_procs(CostKind, InclDesc, Scope, Limit, Deep),
    (
        MaybeTopPSIs = error(ErrorMessage),
        MaybeTopProcsReport = error("Internal error: " ++ ErrorMessage)
    ;
        MaybeTopPSIs = ok(TopPSIs),
        Ordering = report_ordering(Limit, CostKind, InclDesc, Scope),
        list.map(psi_to_perf_row_data(Deep), TopPSIs, ProcRowDatas),
        TopProcsReport = top_procs_report(Ordering, ProcRowDatas),
        MaybeTopProcsReport = ok(TopProcsReport)
    ).

%-----------------------------------------------------------------------------%
%
% Code to build a proc report.
%

:- pred create_proc_report(deep::in, proc_static_ptr::in,
    maybe_error(proc_report)::out) is det.

create_proc_report(Deep, PSPtr, MaybeProcReport) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        ProcDesc = describe_proc(Deep, PSPtr),
        deep_lookup_ps_own(Deep, PSPtr, Own),
        deep_lookup_ps_desc(Deep, PSPtr, Desc),
        own_and_inherit_to_perf_row_data(Deep, ProcDesc, Own, Desc,
            ProcSummaryRowData),

        deep_lookup_proc_statics(Deep, PSPtr, PS),
        CallSitesArray = PS ^ ps_sites,
        array.to_list(CallSitesArray, CallSites),
        ProcCallSiteSummaryRowDatas = list.map(create_call_site_summary(Deep),
            CallSites),

        ProcReport = proc_report(ProcSummaryRowData,
            ProcCallSiteSummaryRowDatas),
        MaybeProcReport = ok(ProcReport)
    ;
        MaybeProcReport = error("invalid proc_static index")
    ).

:- func create_call_site_summary(deep, call_site_static_ptr) = call_site_perf.

create_call_site_summary(Deep, CSSPtr) = CallSitePerf :-
    CallSiteDesc = describe_call_site(Deep, CSSPtr),

    deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
    KindAndCallee = CSS ^ css_kind,
    CallerPSPtr = CSS ^ css_container,
    GoalPathString = CSS ^ css_goal_path,
    ( goal_path_from_string(GoalPathString, GoalPathPrime) ->
        GoalPath = GoalPathPrime
    ;
        error("create_call_site_summary: " ++ 
            "Couldn't convert string to goal path: " ++ GoalPathString)
    ),

    deep_lookup_call_site_calls(Deep, CSSPtr, CallSiteCallMap),
    map.to_assoc_list(CallSiteCallMap, CallSiteCalls),

    (
        KindAndCallee = normal_call_and_callee(CalleePSPtr, TypeSubstStr),
        CalleeDesc = describe_proc(Deep, CalleePSPtr),
        NormalCallId = normal_callee_id(CalleeDesc, TypeSubstStr),
        KindAndInfo = normal_call_and_info(NormalCallId),
        (
            CallSiteCalls = [],
            Own = zero_own_prof_info,
            Desc = zero_inherit_prof_info
        ;
            CallSiteCalls = [CallSiteCall],
            CallSiteCall = CalleePSPtrFromCall - _,
            require(unify(CalleePSPtr, CalleePSPtrFromCall),
                "create_call_site_summary: callee mismatch"),
            CallSiteCalleePerf = generate_call_site_callee_perf(Deep,
                CallerPSPtr, CallSiteCall),
            CallSiteCalleePerf = call_site_callee_perf(_, Own, Desc)
        ;
            CallSiteCalls = [_, _ | _],
            error("create_call_site_summary: >1 proc called at site")
        ),
        own_and_inherit_to_perf_row_data(Deep, CallSiteDesc, Own, Desc,
            SummaryRowData),
        SubRowDatas = []
    ;
        (
            KindAndCallee = special_call_and_no_callee,
            KindAndInfo = special_call_and_no_info
        ;
            KindAndCallee = higher_order_call_and_no_callee,
            KindAndInfo = higher_order_call_and_no_info
        ;
            KindAndCallee = method_call_and_no_callee,
            KindAndInfo = method_call_and_no_info
        ;
            KindAndCallee = callback_and_no_callee,
            KindAndInfo = callback_and_no_info
        ),
        CallSiteCalleePerfs =
            list.map(generate_call_site_callee_perf(Deep, CallerPSPtr),
            CallSiteCalls),
        list.map_foldl2(accumulate_call_site_callees(Deep),
            CallSiteCalleePerfs, SubRowDatas,
            zero_own_prof_info, SumOwn, zero_inherit_prof_info, SumDesc),
        own_and_inherit_to_perf_row_data(Deep, CallSiteDesc, SumOwn, SumDesc,
            SummaryRowData)
    ),
    CallSitePerf = call_site_perf(KindAndInfo, SummaryRowData, SubRowDatas, 
        GoalPath).

:- type call_site_callee_perf
    --->    call_site_callee_perf(
                cscpi_callee            :: proc_static_ptr,
                cscpi_own_prof_info     :: own_prof_info,
                cscpi_inherit_prof_info :: inherit_prof_info
            ).

:- func generate_call_site_callee_perf(deep, proc_static_ptr,
    pair(proc_static_ptr, list(call_site_dynamic_ptr)))
    = call_site_callee_perf.

generate_call_site_callee_perf(Deep, CallerPSPtr, PSPtr - CSDPtrs)
        = CalleeProf :-
    list.foldl2(accumulate_csd_prof_info(Deep, CallerPSPtr), CSDPtrs,
        zero_own_prof_info, Own, zero_inherit_prof_info, Desc),
    CalleeProf = call_site_callee_perf(PSPtr, Own, Desc).

:- pred accumulate_csd_prof_info(deep::in, proc_static_ptr::in,
    call_site_dynamic_ptr::in,
    own_prof_info::in, own_prof_info::out,
    inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_csd_prof_info(Deep, CallerPSPtr, CSDPtr, !Own, !Desc) :-
    deep_lookup_csd_own(Deep, CSDPtr, CSDOwn),
    deep_lookup_csd_desc(Deep, CSDPtr, CSDDesc),
    !:Own = add_own_to_own(!.Own, CSDOwn),
    !:Desc = add_inherit_to_inherit(!.Desc, CSDDesc),
    deep_lookup_csd_comp_table(Deep, CSDPtr, CompTableArray),
    ( map.search(CompTableArray, CallerPSPtr, InnerTotal) ->
        !:Desc = subtract_inherit_from_inherit(InnerTotal, !.Desc)
    ;
        true
    ).

:- pred accumulate_call_site_callees(deep::in,
    call_site_callee_perf::in, perf_row_data(proc_desc)::out,
    own_prof_info::in, own_prof_info::out,
    inherit_prof_info::in, inherit_prof_info::out) is det.

accumulate_call_site_callees(Deep, CalleePerf, RowData, !Own, !Desc) :-
    CalleePerf = call_site_callee_perf(CalleePSPtr, CalleeOwn, CalleeDesc),
    CalleeProcDesc = describe_proc(Deep, CalleePSPtr),
    own_and_inherit_to_perf_row_data(Deep, CalleeProcDesc,
        CalleeOwn, CalleeDesc, RowData),
    !:Own = add_own_to_own(!.Own, CalleeOwn),
    !:Desc = add_inherit_to_inherit(!.Desc, CalleeDesc).

%-----------------------------------------------------------------------------%
%
% Code to build a proc_callers report.
%

:- pred create_proc_callers_report(deep::in, proc_static_ptr::in,
    caller_groups::in, int::in, contour_exclusion::in,
    maybe_error(proc_callers_report)::out) is det.

create_proc_callers_report(Deep, PSPtr, CallerGroups, BunchNum, Contour,
        MaybeProcCallersReport) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        ProcDesc = describe_proc(Deep, PSPtr),

        deep_lookup_proc_callers(Deep, PSPtr, CallerCSDPtrs),
        MaybeMaybeExcludeFile = Deep ^ exclude_contour_file,
        (
            Contour = do_not_apply_contour_exclusion,
            CallerCSDPtrPairs = list.map(pair_self, CallerCSDPtrs),
            Messages = []
        ;
            Contour = apply_contour_exclusion,
            (
                MaybeMaybeExcludeFile = no,
                % There is no contour exclusion file, so do the same as for
                % do_not_apply_contour_exclusion, but add a message to the
                % report.
                CallerCSDPtrPairs = list.map(pair_self, CallerCSDPtrs),
                Message = "There is no readable contour exclusion file.",
                Messages = [Message]
            ;
                MaybeMaybeExcludeFile = yes(MaybeExcludeFile),
                (
                    MaybeExcludeFile = ok(ExcludeSpec),
                    CallerCSDPtrPairs = list.map(
                        pair_contour(Deep, ExcludeSpec), CallerCSDPtrs),
                    Messages = []
                ;
                    MaybeExcludeFile = error(ErrorMsg),
                    CallerCSDPtrPairs = list.map(pair_self, CallerCSDPtrs),
                    MessagePrefix = "The contour exclusion file has an error:",
                    Messages = [MessagePrefix, ErrorMsg]
                )
            )
        ),
        (
            CallerGroups = group_by_call_site,
            CallSiteCallerGroups = group_csds_by_call_site(Deep,
                CallerCSDPtrPairs),
            ProcCallerCallSites = list.map(
                create_proc_caller_call_sites(Deep, PSPtr),
                CallSiteCallerGroups),
            Callers = proc_caller_call_sites(ProcCallerCallSites)
        ;
            CallerGroups = group_by_proc,
            ProcCallerGroups = group_csds_by_procedure(Deep,
                CallerCSDPtrPairs),
            ProcCallerProcs = list.map(
                create_proc_caller_procedures(Deep, PSPtr),
                ProcCallerGroups),
            Callers = proc_caller_procedures(ProcCallerProcs)
        ;
            CallerGroups = group_by_module,
            ModuleCallerGroups = group_csds_by_module(Deep,
                CallerCSDPtrPairs),
            ProcCallerModules = list.map(
                create_proc_caller_modules(Deep, PSPtr),
                ModuleCallerGroups),
            Callers = proc_caller_modules(ProcCallerModules)
        ;
            CallerGroups = group_by_clique,
            CliqueCallerGroups = group_csds_by_clique(Deep,
                CallerCSDPtrPairs),
            ProcCallerCliques = list.map(
                create_proc_caller_cliques(Deep, PSPtr),
                CliqueCallerGroups),
            Callers = proc_caller_cliques(ProcCallerCliques)
        ),

        ProcCallersReport = proc_callers_report(ProcDesc, Callers,
            BunchNum, Contour, Messages),
        MaybeProcCallersReport = ok(ProcCallersReport)
    ;
        MaybeProcCallersReport = error("invalid proc_static index")
    ).

:- func create_proc_caller_call_sites(deep, proc_static_ptr,
    pair(call_site_static_ptr, list(call_site_dynamic_ptr)))
    = perf_row_data(call_site_desc).

create_proc_caller_call_sites(Deep, CalleePSPtr, CSSPtr - CSDPtrs) =
        PerfRowData :-
    CallSiteDesc = describe_call_site(Deep, CSSPtr),
    compute_parent_csd_prof_info(Deep, CalleePSPtr, CSDPtrs, Own, Desc),
    own_and_inherit_to_perf_row_data(Deep, CallSiteDesc, Own, Desc,
        PerfRowData).

:- func create_proc_caller_procedures(deep, proc_static_ptr,
    pair(proc_static_ptr, list(call_site_dynamic_ptr)))
    = perf_row_data(proc_desc).

create_proc_caller_procedures(Deep, CalleePSPtr, PSSPtr - CSDPtrs) =
        PerfRowData :-
    ProcDesc = describe_proc(Deep, PSSPtr),
    compute_parent_csd_prof_info(Deep, CalleePSPtr, CSDPtrs, Own, Desc),
    own_and_inherit_to_perf_row_data(Deep, ProcDesc, Own, Desc,
        PerfRowData).

:- func create_proc_caller_modules(deep, proc_static_ptr,
    pair(string, list(call_site_dynamic_ptr)))
    = perf_row_data(string).

create_proc_caller_modules(Deep, CalleePSPtr, ModuleName - CSDPtrs) =
        PerfRowData :-
    compute_parent_csd_prof_info(Deep, CalleePSPtr, CSDPtrs, Own, Desc),
    own_and_inherit_to_perf_row_data(Deep, ModuleName, Own, Desc,
        PerfRowData).

:- func create_proc_caller_cliques(deep, proc_static_ptr,
    pair(clique_ptr, list(call_site_dynamic_ptr)))
    = perf_row_data(clique_desc).

create_proc_caller_cliques(Deep, CalleePSPtr, CliquePtr - CSDPtrs) =
        PerfRowData :-
    CliqueDesc = describe_clique(Deep, CliquePtr),
    compute_parent_csd_prof_info(Deep, CalleePSPtr, CSDPtrs, Own, Desc),
    own_and_inherit_to_perf_row_data(Deep, CliqueDesc, Own, Desc,
        PerfRowData).

%----------------------------------------------------------------------------%
%
% Code to generate the coverage annotated procedure representation report.
%

:- pred generate_procrep_coverage_dump_report(deep::in, prog_rep::in,
    proc_static_ptr::in, maybe_error(procrep_coverage_info)::out) is det.

generate_procrep_coverage_dump_report(Deep, ProgRep, PSPtr, MaybeReport) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        ProcLabel = PS ^ ps_id,
        ( progrep_search_proc(ProgRep, ProcLabel, ProcRep0) ->
            % Information about the procedure.
            deep_lookup_ps_own(Deep, PSPtr, Own),

            % Gather call site information.
            CallSitesArray = PS ^ ps_sites,
            array.foldl(create_cs_summary_add_to_map(Deep), CallSitesArray, 
                map.init) = CallSitesMap,

            % Gather information about coverage points.
            CoveragePointsArray = PS ^ ps_coverage_points,
            array.foldl2(add_coverage_point_to_map, CoveragePointsArray, 
                map.init, SolnsCoveragePointMap,
                map.init, BranchCoveragePointMap),

            procrep_annotate_with_coverage(Own, CallSitesMap,
                SolnsCoveragePointMap, BranchCoveragePointMap, 
                ProcRep0, ProcRep),
            MaybeReport = ok(procrep_coverage_info(PSPtr, ProcRep))
        ;
            MaybeReport =
                error("Program Representation doesn't contain procedure")
        )
    ;
        MaybeReport = error("Invalid proc_static index")
    ).

:- func create_cs_summary_add_to_map(deep, call_site_static_ptr, 
    map(goal_path, call_site_perf)) =  map(goal_path, call_site_perf).

create_cs_summary_add_to_map(Deep, CSStatic, Map0) = Map :-
    create_call_site_summary(Deep, CSStatic) = CSSummary,
    GoalPath = CSSummary ^ csf_goal_path,
    map.det_insert(Map0, GoalPath, CSSummary, Map).

:- pred add_coverage_point_to_map(coverage_point::in, 
    map(goal_path, coverage_point)::in, map(goal_path, coverage_point)::out, 
    map(goal_path, coverage_point)::in, map(goal_path, coverage_point)::out)
    is det.

add_coverage_point_to_map(CoveragePoint, !SolnsMap, !BranchMap) :-
    CoveragePoint = coverage_point(_, GoalPath, CPType),
    (
        CPType = cp_type_coverage_after,
        svmap.det_insert(GoalPath, CoveragePoint, !SolnsMap)
    ;
        CPType = cp_type_branch_arm,
        svmap.det_insert(GoalPath, CoveragePoint, !BranchMap)
    ).

%-----------------------------------------------------------------------------%
%
% Code to build the other dump reports.
%

:- pred create_proc_static_dump_report(deep::in, proc_static_ptr::in,
    maybe_error(proc_static_dump_info)::out) is det.

create_proc_static_dump_report(Deep, PSPtr, MaybeProcStaticDumpInfo) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        % Should we dump some other fields?
        PS = proc_static(_ProcId, _DeclModule, RefinedName, RawName,
            FileName, LineNumber, _InInterface, CallSites, CoveragePoints,
            _IsZeroed),
        array.max(CallSites, MaxCallSiteIdx),
        NumCallSites = MaxCallSiteIdx + 1,
        array.max(CoveragePoints, MaxCoveragePointIdx),
        NumCoveragePoints = MaxCoveragePointIdx + 1,
        ProcStaticDumpInfo = proc_static_dump_info(PSPtr, RawName, RefinedName,
            FileName, LineNumber, NumCallSites, NumCoveragePoints),
        MaybeProcStaticDumpInfo = ok(ProcStaticDumpInfo)
    ;
        MaybeProcStaticDumpInfo = error("invalid proc_static index")
    ).

:- pred create_proc_dynamic_dump_report(deep::in, proc_dynamic_ptr::in,
    maybe_error(proc_dynamic_dump_info)::out) is det.

create_proc_dynamic_dump_report(Deep, PDPtr, MaybeProcDynamicDumpInfo) :-
    ( valid_proc_dynamic_ptr(Deep, PDPtr) ->
        deep_lookup_proc_dynamics(Deep, PDPtr, PD),
        PD = proc_dynamic(PSPtr, CallSiteArray),
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        RawName = PS ^ ps_raw_id,
        RefinedName = PS ^ ps_refined_id,
        array.to_list(CallSiteArray, CallSites),
        ProcDynamicDumpInfo = proc_dynamic_dump_info(PDPtr, PSPtr,
            RawName, RefinedName, CallSites),
        MaybeProcDynamicDumpInfo = ok(ProcDynamicDumpInfo)
    ;
        MaybeProcDynamicDumpInfo = error("invalid proc_dynamic index")
    ).

:- pred create_call_site_static_dump_report(deep::in, call_site_static_ptr::in,
    maybe_error(call_site_static_dump_info)::out) is det.

create_call_site_static_dump_report(Deep, CSSPtr,
        MaybeCallSiteStaticDumpInfo) :-
    ( valid_call_site_static_ptr(Deep, CSSPtr) ->
        deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
        CSS = call_site_static(ContainingPSPtr, SlotNumber, CallSiteKind,
            LineNumber, GoalPath),
        CallSiteStaticDumpInfo = call_site_static_dump_info(CSSPtr,
            ContainingPSPtr, SlotNumber, LineNumber, GoalPath, CallSiteKind),
        MaybeCallSiteStaticDumpInfo = ok(CallSiteStaticDumpInfo)
    ;
        MaybeCallSiteStaticDumpInfo = error("invalid call_site_static index")
    ).

:- pred create_call_site_dynamic_dump_report(deep::in,
    call_site_dynamic_ptr::in,
    maybe_error(call_site_dynamic_dump_info)::out) is det.

create_call_site_dynamic_dump_report(Deep, CSDPtr,
        MaybeCallSiteDynamicDumpInfo) :-
    ( valid_call_site_dynamic_ptr(Deep, CSDPtr) ->
        deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
        CSD = call_site_dynamic(CallerPSPtr, CalleePSDPtr, Own),
        Desc = zero_inherit_prof_info,
        deep_lookup_call_site_static_map(Deep, CSDPtr, CSSPtr),
        CallSiteDesc = describe_call_site(Deep, CSSPtr),
        own_and_inherit_to_perf_row_data(Deep, CallSiteDesc, Own, Desc,
            PerfRowData),
        CallSiteDynamicDumpInfo = call_site_dynamic_dump_info(CSDPtr,
            CallerPSPtr, CalleePSDPtr, PerfRowData),
        MaybeCallSiteDynamicDumpInfo = ok(CallSiteDynamicDumpInfo)
    ;
        MaybeCallSiteDynamicDumpInfo = error("invalid call_site_dynamic index")
    ).

%-----------------------------------------------------------------------------%

    % Lookup the proc_static structure with the given PSI index number
    % and return performance information about it.
    %
:- pred psi_to_perf_row_data(deep::in, int::in, perf_row_data(proc_desc)::out)
    is det.

psi_to_perf_row_data(Deep, PSI, RowData) :-
    PSPtr = proc_static_ptr(PSI),
    ProcDesc = describe_proc(Deep, PSPtr),
    deep_lookup_ps_own(Deep, PSPtr, Own),
    deep_lookup_ps_desc(Deep, PSPtr, Desc),
    own_and_inherit_to_perf_row_data(Deep, ProcDesc, Own, Desc, RowData).

:- pred own_and_inherit_to_perf_row_data(deep::in, T::in,
    own_prof_info::in, inherit_prof_info::in, perf_row_data(T)::out) is det.

own_and_inherit_to_perf_row_data(Deep, Subject, Own, Desc, RowData) :-
    own_and_maybe_inherit_to_perf_row_data(Deep, Subject, Own, yes(Desc),
        RowData).

:- pred own_and_maybe_inherit_to_perf_row_data(deep::in, T::in,
    own_prof_info::in, maybe(inherit_prof_info)::in, perf_row_data(T)::out)
    is det.

own_and_maybe_inherit_to_perf_row_data(Deep, Subject, Own, MaybeDesc,
        RowData) :-
    % Look up global parameters and totals.
    ProfileStats = Deep ^ profile_stats,
    TicksPerSec = ProfileStats ^ ticks_per_sec,
    WordSize = ProfileStats ^ word_size,

    Root = root_total_info(Deep),
    RootQuanta = inherit_quanta(Root),
    RootCallseqs = inherit_callseqs(Root),
    RootAllocs = inherit_allocs(Root),
    RootWords = inherit_words(Root),

    % Port counts.
    Calls = calls(Own),
    Exits = exits(Own),
    Fails = fails(Own),
    Redos = redos(Own),
    Excps = excps(Own),

    % Self times.
    SelfTicks = quanta(Own),
    SelfTime = ticks_to_time(SelfTicks, TicksPerSec),
    SelfTimePercent = percent_from_ints(SelfTicks, RootQuanta),
    SelfTimePerCall = time_percall(SelfTime, Calls),

    % Self call sequence counts.
    SelfCallseqs = callseqs(Own),
    SelfCallseqsPercent = percent_from_ints(SelfCallseqs, RootCallseqs),
    SelfCallseqsPerCall = int_per_call(SelfCallseqs, Calls),

    % Self memory allocations.
    SelfAllocs = allocs(Own),
    SelfAllocsPercent = percent_from_ints(SelfAllocs, RootAllocs),
    SelfAllocsPerCall = int_per_call(SelfAllocs, Calls),

    % Self memory words.
    SelfWords = words(Own),
    SelfMemory = memory_words(SelfWords, WordSize),
    SelfMemoryPercent = percent_from_ints(SelfWords, RootWords),
    SelfMemoryPerCall = SelfMemory / Calls,

    SelfPerf = inheritable_perf(
        SelfTicks, SelfTime, SelfTimePercent, SelfTimePerCall,
        SelfCallseqs, SelfCallseqsPercent, SelfCallseqsPerCall,
        SelfAllocs, SelfAllocsPercent, SelfAllocsPerCall,
        SelfMemory, SelfMemoryPercent, SelfMemoryPerCall),

    (
        MaybeDesc = no,
        MaybeTotalPerf = no
    ;
        MaybeDesc = yes(Desc),

        % Self + descendants times.
        TotalTicks = SelfTicks + inherit_quanta(Desc),
        TotalTime = ticks_to_time(TotalTicks, TicksPerSec),
        TotalTimePercent = percent_from_ints(TotalTicks, RootQuanta),
        TotalTimePerCall = time_percall(TotalTime, Calls),

        % Self + descendants call sequence counts.
        TotalCallseqs = callseqs(Own) + inherit_callseqs(Desc),
        TotalCallseqsPercent = percent_from_ints(TotalCallseqs, RootCallseqs),
        TotalCallseqsPerCall = int_per_call(TotalCallseqs, Calls),

        % Self + descendants memory allocations.
        TotalAllocs = SelfAllocs + inherit_allocs(Desc),
        TotalAllocsPercent = percent_from_ints(TotalAllocs, RootAllocs),
        TotalAllocsPerCall = int_per_call(TotalAllocs, Calls),

        % Self + descendants memory words.
        TotalWords = SelfWords + inherit_words(Desc),
        TotalMemory = memory_words(TotalWords, WordSize),
        TotalMemoryPercent = percent_from_ints(TotalWords, RootWords),
        TotalMemoryPerCall = TotalMemory / Calls,

        TotalPerf = inheritable_perf(
            TotalTicks, TotalTime, TotalTimePercent, TotalTimePerCall,
            TotalCallseqs, TotalCallseqsPercent, TotalCallseqsPerCall,
            TotalAllocs, TotalAllocsPercent, TotalAllocsPerCall,
            TotalMemory, TotalMemoryPercent, TotalMemoryPerCall),
        MaybeTotalPerf = yes(TotalPerf)
    ),

    RowData = perf_row_data(Subject, Calls, Exits, Fails, Redos, Excps,
        WordSize, SelfPerf, MaybeTotalPerf).

%-----------------------------------------------------------------------------%

    % int_per_call(Num, Calls) is the quotient of Nom and Calls, after they've
    % both been cast to float.
    %
:- func int_per_call(int, int) = float.

int_per_call(Num, Calls) =
    ( Calls = 0 ->
        0.0
    ;
        float(Num) / float(Calls)
    ).

    % Give the percentage of two 'counts'.
    %
:- func percent_from_ints(int, int) = percent.

percent_from_ints(Nom, Denom) = Percent :-
    ( Denom = 0 ->
        Percent = percent(0.0)
    ;
        Percent = percent(float(Nom) / float(Denom))
    ).

%-----------------------------------------------------------------------------%

    % Create a proc_desc structure for a given proc static pointer.
    %
:- func describe_proc(deep, proc_static_ptr) = proc_desc.

describe_proc(Deep, PSPtr) = ProcDesc :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        FileName = PS ^ ps_file_name,
        LineNumber = PS ^ ps_line_number,
        RefinedName = PS ^ ps_refined_id
    ;
        FileName = "",
        LineNumber = 0,
        RefinedName = "mercury_runtime"
    ),
    ProcDesc = proc_desc(PSPtr, FileName, LineNumber, RefinedName).

    % Create a call_site_desc structure for a given call site static pointer.
    %
:- func describe_call_site(deep, call_site_static_ptr) = call_site_desc.

describe_call_site(Deep, CSSPtr) = CallSiteDesc :-
    ( valid_call_site_static_ptr(Deep, CSSPtr) ->
        deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
        CSS = call_site_static(ContainingPSPtr, SlotNumber, _Kind, LineNumber,
            GoalPath),
        deep_lookup_proc_statics(Deep, ContainingPSPtr, ContainingPS),
        FileName = ContainingPS ^ ps_file_name,
        RefinedName = ContainingPS ^ ps_refined_id
    ;
        ContainingPSPtr = dummy_proc_static_ptr,
        FileName = "",
        LineNumber = 0,
        RefinedName = "mercury_runtime",
        SlotNumber = -1,
        GoalPath = ""
    ),
    CallSiteDesc = call_site_desc(CSSPtr, ContainingPSPtr,
        FileName, LineNumber, RefinedName, SlotNumber, GoalPath).

    % Create a clique_desc structure for a given clique.
    %
:- func describe_clique(deep, clique_ptr) = clique_desc.

describe_clique(Deep, CliquePtr) = CliqueDesc :-
    ( valid_clique_ptr(Deep, CliquePtr) ->
        deep_lookup_clique_members(Deep, CliquePtr, MemberPDPtrs),
        ProcDescs = list.map(describe_clique_member(Deep), MemberPDPtrs),
        CliqueDesc = clique_desc(CliquePtr, ProcDescs)
    ;
        error("describe_clique", "invalid clique_ptr")
    ).

:- func describe_clique_member(deep, proc_dynamic_ptr) = proc_desc.

describe_clique_member(Deep, PDPtr) = ProcDesc :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    ProcDesc = describe_proc(Deep, PD ^ pd_proc_static).

%-----------------------------------------------------------------------------%
%
% Code shared across entire module.
%

:- func this_file = string.

this_file = "create_report.m".

%-----------------------------------------------------------------------------%

:- func error_message(string, string) = string.

error_message(Pred, Message) = Error :-
    Error = this_file ++ ": " ++ Pred ++ ": " ++ Message.

%-----------------------------------------------------------------------------%

:- pred error(string::in, string::in) is erroneous.

error(Pred, Message) :-
    throw(software_error(error_message(Pred, Message))).

%-----------------------------------------------------------------------------%
:- end_module create_report.
%-----------------------------------------------------------------------------%
