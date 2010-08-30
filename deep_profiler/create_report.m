%-----------------------------------------------------------------------------%
% vim: ft=mercury ts=4 sw=4 et
%-----------------------------------------------------------------------------%
% Copyright (C) 2008-2010 The University of Melbourne.
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

:- import_module measurements.
:- import_module profile.
:- import_module query.
:- import_module report.

:- import_module list.
:- import_module maybe.

%-----------------------------------------------------------------------------%

:- pred create_report(cmd::in, deep::in, deep_report::out) is det.

%-----------------------------------------------------------------------------%

    % Create a proc var use dump report.
    %
:- pred create_proc_var_use_dump_report(deep::in, proc_static_ptr::in,
    maybe_error(proc_var_use_dump_info)::out) is det.

    % Create a proc report.
    %
:- pred create_proc_report(deep::in, proc_static_ptr::in,
    maybe_error(proc_report)::out) is det.

    % Create a procrep coverage report.
    %
:- pred create_procrep_coverage_report(deep::in,
    proc_static_ptr::in, maybe_error(procrep_coverage_info)::out) is det.
    
    % Create a top procs report, from the given data with the specified
    % parameters.
    %
:- pred create_top_procs_report(deep::in, display_limit::in, cost_kind::in,
    include_descendants::in, measurement_scope::in,
    maybe_error(top_procs_report)::out) is det.
    
    % Create a clique report, from the given data with the specified
    % parameters.
    %
:- pred create_clique_report(deep::in, clique_ptr::in,
    maybe_error(clique_report)::out) is det.

    % Instead of using the clique report above to find proc dynamics for a
    % clique, use this as it is much faster.
    %
:- pred find_clique_first_and_other_procs(deep::in, clique_ptr::in, 
    maybe(proc_dynamic_ptr)::out, list(proc_dynamic_ptr)::out) is det.
    
    % Create a proc_desc structure for a given proc static pointer.
    %
:- func describe_proc(deep, proc_static_ptr) = proc_desc.

%----------------------------------------------------------------------------%

    % Convert own and inherit perf information to row data used for reports.
    %
:- pred own_and_inherit_to_perf_row_data(deep::in, T::in,
    own_prof_info::in, inherit_prof_info::in, perf_row_data(T)::out) is det.

%----------------------------------------------------------------------------%

:- implementation.

:- import_module apply_exclusion.
:- import_module coverage.
:- import_module exclude.
:- import_module mdbcomp.
:- import_module mdbcomp.program_representation.
:- import_module measurement_units.
:- import_module program_representation_utils.
:- import_module recursion_patterns.
:- import_module top_procs.
:- import_module var_use_analysis.

:- import_module array.
:- import_module assoc_list.
:- import_module char.
:- import_module exception.
:- import_module float.
:- import_module int.
:- import_module map.
:- import_module math.
:- import_module pair.
:- import_module require.
:- import_module set.
:- import_module string.
:- import_module svmap.
:- import_module unit.
:- import_module univ.

%-----------------------------------------------------------------------------%

create_report(Cmd, Deep, Report) :-
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
        Cmd = deep_cmd_root(MaybePercent),
        create_root_report(Deep, MaybePercent, MaybeCliqueReport),
        Report = report_clique(MaybeCliqueReport)
    ;
        Cmd = deep_cmd_clique(CliquePtr),
        create_clique_report(Deep, CliquePtr, MaybeCliqueReport),
        Report = report_clique(MaybeCliqueReport)
    ;
        Cmd = deep_cmd_clique_recursive_costs(CliquePtr),
        create_clique_recursion_costs_report(Deep, CliquePtr,
            MaybeCliqueRecursionReport),
        Report = report_clique_recursion_costs(MaybeCliqueRecursionReport)
    ;
        Cmd = deep_cmd_recursion_types_frequency,
        create_recursion_types_frequency_report(Deep, MaybeRecTypesFreqReport),
        Report = report_recursion_types_frequency(MaybeRecTypesFreqReport)
    ;
        Cmd = deep_cmd_program_modules,
        create_program_modules_report(Deep, MaybeProgramModulesReport),
        Report = report_program_modules(MaybeProgramModulesReport)
    ;
        Cmd = deep_cmd_module(ModuleName),
        create_module_report(Deep, ModuleName, MaybeModuleReport),
        Report = report_module(MaybeModuleReport)
    ;
        Cmd = deep_cmd_module_getter_setters(ModuleName),
        create_module_getter_setter_report(Deep, ModuleName,
            MaybeModuleGetterSettersReport),
        Report = report_module_getter_setters(MaybeModuleGetterSettersReport)
    ;
        Cmd = deep_cmd_top_procs(Limit, CostKind, InclDesc, Scope),
        create_top_procs_report(Deep, Limit, CostKind, InclDesc, Scope,
            MaybeTopProcsReport),
        Report = report_top_procs(MaybeTopProcsReport)
    ;
        Cmd = deep_cmd_procrep_coverage(PSPtr),
        create_procrep_coverage_report(Deep, PSPtr,
            MaybeProcrepCoverageReport),
        Report = report_procrep_coverage(MaybeProcrepCoverageReport)
    ;
        Cmd = deep_cmd_proc(PSPtr),
        create_proc_report(Deep, PSPtr, MaybeProcReport),
        Report = report_proc(MaybeProcReport)
    ;
        Cmd = deep_cmd_proc_callers(PSPtr, CallerGroups, BunchNum,
            CallersPerBunch, Contour),
        create_proc_callers_report(Deep, PSPtr, CallerGroups, BunchNum,
            CallersPerBunch, Contour, MaybeProcCallersReport),
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
        Cmd = deep_cmd_dump_clique(CliquePtr),
        create_clique_dump_report(Deep, CliquePtr, MaybeCliqueDump),
        Report = report_clique_dump(MaybeCliqueDump)
    ;
        Cmd = deep_cmd_dump_proc_var_use(PSPtr),
        create_proc_var_use_dump_report(Deep, PSPtr, MaybeProcVarUseDump),
        Report = report_proc_var_use_dump(MaybeProcVarUseDump)
    ;
        Cmd = deep_cmd_restart,
        error("create_report/3", "unexpected restart command")
    ).

%-----------------------------------------------------------------------------%
%
% Code to build a clique report for the root clique, or the clique where
% the action begins.
%

:- pred create_root_report(deep::in, maybe(int)::in,
    maybe_error(clique_report)::out) is det.

create_root_report(Deep, MaybePercent, MaybeReport) :-
    deep_lookup_clique_index(Deep, Deep ^ root, RootCliquePtr),
    create_clique_report(Deep, RootCliquePtr, MaybeRootCliqueReport),
    (
        MaybeRootCliqueReport = error(_),
        MaybeReport = MaybeRootCliqueReport
    ;
        MaybeRootCliqueReport = ok(RootCliqueReport),
        (
            MaybePercent = no,
            MaybeReport = ok(RootCliqueReport)
        ;
            MaybePercent = yes(Percent),
            find_start_of_action(Deep, Percent, RootCliqueReport, Report),
            MaybeReport = ok(Report)
        )
    ).

:- pred find_start_of_action(deep::in, int::in,
    clique_report::in, clique_report::out) is det.

find_start_of_action(Deep, Percent, CurrentReport, SelectedReport) :-
    CurrentReport = clique_report(_, _, CliqueProcs),
    list.foldl(find_start_of_action_clique_proc(Percent), CliqueProcs,
        [], ActionCliquePtrs),
    (
        ActionCliquePtrs = [ActionCliquePtr],
        create_clique_report(Deep, ActionCliquePtr, MaybeActionCliqueReport),
        MaybeActionCliqueReport = ok(ActionCliqueReport)
    ->
        find_start_of_action(Deep, Percent, ActionCliqueReport, SelectedReport)
    ;
        SelectedReport = CurrentReport
    ).

:- pred find_start_of_action_clique_proc(int::in, clique_proc_report::in,
    list(clique_ptr)::in, list(clique_ptr)::out) is det.

find_start_of_action_clique_proc(Percent, CliqueProcReport,
        !ActionCliquePtrs) :-
    CliqueProcReport = clique_proc_report(_, FirstPDReport, LaterPDReports),
    find_start_of_action_clique_proc_dynamic(Percent,
        FirstPDReport, !ActionCliquePtrs),
    list.foldl(find_start_of_action_clique_proc_dynamic(Percent),
        LaterPDReports, !ActionCliquePtrs).

:- pred find_start_of_action_clique_proc_dynamic(int::in,
    clique_proc_dynamic_report::in,
    list(clique_ptr)::in, list(clique_ptr)::out) is det.

find_start_of_action_clique_proc_dynamic(Percent, CliquePDReport,
        !ActionCliquePtrs) :-
    CliquePDReport = clique_proc_dynamic_report(_, CallSites),
    list.foldl(find_start_of_action_call_site(Percent), CallSites,
        !ActionCliquePtrs).

:- pred find_start_of_action_call_site(int::in, clique_call_site_report::in,
    list(clique_ptr)::in, list(clique_ptr)::out) is det.

find_start_of_action_call_site(Percent, CallSiteReport, !ActionCliquePtrs) :-
    CallSiteReport = clique_call_site_report(_, _, CalleeRowDatas),
    list.foldl(find_start_of_action_callee(Percent), CalleeRowDatas,
        !ActionCliquePtrs).

:- pred find_start_of_action_callee(int::in, perf_row_data(clique_desc)::in,
    list(clique_ptr)::in, list(clique_ptr)::out) is det.

find_start_of_action_callee(Percent, RowData, !ActionCliquePtrs) :-
    MaybeTotalPerf = RowData ^ perf_row_maybe_total,
    (
        MaybeTotalPerf = no,
        error("find_start_of_action_callee: no total perf")
    ;
        MaybeTotalPerf = yes(TotalPerf),
        CallSeqsPercent = TotalPerf ^ perf_row_callseqs_percent,
        ( percent_at_or_above_threshold(Percent, CallSeqsPercent) ->
            CliqueDesc = RowData ^ perf_row_subject,
            CliquePtr = CliqueDesc ^ cdesc_clique_ptr,
            !:ActionCliquePtrs = [CliquePtr | !.ActionCliquePtrs]
        ;
            true
        )
    ).

%-----------------------------------------------------------------------------%
%
% Code to build a clique report.
%

create_clique_report(Deep, CliquePtr, MaybeCliqueReport) :-
    AncestorRowDatas = find_clique_ancestors(Deep, CliquePtr),

    deep_lookup_clique_members(Deep, CliquePtr, PDPtrs),
    list.foldl(group_proc_dynamics_by_proc_static(Deep), PDPtrs,
        map.init, PStoPDsMap),
    map.to_assoc_list(PStoPDsMap, PStoPDsList0),
    deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
    ( valid_call_site_dynamic_ptr(Deep, EntryCSDPtr) ->
        deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
        EntryPDPtr = EntryCSD ^ csd_callee,
        list.filter(proc_group_contains(EntryPDPtr), PStoPDsList0,
            EntryGroup, RestGroup),
        PStoPDsList = EntryGroup ++ RestGroup
    ;
        PStoPDsList = PStoPDsList0
    ),
    list.map(create_clique_proc_report(Deep, CliquePtr),
        PStoPDsList, CliqueProcs),

    CliqueReport = clique_report(CliquePtr, AncestorRowDatas, CliqueProcs),
    MaybeCliqueReport = ok(CliqueReport).

find_clique_first_and_other_procs(Deep, CliquePtr, MaybeFirstPDPtr,
        OtherPDPtrs) :-
    deep_lookup_clique_members(Deep, CliquePtr, PDPtrs),
    deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
    ( valid_call_site_dynamic_ptr(Deep, EntryCSDPtr) ->
        deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
        FirstPDPtr = EntryCSD ^ csd_callee,
        MaybeFirstPDPtr = yes(FirstPDPtr),
        list.negated_filter(unify(FirstPDPtr), PDPtrs,
            OtherPDPtrs)
    ;
        MaybeFirstPDPtr = no,
        OtherPDPtrs = PDPtrs
    ).

:- func find_clique_ancestors(deep, clique_ptr) =
    list(perf_row_data(ancestor_desc)).

find_clique_ancestors(Deep, CliquePtr) = Ancestors :-
    deep_lookup_clique_parents(Deep, CliquePtr, EntryCSDPtr),
    ( valid_call_site_dynamic_ptr(Deep, EntryCSDPtr) ->
        deep_lookup_call_site_dynamics(Deep, EntryCSDPtr, EntryCSD),
        EntryPDPtr = EntryCSD ^ csd_caller,
        ( EntryPDPtr = Deep ^ root ->
            % We could return the true root node, which is the Mercury runtime
            % system, but that is of no interest to either users or programs.
            Ancestors = []
        ;
            deep_lookup_clique_index(Deep, EntryPDPtr, EntryCliquePtr),
            CalleePDPtr = EntryCSD ^ csd_callee,
            deep_lookup_proc_dynamics(Deep, CalleePDPtr, CalleePD),
            CalleePSPtr = CalleePD ^ pd_proc_static,
            CalleeDesc = describe_proc(Deep, CalleePSPtr),
            deep_lookup_call_site_static_map(Deep, EntryCSDPtr, EntryCSSPtr),
            EntryCallSiteDesc = describe_call_site(Deep, EntryCSSPtr),
            AncestorDesc = ancestor_desc(EntryCliquePtr, CliquePtr,
                CalleeDesc, EntryCallSiteDesc),
            Own = EntryCSD ^ csd_own_prof,
            deep_lookup_csd_desc(Deep, EntryCSDPtr, Desc),
            own_and_inherit_to_perf_row_data(Deep, AncestorDesc, Own, Desc,
                Parent),
            MoreAncestors = find_clique_ancestors(Deep, EntryCliquePtr),
            Ancestors = [Parent | MoreAncestors]
        )
    ;
        Ancestors = []
    ).

:- pred group_proc_dynamics_by_proc_static(deep::in, proc_dynamic_ptr::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::in,
    map(proc_static_ptr, list(proc_dynamic_ptr))::out) is det.

group_proc_dynamics_by_proc_static(Deep, PDPtr, PStoPDsMap0, PStoPDsMap) :-
    require(valid_proc_dynamic_ptr(Deep, PDPtr),
        "group_proc_dynamics_by_proc_static: invalid PDPtr"),
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    ( map.search(PStoPDsMap0, PSPtr, PSPDs0) ->
        PSPDs = [PDPtr | PSPDs0],
        map.det_update(PStoPDsMap0, PSPtr, PSPDs, PStoPDsMap)
    ;
        map.det_insert(PStoPDsMap0, PSPtr, [PDPtr], PStoPDsMap)
    ).

:- pred proc_group_contains(proc_dynamic_ptr::in,
    pair(proc_static_ptr, list(proc_dynamic_ptr))::in) is semidet.

proc_group_contains(EntryPDPtr, _ - PDPtrs) :-
    list.member(EntryPDPtr, PDPtrs).

:- pred create_clique_proc_report(deep::in, clique_ptr::in,
    pair(proc_static_ptr, list(proc_dynamic_ptr))::in,
    clique_proc_report::out) is det.

create_clique_proc_report(Deep, CliquePtr, PSPtr - PDPtrs, CliqueProcReport) :-
    (
        PDPtrs = [],
        error("create_clique_proc_report", "PDPtrs = []")
    ;
        PDPtrs = [FirstPDPtr | LaterPDPtrs],
        ProcDesc = describe_proc(Deep, PSPtr),
        create_clique_proc_dynamic_report(Deep, CliquePtr, ProcDesc,
            FirstPDPtr, FirstPDOwn, FirstPDDesc, FirstPDReport),
        list.map3(create_clique_proc_dynamic_report(Deep, CliquePtr, ProcDesc),
            LaterPDPtrs, LaterPDOwns, LaterPDDescs, LaterPDReports),
        SummaryOwn = sum_own_infos([FirstPDOwn | LaterPDOwns]),
        SummaryDesc = sum_inherit_infos([FirstPDDesc | LaterPDDescs]),
        own_and_inherit_to_perf_row_data(Deep, ProcDesc,
            SummaryOwn, SummaryDesc, ProcSummaryRowData),
        CliqueProcReport = clique_proc_report(ProcSummaryRowData,
            FirstPDReport, LaterPDReports)
    ).

:- pred create_clique_proc_dynamic_report(deep::in, clique_ptr::in,
    proc_desc::in, proc_dynamic_ptr::in,
    own_prof_info::out, inherit_prof_info::out,
    clique_proc_dynamic_report::out) is det.

create_clique_proc_dynamic_report(Deep, _CliquePtr, ProcDesc, PDPtr,
        Own, Desc, CliquePDReport) :-
    ( valid_proc_dynamic_ptr(Deep, PDPtr) ->
        deep_lookup_pd_own(Deep, PDPtr, Own),
        deep_lookup_pd_desc(Deep, PDPtr, Desc),
        own_and_inherit_to_perf_row_data(Deep, ProcDesc, Own, Desc,
            PDRowData),
        deep_lookup_proc_dynamics(Deep, PDPtr, PD),
        PSPtr = PD ^ pd_proc_static,
        require(unify(PSPtr, ProcDesc ^ pdesc_ps_ptr),
            "create_clique_proc_dynamic_report: psptr mismatch"),
        create_child_call_site_reports(Deep, PDPtr, CliqueCallSiteReports),
        CliquePDReport = clique_proc_dynamic_report(PDRowData,
            CliqueCallSiteReports)
    ;
        error("invalid proc_dynamic index")
    ).

:- pred create_child_call_site_reports(deep::in, proc_dynamic_ptr::in,
    list(clique_call_site_report)::out) is det.

create_child_call_site_reports(Deep, PDPtr, CliqueCallSiteReports) :-
    deep_lookup_proc_dynamics(Deep, PDPtr, PD),
    PSPtr = PD ^ pd_proc_static,
    CSDArray = PD ^ pd_sites,
    deep_lookup_proc_statics(Deep, PSPtr, PS),
    CSSArray = PS ^ ps_sites,
    array.to_list(CSDArray, CSDSlots),
    array.to_list(CSSArray, CSSSlots),
    assoc_list.from_corresponding_lists(CSSSlots, CSDSlots, PairedSlots),
    list.map(create_child_call_site_report(Deep), PairedSlots,
        CliqueCallSiteReports).

:- pred create_child_call_site_report(deep::in,
    pair(call_site_static_ptr, call_site_array_slot)::in,
    clique_call_site_report::out) is det.

create_child_call_site_report(Deep, Pair, CliqueCallSiteReport) :-
    Pair = CSSPtr - CallSiteArraySlot,
    deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
    CallSiteDesc = describe_call_site(Deep, CSSPtr),
    Kind = CSS ^ css_kind,
    (
        Kind = normal_call_and_callee(CalleePSPtr, TypeSubst),
        KnownCalleeDesc = describe_proc(Deep, CalleePSPtr),
        ProcDescKind = normal_call_and_callee(KnownCalleeDesc, TypeSubst),
        (
            CallSiteArraySlot = slot_normal(CSDPtr)
        ;
            CallSiteArraySlot = slot_multi(_, _),
            error("create_child_call_site_report: normal_call error")
        ),
        ( valid_call_site_dynamic_ptr(Deep, CSDPtr) ->
            create_callee_clique_perf_row_data(Deep, CSDPtr, Own, Desc,
                CalleeCliqueRowData),
            CalleeCliqueRowDatas = [CalleeCliqueRowData]
        ;
            Own = zero_own_prof_info,
            Desc = zero_inherit_prof_info,
            CalleeCliqueRowDatas = []
        )
    ;
        (
            Kind = special_call_and_no_callee,
            ProcDescKind = special_call_and_no_callee
        ;
            Kind = higher_order_call_and_no_callee,
            ProcDescKind = higher_order_call_and_no_callee
        ;
            Kind = method_call_and_no_callee,
            ProcDescKind = method_call_and_no_callee
        ;
            Kind = callback_and_no_callee,
            ProcDescKind = callback_and_no_callee
        ),
        (
            CallSiteArraySlot = slot_normal(_),
            error("create_child_call_site_report: non-normal_call error")
        ;
            CallSiteArraySlot = slot_multi(_IsZeroed, CSDPtrsArray),
            array.to_list(CSDPtrsArray, CSDPtrs)
        ),
        list.map3(create_callee_clique_perf_row_data(Deep), CSDPtrs,
            Owns, Descs, CalleeCliqueRowDatas),
        Own = sum_own_infos(Owns),
        Desc = sum_inherit_infos(Descs)
    ),
    own_and_inherit_to_perf_row_data(Deep, CallSiteDesc, Own, Desc,
        SummaryRowData),
    CliqueCallSiteReport = clique_call_site_report(SummaryRowData,
        ProcDescKind, CalleeCliqueRowDatas).

:- pred create_callee_clique_perf_row_data(deep::in, call_site_dynamic_ptr::in,
    own_prof_info::out, inherit_prof_info::out,
    perf_row_data(clique_desc)::out) is det.

create_callee_clique_perf_row_data(Deep, CSDPtr, Own, Desc,
        CalleeCliqueRowData) :-
    require(valid_call_site_dynamic_ptr(Deep, CSDPtr),
        "create_callee_clique_perf_row_data: invalid call_site_dynamic_ptr"),
    deep_lookup_call_site_dynamics(Deep, CSDPtr, CSD),
    CalleePDPtr = CSD ^ csd_callee,
    Own = CSD ^ csd_own_prof,
    deep_lookup_csd_desc(Deep, CSDPtr, Desc),
    deep_lookup_clique_index(Deep, CalleePDPtr, CalleeCliquePtr),
    CliqueDesc = describe_clique(Deep, CalleeCliquePtr, yes(CalleePDPtr)),
    own_and_inherit_to_perf_row_data(Deep, CliqueDesc, Own, Desc,
        CalleeCliqueRowData).

%-----------------------------------------------------------------------------%
%
% Code to build a program_modules report.
%

    % Create a program_modules report, from the given data with the specified
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
% Code to build a module_getter_setters report.
%

:- type gs_field_raw_data
    --->    gs_field_raw_data(
                gs_raw_proc         :: proc_desc,
                gs_raw_own          :: own_prof_info,
                gs_raw_desc         :: inherit_prof_info
            ).

:- type raw_gs_field_info   == gs_field_info(gs_field_raw_data, unit).
:- type raw_gs_field_map    == gs_field_map(raw_gs_field_info).
:- type raw_gs_ds_map       == gs_ds_map(raw_gs_field_info).

    % Create a module_getter_setters report, from the given data
    % with the specified parameters.
    %
:- pred create_module_getter_setter_report(deep::in, string::in,
    maybe_error(module_getter_setters_report)::out) is det.

create_module_getter_setter_report(Deep, ModuleName,
        MaybeModuleGetterSettersReport) :-
    ( map.search(Deep ^ module_data, ModuleName, ModuleData) ->
        PSPtrs = ModuleData ^ module_procs,
        list.foldl(gather_getters_setters(Deep), PSPtrs,
            map.init, GetterSetterDataMap),
        map.map_values(getter_setter_raw_map_to_info_map(Deep),
            GetterSetterDataMap, GetterSetterInfoMap),
        ModuleGetterSettersReport = module_getter_setters_report(ModuleName,
            GetterSetterInfoMap),
        MaybeModuleGetterSettersReport = ok(ModuleGetterSettersReport)
    ;
        Msg = string.format("There is no module named `%s'.\n",
            [s(ModuleName)]),
        MaybeModuleGetterSettersReport = error(Msg)
    ).

:- pred getter_setter_raw_map_to_info_map(deep::in, data_struct_name::in,
    raw_gs_field_map::in, gs_field_map::out) is det.

getter_setter_raw_map_to_info_map(Deep, _DataStructName, RawMap, Map) :-
    map.map_values(getter_setter_raw_data_to_info(Deep), RawMap, Map).

:- pred getter_setter_raw_data_to_info(deep::in, field_name::in,
    raw_gs_field_info::in, gs_field_info::out) is det.

getter_setter_raw_data_to_info(Deep, _FieldName, RawData, Data) :-
    (
        RawData = gs_field_both(RawGetter, RawSetter, _),
        RawGetter = gs_field_raw_data(GetterProcDesc, GetterOwn, GetterDesc),
        RawSetter = gs_field_raw_data(SetterProcDesc, SetterOwn, SetterDesc),
        own_and_inherit_to_perf_row_data(Deep, GetterProcDesc,
            GetterOwn, GetterDesc, GetterRowData),
        own_and_inherit_to_perf_row_data(Deep, SetterProcDesc,
            SetterOwn, SetterDesc, SetterRowData),
        SumOwn = add_own_to_own(GetterOwn, SetterOwn),
        SumDesc = add_inherit_to_inherit(GetterDesc, SetterDesc),
        own_and_inherit_to_perf_row_data(Deep, unit, SumOwn, SumDesc,
            SumRowData),
        Data = gs_field_both(GetterRowData, SetterRowData, SumRowData)
    ;
        RawData = gs_field_getter(RawGetter),
        RawGetter = gs_field_raw_data(GetterProcDesc, GetterOwn, GetterDesc),
        own_and_inherit_to_perf_row_data(Deep, GetterProcDesc, GetterOwn,
            GetterDesc, GetterRowData),
        Data = gs_field_getter(GetterRowData)
    ;
        RawData = gs_field_setter(RawSetter),
        RawSetter = gs_field_raw_data(SetterProcDesc, SetterOwn, SetterDesc),
        own_and_inherit_to_perf_row_data(Deep, SetterProcDesc, SetterOwn,
            SetterDesc, SetterRowData),
        Data = gs_field_setter(SetterRowData)
    ).

:- pred gather_getters_setters(deep::in, proc_static_ptr::in,
    raw_gs_ds_map::in, raw_gs_ds_map::out) is det.

gather_getters_setters(Deep, PSPtr, !GSDSRawMap) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        Id = PS ^ ps_id,
        ( is_getter_or_setter(Id, GetterSetter, DataStructName, FieldName) ->
            deep_lookup_ps_own(Deep, PSPtr, Own),
            deep_lookup_ps_desc(Deep, PSPtr, Desc),
            ProcDesc = describe_proc(Deep, PSPtr),
            RawData = gs_field_raw_data(ProcDesc, Own, Desc),
            ( map.search(!.GSDSRawMap, DataStructName, FieldMap0Prime) ->
                FieldMap0 = FieldMap0Prime
            ;
                map.init(FieldMap0)
            ),
            ( map.search(FieldMap0, FieldName, FieldData0) ->
                (
                    GetterSetter = getter,
                    (
                        ( FieldData0 = gs_field_both(_, _, _)
                        ; FieldData0 = gs_field_getter(_)
                        ),
                        error("gather_getters_setters: redundant getter")
                    ;
                        FieldData0 = gs_field_setter(SetterRawData),
                        FieldData = gs_field_both(RawData, SetterRawData, unit)
                    )
                ;
                    GetterSetter = setter,
                    (
                        ( FieldData0 = gs_field_both(_, _, _)
                        ; FieldData0 = gs_field_setter(_)
                        ),
                        error("gather_getters_setters: redundant setter")
                    ;
                        FieldData0 = gs_field_getter(GetterRawData),
                        FieldData = gs_field_both(GetterRawData, RawData, unit)
                    )
                ),
                map.det_update(FieldMap0, FieldName, FieldData, FieldMap)
            ;
                (
                    GetterSetter = getter,
                    FieldData = gs_field_getter(RawData)
                ;
                    GetterSetter = setter,
                    FieldData = gs_field_setter(RawData)
                ),
                map.det_insert(FieldMap0, FieldName, FieldData, FieldMap)
            ),
            svmap.set(DataStructName, FieldMap, !GSDSRawMap)
        ;
            true
        )
    ;
        true
    ).

:- pred is_getter_or_setter(string_proc_label::in, getter_or_setter::out,
    data_struct_name::out, field_name::out) is semidet.

is_getter_or_setter(StringProcLabel, GetterSetter, DataStructName, FieldName) :-
    StringProcLabel = str_ordinary_proc_label(_PorF, DeclModule, DefModule,
        Name, Arity, _Mode),
    DeclModule = DefModule,
    string.to_char_list(Name, NameChars),
    is_getter_or_setter_2(NameChars, GetterSetter, DataStructNameChars,
        FieldNameChars),
    (
        GetterSetter = getter,
        Arity = 2
    ;
        GetterSetter = setter,
        Arity = 3
    ),
    string.from_char_list(DataStructNameChars, DataStructNameStr),
    string.from_char_list(FieldNameChars, FieldNameStr),
    DataStructName = data_struct_name(DataStructNameStr),
    FieldName = field_name(FieldNameStr).

:- pred is_getter_or_setter_2(list(char)::in, getter_or_setter::out,
    list(char)::out, list(char)::out) is semidet.

is_getter_or_setter_2(NameChars, GetterSetter, DataStructNameChars,
        FieldNameChars) :-
    ( NameChars = ['_', 'g', 'e', 't', '_' | FieldNameCharsPrime] ->
        GetterSetter = getter,
        DataStructNameChars = [],
        FieldNameChars = FieldNameCharsPrime
    ; NameChars = ['_', 's', 'e', 't', '_' | FieldNameCharsPrime] ->
        GetterSetter = setter,
        DataStructNameChars = [],
        FieldNameChars = FieldNameCharsPrime
    ;
        NameChars = [FirstNameChar | LaterNameChars],
        is_getter_or_setter_2(LaterNameChars, GetterSetter,
            LaterDataStructNameChars, FieldNameChars),
        DataStructNameChars = [FirstNameChar | LaterDataStructNameChars]
    ).

%-----------------------------------------------------------------------------%
%
% Code to build a top_procs report.
%

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
    CallSitePerf = call_site_perf(KindAndInfo, SummaryRowData, SubRowDatas).

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
    caller_groups::in, int::in, int::in, contour_exclusion::in,
    maybe_error(proc_callers_report)::out) is det.

create_proc_callers_report(Deep, PSPtr, CallerGroups, BunchNum,
        CallersPerBunch, Contour, MaybeProcCallersReport) :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        ProcDesc = describe_proc(Deep, PSPtr),

        deep_lookup_proc_callers(Deep, PSPtr, CallerCSDPtrs0),
        (
            Contour = do_not_apply_contour_exclusion,
            CallerCSDPtrPairs0 = list.map(pair_self, CallerCSDPtrs0),
            MaybeCallerCSDPtrPairs = ok(CallerCSDPtrPairs0),
            MaybeWarnMessage = no
        ;
            Contour = apply_contour_exclusion,
            ExcludeFile = Deep ^ exclude_contour_file,
            ExcludeFile = exclude_file(ExcludeFileName, ExcludeContents),
            (
                ExcludeContents = no_exclude_file,
                % There is no contour exclusion file, so do the same as for
                % do_not_apply_contour_exclusion, but add a message to the
                % report.
                string.format("Could not read contour exclusion file `%s'.",
                    [s(ExcludeFileName)], ErrorMessage0),
                MaybeCallerCSDPtrPairs = error(ErrorMessage0),
                MaybeWarnMessage = no
            ;
                ExcludeContents = unreadable_exclude_file(ErrorMsg),
                string.format(
                    "The contour exclusion file `%s' has an error: %s.",
                    [s(ExcludeFileName), s(ErrorMsg)], ErrorMessage0),
                MaybeCallerCSDPtrPairs = error(ErrorMessage0),
                MaybeWarnMessage = no
            ;
                ExcludeContents = readable_exclude_file(ExcludeModules,
                    MaybeWarnMsg),
                CallerCSDPtrPairs0 = list.map(
                    pair_contour(Deep, ExcludeModules), CallerCSDPtrs0),
                MaybeCallerCSDPtrPairs = ok(CallerCSDPtrPairs0),
                (
                    MaybeWarnMsg = no,
                    MaybeWarnMessage = no
                ;
                    MaybeWarnMsg = yes(WarnMessage),
                    MaybeWarnMessage = yes(WarnMessage)
                )
            )
        ),
        (
            MaybeCallerCSDPtrPairs = error(ErrorMessage),
            MaybeProcCallersReport = error(ErrorMessage)
        ;
            MaybeCallerCSDPtrPairs = ok(CallerCSDPtrPairs),
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
                BunchNum, CallersPerBunch, Contour, MaybeWarnMessage),
            MaybeProcCallersReport = ok(ProcCallersReport)
        )
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
    CliqueDesc = describe_clique(Deep, CliquePtr, no),
    compute_parent_csd_prof_info(Deep, CalleePSPtr, CSDPtrs, Own, Desc),
    own_and_inherit_to_perf_row_data(Deep, CliqueDesc, Own, Desc,
        PerfRowData).

%----------------------------------------------------------------------------%
%
% Code to create the coverage annotated procedure representation report.
%

create_procrep_coverage_report(Deep, PSPtr, MaybeReport) :-
    deep_get_maybe_progrep(Deep, MaybeProgRep),
    (
        MaybeProgRep = error(Error),
        MaybeReport = error(Error)
    ;
        MaybeProgRep = ok(ProgRep),
        ( valid_proc_static_ptr(Deep, PSPtr) ->
            deep_lookup_proc_statics(Deep, PSPtr, PS),
            ProcLabel = PS ^ ps_id,
            ( progrep_search_proc(ProgRep, ProcLabel, ProcRep0) ->
                % Information about the procedure.
                deep_lookup_ps_own(Deep, PSPtr, Own),

                % Gather call site information.
                CallSitesArray = PS ^ ps_sites,
                CallSitesMap = array.foldl(add_ps_own_info_to_map(Deep),
                    CallSitesArray, map.init),

                % Gather information about coverage points.
                CoveragePointsArray = PS ^ ps_coverage_points,
                array.foldl2(add_coverage_point_to_map, CoveragePointsArray,
                    map.init, SolnsCoveragePointMap,
                    map.init, BranchCoveragePointMap),

                procrep_annotate_with_coverage(Own, CallSitesMap,
                    SolnsCoveragePointMap, BranchCoveragePointMap,
                    ProcRep0, MaybeProcRep),
                (
                    MaybeProcRep = ok(ProcRep),
                    MaybeReport = ok(procrep_coverage_info(PSPtr, ProcRep))
                ;
                    MaybeProcRep = error(Error),
                    MaybeReport = error(Error)
                )
            ;
                MaybeReport =
                    error("Program Representation doesn't contain procedure: "
                        ++ string(PSPtr))
            )
        ;
            MaybeReport = error("Invalid proc_static index")
        )
    ).

:- func add_ps_own_info_to_map(deep, call_site_static_ptr,
    map(goal_path, own_prof_info)) = map(goal_path, own_prof_info).

add_ps_own_info_to_map(Deep, CSSPtr, !.Map) = !:Map :-
    lookup_call_site_statics(Deep ^ call_site_statics, CSSPtr, CSS),
    goal_path_from_string_det(CSS ^ css_goal_path, GoalPath),
    lookup_css_own(Deep ^ css_own, CSSPtr, Own),
    svmap.det_insert(GoalPath, Own, !Map).

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
        PS = proc_static(_ProcId, _DeclModule,
            UnQualRefinedName, QualRefinedName, RawName, FileName, LineNumber,
            _InInterface, CallSites, CoveragePoints, _IsZeroed),
        array.max(CallSites, MaxCallSiteIdx),
        NumCallSites = MaxCallSiteIdx + 1,
        array.max(CoveragePoints, MaxCoveragePointIdx),
        NumCoveragePoints = MaxCoveragePointIdx + 1,
        ProcStaticDumpInfo = proc_static_dump_info(PSPtr, RawName,
            UnQualRefinedName, QualRefinedName,
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
        ModuleName = PS ^ ps_decl_module,
        UnQualRefinedName = PS ^ ps_uq_refined_id,
        QualRefinedName = PS ^ ps_q_refined_id,
        array.to_list(CallSiteArray, CallSites),
        ProcDynamicDumpInfo = proc_dynamic_dump_info(PDPtr, PSPtr,
            RawName, ModuleName, UnQualRefinedName, QualRefinedName,
            CallSites),
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

:- pred create_clique_dump_report(deep::in, clique_ptr::in,
    maybe_error(clique_dump_info)::out) is det.

create_clique_dump_report(Deep, CliquePtr, MaybeCliqueDumpInfo) :-
    ( valid_clique_ptr(Deep, CliquePtr) ->
        CliqueDesc = describe_clique(Deep, CliquePtr, no),
        deep_lookup_clique_parents(Deep, CliquePtr, ParentCSDPtr),
        deep_lookup_clique_members(Deep, CliquePtr, MemberPDPtrs),
        CliqueDumpInfo = clique_dump_info(CliqueDesc, ParentCSDPtr,
            MemberPDPtrs),
        MaybeCliqueDumpInfo = ok(CliqueDumpInfo)
    ;
        MaybeCliqueDumpInfo = error("invalid clique_ptr")
    ).

:- pragma memo(create_proc_var_use_dump_report/3,
    [specified([addr, value, output])]).
create_proc_var_use_dump_report(Deep, PSPtr, MaybeProcVarUseDump) :-
    generic_vars_first_use(head_vars_all, Deep, PSPtr, set.init,
        MaybeProcVarUseDump).

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

describe_proc(Deep, PSPtr) = ProcDesc :-
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        FileName = PS ^ ps_file_name,
        LineNumber = PS ^ ps_line_number,
        ModuleName = PS ^ ps_decl_module,
        UnQualRefinedName = PS ^ ps_uq_refined_id,
        QualRefinedName = PS ^ ps_q_refined_id
    ;
        FileName = "",
        LineNumber = 0,
        ModuleName = "",
        UnQualRefinedName = "mercury_runtime",
        QualRefinedName = "mercury_runtime"
    ),
    ProcDesc = proc_desc(PSPtr, FileName, LineNumber, ModuleName,
        UnQualRefinedName, QualRefinedName).

    % Create a call_site_desc structure for a given call site static pointer.
    %
:- func describe_call_site(deep, call_site_static_ptr) = call_site_desc.

describe_call_site(Deep, CSSPtr) = CallSiteDesc :-
    ( valid_call_site_static_ptr(Deep, CSSPtr) ->
        deep_lookup_call_site_statics(Deep, CSSPtr, CSS),
        CSS = call_site_static(ContainingPSPtr, SlotNumber, Kind, LineNumber,
            GoalPathString),
        deep_lookup_proc_statics(Deep, ContainingPSPtr, ContainingPS),
        FileName = ContainingPS ^ ps_file_name,
        ModuleName = ContainingPS ^ ps_decl_module,
        UnQualRefinedName = ContainingPS ^ ps_uq_refined_id,
        QualRefinedName = ContainingPS ^ ps_q_refined_id,
        goal_path_from_string_det(GoalPathString, GoalPath),
        (
            Kind = normal_call_and_callee(CalleePSPtr, _TypeSubst),
            CalleeDesc = describe_proc(Deep, CalleePSPtr),
            MaybeCalleeDesc = yes(CalleeDesc)
        ;
            ( Kind = special_call_and_no_callee
            ; Kind = higher_order_call_and_no_callee
            ; Kind = method_call_and_no_callee
            ; Kind = callback_and_no_callee
            ),
            MaybeCalleeDesc = no
        )
    ;
        ContainingPSPtr = dummy_proc_static_ptr,
        FileName = "",
        LineNumber = 0,
        ModuleName = "",
        UnQualRefinedName = "mercury_runtime",
        QualRefinedName = "mercury_runtime",
        SlotNumber = -1,
        GoalPath = empty_goal_path,
        MaybeCalleeDesc = no
    ),
    CallSiteDesc = call_site_desc(CSSPtr, ContainingPSPtr,
        FileName, LineNumber, ModuleName, UnQualRefinedName, QualRefinedName,
        SlotNumber, GoalPath, MaybeCalleeDesc).

    % describe_clique(Deep, CliquePtr, MaybeEntryPDPtr) = CliqueDesc
    %
    % Create a clique_desc structure for a given clique.  The calculation for
    % the entry procedure into the clique can be overridden by supplying a
    % EntryPDPtr in MaybeEntryPDPtr.  This is useful when referring to a clique
    % from itself.
    %
:- func describe_clique(deep, clique_ptr, maybe(proc_dynamic_ptr)) =
    clique_desc.

describe_clique(Deep, CliquePtr, MaybeEntryPDPtr) = CliqueDesc :-
    ( valid_clique_ptr(Deep, CliquePtr) ->
        deep_lookup_clique_members(Deep, CliquePtr, MemberPDPtrs),
        deep_lookup_clique_parents(Deep, CliquePtr, ParentCSDPtr),
        deep_lookup_call_site_dynamics(Deep, ParentCSDPtr, ParentCSD),
        (
            MaybeEntryPDPtr = yes(EntryPDPtr)
        ;
            MaybeEntryPDPtr = no,
            EntryPDPtr = ParentCSD ^ csd_callee
        ),
        ( list.delete_first(MemberPDPtrs, EntryPDPtr, OtherPDPtrs) ->
            EntryProcDesc = describe_clique_member(Deep, EntryPDPtr),
            OtherProcDescs =
                list.map(describe_clique_member(Deep), OtherPDPtrs),
            CliqueDesc = clique_desc(CliquePtr, EntryProcDesc, OtherProcDescs)
        ;
            error("describe_clique", "entry pdptr not a member")
        )
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
