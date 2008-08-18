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

:- import_module report.
:- import_module profile.
:- import_module query.

%-----------------------------------------------------------------------------%

:- pred create_report(cmd::in, deep::in, deep_report::out) is det.

%-----------------------------------------------------------------------------%

:- implementation.

:- import_module measurement_units.
:- import_module measurements.
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
:- import_module univ.

%-----------------------------------------------------------------------------%

create_report(Cmd, Deep, Report) :-
    (
        Cmd = deep_cmd_quit,
        Msg = string.format("Shutting down deep profile server for %s.",
            [s(Deep ^ data_file_name)]),
        MessageInfo = message_report(Msg),
        Report = report_message(MessageInfo)
    ;
        Cmd = deep_cmd_timeout(Timeout),
        Msg = string.format("Timeout set to %d minutes.", [i(Timeout)]),
        MessageInfo = message_report(Msg),
        Report = report_message(MessageInfo)
    ;
        Cmd = deep_cmd_menu,
        Deep ^ profile_stats = profile_stats(ProgramName, 
            NumCSD, NumCSS, NumPD, NumPS,
            QuantaPerSec, InstrumentationQuanta, UserQuanta, NumCallseqs,
            _, _),
        NumCliques = array.max(Deep ^ clique_members),
        MenuInfo = menu_report(ProgramName, QuantaPerSec,
            UserQuanta, InstrumentationQuanta,
            NumCallseqs, NumCSD, NumCSS, NumPD, NumPS, NumCliques),
        Report = report_menu(ok(MenuInfo))
    ;
        Cmd = deep_cmd_top_procs(Limit, CostKind, InclDesc, Scope),
        create_top_procs_report(Deep, Limit, CostKind, InclDesc, Scope,
            MaybeTopProcsInfo),
        Report = report_top_procs(MaybeTopProcsInfo)
    ;
        Cmd = deep_cmd_proc(PSI),
        create_proc_report(Deep, PSI, MaybeProcReport),
        Report = report_proc(MaybeProcReport)
    ;
        Cmd = deep_cmd_proc_static(PSI),
        create_proc_static_dump_report(Deep, PSI, MaybeProcStaticDumpInfo),
        Report = report_proc_static_dump(MaybeProcStaticDumpInfo)
    ;
        Cmd = deep_cmd_proc_dynamic(PDI),
        create_proc_dynamic_dump_report(Deep, PDI, MaybeProcDynamicDumpInfo),
        Report = report_proc_dynamic_dump(MaybeProcDynamicDumpInfo)
    ;
        Cmd = deep_cmd_call_site_static(CSSI),
        create_call_site_static_dump_report(Deep, CSSI,
            MaybeCallSiteStaticDumpInfo),
        Report = report_call_site_static_dump(MaybeCallSiteStaticDumpInfo)
    ;
        Cmd = deep_cmd_call_site_dynamic(CSDI),
        create_call_site_dynamic_dump_report(Deep, CSDI,
            MaybeCallSiteStaticDumpInfo),
        Report = report_call_site_dynamic_dump(MaybeCallSiteStaticDumpInfo)
    ;
        Cmd = deep_cmd_restart,
        error("create_report/3", "unexpected restart command")
    ;
        ( Cmd = deep_cmd_root(_)
        ; Cmd = deep_cmd_clique(_)
        ; Cmd = deep_cmd_proc_callers(_, _, _)
        ; Cmd = deep_cmd_modules
        ; Cmd = deep_cmd_module(_)
        ; Cmd = deep_cmd_raw_clique(_)
        ),
        error("create_report/3", "Command not supported: " ++ string(Cmd))
    ).

%-----------------------------------------------------------------------------%
%
% Code to build top_procs report.
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
        list.map(psi_to_perf_row_data(Deep), TopPSIs, RowData),
        TopProcsReport = top_procs_report(Ordering, RowData),
        MaybeTopProcsReport = ok(TopProcsReport)
    ).

%-----------------------------------------------------------------------------%
%
% Code to build proc report.
%

:- pred create_proc_report(deep::in, int::in, maybe_error(proc_report)::out)
    is det.

create_proc_report(Deep, PSI, MaybeProcReport) :-
    PSPtr = proc_static_ptr(PSI),
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

        ProcReport = proc_report(PSPtr, ProcSummaryRowData,
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
% Code to build the dump reports.
%

:- pred create_proc_static_dump_report(deep::in, int::in,
    maybe_error(proc_static_dump_info)::out) is det.

create_proc_static_dump_report(Deep, PSI, MaybeProcStaticDumpInfo) :-
    PSPtr = proc_static_ptr(PSI),
    ( valid_proc_static_ptr(Deep, PSPtr) ->
        deep_lookup_proc_statics(Deep, PSPtr, PS),
        % Should we dump some other fields?
        PS = proc_static(_ProcId, _DeclModule, RefinedName, RawName,
            FileName, LineNumber, _InInterface, CallSites, _CoveragePoints,
            _IsZeroed),
        array.max(CallSites, NumCallSites),
        ProcStaticDumpInfo = proc_static_dump_info(PSPtr, RawName, RefinedName,
            FileName, LineNumber, NumCallSites),
        MaybeProcStaticDumpInfo = ok(ProcStaticDumpInfo)
    ;
        MaybeProcStaticDumpInfo = error("invalid proc_static index")
    ).

:- pred create_proc_dynamic_dump_report(deep::in, int::in,
    maybe_error(proc_dynamic_dump_info)::out) is det.

create_proc_dynamic_dump_report(Deep, PDI, MaybeProcDynamicDumpInfo) :-
    PDPtr = proc_dynamic_ptr(PDI),
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

:- pred create_call_site_static_dump_report(deep::in, int::in,
    maybe_error(call_site_static_dump_info)::out) is det.

create_call_site_static_dump_report(Deep, CSSI,
        MaybeCallSiteStaticDumpInfo) :-
    CSSPtr = call_site_static_ptr(CSSI),
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

:- pred create_call_site_dynamic_dump_report(deep::in, int::in,
    maybe_error(call_site_dynamic_dump_info)::out) is det.

create_call_site_dynamic_dump_report(Deep, CSDI,
        MaybeCallSiteDynamicDumpInfo) :-
    CSDPtr = call_site_dynamic_ptr(CSDI),
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

    % Self + descendants times.
    TotalTicks = SelfTicks + inherit_quanta(Desc),
    TotalTime = ticks_to_time(TotalTicks, TicksPerSec),
    TotalTimePercent = percent_from_ints(TotalTicks, RootQuanta),
    TotalTimePerCall = time_percall(TotalTime, Calls),

    % Self call sequence counts.
    SelfCallseqs = callseqs(Own),
    SelfCallseqsPercent = percent_from_ints(SelfCallseqs, RootCallseqs),
    SelfCallseqsPerCall = int_per_call(SelfCallseqs, Calls),

    % Self + descendants call sequence counts.
    TotalCallseqs = callseqs(Own) + inherit_callseqs(Desc),
    TotalCallseqsPercent = percent_from_ints(TotalCallseqs, RootCallseqs),
    TotalCallseqsPerCall = int_per_call(TotalCallseqs, Calls),

    % Self memory allocations.
    SelfAllocs = allocs(Own),
    SelfAllocsPercent = percent_from_ints(SelfAllocs, RootAllocs),
    SelfAllocsPerCall = int_per_call(SelfAllocs, Calls),

    % Self + descendants memory allocations.
    TotalAllocs = SelfAllocs + inherit_allocs(Desc),
    TotalAllocsPercent = percent_from_ints(TotalAllocs, RootAllocs),
    TotalAllocsPerCall = int_per_call(TotalAllocs, Calls),

    % Self memory words.
    SelfWords = words(Own),
    SelfMemory = memory_words(SelfWords, WordSize),
    SelfMemoryPercent = percent_from_ints(SelfWords, RootWords),
    SelfMemoryPerCall = SelfMemory / Calls,

    % Self + descendants memory words.
    TotalWords = SelfWords + inherit_words(Desc),
    TotalMemory = memory_words(TotalWords, WordSize),
    TotalMemoryPercent = percent_from_ints(TotalWords, RootWords),
    TotalMemoryPerCall = TotalMemory / Calls,

    RowData = perf_row_data(Subject,
        Calls, Exits, Fails, Redos, Excps,

        SelfTicks, SelfTime, SelfTimePercent, SelfTimePerCall,
        TotalTicks, TotalTime, TotalTimePercent, TotalTimePerCall,

        SelfCallseqs, SelfCallseqsPercent, SelfCallseqsPerCall,
        TotalCallseqs, TotalCallseqsPercent, TotalCallseqsPerCall,

        SelfAllocs, SelfAllocsPercent, SelfAllocsPerCall,
        TotalAllocs, TotalAllocsPercent, TotalAllocsPerCall,

        WordSize,
        SelfMemory, SelfMemoryPercent, SelfMemoryPerCall,
        TotalMemory, TotalMemoryPercent, TotalMemoryPerCall
    ).

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

    % Create a report_proc structure for a given proc static pointer.
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

    % Create a report_call_site structure for a given call site static pointer.
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
